(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open DownloadSearch
open Options
open Mftp
open Mftp_comm
open DownloadServers
open BasicSocket
open TcpClientSocket
open DownloadOneFile
open DownloadFiles
open DownloadComplexOptions
open DownloadTypes
open DownloadOptions
open DownloadGlobals
open DownloadClient
open Gui_types
  
exception CommandCloseSocket

let result_name r =
  match r.result_names with
    [] -> None
  | name :: _ -> Some name


let percent file = 
  let downloaded = Int32.to_float file.file_downloaded in
  let size = Int32.to_float file.file_size in
  (downloaded *. 100.) /. size
      
let reconnect_all file =
  List.iter (fun c ->
      connection_must_try c.client_connection_control;
      connect_client !!client_ip [file] c) file.file_known_locations;
  List.iter (fun s ->
      match s.server_sock, s.server_state with
      | Some sock, (Connected_idle | Connected_busy) ->
          query_locations file s sock    
      | _ -> ()
  ) !connected_server_list
    
let last_search = ref []

let forget_search num =  
  if !last_xs = num then last_xs := (-1);
  searches := List.rev (List.fold_left (fun list s ->
        if s.search_num = num then list else s :: list) 
    [] !searches)

  
  
let save_file md4 name =
  let real_name = Filename.concat !!incoming_directory name in
  let files = ref [] in
  List.iter (fun file ->
      if file.file_md4 = md4 then begin
          old_files =:= file.file_md4 :: !!old_files;
          file.file_state <- FileRemoved;
          Unix32.close file.file_fd;
          let old_name = file.file_hardname in
          (try 
              Sys.rename old_name real_name ;
              change_hardname file real_name;
            with e -> 
                Printf.printf "Error in rename %s (src [%s] dst [%s])"
                  (Printexc.to_string e) old_name real_name; 
                print_newline ();
                let new_name = Filename.concat (Filename.dirname old_name)
                  (Filename.basename real_name) in
                try 
                  Sys.rename old_name new_name;
                  change_hardname file new_name
                with _ -> ()
          )
          ;
          file.file_changed <- FileInfoChange;
          !file_change_hook file;
        end 
      else
        files := file :: !files) 
  !!done_files;
  done_files =:= List.rev !files
  
  
  
let print_search buf s output = 
  last_search := [];
  let counter = ref 0 in
  Printf.bprintf buf "Result of search %d\n" s.search_num;
  Printf.bprintf buf "Reinitialising download selectors\n";
  Printf.bprintf buf "%d results (%s)\n" s.search_nresults 
    (if s.search_waiting = 0 then "done" else
      (string_of_int s.search_waiting) ^ " waiting");
  let results = ref [] in
  Hashtbl.iter (fun _ (r, avail) ->
      results := (r, avail) :: !results) s.search_files;
  let results = Sort.list (fun (r1,_) (r2,_) ->
        let r1 = Store.get DownloadIndexer.store r1 in
        let r2 = Store.get DownloadIndexer.store r2 in
        r1.result_size > r2.result_size
    ) !results in
  List.iter (fun (doc,avail) ->
      let r = Store.get DownloadIndexer.store doc in
      incr counter;
      Printf.bprintf  buf "[%5d]" !counter;
      if output.conn_output = HTML then 
        if !!use_html_frames then
          Printf.bprintf buf "\<A HREF=/submit\?q=download\&md4=%s\&size=%s target=status\>"
            (Md4.to_string r.result_md4) (Int32.to_string r.result_size)
        else
          Printf.bprintf buf "\<A HREF=/submit\?q=download\&md4=%s\&size=%s\>"
            (Md4.to_string r.result_md4) (Int32.to_string r.result_size);
      last_search := (!counter, 
        (r.result_size, r.result_md4, result_name r)
      ) :: !last_search;
      begin
        match r.result_names with
          [] -> ()
        | name :: names ->
            Printf.bprintf buf "%s\n" name;
            List.iter (fun s -> Printf.bprintf buf "       %s\n" s) names;
      end;
      if r.result_done then Printf.bprintf buf " ALREADY DOWNLOADED\n ";

      begin
        match r.result_comment with
          None -> ()
        | Some comment ->
            Printf.bprintf buf "COMMENT: %s\n" comment;
      end;
      if output.conn_output = HTML then 
        Printf.bprintf buf "\</A HREF\>";
      Printf.bprintf  buf "          %10s %10s " 
        (Int32.to_string r.result_size)
      (Md4.to_string r.result_md4);
      List.iter (fun t ->
          Buffer.add_string buf (Printf.sprintf "%-3s "
              (if t.tag_name = "availability" then string_of_int !avail else
              match t.tag_value with
                String s -> s
              | Uint32 i -> Int32.to_string i
              | Fint32 i -> Int32.to_string i
              | _ -> "???"
            ))
      ) r.result_tags;
      
      Buffer.add_char buf '\n';
  ) results
  
let check_shared_files () = 
  let list = ref [] in
  Hashtbl.iter (fun md4 file -> 
      if file.file_shared then
        match file.file_state with
          FileRemoved ->
            if not (Sys.file_exists file.file_hardname) then begin
                file.file_shared <- false;
                decr nshared_files;
                Unix32.close  file.file_fd;
                file.file_hardname <- "";
                list := file.file_md4 :: !list;
              end
        | _ -> ()) files_by_md4;
  List.iter (fun md4 ->
      try Hashtbl.remove files_by_md4 md4 with _ -> ()
  ) !list

let load_server_met filename =
  try
    let module S = Files.Server in
    let s = File.to_string filename in
    let ss = S.read s in
    List.iter (fun r ->
        let server = add_server r.S.ip r.S.port in
        List.iter (fun tag ->
            match tag with
              { tag_name = "name"; tag_value = String s } -> 
                server.server_name <- s;
            |  { tag_name = "description" ; tag_value = String s } ->
                server.server_description <- s
            | _ -> ()
        ) r.S.tags
    ) ss
  with e ->
      Printf.printf "Exception %s while loading %s" (Printexc.to_string e)
      filename;
      print_newline () 

        
let load_url kind url =
  Printf.printf "QUERY URL %s" url; print_newline ();
  let filename = Filename.temp_file "http_" ".tmp" in
  let file_oc = open_out filename in
  let file_size = ref 0 in
  Http_client.get_page (Url.of_string url) []
    (Http_client.default_headers_handler 
      (fun maxlen sock nread ->
        let buf = TcpClientSocket.buf sock in
        
        if nread > 0 then begin
            let left = 
              if maxlen >= 0 then
                min (maxlen - !file_size) nread
              else nread
            in
            output file_oc buf.buf buf.pos left;
            buf_used sock left;
            file_size := !file_size + left;
            if nread > left then
              TcpClientSocket.close sock "end read"
          end
        else
        if nread = 0 then begin
            close_out file_oc;
            try
              begin
                match kind with
                  "server.met" ->
                    load_server_met filename;
                    Printf.printf "SERVERS ADDED"; print_newline ();
                | "comments.met" ->
                    DownloadIndexer.load_comments filename;
                    Printf.printf "COMMENTS ADDED"; print_newline ();
                | _ -> failwith (Printf.sprintf "Unknown kind [%s]" kind)
              end;
              Sys.remove filename
              with e ->
                  Printf.printf
                    "Exception %s in loading downloaded file %s"
                    (Printexc.to_string e) filename
          
          end
    ))

      
let really_query_download filenames size md4 location old_file absents =
  
  List.iter (fun file -> 
      if file.file_md4 = md4 then raise Already_done) 
  !!done_files;
  
  List.iter (fun file -> 
      if file.file_md4 = md4 then raise Already_done) 
  !!files;

  let temp_file = Filename.concat !!temp_directory (Md4.to_string md4) in
  begin
    match old_file with
      None -> ()
    | Some filename ->
        if Sys.file_exists filename && not (
            Sys.file_exists temp_file) then
          (try 
              Printf.printf "Renaming from %s to %s" filename
                temp_file; print_newline ();
              Sys.rename filename temp_file with _ -> ());        
  end;
  
  let file = new_file  temp_file md4 size true in
  begin
    match absents with
      None -> ()
    | Some absents -> 
        let absents = Sort.list (fun (p1,_) (p2,_) -> p1 <= p2) absents in
        file.file_absent_chunks <- absents;
  end;
  
  let other_names = DownloadIndexer.find_names md4 in
  let filenames = List.fold_left (fun names name ->
        if List.mem name names then names else name :: names
    ) filenames other_names in 
  file.file_filenames <- filenames @ file.file_filenames;
  file.file_state <- FileDownloading;
  files =:= file :: !!files;
  !file_change_hook file;
  set_file_size file file.file_size;
  List.iter (fun s ->
      match s.server_sock with
        None -> () (* assert false !!! *)
      | Some sock ->
          query_locations file s sock
  ) !connected_server_list;
  
  (match location with
      None -> ()
    | Some num ->
        try 
          let c = find_client num in
          (match c.client_kind with
              Indirect_location -> 
                if not (List.memq c file.file_indirect_locations) then
                  file.file_known_locations <- c :: 
                  file.file_indirect_locations
            
            | _ -> 
                if not (List.memq c file.file_known_locations) then
                  file.file_known_locations <- c :: 
                  file.file_known_locations
          );
          match c.client_state with
            NotConnected -> 
              connect_client !!client_ip [file] c
          | Connected_busy | Connected_idle | Connected_queued ->
              begin
                match c.client_sock with
                  None -> ()
                | Some sock -> 
                    DownloadClient.query_files c sock [file]
              end
          | _ -> ()
        with _ -> ())


let aborted_download = ref None
        
let query_download filenames size md4 location old_file absents =
  
  List.iter (fun m -> 
      if m = md4 then begin
          aborted_download := Some (
            filenames,size,md4,location,old_file,absents);
          raise Already_done
        end) 
  !!old_files;
  really_query_download filenames size md4 location old_file absents
  
let load_prefs filename = 
  try
    let module P = Files.Pref in
    let s = File.to_string filename in
    let t = P.read s in
    t.P.client_tags, t.P.option_tags
  with e ->
      Printf.printf "Exception %s while loading %s" (Printexc.to_string e)
      filename;
      print_newline ();
      [], []
      
  
let import_config dirname =
  load_server_met (Filename.concat dirname "server.met");
  let ct, ot = load_prefs (Filename.concat dirname "pref.met") in
  let temp_dir = ref (Filename.concat dirname "temp") in

  List.iter (fun tag ->
      match tag with
      | { tag_name = "name"; tag_value = String s } ->
          client_name =:=  s
      | { tag_name = "port"; tag_value = Uint32 v } ->
          port =:=  Int32.to_int v
      | _ -> ()
  ) ct;

  List.iter (fun tag ->
      match tag with
      | { tag_name = "temp"; tag_value = String s } ->
          if Sys.file_exists s then (* be careful on that *)
            temp_dir := s
          else (Printf.printf "Bad temp directory, using default";
              print_newline ();)
      | _ -> ()
  ) ot;
  
  let list = Unix2.list_directory !temp_dir in
  let module P = Files.Part in
  List.iter (fun filename ->
      try
        if Filename2.last_extension filename = ".part" then
          let filename = Filename.concat !temp_dir filename in
          let met = filename ^ ".met" in
          if Sys.file_exists met then
            let s = File.to_string met in
            let f = P.read s in
            let filenames = ref [] in
            let size = ref Int32.zero in
            List.iter (fun tag ->
                match tag with
                  { tag_name = "filename"; tag_value = String s } ->
                    Printf.printf "Import Download %s" s; 
                    print_newline ();
                    
                    filenames := s :: !filenames;
                | { tag_name = "size"; tag_value = Uint32 v } ->
                    size := v
                | _ -> ()
            ) f.P.tags;
            query_download !filenames !size f.P.md4 None 
              (Some filename) (Some (List.rev f.P.absents));
      
      with _ -> ()
  ) list
  
let broadcast msg =
  let s = msg ^ "\n" in
  let len = String.length s in
  List.iter (fun sock ->
      TcpClientSocket.write sock s 0 len
  ) !user_socks
  
  
type arg_handler =  Buffer.t -> connection_options -> string
type arg_kind = 
  Arg_none of arg_handler
| Arg_multiple of (string list -> arg_handler)
| Arg_one of (string -> arg_handler)
| Arg_two of (string -> string -> arg_handler)
  
let execute_command arg_list buf output cmd args =
  try
    List.iter (fun (command, arg_kind, help) ->
        if command = cmd then
          Buffer.add_string buf (
            match arg_kind, args with
              Arg_none f, [] -> f buf output
            | Arg_multiple f, _ -> f args buf output
            | Arg_one f, [arg] -> f arg buf output
            | Arg_two f, [a1;a2] -> f a1 a2 buf output
            | _ -> "Bad number of arguments"
          )
    ) arg_list
  with Not_found -> ()

let print_file buf file =
  Printf.bprintf buf "[%-5d] %s %10s %32s %s" 
    file.file_num
    (first_name file)
  (Int32.to_string file.file_size)
  (Md4.to_string file.file_md4)
    (if file.file_state = FileDownloaded then
      "done" else
      Int32.to_string file.file_downloaded);
  Buffer.add_char buf '\n';
  Printf.bprintf buf "Connected clients:\n";
  List.iter (fun c ->
      if c.client_state <> NotConnected then
        match c.client_kind with
          Known_location (ip, port) ->
            Printf.bprintf  buf "[%-5d] %12s %-5d    %s\n"
              c.client_num
              (Ip.to_string ip)
            port
            (match c.client_sock with
                None -> ""
              | Some _ -> "Connected")
        | _ ->
            Printf.bprintf  buf "[%-5d] %12s            %s\n"
              c.client_num
              "indirect"
              (match c.client_sock with
                None -> ""
              | Some _ -> "Connected")
  ) (file.file_known_locations @ file.file_indirect_locations);
  Printf.bprintf buf "\nChunks: \n";
  Array.iteri (fun i c ->
      Buffer.add_char buf (
        match c with
          PresentVerified -> 'V'
        | PresentTemp -> 'p'
        | AbsentVerified -> '_'
        | AbsentTemp -> '.'
        | PartialTemp _ -> '?'
        | PartialVerified _ -> '!'
      )
  ) file.file_chunks
  
  

let short_name file =
  let name = first_name file in
  let len = String.length name in
  let max_name_len = max !!max_name_len 10 in
  if len > max_name_len then
    let prefix = String.sub name 0 (max_name_len -7) in
    let suffix = String.sub name (len-4) 4 in
    Printf.sprintf "%s...%s" prefix suffix
  else name

type table_align = 
  Align_Left
| Align_Right
| Align_Center

let col_sep = "  "
let add buf s align max_len =
  let slen = String.length s in
  let diff = max_len - slen in
  match align with
    Align_Center ->
      let left = diff / 2 in
      let right = diff - left in
      Printf.bprintf buf "%s%s%s" 
        (String.make left ' ') s (String.make right ' ')
  | Align_Right ->
      Printf.bprintf buf "%s%s" (String.make diff ' ') s
  | Align_Left ->
      Printf.bprintf buf "%s%s" s (String.make diff ' ')
      
let print_table_text buf alignments titles lines =
  let max_cols = ref (max (Array.length titles) (Array.length alignments)) in
  List.iter (fun line ->
      let len = Array.length line in
      if len > !max_cols then max_cols := len
  ) lines;
  let ncols = !max_cols in
  let cols = Array.create ncols 0 in
  List.iter (fun line ->
      let len = Array.length line in
      for i = 0 to len-1 do 
        let slen = String.length line.(i) in
        if cols.(i) <  slen then cols.(i) <- slen
      done;
  ) (titles :: lines);
  Array.iteri (fun i s -> 
      add buf s Align_Center cols.(i);
      Buffer.add_string buf col_sep;
  ) titles;
  Buffer.add_char buf '\n';
  let aligns = Array.create ncols Align_Center in
  Array.iteri (fun i al -> aligns.(i) <- al) alignments; 
  List.iter (fun line ->
      Array.iteri (fun i s ->
          add buf s aligns.(i) cols.(i);
      Buffer.add_string buf col_sep;
      ) line;
      Buffer.add_char buf '\n';      
  ) lines

let print_table_html buf aligns titles lines =
  Printf.bprintf buf "\<TABLE\>";
  Printf.bprintf buf "\<TR\>";
  Array.iter (fun title ->
      Printf.bprintf buf "\<TD ALIGN=CENTER\>%s\</TD\>" title;
  ) titles;
  let naligns = Array.length aligns in
  Printf.bprintf buf "\</TR\>";
  List.iter (fun line ->
      Printf.bprintf buf "\<TR\>";
      Array.iteri (fun i title ->
          Printf.bprintf buf "\<TD%s\>%s\</TD\>" 
            (if i >= naligns then "" else
            match aligns.(i) with
              Align_Center -> " ALIGN=CENTER"
            | Align_Left -> " ALIGN=LEFT"
            | Align_Right -> " ALIGN=RIGHT")
          title;
      ) line;
      Printf.bprintf buf "\</TR\>";
  ) lines;
  Printf.bprintf buf "\</TABLE\>"

  (*
  
let simple_print_file buf name_len done_len size_len format file =
  Printf.bprintf buf "[%-5d] "
      file.file_num;
  if format.conn_output = HTML && file.file_state <> FileDownloaded then 
    Printf.bprintf buf "[\<a href=/submit\?q\=cancel\+%d target\=status\>CANCEL\</a\>] " 
    file.file_num;
  let s = short_name file in
  Printf.bprintf buf "%s%s " s
    (String.make (name_len - (String.length s)) ' ');

  if file.file_state <> FileDownloaded then begin
      let s = Int32.to_string file.file_downloaded in
      Printf.bprintf buf "%s%s " s (String.make (
          done_len - (String.length s)) ' ');
    end;

  let s = Int32.to_string file.file_size in
  Printf.bprintf buf "%s%s " s (String.make (
      size_len - (String.length s)) ' ');

  if file.file_state = FileDownloaded then
    Buffer.add_string buf (Md4.to_string file.file_md4)
  else
  if file.file_state = FilePaused then
    Buffer.add_string buf "Paused"
  else
  if file.file_last_rate < 10.24 then
    Buffer.add_string buf "-"
  else
    Printf.bprintf buf "%5.1f" (file.file_last_rate /. 1024.);
  Buffer.add_char buf '\n'
*)
  
let simple_print_file_list finished buf files format =
  let print_table = if format.conn_output = HTML then print_table_html else print_table_text in
  if not finished then
    print_table buf 
      [| Align_Left; Align_Left; Align_Right; Align_Right; Align_Right; Align_Right |] 
      (if format.conn_output = HTML then
        [|
          "[ Num ]"; 
          "\<a href=/submit\?q\=vd\&sortby\=name\> File \</a\>"; 
          "\<a href=/submit\?q\=vd\&sortby\=percent\> Percent \</a\>"; 
          "\<a href=/submit\?q\=vd\&sortby\=done\> Downloaded \</a\>";
          "\<a href=/submit\?q\=vd\&sortby\=size\> Size \</a\>"; 
          "\<a href=/submit\?q\=vd\&sortby\=rate\> Rate \</a\>"; 
        |] else
        [|
          "[ Num ]"; 
          "File";
          "Percent"; 
          "Downloaded";
          "Size";
          "Rate";
        |]     
    )
      (List.map (fun file ->
          [|
            (Printf.sprintf "[%-5d]%s"
                file.file_num
                (if format.conn_output = HTML then  Printf.sprintf "[\<a href=/submit\?q\=cancel\+%d target\=status\>CANCEL\</a\>] " file.file_num else ""));
            (short_name file);
            (Printf.sprintf "%5.1f" (percent file));
            (Int32.to_string file.file_downloaded);
            (Int32.to_string file.file_size);

            (if file.file_state = FilePaused then
                "Paused"
              else
              if file.file_last_rate < 10.24 then
                "-"
              else
                Printf.sprintf "%5.1f" (file.file_last_rate /. 1024.));
          |]
      ) files)
  else
    print_table buf 
      [||]     
    (if format.conn_output = HTML then
    [|
      "[ Num ]"; 
      "\<a href=/submit\?q\=vd\&sortby\=name\> File \</a\>"; 
      "\<a href=/submit\?q\=vd\&sortby\=size\> Size \</a\>"; 
      "MD4"; 
        |] 
      else
    [|
      "[ Num ]"; 
      "File"; 
      "Size"; 
      "MD4"; 
        |] 
    )
    (List.map (fun file ->
        [|
            (Printf.sprintf "[%-5d]" file.file_num);
            (short_name file);
            (Int32.to_string file.file_size);
            (Md4.to_string file.file_md4)
        |]
    ) files)
  
  (*
  Printf.bprintf buf "\<TABLE\>\<TR\>
  \<TD\> [ Num ] \</TD\> 
  \<TD\> \<a href=/submit\?q\=vd\&sortby\=name\> File \</a\> \</TD\>";
  
  if not finished then
    Printf.bprintf buf 
    "\<TD ALIGN\=RIGHT\> \<a href=/submit\?q\=vd\&sortby\=percent\> Percent \</a\> \</TD\> 
\<TD ALIGN\=RIGHT\> \<a href=/submit\?q\=vd\&sortby\=done\> Downloaded \</a\> \</TD\> ";
  
  Printf.bprintf buf 
    "\<TD ALIGN=RIGHT\> \<a href=/submit\?q\=vd\&sortby\=size\> Size \</a\> \</TD\> ";
  
  Printf.bprintf buf "\<TD\> \<a href=/submit\?q\=vd\&sortby\=rate\> %s \</a\> \</TD\> " (if finished then "MD4" else "Rate");
  Printf.bprintf  buf "\</TR\>\n";
  
  List.iter (fun file ->
      Printf.bprintf buf "\<TR\> \<TD ALIGN\=RIGHT\> [%-5d]"
        file.file_num;
      if file.file_state <> FileDownloaded then 
        Printf.bprintf buf "[\<a href=/submit\?q\=cancel\+%d target\=status\>CANCEL\</a\>] " 
          file.file_num;
      Printf.bprintf  buf "\</TD\>";
      Printf.bprintf buf " \<TD\> %s \</TD\> " (short_name file);

      if file.file_state <> FileDownloaded then 
        Printf.bprintf buf "\<TD ALIGN\=RIGHT\> %5.1f \</TD\> \<TD ALIGN\=RIGHT\> %s \</TD\> " (percent file) (Int32.to_string file.file_downloaded);

      Printf.bprintf buf "\<TD ALIGN=RIGHT\> %s \</TD\> " (Int32.to_string file.file_size);

      Printf.bprintf buf "\<TD ALIGN=RIGHT\> %s \</TD\>"
        (if file.file_state = FileDownloaded then
          Md4.to_string file.file_md4
        else
        if file.file_state = FilePaused then
          "Paused"
        else
        if file.file_last_rate < 10.24 then
          "-"
        else
          Printf.sprintf "%5.1f" (file.file_last_rate /. 1024.));
      Buffer.add_string buf "\</TR\>"
      
  ) files;
  
  Printf.bprintf  buf "\</TABLE\>\n"
*)
(*  
let simple_print_file_list finished buf files format =
(*  if format.conn_output = HTML then *)
    simple_print_file_list_html finished buf files format
(*  else
  let size_len = ref 10 in
  let done_len = ref 10 in
  let name_len = ref 1 in
  List.iter 
    (fun f ->
      name_len := max !name_len (String.length (short_name f));
      size_len := max !size_len (String.length (Int32.to_string f.file_size));
      done_len := max !done_len (String.length (Int32.to_string f.file_downloaded));
  )
  files;
  Printf.bprintf buf "[ Num ] ";
  if format.conn_output = HTML then Printf.bprintf buf "         ";

  let make_spaces len s =
    String.make (max 0 (len - (String.length s))) ' '
  in
  let s = "File" in
  if format.conn_output = HTML then
    Printf.bprintf buf "\<a href=/submit\?q\=vd\&sortby\=name\>%s\</a\>%s "
    s (make_spaces  !name_len s)
  else
    Printf.bprintf buf "%s%s " s (make_spaces  !name_len s);
  
  if not finished then
    begin
      let s = "Downloaded" in
      if format.conn_output = HTML then
        Printf.bprintf buf "\<a href=/submit\?q\=vd\&sortby\=done\>%s\</a\>%s "
          s (make_spaces !done_len s)
      else
        Printf.bprintf buf "%s%s " s (make_spaces !done_len s);
    end;
  
  let s = "Size" in
  if format.conn_output = HTML then
    Printf.bprintf buf "\<a href=/submit\?q\=vd\&sortby\=size\>%s\</a\>%s "
      s (make_spaces !size_len s)
  else
    Printf.bprintf buf "%s%s " s (make_spaces !size_len s);
  
  let s = if finished then "MD4" else
    if format.conn_output = HTML then
      "\<a href=/submit\?q\=vd\&sortby\=rate\>Rate\</a\>"
    else "Rate"
  in
  Printf.bprintf buf "%s\n" s;
  
  List.iter (simple_print_file buf !name_len !done_len !size_len format) files
*)
*)  

          
  
let commands = [
    "n", Arg_multiple (fun args buf _ ->
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4661"
          | _ -> failwith "n <ip> [<port>]: bad argument number"
        in
        let ip = Ip.of_string ip in
        let port = int_of_string port in
        
        let s = add_server ip port in
        Printf.bprintf buf "New server %s:%d\n" 
          (Ip.to_string s.server_ip) 
        s.server_port;
        ""
    ), " <ip> [<port>]: add a server";
    
    "vu", Arg_none (fun buf _ ->
        Printf.sprintf "Upload credits : %d minutes\nUpload disabled for %d minutes" !upload_credit !has_upload;
    
    ), " : view upload credits";
    
    "vc", Arg_one (fun num buf output ->
        try
          let num = int_of_string num in
          let c = find_client num in
          (match c.client_kind with
              Indirect_location -> 
                Printf.bprintf buf "Client [%5d] Indirect client\n" num
            | Known_location (ip, port) ->
                Printf.bprintf buf "Client [%5d] %s:%d\n" num
                  (Ip.to_string ip) port);
          Printf.bprintf buf "Name: %s\n" c.client_name;
          (match c.client_all_files with
              None -> ()
            | Some results ->
                Printf.bprintf buf "Files:\n";
                List.iter (fun doc ->
                    let r = Store.get DownloadIndexer.store doc in
                    if output.conn_output = HTML then 
                      Printf.bprintf buf "\<A HREF=/submit\?q=download\&md4=%s\&size=%s\>"
                        (Md4.to_string r.result_md4) (Int32.to_string r.result_size);
                    begin
                      match r.result_names with
                        [] -> ()
                      | name :: names ->
                          Printf.bprintf buf "%s\n" name;
                          List.iter (fun s -> Printf.bprintf buf "       %s\n" s) names;
                    end;
                    begin
                      match r.result_comment with
                        None -> ()
                      | Some comment ->
                          Printf.bprintf buf "COMMENT: %s\n" comment;
                    end;
                    if output.conn_output = HTML then 
                      Printf.bprintf buf "\</A HREF\>";
                    Printf.bprintf  buf "          %10s %10s " 
                      (Int32.to_string r.result_size)
                    (Md4.to_string r.result_md4);
                    List.iter (fun t ->
                        Buffer.add_string buf (Printf.sprintf "%-3s "
                            (match t.tag_value with
                              String s -> s
                            | Uint32 i -> Int32.to_string i
                            | Fint32 i -> Int32.to_string i
                            | _ -> "???"
                          ))
                    ) r.result_tags;
                    Buffer.add_char buf '\n';
                ) results
          
          );
          
          ""
        with _ -> "No such client"
    ), " <num> : view client";

    "mem_stats", Arg_none (fun buf _ -> 
        DownloadGlobals.mem_stats buf;
        ""
    ), " : print memory stats";
    
    "comments", Arg_one (fun filename buf _ ->
        DownloadIndexer.load_comments filename;
        DownloadIndexer.save_comments ();
        "comments loaded and saved"
    ), " <filename> : load comments from file";
    
    "comment", Arg_two (fun md4 comment buf _ ->
        let md4 = Md4.of_string md4 in
        DownloadIndexer.add_comment md4 comment;
        "Comment added"
    ), " <md4> \"<comment>\" : add comment on an md4";
    
    "nu", Arg_one (fun num buf _ ->
        let num = int_of_string num in
        if num <= !upload_credit then
          begin
            upload_credit := !upload_credit - num;
            has_upload := !has_upload + num;
            Printf.sprintf "upload disabled for %d minutes" num
          end
        else 
          "not enough upload credits"
    
    
    ), " <m> : disable upload during <m> minutes (multiple of 5)";
    
    "import", Arg_one (fun dirname buf _ ->
        
        try
          import_config dirname;
          "config loaded"
        with e ->
            Printf.sprintf "error %s while loading config" (
              Printexc.to_string e)
    ), " <dirname> : import the config from dirname";
    
    "load_old_history", Arg_none (fun buf _ ->
        DownloadIndexer.load_old_history ();
        "Old history loaded"
    ), " : load history.dat file";
    
    "x", Arg_one (fun num buf _ ->
        try
          let num = int_of_string num in
          let s = Hashtbl.find servers_by_num num in
          match s.server_sock with
            None -> "Not connected"
          | Some sock ->
              shutdown sock "user disconnect";
              "Disconnected"
        with e ->
            Printf.sprintf "Error: %s" (Printexc.to_string e)
    ), " <num> : disconnect from server";
    
    "servers", Arg_one (fun filename buf _ ->
        try
          load_server_met filename;
          "file loaded"
        with e -> 
            Printf.sprintf "error %s while loading file" (Printexc.to_string e)
    ), " <filename> : add the servers from a server.met file";
    
    "close_fds", Arg_none (fun buf _ ->
        Unix32.close_all ();
        "All files closed"
    ), " : close all files (use to free space on disk after remove)";
    
    "commit", Arg_none (fun buf _ ->
        List.iter (fun file ->
            save_file file.file_md4 (first_name file)
        ) !!done_files;
        "commited"
    ) , ": move downloaded files to incoming directory";
    
    "vd", Arg_multiple (fun args buf format -> 
        match args with
          [arg] ->
            let num = int_of_string arg in
            List.iter 
              (fun file -> if file.file_num = num then print_file buf file)
            !!files;
            List.iter
              (fun file -> if file.file_num = num then print_file buf file)
            !!done_files;
            ""
        | _ ->
            Printf.bprintf  buf "Downloaded %d/%d files\n"           (List.length !!done_files) (List.length !!files);
            let list = 
              try
                let sorter =
                  match format.conn_sortvd with
                  
                  | BySize -> (fun f1 f2 -> f1.file_size >= f2.file_size)
                  | ByRate -> (fun f1 f2 -> 
                          f1.file_last_rate >= f2.file_last_rate)
                  | ByName -> (fun f1 f2 -> 
                          match f1.file_filenames, f2.file_filenames with
                            n1 :: _ , n2 :: _ -> n1 <= n2
                          | _ -> true)
                  | ByDone -> (fun f1 f2 -> 
                          f1.file_downloaded >= f2.file_downloaded)
                  | ByPercent -> (fun f1 f2 ->
                          percent f1 >= percent f2)
                  | _ -> raise Not_found
                in
                Sort.list sorter !!files
              with _ -> !!files
            in
            simple_print_file_list false buf list format;
            Printf.bprintf  buf "\nDownloaded %d files\n" 
              (List.length !!done_files);
            if !!done_files = [] then "" else begin
                simple_print_file_list true buf !!done_files format;
                "Use 'commit' to move downloaded files to the incoming directory"
              end
    
    ), "<num>: view file info";
    
    "add_url", Arg_two (fun kind url bud _ ->
        let v = (kind, 1, url) in
        if not (List.mem v !!web_infos) then
          web_infos =:=  v :: !!web_infos;
        load_url kind url;
        "url added to web_infos. downloading now"
    ), " <kind> <url> : load this file from the web. 
    kind is either server.met (if the downloaded file is a server.met)";
    
    "recover_temp", Arg_none (fun buf _ ->
        let files = Unix2.list_directory !!temp_directory in
        List.iter (fun filename ->
            if String.length filename = 32 then
              try
                let md4 = Md4.of_string filename in
                try
                  ignore (Hashtbl.find files_by_md4 md4)
                with Not_found ->
                    let size = Unix32.getsize32 (Filename.concat 
                          !!temp_directory filename) in
                    query_download [] size md4 None None None
              with e ->
                  Printf.printf "exception %s in recover_temp"
                    (Printexc.to_string e); print_newline ();
        ) files;
        "done"
    ), " : recover lost files from temp directory";
    
    "reshare", Arg_none (fun buf _ ->
        check_shared_files ();
        "check done"
    ), " : check shared files for removal";
    
    "vm", Arg_none (fun buf _ ->
        Printf.bprintf  buf "Connected to %d servers\n" (List.length !connected_server_list);
        List.iter (fun s ->
            Printf.bprintf buf "[%-5d] %s:%-5d  "
              s.server_num
              (Ip.to_string s.server_ip) s.server_port;
            List.iter (fun t ->
                Printf.bprintf buf "%-3s "
                  (match t.tag_value with
                    String s -> s
                  | Uint32 i -> Int32.to_string i
                  | Fint32 i -> Int32.to_string i
                  | _ -> "???"
                )
            ) s.server_tags;
            Printf.bprintf buf " %6d %7d" s.server_nusers s.server_nfiles;
            Buffer.add_char buf '\n'
        ) !connected_server_list;
        ""), ": list connected servers";
    
    "vma", Arg_none (fun buf _ ->
        let list = DownloadServers.all_servers ()
        
        in        
        Printf.bprintf  buf "Servers: %d known\n" (List.length list);
        List.iter (fun s ->
            Printf.bprintf buf "[%-5d] %s:%-5d  "
              s.server_num
              (Ip.to_string s.server_ip) s.server_port;
            List.iter (fun t ->
                Printf.bprintf buf "%-3s "
                  (match t.tag_value with
                    String s -> s
                  | Uint32 i -> Int32.to_string i
                  | Fint32 i -> Int32.to_string i
                  | _ -> "???"
                )
            ) s.server_tags;
            (match s.server_sock with
                None -> ()
              | Some _ ->
                  Printf.bprintf buf " %6d %7d" s.server_nusers s.server_nfiles);
            Buffer.add_char buf '\n'
        ) list; ""), ": list all known servers";
    
    "q", Arg_none (fun buf _ ->
        raise CommandCloseSocket
    ), ": close telnet";
    
    "bs", Arg_multiple (fun args buf _ ->
        List.iter (fun arg ->
            let ip = Ip.of_string arg in
            server_black_list =:=  ip :: !!server_black_list;
        ) args;
        "done"
    ), " <ip1> <ip2> ... : add these IPs to the servers black list";
    
    "gen_bug", Arg_one (fun arg buf _ ->
        let n = int_of_string arg in
        match !!files with
          [] -> "Couldn't create the bug (no file to download)"
        | file :: _ ->
            match file.file_known_locations with
              [] -> "Couldn't create the bug (no source to add)"
            | c :: _ ->
                match c.client_kind with
                  Known_location (ip, port) ->
                    for i = 1 to n do
                      let c = new_client (Known_location (ip, port+i)) in
                      file.file_known_locations <- 
                        c :: file.file_known_locations
                    done;
                    file.file_new_locations <- true;
                    Printf.sprintf "%d sources added" n
                | _ -> "Couldn't create the bug (source is indirect)"
    ), " : create a bug by adding 10000 new sources to a file";
    
    "kill", Arg_none (fun buf _ ->
        exit_properly ();
        "exit"), ": save and kill the server";
    
    "save", Arg_none (fun buf _ ->
        force_save_options ();
        "saved"), ": save";
    
    "dllink", Arg_multiple (fun args buf _ ->        
        let url = String2.unsplit args ' ' in
        match String2.split (String.escaped url) '|' with
          "ed2k://" :: "file" :: name :: size :: md4 :: _
        |              "file" :: name :: size :: md4 :: _ ->
            query_download [name] (Int32.of_string size)
            (Md4.of_string md4) None None None;
            "download started"
        | _ -> "bad syntax"    
    ), " <ed2klink> : download ed2k:// link";
    
    "force_download", Arg_none (fun buf _ ->
        match !aborted_download with
          None -> "No download to force"
        | Some (filenames,size,md4,location,old_file,absents) ->
            really_query_download filenames size md4 location old_file absents;
            "download started"
    ), " : force download of an already downloaded file";
    
    "d", Arg_multiple (fun args buf _ ->
        try
          let (size, md4, name) =
            match args with
            | [size; md4] -> (Int32.of_string size),(Md4.of_string md4), None
            | [size; md4; name] -> 
                (Int32.of_string size),(Md4.of_string md4), Some name
            | [num] -> 
                List.assoc (int_of_string num) !last_search
            | _ -> failwith "Bad number of arguments"
          in
          query_download [] size md4 None None None;
          "download started"
        with 
          Already_done -> "already done"
        | Not_found ->  "not found"
        | Failure s -> s
    ), "<size> <md4> : download this file";
    
    "upstats", Arg_none (fun buf _ ->
        Printf.bprintf buf "Upload statistics:\n";
        Printf.bprintf buf "Total: %d blocks uploaded\n" !upload_counter;
        let list = ref [] in
        Hashtbl.iter (fun _ file ->
            if file.file_shared then 
              list := file :: !list
        ) files_by_md4;
        let list = Sort.list (fun f1 f2 ->
              f1.file_upload_requests >= f2.file_upload_requests)
          
          !list in
        List.iter (fun file ->
            Printf.bprintf buf "%-50s requests: %8d blocs: %8d\n"
              (first_name file) file.file_upload_requests
              file.file_upload_blocks;
        ) list;
        "done"
    ), " : statistics on upload";
    
    "port", Arg_one (fun arg buf _ ->
        port =:= int_of_string arg;
        "new port will change at next restart"),
    " <port> : change connection port";
    
    "vo", Arg_none (fun buf format ->
        if format.conn_output = HTML then
          Printf.bprintf  buf "\<table border=0\>";
        List.iter (fun (name, value) ->
            if format.conn_output = HTML then
              if String.contains value '\n' then
                Printf.bprintf buf "
              \<tr\>\<td\>\<form action=/submit target=status\> 
\<input type=hidden name=setoption value=q\>
\<input type=hidden name=option value=%s\> %s \</td\>\<td\>
                \<textarea name=value rows=10 cols=70 wrap=virtual\> 
                %s
                \</textarea\>
\<input type=submit value=Modify\>
\</td\>\</tr\>
\</form\>
                " name name value
              else
              Printf.bprintf buf "
              \<tr\>\<td\>\<form action=/submit target=status\> 
\<input type=hidden name=setoption value=q\>
\<input type=hidden name=option value=%s\> %s \</td\>\<td\>
              \<input type=text name=value size=40 value=\\\"%s\\\"\>
\</td\>\</tr\>
\</form\>
" name name value
            else
              Printf.bprintf buf "%s = %s\n" name value)
        (Options.simple_options downloads_ini);
        if format.conn_output = HTML then
          Printf.bprintf  buf "\</table\>";
        
        ""
    ), " : print options";
    
    "set", Arg_two (fun name value buf _ ->
        try
          Options.set_simple_option downloads_ini name value;
          Printf.sprintf "option %s value changed" name
        with e ->
            Printf.sprintf "Error %s" (Printexc.to_string e)
    ), " <option_name> <option_value> : change option value";
    
    "vr", Arg_multiple (fun args buf output ->
        match args with
          num :: _ -> 
            let num = int_of_string num in
            List.iter (fun s ->
                if s.search_num = num then
                  print_search buf s output
            ) !searches;
            ""
        | [] ->   
            match !searches with 
              s :: _ ->
                print_search buf s output;
                ""
            | _ -> 
                "no searches done\n"
    ), "  [<num>]: view results of a search";
    
    "forget", Arg_one (fun num buf _ ->
        let num = int_of_string num in
        forget_search num;
        ""  
    ), " <num> : forget search <num>";
    
    "ls", Arg_multiple (fun args buf _ ->
        let query = search_of_args args in
        let search = new_search query in
        searches := search :: !searches;
        DownloadIndexer.find search;
        "local search started"
    ), " <query> : local search";
    
    "s", Arg_multiple (fun args buf _ ->
        let query = search_of_args args in
        start_search query buf;
        ""
    ), " <query> : search for files\n
\tWith special args:
\t-minsize <size>
\t-maxsize <size>
\t-media <Video|Audio|...>
\t-Video
\t-Audio
\t-format <format>
\t-title <word in title>
\t-album <word in album>
\t-artist <word in artist>
\t-field <field> <fieldvalue>
\t-not <word>
\t-and <word> 
\t-or <word> :

";

    "remove_old_servers", Arg_none (fun buf format ->
        DownloadServers.remove_old_servers ();
        "clean done"
    ), ": remove servers that have not been connected for several days";
    
    "vs", Arg_none (fun buf format ->
        Printf.bprintf  buf "Searching %d queries\n" (List.length !searches);
        List.iter (fun s ->
            Printf.bprintf buf "%s[%-5d]%s %s %s\n" 
              (if format.conn_output = HTML then 
                Printf.sprintf "\<a href=/submit\?q=vr\+%d\>" s.search_num
              else "")
            s.search_num 
              (if format.conn_output = HTML then "\</a\>" else "")
            s.search_string
              (if s.search_waiting = 0 then "done" else
                string_of_int s.search_waiting)
        ) !searches; ""), ": view all queries";

    "view_custom_queries", Arg_none (fun buf format ->
        if format.conn_output <> HTML then
          Printf.bprintf buf "%d custom queries defined\n" 
            (List.length !!customized_queries);
        List.iter (fun (name, q) ->
            if format.conn_output = HTML then
              Printf.bprintf buf 
                "\<a href=/submit\?custom=%s target=output\> %s \</a\>\n" 
              (Url.encode name) name
            else
              Printf.bprintf buf "[%s]\n" name
        ) !! customized_queries; ""
    ), ": view custom queries";
    
    "cancel", Arg_multiple (fun args buf _ ->
        if args = ["all"] then
          List.iter (fun file ->
              remove_file file.file_md4
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if file.file_num = num then remove_file file.file_md4
              ) !!files) args; 
        ""
    ), " <num> : cancel download (use arg 'all' for all files)";
    
    "pause", Arg_multiple (fun args buf _ ->
        if args = ["all"] then
          List.iter (fun file ->
              file.file_state <- FilePaused;
              file.file_changed <- FileInfoChange;
              !file_change_hook file
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if file.file_num = num then begin
                      file.file_state <- FilePaused;
                      file.file_changed <- FileInfoChange;
                      !file_change_hook file
                    end
              ) !!files) args; ""
    ), " <num> : pause a download (use arg 'all' for all files)";
    
    "resume", Arg_multiple (fun args buf _ ->
        if args = ["all"] then
          List.iter (fun file ->
              file.file_state <- FileDownloading;
              reconnect_all file;
              file.file_changed <- FileInfoChange;
              !file_change_hook file            
          ) !!files
        else
        List.iter (fun num ->
            let num = int_of_string num in
            List.iter (fun file ->
                if file.file_num = num then begin
                    file.file_state <- FileDownloading;
                    reconnect_all file;
                    file.file_changed <- FileInfoChange;
                    !file_change_hook file
                  end
            ) !!files) args; ""
    ), " <num> : resume a paused download (use arg 'all' for all files)";

    "xs", Arg_none (fun buf _ ->
        if !last_xs >= 0 then begin
            try
              let ss = DownloadFiles.find_search !last_xs in
              make_xs ss;
              "extended search done"
            with e -> Printf.sprintf "Error %s" (Printexc.to_string e)
          end else "No previous extended search"),
    ": extended search";

    "clh", Arg_none (fun buf _ ->
        DownloadIndexer.clear ();
        "local history cleared"
    ), " : clear local history";
    
    "c", Arg_multiple (fun args buf _ ->
        match args with
          [] ->
            force_check_server_connections true;
            "connecting more servers"
        | _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = Hashtbl.find servers_by_num num in
                connect_server s
            ) args;
            "connecting server"
    ),
    " [<num>]: connect to more servers (or to server <num>)";
    
      
  ]

let eval auth buf cmd options =
  let l = String2.tokens cmd in
  match l with
    [] -> ()
  | cmd :: args ->
      if cmd = "help" || cmd = "?" then begin
          Printf.bprintf  buf "Available commands are:\n";
          List.iter (fun (cmd, _, help) ->
              Printf.bprintf  buf "%s %s\n" cmd help) commands
        end else
      if cmd = "q" then
        raise CommandCloseSocket
      else
      if cmd = "auth" then
        let arg_password =
          match args with
            [] -> ""
          | s1 :: _ -> s1
        in
        if !!password = arg_password then begin
            auth := true;
            Printf.bprintf buf "Full access enabled"
          end else
          Printf.bprintf buf "Bad login/password"
      else
      if !auth then
        execute_command commands buf options cmd args      
      else
          Printf.bprintf buf "Command not authorized\n Use 'auth <password>' before."

(* The telnet client *)
        
let buf = Buffer.create 1000

let user_reader options auth sock nread  = 
  let b = TcpClientSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  for i = new_pos to end_pos - 1 do
    let c = b.buf.[i] in
    if c = '\n' || c = '\r' || c = '\000' then 
      let len = i - b.pos in
      let cmd = String.sub b.buf b.pos len in
      buf_used sock (len+1);
      try
        Buffer.clear buf;
        eval auth buf cmd options;
        Buffer.add_char buf '\n';
        TcpClientSocket.write_string sock (Buffer.contents buf)
      with
        CommandCloseSocket ->
          (try
              shutdown sock "user quit";
          with _ -> ());
      | e ->
          TcpClientSocket.write_string sock
            (Printf.sprintf "exception [%s]\n" (Printexc.to_string e));
          
  done
  
let user_closed sock  msg =
  user_socks := List2.removeq sock !user_socks;
  ()
  
let telnet_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        let sock = TcpClientSocket.create_simple s in
        let auth = ref (!!password = "") in
        let options = {
            conn_output = TEXT;
            conn_sortvd = NotSorted;
          } in
        TcpClientSocket.set_reader sock (user_reader options auth);
        TcpClientSocket.set_closer sock user_closed;
        user_socks := sock :: !user_socks;
        TcpClientSocket.write_string sock "\nWelcome on mldonkey command-line\n";
        TcpClientSocket.write_string sock "\nUse ? for help\n\n";
      else 
        Unix.close s

  | _ -> ()

(* The HTTP client *)

let buf = Buffer.create 1000
      
open Http_server

let add_submit_entry buf = ()
  
let add_simple_commands buf =
  Buffer.add_string buf !!web_header
  

let html_open_page buf =
  Buffer.add_string buf "<HTML>\n";
  Buffer.add_string buf "<HEAD>\n";
  Buffer.add_string buf "<TITLE>\n";
  Buffer.add_string buf "MLdonkey WEB Interface\n";
  Buffer.add_string buf "</TITLE>\n";
  Buffer.add_string buf "</HEAD>\n";
  Buffer.add_string buf "<BODY>\n";
  ()
  
let html_close_page buf =
  Buffer.add_string buf "</BODY>\n";  
  Buffer.add_string buf "</HTML>\n";
  ()
  
let http_handler options t r =
  Buffer.clear buf;  
  if (!!http_password <> "" || !!http_login <> "") &&
    (r.options.passwd <> !!http_password || r.options.login <> !!http_login)
  then begin
      need_auth buf "MLdonkey"
    end
  else
    begin
      Buffer.add_string  buf "HTTP/1.0 200 OK\r\n";
      Buffer.add_string  buf "Pragma: no-cache\r\n";
      Buffer.add_string  buf "Server: MLdonkey\r\n";
      Buffer.add_string  buf "Connection: close\r\n";
      Buffer.add_string  buf "Content-Type: text/html; charset=iso-8859-1\r\n";
      Buffer.add_string  buf "\r\n";
      
      html_open_page buf;
      if not !!use_html_frames then begin
          add_simple_commands buf;
          add_submit_entry buf;
        end;
      try
        match r.get_url.Url.file with
        | "/commands.html" ->
            Buffer.add_string buf !!web_commands_frame
        | "/" | "/index.html" -> 
            if !!use_html_frames then begin
                Buffer.clear buf;
                Printf.bprintf buf 
                  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
            \"http://www.w3.org/TR/html4/frameset.dtd\">";
                
                Buffer.add_string buf "<HTML>\n";
                Buffer.add_string buf "<HEAD>\n";
                Buffer.add_string buf "<TITLE>\n";
                Buffer.add_string buf "MLdonkey WEB Interface\n";
                Buffer.add_string buf "</TITLE>\n";
                Buffer.add_string buf "</HEAD>\n";
                Printf.bprintf buf "
            <frameset src=\"index\" rows=\"%d,2*\">
               <frameset src=\"index\" cols=\"5*,1*\">
                  <frame name=\"commands\" src=\"/commands.html\">
                  <frame name=\"status\" src=\"/noframe.html\">
               </frameset>
               <frame name=\"output\" src=\"/oneframe.html\">
            </frameset>" !!commands_frame_height             
              end
              
        | "/complex_search.html" ->
            complex_search buf
        | "/noframe.html" ->
            Buffer.clear buf;
            html_open_page buf;

        | "/oneframe.html" -> ()

        | "/submit" ->
            begin
              match r.get_url.Url.args with
              | [ "q", "download"; "md4", md4_string; "size", size_string ] ->
                  if !!use_html_frames then
                    begin
                      Buffer.clear buf;
                      html_open_page buf;
                    end;
                  query_download [] (Int32.of_string size_string)
                  (Md4.of_string md4_string) None None None;
                  Printf.bprintf buf  "\n<pre>\nDownload started\n</pre>\n";
                  
              | ("q", cmd) :: other_args ->
                  List.iter (fun arg ->
                      match arg with
                      | "sortby", "size" -> options.conn_sortvd <- BySize
                      | "sortby", "name" -> options.conn_sortvd <- ByName
                      | "sortby", "rate" -> options.conn_sortvd <- ByRate
                      | "sortby", "done" -> options.conn_sortvd <- ByDone
                      | "sortby", "percent" -> options.conn_sortvd <- ByPercent
                      | _ -> ()
                  ) other_args;
                  let s = 
                    let b = Buffer.create 10000 in
                    eval (ref true) b cmd options;
                    html_escaped (Buffer.contents b)
                  in
                  Printf.bprintf buf  "\n<pre>\n%s\n</pre>\n" s;

              | [ ("custom", query) ] ->
                  
                  custom_query buf query
                  
              | ("custom", query) :: args ->
                  send_custom_query buf query args
  
              | [ "setoption", _ ; "option", name; "value", value ] ->
                  Options.set_simple_option downloads_ini name value;
                  Buffer.add_string buf "Option value changed"
                  
              | args -> 
                  List.iter (fun (s,v) ->
                      Printf.printf "[%s]=[%s]" (String.escaped s) (String.escaped v);
                      print_newline ()) args;
                  
                  raise Not_found
            end
        | cmd -> 
            Printf.bprintf buf "No page named %s" cmd
      with e ->
          Printf.bprintf buf "\nException %s\n" (Printexc.to_string e);
    end;
  
  html_close_page buf;
  let s = Buffer.contents buf in
  let len = String.length s in
(*  TcpClientSocket.set_monitored t; *)
  TcpClientSocket.set_max_write_buffer t (len + 100);
  
  TcpClientSocket.write t s 0 len;
  TcpClientSocket.close_after_write t
        
let http_options = { 
    conn_output = HTML;
    conn_sortvd = NotSorted;
  }      
  
let create_http_handler () = 
  create {
    port = !!http_port;
    requests = [];
    addrs = !!allowed_ips;
    base_ref = "";
    default = http_handler http_options;
  }
