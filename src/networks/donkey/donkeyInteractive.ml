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

open Printf2
open Md4

open CommonShared
open CommonServer
open CommonResult
open CommonClient
open CommonUser
open CommonInteractive
open CommonNetwork
open GuiTypes
open CommonTypes
open CommonComplexOptions
open CommonFile
open DonkeySearch
open Options
open DonkeyMftp
open DonkeyProtoCom
open DonkeyServers
open BasicSocket
open TcpBufferedSocket
open DonkeyOneFile
open DonkeyFiles
open DonkeyComplexOptions
open DonkeyTypes
open DonkeyOptions
open DonkeyGlobals
open DonkeyClient
open CommonGlobals
open CommonOptions
open DonkeyStats
  
let result_name r =
  match r.result_names with
    [] -> None
  | name :: _ -> Some name

      
let reconnect_all file =
  DonkeyOvernet.recover_file file;
  
(* This is expensive, no ? *)
  DonkeySources.reschedule_sources file;
  List.iter (fun s ->
      match s.server_sock, server_state s with
      | Some sock, (Connected _ | Connected_downloading) ->
          s.server_waiting_queries <- file :: s.server_waiting_queries
      | _ -> ()
  ) (connected_servers())
    
let forget_search s =  
  if !xs_last_search = s.search_num then begin
      xs_last_search := (-1);
      xs_servers_list := [];
    end
  
  
let load_server_met filename =
  try
    let module S = DonkeyImport.Server in
    let s = File.to_string filename in
    let ss = S.read s in
    List.iter (fun r ->
        try
          let server = check_add_server r.S.ip r.S.port in
          List.iter (fun tag ->
              match tag with
                { tag_name = "name"; tag_value = String s } -> 
                  server.server_name <- s;
              |  { tag_name = "description" ; tag_value = String s } ->
                  server.server_description <- s
              | _ -> ()
          ) r.S.tags
        with _ -> ()
    ) ss;
    List.length ss
  with e ->
      lprintf "Exception %s while loading %s" (Printexc2.to_string e)
      filename;
      lprint_newline ();
      0

let already_done = Failure "File already downloaded"
      
let really_query_download filenames size md4 location old_file absents =
  begin
    try
      let file = Hashtbl.find files_by_md4 md4 in
      if file_state file = FileDownloaded then 
        raise already_done;
    with Not_found -> ()
  end;
  
  List.iter (fun file -> 
      if file.file_md4 = md4 then raise already_done) 
  !current_files;

  let temp_file = Filename.concat !!temp_directory (Md4.to_string md4) in
  begin
    match old_file with
      None -> ()
    | Some filename ->
        if Sys.file_exists filename && not (
            Unix32.file_exists temp_file) then
          (try 
              if !verbose then begin
                  lprintf "Renaming from %s to %s" filename
                    temp_file; lprint_newline ();
                end;
              Unix2.rename filename temp_file;
              Unix.chmod temp_file 0o644;
              with e -> 
                lprintf "Could not rename %s to %s: exception %s"
                  filename temp_file (Printexc2.to_string e);
                lprint_newline () );        
  end;
  
  let file = new_file FileDownloading temp_file md4 size true in
  begin
    match absents with
      None -> ()
    | Some absents -> 
        let absents = Sort.list (fun (p1,_) (p2,_) -> p1 <= p2) absents in
        file.file_absent_chunks <- absents;
  end;
  
  let other_names = DonkeyIndexer.find_names md4 in
  let filenames = List.fold_left (fun names name ->
        if List.mem name names then names else name :: names
    ) filenames other_names in 
  file.file_filenames <- file.file_filenames @ filenames ;
  update_best_name file;

  DonkeyOvernet.recover_file file;
  
  current_files := file :: !current_files;
(*  !file_change_hook file; *)
  set_file_size file (file_size file);
  List.iter (fun s ->
      match s.server_sock with
        None -> () (* assert false !!! *)
      | Some sock ->
          query_location file sock
  ) (connected_servers());

  (try
      let servers = Hashtbl.find_all udp_servers_replies file.file_md4 in
      List.iter (fun s ->
          udp_server_send s (DonkeyProtoUdp.QueryLocationUdpReq file.file_md4)
      ) servers
    with _ -> ());
  
  (match location with
      None -> ()
    | Some num ->
        let c = client_find num in
        client_connect c
        (*
        try 
          let c = find_client num in
          (match c.client_kind with
              Indirect_location -> 
                if not (Intmap.mem c.client_num file.file_indirect_locations) then
                  file.file_indirect_locations <- Intmap.add c.client_num c
                    file.file_indirect_locations
            
            | _ -> 
                if not (Intmap.mem c.client_num file.file_known_locations) then
                  new_known_location file c
          );
          if not (List.memq file c.client_files) then
            c.client_files <- file :: c.client_files;
          match client_state c with
            NotConnected -> 
              connect_client !!client_ip [file] c
          | Connected_busy | Connected_idle | Connected_queued ->
              begin
                match c.client_sock with
                  None -> ()
                | Some sock -> 
                    DonkeyClient.query_files c sock [file]
              end
          | _ -> ()
with _ -> ()
*)
  )
        
let query_download filenames size md4 location old_file absents force =
  if not force then
    List.iter (fun m -> 
        if m = md4 then begin
            let r = try
                DonkeyIndexer.find_result md4
              with _ -> 
(* OK, we temporary create a result corresponding to the
file that should have been download, but has already been downloaded *)
                  
                  
                  let r = { 
                      result_num = 0;
                      result_network = network.network_num;
                      result_md4 = md4;
                      result_names = filenames;
                      result_size = size;
                      result_tags = [];
                      result_type = "";
                      result_format = "";
                      result_comment = "";
                      result_done = false;
                    } in
                  DonkeyIndexer.index_result_no_filter r 
            in
            aborted_download := Some (result_num (as_result r.result_result));
            raise already_done
          end) 
    !!old_files;
  really_query_download filenames size md4 location old_file absents

let result_download rs filenames force =
  let r = Store.get store rs.result_index in
  query_download filenames r.result_size r.result_md4 None None None force
  
let load_prefs filename = 
  try
    let module P = DonkeyImport.Pref in
    let s = File.to_string filename in
    let t = P.read s in
    t.P.client_tags, t.P.option_tags
  with e ->
      lprintf "Exception %s while loading %s" (Printexc2.to_string e)
      filename;
      lprint_newline ();
      [], []
      
let import_temp temp_dir =  
  let list = Unix2.list_directory temp_dir in
  let module P = DonkeyImport.Part in
  List.iter (fun filename ->
      try
        if Filename2.last_extension filename = ".part" then
          let filename = Filename.concat temp_dir filename in
          let met = filename ^ ".met" in
          if Sys.file_exists met then
            let s = File.to_string met in
            let f = P.read s in
            let filenames = ref [] in
            let size = ref Int64.zero in
            List.iter (fun tag ->
                match tag with
                  { tag_name = "filename"; tag_value = String s } ->
                    lprintf "Import Donkey %s" s; 
                    lprint_newline ();
                    
                    filenames := s :: !filenames;
                | { tag_name = "size"; tag_value = Uint64 v } ->
                    size := v
                | _ -> ()
            ) f.P.tags;
            query_download !filenames !size f.P.md4 None 
              (Some filename) (Some (List.rev f.P.absents)) true;
      
      with _ -> ()
  ) list
  
  
let import_config dirname =
  ignore (load_server_met (Filename.concat dirname "server.met"));
  let ct, ot = load_prefs (Filename.concat dirname "pref.met") in
  let temp_dir = ref (Filename.concat dirname "temp") in

  List.iter (fun tag ->
      match tag with
      | { tag_name = "name"; tag_value = String s } ->
          client_name =:=  s
      | { tag_name = "port"; tag_value = Uint64 v } ->
          port =:=  Int64.to_int v
      | _ -> ()
  ) ct;

  List.iter (fun tag ->
      match tag with
      | { tag_name = "temp"; tag_value = String s } ->
          if Sys.file_exists s then (* be careful on that *)
            temp_dir := s
          else (lprintf "Bad temp directory, using default";
              lprint_newline ();)
      | _ -> ()
  ) ot;

  import_temp !temp_dir
  
let broadcast msg =
  let s = msg ^ "\n" in
  let len = String.length s in
  List.iter (fun sock ->
      TcpBufferedSocket.write sock s 0 len
  ) !user_socks

  (*
let saved_name file =
  let name = longest_name file in
(*  if !!use_mp3_tags then
    match file.file_format with
      Mp3 tags ->
        let module T = Mp3tag in
        let name = match name.[0] with
            '0' .. '9' -> name
          | _ -> Printf.sprintf "%02d-%s" tags.T.tracknum name
        in
        let name = if tags.T.album <> "" then
            Printf.sprintf "%s/%s" tags.T.album name
          else name in
        let name = if tags.T.artist <> "" then
            Printf.sprintf "%s/%s" tags.T.artist name
          else name in
        name          
    | _ -> name
else *)
  name
    *)

let print_file buf file =
  Printf.bprintf buf "[%-5d] %s %10Ld %32s %s" 
    (file_num file)
    (file_best_name file)
  (file_size file)
  (Md4.to_string file.file_md4)
  (if file_state file = FileDownloaded then
      "Done" else
      Int64.to_string (file_downloaded file));
  Buffer.add_char buf '\n';
  Printf.bprintf buf "Connected clients:\n";
  let f _ c =
    match c.client_kind with
      Known_location (ip, port) ->
        Printf.bprintf  buf "[%-5d] %12s %-5d    %s\n"
          (client_num c)
          (Ip.to_string ip)
        port
          (match c.client_sock with
            None -> string_of_date (connection_last_conn
                  c.client_connection_control)
          | Some _ -> "Connected")
    | _ ->
        Printf.bprintf  buf "[%-5d] %12s            %s\n"
          (client_num c)
          "Indirect"
          (match c.client_sock with
            None -> string_of_date (connection_last_conn
                  c.client_connection_control)
          | Some _ -> "Connected")
  in

  (* Intmap.iter f file.file_sources; *)
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

let recover_md4s md4 =
  let file = find_file md4 in  
  if file.file_chunks <> [||] then
    for i = 0 to file.file_nchunks - 1 do
      file.file_chunks.(i) <- (match file.file_chunks.(i) with
          PresentVerified -> PresentTemp
        | AbsentVerified -> AbsentTemp
        | PartialVerified x -> PartialTemp x
        | x -> x)
    done
    

    
let parse_donkey_url url =
  match String2.split (String.escaped url) '|' with
  | "ed2k://" :: "file" :: name :: size :: md4 :: _
  | "file" :: name :: size :: md4 :: _ ->
      let md4 = if String.length md4 > 32 then
          String.sub md4 0 32 else md4 in
      query_download [name] (Int64.of_string size)
      (Md4.of_string md4) None None None false;
      true
  | "ed2k://" :: "server" :: ip :: port :: _
  | "server" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let s = add_server ip (int_of_string port) in
      server_connect (as_server s.server_server);
      true 
  | "ed2k://" :: "friend" :: ip :: port :: _
  | "friend" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let port = int_of_string port in
      let c = new_client (Known_location (ip,port)) in
      friend_add c;
      true
  
  | _ ->  false
      
let commands = [
    "n", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4661"
          | _ -> failwith "n <ip> [<port>]: bad argument number"
        in
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        
        let s = add_server ip port in
        Printf.bprintf buf "New server %s:%d\n" 
          (Ip.to_string ip) port;
        ""
    ), "<ip> [<port>] :\t\t\tadd a server";
    
    "afr", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4662"
          | _ -> failwith "afr <ip> [<port>]: bad argument number"
        in
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        let c = new_client (Known_location (ip,port)) in
        friend_add c;
        "friend added";
    ),  "<ip> [<port>] :\t\t\tadd a friend";
    
    "vu", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.sprintf "Upload credits : %d minutes\nUpload disabled for %d
minutes" !CommonUploads.upload_credit !CommonUploads.has_upload;
    
    ), ":\t\t\t\t\tview upload credits";
    
    
    "comments", Arg_one (fun filename o ->
        let buf = o.conn_buf in
        DonkeyIndexer.load_comments filename;
        DonkeyIndexer.save_comments ();
        "comments loaded and saved"
    ), "<filename> :\t\t\tload comments from file";
    
    "comment", Arg_two (fun md4 comment o ->
        let buf = o.conn_buf in
        let md4 = Md4.of_string md4 in
        DonkeyIndexer.add_comment md4 comment;
        "Comment added"
    ), "<md4> \"<comment>\" :\t\tadd comment on a md4";
        
    "import", Arg_one (fun dirname o ->
        let buf = o.conn_buf in
        try
          import_config dirname;
          "config loaded"
        with e ->
            Printf.sprintf "error %s while loading config" (
              Printexc2.to_string e)
    ), "<dirname> :\t\t\timport the config from dirname";
    
    "import_temp", Arg_one (fun dirname o ->
        let buf = o.conn_buf in
        try
          import_temp dirname;
          "temp files loaded"
        with e ->
            Printf.sprintf "error %s while loading temp files" (
              Printexc2.to_string e)
    ), "<temp_dir> :\t\timport the old edonkey temp directory";
    
    "load_old_history", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyIndexer.load_old_history ();
        "Old history loaded"
    ), ":\t\t\tload history.dat file";
    
    "servers", Arg_one (fun filename o ->
        let buf = o.conn_buf in
        try
          let n = load_server_met filename in
          Printf.sprintf "%d servers loaded" n
        with e -> 
            Printf.sprintf "error %s while loading file" (Printexc2.to_string e)
    ), "<filename> :\t\t\tadd the servers from a server.met file";
    
    
    "id", Arg_none (fun o ->
        let buf = o.conn_buf in
        List.iter (fun s ->
            Printf.bprintf buf "For %s:%d  --->   %s\n"
              (Ip.to_string s.server_ip) s.server_port
              (match s.server_cid with
                None -> "waiting"
              | Some ip ->
                  if Ip.valid ip then
                    Ip.to_string ip
                  else
                    Int64.to_string (Ip.to_int64 (Ip.rev ip)))
        ) (connected_servers());
        ""
    ), ":\t\t\t\t\tprint ID on connected servers";
    
    "bs", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let ip = Ip.of_string arg in
            server_black_list =:=  ip :: !!server_black_list;
        ) args;
        "done"
    ), "<ip1> <ip2> ... :\t\t\tadd these IPs to the servers black list";
    
    "port", Arg_one (fun arg o ->
        port =:= int_of_string arg;
        "new port will change at next restart"),
    "<port> :\t\t\t\tchange connection port";
    
    "add_url", Arg_two (fun kind url o ->
        let buf = o.conn_buf in
        let v = (kind, 1, url) in
        if not (List.mem v !!web_infos) then
          web_infos =:=  v :: !!web_infos;
        load_url kind url;
        "url added to web_infos. downloading now"
    ), "<kind> <url> :\t\t\tload this file from the web.
\t\t\t\t\tkind is either server.met (if the downloaded file is a server.met)";
    
    "scan_temp", Arg_none (fun o ->
        let buf = o.conn_buf in
        let list = Unix2.list_directory !!temp_directory in
        
        let counter = ref 0 in
        let tr = ref "dl-1" in
        
        if use_html_mods o then
          
          html_mods_table_header buf "scan_tempTable" "scan_temp" [ 
            ( "0", "srh", "Filename", "Filename" ) ; 
            ( "0", "srh", "Status", "Status" ) ;
            ( "0", "srh", "MD4", "MD4" ) ];
        
        
        List.iter (fun filename ->
            incr counter;
            if (!counter mod 2 == 0) then tr := "dl-1"
            else tr := "dl-2";
            try
              let md4 = Md4.of_string filename in
              try
                let file = find_file md4 in
                if use_html_mods o then
                  Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td
                class=\\\"sr\\\"\\>\\<A HREF=\\\"%s\\\"\\>%s\\</A\\>\\</td\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\> 
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" 
                    !tr (file_comment (as_file file.file_file)) (file_best_name file) "Downloading" filename
                else
                  Printf.bprintf buf "%s is %s %s\n" filename
                    (file_best_name file)
                  "(downloading)" 
              with _ ->
                  if use_html_mods o then
                    Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\> 
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" !tr 
                      (try
                        let names = DonkeyIndexer.find_names md4 in
                        List.hd names
                      with _ -> "Never Seen") 
                    (if List.mem md4 !!old_files then
                        "Old file" else "Unknown")
                    filename
                  else
                    Printf.bprintf buf "%s %s %s\n"
                      filename
                      (if List.mem md4 !!old_files then
                        "is an old file" else "is unknown")
                    (try
                        let names = DonkeyIndexer.find_names md4 in
                        List.hd names
                      with _ -> "and never seen")
            
            with _ -> 
                if use_html_mods o then
                  Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>Unknown\\</td\\>
                \\<td class=\\\"sr \\\"\\>\\</td\\> 
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" !tr filename 
                else
                  Printf.bprintf buf "%s unknown\n" filename
        
        ) list;
        
        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
        
        "done";
    ), ":\t\t\t\tprint temp directory content";
    
    "recover_temp", Arg_none (fun o ->
        let buf = o.conn_buf in
        let files = Unix2.list_directory !!temp_directory in
        List.iter (fun filename ->
            if String.length filename = 32 then
              try
                let md4 = Md4.of_string filename in
                try
                  ignore (Hashtbl.find files_by_md4 md4)
                with Not_found ->
                    let size = Unix32.getsize64 (Filename.concat 
                          !!temp_directory filename) in
                    let names = try DonkeyIndexer.find_names md4 
                      with _ -> [] in
                    query_download names size md4 None None None true;
                    recover_md4s md4
              with e ->
                  lprintf "exception %s in recover_temp"
                    (Printexc2.to_string e); lprint_newline ();
        ) files;
        "done"
    ), ":\t\t\t\trecover lost files from temp directory";
    
    "sources", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeySources.print_sources buf;
        "done"
    ), ":\t\t\t\tshow sources currently known";
    
    "update_sources", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeySources.recompute_ready_sources ();
        "done"
    ), ":\t\t\trecompute order of connections to sources (experimental)";
    
    "xs", Arg_none (fun o ->
        let buf = o.conn_buf in
        if !xs_last_search >= 0 then begin
            try
              make_xs (CommonSearch.search_find !xs_last_search);
              "extended search started"
            with e -> Printf.sprintf "Error %s" (Printexc2.to_string e)
          end else "No previous search to extend"),
    ":\t\t\t\t\textend the last search";
    
    "clh", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyIndexer.clear ();
        "local history cleared"
    ), ":\t\t\t\t\tclear local history";
    
    "dd", Arg_two(fun size md4 o ->
        let buf = o.conn_buf in
        query_download [] (Int64.of_string size)
        (Md4.of_string md4) None None None false;
        "download started"
    
    ), "<size> <md4> :\t\t\tdownload from size and md4";
    
    "dup", Arg_none (fun _ ->
        DonkeyChunks.duplicate_chunks (); "done"),
    ":\t\t\t\t\tfind duplicate chunks (experimental)";
    
    "remove_old_servers", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyServers.remove_old_servers ();
        "clean done"
    ), ":\t\t\tremove servers that have not been connected for several days";
    
    
    
    "bp", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let port = int_of_string arg in
            port_black_list =:=  port :: !!port_black_list;
        ) args;
        "done"
    ), "<port1> <port2> ... :\t\tadd these Ports to the port black list";
    
    "send_servers", Arg_none (fun o ->
        DonkeyProtoCom.propagate_working_servers 
          (List.map (fun s -> s.server_ip, s.server_port)
          (connected_servers()))
        (DonkeyOvernet.connected_peers ())
        ;
        "done"
    ), ":\t\t\t\tsend the list of connected servers to the redirector";
  
  ]
  
let _ =
  register_commands commands;
  file_ops.op_file_resume <- (fun file ->
      reconnect_all file;
  );
  file_ops.op_file_set_priority <- (fun file _ ->
      DonkeySources.recompute_ready_sources ()     
  );
  file_ops.op_file_pause <- (fun file -> ()
  );
  file_ops.op_file_commit <- (fun file new_name ->
      if not (List.mem file.file_md4 !!old_files) then
        old_files =:= file.file_md4 :: !!old_files;
      lprintf "REMEMBER SHARE FILE INFO %s" new_name; lprint_newline (); 
      DonkeyShare.remember_shared_info file new_name
  );
  network.op_network_connected <- (fun _ ->
    !nservers > 0  
  );
  network.op_network_private_message <- (fun iddest s ->      
      try
        let c = DonkeyGlobals.find_client_by_name iddest in
        match c.client_sock with
          None -> 
            (
(* A VOIR  : est-ce que c'est bien de fait comme ça ? *)
              DonkeyClient.reconnect_client c;
              match c.client_sock with
                None ->
                  CommonChat.send_text !!CommonOptions.chat_console_id None 
                    (Printf.sprintf "client %s : could not connect (client_sock=None)" iddest)
              | Some sock ->
                  direct_client_send c (DonkeyProtoClient.SayReq s)
            )
        | Some sock ->
            direct_client_send c (DonkeyProtoClient.SayReq s)
      with
        Not_found ->
          CommonChat.send_text !!CommonOptions.chat_console_id None 
            (Printf.sprintf "client %s unknown" iddest)
  )

let _ =
  result_ops.op_result_info <- (fun rs ->
      let r = Store.get store rs.result_index in
      r.result_num <- rs.result_result.impl_result_num;
      r
  )

module P = GuiTypes

let _ =
  file_ops.op_file_info <- (fun file ->
      try
        let v = 
          {
            P.file_name = file_best_name file;
            P.file_num = (file_num file);
            P.file_network = network.network_num;
            P.file_names = file.file_filenames;
            P.file_md4 = file.file_md4;
            P.file_size = file_size file;
            P.file_downloaded = file_downloaded file;
            P.file_nlocations = 0;
            P.file_nclients = 0;
            P.file_state = file_state file;
            P.file_sources = None;
            P.file_download_rate = file_download_rate file.file_file;
            P.file_chunks = (
              let nchunks = file.file_nchunks in
              let s = String.make file.file_nchunks '0' in
              if file.file_chunks <> [||] then begin
                  for i = 0 to nchunks - 1 do
                    match file.file_chunks.(i) with
                      PresentTemp | PresentVerified -> s.[i] <- '2'
                    | PartialTemp _ | PartialVerified _ -> s.[i] <- '1'
                    | _ -> ()
                  done;
                end;
              s
            
            );          
            P.file_priority = file_priority  file;
            P.file_availability = String2.init file.file_nchunks (fun i ->

(* Could file.file_available_chunks.(i) be under 0 ? *)
                let n = maxi 0 (mini file.file_available_chunks.(i) 127) in
                char_of_int n 
            );
            P.file_format = file.file_format;
            P.file_chunks_age = file.file_chunks_age;
            P.file_age = file_age file;
            P.file_last_seen = file.file_file.impl_file_last_seen;
          } in
        v
      with e ->
          lprintf "Exception %s in op_file_info" (Printexc2.to_string e);
          lprint_newline ();
          raise e
          
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      {
        P.server_num = (server_num s);
        P.server_network = network.network_num;
        P.server_addr = new_addr_ip s.server_ip;
        P.server_port = s.server_port;
        P.server_score = s.server_score;
        P.server_tags = s.server_tags;
        P.server_nusers = s.server_nusers;
        P.server_nfiles = s.server_nfiles;
        P.server_state = server_state s;
        P.server_name = s.server_name;
        P.server_description = s.server_description;
        P.server_banner = s.server_banner;
        P.server_users = None;
      }
  )


let _ =
  user_ops.op_user_info <- (fun u ->
      {
        P.user_num = u.user_user.impl_user_num;
        P.user_md4 = u.user_md4;
        P.user_ip = u.user_ip;
        P.user_port = u.user_port;
        P.user_tags = u.user_tags;
        P.user_name = u.user_name;
        P.user_server = u.user_server.server_server.impl_server_num;
      }
  )

let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = c.client_kind;
        P.client_state = client_state c;
        P.client_type = client_type c;
        P.client_tags = c.client_tags;
        P.client_name = c.client_name;
        P.client_files = None;
        P.client_num = (client_num c);
        P.client_rating = c.client_rating;
        P.client_chat_port = c.client_chat_port ;
        P.client_connect_time = c.client_connect_time;
        P.client_software = gbrand_to_string c.client_brand;
        P.client_downloaded = c.client_downloaded;
        P.client_uploaded = c.client_uploaded;
		P.client_sock_addr = 
		(try match c.client_sock with
          Some sock -> (Ip.to_string (peer_ip sock)) 
        | None -> ""
		with _ -> "");
        P.client_upload = 
        (match c.client_upload with
            Some cu -> Some (file_best_name cu.up_file)
          | None -> None);
                        
      }
  );
  client_ops.op_client_debug <- (fun c debug ->
      c.client_debug <- debug)

let _ =
  result_ops.op_result_download <- result_download
  
let _ =
  network.op_network_connect_servers <- (fun _ ->
      force_check_server_connections true
  )
let disconnect_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
      TcpBufferedSocket.shutdown sock "user disconnect"
      
let _ =
  server_ops.op_server_remove <- (fun s ->
      DonkeyComplexOptions.remove_server s.server_ip s.server_port
  );
  server_ops.op_server_connect <- connect_server;
  server_ops.op_server_disconnect <- disconnect_server;

  server_ops.op_server_query_users <- (fun s ->
      match s.server_sock, server_state s with
        Some sock, (Connected _ | Connected_downloading) ->
          direct_server_send sock (DonkeyProtoServer.QueryUsersReq "");
          Fifo.put s.server_users_queries false
      | _ -> ()
  );
  server_ops.op_server_find_user <- (fun s user ->
      match s.server_sock, server_state s with
        Some sock, (Connected _ | Connected_downloading) ->
          direct_server_send sock (DonkeyProtoServer.QueryUsersReq user);
          Fifo.put s.server_users_queries true
      | _ -> ()      
  );
  server_ops.op_server_users <- (fun s ->
      List2.tail_map (fun u -> as_user u.user_user) s.server_users)    ;
  server_ops.op_server_cid <- (fun s -> 
      match s.server_cid with
        None -> Ip.null
      | Some ip -> ip);

  ()

let _ =
  file_ops.op_file_save_as <- (fun file name ->
      file.file_filenames <- [name];
      set_file_best_name (as_file file.file_file) name
  );
  file_ops.op_file_set_format <- (fun file format ->
      file.file_format <- format);
  file_ops.op_file_check <- (fun file ->
      DonkeyChunks.verify_chunks file);  
  file_ops.op_file_recover <- (fun file ->
      if file_state file = FileDownloading then 
        reconnect_all file);  
  file_ops.op_file_sources <- (fun file ->
      let list = ref [] in
      Intmap.iter (fun _ c -> 
          list := (as_client c.client_client) :: !list) file.file_locations;
      !list
  );
  file_ops.op_file_print_sources_html <- (fun file buf ->
      if !!source_management = 3 then DonkeySources3.print_sources_html file buf
  );
  file_ops.op_file_cancel <- (fun file ->
      Hashtbl.remove files_by_md4 file.file_md4;
      current_files := List2.removeq file !current_files;
      (try  Unix32.remove (file_fd file)  with e -> 
            lprintf "Sys.remove %s exception %s" 
            (file_disk_name file)
            (Printexc2.to_string e); lprint_newline ());
      if !!keep_cancelled_in_old_files &&
        not (List.mem file.file_md4 !!old_files) then
        old_files =:= file.file_md4 :: !!old_files;
  );
  file_ops.op_file_comment <- (fun file ->
      Printf.sprintf "ed2k://|file|%s|%Ld|%s|" 
        (file_best_name file)
      (file_size file)
      (Md4.to_string file.file_md4)
  )
  
let _ =
  network.op_network_extend_search <- (fun s e ->
      match e with
      | ExtendSearchLocally ->
          DonkeyIndexer.find s      
      | ExtendSearchRemotely ->
          make_xs s
  );
  
  network.op_network_clean_servers <- (fun _ ->
      DonkeyServers.remove_old_servers ());
  
  network.op_network_connect_servers <- (fun _ ->
      force_check_server_connections true);
  
  network.op_network_parse_url <- parse_donkey_url;
  
  network.op_network_forget_search <- forget_search

(* emule<->mldonkey disconnects during chat, and this doesn't seem to auto reconnect
when sending a message? emule or ml problem? *)
let _ =
  client_ops.op_client_say <- (fun c s ->
      match c.client_sock with
        None -> ()
      | Some sock ->
          direct_client_send c (DonkeyProtoClient.SayReq s)
  );  
  client_ops.op_client_files <- (fun c ->
      match c.client_all_files with
        None ->  []
      | Some files -> 
          List2.tail_map (fun r -> "", as_result r.result_result) files);
  client_ops.op_client_browse <- (fun c immediate ->
      lprintf "*************** should browse  ***********"; lprint_newline (); 
      match c.client_sock with
      | Some sock    ->
(*
      lprintf "****************************************";
      lprint_newline ();
      lprintf "       ASK VIEW FILES         ";
lprint_newline ();
  *)
          direct_client_send c (
            let module M = DonkeyProtoClient in
            let module C = M.ViewFiles in
            M.ViewFilesReq C.t);          
      | _ -> 
          lprintf "****************************************";
          lprint_newline ();
          lprintf "       TRYING TO CONTACT FRIEND         ";
          lprint_newline ();
          
          reconnect_client c
  );
  client_ops.op_client_connect <- (fun c ->
      match c.client_sock with
        None ->  reconnect_client c
      | _ -> ()
  );
  client_ops.op_client_clear_files <- (fun c ->
      c.client_all_files <- None;
  );
  
  client_ops.op_client_bprint <- (fun c buf ->
      Printf.bprintf buf "\t\t%s (last_ok <%s> lasttry <%s> nexttry <%s>)\n"
        c.client_name
        (let last = c.client_connection_control.control_last_ok in
        if last < 1 then "never" else string_of_date last)
      (let last = c.client_connection_control.control_last_try in
        if last < 1 then "never" else string_of_date last)
      (string_of_date (connection_next_try c.client_connection_control))
  );
  
  
  client_ops.op_client_bprint_html <- (fun c buf file ->
      
      begin
        try 
		let i = (client_info (as_client c.client_client)) in
          
		Printf.bprintf buf "\\<td title=\\\"Add as Friend\\\" class=\\\"srb ar\\\"
		onClick=\\\"parent.fstatus.location.href='/submit?q=friend_add+%d'\\\"\\>%d\\</TD\\>"
		(client_num c) (client_num c);

		html_mods_td buf [
		("", "sr", (match c.client_block with 
              None -> Printf.sprintf "" 
            | Some b -> Printf.sprintf "%s" ( 
                  let qfiles = c.client_file_queue in
                  let (qfile, qchunks) =  List.hd qfiles in
                  if (qfile = (as_file_impl file).impl_file_val) then
                    "A" else "";)) );
		((string_of_connection_state (client_state c)), "sr", 
			(short_string_of_connection_state (client_state c)) );
		("", "sr", c.client_name);
		(brand_to_string c.client_brand, "sr", gbrand_to_string c.client_brand);
		("", "sr", (if c.client_overnet then "T" else "F"));
		("", "sr", (match c.client_kind with 
              Indirect_location _ -> Printf.sprintf "I"
            | Known_location (ip,port) -> Printf.sprintf "D"));
		("", "sr br", match c.client_kind with
                    Known_location (ip,port) -> Printf.sprintf "%s" (Ip.to_string ip)
                  | Indirect_location _ -> i.client_sock_addr);
		("", "sr ar", (size_of_int64 c.client_uploaded));
		("", "sr ar br", (size_of_int64 c.client_downloaded));
		("", "sr ar", Printf.sprintf "%d" c.client_rank);
		("", "sr ar br", Printf.sprintf "%d" c.client_score);
		("", "sr ar", (let last = c.client_connection_control.control_last_ok in
            if last < 1 then "never" else (string_of_int (((last_time ()) - last) / 60))
          ));
		("", "sr ar", (let last = c.client_connection_control.control_last_try in
            if last < 1 then "never" else ( string_of_int (((last_time ()) - last) / 60))
          ));
		("", "sr ar br", (let next = (connection_next_try c.client_connection_control) in
            string_of_int ((next - (last_time ())) / 60)
          ));
		("", "sr ar", (if client_has_a_slot (as_client c.client_client) then "T" else "F"));
		("", "sr ar br", (if c.client_banned then "T" else "F"));
		("", "sr ar", Printf.sprintf "%d" c.client_requests_sent);
		("", "sr ar", Printf.sprintf "%d" c.client_requests_received);
		("", "sr ar br", Printf.sprintf "%d" (((last_time ()) - c.client_connect_time) / 60));
		("", "sr br", (Md4.to_string c.client_md4)); ];

		Printf.bprintf buf "\\<td class=\\\"sr \\\"\\>";
          
          ( let qfiles = c.client_file_queue in
            if qfiles <> [] then begin
                try
                  let _, (qchunks) = List.find (fun (qfile, _) ->
                        qfile = (as_file_impl file).impl_file_val) qfiles in
                  let tc = ref 0 in
                  Printf.bprintf buf "%s\\</td\\>\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>" 
                    (CommonFile.colored_chunks (Array.init (Array.length qchunks) 
						(fun i -> (if qchunks.(i) then 2 else 0)) )) 
                  (Array.iter (fun b -> if b then incr tc) qchunks;!tc);
                with Not_found -> (
                      Printf.bprintf buf "\\</td\\>\\<td class=\\\"sr ar\\\"\\>\\</td\\>" 
                    );
              end
            else
              Printf.bprintf buf "\\</td\\>\\<td class=\\\"sr ar\\\"\\>\\</td\\>" 
          );
       with _ -> ()
      end;
  );
  
  client_ops.op_client_dprint <- (fun c o file ->
      let info = file_info file in
      let buf = o.conn_buf in
      
      try
        
        (match c.client_block with
            None -> ()
          | Some b ->  ( 
                let qfiles = c.client_file_queue in
                let (qfile, qchunks) =  List.hd qfiles in
                if (qfile = (as_file_impl file).impl_file_val) then begin
                    client_print (as_client c.client_client) o;
                    Printf.bprintf buf "client: %s downloaded: %s uploaded: %s" 
                      (brand_to_string c.client_brand) 
                    (Int64.to_string c.client_downloaded) 
                    (Int64.to_string c.client_uploaded);
                    Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
                  end;
              
              )
        
        )
      
      with _ -> ()
  
  );
  
  client_ops.op_client_dprint_html <- (fun c o file str ->
      let info = file_info file in
      let buf = o.conn_buf in
      try
        (match c.client_block with
            None -> false 
          | Some b ->  ( 
                let qfiles = c.client_file_queue in
                let (qfile, qchunks) =  List.hd qfiles in
                if (qfile = (as_file_impl file).impl_file_val) then begin
				let i = (client_info (as_client c.client_client)) in
                    
			Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\" 
			class=\\\"%s\\\"\\> \\<td title=\\\"Add as friend\\\" class=\\\"srb ar\\\"
			onClick=\\\"parent.fstatus.location.href='/submit?q=friend_add+%d'\\\"\\>%d\\</TD\\>"
			str (client_num c) (client_num c);

			html_mods_td buf [
			(string_of_connection_state (client_state c), "sr", 
				short_string_of_connection_state (client_state c));
			(Md4.to_string c.client_md4, "sr", c.client_name);
			("", "sr", gbrand_to_string c.client_brand);
			("", "sr", (if c.client_overnet then "T" else "F"));
			("", "sr ar", Printf.sprintf "%d" (((last_time ()) - c.client_connect_time) / 60));
			("", "sr", (match c.client_kind with  
                        Indirect_location _ -> Printf.sprintf "I"
                      | Known_location (ip,port) -> Printf.sprintf "D"));
			("", "sr", match c.client_kind with
            	        Known_location (ip,port) -> Printf.sprintf "%s" (Ip.to_string ip)
						| Indirect_location _ -> i.client_sock_addr);
			("", "sr ar", (size_of_int64 c.client_uploaded));
			("", "sr ar", (size_of_int64 c.client_downloaded));
			("", "sr", info.GuiTypes.file_name) ];

			Printf.bprintf buf "\\</tr\\>";
            true
            end
            else false;
              )
        )
      with _ -> false;
  )
  
let _ =
  user_ops.op_user_set_friend <- (fun u ->
      let s = u.user_server in
      add_user_friend s u
  )

  
let _ =
  shared_ops.op_shared_unshare <- (fun file ->
      match file.file_shared with
        None -> ()
      | Some s -> 
          file.file_shared <- None;
          decr nshared_files;
          (try Unix32.close  (file_fd file) with _ -> ());
          try Hashtbl.remove files_by_md4 file.file_md4 with _ -> ()
  );
  shared_ops.op_shared_info <- (fun file ->
   let module T = GuiTypes in
     match file.file_shared with
        None -> assert false
      | Some impl ->
          { (impl_shared_info impl) with 
            T.shared_network = network.network_num;
            T.shared_filename = file_best_name file;
            T.shared_id = file.file_md4;
            }
  );
  pre_shared_ops.op_shared_info <- (fun s ->
      let module T = GuiTypes in
      let impl = s.shared_shared in
      { (impl_shared_info impl) with 
        T.shared_network = network.network_num }
  )

let _ =
  add_web_kind "server.met" (fun filename ->
      lprintf "FILE LOADED"; lprint_newline ();
      let n = load_server_met filename in
      lprintf "%d SERVERS ADDED" n; lprint_newline ();    
  );
  add_web_kind "servers.met" (fun filename ->
      lprintf "FILE LOADED"; lprint_newline ();
      let n = load_server_met filename in
      lprintf "%d SERVERS ADDED" n; lprint_newline ();    
  );
  add_web_kind "comments.met" (fun filename ->
      DonkeyIndexer.load_comments filename;
      lprintf "COMMENTS ADDED"; lprint_newline ();   
  )
  
  
