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

open CommonShared
open CommonServer
open CommonResult
open CommonClient
open CommonUser
open CommonInteractive
open CommonNetwork
open Gui_proto
open CommonTypes
open CommonComplexOptions
open CommonFile
open DonkeySearch
open Options
open DonkeyMftp
open Mftp_comm
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
  
let result_name r =
  match r.result_names with
    [] -> None
  | name :: _ -> Some name

      
let reconnect_all file =
  Intmap.iter (fun _ c ->
      connection_must_try c.client_connection_control;
      match c.client_kind with
        Known_location _ ->
          connect_client !!client_ip [file] c
      | _ -> ()) file.file_sources;
  List.iter (fun s ->
      match s.server_sock, server_state s with
      | Some sock, (Connected_idle | Connected_busy) ->
          query_locations file s sock    
      | _ -> ()
  ) (connected_servers())
    
let forget_search num =  
  if !last_xs = num then last_xs := (-1);
  local_searches := List.rev (List.fold_left (fun list s ->
        if s.search_search.search_num = num then list else s :: list) 
    [] !local_searches)

  
  
let save_as file real_name =
(*
Source of bug ...
Unix2.safe_mkdir (Filename.dirname real_name);
*)
  old_files =:= file.file_md4 :: !!old_files;
  file_commit (as_file file.file_file);
  Unix32.close (file_fd file);
  let old_name = file.file_hardname in
  Printf.printf "\nMOVING %s TO %s\n" old_name real_name; 
  print_newline ();
  (try 
      Unix2.rename old_name real_name ;
      change_hardname file real_name;
    with e -> 
        Printf.printf "Error in rename %s (src [%s] dst [%s])"
          (Printexc.to_string e) old_name real_name; 
        print_newline ();
        let new_name = Filename.concat (Filename.dirname old_name)
          (Filename.basename real_name) in
        try 
          Unix2.rename old_name new_name;
          change_hardname file new_name
        with _ -> ()
  )
  ;
  remove_file_clients file;
  file.file_changed <- FileInfoChange;
  !file_change_hook file
  
let save_file file name =
  let real_name = Filename.concat !!incoming_directory name in
  save_as file real_name;
  file_commit (as_file file.file_file)

let load_server_met filename =
  try
    let module S = DonkeyImport.Server in
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
    ) ss;
    List.length ss
  with e ->
      Printf.printf "Exception %s while loading %s" (Printexc.to_string e)
      filename;
      print_newline ();
      0

    
      
let really_query_download filenames size md4 location old_file absents =
  begin
    try
      let file = Hashtbl.find files_by_md4 md4 in
      if file_state file = FileDownloaded then 
        raise Already_done;
    with Not_found -> ()
  end;
  
  List.iter (fun file -> 
      if file.file_md4 = md4 then raise Already_done) 
  !current_files;

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
              Unix2.rename filename temp_file with e -> 
                Printf.printf "Could not rename %s to %s: exception %s"
                  filename temp_file (Printexc.to_string e);
                print_newline () );        
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
  file.file_filenames <- filenames @ file.file_filenames;

  current_files := file :: !current_files;
  !file_change_hook file;
  set_file_size file (file_size file);
  List.iter (fun s ->
      match s.server_sock with
        None -> () (* assert false !!! *)
      | Some sock ->
          query_locations file s sock
  ) (connected_servers());

  (try
      let servers = Hashtbl.find_all udp_servers_replies file.file_md4 in
      List.iter (fun s ->
          udp_server_send s (Mftp_server.QueryLocationUdpReq file.file_md4)
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
    let module P = DonkeyImport.Pref in
    let s = File.to_string filename in
    let t = P.read s in
    t.P.client_tags, t.P.option_tags
  with e ->
      Printf.printf "Exception %s while loading %s" (Printexc.to_string e)
      filename;
      print_newline ();
      [], []
      
  
let import_config dirname =
  ignore (load_server_met (Filename.concat dirname "server.met"));
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
  let module P = DonkeyImport.Part in
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
                    Printf.printf "Import Donkey %s" s; 
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
      TcpBufferedSocket.write sock s 0 len
  ) !user_socks

let longest_name file =
  let max = ref "" in
  let maxl = ref 0 in
  List.iter (fun name ->
      if String.length name > !maxl then begin
          maxl := String.length name;
          max := name
        end
  ) file.file_filenames;
  !max

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
      
let print_file buf file =
  Printf.bprintf buf "[%-5d] %s %10ld %32s %s" 
    (file_num file)
    (first_name file)
  (file_size file)
  (Md4.to_string file.file_md4)
  (if file_state file = FileDownloaded then
      "done" else
      Int32.to_string (file_downloaded file));
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
            None -> Date.to_string (connection_last_conn
                  c.client_connection_control)
          | Some _ -> "Connected")
    | _ ->
        Printf.bprintf  buf "[%-5d] %12s            %s\n"
          (client_num c)
          "indirect"
          (match c.client_sock with
            None -> Date.to_string (connection_last_conn
                  c.client_connection_control)
          | Some _ -> "Connected")
  in
  Intmap.iter f file.file_sources;
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

let parse_donkey_url url =
  match String2.split (String.escaped url) '|' with
  | "ed2k://" :: "file" :: name :: size :: md4 :: _
  | "file" :: name :: size :: md4 :: _ ->
      query_download [name] (Int32.of_string size)
      (Md4.of_string md4) None None None;
      true
  | "ed2k://" :: "server" :: ip :: port :: _
  | "server" :: ip :: port :: _ ->
      let s = add_server (Ip.of_string ip) (int_of_string port) in
      server_must_update s;
      true
  | "ed2k://" :: "friend" :: ip :: port :: _
  | "friend" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let port = int_of_string port in
      let c = new_client (Known_location (ip,port)) in
      new_friend c;
      true

  | _ -> false

let commands = [
    "n", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
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
        
    "vu", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.sprintf "Upload credits : %d minutes\nUpload disabled for %d minutes" !upload_credit !has_upload;
    
    ), " : view upload credits";
            
    "mem_stats", Arg_none (fun o -> 
        let buf = o.conn_buf in
        DonkeyGlobals.mem_stats buf;
        ""
    ), " : print memory stats";

        
    "comments", Arg_one (fun filename o ->
        let buf = o.conn_buf in
        DonkeyIndexer.load_comments filename;
        DonkeyIndexer.save_comments ();
        "comments loaded and saved"
    ), " <filename> : load comments from file";
    
    "comment", Arg_two (fun md4 comment o ->
        let buf = o.conn_buf in
        let md4 = Md4.of_string md4 in
        DonkeyIndexer.add_comment md4 comment;
        "Comment added"
    ), " <md4> \"<comment>\" : add comment on an md4";
    
    "nu", Arg_one (fun num o ->
        let buf = o.conn_buf in
        let num = int_of_string num in
        let num = maxi 0 num in
        if num <= !upload_credit then
          begin
            upload_credit := !upload_credit - num;
            has_upload := !has_upload + num;
            Printf.sprintf "upload disabled for %d minutes" num
          end
        else 
          "not enough upload credits"
    
    
    ), " <m> : disable upload during <m> minutes (multiple of 5)";
    
    "import", Arg_one (fun dirname o ->
        let buf = o.conn_buf in
        try
          import_config dirname;
          "config loaded"
        with e ->
            Printf.sprintf "error %s while loading config" (
              Printexc.to_string e)
    ), " <dirname> : import the config from dirname";
    
    "load_old_history", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyIndexer.load_old_history ();
        "Old history loaded"
    ), " : load history.dat file";
    
    "servers", Arg_one (fun filename o ->
        let buf = o.conn_buf in
        try
          let n = load_server_met filename in
          Printf.sprintf "%d servers loaded" n
        with e -> 
            Printf.sprintf "error %s while loading file" (Printexc.to_string e)
    ), " <filename> : add the servers from a server.met file";

    
    "id", Arg_none (fun o ->
        let buf = o.conn_buf in
        List.iter (fun s ->
            Printf.bprintf buf "For %s:%d  --->   %s\n"
              (Ip.to_string s.server_ip) s.server_port
              (if Ip.valid s.server_cid then
                Ip.to_string s.server_cid
              else
                Int32.to_string (Ip.to_int32 s.server_cid))
        ) (connected_servers());
        ""
    ), " : print ID on connected servers";
    
    "add_url", Arg_two (fun kind url o ->
        let buf = o.conn_buf in
        let v = (kind, 1, url) in
        if not (List.mem v !!web_infos) then
          web_infos =:=  v :: !!web_infos;
        load_url kind url;
        "url added to web_infos. downloading now"
    ), " <kind> <url> : load this file from the web. 
    kind is either server.met (if the downloaded file is a server.met)";
    
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
                    let size = Unix32.getsize32 (Filename.concat 
                          !!temp_directory filename) in
                    query_download [] size md4 None None None
              with e ->
                  Printf.printf "exception %s in recover_temp"
                    (Printexc.to_string e); print_newline ();
        ) files;
        "done"
    ), " : recover lost files from temp directory";
    
    "upstats", Arg_none (fun o ->
        let buf = o.conn_buf in
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
              file.file_upload_kbs;
        ) list;
        "done"
    ), " : statistics on upload";
    
    "xs", Arg_none (fun o ->
        let buf = o.conn_buf in
        if !last_xs >= 0 then begin
            try
              let ss = DonkeyFiles.find_search !last_xs in
              make_xs ss;
              "extended search done"
            with e -> Printf.sprintf "Error %s" (Printexc.to_string e)
          end else "No previous extended search"),
    ": extended search";
    
    "clh", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyIndexer.clear ();
        "local history cleared"
    ), " : clear local history";
    
    "scan_temp", Arg_none (fun o ->
        let buf = o.conn_buf in
        let list = Unix2.list_directory !!temp_directory in
        List.iter (fun filename ->
            try
              let md4 = Md4.of_string filename in
              try
                let file = find_file md4 in
                Printf.bprintf buf "%s is %s %s\n" filename
                  (first_name file)
                "(downloading)" 
              with _ ->
                  Printf.bprintf buf "%s %s %s\n"
                  filename
                    (if List.mem md4 !!old_files then
                      "is an old file" else "is unknown")
                  (try
                      let names = DonkeyIndexer.find_names md4 in
                      List.hd names
                    with _ -> "and never seen")
                    
            with _ -> 
                Printf.bprintf buf "%s unknown\n" filename
        
        ) list;
        "done";
    ), " : print temp directory content";
    
    "dllink", Arg_multiple (fun args o ->        
        let buf = o.conn_buf in
        let url = String2.unsplit args ' ' in
        if parse_donkey_url url then
          "download started"
        else "bad syntax"
    ), " <ed2klink> : download ed2k:// link";
    
    "force_download", Arg_none (fun o ->
        let buf = o.conn_buf in
        match !aborted_download with
          None -> "No download to force"
        | Some (filenames,size,md4,location,old_file,absents) ->
            really_query_download filenames
            size md4 location old_file absents;
            "download started"
    ), " : force download of an already downloaded file";

        
    "forget", Arg_one (fun num o ->
        let buf = o.conn_buf in
        let num = int_of_string num in
        forget_search num;
        ""  
    ), " <num> : forget search <num>";

    (*
    "ls", Arg_multiple (fun args o ->
        let query = CommonSearch.search_of_args args in
        let search = CommonSearch.new_search query in
        local_searches := search :: !local_searches;
        DonkeyIndexer.find search;
        "local search started"
    ), " <query> : local search";
*)
    
    "remove_old_servers", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyServers.remove_old_servers ();
        "clean done"
    ), ": remove servers that have not been connected for several days";

  ]

let _ =
  register_commands commands;
  file_ops.op_file_resume <- (fun file ->
      reconnect_all file;
      file.file_changed <- FileInfoChange;
      !file_change_hook file
  );
  file_ops.op_file_pause <- (fun file ->
      file.file_changed <- FileInfoChange;
      !file_change_hook file
  );
  network.op_network_private_message <- (fun iddest s ->      
      try
        let c = DonkeyGlobals.find_client_by_name iddest in
        match c.client_sock with
          None -> 
            (
(* A VOIR  : est-ce que c'est bien de fait comme ça ? *)
              DonkeyClient.connect_client !!CommonOptions.client_ip [] c;
              match c.client_sock with
                None ->
                  CommonChat.send_text !!CommonOptions.chat_console_id None 
                    (Printf.sprintf "client %s : could not connect (client_sock=None)" iddest)
              | Some sock ->
                  direct_client_send sock (Mftp_client.SayReq s)
            )
        | Some sock ->
            direct_client_send sock (Mftp_client.SayReq s)
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

module P = Gui_proto

let _ =
  file_ops.op_file_info <- (fun file ->
      {
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
        P.file_chunks = file.file_all_chunks;
        P.file_availability = String2.init file.file_nchunks (fun i ->
            if file.file_available_chunks.(i) > 1 then '2' else
            if file.file_available_chunks.(i) > 0 then '1' else
              '0');
        P.file_format = file.file_format;
        P.file_chunks_age = file.file_chunks_age;
        P.file_age = file_age file;
      }
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
      }
  )

let _ =
  result_ops.op_result_download <- (fun rs filenames ->
      let r = Store.get store rs.result_index in
      query_download filenames r.result_size r.result_md4 None None None;
  )

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
        Some sock, (Connected_idle | Connected_busy) ->
          direct_server_send sock (Mftp_server.QueryUsersReq "");
          Fifo.put s.server_users_queries false
      | _ -> ()
  );
  server_ops.op_server_find_user <- (fun s user ->
      match s.server_sock, server_state s with
        Some sock, (Connected_idle | Connected_busy) ->
          direct_server_send sock (Mftp_server.QueryUsersReq user);
          Fifo.put s.server_users_queries true
      | _ -> ()      
  );
  server_ops.op_server_users <- (fun s ->
      List2.tail_map (fun u -> as_user u.user_user) s.server_users)    ;
  ()

let _ =
  file_ops.op_file_save_as <- (fun file name ->
      file.file_filenames <- [name]
  );
  file_ops.op_file_disk_name <- (fun file -> file.file_hardname);
  file_ops.op_file_best_name <- (fun file -> first_name file);
  file_ops.op_file_set_format <- (fun file format ->
      file.file_format <- format);
  file_ops.op_file_check <- (fun file ->
      DonkeyOneFile.verify_chunks file);  
  file_ops.op_file_recover <- (fun file ->
      if file_state file = FileDownloading then 
        reconnect_all file);
  
  file_ops.op_file_sources <- (fun file ->
      let list = ref [] in
      Intmap.iter (fun _ c -> 
          list := (as_client c.client_client) :: !list) file.file_sources;
      !list)
  
let _ =
  network.op_network_extend_search <- (fun _ ->
      if !last_xs >= 0 then begin
          try
            let ss = DonkeyFiles.find_search !last_xs in
            make_xs ss;
          with _ -> ()
        end
  );
  
  network.op_network_clean_servers <- (fun _ ->
      DonkeyServers.remove_old_servers ());
  
  network.op_network_connect_servers <- (fun _ ->
      force_check_server_connections true);
  
  network.op_network_parse_url <- parse_donkey_url

let _ =
  client_ops.op_client_say <- (fun c s ->
      match c.client_sock with
        None -> ()
      | Some sock ->
          direct_client_send sock (Mftp_client.SayReq s)
  );  
  client_ops.op_client_files <- (fun c ->
      match c.client_all_files with
        None ->  []
      | Some files -> 
          List2.tail_map (fun r -> "", as_result r.result_result) files);
  client_ops.op_client_browse <- (fun c immediate ->
(*      Printf.printf "should browse"; print_newline (); *)
      if immediate then browse_client c else begin
(*          Printf.printf "Adding friend to clients_list"; print_newline (); *)
          clients_list := (c, []) :: !clients_list;
          incr clients_list_len
        end);
  client_ops.op_client_connect <- (fun c ->
      connection_must_try c.client_connection_control;
      connect_client !!client_ip [] c
  );
  client_ops.op_client_clear_files <- (fun c ->
      c.client_all_files <- None;
      check_useful_client c)

let _ =
  user_ops.op_user_set_friend <- (fun u ->
      let s = u.user_server in
      add_user_friend s u
  )

  
let _ =
  shared_ops.op_shared_unshare <- (fun s file ->
      file.file_shared <- false;
      decr nshared_files;
      Unix32.close  (file_fd file);
      file.file_hardname <- "";
      try Hashtbl.remove files_by_md4 file.file_md4 with _ -> ()
  )

let _ =
  add_web_kind "server.met" (fun filename ->
      let n = load_server_met filename in
      Printf.printf "%d SERVERS ADDED" n; print_newline ();    
  );
  add_web_kind "comments.met" (fun filename ->
      DonkeyIndexer.load_comments filename;
      Printf.printf "COMMENTS ADDED"; print_newline ();   
  )
  
