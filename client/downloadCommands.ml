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
open TcpBufferedSocket
open DownloadOneFile
open DownloadFiles
open DownloadComplexOptions
open DownloadTypes
open DownloadOptions
open DownloadGlobals
open DownloadClient
open Gui_types
open DownloadInteractive

      
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

    "dump_heap", Arg_none (fun buf _ ->
        Heap.dump_heap ();
        "heap dumped"
    ), " : dump heap for debug";
    
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
              TcpBufferedSocket.shutdown sock "user disconnect";
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
            save_file file.file_md4 (saved_name file)
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
            display_file_list buf format
    
    ), "<num>: view file info";

    "id", Arg_none (fun buf _ ->
        List.iter (fun s ->
            Printf.bprintf buf "For %s:%d  --->   %s\n"
              (Ip.to_string s.server_ip) s.server_port
              (if Ip.valid s.server_cid then
                Ip.to_string s.server_cid
              else
                Int32.to_string (Ip.to_int32 s.server_cid))
        ) !connected_server_list;
        ""
    ), " : print ID on connected servers";
    
    "add_url", Arg_two (fun kind url buf _ ->
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

    (*
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
*)
    
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
          let (size, md4, names) =
            match args with
            | [size; md4] -> (Int32.of_string size),(Md4.of_string md4), []
            | [size; md4; name] -> 
                (Int32.of_string size),(Md4.of_string md4), [name]
            | [num] -> 
                let doc = Intmap.find (int_of_string num) !last_search in
                let r = Store.get DownloadIndexer.store doc in
                r.result_size, r.result_md4, r.result_names
            | _ -> failwith "Bad number of arguments"
          in
          query_download names size md4 None None None;
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
              \<tr\>\<td\>\<form action=/submit $S\> 
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
              \<tr\>\<td\>\<form action=/submit $S\> 
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
                "\<a href=/submit\?custom=%s $O\> %s \</a\>\n" 
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
    
    "scan_temp", Arg_none (fun buf _ ->
        let list = Unix2.list_directory !!temp_directory in
        List.iter (fun filename ->
            try
              let md4 = Md4.of_string filename in
              try
                let file = find_file md4 in
                Printf.bprintf buf "%s is %s %s\n" filename
                  (first_name file)
                (if List.memq file !!files then
                    "(downloading)" else
                    "(????)")
              with _ ->
                  Printf.bprintf buf "%s %s %s\n"
                  filename
                    (if List.mem md4 !!old_files then
                      "is an old file" else "is unknown")
                  (try
                      let names = DownloadIndexer.find_names md4 in
                      List.hd names
                    with _ -> "and never seen")
                    
            with _ -> 
                Printf.bprintf buf "%s unknown\n" filename
        
        ) list;
        "done";
    ), " : print temp directory content";
      
  ]
