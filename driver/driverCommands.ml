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

open CommonMessages
open CommonGlobals
open CommonShared
open CommonSearch
open CommonClient
open CommonServer
open CommonNetwork
open GuiTypes
open CommonTypes
open CommonFile
open CommonComplexOptions
open Options
open BasicSocket
open TcpBufferedSocket
open DriverInteractive
open CommonOptions

let execute_command arg_list output cmd args =
  let buf = output.conn_buf in
  try
    let rec iter list =
      match list with
        [] -> 
          Gettext.buftext buf no_such_command cmd
      | (command, arg_kind, help) :: tail ->
          if command = cmd then
            Buffer.add_string buf (
              match arg_kind, args with
                Arg_none f, [] -> f output
              | Arg_multiple f, _ -> f args output
              | Arg_one f, [arg] -> f arg  output
              | Arg_two f, [a1;a2] -> f a1 a2 output
              | Arg_three f, [a1;a2;a3] -> f a1 a2 a3 output
              | _ -> !!bad_number_of_args
            )
          else
            iter tail
    in
    iter arg_list
  with Not_found -> ()

let list_options o list = 
  let buf = o.conn_buf in
  if o.conn_output = HTML then
    Printf.bprintf  buf "\\<table border=0\\>";
  List.iter (fun (name, value) ->
      if String.contains value '\n' then begin
          if o.conn_output = HTML then
            Printf.bprintf buf "
                  \\<tr\\>\\<td\\>\\<form action=/submit $S\\> 
                  \\<input type=hidden name=setoption value=q\\>
                  \\<input type=hidden name=option value=%s\\> %s \\</td\\>\\<td\\>
                  \\<textarea name=value rows=10 cols=70 wrap=virtual\\> 
                  %s
                  \\</textarea\\>
                  \\<input type=submit value=Modify\\>
                  \\</td\\>\\</tr\\>
                  \\</form\\>
                  " name name value
        end
      else
      if o.conn_output = HTML then
        Printf.bprintf buf "
              \\<tr\\>\\<td\\>\\<form action=/submit $S\\> 
\\<input type=hidden name=setoption value=q\\>
\\<input type=hidden name=option value=%s\\> %s \\</td\\>\\<td\\>
              \\<input type=text name=value size=40 value=\\\"%s\\\"\\>
\\</td\\>\\</tr\\>
\\</form\\>
              " name name value
      else
        Printf.bprintf buf "%s = %s\n" name value)
  list;
  if o.conn_output = HTML then
    Printf.bprintf  buf "\\</table\\>"
  
let commands = [
    
    "dump_heap", Arg_none (fun o ->
        Heap.dump_heap ();
        "heap dumped"
    ), ":\t\t\t\tdump heap for debug";

    "dump_usage", Arg_none (fun o ->
        Heap.dump_usage ();
        "usage dumped"
    ), ":\t\t\t\tdump main structures for debug";
    
    "close_fds", Arg_none (fun o ->
        Unix32.close_all ();
        "All files closed"
    ), ":\t\t\t\tclose all files (use to free space on disk after remove)";
    
    "commit", Arg_none (fun o ->
        List.iter (fun file ->
            file_commit file
        ) !!done_files;
        "commited"
    ) , ":\t\t\t\tmove downloaded files to incoming directory";
    
    "vd", Arg_multiple (fun args o -> 
        let buf = o.conn_buf in
        match args with
          [arg] ->
            let num = int_of_string arg in
              if o.conn_output = HTML then
		Printf.bprintf  buf "\\<a href=/files\\>Display all files\\</a\\>\\<br\\>";
            List.iter 
              (fun file -> if (as_file_impl file).impl_file_num = num then 
                  CommonFile.file_print file o)
            !!files;
            List.iter
              (fun file -> if (as_file_impl file).impl_file_num = num then 
                  CommonFile.file_print file o)
            !!done_files;
            ""
        | _ ->
            DriverInteractive.display_file_list buf o;
            ""    
    ), "<num> :\t\t\t\tview file info";
    
    "vm", Arg_none (fun o ->
        CommonInteractive.print_connected_servers o;
        ""), ":\t\t\t\t\tlist connected servers";
    
    "q", Arg_none (fun o ->
        raise CommonTypes.CommandCloseSocket
    ), ":\t\t\t\t\tclose telnet";
    
    "debug_socks", Arg_none (fun o ->
        BasicSocket.print_sockets ();
        "done"), ":\t\t\t\tfor debugging only";
    
    "kill", Arg_none (fun o ->
        CommonGlobals.exit_properly ();
        "exit"), ":\t\t\t\t\tsave and kill the server";
    
    "save", Arg_none (fun o ->
        DriverInteractive.save_config ();
        "saved"), ":\t\t\t\t\tsave";
    
    "vo", Arg_none (fun o ->
        let buf = o.conn_buf in
        list_options o  (
          List.map Options.strings_of_option 
            [max_hard_upload_rate; max_hard_download_rate;
            telnet_port; gui_port; http_port]
        );        
        "\nUse 'voo' for all options"    
    ), ":\t\t\t\t\tdisplay options";

    "voo", Arg_none (fun o ->
        let buf = o.conn_buf in
        list_options o  (CommonInteractive.all_simple_options ());
        ""
    ), ":\t\t\t\t\tprint options";
    
    "upstats", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf buf "Upload statistics:\n";
        Printf.bprintf buf "Total: %s bytes uploaded\n" 
          (Int64.to_string !upload_counter);
        let list = ref [] in
        shared_iter (fun s ->
            let impl = as_shared_impl s in
            list := impl :: !list
        );
        let list = Sort.list (fun f1 f2 ->
              (f1.impl_shared_requests = f2.impl_shared_requests &&
              f1.impl_shared_uploaded > f2.impl_shared_uploaded) ||
              (f1.impl_shared_requests > f2.impl_shared_requests )
          ) !list in
        List.iter (fun impl ->
            Printf.bprintf buf "%-50s requests: %8d bytes: %10s\n"
              impl.impl_shared_codedname impl.impl_shared_requests
              (Int64.to_string impl.impl_shared_uploaded);
        ) list;
        "done"
    ), ":\t\t\t\tstatistics on upload";
    
    "set", Arg_two (fun name value o ->
        try
          try
            let buf = o.conn_buf in
            CommonInteractive.set_fully_qualified_options name value;
            Printf.sprintf "option %s value changed" name
            
            (*
            let pos = String.index name '-' in
            let prefix = String.sub name 0 pos in
            let name = String.sub name (pos+1) (String.length name - pos-1) in
            networks_iter (fun n ->
                match n.network_config_file with
                  None -> ()
                | Some opfile ->
                    List.iter (fun p ->
                        if p = prefix then begin
                            set_simple_option opfile name value;
                            Printf.bprintf buf "option %s :: %s value changed" 
                            n.network_name name
                            
                          end)
                    n.network_prefixes      
);
  *)
          with _ -> 
              Options.set_simple_option downloads_ini name value;
              Printf.sprintf "option %s value changed" name
        with e ->
            Printf.sprintf "Error %s" (Printexc2.to_string e)
    ), "<option_name> <option_value> :\tchange option value";
    
    "vr", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          num :: _ -> 
            List.iter (fun num ->
                let num = int_of_string num in
                let s = search_find num in
                DriverInteractive.print_search buf s o) args;
            ""
        | [] ->   
            begin
              match !searches with
                [] -> "No search to print"
              | s :: _ ->
                  DriverInteractive.print_search buf s o;
                  ""
            end;
    ), "[<num>] :\t\t\t\tview results of a search";

    "vr", Arg_none (fun o ->
        "done"
    ), ":\t\t\t\t\tprint last Common search results";
    
    "s", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let query = CommonSearch.search_of_args args in
        ignore (CommonInteractive.start_search 
            (let module G = GuiTypes in
            { G.search_num = 0;
              G.search_query = query;
              G.search_max_hits = 10000;
              G.search_type = RemoteSearch;
            }) buf);
        ""
    ), "<query> :\t\t\t\tsearch for files on all networks\n
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
    
    "ls", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let query = CommonSearch.search_of_args args in
        ignore (CommonInteractive.start_search 
            (let module G = GuiTypes in
            { G.search_num = 0;
              G.search_query = query;
              G.search_max_hits = 10000;
              G.search_type = LocalSearch;
            }) buf);
        ""
    ), "<query> :\t\t\t\tsearch for files on all networks\n
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
  
    "d", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            CommonInteractive.download_file o.conn_buf arg) args;
        ""),
    "<num> :\t\t\t\tfile to download";

    "force_download", Arg_none (fun o ->
        let buf = o.conn_buf in
        match !CommonGlobals.aborted_download with
          None -> "No download to force"
        | Some r ->
            CommonResult.result_download (CommonResult.result_find r) [] true;
            "download forced"
    ), ":\t\t\tforce download of an already downloaded file";

    "vs", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf  buf "Searching %d queries\n" (
          List.length !searches);
        List.iter (fun s ->
            Printf.bprintf buf "%s[%-5d]%s %s %s\n" 
              (if o.conn_output = HTML then 
                Printf.sprintf "\\<a href=/submit\\?q=vr\\+%d\\>" s.search_num
              else "")
            s.search_num 
              (if o.conn_output = HTML then "\\</a\\>" else "")
            s.search_string
              (if s.search_waiting = 0 then "done" else
                string_of_int s.search_waiting)
        ) !searches; ""), ":\t\t\t\t\tview all queries";

    "view_custom_queries", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output <> HTML then
          Printf.bprintf buf "%d custom queries defined\n" 
            (List.length !!customized_queries);
        List.iter (fun (name, q) ->
            if o.conn_output = HTML then
              Printf.bprintf buf 
                "\\<a href=/submit\\?custom=%s $O\\> %s \\</a\\>\n" 
              (Url.encode name) name
            else
              Printf.bprintf buf "[%s]\n" name
        ) !! customized_queries; ""
    ), ":\t\t\tview custom queries";
    
    "cancel", Arg_multiple (fun args o ->
        if args = ["all"] then
          List.iter (fun file ->
              file_cancel file
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if (as_file_impl file).impl_file_num = num then begin
                      Printf.printf "TRY TO CANCEL FILE"; print_newline ();
                      file_cancel file
                    end
              ) !!files) args; 
        ""
    ), "<num> :\t\t\t\tcancel download (use arg 'all' for all files)";
    
    "pause", Arg_multiple (fun args o ->
        if args = ["all"] then
          List.iter (fun file ->
              file_pause file;
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if (as_file_impl file).impl_file_num = num then begin
                      file_pause file
                    end
              ) !!files) args; ""
    ), "<num> :\t\t\t\tpause a download (use arg 'all' for all files)";
    
    "resume", Arg_multiple (fun args o ->
        if args = ["all"] then
          List.iter (fun file ->
              file_resume file
          ) !!files
        else
        List.iter (fun num ->
            let num = int_of_string num in
            List.iter (fun file ->
                  if (as_file_impl file).impl_file_num = num then begin
                      file_resume file
                  end
            ) !!files) args; ""
    ), "<num> :\t\t\t\tresume a paused download (use arg 'all' for all files)";

    "c", Arg_multiple (fun args o ->
        match args with
          [] ->
            networks_iter network_connect_servers;
            "connecting more servers"
        | _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = server_find num in
                server_connect s
            ) args;
            "connecting server"
    ),
    "[<num>] :\t\t\t\tconnect to more servers (or to server <num>)";

    "vc", Arg_one (fun num o ->
        let num = int_of_string num in
        let c = client_find num in
        client_print c o;
        ""
    ), "<num> :\t\t\t\tview client";
    
    "x", Arg_one (fun num o ->
        let num = int_of_string num in
        let s = server_find num in
        if server_state s <> NotConnected then
          server_disconnect s;
        ""
    ), "<num> :\t\t\t\tdisconnect from server";

    "use_poll", Arg_one (fun arg o ->
        let b = bool_of_string arg in
        BasicSocket.use_poll b;
        Printf.sprintf "poll: %b" b
    ), "<bool> :\t\t\tuse poll instead of select";
        
    "vma", Arg_none (fun o ->
        let buf = o.conn_buf in       
        let nb_servers = ref 0 in
        Intmap.iter (fun _ s ->
            try
              incr nb_servers;
              server_print s o
            with e ->
                Printf.printf "Exception %s in server_print"
                  (Printexc2.to_string e); print_newline ();
        ) !!servers;
        Printf.sprintf "Servers: %d known\n" !nb_servers
        ), ":\t\t\t\t\tlist all known servers";
        
    "reshare", Arg_none (fun o ->
        let buf = o.conn_buf in
        shared_check_files ();
        "check done"
    ), ":\t\t\t\tcheck shared files for removal";

    "priority", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          p :: files ->
            let p = int_of_string p in
            let p = if p < 0 then 0 else p in
            List.iter (fun arg ->
                try
                  let file = file_find (int_of_string arg) in
                  set_file_priority file p;
                  Printf.bprintf buf "Setting priority of %s to %d\n"
                    (file_best_name file) (file_priority file);
                with _ -> failwith (Printf.sprintf "No file number %s" arg)
            ) files;
            "Done"
        | [] -> "Bad number of args"
        
    ), "<priority> <files numbers> :\tchange file priorities";
    
    "version", Arg_none (fun o ->
        CommonGlobals.version
    ), ":\t\t\t\tprint mldonkey version";

    "forget", Arg_one (fun num o ->
        let buf = o.conn_buf in
        let num = int_of_string num in
        CommonSearch.search_forget (CommonSearch.search_find num);
        ""  
    ), "<num> :\t\t\t\tforget search <num>";

    "close_all_sockets", Arg_none (fun o ->
        BasicSocket.close_all ();
        "All sockets closed"
    ), ":\t\t\tclose all opened sockets";

    "friends", Arg_none (fun o ->
        let buf = o.conn_buf in
        List.iter (fun c ->
            let i = client_info c in
            let n = network_find_by_num i.client_network in
            Printf.bprintf buf "[%s %d] %s" n.network_name
            i.client_num i.client_name
        ) !!friends;
        ""
    ), ":\t\t\tdisplay all friends";
    
    "files", Arg_one (fun arg o ->
        let buf = o.conn_buf in
        let n = int_of_string arg in
        List.iter (fun c ->
            if client_num c = n then begin
                let rs = client_files c in
                
                let rs = List2.tail_map (fun (s, r) ->
                      r, CommonResult.result_info r, 1
                  ) rs in
                CommonInteractive.last_results := [];
                Printf.bprintf buf "Reinitialising download selectors\n";
                print_results buf o rs;
                
                ()
              end
        ) !!friends;
        ""), " <friend_num> :\tPrint friend files";
    ]

let _ =
  CommonNetwork.register_commands commands
