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
open CommonSearch
open CommonClient
open CommonServer
open CommonNetwork
open Gui_proto
open CommonTypes
open CommonFile
open CommonComplexOptions
open Options
open BasicSocket
open TcpBufferedSocket
open DriverInteractive
open CommonOptions
  
let execute_command arg_list output cmd args =
  try
    let buf = output.conn_buf in
    List.iter (fun (command, arg_kind, help) ->
        if command = cmd then
          Buffer.add_string buf (
            match arg_kind, args with
              Arg_none f, [] -> f output
            | Arg_multiple f, _ -> f args output
            | Arg_one f, [arg] -> f arg  output
            | Arg_two f, [a1;a2] -> f a1 a2 output
            | Arg_three f, [a1;a2;a3] -> f a1 a2 a3 output
            | _ -> "Bad number of arguments"
          )
    ) arg_list
  with Not_found -> ()

let commands = [
    
    "dump_heap", Arg_none (fun o ->
        Heap.dump_heap ();
        "heap dumped"
    ), " : dump heap for debug";
 
    "close_fds", Arg_none (fun o ->
        Unix32.close_all ();
        "All files closed"
    ), " : close all files (use to free space on disk after remove)";
    
    "commit", Arg_none (fun o ->
        List.iter (fun file ->
            file_commit file
        ) !!done_files;
        "commited"
    ) , ": move downloaded files to incoming directory";
    
    "vd", Arg_multiple (fun args o -> 
        let buf = o.conn_buf in
        match args with
          [arg] ->
            let num = int_of_string arg in
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
            (*
            Printf.bprintf  buf "\nDownloading %d files\n" 
              (List.length !!files);
            
            List.iter (fun file ->
                file_print file o) !!files;
*)
            
            Printf.bprintf  buf "\nDownloaded %d files\n" 
              (List.length !!done_files);
            if !!done_files <> [] then begin
                List.iter (fun file -> CommonFile.file_print file o) 
                !!done_files;
(*                simple_print_file_list true buf !!done_files format; *)
                Printf.bprintf buf
                  "Use 'commit' to move downloaded files to the incoming directory"
              end;
              
            ""    
    ), "<num>: view file info";

    "vm", Arg_none (fun o ->
        CommonInteractive.print_connected_servers o;
        ""), ": list connected servers";
    
    "q", Arg_none (fun o ->
        raise CommonTypes.CommandCloseSocket
    ), ": close telnet";
    
    "bs", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let ip = Ip.of_string arg in
            server_black_list =:=  ip :: !!server_black_list;
        ) args;
        "done"
    ), " <ip1> <ip2> ... : add these IPs to the servers black list";

    "debug_socks", Arg_none (fun o ->
        BasicSocket.print_sockets ();
        "done"), " : for debugging only";
    
    "kill", Arg_none (fun o ->
        CommonGlobals.exit_properly ();
        "exit"), ": save and kill the server";
    
    "save", Arg_none (fun o ->
        DriverInteractive.save_config ();
        "saved"), ": save";
  
    "port", Arg_one (fun arg o ->
        port =:= int_of_string arg;
        "new port will change at next restart"),
    " <port> : change connection port";
    
    "vo", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          Printf.bprintf  buf "\<table border=0\>";
        List.iter (fun (name, value) ->
            if o.conn_output = HTML then
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
        if o.conn_output = HTML then
          Printf.bprintf  buf "\</table\>";
        
        ""
    ), " : print options";
    
    "set", Arg_two (fun name value o ->
        try
          Options.set_simple_option downloads_ini name value;
          Printf.sprintf "option %s value changed" name
        with e ->
            Printf.sprintf "Error %s" (Printexc.to_string e)
    ), " <option_name> <option_value> : change option value";

    "set_in", Arg_three (fun name network value o ->
        try
          let n = network_find_by_name network in
          let opfile = n.op_network_config_file () in
          Options.set_simple_option opfile name value;
          Printf.sprintf "option %s value changed" name
        with e ->
            Printf.sprintf "Error %s" (Printexc.to_string e)
    ), " <network> <option_name> <option_value> : change option value for a given network";
    
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
    ), "  [<num>]: view results of a search";

    "vr", Arg_none (fun o ->
        "done"
    ), " : print last Common search results";
    
    "s", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let query = CommonSearch.search_of_args args in
        ignore (CommonInteractive.start_search query buf);
        ""
    ), " <query> : search for files on all networks\n
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
  
    "d", Arg_one (fun arg o ->
        CommonInteractive.download_file buf arg),
    " <num> : file to download on OpenNap";

    "vs", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf  buf "Searching %d queries\n" (
          List.length !CommonSearch.searches);
        List.iter (fun s ->
            Printf.bprintf buf "%s[%-5d]%s %s %s\n" 
              (if o.conn_output = HTML then 
                Printf.sprintf "\<a href=/submit\?q=vr\+%d\>" s.search_num
              else "")
            s.search_num 
              (if o.conn_output = HTML then "\</a\>" else "")
            s.search_string
              (if s.search_waiting = 0 then "done" else
                string_of_int s.search_waiting)
        ) !CommonSearch.searches; ""), ": view all queries";

    "view_custom_queries", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output <> HTML then
          Printf.bprintf buf "%d custom queries defined\n" 
            (List.length !!customized_queries);
        List.iter (fun (name, q) ->
            if o.conn_output = HTML then
              Printf.bprintf buf 
                "\<a href=/submit\?custom=%s $O\> %s \</a\>\n" 
              (Url.encode name) name
            else
              Printf.bprintf buf "[%s]\n" name
        ) !! customized_queries; ""
    ), ": view custom queries";
    
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
    ), " <num> : cancel download (use arg 'all' for all files)";
    
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
    ), " <num> : pause a download (use arg 'all' for all files)";
    
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
    ), " <num> : resume a paused download (use arg 'all' for all files)";

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
    " [<num>]: connect to more servers (or to server <num>)";

    "vc", Arg_one (fun num o ->
        let num = int_of_string num in
        let c = client_find num in
        client_print c o;
        ""
    ), " <num> : view client";
    
    "x", Arg_one (fun num o ->
        let num = int_of_string num in
        let s = server_find num in
        if server_state s <> NotConnected then
          server_disconnect s;
        ""
    ), " <num> : disconnect from server";

        
    "vma", Arg_none (fun o ->
        let buf = o.conn_buf in       
        Printf.bprintf  buf "Servers: %d known\n" (List.length !!servers);
        List.iter (fun s ->
            server_print s o
        ) !!servers; ""), ": list all known servers";
        
    "reshare", Arg_none (fun o ->
        let buf = o.conn_buf in
        shared_check_files ();
        "check done"
    ), " : check shared files for removal";

  ]

let _ =
  CommonNetwork.register_commands commands
