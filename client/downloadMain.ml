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


open Options
open Mftp
open Mftp_comm
open DownloadServers
open BasicSocket
open DownloadComplexOptions
open TcpBufferedSocket
open DownloadOneFile
open DownloadFiles
open DownloadInteractive
open DownloadInterface  
open DownloadTypes  
open DownloadGlobals
open DownloadClient
open Gui_types
open DownloadOptions


  
let _ =
  Sys.set_signal  Sys.sigchld (*Sys.Signal_ignore;*)
    (Sys.Signal_handle (fun _ ->
      Printf.printf "SIGCHLD"; print_newline ()));
  Sys.set_signal  Sys.sigpipe (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ ->
      Printf.printf "SIGPIPE"; print_newline ()))

let is_directory filename =
  try let s = Unix.stat filename in s.Unix.st_kind = Unix.S_DIR with _ -> false

let rec safe_mkdir dir =  
    if Sys.file_exists dir then begin
      if not (is_directory dir) then 
        failwith (Printf.sprintf "%s not a directory" dir)
    end
  else begin
      let predir = Filename.dirname dir in
      if predir <> dir then safe_mkdir predir;
      Unix.mkdir dir 0o777
    end    

    
    
let hourly_timer timer =
  reactivate_timer timer;
  DownloadServers.remove_old_servers_timer ();
  DownloadInteractive.check_shared_files ();
  DownloadFiles.remove_old_clients ();
  DownloadClient.clean_groups ();
  Mftp_comm.propagate_working_servers 
    (List.map (fun s -> s.server_ip, s.server_port) !connected_server_list)
  
let day = ref (-1)
  
let do_daily () =
  incr day;
  List.iter (fun (kind, period, url) ->
      if !day mod period = 0 then load_url kind url
  ) !!web_infos
  
let daily_timer timer =
  reactivate_timer timer;
  do_daily ()

let quarter_timer timer =
  reactivate_timer timer;
  DownloadFiles.fill_clients_list ()

let second_timer timer =
  reactivate_timer timer;
  DownloadFiles.check_clients ();
  DownloadFiles.reset_upload_timer ()

let halfmin_timer timer =
  reactivate_timer timer;
  DownloadServers.update_master_servers ();
  DownloadFiles.upload_credit_timer ();
  DownloadIndexer.add_to_local_index_timer ()
  
let _ = 
  try
(*  DownloadClient.verbose := true; *)

(**** INSTALL HOOKS ****)
    
    DownloadInterface.install_hooks ();
    DownloadFiles.install_hooks ();



(**** LOAD OPTIONS ****)
    
    let exists_downloads_ini = Sys.file_exists 
        (options_file_name downloads_ini) in
    let exists_servers_ini = Sys.file_exists (options_file_name servers_ini) in
    let exists_files_ini = Sys.file_exists (options_file_name files_ini) in
    let exists_friends_ini = Sys.file_exists (options_file_name friends_ini) in
    if not exists_downloads_ini then begin
        Printf.printf "No config file found. Generating one."; 
        print_newline ();
        let oc = open_out (options_file_name downloads_ini) in
        close_out oc; 
      end;
    (try Options.load downloads_ini with e -> 
          Printf.printf "exception during options load"; print_newline ();
          exit 2;
          ());  

(**** PARSE ARGUMENTS ***)    
    
    Arg.parse ([
        "-client_ip", Arg.String (fun s ->
            client_ip =:= Ip.of_string s;
            force_client_ip =:= true;
            ip_verified := 10;
        ), " <ip> : force client IP address";
        "-exit", Arg.Unit (fun _ -> exit 0), ": exit immediatly";
        "-dump", Arg.String (fun file -> 
            Files.dump_file file), " <filename> : dump file";
        "-known", Arg.String (fun file ->
            let module K = Files.Known in
            let s = File.to_string file in
            let t = K.read s in
            K.print t;
            print_newline ();
        ), " <filename> : print a known.met file";
        "-part", Arg.String (fun file ->
            let module K = Files.Part in
            let s = File.to_string file in
            let t = K.read s in
            K.print t;
            print_newline ();
        ), " <filename> : print a .part.met file";
        "-server", Arg.String (fun file ->
            let module K = Files.Server in
            let s = File.to_string file in
            let t = K.read s in
            K.print t;
            print_newline ();
        ), " <filename> : print a server.met file";
        "-pref", Arg.String (fun file ->
            let module K = Files.Pref in
            let s = File.to_string file in
            let t = K.read s in
            K.print t;
            print_newline ();
        ), " <filename> : print a server.met file";
        "-format", Arg.String (fun file ->
            let format = DownloadMultimedia.get_info file in
            ()), " <filename> : check file format";
      ]@ (Options.simple_args downloads_ini) )(fun file -> 
        Files.dump_file file; exit 0
    ) "";


(**** CREATE DIRS   ****)
    
    safe_mkdir !!incoming_directory;
    safe_mkdir !!temp_directory;    

(**** LOAD OTHER OPTIONS ****)
    
    (try Options.load shared_files_ini with _ -> ());
    (try Options.load servers_ini with _ -> ());
    (try Options.load files_ini with _ -> ());
    (try Options.load friends_ini with _ -> ());
    (try Options.load searches_ini with _ -> ());
    if exists_downloads_ini && not exists_files_ini then begin
        Options.append files_ini (options_file_name downloads_ini);
        Options.save_with_help files_ini;
      end;
    if exists_downloads_ini && not exists_servers_ini then begin
        Options.append servers_ini (options_file_name downloads_ini);
        Options.save_with_help servers_ini;
      end;
    if exists_downloads_ini && not exists_friends_ini then begin
        Options.append friends_ini (options_file_name downloads_ini);
        Options.save_with_help friends_ini;
      end;
    Options.prune_file servers_ini;
    Options.prune_file files_ini;
    Options.prune_file downloads_ini;
    Options.prune_file friends_ini;
    Options.save_with_help searches_ini;
    
    features =:= !!features;  
    List.iter (fun file ->
        set_file_size file file.file_size) !!files;
    let list = ref [] in
    List.iter (fun file -> 
        try
          if Sys.file_exists file.file_hardname &&
            Unix32.getsize32 file.file_hardname <> Int32.zero then begin
              file.file_state <- FileDownloaded;
              (try
                  let format = DownloadMultimedia.get_info file.file_hardname
                  in
                  file.file_format <- format
                with _ -> ());        
              list := file :: !list
            end
        with _ -> ()
    ) !!done_files;
    done_files =:= List.rev !list;
    List.iter (fun c ->
        c.client_is_friend <- Friend;
    ) !!known_friends;
    
    let list = ref [] in
    List.iter (fun file ->
        if Sys.file_exists file.sh_name then begin
            Hashtbl.add shared_files_info file.sh_name file;
            list := file :: !list
          end
    ) !!known_shared_files;
    known_shared_files =:= !list;
    Options.save shared_files_ini;
    DownloadOneFile.add_shared_files !!incoming_directory;
    List.iter (DownloadOneFile.add_shared_files) !!shared_directories;
    DownloadIndexer.init ();

(**** CREATE WAITING SOCKETS ****)
    
    if !!http_port <> 0 then begin try
          ignore (create_http_handler ());
        with e ->
            Printf.printf "Exception %s while starting HTTP interface"
            (Printexc.to_string e);
            print_newline ();
      end;
    
    if !!telnet_port <> 0 then begin try
          ignore(TcpServerSocket.create !!telnet_port telnet_handler);  
        with e ->
            Printf.printf "Exception %s while starting telnet interface"
            (Printexc.to_string e);
            print_newline ();
      end;
    
    begin try
        udp_sock := Some (UdpSocket.create (!!port + 4) 
          (udp_handler DownloadFiles.udp_client_handler));
      with e ->
          Printf.printf "Exception %s while binding UDP socket"
            (Printexc.to_string e);
          print_newline ();
    end;
    
    if !!gui_port <> 0 then
      begin
        restart_gui_server := (fun _ ->
            begin
              match !gui_server_sock with
                None -> ()
              | Some sock ->
                  TcpServerSocket.close sock "gui server restart"
            end;
            gui_server_sock := Some 
              (TcpServerSocket.create !!gui_port gui_handler);  
        );
        try
          !restart_gui_server ();
        with e ->
            Printf.printf "Exception %s while starting GUI interface"
            (Printexc.to_string e);
            print_newline ();
      end;
    
    let sock = TcpServerSocket.create !!port client_connection_handler in
    
    begin
      match Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock)) with
        Unix.ADDR_INET (ip, port) ->
          client_port :=  port
      | _ -> failwith "Bad socket address"
    end;
    
    if not !!force_client_ip then begin
        ip_verified := 0;
        (try
            client_ip =:= Ip.from_name (Unix.gethostname ())
          with _ -> ());
      end;
    
    let port = !client_port in
    let sport = (port mod 256) * 256 + (port / 256) in
    
    begin
      try
        ignore (TcpServerSocket.create sport client_connection_handler);
      with _ ->
          Printf.printf "Unable to open Second listening port\n";
          Printf.printf "mldonkey will be enable to receive indirect\n";
          Printf.printf "connections from linux clients due to a bug\n";
          Printf.printf "in these clients.";
          print_newline ();
    end;
        
    client_tags :=
    [
      { tag_name = "name"; tag_value =  String !!client_name };
      { tag_name = "version"; tag_value =  Uint32 (Int32.of_int 
          Mftp_server.protocol) };
      { tag_name = "port"; tag_value =  Uint32 (Int32.of_int !client_port) };
    ];

    DownloadFiles.fill_clients_list ();
    

(**** START TIMERS ****)
    add_timer !!update_gui_delay         DownloadInterface.update_gui_info;  
    add_timer !!check_client_connections_delay DownloadFiles.check_locations;
    add_timer !!save_options_delay DownloadFiles.save_options;  
    add_timer !!check_connections_delay DownloadServers.check_server_connections;
    add_timer !!compute_md4_delay DownloadOneFile.check_files_md4s;  
    add_timer 5.0 DownloadServers.walker_timer;
    
    add_timer 3600. hourly_timer;
    add_timer (3600. *. 24.) daily_timer;    
    add_timer 30. halfmin_timer;
    add_timer 900. quarter_timer;
    add_timer 1. second_timer;
    add_timer 0.1 DownloadFiles.upload_timer;

    

(**** START PLAYING ****)  
    (try force_check_locations () with _ -> ());
    (try force_check_server_connections true with _ -> ());
    (try do_daily () with _ -> ());

(**** MAIN LOOP ****)  
    force_save_options ();
    
    Printf.printf "Welcome to MLdonkey client"; print_newline ();
    Printf.printf "Check http://go.to/mldonkey for updates"; 
    print_newline ();
    Printf.printf "To command: telnet 127.0.0.1 %d" !!telnet_port; 
    print_newline ();
    Printf.printf "Or with browser: http://127.0.0.1:%d" !!http_port; 
    print_newline ();
    BasicSocket.loop ()
  with e ->
      Printf.printf "Error: Exception %s during startup"
        (Printexc.to_string e); print_newline ();