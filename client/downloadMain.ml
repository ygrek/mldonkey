open Options
open Mftp
open Mftp_comm
open DownloadServers
open BasicSocket
open TcpClientSocket
open DownloadOneFile
open DownloadFiles
open DownloadInteractive
open DownloadInterface  
open DownloadTypes  
open DownloadOptions
open DownloadGlobals
open DownloadClient
open Gui_types

  
    
let config_filename = "downloads.ini"

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
  DownloadInteractive.check_shared_files ()
  
let _ = 
  try
(*  DownloadClient.verbose := true; *)
    Arg.parse [
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
    ] (fun file -> 
        Files.dump_file file; exit 0
    ) "";
(**** CREATE DIRS   ****)
    
    safe_mkdir !!incoming_directory;
    safe_mkdir !!temp_directory;

(**** INSTALL HOOKS ****)
    
    DownloadInterface.install_hooks ();
    DownloadFiles.install_hooks ();



(**** LOAD OPTIONS ****)
    
    Options.set_options_file downloads_ini 
    (try Filepath.find_in_path ["."] config_filename with
        _ -> 
          Printf.printf "No config file found. Generating one."; 
          print_newline ();
          let name = Filename.concat "." config_filename in
          let oc = open_out name in close_out oc; name
    );
    (try Options.load downloads_ini with e -> 
          Printf.printf "exception during options load"; print_newline ();
          exit 2;
          ());  
    (try Options.load shared_files_ini with _ -> ());
    features =:= !!features;  
    List.iter (fun file ->
        set_file_size file file.file_size) !!files;
    let list = ref [] in
    List.iter (fun file -> 
        try
          if Sys.file_exists file.file_name &&
            Unix32.getsize32 file.file_name <> Int32.zero then begin
              file.file_state <- FileDownloaded;
              (try
                  let format = DownloadMultimedia.get_info file.file_name in
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
    DownloadIndexer.init ();
    
(**** CREATE WAITING SOCKETS ****)
    
    ignore (create_http_handler ());
    
    ignore(TcpServerSocket.create !!telnet_port telnet_handler);  
    udp_sock := Some (UdpSocket.create (!!port + 4) 
      (udp_handler DownloadFiles.udp_client_handler));
    
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
      !restart_gui_server ();
    end;
    
    let sock = TcpServerSocket.create !!port client_connection_handler in
    
    begin
      match Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock)) with
        Unix.ADDR_INET (ip, port) ->
          client_port :=  port
      | _ -> failwith "Bad socket address"
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
    
    client_ip := Ip.of_inet_addr
      (try
        Host.ip (Host.from_name (Unix.gethostname ()))
      with _ -> Host.local_ip
    );
    
    client_tags :=
    [
      { tag_name = "name"; tag_value =  String !!client_name };
      { tag_name = "version"; tag_value =  Uint32 (Int32.of_int 57) };
      { tag_name = "port"; tag_value =  Uint32 (Int32.of_int !client_port) };
    ];


(**** START TIMERS ****)
    add_timer !!update_gui_delay         DownloadInterface.update_gui_info;  
    add_timer !!check_client_connections_delay DownloadFiles.check_locations;
    add_timer !!save_options_delay DownloadFiles.save_options;  
    add_timer !!check_connections_delay DownloadServers.check_server_connections;
    add_timer 5.0 DownloadOneFile.check_files_md4s;  
    
    add_timer 3600. hourly_timer;
    
    add_timer 30. DownloadFiles.upload_credit_timer;
    
    add_timer 30. DownloadServers.update_master_servers;
    
    add_timer 1.0 DownloadFiles.reset_upload_timer;
    add_timer 0.1 DownloadFiles.upload_timer;

(**** START PLAYING ****)  
    (try force_check_locations () with _ -> ());
    (try force_check_server_connections true with _ -> ());

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