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
open CommonTypes
open BasicSocket
open Options
open DriverInterface
open CommonOptions
open CommonGlobals
open CommonNetwork
    
let start_interfaces () =
  
  if !!http_port <> 0 then begin try
        ignore (DriverControlers.create_http_handler ());
      with e ->
          Printf.printf "Exception %s while starting HTTP interface"
            (Printexc.to_string e);
          print_newline ();
    end;
  
  if !!telnet_port <> 0 then begin try
        ignore(TcpServerSocket.create "telnet server"
          (Ip.to_inet_addr !!telnet_bind_addr)
          !!telnet_port DriverControlers.telnet_handler);  
      with e ->
          Printf.printf "Exception %s while starting telnet interface"
            (Printexc.to_string e);
          print_newline ();
    end;

  if !!chat_port <> 0 then 
    begin
      try
        ignore(TcpServerSocket.create "chat server"
		 (Ip.to_inet_addr !!chat_bind_addr)
		 !!chat_port DriverControlers.chat_handler);  
	(** Send hello to the chat app *)
	CommonChat.send_hello ()
      with e ->
          Printf.printf "Exception %s while starting chat interface"
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
            (TcpServerSocket.create "gui server" (Ip.to_inet_addr !!gui_bind_addr)
            !!gui_port gui_handler);  
      );
      try
        !restart_gui_server ();
      with e ->
          Printf.printf "Exception %s while starting GUI interface"
            (Printexc.to_string e);
          print_newline ();
    end  ;
  add_timer !!update_gui_delay DriverInterface.update_gui_info
  
let load_config () =

  CommonGlobals.do_at_exit DriverInteractive.save_config;
  DriverInterface.install_hooks ();

(**** LOAD OPTIONS ****)
  
  let exists_downloads_ini = Sys.file_exists 
      (options_file_name downloads_ini) in
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

  networks_iter_all (fun r -> network_load_simple_options r);
  
(**** PARSE ARGUMENTS ***)    

  let more_args = ref [] in
  
  more_args := !more_args
    @ (Options.simple_args downloads_ini);
  
  networks_iter_all (fun r ->
      more_args := !more_args @ network_prefixed_args r);
  
  Arg.parse ([
      "-client_ip", Arg.String (fun s ->
          client_ip =:= Ip.of_string s;
          force_client_ip =:= true;
          ip_verified := 10;
      ), " <ip> : force client IP address";
      "-v", Arg.Unit (fun _ ->
          Printf.printf "%s" CommonGlobals.version;
          print_newline ();
          exit 0), " : print version number and exit";
      "-exit", Arg.Unit (fun _ -> exit 0), ": exit immediatly";
(*
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
  *)
      "-format", Arg.String (fun file ->
          let format = CommonMultimedia.get_info file in
          ()), " <filename> : check file format";
      "-test_ip", Arg.String (fun ip ->
          Printf.printf "%s = %s" ip (Ip.to_string (Ip.of_string ip));
          print_newline ();
          exit 0), "<ip> : undocumented";
    ] @ !more_args)
    (fun file -> ()
(*      Files.dump_file file; exit 0 *)
  ) "";

  networks_iter (fun r -> network_load_complex_options r);
    

(**** CREATE DIRS   ****)
  
  Unix2.safe_mkdir !!incoming_directory;
  Unix2.safe_mkdir !!temp_directory

  
let _ =  
  Sys.set_signal  Sys.sigchld (*Sys.Signal_ignore;*)
    (Sys.Signal_handle (fun _ ->
        Printf.printf "SIGCHLD"; print_newline ()));
  Sys.set_signal  Sys.sigpipe (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ ->
        Printf.printf "SIGPIPE"; print_newline ()));
  Sys.set_signal  Sys.sigint (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ ->
        if !!CommonOptions.verbose then
          BasicSocket.print_sockets ();
        DriverInteractive.save_config ();
        exit 0));
  load_config ();
  
  
  (try Options.load servers_ini with _ -> ());
  (try Options.load files_ini with _ -> ());
  (try Options.load friends_ini with _ -> ());
  (try Options.load searches_ini with _ -> ());

  Options.save_with_help servers_ini;
  Options.save_with_help files_ini;
  Options.save_with_help friends_ini;
  Options.save_with_help searches_ini;

  networks_iter_all (fun r -> 
      Printf.printf "Network %s %s" r.network_name
        (if network_is_enabled r then "enabled" else "disabled");
      print_newline () );
  networks_iter (fun r -> network_enable r);
  
  add_timer !!save_options_delay (fun timer ->
      reactivate_timer timer;
      DriverInteractive.save_config ());  
  start_interfaces ();

  add_timer 3600. (fun timer ->
      reactivate_timer timer;
      CommonShared.shared_check_files ());
  shared_add_directory !!incoming_directory;
  List.iter shared_add_directory !!shared_directories;

  
  Options.prune_file downloads_ini;
  
  Printf.printf "Welcome to MLdonkey client"; print_newline ();
  Printf.printf "Check http://go.to/mldonkey for updates"; 
  print_newline ();
  Printf.printf "To command: telnet 127.0.0.1 %d" !!telnet_port; 
  print_newline ();
  Printf.printf "Or with browser: http://127.0.0.1:%d" !!http_port; 
  print_newline ();

  DriverInteractive.save_config ();
  
  BasicSocket.loop ()
