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

open CommonInteractive
open CommonShared
open CommonTypes
open BasicSocket
open Options
open DriverInterface
open CommonOptions
open CommonGlobals
open CommonNetwork

  
let do_daily () =
  incr days;
  load_web_infos ()
    
let hourly_timer timer =
  incr hours;
  if !hours mod 24 = 0 then do_daily ();
  CommonShared.shared_check_files ();
  if !hours mod !!compaction_delay = 0 then Gc.compact ();
  DriverControlers.check_calendar ()

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
  add_infinite_option_timer update_gui_delay DriverInterface.update_gui_info

let save_mlsubmit_reg () =
  
(* Generate the mlsubmit.reg file *)
  
  let file = Printf.sprintf 
    
   "Windows Registry Editor Version 5.00

[HKEY_CLASSES_ROOT\\ed2k\\shell\\open\\command]
@=\"C:\\\\Program Files\\\\Internet Explorer\\\\IEXPLORE.EXE http://%s:%s@%s:%d/submit?q=dllink+%%1\""

    !!http_login !!http_password (Ip.to_string (client_ip None)) !!http_port
  in
  File.from_string "mlsubmit.reg" file;
    
(* Generate the mldonkey_submit file *)
  
  let file = Printf.sprintf 
  
"#!%s

# Submit an eDonkey download request to mldonkey
#
# Argument(s): An ed2k URI of the form:
#
# ed2k://|file|<filename>|<filesize>|<MD4-sum|
use LWP::UserAgent;

($#ARGV >= 0) || die \"Usage: mldonkey_submit <ed2kURI> ...\n\";

$vars{'HTTPURL'} = \"http://%s:%d\";
$vars{'HTTPUSER'} = \"%s\";
$vars{'HTTPPASS'} = \"%s\";

my $ua = LWP::UserAgent->new;

while (my $uri = shift @ARGV) {
	$_ = URI::Escape::uri_unescape($uri);
	if (/^ed2k:\\/\\/\\|file\\|[^|]+\\|(\\d+)\\|([\\dabcdef]+)\\|$/) {
		my $size = $1;
		my $md4 = $2;
		my $req = HTTP::Request->new(
			GET => \"$vars{'HTTPURL'}/submit?q=dllink+$uri\"
		);
		if (($vars{'HTTPUSER'}) && ($vars{'HTTPPASS'})) {
			$req->authorization_basic($vars{'HTTPUSER'},
				$vars{'HTTPPASS'});
		}
		my $response = $ua->request($req);
		if (!($response->is_success)) {
			print $response->error_as_HTML;
			exit 1;
		}
	} else {
		print \"Not an ed2k URI: $_\n\";
	}
}
" 
      Autoconf.perl_path
    (Ip.to_string (client_ip None)) !!http_port
    !!http_login !!http_password
  in
  File.from_string "mldonkey_submit" file;
  Unix.chmod  "mldonkey_submit" 0o755
    
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

  networks_iter_all (fun r -> 
      match r.network_config_file with
        None -> ()
      | Some opfile -> 
          try
            Options.load opfile
          with Sys_error _ ->
              Options.save_with_help opfile);
  
(**** PARSE ARGUMENTS ***)    

  let more_args = ref [] in
  
  more_args := !more_args
    @ (Options.simple_args downloads_ini);
  
  networks_iter_all (fun r ->
      match r.network_config_file with
        None -> ()
      | Some opfile ->
          let args = simple_args opfile in
          List.iter (fun prefix ->
              let args = List2.tail_map (fun (arg, spec, help) ->
                    (Printf.sprintf "-%s%s" prefix arg, spec, help)) args
              in
              more_args := !more_args @ args)
          r.network_prefixes;
  );
  
  Arg.parse ([
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
          let module K = DonkeyImport.Server in
          let s = File.to_string file in
          let t = K.read s in
          K.print t;
          print_newline ();
          exit 0
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
      "-check_impl", Arg.Unit (fun _ ->
          CommonNetwork.check_network_implementations ();
          CommonClient.check_client_implementations ();
          CommonServer.check_server_implementations ();
          CommonFile.check_file_implementations ();
          CommonResult.check_result_implementations ();
          print_newline ();
          exit 0), 
      " : display information on the implementations";
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
  Sys.set_signal  Sys.sighup
    (Sys.Signal_handle (fun _ ->
        Printf.printf "SIGUP"; print_newline ();
        networks_iter  network_disable;
        BasicSocket.print_sockets ();
        networks_iter  network_enable;
    ));
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
  
  add_infinite_option_timer download_sample_rate CommonFile.sample_timer;  
  
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
  
  add_infinite_option_timer save_options_delay (fun timer ->
      DriverInteractive.save_config ());  
  start_interfaces ();

  add_infinite_timer 3600. hourly_timer;
  shared_add_directory !!incoming_directory;
  List.iter shared_add_directory !!shared_directories;

  add_infinite_timer 300. (fun timer ->
      DriverInteractive.browse_friends ());
  
  Options.prune_file downloads_ini;

  (try load_web_infos () with _ -> ());
  
  Printf.printf "Welcome to MLdonkey client"; print_newline ();
  Printf.printf "Check http://go.to/mldonkey for updates"; 
  print_newline ();
  Printf.printf "To command: telnet 127.0.0.1 %d" !!telnet_port; 
  print_newline ();
  Printf.printf "Or with browser: http://127.0.0.1:%d" !!http_port; 
  print_newline ();

  save_mlsubmit_reg ();
  DriverInteractive.save_config ();
  
  BasicSocket.loop ()
