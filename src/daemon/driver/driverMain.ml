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
open CommonInteractive
open CommonShared
open CommonTypes
open BasicSocket
open Options
open DriverInterface
open CommonOptions
open CommonGlobals
open CommonNetwork

let keep_console_output = ref false
let daemon = ref false

let do_daily () =
  incr days;
  load_web_infos ()

let minute_timer () =
  CommonUploads.upload_credit_timer ();
  CommonInteractive.force_download_quotas ();
  if !!auto_commit then
    List.iter (fun file ->
        CommonComplexOptions.file_commit file
    ) !!CommonComplexOptions.done_files;
  MultinetMain.minute_timer ();
  ()
  
let hourly_timer timer =
  incr hours;
  if !hours mod 24 = 0 then do_daily ();
  CommonShared.shared_check_files ();
  if !hours mod !!compaction_delay = 0 then Gc.compact ();
  MultinetMain.hourly_timer ();
  DriverControlers.check_calendar ()

let second_timer timer =
  (try 
      update_link_stats () 
    with e -> 
        lprintf "Exception %s\n" (Printexc2.to_string e));
  (try 
      CommonUploads.refill_upload_slots ()
    with e -> 
        lprintf "Exception %s\n" (Printexc2.to_string e); );
  CommonGlobals.schedule_connections ();
  CommonUploads.reset_upload_timer ();
  CommonUploads.shared_files_timer ();
  MultinetMain.second_timer ()  
  
  
let start_interfaces () =
  
  
  if !!http_port <> 0 then begin try
        ignore (DriverControlers.create_http_handler ());
      with e ->
          lprintf "Exception %s while starting HTTP interface\n"
            (Printexc2.to_string e);
    end;
  
  ignore (find_port  "telnet server" !!telnet_bind_addr
      telnet_port DriverControlers.telnet_handler);  
  
  if !!chat_port <> 0 then begin
      ignore (find_port "chat server" !!chat_bind_addr
          chat_port DriverControlers.chat_handler);  
      try
        CommonChat.send_hello ()
      with _ -> lprintf "CommonChat.send_hello failed\n"; 
    end;

  gui_server_sock := find_port "gui server"  !!gui_bind_addr
    gui_port gui_handler;  
  if !!gift_port <> 0 then
    ignore (find_port "gift server"  !!gui_bind_addr
        gift_port gift_handler);

  add_infinite_option_timer update_gui_delay DriverInterface.update_gui_info;
  add_infinite_timer 1. second_timer


let _ =
  add_web_kind "motd.html" (fun filename ->
      lprintf "motd.html changed\n"; 
    motd_html =:= File.to_string filename
  );
  add_web_kind "motd.conf" (fun filename ->
    let ic = open_in filename in
    try
      while true do
	let line = input_line ic in
	let cmd, args = String2.cut_at line ' ' in
	let name, value = String2.cut_at args ' ' in
	match cmd with
	  "set" ->
            CommonInteractive.set_fully_qualified_options name value
	| "add_item" ->
            CommonInteractive.add_item_to_fully_qualified_options name value
	| "del_item" ->
            CommonInteractive.del_item_from_fully_qualified_options name value
	| _ -> 
	    lprintf "UNUSED LINE: %s\n" line; 
          
      done;
    with 
    | End_of_file ->
	close_in ic
    | e -> lprintf "Error while reading motd.conf(%s): %s\n" filename
	(Printexc2.to_string e); 
	close_in ic
			   )


let save_mlsubmit_reg () =
  
(* Generate the mlsubmit.reg file *)

  let file = Printf.sprintf 
    
   "Windows Registry Editor Version 5.00

[HKEY_CLASSES_ROOT\\ed2k]
@=\"URL: ed2k Protocol\"
\"URL Protocol\"=\"\"

[HKEY_CLASSES_ROOT\\ed2k\\shell]

[HKEY_CLASSES_ROOT\\ed2k\\shell\\open]

[HKEY_CLASSES_ROOT\\ed2k\\shell\\open\\command]
@=\"\\\"IEXPLORE.EXE\\\" \\\"http://%s:%s@%s:%d/submit?q=dllink+%%1\\\"\"
"

    "admin" "" (Ip.to_string (client_ip None)) !!http_port
  in
  File.from_string (Filename.concat file_basedir "mlsubmit.reg") file;
    
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
    "admin" ""
  in
  File.from_string (Filename.concat file_basedir "mldonkey_submit") file;
  Unix.chmod  (Filename.concat file_basedir "mldonkey_submit") 0o755
    
let load_config () =
  
  CommonGlobals.do_at_exit DriverInteractive.save_config;
  DriverInterface.install_hooks ();

(**** LOAD OPTIONS ****)
  
  let exists_downloads_ini = Sys.file_exists 
      (options_file_name downloads_ini) in
  if not exists_downloads_ini then begin
      lprintf "No config file found. Generating one.\n"; 
      let oc = open_out (options_file_name downloads_ini) in
      close_out oc; 
    end;
(*  let exists_expert_ini = Sys.file_exists 
      (options_file_name downloads_expert_ini) in

  if exists_expert_ini then begin
      if exists_downloads_ini then begin
          lprintf "Using old config file\n";
          (try Unix2.copy "downloads.ini" "downloads_expert.ini" with _ -> ());
          (try Unix2.copy "downloads.ini" "donkey.ini" with _ -> ());
          (try Unix2.copy "downloads.ini" "donkey_expert.ini" with _ -> ());
        end else begin          
          lprintf "No config file found. Generating one.\n"; 
          let oc = open_out (options_file_name downloads_expert_ini) in
          close_out oc; 
        end
    end; *)
  (try 
      Options.load downloads_ini;
      (try
          Options.append downloads_ini "downloads_expert.ini";
          Sys.remove "downloads_expert.ini"
          with _ -> ());
    with e -> 
        lprintf "Exception %s during options load\n" (Printexc2.to_string e); 
        exit 2;
        ());  
  CommonMessages.load_message_file ();
  if !!html_mods then begin
		if !!html_mods_style > 0 && !!html_mods_style < (Array.length !html_mods_styles) then
		commands_frame_height =:= (snd !html_mods_styles.(!!html_mods_style));
		CommonMessages.colour_changer ();
  end;
  networks_iter_all (fun r -> 
      List.iter (fun opfile ->
          try
            Options.load opfile
          with Sys_error _ ->
              Options.save_with_help opfile
      )      
      r.network_config_file 
  );
(* Here, we try to update options when a new version of mldonkey is
used. For example, we can add new web_infos... *)
  if !!options_version < 1 then begin
      lprintf "Updating options to level 1\n"; 
    (*  web_infos =:= 
        (
        ("server.met", 1, "http://ocbmaurice.dyns.net/pl/slist.pl?download");        
        ):: 
      !!web_infos;*)
    end;
  if !!options_version < 2 then begin
      lprintf "Updating options to level 2\n"; 
      if !!min_reask_delay = 720 then min_reask_delay =:= 600
    end;
  options_version =:= 2;
  
(**** PARSE ARGUMENTS ***)    

  let more_args = ref [] in
  
  
  more_args := !more_args
    @ (Options.simple_args downloads_ini);
  
  networks_iter_all (fun r ->
      List.iter (fun opfile ->
          let args = simple_args opfile in
          let prefix = r.network_prefix () in
          let args = List2.tail_map (fun (arg, spec, help) ->
                (Printf.sprintf "-%s%s" prefix arg, spec, help)) args
            in
          more_args := !more_args @ args
      ) r.network_config_file 
  );
  
  Arg.parse ([
      "-v", Arg.Unit (fun _ ->
          lprintf "%s\n" (CommonGlobals.version ());
          exit 0), " : print version number and exit";
      "-exit", Arg.Unit (fun _ -> exit 0), ": exit immediatly";
      "-format", Arg.String (fun file ->
          let format = CommonMultimedia.get_info file in
          ()), " <filename> : check file format";
      "-test_ip", Arg.String (fun ip ->
          lprintf "%s = %s\n" ip (Ip.to_string (Ip.of_string ip));
          exit 0), "<ip> : undocumented";
      "-check_impl", Arg.Unit (fun _ ->
          CommonNetwork.check_network_implementations ();
          CommonClient.check_client_implementations ();
          CommonServer.check_server_implementations ();
          CommonFile.check_file_implementations ();
          CommonResult.check_result_implementations ();
          lprint_newline ();
          exit 0), 
      " : display information on the implementations";
      "-stdout", Arg.Unit (fun _ ->
          keep_console_output := true;
          lprintf_output := Some stdout
      ), 
      ": keep output to stdout after startup";
      "-stderr", Arg.Unit (fun _ ->
          keep_console_output := true;
          lprintf_output := Some stderr;
      ), 
      ": keep output to stderr after startup";
      "-daemon", Arg.Set daemon,
      ": start as a daemon (detach from console and run in background";
      "-find_port", Arg.Set find_other_port, " : find another port when one
      is already used";
    ] @ 
      !more_args
      @
    !main_options)
    (fun file -> ()
(*      Files.dump_file file; exit 0 *)
  ) "";
    

(**** CREATE DIRS   ****)
  
  Unix2.safe_mkdir !!incoming_directory;
  Unix2.safe_mkdir !!temp_directory

let _ =  
  MlUnix.set_signal  Sys.sigchld (*Sys.Signal_ignore;*)
    (Sys.Signal_handle (fun _ ->
        lprintf "SIGCHLD\n"; ));
  MlUnix.set_signal  Sys.sighup
    (Sys.Signal_handle (fun _ ->
        lprintf "SIGHUP\n"; 
        BasicSocket.close_all ();
(*        BasicSocket.print_sockets (); *)
    ));
  MlUnix.set_signal  Sys.sigpipe (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ ->
        lprintf "SIGPIPE\n"; ));
  MlUnix.set_signal  Sys.sigint (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ ->
(*      if !CommonOptions.verbose then
           BasicSocket.print_sockets (); *)
        CommonGlobals.exit_properly 0));
  MlUnix.set_signal  Sys.sigterm (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ ->
(*      if !CommonOptions.verbose then
           BasicSocket.print_sockets (); *)
        CommonGlobals.exit_properly 0))

let _ =
  
  load_config ();
  
  add_infinite_option_timer download_sample_rate CommonFile.sample_timer;  
  
  (try Options.load servers_ini with _ -> ());
  (try Options.load files_ini with _ -> ());
  (try Options.load friends_ini with _ -> ());
  (try Options.load searches_ini with _ -> ());

  (try Options.load CommonUploads.M.shared_files_ini;
      let list = ref [] in
(* Normally, we should check that downloaded files are still there.
  
  *)    
      let list = ref [] in
      List.iter (fun file ->
          if Unix32.file_exists file.sh_name then begin
              Hashtbl.add CommonUploads.M.shared_files_info file.sh_name file;
              list := file :: !list
            end
      ) !!CommonUploads.M.known_shared_files;
      CommonUploads.M.known_shared_files =:= !list;
      with _ -> ());
  Options.save_with_help servers_ini;
  Options.save_with_help files_ini;
  Options.save_with_help friends_ini;
  Options.save_with_help searches_ini;
  Options.save_with_help CommonUploads.M.shared_files_ini;

  
  networks_iter (fun r -> network_load_complex_options r);
  networks_iter_all (fun r -> 
      lprintf  "Network %s %s\n" r.network_name
        (if network_is_enabled r then "enabled" else "disabled");
      );  
  networks_iter (fun r -> network_enable r);
  CommonOptions.start_running_plugins := true;
  CommonInteractive.force_download_quotas ();
  
  add_infinite_option_timer save_options_delay (fun timer ->
      DriverInteractive.save_config ());
  start_interfaces ();
  
  add_infinite_timer 60. minute_timer;
  add_infinite_timer 3600. hourly_timer;
  add_infinite_timer 0.1 CommonUploads.upload_download_timer;
  add_infinite_timer 1. CanBeCompressed.deflate_timer;

  shared_add_directory (!!incoming_directory, !!incoming_directory_prio);
  List.iter shared_add_directory !!shared_directories;  
  
  add_infinite_timer 1800. (fun timer ->
      DriverInteractive.browse_friends ());
  
  Options.prune_file downloads_ini;
  (try load_web_infos () with _ -> ());
  lprintf "Welcome to MLdonkey client\n"; 
  lprintf "Check http://www.mldonkey.net/ for updates\n"; 
  lprintf "To command: telnet %s %d\n" 
	(if !!telnet_bind_addr = Ip.any then "127.0.0.1" 
		else Ip.to_string !!telnet_bind_addr)  !!telnet_port; 
   lprintf "Or with browser: http://%s:%d/\n" 
 	(if !!http_bind_addr = Ip.any then "127.0.0.1" 
 		else Ip.to_string !!http_bind_addr)  !!http_port;   
  lprintf "%s\n" (DriverControlers.text_of_html !!motd_html);
  

  add_init_hook (fun _ ->
      if not !gui_included then
      (try
          let _ = Sys.getenv("DISPLAY") in
          if !!start_gui && Sys.file_exists !!mldonkey_gui then
            ignore (Sys.command (Printf.sprintf "%s &" !!mldonkey_gui))
          else
          let asker = Filename.concat !!mldonkey_bin "mlguistarter" in
          if !!ask_for_gui  && Sys.file_exists !!mldonkey_gui &&
            Sys.file_exists asker then
            ignore (Sys.command (Printf.sprintf "%s %s&" asker !!mldonkey_gui));
        with Not_found -> 
            lprintf "Not running under X, not trying to start the GUI\n";
            );
  );
 
  if !!run_as_user <> "" then begin
      try
        let new_pw = Unix.getpwnam !!run_as_user  in
        MlUnix.setuid new_pw.Unix.pw_uid;
        let pw = Unix.getpwuid (Unix.getuid()) in
        lprintf "mldonkey is now running as %s\n"  pw.Unix.pw_name;
      with e ->
          lprintf "Exception %s trying to set user_uid [%s]\n"
          (Printexc2.to_string e) !!run_as_user;
          exit 2
    end;
 
  if !!run_as_useruid <> 0 then begin
      try
        MlUnix.setuid !!run_as_useruid;
        lprintf "mldonkey is now running as uid %d\n"  !!run_as_useruid;
      with e ->
          lprintf "Exception %s trying to set user_uid [%d]\n"
          (Printexc2.to_string e) !!run_as_useruid;
          exit 2
    end;
  
  save_mlsubmit_reg ();
  DriverInteractive.save_config ();
  
  Unix32.max_cache_size := MlUnix.max_filedescs
 
let _ =
  lprintf "Core started\n"; 
  core_included := true;
  
  if not !keep_console_output then begin
      if !!log_file = "" then lprintf_to_stdout := false;


(* Question: is-it not to late to go in background ? Is-it possible that
we have started some threads already ? What happens then ? *)
      if !daemon then MlUnix.detach_daemon ()
    end    
