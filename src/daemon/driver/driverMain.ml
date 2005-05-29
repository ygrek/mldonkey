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
open Int64ops
open Printf2

open BasicSocket
open Autoconf  
open CommonInteractive
open CommonInteractive

open CommonDownloads
open CommonTypes
open CommonOptions
open CommonGlobals
open CommonNetwork
open CommonGraphics
  
open DriverInterface

module Dp500 = DriverLink.DP500(struct
      
      module CommonTypes = CommonTypes
      module CommonFile = CommonFile
      module CommonOptions = CommonOptions

      let incoming_directory () =
        (CommonComplexOptions.incoming_files ()).shdir_dirname
      let files () = !!CommonComplexOptions.files
      
    end)
  
open Gettext (* open last  as most modules redefine _s and _b *)
  
let _s x = _s "DriverMain" x
let _b x = _b "DriverMain" x
  
let keep_console_output = ref false
  
let do_daily () =
  incr CommonWeb.days;
  CommonWeb.load_web_infos ()

let minute_timer () =
  CommonUploads.upload_credit_timer ();
  CommonInteractive.force_download_quotas ();
  CommonResult.dummy_result.result_time <- last_time ();
  (try
      Int64Swarmer.verify_some_chunks ()
    with _ -> ()
  );
  
  if !!auto_commit then
    List.iter (fun file ->
        file_commit file
    ) !!CommonComplexOptions.done_files
  
let hourly_timer timer =
  incr CommonWeb.hours;
  if !CommonWeb.hours mod 24 = 0 then do_daily ();
  CommonShared.shared_check_files ();
  if !CommonWeb.hours mod !!compaction_delay = 0 then Gc.compact ();
  DriverControlers.check_calendar ();
  CommonWeb.connect_redirector ();
  CommonFile.propose_filenames ()
  

let second_timer timer =
  (try 
      update_link_stats () 
    with e -> 
        lprintf (_b "Exception %s\n") (Printexc2.to_string e));
	  (try 
     CommonUploads.refill_upload_slots ()
   with e -> 
        lprintf (_b "Exception %s\n") (Printexc2.to_string e));
  CommonUploads.reset_upload_timer ();
  CommonUploads.shared_files_timer ();
  ()
  
let start_interfaces () =
  
  
  if !!http_port <> 0 then begin try
        ignore (DriverControlers.create_http_handler ());
      with e ->
          lprintf (_b "Exception %s while starting HTTP interface\n")
            (Printexc2.to_string e);
    end;
  
  ignore (find_port  "telnet server" !!telnet_bind_addr
      telnet_port DriverControlers.telnet_handler);  

  Dp500.start ();
  
  if !!chat_port <> 0 then begin
      ignore (find_port "chat server" !!chat_bind_addr
          chat_port DriverControlers.chat_handler);  
      try
        CommonChat.send_hello ()
      with _ -> lprintf (_b "CommonChat.send_hello failed"); 
    end;
  
  gui_server_sock := find_port "gui server"  !!gui_bind_addr
    gui_port gui_handler;  
  if !!gift_port <> 0 then
    ignore (find_port "gift server"  !!gui_bind_addr
        gift_port gift_handler);
  
  add_infinite_option_timer update_gui_delay DriverInterface.update_gui_info;
  add_infinite_timer 1. second_timer


let _ =
  CommonWeb.add_web_kind "motd.html" (fun _ filename ->
      lprintf (_b "motd.html changed\n"); 
    motd_html =:= File.to_string filename
  );
  CommonWeb.add_web_kind "motd.conf" (fun _ filename ->
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
	    lprintf (_b "UNUSED LINE: %s\n") line
          
      done;
    with 
    | End_of_file ->
	close_in ic
    | e -> lprintf (_b "Error while reading motd.conf(%s): %s\n") filename
	(Printexc2.to_string e); 
	close_in ic
			   );
  CommonWeb.add_web_kind "guarding.p2p" (fun _ filename ->
      Ip_set.bl := Ip_set.load filename
(*      Ip_set.bl := Ip_set.load_merge !Ip_set.bl filename *)
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
    "admin" ""
  in
  File.from_string "mldonkey_submit" file;
  try
  Unix.chmod "mldonkey_submit" 0o755
  with
  e -> ()

let load_config () =
  
  DriverInterface.install_hooks ();

(**** LOAD OPTIONS ****)
  
  let exists_downloads_ini =
    Sys.file_exists (options_file_name downloads_ini) in
  let exists_users_ini =
    Sys.file_exists (options_file_name users_ini)
  in
  if not exists_downloads_ini then
    begin
      lprintf "No config file (downloads.ini) found. Generating one.\n"; 
      let oc = open_out (options_file_name downloads_ini) in
      close_out oc; 
      if not exists_users_ini then
        begin
          lprintf "No config file (users.ini) found. Generating one.\n"; 
          let oc = open_out (options_file_name users_ini) in
          close_out oc; 
        end;
    end
  else
    if not exists_users_ini then
      begin
        lprintf "No config file (users.ini) found. Importing users from downloads.ini.\n"; 
        ( try Unix2.copy "downloads.ini" "users.ini" with _ -> () );
      end;

(*
  let exists_expert_ini = Sys.file_exists 
      (options_file_name downloads_expert_ini) in
  if not exists_expert_ini then begin
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
      Options.load users_ini;
(*      Options.load downloads_expert_ini;       *)
    with e -> 
        lprintf "Exception %s during options load\n" (Printexc2.to_string e); 
        exit 2;
        ());  
  
  (* Here, we try to update options when a new version of mldonkey is
     used. For example, we can add new web_infos... *)
  CommonOptions.update_options ();

  CommonMessages.load_message_file ();
  if !!html_mods then begin
      if !!html_mods_style > 0 && !!html_mods_style < (Array.length !html_mods_styles) then
        commands_frame_height =:= (snd !html_mods_styles.(!!html_mods_style));
      CommonMessages.colour_changer ();
    end;
  networks_iter_all (fun r -> 
(*      lprintf "(n) loading network config file\n"; *)
      List.iter (fun opfile ->
          try
            Options.load opfile
          with Sys_error _ ->
              Options.save_with_help opfile
      )      
      r.network_config_file 
  );


(**** PARSE ARGUMENTS ***)    
  
  let more_args = ref [] in
  
  
  more_args := !more_args
    @ (Options.simple_args "" downloads_ini);
  more_args := !more_args
    @ (Options.simple_args "" users_ini);
  
  networks_iter_all (fun r ->
      List.iter (fun opfile ->
          let prefix = r.network_shortname ^ "-" in
          let args = simple_args prefix opfile in
          let args = List2.tail_map (fun (arg, spec, help) ->
                (Printf.sprintf "-%s" arg, spec, help)) args
          in
          more_args := !more_args @ args
      ) r.network_config_file 
  );
  
  Arg.parse ([
      "-v", Arg.Unit (fun _ ->
          lprintf "%s\n" (CommonGlobals.version ());
          exit 0), _s " : print version number and exit";
      "-exit", Arg.Unit (fun _ -> exit 0), ": exit immediatly";
      "-format", Arg.String (fun file ->
          let format = CommonMultimedia.get_info file in
          ()), _s  " <filename> : check file format";
      "-test_ip", Arg.String (fun ip ->
          lprintf "%s = %s\n" ip (Ip.to_string (Ip.of_string ip));
          exit 0), _s "<ip> : undocumented";
      "-check_impl", Arg.Unit (fun _ ->
          CommonNetwork.check_network_implementations ();
          CommonClient.check_client_implementations ();
          CommonServer.check_server_implementations ();
          CommonFile.check_file_implementations ();
(*          CommonResult.check_result_implementations (); *)
          lprint_newline ();
          exit 0), 
      _s " : display information on the implementations";
      "-stdout", Arg.Unit (fun _ ->
          keep_console_output := true;
          log_to_file stdout;
      ), 
       _s ": keep output to stdout after startup";
       "-stderr", Arg.Unit (fun _ ->
           keep_console_output := true;
           log_to_file stderr;
       ), 
       _s ": keep output to stderr after startup";
      "-daemon", Arg.Unit (fun _ ->
          (* Removed due to savannah bug #11514 . *)
          lprintf "\n\nOption -daemon was removed.\nUse 'mlnet > /dev/null 2>&1 &' instead. Exiting...\n";
          exit 0), _s " : this argument was removed, core will exit";
      "-find_port", Arg.Set find_other_port, 
      _s " : find another port when one is already used";
    ] @ 
      !more_args
      @
    !main_options)
    (fun file -> ()
(*      Files.dump_file file; exit 0 *)
  ) "";
    

(**** CREATE DIRS   ****)
  
  (*
  Unix2.safe_mkdir !!incoming_directory;
  
  File.from_string (Filename.concat !!incoming_directory "Readme.txt")
  "This directory contains the files downloaded, after commit.
The 'incoming/files/' folder contains simple files. 
The 'incoming/directories/' folder contains whole directories (probably
   downloaded with Bittorrent).

If you want to share files, you should:
  - Put simple files or directories where all files should be shared separately
   in the 'incoming/files/' folder, or better, in the shared/ folder.
   is for downloaded files. Files to be shared should be
   put in the 'shared/' directory instead.
  - Put directories that should be shared as one file in the 
   'incoming/directories/' folder. Currently, such directories can only be
   shared on the Bittorrent network, by providing the corresponding
   .torrent in the 'torrents/seeded/' folder.
";
  
  Unix2.safe_mkdir (Filename.concat !!incoming_directory "files");
  Unix2.safe_mkdir (Filename.concat !!incoming_directory "directories");
*)
  
  List.iter (fun s ->
      Unix2.safe_mkdir s.shdir_dirname;
  ) !!CommonComplexOptions.shared_directories;
  Unix2.safe_mkdir "searches";
  Unix2.safe_mkdir !!temp_directory

let _ =
  if Autoconf.system <> "windows" then 
    MlUnix.set_signal  Sys.sigchld
      (Sys.Signal_handle (fun _ -> lprintf "SIGCHLD\n"));

  if Autoconf.system <> "windows" then 
    MlUnix.set_signal  Sys.sighup
      (Sys.Signal_handle (fun _ -> lprintf "SIGHUP\n";
         BasicSocket.close_all ();
	 Unix32.close_all ();
         CommonGlobals.print_localtime ();));

  if Autoconf.system <> "windows" then
    MlUnix.set_signal  Sys.sigpipe
      (Sys.Signal_handle (fun _ -> lprintf "SIGPIPE\n"));

  MlUnix.set_signal  Sys.sigint
    (Sys.Signal_handle (fun _ -> lprintf "SIGINT\n";
        CommonGlobals.exit_properly 0));

  MlUnix.set_signal  Sys.sigterm
    (Sys.Signal_handle (fun _ -> lprintf "SIGTERM\n";
        CommonGlobals.exit_properly 0))

let _ =

  let t = Unix.localtime (Unix.time ()) in
  if (t.Unix.tm_year<=104) then
    begin
      lprintf "\n\n\nYour system has a system date earlier than 2004, please correct it.\n";
      lprintf "MLdonkey can not work with such a system date, exiting...\n";
      CommonGlobals.exit_properly 0
    end;

  (
    let hostname = "www.mldonkey.net" in
    try
      ignore(Ip.from_name hostname)
    with
     e ->
        lprintf "\nDNS resolution does not work! Looking up %s failed with %s.\n"
           hostname (Printexc2.to_string e);
	lprintf "The core therefore is unable to get eDonkey serverlists and loading\n";
	lprintf ".torrent files via dllink from websites is also impossible.\n\n";
	lprintf "If you are using MLDonkey in a chroot environment you should\n";
	lprintf "consider reading this article to get DNS support back:\n";
	lprintf "http://mldonkey.berlios.de/modules.php?name=Wiki&pagename=Chroot\n\n"
  );

  load_config ();
  
  add_infinite_option_timer download_sample_rate CommonFile.sample_timer;  

(*  lprintf "(1) CommonComplexOptions.load\n"; *)
  CommonComplexOptions.load ();
  CommonUploads.load ();

(*  lprintf "(2) CommonComplexOptions.load done\n"; *)
  begin
    let old_save_results = !!save_results in
    save_results =:= 0;
    CommonComplexOptions.save ();
    CommonUploads.save ();
    save_results =:= old_save_results;
  end;

(*  lprintf "(3) networks_iter load_complex_options\n"; *)
  networks_iter (fun r -> network_load_complex_options r);
  networks_iter_all (fun r -> 
      lprintf  (_b "Network %s %s\n") r.network_name
        (if network_is_enabled r then 
          (_s "enabled") else (_s "disabled"));
      );  
  networks_iter (fun r -> 
(*      lprintf "(4) networks_iter enabling\n"; *)
      network_enable r;
(* are there drawbacks to start recover_temp unconditionally here ? *)
      if !!recover_temp_on_startup then
        network_recover_temp r;
  );
  CommonOptions.start_running_plugins := true;
  CommonInteractive.force_download_quotas ();
  
  TcpBufferedSocket.set_max_opened_connections 
    (fun _ -> !!max_opened_connections);
  TcpBufferedSocket.set_max_connections_per_second 
    (fun _ -> !!max_connections_per_second);
  
  add_infinite_option_timer save_options_delay (fun timer ->
      DriverInteractive.save_config ());
  start_interfaces ();
  
  add_infinite_timer 60. minute_timer;
  add_infinite_timer 3600. hourly_timer;
  add_infinite_timer 0.1 CommonUploads.upload_download_timer;

  List.iter 
    CommonShared.shared_add_directory
  !!CommonComplexOptions.shared_directories;  
  
  add_infinite_timer 1800. (fun timer ->
      DriverInteractive.browse_friends ());
  
  Options.prune_file downloads_ini;
  Options.prune_file users_ini;
(*  Options.prune_file downloads_expert_ini; *)
  add_timer 20. (fun _ -> try CommonWeb.load_web_infos () with _ -> ());
  lprintf (_b "\nWelcome to MLdonkey client\n"); 
  lprintf (_b "Check http://www.mldonkey.net/ for updates\n"); 
  lprintf (_b "To command: telnet %s %d\n") 
	(if !!telnet_bind_addr = Ip.any then "127.0.0.1" 
		else Ip.to_string !!telnet_bind_addr)  !!telnet_port; 
  lprintf (_b "Or with browser: http://%s:%d\n") 
	(if !!http_bind_addr = Ip.any then "127.0.0.1" 
		else Ip.to_string !!http_bind_addr)  !!http_port; 
  
  lprint_string (DriverControlers.text_of_html !!motd_html);
  lprint_newline ();
  
  if Autoconf.system <> "windows" then
    (* Doesn't work on windows with mingw, because getpid allways returns 948 *)
    (
      let oc = open_out "mlnet.pid" in
      output_string oc ( Printf.sprintf "%s\n" ( string_of_int ( Unix.getpid () ) ) );
      close_out oc;
      CommonGlobals.do_at_exit (fun _ -> Sys.remove "mlnet.pid" );
      lprintf "Starting with pid %s\n" ( string_of_int ( Unix.getpid () ) )
    );

  add_init_hook (fun _ ->
      if not !gui_included && ( !!start_gui || !!ask_for_gui ) then
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
            lprintf (_b "Not running under X, not trying to start the GUI\n")
            );
  );
 
  if !!run_as_user <> "" then begin
      try
        let new_pw = Unix.getpwnam !!run_as_user  in
        MlUnix.setuid new_pw.Unix.pw_uid;
        let pw = Unix.getpwuid (Unix.getuid()) in
        lprintf (_b "mldonkey is now running as %s\n")  pw.Unix.pw_name;
      with e ->
          lprintf (_b "Exception %s trying to set user_uid [%s]\n")
          (Printexc2.to_string e) !!run_as_user;
          exit 2
    end;
 
  if !!run_as_useruid <> 0 then begin
      try
        MlUnix.setuid !!run_as_useruid;
        lprintf (_b "mldonkey is now running as uid %d\n")  !!run_as_useruid;
      with e ->
          lprintf (_b "Exception %s trying to set user_uid [%d]\n")
          (Printexc2.to_string e) !!run_as_useruid;
          exit 2
    end;
  
  if !!create_mlsubmit then save_mlsubmit_reg ();
  DriverInteractive.initialization_completed := true;
  DriverInteractive.save_config ();
  
  Unix32.max_cache_size := MlUnix.max_filedescs
 
let _ =
  lprintf (_b "Core started"); 
  core_included := true;
  CommonGlobals.print_localtime ();
  CommonGlobals.do_at_exit (fun _ -> CommonGlobals.print_localtime ());
  CommonGlobals.do_at_exit (fun _ -> lprintf "Core stopped");

  let security_space_filename = "config_files_space.tmp" in  
  begin
(* Create a 'config_files_security_space' megabytes file to protect some space
for config files at the end. *)
    try
      let security_space_fd = Unix32.create_rw security_space_filename in
      let _ =
        let len = 32768 in
        let len64 = Int64.of_int len in
        let s = String.create len in
        let pos = ref zero in
        for i = 1 to !!config_files_security_space do
          for j = 1 to 32 do (* 32 = 1 MB / 32kB *)
            Unix32.write security_space_fd !pos s 0 len;
            pos := !pos ++ len64
          done
        done
      in
      Unix32.close security_space_fd;
    with _ ->
        lprintf (_b "Cannot create Security space file:\n");
        lprintf (_b " not enough space on device or bad permissions\n");
        lprintf (_b "Exiting...\n");
        exit 2;
  end;  
  CommonGlobals.do_at_exit (fun _ -> 
      (* If we have an error with too many file-descriptors,
         just close all of them *)
      (try
         BasicSocket.close_all ();
       with e ->
           lprintf "Exception %s in do_at_exit while closing sockets.\n"
             (Printexc2.to_string e);
      );
      CommonGraphics.remove_files ();
      (* In case we have no more space on filesystem for
         config files, remove the security space file *)
      Sys.remove security_space_filename;
      DriverInteractive.save_config ();

      CommonGlobals.print_localtime ();
      lprintf (_b "Core stopped\n")
    );
  
  if not !keep_console_output then begin
      lprintf (_b "Disabling output to console, to enable: stdout true\n");
      
      if !!log_file <> "" then begin
          (*
          try
            Printf.printf "+XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX+\n";
            let oc = open_out !!log_file in
            lprintf "Logging in %s\n" !!log_file;
            
            (* Don't close stdout !!!
            (match !lprintf_output with
               None -> () | Some oc -> close_out oc);
*)
            log_to_file oc;
            lprintf "Start log in %s\n" !!log_file;
          with e ->
              lprintf "Exception %s while opening log file: %s\n"
(Printexc2.to_string e) !!log_file *)
          ()
        end else
              close_log ()
    end
