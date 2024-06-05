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

open CommonInteractive

open CommonTypes
open CommonOptions
open CommonUserDb
open CommonGlobals
open CommonNetwork

open DriverInterface

open Gettext (* open last  as most modules redefine _s and _b *)

let _s x = _s "DriverMain" x
let _b x = _b "DriverMain" x

let log_prefix = "[dMain]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let pid = ref ""

let do_daily () =
  incr CommonWeb.days

let minute_timer () =
  DriverInteractive.hdd_check ();
  CommonUploads.upload_credit_timer ();
  CommonInteractive.force_download_quotas ();
  CommonResult.dummy_result.result_time <- last_time ();
  (try
    CommonSwarming.verify_some_chunks ()
  with _ -> ());
  CommonClient.clear_upload_slots ()

let hourly_timer timer =
  incr CommonWeb.hours;
  CommonWeb.load_web_infos false false;
  if !CommonWeb.hours mod !!compaction_delay = 0 then Gc.compact ();
  if !!backup_options_delay <> 0
     && !CommonWeb.hours mod !!backup_options_delay = 0 then
       CommonComplexOptions.backup_options ();
  DriverControlers.check_calendar ();
  CommonFile.propose_filenames ()

let ten_second_timer timer =
  if !!auto_commit then
    List.iter (fun file ->
        file_commit file
    ) !!CommonComplexOptions.done_files

let second_timer timer =
  (try
      update_link_stats ()
    with e ->
        lprintf_nl (_b "Exception %s") (Printexc2.to_string e));
  (try
     CommonUploads.refill_upload_slots ()
   with e ->
        lprintf_nl (_b "Exception %s") (Printexc2.to_string e));
  CommonUploads.shared_files_timer ();
  ()

let start_interfaces () =

  (* option_hook(s) are not called when ini files are created the first time
     force re-load of allowed_ips to call option_hook which fills the IP blocklist *)
  ( 
  match !created_new_base_directory with
    None -> ()
  | Some dir -> allowed_ips =:= !!allowed_ips
  );

  if !!http_port <> 0 then begin 
      try
        ignore (DriverControlers.create_http_handler ());
      with e ->
          lprintf_nl (_b "Exception %s while starting HTTP interface")
            (Printexc2.to_string e);
    end;

  if !!telnet_port <> 0 then begin 
      try
        ignore (find_port "telnet server" !!telnet_bind_addr
          telnet_port DriverControlers.telnet_handler);
      with e ->
          lprintf_nl (_b "Exception %s while starting Telnet interface")
            (Printexc2.to_string e);
    end;

  if !!gui_port <> 0 then begin 
      try
        ignore (find_port "gui server" !!gui_bind_addr
          gui_port gui_handler);
      with e ->
          lprintf_nl (_b "Exception %s while starting GUI interface")
            (Printexc2.to_string e);
    end;

  if !!gift_port <> 0 then begin
      try
        ignore (find_port "gift server" !!gui_bind_addr
          gift_port gift_handler);
      with e ->
          lprintf_nl (_b "Exception %s while starting GUI interface")
            (Printexc2.to_string e);
    end;

  add_infinite_option_timer update_gui_delay DriverInterface.update_gui_info;
  add_infinite_timer 1. second_timer


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
  Unix2.chmod (Filename.concat file_basedir "mldonkey_submit") 0o755

let load_config () =

  DriverInterface.install_hooks ();

(**** LOAD OPTIONS ****)

  let exists_downloads_ini =
    Sys.file_exists (options_file_name downloads_ini) in
  let exists_users_ini =
    Sys.file_exists (options_file_name users_ini)
  in
  if not exists_users_ini && exists_downloads_ini then
    begin
      lprintf_nl "No config file (users.ini) found. Importing users from downloads.ini.";
      ( try Unix2.copy "downloads.ini" "users.ini" with _ -> () );
    end;

  let ini_files_exist = Sys.file_exists (options_file_name downloads_ini) in

  (try
      Options.load downloads_ini;
      Options.load users_ini;
      DriverInteractive.hdd_check ()
    with e ->
        lprintf_nl "Exception %s during options load" (Printexc2.to_string e);
        exit 70);

  (* Here, we try to update options when a new version of mldonkey is
     used. For example, we can add new web_infos... *)
  CommonOptions.update_options ();

  CommonMessages.load_message_file ();
  if !!html_mods then begin
      if !!html_mods_style > 0 && !!html_mods_style < Array.length CommonMessages.styles then
        commands_frame_height =:= CommonMessages.styles.(!!html_mods_style).CommonMessages.frame_height;
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
          lprintf_nl "%s" (CommonGlobals.version ());
          exit 0), _s " : print version number and exit";
      "-exit", Arg.Unit (fun _ -> exit 0), ": exit immediatly";
      "-format", Arg.String (fun file ->
          ignore (CommonMultimedia.get_info file)),
          _s  " <filename> : check file format";
      "-test_ip", Arg.String (fun ip ->
          lprintf_nl "%s = %s" ip (Ip.to_string (Ip.of_string ip));
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
          lprintf_original_output := (Some stdout);
          log_to_file stdout
      ),
       _s ": keep output to stdout after startup";
       "-stderr", Arg.Unit (fun _ ->
          lprintf_original_output := (Some stderr);
          log_to_file stderr
       ),
       _s ": keep output to stderr after startup";
      "-daemon", Arg.Unit (fun _ ->
          (* Removed due to savannah bug #11514 . *)
          lprintf_nl "\n\nOption -daemon was removed.\nUse 'mlnet > /dev/null 2>&1 &' instead. Exiting...";
          exit 64), _s " : this argument was removed, core will exit";
      "-find_port", Arg.Set find_other_port,
      _s " : find another port when one is already used";
      "-pid", Arg.String (fun s -> pid := s;
      ),
       _s ": directory for pid file";
      "-useradd", Arg.Rest (fun s ->
        (match String2.split s ' ' with
        | user :: pass :: _ ->
            if user2_user_exists user then
              begin
                user2_user_set_password (user2_user_find user) pass;
                Printf.printf "%sPassword of user %s changed\n%!" (log_time ()) user
              end
            else
              begin
                user2_user_add user (Md4.Md4.string pass) ();
                Printf.printf "%sUser %s added\n%!" (log_time ()) user
              end;
            Options.save_with_help_private users_ini;
            Printf.printf "%sSaved changes to users.ini\n%!" (log_time ())
        | _ -> raise (Arg.Bad "invalid syntax"));
          exit 0), _s "\"<user> <pass>\" : create user/change password";
    ] @
      !more_args
      @
    !main_options)
    (fun file -> ()
(*      Files.dump_file file; exit 0 *)
  ) "";

  if not ini_files_exist && not (keep_console_output ()) then log_file =:= "mlnet.log";

  (**** CREATE DIRS   ****)

  List.iter (fun s ->
      Unix2.safe_mkdir s.shdir_dirname;
      if s.shdir_strategy = "incoming_directories" ||
         s.shdir_strategy = "incoming_files" then
        Unix2.can_write_to_directory s.shdir_dirname
  ) !!CommonComplexOptions.shared_directories;
  Unix2.safe_mkdir "searches";
  Unix2.can_write_to_directory "searches";
  Unix2.safe_mkdir "web_infos";
  Unix2.can_write_to_directory "web_infos";
  Unix2.safe_mkdir !!temp_directory;
  Unix2.can_write_to_directory !!temp_directory

let _ =

  let t = Unix.localtime (Unix.time ()) in
  if (t.Unix.tm_year<=104) then
    begin
      lprintf_nl (_b "\n\n\nYour system has a system date earlier than 2004, please correct it.");
      lprintf_nl (_b "MLdonkey can not work with such a system date, exiting...");
      CommonGlobals.exit_properly 71
    end;

  ( let resolve_name hostname =
      try
        ignore (Ip.from_name hostname);
        true
      with _ -> false
    in
    let hostnames =
      ["mldonkey.sf.net"; "www.google.com"]
    in
    DriverInteractive.dns_works := List.exists resolve_name hostnames;

    if not !DriverInteractive.dns_works then lprintf "
The core therefore is unable to get eDonkey serverlists and loading
.torrent files via dllink from websites is also impossible.
If you are using MLDonkey in a chroot environment you should
consider reading this article to get DNS support back:
http://mldonkey.sourceforge.net/Chroot\n\n");
  (
    let real_glibc_version = MlUnix.glibc_version_num () in
      if real_glibc_version <> Autoconf.glibc_version
         && real_glibc_version <> "" then
        lprintf (_b"
Attention!
This core is running with glibc %s but it was compiled with glibc %s.
This can lead to unexpected behaviour. Consider compiling the core yourself
or getting a binary compiled with glibc %s.\n\n")
          real_glibc_version Autoconf.glibc_version Autoconf.glibc_version
  );
  (
    if Autoconf.magic then begin
      (if Sys.file_exists "./magic/magic" then
         try Unix.putenv "MAGIC" "./magic/magic" with _ -> ());
      if Magic.M.magic_works () then
        begin
          Autoconf.magic_works := true;
          lprintf_nl (_b "Libmagic file-type recognition database present")
        end
      else
        begin
          Autoconf.magic_works := false;
          lprintf_nl (_b "Libmagic file-type recognition database not present")
        end
      end
  );
  if not !Charset.Locale.conversion_enabled then
    lprintf_nl (_b "Self-test failed, charset conversion disabled.");

  load_config ();
  
  add_infinite_option_timer download_sample_rate CommonFile.sample_timer;

(*  lprintf "(1) CommonComplexOptions.load\n"; *)
  CommonComplexOptions.load ();
  CommonUploads.load ();
  CommonStats.load ();

(*  lprintf "(2) CommonComplexOptions.load done\n"; *)
  begin
    let old_save_results = !!save_results in
    save_results =:= 0;
    CommonComplexOptions.save ();
    CommonUploads.save ();
    save_results =:= old_save_results;
  end;

  CommonGlobals.is_startup_phase := false;

(* before activating network modules load all local files from web_infos/
   to avoid security holes, especially for IP blocking *)
  Hashtbl.iter (fun key w ->
    let file = Filename.concat "web_infos" (Filename.basename w.url) in
    if Sys.file_exists file then
      try
        lprintf_nl "loading %s from %s" w.kind file;
        ((List.assoc w.kind !CommonWeb.file_kinds).f w.url) file;
        w.state <- Some FileLoaded;
      with _ -> ()
  ) web_infos_table;

  discover_ip false;

  lprintf_nl (_b "Check http://mldonkey.sf.net for updates");
  networks_iter (fun r -> network_load_complex_options r);
  lprintf_nl (_b "enabling networks: ");
  if (upnp_port_forwarding ()) then
    UpnpClient.init_maps ();
    let add_upnp_port p s=
      lprintf_nl "using port %d (%s)" p s;
      if ((upnp_port_forwarding ())) then
        match String2.split_simplify s ' ' with
          | [ "client_port" ; tcpudp   ]
          | [ "tracker_port" ; tcpudp   ]
          | [ "dht_port" ; tcpudp      ]
          | [ "overnet_port" ; tcpudp  ]
          | [ "kademlia_port" ; tcpudp ] ->
          if (String2.contains tcpudp "TCP") then 
            begin
            UpnpClient.maps_add_item 1 p p 1 "" ;
            lprintf_nl "add upnp port forwarding %d TCP" p;
            end;
          if (String2.contains tcpudp "UDP") then
            begin
            UpnpClient.maps_add_item 1 p p 0 "" ;
            lprintf_nl "add upnp port forwarding %d UDP" p;
            end
          | _ -> ()
  in
  networks_iter (fun r ->
      lprintf_nl (_b "---- enabling %s ----") r.network_name;
      network_enable r;
      List.iter (fun (p,s) -> if p <> 0 then add_upnp_port p s) (network_ports r);
(* are there drawbacks to start recover_temp unconditionally here ? *)
      if !!recover_temp_on_startup then
        network_recover_temp r;
  );
  lprintf_nl (_b "---- enabling interfaces ----");
  List.iter (fun (p,s) -> if p <> 0 then lprintf_nl "using port %d (%s)" p s)
    (network_ports (network_find_by_name "Global Shares"));
  lprintf (_b "%s[dMain] disabled networks: ") (log_time ());
  let found = ref false in
    networks_iter_all (fun r ->
        if not (network_is_enabled r) then
          begin
            found := true;
            lprintf (_b "%s ") r.network_name
          end);
  if not !found then lprintf (_b "none");
  lprint_newline ();
  if (upnp_port_forwarding ()) then
    UpnpClient.job_start ();
  networks_iter_all (fun n -> network_update_options n);
  CommonOptions.start_running_plugins := true;
  CommonInteractive.force_download_quotas ();

  TcpBufferedSocket.set_max_connections_per_second
    (fun _ -> !!max_connections_per_second);

  add_infinite_option_timer save_options_delay (fun timer ->
      DriverInteractive.save_config ());
  start_interfaces ();

  add_infinite_timer 60. minute_timer;
  add_infinite_timer 10. ten_second_timer;
  add_infinite_timer 3600. hourly_timer;
  add_infinite_timer 0.1 CommonUploads.upload_download_timer;
  add_infinite_timer !!buffer_writes_delay (fun _ -> Unix32.flush ());

  if !!share_scan_interval <> 0 then
  add_infinite_timer ((float_of_int !!share_scan_interval) *. 60.)
    (fun _ -> CommonShared.shared_check_files ());
  CommonShared.shared_check_files ();

  history_timeflag := (Unix.time()); 
  update_download_history (); 
  update_upload_history ();
  history_h_timeflag := (Unix.time()); 
  update_h_download_history (); 
  update_h_upload_history ();
  history_size_for_h_graph := history_size * !!html_mods_vd_gfx_h_intervall / 60;
  history_h_step := 60 * !!html_mods_vd_gfx_h_intervall;
        
  add_infinite_timer (float_of_int history_step) (fun timer -> 
    history_timeflag := (Unix.time()); 
    update_download_history (); 
    update_upload_history ());

  add_infinite_timer (float_of_int !history_h_step) (fun timer -> 
    history_h_timeflag := (Unix.time()); 
    update_h_download_history (); 
    update_h_upload_history ());
                
  if Autoconf.system = "mingw" then
    add_infinite_timer 1. (fun timer ->
        MlUnix.set_console_title (DriverInteractive.console_topic ()));

  List.iter
    CommonShared.shared_add_directory
  !!CommonComplexOptions.shared_directories;

  add_infinite_timer 1800. (fun timer ->
      DriverInteractive.browse_friends ());

  Options.prune_file downloads_ini;
  Options.prune_file users_ini;
  add_timer 1. (fun _ -> try CommonWeb.load_web_infos true false with _ -> ());
  if !!telnet_port <> 0 then lprintf_nl  (_b "To command: telnet %s %d")
        (if !!telnet_bind_addr = Ip.any then "127.0.0.1"
                else Ip.to_string !!telnet_bind_addr)  !!telnet_port;
  if !!http_port <> 0 then begin
    lprintf_nl  (_b "Or with browser: http://%s:%d")
        (if !!http_bind_addr = Ip.any then "127.0.0.1"
                else Ip.to_string !!http_bind_addr)  !!http_port;
    lprintf_nl  (_b "For a GUI check out http://sancho-gui.sourceforge.net")
  end;
  if !!gui_port <> 0 then lprintf_nl  (_b "Connect to IP %s, port %d")
        (if !!gui_bind_addr = Ip.any then "127.0.0.1"
                else Ip.to_string !!gui_bind_addr)  !!gui_port;
  lprintf_nl  (_b "If you connect from a remote machine adjust allowed_ips");
  if Autoconf.system = "cygwin" && not (keep_console_output ()) then lprintf (_b "%s") win_message;

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
            lprintf_nl (_b "Not running under X, not trying to start the GUI")
            );
  );

  if !!run_as_group <> "" then begin
      try
        let new_gr = Unix.getgrnam !!run_as_group in
        MlUnix.setgid new_gr.Unix.gr_gid;
        let gr = Unix.getgrgid (Unix.getgid()) in
        lprintf_nl (_b "mldonkey is now running as group %s") gr.Unix.gr_name;
      with e ->
          lprintf_nl (_b "Exception %s trying to set group_gid [%s]")
          (Printexc2.to_string e) !!run_as_group;
          exit 67
    end;

  if !!run_as_groupgid <> 0 then begin
      try
        MlUnix.setgid !!run_as_groupgid;
        lprintf_nl (_b "mldonkey is now running as gid %d")  !!run_as_groupgid;
      with e ->
          lprintf_nl (_b "Exception %s trying to set group_gid [%d]")
          (Printexc2.to_string e) !!run_as_groupgid;
          exit 67
    end;

  if !!run_as_user <> "" then begin
      try
        let new_pw = Unix.getpwnam !!run_as_user in
        MlUnix.setuid new_pw.Unix.pw_uid;
        let pw = Unix.getpwuid (Unix.getuid()) in
        lprintf_nl (_b "mldonkey is now running as user %s") pw.Unix.pw_name;
      with e ->
          lprintf_nl (_b "Exception %s trying to set user_uid [%s]")
          (Printexc2.to_string e) !!run_as_user;
          exit 67
    end;

  if !!run_as_useruid <> 0 then begin
      try
        MlUnix.setuid !!run_as_useruid;
        lprintf_nl (_b "mldonkey is now running as uid %d")  !!run_as_useruid;
      with e ->
          lprintf_nl (_b "Exception %s trying to set user_uid [%d]")
          (Printexc2.to_string e) !!run_as_useruid;
          exit 67
    end;

  if !!create_mlsubmit then save_mlsubmit_reg ();
  DriverInteractive.initialization_completed := true;
  DriverInteractive.save_config ();

  if not Autoconf.windows then
    MlUnix.set_signal  Sys.sigchld
      (Sys.Signal_handle (fun _ -> if !verbose then lprintf_nl (_b "Received SIGCHLD, doing nothing")));

  if not Autoconf.windows then
    MlUnix.set_signal  Sys.sighup
      (Sys.Signal_handle (fun _ -> lprintf_nl (_b "Received SIGHUP, closing all files and client/server sockets, start IP discovery");
         networks_iter (fun r -> CommonNetwork.network_reset r); (* stop_all_bt *)
         CommonServer.disconnect_all_servers ();
         CommonClient.disconnect_all_clients ();
         Unix32.close_all (); (* close all files *)
         discover_ip false;
         ));

  if not Autoconf.windows then
    MlUnix.set_signal  Sys.sigpipe
      (Sys.Signal_handle (fun _ -> if !verbose then lprintf_nl (_b "Received SIGPIPE, doing nothing")));

  MlUnix.set_signal  Sys.sigint
    (Sys.Signal_handle (fun _ -> lprintf_nl (_b "Received SIGINT, stopping MLDonkey...");
        CommonInteractive.clean_exit 0));

  MlUnix.set_signal  Sys.sigterm
    (Sys.Signal_handle (fun _ -> lprintf_nl (_b "Received SIGTERM, stopping MLDonkey...");
        CommonInteractive.clean_exit 0));

  if not Autoconf.windows then
  MlUnix.set_signal  Sys.sigusr1
    (Sys.Signal_handle (fun _ -> lprintf_nl (_b "Received SIGUSR1, saving options...");
        DriverInteractive.save_config ()));

  if not Autoconf.windows then
  MlUnix.set_signal  Sys.sigusr2
    (Sys.Signal_handle (fun _ -> lprintf_n (_b "Received SIGUSR2, starting garbage collection...");
        Gc.compact ();
        lprintf " finished";
        lprint_newline ()));

  if !verbose then lprintf_nl (_b "Activated system signal handling")

let _ =
  let security_space_oc = ref None in
  begin
(* Create a 'config_files_security_space' megabytes file to protect some space
for config files at the end. *)
    try
      let oc = Unix.openfile security_space_filename [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
        let len = 32768 in
        let s = Bytes.make len ' ' in
        let pos = ref zero in
        for i = 1 to !!config_files_security_space do
          for j = 1 to 32 do (* 32 = 1 MB / 32kB *)
            ignore(Unix2.c_seek64 oc !pos Unix.SEEK_SET);
            Unix2.really_write oc s 0 len;
            pos := !pos ++ (Int64.of_int len)
          done
        done;
        ignore(Unix2.c_seek64 oc zero Unix.SEEK_SET);
        (try Unix.lockf oc Unix.F_LOCK (!!config_files_security_space * 1024 * 1024) with _ -> ());
        security_space_oc := Some oc
    with e ->
        lprintf_nl (_b "Cannot create Security space file: %s") (Printexc2.to_string e);
        lprintf_nl (_b " not enough space on device or bad permissions");
        lprintf_nl (_b "Exiting...");
        exit 73;
  end;
  Unix32.external_start ();

  (
    let pid_file, s =
        Filename.concat !pid pid_filename,
        Printf.sprintf "%s\n" (string_of_int(Unix.getpid()))
    in
    Unix2.tryopen_write pid_file (fun oc -> output_string oc s);
    CommonGlobals.do_at_exit (fun _ -> try Sys.remove pid_file with _ -> ());
    if !verbose then
      lprintf_nl (_b "Starting with pid %s") (string_of_int(Unix.getpid ()))
  );

(* When a core is spawned from a gui, the only way to know the startup has
   succeeded is the string token "Core started". *)
  if not (keep_console_output ()) then
    begin
      try
        Printf.eprintf "%s[dMain] Core started\n%!" (log_time ());
      with _ -> ()
    end;

  lprintf_nl (_b "Core started");
  core_included := true;

  CommonGlobals.do_at_exit (fun _ ->
      (* If we have an error with too many file-descriptors,
         just close all of them *)
      (try
         BasicSocket.close_all ();
       with e ->
           lprintf_nl "Exception %s in do_at_exit while closing sockets."
             (Printexc2.to_string e);
      );
      DriverGraphics.G.remove_files ();
      (* In case we have no more space on filesystem for
         config files, remove the security space file *)
      (match !security_space_oc with
        None -> ()
      | Some oc -> Unix.close oc);
      (try Sys.remove security_space_filename with _ -> ());
      CommonComplexOptions.allow_saving_ini_files := true;
      DriverInteractive.save_config ();
      CommonComplexOptions.save_sources ();
      CommonComplexOptions.backup_options ();
      Geoip.close ();
      Unix32.external_exit ();
      lprintf_nl (_b "Core stopped")
    );

  if not (keep_console_output ()) then
    if !!log_file = "" then 
      begin
        lprintf_nl (_b "Option log_file is empty, disable logging completely...");
        lprintf_nl (_b "Disabling output to console, to enable: stdout true");
        log_to_file stdout;
        close_log ()
      end
