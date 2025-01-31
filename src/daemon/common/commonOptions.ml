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

open Gettext
open Printf2
open Options
open CommonTypes

let log_prefix = "[cO]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let _s x = _s "CommonOptions" x
let _b x = _b "CommonOptions" x

let startup_message = ref ""

let bin_dir = Filename.dirname Sys.argv.(0)

let hidden_dir_prefix =
  if Autoconf.windows then "" else "."

let config_dir_basename = hidden_dir_prefix ^ "mldonkey"

let pid_filename = Printf.sprintf "%s.pid" (Filename.basename Sys.argv.(0))

let security_space_filename = "config_files_space.tmp"

let home_dir =
  match Autoconf.system with
  | "cygwin"
  | "mingw" -> "."
  | _ -> Filename.concat (try Sys.getenv "HOME" with _ -> ".") config_dir_basename

let installer_ini = create_options_file (Filename.concat home_dir
      "installer.ini")

let installer_section = file_section installer_ini [] ""

let created_new_base_directory = ref None

let mldonkey_directory = define_option installer_section ["mldonkey_directory"]
  "The directory where mldonkey's option files are" 
    string_option home_dir

let _ =
  (try Options.load installer_ini with _ -> ())

let file_basedir_pre =
  try
    if (String2.starts_with
          (Filename.basename Sys.argv.(0))  "mlgui")
    then raise Exit;
    let chroot_dir = Sys.getenv "MLDONKEY_CHROOT" in
    try
      Unix.chdir chroot_dir;
      let new_passwd = Filename.concat chroot_dir "/etc/passwd" in
      if not (Sys.file_exists new_passwd) then
        lprintf_nl "No /etc/passwd in your chroot directory\n create one if you want to use 'run_as_user' option";
      MlUnix.chroot chroot_dir;
      lprintf_nl "mldonkey is now running in %s"  chroot_dir;
        "."

    with e ->
        lprintf_nl "Exception %s trying to chroot %s"
          (Printexc2.to_string e) chroot_dir;
        exit 2
  with _ ->
        try
          let s = Sys.getenv "MLDONKEY_DIR" in
          if s = "" then home_dir else Filename2.normalize s
        with _ ->
            home_dir

let file_basedir =
(* Creating dirs does work differently on Windows than Unix.
   Dirs like c:\b are split down by unix2.safe_mkdir to "c".
   This function splits the directory name into the drive name
   and chdir to it before creating directories.
   Non-absolute paths in $MLDONKEY_DIR do not work as well *)
  if Sys.file_exists (Filename.concat (Sys.getcwd ()) "downloads.ini") then
    "."
  else
    if Autoconf.windows && file_basedir_pre <> home_dir then
      match String2.split file_basedir_pre ':' with
      | drive :: directory :: _ ->
          Unix.chdir (drive ^ ":\\");
          directory
      | _ -> lprintf "Please provide an absolute path in MLDONKEY_DIR like d:\\mldonkey, exiting...\n"; exit 2
    else file_basedir_pre

let exit_message file = Printf.sprintf
"\nThis means another MLDonkey process could still be working
in this directory. Please shut it down before starting
a new instance here. If you are sure no other process uses
this directory delete %s and restart the core.\n" file

let exit_message_dev file exit = Printf.sprintf
"\n/dev/%s does not exist, please create it%s
If you are using a chroot environment, create it inside the chroot.\n"
  file (if exit then ", exiting..." else "")

let windows_sleep seconds =
  lprintf_nl "waiting %d seconds to exit..." seconds;
  Unix.sleep seconds

let min_reserved_fds = 50
let min_connections = 50

let () =
  lprintf_nl "Starting MLDonkey %s ... " Autoconf.current_version;

  Curl.global_init Curl.CURLINIT_GLOBALALL;
  lprintf_nl "Curl initialized. Version: %s" (Curl.version ());

  let ulof_old = Unix2.c_getdtablesize () in
  lprintf_nl "Language %s, locale %s, ulimit for open files %d"
    Charset.Locale.default_language Charset.Locale.locale_string ulof_old;

  let nofile = Unix2.ml_getrlimit Unix2.RLIMIT_NOFILE in
    if nofile.Unix2.rlim_max > 0 && nofile.Unix2.rlim_max > nofile.Unix2.rlim_cur then
      Unix2.ml_setrlimit Unix2.RLIMIT_NOFILE nofile.Unix2.rlim_max;
  let ulof = Unix2.c_getdtablesize () in
  if ulof_old <> ulof then
    lprintf_nl "raised ulimit for open files from %d to %d" ulof_old ulof;
  let absolute_min = Unix32.max_cache_size_default +
    min_reserved_fds + min_connections in
  if ulof < absolute_min then begin
    lprintf_nl "ulimit for open files is set to %d, at least %d is required, exiting..." ulof absolute_min;
    exit 2
  end;

  lprintf_nl "MLDonkey is working in %s" file_basedir;
  if not (Sys.file_exists file_basedir) ||
         not (Sys.file_exists (Filename.concat file_basedir "downloads.ini")) then begin
    lprint_newline ();
    lprintf_nl "creating new MLDonkey base directory in %s\n" file_basedir;
    created_new_base_directory := Some file_basedir
  end;
  (try
     Unix2.safe_mkdir file_basedir
   with e ->
     lprintf_nl "Exception (%s) trying to create dir %s"
       (Printexc2.to_string e) file_basedir;
     exit 2);
  Unix2.can_write_to_directory file_basedir;
  Unix.chdir file_basedir;

  let filename =
    try
      Sys.getenv "MLDONKEY_STRINGS"
    with _ ->
        "mlnet_strings"
  in
  set_strings_file filename;
  lprintf_nl (_b "loaded language resource file");

  let uname = Unix32.uname () in
  if uname = "" then
    begin
      lprintf_nl "Unknown operating system, please report to the MLDonkey development team";
      lprintf_nl "at https://github.com/ygrek/mldonkey/issues"
    end
  else
    if not (Unix32.os_supported ()) then begin
      lprintf_nl "WARNING: MLDonkey is not supported on %s" uname;
      if Autoconf.windows then
        lprintf_nl "WARNING: MLDonkey is only supported on Windows NT/2000/XP/Server 2003."
    end;

  if (String2.starts_with (Filename.basename Sys.argv.(0)) "mlnet")
    && not Autoconf.windows && not (Autoconf.system = "morphos")
    && Autoconf.donkey_sui = "yes" && not (Sys.file_exists "/dev/urandom") then
    begin
      Autoconf.donkey_sui_urandom := false;
      lprintf "%s" (exit_message_dev "urandom" false);
      if Autoconf.system = "hpux" then
        lprintf_nl "For HP-UX get urandom support from http://www.josvisser.nl/hpux11-random";
    end
  else
    Autoconf.donkey_sui_urandom := true;

  if (String2.starts_with (Filename.basename Sys.argv.(0)) "mlnet")
    && not Autoconf.windows && not (Sys.file_exists "/dev/null") then begin
      lprintf "%s" (exit_message_dev "null" true);
      exit 2
    end;  

  (* Charset conversion self-test *)
  let filename = "abcdefghijklmnopqrstuvwxyz" in
  let conv_filename = Charset.Locale.to_locale filename in
  if filename <> conv_filename then Charset.Locale.conversion_enabled := false;

  (try
     ignore (Sys.getenv "MLDONKEY_TEMP")
   with Not_found ->
     Unix.putenv "MLDONKEY_TEMP" ((Filename.basename Sys.argv.(0)) ^ "_tmp")
  );

  Unix2.can_write_to_directory (Filename2.temp_dir_name ());

  if (String2.starts_with (Filename.basename Sys.argv.(0)) "mlnet") then begin
    if Sys.file_exists pid_filename then begin
      lprintf_nl "PID file %s exists." (Filename.concat file_basedir pid_filename);
      let pid =
        try
          Unix2.tryopen_read pid_filename (fun pid_ci ->
          int_of_string (input_line pid_ci))
        with _ ->
          lprintf_nl "But it couldn't be read to check if the process still exists.";
          lprintf_nl "To avoid doing any harm, MLDonkey will now stop.";
          if Autoconf.windows then windows_sleep 10;
          exit 2
      in
      try
        lprintf_nl "Checking whether PID %d is still used..." pid;
        Unix.kill pid 0;
        lprintf "%s" (exit_message pid_filename);
        exit 2
      with (* stalled pid file, disregard it *)
      | Unix.Unix_error (Unix.ESRCH, _, _) ->
          lprintf_nl "Removing stalled file %s..." pid_filename;
          (try Sys.remove pid_filename with _ -> ())
      | e -> 
          lprintf "%s" (exit_message pid_filename);
          if Autoconf.system = "mingw" then lprintf_nl
            "can not check for stalled pid file because Unix.kill is not implemented on MinGW";
          lprintf_nl "Exception %s, exiting..." (Printexc2.to_string e);
          if Autoconf.system = "mingw" then windows_sleep 10;
          exit 2
    end;
    if Sys.file_exists security_space_filename then begin
      try
        let security_space_oc =
          Unix.openfile security_space_filename [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
        Unix.lockf security_space_oc Unix.F_TLOCK 0;
        Unix.close security_space_oc;
        lprintf_nl "Removing stalled file %s..."
          (Filename.concat file_basedir security_space_filename);
        begin
          try
            (try Unix.close security_space_oc with _ -> ());
            Sys.remove security_space_filename
          with e ->
            lprintf_nl "can not remove %s: %s"
              (Filename.concat file_basedir security_space_filename)
              (Printexc2.to_string e);
            if Autoconf.windows then windows_sleep 10;
            exit 2
        end
      with
        Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
          lprintf_nl "%s exists and is locked by another process."
            (Filename.concat file_basedir security_space_filename);
          lprintf "%s" (exit_message security_space_filename);
          if Autoconf.windows then windows_sleep 10;
          exit 2
      | e ->
          lprintf_nl "error while checking file %s: %s"
            (Filename.concat file_basedir security_space_filename)
            (Printexc2.to_string e);
          lprintf "%s" (exit_message security_space_filename);
          if Autoconf.windows then windows_sleep 10;
          exit 2
    end
  end

let define_option a b ?desc ?restart ?public ?internal c d e =
  match desc with
    None -> define_option a b (_s c) d e ?restart ?public ?internal
  | Some desc -> define_option a b ~desc: (_s desc) (_s c) d e ?restart ?public ?internal

let define_expert_option a b ?desc ?restart ?public ?internal c d e =
  match desc with
    None -> define_expert_option a b (_s c) d e ?restart ?public ?internal
  | Some desc -> define_expert_option a b ~desc: (_s desc) (_s c) d e ?restart ?public ?internal

let html_themes_dir = "html_themes"
let downloads_ini = create_options_file "downloads.ini"
let servers_ini = create_options_file "servers.ini"
let searches_ini = create_options_file "searches.ini"
let results_ini = create_options_file "results.ini"
let files_ini = create_options_file "files.ini"
let friends_ini = create_options_file "friends.ini"

let messages_log = "messages.log"

let servers_section = file_section servers_ini [] ""

let ip_list_option = list_option Ip.option

let ip_range_list_option = list_option Ip.range_option

let int_list_option = list_option int_option

let string_list_option = list_option string_option

let allow_browse_share_option = define_option_class "Integer"
    (fun v ->
      match v with
        StringValue "true" -> 2
      | StringValue "false" -> 0
      | _ -> value_to_int v)
  int_to_value

let addr_option  =  define_option_class "Addr"
    (fun value ->
      let s = value_to_string value in
      let addr, port = String2.cut_at s ':' in
      addr, int_of_string port)
      (fun (addr, port) -> string_to_value (Printf.sprintf "%s:%d" addr port))

let _ =
  Options.set_string_wrappers ip_list_option
    (fun list ->
      List.fold_left (fun s ip ->
          Printf.sprintf "%s %s" (Ip.to_string ip) s
      ) "" list
  )
  (fun s ->
      let list = String2.tokens s in
      List.map (fun ip -> Ip.of_string ip) list
  );

  Options.set_string_wrappers ip_range_list_option
    (fun list ->
      String.concat " " (List.map Ip.string_of_range (List.rev list))
  )
  (fun s ->
      let list = String2.tokens s in
      List.map (fun ip -> Ip.range_of_string ip) list
  );

  Options.set_string_wrappers int_list_option
    (fun list ->
      List.fold_left (fun s i ->
          Printf.sprintf "%s %s" (string_of_int i) s
      ) "" (List.rev list)
  )
  (fun s ->
      let list = String2.tokens s in
      List.map (fun i -> int_of_string i) list
  );
  Options.set_string_wrappers string_list_option
  (String.concat " ")
  String2.tokens

let is_not_spam = ref (fun _ -> true)
let is_not_comment_spam = ref (fun _ -> true)




(*************************************************************************)
(*                                                                       *)
(*                         BASIC OPTIONS                                 *)
(*                                                                       *)
(*************************************************************************)

let _ = Random.self_init ()

let random_letter () =
  char_of_int (97 + Random.int 26)

let new_name () =
  (Printf.sprintf "%c%c%c%c%c%c"
    (random_letter ()) (random_letter ()) (random_letter ())
    (random_letter ()) (random_letter ()) (random_letter ()))


let main_section = file_section downloads_ini ["Main"]
  "Main options"
let interfaces_section = file_section downloads_ini ["Interfaces"]
  "Options to control ports used by mldonkey interfaces"
let bandwidth_section = file_section downloads_ini ["Bandwidth"]
  "Bandwidth options"
let networks_section = file_section downloads_ini ["Networks"]
  "Networks options"
let network_section = file_section downloads_ini ["Network Config"]
  "Network config options"
let html_section = file_section downloads_ini ["HTML mods"]
  "Options to configure HTML mode"
let debug_section = file_section downloads_ini ["Debug"]
  "Debug options"
let download_section = file_section downloads_ini ["Download"]
  "Download options"
let startup_section = file_section downloads_ini ["Startup"]
  "Startup options"
let mail_section = file_section downloads_ini ["Mail"]
  "Mail options"
let path_section = file_section downloads_ini ["Paths"]
  "Paths options"
let security_section = file_section downloads_ini ["Security"]
  "Security options"
let other_section = file_section downloads_ini ["Other"]
  "Other options"




(*************************************************************************)
(*                                                                       *)
(*                         Main section                                  *)
(*                                                                       *)
(*************************************************************************)

let current_section = main_section

let global_login = define_option current_section ["client_name"]
  "small name of client"
    string_option (new_name ())




(*************************************************************************)
(*                                                                       *)
(*                         Interfaces section                            *)
(*                                                                       *)
(*************************************************************************)

let current_section = interfaces_section

let allowed_ips = define_option current_section ["allowed_ips"]
  ~desc: "Allowed IPs"
  "list of IP address allowed to connect to the core via telnet/GUI/WEB
  for internal command set: list separated by spaces
  example for internal command: set allowed_ips \"127.0.0.0/8 192.168.1.2\"
  or for editing the ini-file: list separated by semi-colon
  example for ini-file: allowed_ips = [ \"127.0.0.0/8\"; \"192.168.1.2\";]
  CIDR and range notations are supported: ie use 192.168.0.0/24
  or 192.168.0.0-192.168.0.255 for 192.168.0.*"
    ip_range_list_option [ Ip.RangeSingleIp Ip.localhost ]

let allowed_ips_set = ref Ip_set.bl_empty

let _ =
  option_hook allowed_ips (fun _ ->
    let new_list = ref [] in
    List.iter (fun i ->
        let new_range =
          match i with
          | Ip.RangeSingleIp ip ->
            (let a, b, c, d = Ip.to_ints ip in
              match a = 255, b = 255, c = 255, d = 255 with
              |  true,  true,  true,  true -> Ip.RangeCIDR (Ip.null, 0)
              | false,  true,  true,  true -> Ip.RangeCIDR ((Ip.of_string (Printf.sprintf "%d.0.0.0" a)), 8)
              | false, false,  true,  true -> Ip.RangeCIDR ((Ip.of_string (Printf.sprintf "%d.%d.0.0" a b)), 16)
              | false, false, false,  true -> Ip.RangeCIDR ((Ip.of_string (Printf.sprintf "%d.%d.%d.0" a b c)), 24)
              | false, false, false, false -> i
              | _ -> i)
          | Ip.RangeRange (ip1, ip2) -> i
          | Ip.RangeCIDR (ip, shift) -> i
        in
        if i <> new_range then
          lprintf_nl "allowed_ips: converted %s to %s" (Ip.string_of_range i) (Ip.string_of_range new_range);
        new_list := new_range :: !new_list
    ) !!allowed_ips;
    new_list := if !new_list = [] then [ Ip.localhost_range ] else List.rev !new_list;
    if !new_list <> !!allowed_ips then allowed_ips =:= !new_list;
    allowed_ips_set := (Ip_set.of_list !!allowed_ips))


let gui_port = define_option current_section ["gui_port"]
  ~desc: "The port to connect the GUI"
  ~restart: true
  "port for Graphical Interfaces, 0 to deactivate GUI interface"
    port_option 4001

let gift_port = define_option current_section ["gift_port"]
  ~desc: "The port to connect for GiFT GUIs."
  ~restart: true
  "port for GiFT Graphical Interfaces interaction. It was 1213, but the default is
  now 0 for disabled, because it does not check for a password." 
    port_option 0

let http_port = define_option current_section ["http_port"]
  ~desc: "The port to connect via HTTP"
  ~public: true
  ~restart: true
  "The port used to connect to your client with a web browser, 0 to deactivate web interface"
    port_option 4080

let telnet_port = define_option current_section ["telnet_port"]
  ~desc: "The port to connect via telnet"
  ~restart: true
  "port for user interaction, 0 to deactivate telnet interface"
    port_option 4000

let http_root_url = define_expert_option current_section ["http_root_url"]
  "Root url for the http interface (makes http proxy setup in front of mldonkey easier)"
  string_option "/"

let http_bind_addr = define_expert_option current_section ["http_bind_addr"]
  ~restart: true
  "The IP address used to bind the http server"
    Ip.option (Ip.any)

let gui_bind_addr = define_expert_option current_section ["gui_bind_addr"]
  ~restart: true
  "The IP address used to bind the gui server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let telnet_bind_addr = define_expert_option current_section ["telnet_bind_addr"]
  ~restart: true
  "The IP address used to bind the telnet server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let print_all_sources = define_expert_option current_section ["print_all_sources"] 
  "Should *all* sources for a file be shown on HTML/telnet vd <num>"
    bool_option true

let improved_telnet = define_expert_option current_section ["improved_telnet"]
  "Improved telnet interface"
    bool_option true

let alias_commands = define_option current_section ["alias_commands"]
  "Aliases to commands. The alias (fist string) has to be
  whitespaceless, the outcome of the alias (second string)
  may have spaces (put it in quotation then)."
    (list_option (tuple2_option (string_option, string_option)))
    [ "quit", "q";
      "exit", "q";
    ]

let upnp_port_forwarding = define_option current_section ["upnp_port_forwarding"]
  ~restart: true
  "upnp port forwarding"
    bool_option false

let clear_upnp_port_at_exit = define_option current_section ["clear_upnp_port_at_exit"]
  "clear all upnp port forwarding before mldonkey exit"
    bool_option true

let verbosity = define_expert_option current_section ["verbosity"]
  "A space-separated list of keywords. Each keyword triggers
  printing information on the corresponding messages:
  verb : verbose mode (interesting not only for coders)
  mc : debug client messages
  mr|raw : debug raw messages
  mct : debug emule clients tags
  ms : debug server messages
  sm : debug source management
  net : debug net
  gui : debug gui
  no-login : disable login messages
  file : debug file handling
  do : some download warnings
  up : some upload warnings
  unk : unknown messages
  ov : overnet
  loc : debug source research/master servers
  share: debug sharing
  md4 : md4 computation
  connect : debug connections
  udp : udp messages
  ultra|super : debug supernode
  swarming : debug swarming
  hc : http_client messages
  hs : http_server messages
  com : commands by non-admin users
  act : debug activity
  bw : debug bandwidth
  geo : debug GeoIP
  unexp : debug unexpected messages
  dht : debug DHT"
    string_option ""




(*************************************************************************)
(*                                                                       *)
(*                         Bandwidth section                             *)
(*                                                                       *)
(*************************************************************************)

let current_section = bandwidth_section

let max_hard_upload_rate = define_option current_section ["max_hard_upload_rate"]
  "The maximal upload rate you can tolerate on your link in kBytes/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
  control and data messages."
    int_option 10

let max_hard_download_rate = define_option current_section ["max_hard_download_rate"]
  "The maximal download rate you can tolerate on your link in kBytes/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
  control and data messages. Maximum value depends on max_hard_upload_rate:
  >= 10    -> unlimited download
  < 10 > 3 -> download limited to upload * 4
  < 4      -> download limited to upload * 3"
    int_option 50

let max_hard_upload_rate_2 = define_option current_section ["max_hard_upload_rate_2"]
    "Second maximal upload rate for easy toggling (use bw_toggle)"
    int_option 5

let max_hard_download_rate_2 = define_option current_section ["max_hard_download_rate_2"]
    "Second maximal download rate for easy toggling (use bw_toggle)"
    int_option 20

let max_opened_connections = define_option current_section ["max_opened_connections"]
  "Maximal number of opened connections"
    int_option 200
    
let max_opened_connections_2 = define_option current_section ["max_opened_connections_2"]
  "Second maximal number of opened connections for easy toggling (use bw_toggle)"
    int_option 100

let max_indirect_connections = define_option current_section ["max_indirect_connections"]
  "Amount of indirect connections in percent (min 30, max 70) of max_opened_connections"
    int_option 30

let max_upload_slots = define_option current_section ["max_upload_slots"]
  "How many slots can be used for upload, minimum 3"
    int_option 5

let max_release_slots = define_option current_section ["max_release_slots"]
  "How many percent of upload slots can be used for downloading files
  tagged as release, maximum 75%"
    percent_option 20

let friends_upload_slot = define_option current_section ["friends_upload_slot"]
  "Set aside a single reserved slot to upload to friends"
    bool_option true

let small_files_slot_limit = define_option current_section ["small_files_slot_limit"]
  "Maximum file size to benefit from the reserved slot for small files (0 to disable)"
    int64_option 10240L

let dynamic_slots = define_option current_section ["dynamic_slots"]
  "Set this to true if you want to have dynamic upload slot allocation (experimental)"
    bool_option false

let max_connections_per_second = define_option current_section ["max_connections_per_second"]
  "Maximal number of connections that can be opened per second"
    int_option 5

let loop_delay = define_expert_option current_section ["loop_delay"]
  "The delay in milliseconds to wait in the event loop. Can be decreased to
  increase the bandwidth usage, or increased to lower the CPU usage."
    int_option 5

let nolimit_ips = define_option current_section ["nolimit_ips"]
  ~desc: "No-limit IPs"
  "list of IP addresses allowed to connect to the core with no limit on
  upload/download and upload slots.  List separated by spaces, wildcard=255
  ie: use 192.168.0.255 for 192.168.0.* "
    ip_list_option [Ip.localhost]

let copy_read_buffer = define_option current_section ["copy_read_buffer"]
  "This option enables MLdonkey to always read as much data as possible
  from a channel, but use more CPU as it must then copy the data in the
  channel buffer."
    bool_option true




(*************************************************************************)
(*                                                                       *)
(*                         Networks section                              *)
(*                                                                       *)
(*************************************************************************)

let current_section = networks_section

let enable_overnet = define_option current_section ["enable_overnet"]
  "Set to true if you also want mldonkey to run as an overnet client
  (enable_donkey must be true)"
    bool_option false

let enable_kademlia = define_option current_section ["enable_kademlia"]
  "Set to true if you also want mldonkey to run as an kademlia client
  (enable_donkey must be true, and only experimental)"
    bool_option false

let enable_servers = define_option current_section ["enable_servers"]
  "Set to true if you want mldonkey to connect to edonkey servers
  (enable_donkey must be true, and only experimental)"
    bool_option true

let enable_bittorrent = define_option current_section ["enable_bittorrent"]
  "Set to true if you also want mldonkey to run as an Bittorrent client"
    bool_option false

let enable_donkey = define_option current_section ["enable_donkey"]
  "Set to true if you also want mldonkey to run as a donkey client"
    bool_option false

let enable_opennap = define_option current_section ["enable_opennap"]
  "Set to true if you also want mldonkey to run as a napster client (experimental)"
    bool_option false

let enable_soulseek = define_option current_section ["enable_soulseek"]
  "Set to true if you also want mldonkey to run as a soulseek client (experimental)"
    bool_option false

let enable_gnutella = define_option current_section ["enable_gnutella"]
  "Set to true if you also want mldonkey to run as a gnutella1 sub node (experimental)"
    bool_option false

let enable_gnutella2 = define_option current_section ["enable_gnutella2"]
  "Set to true if you also want mldonkey to run as a gnutella2 sub node (experimental)"
    bool_option false

let enable_fasttrack = define_option current_section ["enable_fasttrack"]
  "Set to true if you also want mldonkey to run as a Fasttrack sub node (experimental)"
    bool_option false

let enable_directconnect = define_option current_section ["enable_directconnect"]
  "Set to true if you also want mldonkey to run as a direct-connect node (experimental)"
    bool_option false

let enable_openft = define_expert_option current_section ["enable_openft"]
  "Set to true if you also want mldonkey to run as a OpenFT sub node (experimental)"
    bool_option false

let enable_fileTP = define_option current_section ["enable_fileTP"]
  "Set to true if you also want mldonkey to download HTTP files (experimental)"
    bool_option true




(*************************************************************************)
(*                                                                       *)
(*                         HTML section                                  *)
(*                                                                       *)
(*************************************************************************)

let current_section = html_section

let html_mods = define_expert_option current_section ["html_mods"]
  "Whether to use the modified WEB interface"
    bool_option true

let html_mods_style = define_expert_option current_section ["html_mods_style"]
  "Which html_mods style to use (set with html_mods_style command)"
    int_option 0

let html_mods_human_readable = define_expert_option current_section ["html_mods_human_readable"]
  "Whether to use human readable GMk number format"
    bool_option true

let html_mods_use_relative_availability = define_expert_option current_section ["html_mods_use_relative_availability"]
  "Whether to use relative availability in the WEB interface"
    bool_option true

let html_mods_vd_network = define_expert_option current_section ["html_mods_vd_network"]
  "Whether to display the Net column in vd output"
    bool_option true

let html_mods_vd_comments = define_expert_option current_section ["html_mods_vd_comments"]
  "Whether to display the Comments column in vd output"
    bool_option true

let html_mods_vd_user = define_expert_option current_section ["html_mods_vd_user"]
  "Whether to display the User column in vd output"
    bool_option false

let html_mods_vd_group = define_expert_option current_section ["html_mods_vd_group"]
  "Whether to display the Group column in vd output"
    bool_option false

let html_mods_vd_active_sources = define_expert_option current_section ["html_mods_vd_active_sources"]
  "Whether to display the Active Sources column in vd output"
    bool_option true

let html_mods_vd_age = define_expert_option current_section ["html_mods_vd_age"]
  "Whether to display the Age column in vd output"
    bool_option true

let html_flags = define_expert_option current_section ["html_flags"]
  "Whether to display flags instead of country codes"
    bool_option true

let html_mods_vd_gfx = define_expert_option current_section ["html_mods_vd_gfx"]
  "Show graph in vd output"
    bool_option true

let html_mods_vd_gfx_remove = define_expert_option current_section ["html_mods_vd_gfx_remove"]
  "Remove graph files on core shutdown"
    bool_option false

let html_mods_vd_gfx_fill = define_expert_option current_section ["html_mods_vd_gfx_fill"]
  "Fill graph in vd output"
    bool_option true

let html_mods_vd_gfx_split = define_expert_option current_section ["html_mods_vd_gfx_split"]
  "Split download and upload graph in vd output"
    bool_option false

let html_mods_vd_gfx_stack = define_expert_option current_section ["html_mods_vd_gfx_stack"]
  "Stacked download and upload graph"
    bool_option true

let html_mods_vd_gfx_flip = define_expert_option current_section ["html_mods_vd_gfx_flip"]
  "Flip up/side graph position in vd output"
    bool_option true

let html_mods_vd_gfx_mean = define_expert_option current_section ["html_mods_vd_gfx_mean"]
  "Show mean line on graph in vd output"
    bool_option true

let html_mods_vd_gfx_transparent = define_expert_option current_section ["html_mods_vd_gfx_transparent"]
  "Show transparent graph in vd output (only for png)"
    bool_option true

let html_mods_vd_gfx_png = define_expert_option current_section ["html_mods_vd_gfx_png"]
  "Draw graph as png if true, else draw as jpg in vd output"
    bool_option true

let html_mods_vd_gfx_h = define_expert_option current_section ["html_mods_vd_gfx_h"]
  "Show hourly graph in vd output"
    bool_option true

let html_mods_vd_gfx_x_size = define_expert_option current_section ["html_mods_vd_gfx_x_size"]
  "Graph x size in vd output ( 365 < x < 3665 )"
    int_option 795

let html_mods_vd_gfx_y_size = define_expert_option current_section ["html_mods_vd_gfx_y_size"]
  "Graph y size in vd output ( 200 < y < 1200 )"
    int_option 200

let html_mods_vd_gfx_h_intervall = define_expert_option current_section ["html_mods_vd_gfx_h_intervall"]
  ~restart: true
  "compute values for hourly graph every 1,2,3,4,5,10,15,20,30,60 min
        Changes to this option require a core restart."
     int_option 60

let html_mods_vd_gfx_h_dynamic = define_expert_option current_section ["html_mods_vd_gfx_h_dymamic"]
  "Dynamic grid width, start with 1 h/grid, maximum html_mods_vd_gfx_h_grid_time h/grid"
    bool_option true
                
let html_mods_vd_gfx_h_grid_time = define_expert_option current_section ["html_mods_vd_gfx_h_grid_time"]
  "Max hours on time scale per grid (0 = no limit)"
    int_option 0

let html_mods_vd_gfx_subgrid = define_expert_option current_section ["html_mods_vd_gfx_subgrid"]
  "Number of shown subgrids on graph (0 = no subgrids)"
    int_option 0

let html_mods_vd_gfx_tag = define_expert_option current_section ["html_mods_vd_gfx_tag"]
  "Draw tag graph"
    bool_option false

let html_mods_vd_gfx_tag_use_source = define_expert_option current_section ["html_mods_vd_gfx_tag_use_source"]
  "Use tag source image "
    bool_option false

let html_mods_vd_gfx_tag_source = define_expert_option current_section ["html_mods_vd_gfx_tag_source"]
  "Tag source image name"
    string_option "image"

let html_mods_vd_gfx_tag_png = define_expert_option current_section ["html_mods_vd_gfx_tag_png"]
  "Draw tag as png if true, else draw as jpg in vd output"
    bool_option true

let html_mods_vd_gfx_tag_enable_title = define_expert_option current_section ["html_mods_vd_gfx_tag_enable_title"]
  "Enable tag graph title"
    bool_option true

let html_mods_vd_gfx_tag_title = define_expert_option current_section ["html_mods_vd_gfx_tag_title"]
  "Tag graph title"
    string_option "MLNet traffic"

let html_mods_vd_gfx_tag_title_x_pos = define_expert_option current_section ["html_mods_vd_gfx_tag_title_x_pos"]
  "Tag graph title x pos in vd output"
    int_option 4

let html_mods_vd_gfx_tag_title_y_pos = define_expert_option current_section ["html_mods_vd_gfx_tag_title_y_pos"]
  "Tag graph title y pos in vd output"
    int_option 1

let html_mods_vd_gfx_tag_dl_x_pos = define_expert_option current_section ["html_mods_vd_gfx_tag_dl_x_pos"]
  "Tag graph download x pos in vd output"
    int_option 4

let html_mods_vd_gfx_tag_dl_y_pos = define_expert_option current_section ["html_mods_vd_gfx_tag_dl_y_pos"]
  "Tag graph download y pos in vd output"
    int_option 17

let html_mods_vd_gfx_tag_ul_x_pos = define_expert_option current_section ["html_mods_vd_gfx_tag_ul_x_pos"]
  "Tag graph upload x pos in vd output"
    int_option 4

let html_mods_vd_gfx_tag_ul_y_pos = define_expert_option current_section ["html_mods_vd_gfx_tag_ul_y_pos"]
  "Tag graph upload y pos in vd output"
    int_option 33

let html_mods_vd_gfx_tag_x_size = define_expert_option current_section ["html_mods_vd_gfx_tag_x_size"]
  "Tag graph x size in vd output ( 130 < x < 3600 )"
    int_option 80

let html_mods_vd_gfx_tag_y_size = define_expert_option current_section ["html_mods_vd_gfx_tag_y_size"]
  "Tag graph y size in vd output ( 50 < x < 1200 )"
    int_option 50

let html_mods_vd_last = define_expert_option current_section ["html_mods_vd_last"]
  "Whether to display the Last column in vd output"
    bool_option true

let html_mods_vd_prio = define_expert_option current_section ["html_mods_vd_prio"]
  "Whether to display the Priority column in vd output"
    bool_option true

let html_vd_barheight = define_expert_option current_section ["html_vd_barheight"]
  "Change height of download indicator bar in vd output"
    int_option 2

let html_vd_chunk_graph = define_expert_option current_section ["html_vd_chunk_graph"]
  "Whether to display chunks list as graph or text in vd output"
    bool_option true

let html_vd_chunk_graph_style = define_expert_option current_section ["html_vd_chunk_graph_style"]
  "Change style of chunk graph"
    int_option 0

let html_vd_chunk_graph_max_width = define_expert_option current_section ["html_vd_chunk_graph_max_width"]
  "Change max width of chunk graph"
    int_option 200

let html_mods_show_pending = define_expert_option current_section ["html_mods_show_pending"]
  "Whether to display the pending slots in uploaders command"
    bool_option true

let html_mods_load_message_file = define_expert_option current_section ["html_mods_load_message_file"]
  "Whether to load the mldonkey_messages.ini file (false=use internal settings)"
    bool_option false

let html_mods_max_messages = define_expert_option current_section ["html_mods_max_messages"]
  "Maximum chat messages to log in memory"
    int_option 50

let html_mods_bw_refresh_delay = define_option current_section ["html_mods_bw_refresh_delay"]
  "bw_stats refresh delay (seconds)"
    int_option 11

let html_mods_theme = define_option current_section ["html_mods_theme"]
  "html_mods_theme to use (located in relative html_themes/<theme_name> directory
  leave blank to use internal theme"
    string_option ""

let use_html_mods o =
  o.conn_output = HTML && !!html_mods

let html_checkbox_vd_file_list = define_expert_option current_section ["html_checkbox_vd_file_list"]
  "Whether to use checkboxes in the WEB interface for download list"
    bool_option true

let html_checkbox_search_file_list = define_expert_option current_section ["html_checkbox_search_file_list"]
  "Whether to use checkboxes in the WEB interface for search result list"
    bool_option false

let html_use_gzip = define_expert_option current_section ["html_use_gzip"]
  "Use gzip compression on web pages"
    bool_option false

let html_mods_use_js_tooltips = define_expert_option current_section ["html_mods_use_js_tooltips"]
  "Whether to use the fancy javascript tooltips or plain html-title"
    bool_option true

let html_mods_js_tooltips_wait = define_expert_option current_section ["html_mods_js_tooltips_wait"]
  "How long to wait before displaying the tooltips"
    int_option 0

let html_mods_js_tooltips_timeout = define_expert_option current_section ["html_mods_js_tooltips_timeout"]
  "How long to display the tooltips"
    int_option 100000
    
let html_mods_use_js_helptext = define_expert_option current_section ["html_mods_use_js_helptext"]
  "Use javascript to display option help text as js popup (true=use js, false=use html tables)"
    bool_option true




(*************************************************************************)
(*                                                                       *)
(*                         Network section                               *)
(*                                                                       *)
(*************************************************************************)

let current_section = network_section

let set_client_ip = define_option current_section ["client_ip"]
  "The last IP address used for this client" Ip.option
    (Ip.my ())

let force_client_ip = define_option current_section ["force_client_ip"]
  "Use the IP specified by 'client_ip' instead of trying to determine it
  ourself. Don't set this option to true if you have dynamic IP."
    bool_option false

let discover_ip = define_option current_section ["discover_ip"]
  "Use http://ip.discoveryvip.com/ip.asp to obtain WAN IP"
    bool_option true

let user_agent = define_option current_section ["user_agent"]
  "User agent string (default = \"default\")"
    string_option "default"

let get_user_agent () = 
  if !!user_agent = "default" then
     Printf.sprintf "MLDonkey/%s" Autoconf.current_version
  else !!user_agent

let web_infos = define_option current_section ["web_infos"]
  "A list of lines to download on the WEB: each line has
  the format: (kind, period, url), where kind is either
  'server.met' for a server.met file (also in gz/bz2/zip format)
               containing ed2k server, or
  'comments.met' for a file of comments, or
  'guarding.p2p' for a blocklist file (also in gz/bz2/zip format), or
  'ocl' for file in the ocl format containing overnet peers, or
  'contact.dat' for an contact.dat file containing overnet peers,
  'nodes.gzip' for a fasttrack nodes.gzip,
  'hublist' for DirectConnect hubs list,
  and period is the period between updates (in hours),
  a period of zero means the file is only loaded once on startup,
  and url is the url of the file to download.
  IMPORTANT: Put the URL and the kind between quotes.
  EXAMPLE:
    web_infos = [
    (\"server.met\", 0, \"http://www.gruk.org/server.met.gz\");
    (\"hublist\", 0, \"http://dchublist.com/hublist.xml.bz2\");
    (\"guarding.p2p\", 96, \"http://upd.emule-security.org/ipfilter.zip\");
    (\"ocl\", 24, \"http://members.lycos.co.uk/appbyhp2/FlockHelpApp/contact-files/contact.ocl\");
    (\"contact.dat\", 168, \"http://download.overnet.org/contact.dat\");
    (\"geoip.dat\", 168, \"http://www.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz\");
    ]
  "
    (list_option (tuple3_option (string_option, int_option, string_option)))
    [
      ("guarding.p2p", 96,
        "http://upd.emule-security.org/ipfilter.zip");
      ("server.met", 0,
        "http://www.gruk.org/server.met.gz");
      ("geoip.dat", 0,
        "http://www.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz");
      ("nodes.gzip", 0,
        "http://update.kceasy.com/update/fasttrack/nodes.gzip");
      ("hublist", 0,
        "http://dchublist.com/hublist.config.bz2");
(*
    ("slsk_boot", 0,
      "http://www.slsknet.org/slskinfo2");
*)
    ]

let rss_feeds = define_expert_option current_section ["rss_feeds"]
  "URLs of RSS feeds"
    (list_option Url.option) []

let rss_preprocessor = define_expert_option current_section ["rss_preprocessor"]
  "If MLDonkey can not read broken RSS feeds, use this program to preprocess them"
    string_option "xmllint"

let ip_blocking_descriptions = define_expert_option current_section ["ip_blocking_descriptions"]
  "Keep IP blocking ranges descriptions in memory"
    bool_option false

let ip_blocking = define_expert_option current_section ["ip_blocking"]
  "IP blocking list filename (peerguardian format), can also be in gz/bz2/zip format
  Zip files must contain either a file named guarding.p2p or guarding_full.p2p."
    string_option ""

let ip_blocking_countries = define_expert_option current_section ["ip_blocking_countries"]
  "List of countries to block connections from/to (requires Geoip).
  Names are in ISO 3166 format, see http://www.maxmind.com/app/iso3166
  You can also at your own risk use \"Unknown\" for IPs Geoip won't recognize."
    string_list_option []

let ip_blocking_countries_block = define_expert_option current_section ["ip_blocking_countries_block"]
  "false: use ip_blocking_countries as block list, all other countries are allowed
  true: use ip_blocking_countries as allow list, all other countries are blocked"
    bool_option false

let geoip_dat = define_expert_option current_section ["geoip_dat"]
  "Location of GeoIP.dat (Get one from http://www.maxmind.com/download/geoip/database/)"
    string_option ""

let _ =
  option_hook ip_blocking_descriptions (fun _ ->
    Ip_set.store_blocking_descriptions := !!ip_blocking_descriptions
  )

let tcpip_packet_size = define_expert_option current_section ["tcpip_packet_size"]
  "The size of the header of a TCP/IP packet on your connection (ppp adds
  14 bytes sometimes, so modify to take that into account)"
    int_option 40

let mtu_packet_size = define_expert_option current_section ["mtu_packet_size"]
  "The size of the MTU of a TCP/IP packet on your connection"
    int_option 1500

let minimal_packet_size = define_expert_option current_section ["minimal_packet_size"]
  "The size of the minimal packet you want mldonkey to send when data is
  available on the connection"
    int_option !TcpBufferedSocket.minimal_packet_size

let socket_keepalive = define_expert_option current_section ["socket_keepalive"]
  "Should a connection check if the peer we are connected to is still alive?
  This implies some bandwidth-cost (with 200 connections ~10-20%)"
    bool_option !BasicSocket.socket_keepalive

let referers = define_option current_section ["referers"]
  "Cookies send with a http request (used for .torrent files and web_infos)"
    (list_option (tuple2_option (string_option, string_option))) [(".*suprnova.*", "http://www.suprnova.org/")]

let cookies = define_option current_section ["cookies"]
  "Cookies send with a http request (used for .torrent files and web_infos)"
    (list_option (tuple2_option (string_option, list_option (tuple2_option (string_option, string_option))))) []

let http_proxy_server = define_option current_section ["http_proxy_server"]
  "Direct HTTP queries to HTTP proxy"
    string_option ""

let http_proxy_port = define_option current_section ["http_proxy_port"]
  "Port of HTTP proxy"
    port_option 8080

let http_proxy_login = define_option current_section ["http_proxy_login"]
  "HTTP proxy login (leave empty if proxy doesn't require authentication)"
    string_option ""

let http_proxy_password = define_option current_section ["http_proxy_password"]
  "HTTP proxy password"
    string_option ""

let http_proxy_tcp = define_option current_section ["http_proxy_tcp"]
  "Direct TCP connections to HTTP proxy (the proxy should support CONNECT)"
    bool_option false


(*************************************************************************)
(*                                                                       *)
(*                         Mail section                                  *)
(*                                                                       *)
(*************************************************************************)

let current_section = mail_section

let smtp_server = define_option current_section ["smtp_server"]
  (_s"The mail server you want to use (must be SMTP). Use hostname or IP address")
    string_option "127.0.0.1"

let smtp_port = define_option current_section ["smtp_port"]
  (_s"The port to use on the mail server (default 25)")
    port_option 25

let smtp_login = define_option current_section ["smtp_login"]
  (_s"Login to use for SMTP authentication (leave empty to disable). LOGIN, PLAIN and CRAM-MD5 methods are supported")
  string_option ""

let smtp_password = define_option current_section ["smtp_password"]
  (_s"Password to use for SMTP authentication")
  string_option ""

let mail = define_option current_section ["mail"]
  (_s"Email address to receive notifications when downloads are completed or disk is full (leave empty to disable, separate multiple addresses with space)")
    string_option ""

let add_mail_brackets = define_option current_section ["add_mail_brackets"]
  (_s"Set to false if your mail server cannot handle angle-brackets around addresses (RFC 5321)")
    bool_option true

let filename_in_subject = define_option current_section ["filename_in_subject"]
  (_s"Send filename in mail subject")
    bool_option true

let url_in_mail = define_option current_section ["url_in_mail"]
  (_s"Put a prefix for the filename here which shows up in the notification mail")
    string_option ""




(*************************************************************************)
(*                                                                       *)
(*                         Download section                              *)
(*                                                                       *)
(*************************************************************************)

let current_section = download_section

let auto_commit = define_option current_section ["auto_commit"]
  "Set to false if you don't want mldonkey to automatically put completed files
  in incoming directory"
    bool_option true

let pause_new_downloads = define_option current_section ["pause_new_downloads"]
  "Set to true if you want all new downloads be paused immediatly
  will be set to false on core start."
    bool_option false

let release_new_downloads = define_option current_section ["release_new_downloads"]
  "Set to true if you want to activate the release slot feature for all new downloads."
    bool_option false

(*  emulate_sparsefiles does not work, temporarily disabled
let emulate_sparsefiles = define_expert_option current_section ["emulate_sparsefiles"]
  "Set to true if you want MLdonkey to emulate sparse files on your disk.
  Files will use less space, but <preview> and <recover> won't work anymore.
  Works only on Edonkey plugin. EXPERIMENTAL."
    bool_option false
*)

let max_concurrent_downloads = define_option current_section ["max_concurrent_downloads"]
  "The maximal number of files in Downloading state (other ones are Queued)"
    int_option 50

let sources_per_chunk = define_expert_option current_section ["sources_per_chunk"]
  "How many sources to use to download each chunk"
    int_option 3

let max_recover_gap = define_option current_section ["max_recover_zeroes_gap"]
  "The maximal length of zero bytes between non-zero bytes in a file that
  should be interpreted as downloaded during a recovery"
    int64_option 16L

let file_completed_cmd = define_option current_section ["file_completed_cmd"]
  "A command that is called when a file is committed, does not work on MinGW.
  Arguments are (kept for compatibility):
    $1 - temp file name, without path
    $2 - file size
    $3 - filename of the committed file
  Also these environment variables can be used (preferred way):
    $TEMPNAME  - temp file name, including path
    $FILEID    - same as $1
    $FILESIZE  - same as $2
    $FILENAME  - same as $3
    $FILEHASH  - internal hash
    $DURATION  - download duration
    $INCOMING  - directory used for commit
    $NETWORK   - network used for downloading
    $ED2K_HASH - ed2k hash if MD4 is known
    $FILE_OWNER - user who started the download
    $FILE_GROUP - group the file belongs to
    $USER_MAIL - mail address of file_owner
  "
    string_option ""

let file_started_cmd = define_option current_section ["file_started_cmd"]
  "The command which is called when a download is started. Arguments
  are '-file <num>'
  Also these environment variables can be used (preferred way):
    $TEMPNAME  - temp file name, including path
    $FILEID    - same as $1
    $FILESIZE  - same as $2
    $FILENAME  - same as $3
    $FILEHASH  - internal hash
    $NETWORK   - network used for downloading
    $ED2K_HASH - ed2k hash if MD4 is known
    $FILE_OWNER - user who started the download
    $FILE_GROUP - group the file belongs to
    $USER_MAIL - mail address of file_owner
  "
    string_option ""



(*************************************************************************)
(*                                                                       *)
(*                         Startup section                               *)
(*                                                                       *)
(*************************************************************************)

let current_section = startup_section

let run_as_user = define_option current_section ["run_as_user"]
  ~restart: true
  "The login of the user you want mldonkey to run as, after the ports
  have been bound (can be used not to run with root privileges when
  a port < 1024 is needed)"
    string_option ""

let run_as_useruid = define_option current_section ["run_as_useruid"]
  ~restart: true
  "The UID of the user (0=disabled) you want mldonkey to run as, after the ports
  have been bound (can be used not to run with root privileges when
  a port < 1024 is needed)"
    int_option 0

let run_as_group = define_option current_section ["run_as_group"]
  ~restart: true
  "The group of run_as_user user to be used"
    string_option ""

let run_as_groupgid = define_option current_section ["run_as_groupgid"]
  ~restart: true
  "The group of run_as_user user to be used"
    int_option 0

let ask_for_gui = define_option current_section ["ask_for_gui"]
  "Ask for GUI start"
    bool_option false

let start_gui = define_option current_section ["start_gui"]
  "Automatically Start the GUI"
    bool_option false

let recover_temp_on_startup = define_option current_section ["recover_temp_on_startup"]
  "Should MLdonkey try to recover downloads of files in temp/ at startup"
    bool_option true

let config_files_security_space = define_expert_option current_section ["config_files_security_space"]
  ~restart: true
  "How many megabytes should MLdonkey keep for saving configuration files."
    int_option 10




(*************************************************************************)
(*                                                                       *)
(*                         Path section                                  *)
(*                                                                       *)
(*************************************************************************)

let current_section = path_section

let temp_directory = define_option current_section ["temp_directory"]
  "The directory where temporary files should be put"
    string_option "temp"

let share_scan_interval = define_option current_section ["share_scan_interval"]
  ~restart: true
  "How often (in minutes) should MLDonkey scan all shared directories for new/removed files.
  Minimum 5, 0 to disable. Use command reshare to manually scan shares.
  When core starts, shared directories are scanned once, independent of this option."
    int_option 30

let create_file_mode = define_option current_section ["create_file_mode"]
  "New download files are created with these rights (in octal)"
    string_option "664"

let create_dir_mode = define_option current_section ["create_dir_mode"]
  "New directories in incoming_directories are created with these rights (in octal)"
    string_option "755"

let create_file_sparse = define_option current_section ["create_file_sparse"]
  "Create new files as sparse (not supported on FAT volumes)"
    bool_option true

let hdd_temp_minfree = define_option current_section ["hdd_temp_minfree"]
  "Mininum free space in MB on temp_directory, minimum 50"
    int_option 50

let hdd_temp_stop_core = define_option current_section ["hdd_temp_stop_core"]
  "If true core shuts down when free space on temp dir is below hdd_temp_minfree,
  otherwise all downloads are paused and a warning email is sent."
    bool_option false

let hdd_coredir_minfree = define_option current_section ["hdd_coredir_minfree"]
  "Mininum free space in MB on core directory, minimum 20"
    int_option 50

let hdd_coredir_stop_core = define_option current_section ["hdd_coredir_stop_core"]
  "If true core shuts down when free space on core dir is below hdd_coredir_minfree,
  otherwise all downloads are paused and a warning email is sent."
    bool_option true

let hdd_send_warning_interval = define_option current_section ["hdd_send_warning_interval"]
  "Send a warning mail each <interval> hours for each directory, 0 to deactivate mail warnings."
    int_option 1

let previewer = define_expert_option current_section ["previewer"]
  "Name of program used for preview (first arg is local filename, second arg
  is name of file as searched on eDonkey"
    string_option "mldonkey_previewer"

let mldonkey_bin = define_expert_option current_section ["mldonkey_bin"]
  "Directory where mldonkey binaries are installed"
    string_option bin_dir

let mldonkey_gui = define_expert_option current_section ["mldonkey_gui"]
  "Name of GUI to start"
    string_option (Filename.concat bin_dir "mlgui")

let filenames_utf8 = define_option current_section ["filenames_utf8"]
  "Use UTF-8 for filenames (instead of automatic locale detection)."
    bool_option false

(*************************************************************************)
(*                                                                       *)
(*                         Security section                              *)
(*                                                                       *)
(*************************************************************************)

let current_section = security_section

let allowed_commands = define_option current_section ["allowed_commands"]
  "Commands that you are allowed to be call from the interface. These
  commands should short, so that the core is not blocked more than necessary."
    (list_option (tuple2_option (string_option, string_option)))
    [ "df", "df";
      "ls", "ls incoming";
    ]

let allow_any_command = define_option current_section ["allow_any_command"]
  "Allow you to use any command with ! in the interface instead of only the
  ones in allowed_commands"
    bool_option false

let allow_browse_share = define_option current_section ["allow_browse_share"]
  "Allow others to browse our share list (0: none, 1: friends only, 2: everyone"
    allow_browse_share_option 1

let messages_filter = define_option current_section ["messages_filter"]
  "Regexp of messages to filter out, example: string1|string2|string3"
    string_option "DI-Emule|ZamBoR|Ketamine|eMule FX|AUTOMATED MESSAGE|Hi Honey!|Do you live in my area|download HyperMule"

let comments_filter = define_option current_section ["comments_filter"]
  "Regexp of comments to filter out, example: string1|string2|string3"
    string_option "http://|https://|www\\."




(*************************************************************************)
(*                                                                       *)
(*                         Other section                                 *)
(*                                                                       *)
(*************************************************************************)

let current_section = other_section

let save_results = define_option current_section ["save_results"]
  "(experimental)" 
    int_option 0

let buffer_writes = define_option current_section ["buffer_writes"]
  "Buffer writes and flush after buffer_writes_delay seconds (experimental)"
    bool_option false

let buffer_writes_delay = define_expert_option current_section ["buffer_writes_delay"]
  ~restart: true
  "Buffer writes and flush after buffer_writes_delay seconds (experimental)"
    float_option 30.

let buffer_writes_threshold = define_expert_option current_section ["buffer_writes_threshold"]
  "Flush buffers if buffers exceed buffer_writes_threshold kB (experimental)"
    int_option 1024

let emule_mods_count = define_option current_section ["emule_mods_count"]
  "build statistics about eMule mods"
    bool_option false

let emule_mods_showall = define_option current_section ["emule_mods_showall"]
  "show all eMule mods in statistics"
    bool_option false

let backup_options_delay = define_option current_section ["backup_options_delay"]
  "How often (in hours) should a backup of the ini files be written into old_config.
  A value of zero means that a backup is written only when the core shuts down."
    int_option 0

let backup_options_generations = define_option current_section ["backup_options_generations"]
  "Define the total number of options archives in old_config."
    int_option 10

let backup_options_format = define_option current_section ["backup_options_format"]
  "Define the format of the archive, zip or tar.gz are valid."
    string_option "tar.gz"

let shutdown_timeout = define_option current_section ["shutdown_timeout"]
  "The maximum time in seconds to wait for networks to cleanly shutdown."
    int_option 3


(*************************************************************************)
(*                                                                       *)
(*                         EXPERT OPTIONS                                *)
(*                                                                       *)
(*************************************************************************)

let safe_utf8 s =
  if Charset.is_utf8 s
  then s
  else failwith (Printf.sprintf "%s is not an UTF-8 string.\n" s)

let value_to_utf8 v =
  let s = Options.value_to_string v in
  safe_utf8 s

let utf8_to_value s =
  let s = safe_utf8 s in
  Options.string_to_value s

let utf8_option =
  define_option_class "Utf8"
  value_to_utf8 utf8_to_value

let utf8_filename_conversions = define_expert_option current_section ["utf8_filename_conversions"]
  "The conversions to apply on Unicode characters"
    (list_option (tuple2_option (int_option, utf8_option))) []

let interface_buffer = define_expert_option current_section ["interface_buffer"]
  "The size of the buffer between the client and its GUI. Can be useful
  to increase when the connection between them has a small bandwith"
    int_option 1000000

let max_name_len = define_expert_option current_section ["max_name_len"]
  "The size long names will be shortened to in the interface"
    int_option 50

let max_result_name_len = define_expert_option current_section ["max_result_name_len"]
  "The size filenames will be shortened to in search results"
    int_option 50

let max_filenames = define_expert_option current_section ["max_filenames"]
  "The maximum number of different filenames used by MLDonkey"
    int_option 50

let max_client_name_len = define_expert_option current_section ["max_client_name_len"]
  "The size long client names will be shortened to in the interface"
    int_option 25

let term_ansi = define_expert_option current_section ["term_ansi"]
  "Is the default terminal an ANSI terminal (escape sequences can be used)"
    bool_option true

let update_gui_delay = define_expert_option current_section ["update_gui_delay"]
   "Delay between updates to the GUI"
     float_option 1.

let http_realm = define_expert_option current_section ["http_realm"]
  "The realm shown when connecting with a WEB browser"
    string_option "MLdonkey"

let html_frame_border = define_expert_option current_section ["html_frame_border"]
  "This option controls whether the WEB interface should show frame borders or not"
    bool_option true

let commands_frame_height = define_expert_option current_section ["commands_frame_height"]
  "The height of the command frame in pixel (depends on your screen and browser sizes)"
    int_option 46

let motd_html = define_expert_option current_section ["motd_html"]
  "Message printed at startup additional to welcome text"
    string_option ""

let compaction_delay = define_expert_option current_section ["compaction_delay"]
  "Force compaction every <n> hours (in [1..24])"
    int_option 2

let vd_reload_delay = define_expert_option current_section ["vd_reload_delay"]
  "The delay between reloads of the vd output in the WEB interface"
    int_option 120

let client_bind_addr = define_option current_section ["client_bind_addr"]
  ~restart: true
  "The IP address used to bind the p2p clients"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let _ =
  option_hook client_bind_addr (fun _ ->
      TcpBufferedSocket.bind_address := Ip.to_inet_addr !!client_bind_addr
  )

let _ =
  option_hook copy_read_buffer (fun _ ->
      TcpBufferedSocket.copy_read_buffer := !!copy_read_buffer
  )

let () =
  option_hook create_file_mode (fun _ ->
      Unix32.create_file_mode := Misc.int_of_octal_string !!create_file_mode
  );
  option_hook create_dir_mode (fun _ ->
      Unix32.create_dir_mode := Misc.int_of_octal_string !!create_dir_mode
  )

let create_mlsubmit = define_expert_option current_section ["create_mlsubmit"]
  "Should the MLSUBMIT.REG file be created"
    bool_option true

let minor_heap_size = define_expert_option current_section ["minor_heap_size"]
  "Size of the minor heap in kB"
    int_option 32

let relevant_queues = define_expert_option current_section ["relevant_queues"]
  "The source queues to display in source lists (see 'sources' command)"
    int_list_option [0;1;2;3;4;5;6;8;9;10]

let min_reask_delay = define_expert_option current_section ["min_reask_delay"]
  "The minimal delay between two connections to the same client (in seconds)"
    int_option 600

let display_downloaded_results = define_expert_option current_section ["display_downloaded_results"]
  "Whether to display results already downloaded"
    bool_option true

let filter_table_threshold = define_expert_option current_section ["filter_table_threshold"]
  "Minimal number of results for filter form to appear"
    int_option 50

let client_buffer_size = define_expert_option current_section ["client_buffer_size"]
  "Maximal size in byte of the buffers of a client, minimum 50.000 byte.
For high-volume links raise this value to 1.000.000 or higher."
    int_option 500000

let save_options_delay = define_expert_option current_section ["save_options_delay"]
  ~restart: true
  "The delay between two saves of the 'downloads.ini' file (default is 15 minutes).
  Changes to this option require a core restart."
    float_option 900.0

let server_connection_timeout = define_expert_option current_section ["server_connection_timeout"]
  "timeout when connecting to a server"
    float_option 30.

let download_sample_rate = define_expert_option current_section ["download_sample_rate"]
  ~restart: true
  "The delay between one glance at a file and another"
    float_option 1.

let download_sample_size = define_expert_option current_section ["download_sample_size"]
  "How many samples go into an estimate of transfer rates"
    int_option 100

let calendar = define_expert_option current_section ["calendar"]
  "This option defines a set of date at which some commands have to be executed.
  For each tuple, the first argument is a list of week days (from 0 to 6),
  the second is a list of hours (from 0 to 23) and the last one a command to
  execute. Can be used with 'pause all' and 'resume all' for example to
  resume and pause downloads automatically for the night."
    (list_option (tuple3_option (list_option int_option,list_option int_option, string_option)))
    []

let compaction_overhead = define_expert_option current_section ["compaction_overhead"]
  "The percentage of free memory before a compaction is triggered"
    int_option 25

let space_overhead = define_expert_option current_section ["space_overhead"]
  "The major GC speed is computed from this parameter. This is the memory
  that will be \"wasted\" because the GC does not immediatly collect 
  unreachable blocks. It is expressed as a percentage of the memory used
  for live data. The GC will work more (use more CPU time and collect 
  blocks more eagerly) if space_overhead is smaller."
    percent_option 80

let max_displayed_results = define_expert_option current_section ["max_displayed_results"]
  "Maximal number of results displayed for a search"
    int_option 1000

let options_version = define_expert_option current_section ["options_version"]
  ~internal: true
  "(internal option)"
    int_option 23

let max_comments_per_file = define_expert_option current_section ["max_comments_per_file"]
  "Maximum number of comments per file"
  int_option 100

let max_comment_length = define_expert_option current_section ["max_comment_length"]
  "Maximum length of file comments"
  int_option 256


(*************************************************************************)
(*                                                                       *)
(*                         Debug section                                 *)
(*                                                                       *)
(*************************************************************************)

let current_section = debug_section

let allow_local_network = define_expert_option current_section ["allow_local_network"]
  "If this option is set, IP addresses on the local network are allowed"
     bool_option false

let log_size = define_expert_option current_section ["log_size"]
  "size of log in number of records"
    int_option 300

let log_file_size = define_expert_option current_section ["log_file_size"]
   "Maximum size of log_file in MB, this value is only checked on startup,
   log_file will be deleted if its bigger than log_file_size."
     int_option 2

let log_file = define_expert_option current_section ["log_file"]
  "The file in which you want mldonkey to log its debug messages. If you
  set this option, mldonkey will log this info in the file until you use the
  'close_log' command. The log file may become very large. You can
  also enable logging in a file after startup using the 'log_file' command."
    string_option "mlnet.log"

let log_to_syslog = define_expert_option current_section ["log_to_syslog"]
  "Post log messages to syslog. This setting is independent of log_file
  and its associated commands, therefore close_log does not stop log to syslog.
  Its therefore possible to log to syslog and log_file at the same time."
    bool_option false

let gui_log_size = define_expert_option current_section ["gui_log_size"]
  "number of lines for GUI console messages"
    int_option 30




(*************************************************************************)
(*                                                                       *)
(*                         HOOKS On options                              *)
(*                                                                       *)
(*************************************************************************)

let current_section = other_section

let last_high_id = ref Ip.null

let client_ip sock =
  if !!force_client_ip then !!set_client_ip
  else
    if !last_high_id <> Ip.null then
      begin
        if Ip.usable !last_high_id && !!set_client_ip <> !last_high_id then
          set_client_ip =:= !last_high_id;
        !last_high_id
      end
    else
      match sock with
        None -> !!set_client_ip
      | Some sock ->
          let ip = TcpBufferedSocket.my_ip sock in
          if Ip.usable ip && !!set_client_ip <> ip then
            set_client_ip =:= ip;
          ip

let start_running_plugins = ref false

let filter_search_delay = 5.0

(* Infer which nets to start depending on the name used *)
let _ =
  let name = String.lowercase (Filename.basename Sys.argv.(0)) in
  let name = try
      let pos = String.index name '+' in
      String.sub name 0 pos
    with _ -> name in
  let name = try
      let pos = String.index name '.' in
      String.sub name 0 pos
    with _ -> name in

  match name with
  | "mldc" -> enable_directconnect =:= true
  | "mlgnut" -> enable_gnutella =:= true
  | "mldonkey" -> enable_donkey =:= true;
  | "mlslsk" -> enable_soulseek =:= true
  | "mlbt" -> enable_bittorrent =:= true
  | "mlnap" -> enable_opennap =:= true
  | _ ->
(* default *)
      enable_donkey =:= true;
      enable_bittorrent =:= true

let win_message =
"\n\nNEVER close this window with the close button
on the top right corner of this window!
Instead use the kill command in Telnet or HTML,
the kill function of a GUI or CTRL+C.\n\n"

let real_max_indirect_connections = ref 0

let calc_real_max_indirect_connections () =
  real_max_indirect_connections :=
    !!max_opened_connections * !!max_indirect_connections / 100

let _ =
  option_hook max_indirect_connections (fun _ ->
    begin
      if !!max_indirect_connections > 70 then max_indirect_connections =:= 70
      else if !!max_indirect_connections < 30 then max_indirect_connections =:= 30
    end;
    calc_real_max_indirect_connections ()
  );
  option_hook max_release_slots (fun _ ->
    if !!max_release_slots > 75 then max_release_slots =:= 75
  );
  option_hook min_reask_delay (fun _ ->
    if !!min_reask_delay < 600 then min_reask_delay =:= 600
  );
  option_hook share_scan_interval (fun _ ->
    if !!share_scan_interval < 5 && !!share_scan_interval <> 0 then share_scan_interval =:= 5
  );
  option_hook global_login (fun _ ->
      let len = String.length !!global_login in
      let prefix = "mldonkey_" in
      let prefix_len = String.length prefix in
      if len > prefix_len &&
        String.sub !!global_login 0 prefix_len = prefix then
        global_login =:= new_name ()
  );

  let lprintf_to_file = ref false in
  option_hook log_file (fun _ ->
      if !!log_file <> "" then
        try
    if Unix32.file_exists !!log_file then
      if (Unix32.getsize !!log_file)
       > (Int64ops.megabytes !!log_file_size) then begin
        Sys.remove !!log_file;
              lprintf_nl (_b "Logfile %s reset: bigger than %d MB") !!log_file !!log_file_size
      end;
          let oc = open_out_gen [Open_creat; Open_wronly; Open_append] 0o644 !!log_file in
          lprintf_to_file := true;
          if Autoconf.system = "cygwin" then lprintf "%s" win_message;
          lprintf_nl (_b "Logging in %s") ( Filename.concat file_basedir !!log_file);
          log_to_file oc;
    lprintf_nl "Started logging..."
        with e ->
            lprintf_nl "Exception %s while opening log file: %s"
              (Printexc2.to_string e) !!log_file
      else
        if !lprintf_to_file then begin
          lprintf_to_file := false;
          close_log ()
        end
  );
  option_hook buffer_writes_threshold (fun _ ->
      Unix32.max_buffered := Int64.of_int (1024 * !!buffer_writes_threshold));
  option_hook log_size (fun _ ->
      lprintf_max_size := !!log_size
  );
  option_hook hdd_temp_minfree (fun _ ->
      if !!hdd_temp_minfree < 50 then
        hdd_temp_minfree =:= 50);
  option_hook hdd_coredir_minfree (fun _ ->
      if !!hdd_coredir_minfree < 20 then
        hdd_coredir_minfree =:= 20);
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };
  );
  option_hook space_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.space_overhead = !!space_overhead };
  );
  option_hook tcpip_packet_size (fun _ ->
      TcpBufferedSocket.ip_packet_size := !!tcpip_packet_size
  );
  option_hook mtu_packet_size (fun _ ->
      TcpBufferedSocket.mtu_packet_size := !!mtu_packet_size
  );
  option_hook minimal_packet_size (fun _ ->
      TcpBufferedSocket.minimal_packet_size := !!minimal_packet_size
  );
  option_hook minor_heap_size (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.minor_heap_size =
        (!!minor_heap_size * 1024) };
  );
  option_hook client_buffer_size (fun _ ->
      TcpBufferedSocket.max_buffer_size := max 50000 !!client_buffer_size
  );
  if Autoconf.has_gd then begin
    option_hook html_mods_vd_gfx_png (fun _ ->
      if not Autoconf.has_gd_png && !!html_mods_vd_gfx_png then html_mods_vd_gfx_png =:= false;
      if not Autoconf.has_gd_jpg && not !!html_mods_vd_gfx_png then html_mods_vd_gfx_png =:= true
        );
        option_hook html_mods_vd_gfx_h_intervall (fun _ -> 
        let values = [1; 2; 3; 4; 5; 10; 15; 20; 30; 60] in 
        let v = List.find ((<=) (min !!html_mods_vd_gfx_h_intervall 60)) values in 
        if v <> !!html_mods_vd_gfx_h_intervall then html_mods_vd_gfx_h_intervall =:= v 
    )
        end
        
let verbose_msg_clients = ref false
let verbose_msg_raw = ref false
let verbose_msg_clienttags = ref false
let verbose_msg_servers = ref false
let verbose = ref false
let verbose_sources = ref 0
let verbose_download = ref false
let verbose_no_login = ref false
let verbose_upload = ref false
let verbose_unknown_messages = ref false
let verbose_overnet = ref false
let verbose_location = ref false
let verbose_share = ref false
let verbose_md4 = ref false
let verbose_connect = ref false
let verbose_udp = ref false
let verbose_supernode = ref false
let verbose_swarming = ref false
let verbose_activity = ref false
let verbose_user_commands = ref false
let verbose_geoip = ref false
let verbose_unexpected_messages = ref false
let verbose_dht = ref (ref false)

let set_all v =
  verbose_msg_clients := v;
  verbose_msg_raw := v;
  verbose_msg_clienttags := v;
  verbose_msg_servers := v;
  verbose := v;
  BasicSocket.debug := v;
  TcpServerSocket.debug := v;
  UdpSocket.debug := v;
  Unix32.verbose := v;
  GuiProto.verbose_gui_decoding := v;
  verbose_download := v;
  verbose_upload := v;
  verbose_no_login := v;
  verbose_unknown_messages := v;
  verbose_overnet := v;
  verbose_location := v;
  verbose_share := v;
  verbose_md4 := v;
  verbose_connect := v;
  verbose_udp := v;
  verbose_supernode := v;
  verbose_swarming := v;
  Http_client.verbose := v;
  Http_server.verbose := v;
  verbose_activity := v;
  verbose_user_commands := v;
  Geoip.verbose := v;
  verbose_unexpected_messages := v;
  !verbose_dht := v

let _ =
  option_hook verbosity (fun _ ->
      BasicSocket.verbose_bandwidth := 0;
      verbose_sources := 0;
      set_all false;
      List.iter (fun s ->
          match s with
          | "mc" -> verbose_msg_clients := true
          | "mr" | "raw" -> verbose_msg_raw := true
          | "mct" -> verbose_msg_clienttags := true
          | "ms" -> verbose_msg_servers := true
          | "verb" -> verbose := true
          | "sm" -> incr verbose_sources
          | "net" -> BasicSocket.debug := true; TcpServerSocket.debug := true; UdpSocket.debug := true
          | "file" -> Unix32.verbose := true
          | "gui" -> GuiProto.verbose_gui_decoding := true
          | "no-login" -> verbose_no_login := true
          | "do" -> verbose_download := true
          | "up" -> verbose_upload := true
          | "unk" -> verbose_unknown_messages := true
          | "ov" -> verbose_overnet := true
          | "loc" -> verbose_location := true
          | "share" -> verbose_share := true
          | "md4" -> verbose_md4 := true
          | "connect" -> verbose_connect := true
          | "udp" -> verbose_udp := true
          | "ultra" | "super" -> verbose_supernode := true
          | "swarming" -> verbose_swarming := true
          | "hc" -> Http_client.verbose := true
          | "hs" -> Http_server.verbose := true
          | "act" -> verbose_activity := true
          | "bw" -> incr BasicSocket.verbose_bandwidth
          | "unexp" -> verbose_unexpected_messages := true
          | "com" -> verbose_user_commands := true
          | "geo" -> Geoip.verbose := true
          | "dht" -> !verbose_dht := true

          | "all" ->

              verbose_sources := 1;
              set_all true;

          | _ -> lprintf_nl "Unknown verbosity tag: %s" s

      ) (String2.split_simplify !!verbosity ' ')
  )


let _ =
  option_hook log_to_syslog (fun _ ->
    match !Printf2.syslog_oc with
      None ->
        if !!log_to_syslog then
          begin
            Printf2.syslog_oc := (
              try
                Some (Syslog.openlog (Filename.basename Sys.argv.(0)))
              with e -> log_to_syslog =:= false;
                lprintf_nl "error while opening syslog %s" (Printexc2.to_string e); None);
            lprintf_nl "activated syslog"
          end
    | Some oc ->
        if not !!log_to_syslog then
          begin
            lprintf_nl "deactivated syslog";
            Syslog.closelog oc;
            Printf2.syslog_oc := None
          end
  );
  option_hook loop_delay (fun _ ->
     BasicSocket.loop_delay := (float_of_int !!loop_delay) /. 1000.;
  );
  option_hook socket_keepalive (fun _ ->
      BasicSocket.socket_keepalive := !!socket_keepalive
  )

(* convert "|" to "\|" and "\|" to "|" *)
let quote_unquote_bars m =
  let len = String.length m in
  let result = Buffer.create len in
  let rec aux i =
    if i = len then
      Buffer.contents result
    else match m.[i] with
      | '|' -> 
    Buffer.add_string result "\\|";
    aux (i+1)
      | '\\' -> 
    aux_escaped (i+1)
      | _ -> 
    Buffer.add_char result m.[i];
    aux (i+1)
  and aux_escaped i =
    if i = len then begin
      Buffer.add_char result '\\';
      Buffer.contents result
    end else match m.[i] with
      | '|' -> 
    Buffer.add_char result '|';
    aux (i+1)
      | _ ->
    Buffer.add_char result '\\';
    aux i
  in aux 0
      
let _ =
  let regex_fun str = 
    if str <> "" then
      let r = Str.regexp_case_fold (quote_unquote_bars str) in
      (fun s -> 
        try
              ignore (Str.search_forward r s 0);
              false
        with Not_found -> true
      )
    else (fun _ -> true)
  in

  option_hook messages_filter (fun _ ->
      is_not_spam := regex_fun !!messages_filter
  );

  option_hook comments_filter (fun _ ->
      is_not_comment_spam := regex_fun !!comments_filter
  )

let http_proxy = ref None

let http_proxy_tcp_update _ =
  if !!http_proxy_tcp then
    TcpBufferedSocket.http_proxy := !http_proxy
  else
    TcpBufferedSocket.http_proxy := None

let _ =
  let proxy_update _ =
    let auth = match !!http_proxy_login with
    | "" -> None
    | _ -> Some (!!http_proxy_login, !!http_proxy_password)
    in
    http_proxy := 
      (match !!http_proxy_server with
      | "" -> None
      | _  -> Some (!!http_proxy_server, !!http_proxy_port, auth));
    http_proxy_tcp_update ()
  in
  option_hook http_proxy_server proxy_update;
  option_hook http_proxy_port proxy_update;
  option_hook http_proxy_login proxy_update;
  option_hook http_proxy_password proxy_update;
  option_hook http_proxy_tcp http_proxy_tcp_update

let _ =
  option_hook allow_local_network (fun _ ->
      Ip.allow_local_network := !!allow_local_network)

let web_infos_table = Hashtbl.create 10

exception Found_web_infos of web_infos

let web_infos_find url =
  let found = ref None in
  (try
    Hashtbl.iter (fun key w ->
      if w.url = url then raise (Found_web_infos w)
    ) web_infos_table
  with Found_web_infos w -> found := Some w);
  !found

let web_infos_remove url =
  let delete_list = ref [] in
  Hashtbl.iter (fun key w -> 
    if w.url = url then delete_list := !delete_list @ [key]
  ) web_infos_table;
  List.iter (fun key -> Hashtbl.remove web_infos_table key) !delete_list

let web_infos_add kind period url =
  (match web_infos_find url with
  | None -> ()
  | Some w -> web_infos_remove w.url);
  Hashtbl.add web_infos_table (kind, period, url)
    {
      kind = kind;
      period = period;
      url = url;
      state = None;
    }

let web_infos_replace old_url new_url =
  Hashtbl.iter (fun key w -> 
    if w.url = old_url then w.url <- new_url
  ) web_infos_table

let _ =
(* convert list option web_infos to a hashtable for better usage *)
  set_after_load_hook downloads_ini (fun _ ->
    List.iter (fun (kind, period, url) ->
      web_infos_add kind period url
    ) !!web_infos;
    web_infos =:= []
  );
  set_before_save_hook downloads_ini (fun _ ->
    Hashtbl.iter (fun _ w ->
      web_infos =:= !!web_infos @ [(w.kind, w.period, w.url)]
    ) web_infos_table
  );
  set_after_save_hook downloads_ini (fun _ ->
    web_infos =:= []
  )

let rec update_options () =
  let update v =
      lprintf_nl "Updating options to version %i" v;
      options_version =:= v;
      update_options ()
  in

  match !!options_version with
    0 ->
      web_infos =:= List.map (fun (kind, period, url) ->
          kind, period * Date.day_in_hours, url
      ) !!web_infos;
      web_infos_add "rss" 6 "http://www.ed2k-it.com/forum/news_rss.php";
      web_infos_add "rss" 6 "http://www.torrents.co.uk/backend.php";
      web_infos_add "rss" 6 "http://varchars.com/rss/suprnova-movies.rss";
      update 1

  | 1 ->
      (* 5 ms is a good unit, for measuring latency between clients. *)
      loop_delay =:= 5;
      update 2

  | 2 ->
      web_infos_remove "http://www.ed2k-it.com/forum/news_rss.php";
      web_infos_remove "http://www.torrents.co.uk/backend.php";
      web_infos_remove "http://varchars.com/rss/suprnova-movies.rss";
      if !!min_reask_delay = 720 then min_reask_delay =:= 600;
      update 3

  | 3 ->
      web_infos_remove "http://members.lycos.co.uk/appbyhp2/FlockHelpApp/contact-files/contact.ocl";
      web_infos_add "contact.dat" 168 "http://www.overnet.org/download/contact.dat";
      update 4

  | 4 ->
      web_infos_remove "http://ocbmaurice.dyndns.org/pl/slist.pl/server.met?download/server-best.met";
      web_infos_add "server.met" 0 "http://www.gruk.org/server.met.gz";
      update 5

  | 5 ->
      if !!max_indirect_connections > 50 then
          max_indirect_connections =:= 20;
      update 6

  | 6 ->
      (* it's more natural to use | instead of \| for simple case *) 
      messages_filter =:= quote_unquote_bars !!messages_filter;
      update 7

  | 7 ->
      (* update to 20 because of dynamic_loop_delay patch *)
      loop_delay =:= 20;
      update 8

  | 8 ->
      web_infos_add "geoip.dat" 0 "http://www.maxmind.com/download/geoip/database/GeoIP.dat.gz";
      update 9

  | 9 ->
      web_infos_remove "http://www.gruk.org/server.met.gz";
      web_infos_add "server.met" 0 "http://www.jd2k.com/server.met";
      update 10

  | 10 ->
      web_infos_remove "http://www.overnet.org/download/contact.dat";
      web_infos_add "contact.dat" 168 "http://download.overnet.org/contact.dat";
      update 11

  | 11 ->
      web_infos_remove "http://www.bluetack.co.uk/config/antip2p.txt";
      web_infos_add "guarding.p2p" 0 "http://www.bluetack.co.uk/config/level1.gz";
      update 12

  | 12 ->
      web_infos_add "nodes.gzip" 0 "http://update.kceasy.com/update/fasttrack/nodes.gzip";
      update 13

  | 13 ->
      web_infos_remove "http://www.jd2k.com/server.met";
      web_infos_add "server.met" 0 "http://www.gruk.org/server.met.gz";
      update 14

  | 14 ->
      (* set back to 5 because dynamic_loop_delay patch was removed *)
      loop_delay =:= 5;
      update 15

  | 15 ->
      if !!messages_filter = "Your client is connecting too fast" then
        messages_filter =:= "DI-Emule|ZamBoR|Ketamine|eMule FX|AUTOMATED MESSAGE";
      update 16

  | 16 ->
      if !!download_sample_size = 10 then download_sample_size =:= 100;
      update 17

  | 17 ->
      web_infos_add "hublist" 0 "http://dchublist.com/hublist.config.bz2";
      update 18

  | 18 ->
      if !!messages_filter = "DI-Emule|ZamBoR|Ketamine|eMule FX|AUTOMATED MESSAGE" then
        messages_filter =:= "DI-Emule|ZamBoR|Ketamine|eMule FX|AUTOMATED MESSAGE|Hi Honey!|Do you live in my area|download HyperMule";
      update 19

  | 19 ->
      if !!share_scan_interval = 5 then share_scan_interval =:= 30;
      update 20

  | 20 ->
      web_infos_replace
        "http://www.maxmind.com/download/geoip/database/GeoIP.dat.gz"
        "http://www.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz";
      update 21

  | 21 ->
      web_infos_remove "http://download.overnet.org/contact.dat";
      enable_overnet =:= false;
      update 22

  | 22 ->
      web_infos_remove "http://www.bluetack.co.uk/config/level1.gz";
      web_infos_add "guarding.p2p" 0 "http://upd.emule-security.org/ipfilter.zip";
      update 23

  | _ -> ()
