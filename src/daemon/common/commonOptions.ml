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
open Md4
open BasicSocket
open Options
open Unix

let home_dir = (try Sys.getenv "HOME" with _ -> ".")
  
let installer_ini = create_options_file (Filename.concat home_dir 
      ".mldonkey_installer.ini")
  
let mldonkey_directory = 
  define_option installer_ini ["mldonkey_directory"] 
    "The directory where mldonkey's option files are" string_option "."

let _ =
  (try Options.load installer_ini with _ -> ())
  
let (file_basedir, home_basedir) = 
  try
    if (String2.starts_with 
          (Filename.basename Sys.argv.(0))  "mlgui")
    then raise Exit;
    let chroot_dir = Sys.getenv "MLDONKEY_CHROOT" in
    try
      Unix.chdir chroot_dir;
      let new_passwd = Filename.concat chroot_dir "/etc/passwd" in
      if not (Sys.file_exists new_passwd) then begin
          lprintf "No /etc/passwd in your chroot directory\n create one if you want to use 'run_as_user' option\n"
        end;
      MlUnix.chroot chroot_dir;
      lprintf "mldonkey is now running in %s\n"  chroot_dir;
        ".", "."
        
    with e ->
        lprintf "Exception %s trying to chroot %s"
          (Printexc2.to_string e) chroot_dir;
        lprint_newline ();
        exit 2
  with _ ->  
      (try Sys.getenv "MLDONKEY_DIR" with _ -> 
            !!mldonkey_directory),
      home_dir
      
let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)
  
let downloads_ini = create_options_file (
    Filename.concat file_basedir "downloads.ini")
let downloads_expert_ini = create_options_file (
    Filename.concat file_basedir "downloads_expert.ini")
  
let servers_ini = create_options_file (
    Filename.concat file_basedir "servers.ini")
let searches_ini = create_options_file (
    Filename.concat file_basedir "searches.ini")
  (*
let clients_ini = create_options_file (
    Filename.concat file_basedir "files.ini")
  *)
let files_ini = create_options_file (
    Filename.concat file_basedir "files.ini")
let friends_ini = create_options_file (
    Filename.concat file_basedir "friends.ini")

(******************

    BASIC OPTIONS

********************)

  
let _ = Random.self_init ()

let random_letter () =
  char_of_int (97 + Random.int 26)

let new_name () = 
  (Printf.sprintf "%c%c%c%c%c%c" 
    (random_letter ()) (random_letter ()) (random_letter ()) 
    (random_letter ()) (random_letter ()) (random_letter ()))
  
let client_name = define_option downloads_ini ["client_name"] 
    "small name of client" string_option (new_name ())

let _ =
  let in_hook = ref false in
  option_hook client_name (fun _ ->
      let len = String.length !!client_name in
      let prefix = "mldonkey_" in
      let prefix_len = String.length prefix in
      if len > prefix_len && 
        String.sub !!client_name 0 prefix_len = prefix then
        client_name =:= new_name ()
  )


let allow_browse_share = define_option downloads_ini ["allow_browse_share"]
    "Allow others to browse our share list" bool_option true

let buffer_writes = define_option downloads_ini ["buffer_writes"]
    "Buffer writes and flush after buffer_writes_delay seconds (experimental)"
    bool_option false

let buffer_writes_delay = define_option downloads_expert_ini ["buffer_writes_delay"]
    "Buffer writes and flush after buffer_writes_delay seconds (experimental)" 
    float_option 30.

let buffer_writes_threshold = define_option downloads_expert_ini ["buffer_writes_threshold"]
  "Flush buffers if buffers exceed buffer_writes_threshold kB (experimental)" 
    int_option 1024

let _ =
  option_hook buffer_writes_threshold (fun _ ->
      Unix32.max_buffered := Int64.of_int (1024 * !!buffer_writes_threshold))

let dir_option = define_option_class "shared directory"
    (fun v -> 
      match v with
        SmallList [dir; prio] 
      | List [dir; prio]
        -> value_to_string dir, value_to_int prio
      | _ -> value_to_string v, 0
      
      )
  (fun (dir, prio) -> SmallList [string_to_value dir; int_to_value prio])
  
let shared_directories = 
  define_option downloads_ini ["shared_directories" ] 
    "Directories where files will be shared, prio of that directories"
  (list_option dir_option) []

      
let gui_port = 
  define_option downloads_ini ["gui_port"] "port for user interaction" int_option 4001

let http_port = 
  define_option downloads_ini ["http_port"] "The port used to connect to your client with a WEB browser" int_option 4080

let telnet_port = define_option downloads_ini ["telnet_port"] "port for user interaction" int_option 4000

  (*
let http_login = 
  define_option downloads_ini ["http_login"] "Your login when using a WEB browser" string_option ""
  
let http_password = 
  define_option downloads_ini ["http_password"] "Your password when using a WEB browser" string_option ""
    *)

let max_hard_upload_rate = define_option downloads_ini ["max_hard_upload_rate"] 
  "The maximal upload rate you can tolerate on your link in kBytes/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
control and data messages." int_option 7
  
let max_hard_download_rate = define_option downloads_ini ["max_hard_download_rate"] 
  "The maximal download rate you can tolerate on your link in kBytes/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
control and data messages." int_option 50


  (*
let password = define_option downloads_ini ["password"] 
  "The password to access your client from the GUI (setting it disables
  the command-line client)" string_option ""
*)
  
let allowed_ips = define_option downloads_ini ["allowed_ips"]
  "list of IP address allowed to connect to the core via telnet/GUI/WEB
list separated by spaces, wildcard=255 ie: use 192.168.0.255 for 192.168.0.* "
    (list_option Ip.option) [Ip.of_string "127.0.0.1"]  
  
let start_running_plugins = ref false
  
let enable_server = define_option downloads_expert_ini
    ["enable_server"]
  "Set to true if you also want mldonkey to run as a server (experimental)"
    bool_option false

let enable_overnet = define_option downloads_ini
    ["enable_overnet"]
  "Set to true if you also want mldonkey to run as an overnet client (enable_donkey must be true)"
    bool_option false

let enable_bittorrent = define_option downloads_ini
    ["enable_bittorrent"]
  "Set to true if you also want mldonkey to run as an Bittorrent client"
    bool_option false

let enable_donkey = define_option downloads_ini
    ["enable_donkey"]
  "Set to true if you also want mldonkey to run as a donkey client"
    bool_option false
    
let enable_opennap = define_option downloads_ini
    ["enable_opennap"]
  "Set to true if you also want mldonkey to run as a napster client (experimental)"
    bool_option false  
  
let enable_soulseek = define_option downloads_ini
    ["enable_soulseek"]
  "Set to true if you also want mldonkey to run as a soulseek client (experimental)"
    bool_option false
    
let enable_audiogalaxy = define_option downloads_expert_ini
    ["enable_audiogalaxy"]
  "Set to true if you also want mldonkey to run as an audiogalaxy satellite (experimental)"
    bool_option false
  
let enable_gnutella = define_option downloads_ini
    ["enable_gnutella"]
  "Set to true if you also want mldonkey to run as a gnutella1/2 sub node (experimental)"
    bool_option false
  
let enable_fasttrack = define_option downloads_ini
    ["enable_fasttrack"]
  "Set to true if you also want mldonkey to run as a Fasttrack sub node (experimental)"
    bool_option false
  
let enable_directconnect = define_option downloads_ini
    ["enable_directconnect"]
  "Set to true if you also want mldonkey to run as a direct-connect node (experimental)"
    bool_option false

let enable_openft = define_option downloads_expert_ini
    ["enable_openft"]
  "Set to true if you also want mldonkey to run as a OpenFT sub node (experimental)"
    bool_option false
  
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
  | "mldonkey" -> enable_donkey =:= true; enable_overnet =:= true
  | "mlslsk" -> enable_soulseek =:= true
  | "mlbt" -> enable_bittorrent =:= true
  | "mlnap" -> enable_opennap =:= true
  | "mlcymes" -> enable_server =:= true
  | _ -> 
(* default *)
      enable_donkey =:= true;
      enable_overnet =:= true;
      enable_bittorrent =:= true
  
let auto_commit = define_option downloads_ini
    ["auto_commit"]
  "Set to false if you don't want mldonkey to automatically put completed files in incoming directory"
    bool_option true

let smtp_server = define_option downloads_ini ["smtp_server"] 
  "The mail server you want to use (must be SMTP). Use hostname or IP address"
    string_option "127.0.0.1"

let smtp_port = define_option downloads_ini ["smtp_port"] 
  "The port to use on the mail server (default 25)"
  int_option 25

let mail = define_option downloads_ini ["mail"]
  "Your e-mail if you want to receive mails when downloads are completed"
    string_option ""

let add_mail_brackets = define_option downloads_ini ["add_mail_brackets"]
  "Does your mail-server need <...> around addresses"
  bool_option false

  
let max_concurrent_downloads = define_option downloads_ini 
    ["max_concurrent_downloads"] 
  "The maximal number of files in Downloading state (other ones are Queued)"
    int_option 60
  
let filename_in_subject = define_option downloads_ini ["filename_in_subject"]
    "Send filename in mail subject" bool_option true

let web_infos = define_option downloads_ini
    ["web_infos"] "A list of lines to download on the WEB: each line has 
    the format: (kind, period, url), where kind is either
    'server.met' (for a server.met file), or 'comments.met' for
    a file of comments, and period is the period between updates 
    (in days), and url is the url of the file to download.
    IMPORTANT: Put the URL and the kind between quotes.
    EXAMPLE:
 web_infos = [
  ('server.met', 1, 'http://www.primusnet.ch/users/komintern/ed2k/min/server.met'
 )]
  "
    (list_option (
      tuple3_option (string_option, int_option, string_option)))
  [
    ("server.met", 1, "http://ocbmaurice.dyns.net/pl/slist.pl?download");
    ("ocl",1, "http://members.lycos.co.uk/appbyhp2/FlockHelpApp/contact-files/contact.ocl" );

  ]

let _ = 
  option_hook web_infos (fun _ ->
      List.iter (fun remove ->
          if List.mem remove !!web_infos then
            web_infos =:= List2.remove remove !!web_infos
      )
      [
        ("server.met", 1, "http://savannah.nongnu.org/download/mldonkey/network/servers.met");        
        ("ocl",1, "http://savannah.nongnu.org/download/mldonkey/network/peers.ocl");
      ]  
  )
  
(********************

    EXPERT OPTIONS

*********************)
  
      
let shared_extensions = define_option downloads_expert_ini ["shared_extensions"]  
  "A list of extensions of files that should be shared (\".mp3\",\".avi\" for examples). 
  Files with extensions not in the list will not be shared (except if the list 
   is empty :)"
    (list_option string_option) []

let _ =
  option_hook shared_extensions (fun _ ->
      let list = List.map (fun ext ->
            if String.length ext > 0 && ext.[0] <> '.' then "." ^ ext else ext
        ) !!shared_extensions in
      if list <> !!shared_extensions then shared_extensions =:= list
      
  )
  
let client_timeout = define_option downloads_expert_ini ["client_timeout"] 
  "Timeout on client connections when not queued" float_option 40.

let interface_buffer = define_option downloads_expert_ini ["interface_buffer"] 
  "The size of the buffer between the client and its GUI. Can be useful
to increase when the connection between them has a small bandwith" int_option
  1000000

let max_name_len = define_option downloads_expert_ini ["max_name_len"]
    "The size long names will be shorten to in the interface"
  int_option 50

let term_ansi = define_option downloads_expert_ini ["term_ansi"]
    "Is the default terminal an ANSI terminal (escape sequences can be used)"
  bool_option true
  
let previewer = define_option downloads_expert_ini ["previewer"]
  "Name of program used for preview (first arg is local filename, second arg
    is name of file as searched on eDonkey" string_option
  "mldonkey_previewer"

let update_gui_delay = define_option downloads_expert_ini ["update_gui_delay"] 
  "Delay between updates to the GUI" float_option 1.
 
let temp_directory = define_option downloads_ini ["temp_directory" ] 
    "The directory where temporary files should be put" 
    string_option (Filename.concat file_basedir "temp")

let incoming_directory_prio =
  define_option downloads_expert_ini ["incoming_directory_prio" ]
      "The upload prio of the incoming directory"
          int_option 0

let incoming_directory = 
  define_option downloads_ini ["incoming_directory" ] 
    "The directory where downloaded files should be moved after commit" 
    string_option (Filename.concat file_basedir "incoming")

let http_realm = 
  define_option downloads_expert_ini ["http_realm"] "The realm shown when connecting with a WEB browser" string_option "MLdonkey"

let set_client_ip = define_option downloads_ini ["client_ip"] 
    "The last IP address used for this client" Ip.option  
    (Ip.my ())
  
let force_client_ip = define_option downloads_ini ["force_client_ip"] 
  "Use the IP specified by 'client_ip' instead of trying to determine it
    ourself. Don't set this option to true if you have dynamic IP."
    bool_option false
  
let use_html_frames = define_option downloads_expert_ini ["use_html_frames"] 
    "This option controls whether the WEB interface should use frames or not" bool_option true

let commands_frame_height = define_option downloads_expert_ini ["commands_frame_height"] "The height of the command frame in pixel (depends on your screen and browser sizes)" int_option 80

let _ = 
  Options.set_string_wrappers allowed_ips 
    (fun list ->
      List.fold_left (fun s ip ->
          Printf.sprintf "%s %s" (Ip.to_string ip) s
      ) "" list
  )
  (fun s ->
      let list = String2.tokens s in
      List.map (fun ip -> Ip.of_string ip) list
  )
  
  
let ask_for_gui = define_option downloads_ini ["ask_for_gui"]
    "Ask for GUI start"    bool_option true
    
let start_gui = define_option downloads_ini ["start_gui"]
    "Automatically Start the GUI" bool_option false

let bin_dir = Filename.dirname Sys.argv.(0)
  
let mldonkey_bin = define_option downloads_expert_ini ["mldonkey_bin"]
    "Directory where mldonkey binaries are installed"
    string_option bin_dir

let mldonkey_gui = define_option downloads_expert_ini ["mldonkey_gui"]
    "Name of GUI to start" string_option 
    (Filename.concat bin_dir "mlgui")

(*
let filter_search = define_option downloads_expert_ini ["filter_search"]
  "Should mldonkey filter results of searches
  (results are displayed only if they exactly match the request,
    and filtering is done every 'filter_search_delay'." 
    bool_option false
    *)

let filter_search_delay = define_option downloads_expert_ini ["filter_search_delay"]
  "Delay before two filtering on results (results
    are not displayed until filtered). Min is 1 second." float_option 5.
  
let tcpip_packet_size = define_option downloads_expert_ini ["tcpip_packet_size"]
  "The size of the header of a TCP/IP packet on your connection (ppp adds
    14 bytes sometimes, so modify to take that into account)"
    int_option 40
  
let _ =
  option_hook tcpip_packet_size (fun _ ->
      TcpBufferedSocket.ip_packet_size := !!tcpip_packet_size
  )

let mtu_packet_size = define_option downloads_expert_ini ["mtu_packet_size"]
  "The size of the MTU of a TCP/IP packet on your connection"
    int_option 1500

let _ =
  option_hook mtu_packet_size (fun _ ->
      TcpBufferedSocket.mtu_packet_size := !!mtu_packet_size
  )

let minimal_packet_size = define_option downloads_expert_ini ["minimal_packet_size"]
  "The size of the minimal packet you want mldonkey to send when data is
available on the connection"
    int_option !TcpBufferedSocket.minimal_packet_size

let _ =
  option_hook minimal_packet_size (fun _ ->
      TcpBufferedSocket.minimal_packet_size := !!minimal_packet_size
  )

let network_update_url = define_option downloads_expert_ini ["network_update_url"]
    "URL where mldonkey can download update information on the network"
    string_option ""
  
let _ =
  option_hook network_update_url (fun _ ->
      if !!network_update_url = 
        "http://savannah.nongnu.org/download/mldonkey/network/" then
        network_update_url =:= "")
  
let motd_html = define_option downloads_expert_ini ["motd_html"]
    "Message printed at startup (automatically downloaded from the previous
    URL directory" string_option "Welcome to MLdonkey"
  
let run_as_user = define_option downloads_ini ["run_as_user"]
  "The login of the user you want mldonkey to run as, after the ports
  have been bound (can be use not to run with root priviledges when 
a port < 1024 is needed)" string_option ""

let run_as_useruid = define_option downloads_ini ["run_as_useruid"]
  "The UID of the user (0=disabled) you want mldonkey to run as, after the ports
  have been bound (can be use not to run with root priviledges when 
a port < 1024 is needed)" int_option 0

let addr_option  =  define_option_class "Addr" 
    (fun value ->
      let s = value_to_string value in
      let addr, port = String2.cut_at s ':' in
      addr, int_of_string port)
      (fun (addr, port) -> string_to_value (Printf.sprintf "%s:%d" addr port))

let compaction_delay = define_option downloads_expert_ini ["compaction_delay"]
    "Force compaction every <n> hours (in [1..24])"
    int_option 2
  
let vd_reload_delay = define_option downloads_expert_ini ["vd_reload_delay"]
    "The delay between reloads of the vd output in the WEB interface"
    int_option 120
    
let http_bind_addr = define_option downloads_expert_ini ["http_bind_addr"]
    "The IP address used to bind the http server"
    Ip.option (Ip.any)
  
let gui_bind_addr = define_option downloads_expert_ini ["gui_bind_addr"]
    "The IP address used to bind the gui server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
  
let telnet_bind_addr = define_option downloads_expert_ini ["telnet_bind_addr"]
    "The IP address used to bind the telnet server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
  
(** {2 Chat} *)

let chat_app_port = 
  define_option downloads_expert_ini ["chat_app_port"] 
    "port of the external chat application" 
    int_option 5036

let chat_app_host = 
  define_option downloads_expert_ini ["chat_app_host"] 
    "hostname of the external chat application" 
    string_option "localhost"

let chat_port = 
  define_option downloads_expert_ini ["chat_port"] 
    "port used by the external chat application to use the core as a proxy"
    int_option 4002

let chat_bind_addr = define_option downloads_expert_ini ["chat_bind_addr"]
    "The IP address used to bind the chat server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let chat_console_id =
  define_option downloads_expert_ini ["chat_console_id"] 
    "the id to use for communicating with the core console through chat interface" 
    string_option "donkey console"

let chat_warning_for_downloaded = define_option downloads_expert_ini
    ["chat_warning_for_downloaded"]
    "use the chat to indicate when a file has been downloaded"
    bool_option true

let max_opened_connections = define_option downloads_ini
    ["max_opened_connections"] "Maximal number of opened connections" 
  int_option (min MlUnix.max_sockets 200)

  (*
let web_header = define_option downloads_expert_ini
    ["web_header"] "The header displayed in the WEB interface"
    string_option
  "
  <h2>Connected to <a href=http://www.freesoftware.fsf.org/mldonkey/> MLdonkey </a> 
WEB server</h2>
  <br>
</table>
<table width=100% border=0>
<tr>
  <td><a href=submit?q=vm $O> View Connected Servers </a></td>
  <td><a href=submit?q=vma $O> View All Servers </a></td>
  <td><a href=submit?q=c $O> Connect More Servers </a></td>
  <td><a href=submit?q=view_custom_queries $O> Custom Searches </a></td>
  <td><a href=submit?q=xs $O> Extended Search </a></td>
  <td><a href=submit?q=upstats $O> Upload Statistics </a></td>
  </tr>
<tr>
<td><a href=submit?q=vr $O> View Results </a></td>
<td><a href=files $O> View Downloads </a></td>
<td><a href=submit?q=commit $S> Commit Downloads </a></td>
<td><a href=submit?q=vs $O> View Searches </a></td>
<td><a href=submit?q=vo $O> View Options </a></td>
<td><a href=submit?q=help $O> View Help </a></td>
  </tr>
  </table>
<br>
"
*)

let file_completed_cmd = define_option downloads_ini 
    ["file_completed_cmd"] "A command that is called when a file is completely
    downloaded. Arguments are: <file_name on disk> <md4> <size>"
    string_option "" 

  

let minor_heap_size = define_option downloads_expert_ini
    ["minor_heap_size"] "Size of the minor heap in kB"
    int_option 32
      
let _ =
  option_hook minor_heap_size (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.minor_heap_size = 
        (!!minor_heap_size * 1024) };     
  )

let min_reask_delay = define_option downloads_expert_ini ["min_reask_delay"]
  "The minimal delay between two connections to the same client (in seconds)" 
    int_option 600
  
let max_reask_delay = define_option downloads_expert_ini ["max_reask_delay"]
    "The maximal delay between two connections to the same client" 
  int_option 3600

let html_mods = define_option downloads_expert_ini
    ["html_mods"] "Whether to use the modified WEB interface" bool_option true

let html_mods_style = define_option downloads_expert_ini
    ["html_mods_style"] "Which html_mods style to use (set with html_mods_style command)" int_option 0

let html_mods_human_readable = define_option downloads_expert_ini
    ["html_mods_human_readable"] "Whether to use human readable GMk number format" bool_option true

let html_mods_use_relative_availability = define_option downloads_expert_ini
    ["html_mods_use_relative_availability"] "Whether to use relative availability in the WEB interface" bool_option true

let html_mods_vd_network = define_option downloads_expert_ini
    ["html_mods_vd_network"] "Whether to display the Net column in vd output" bool_option false

let html_mods_vd_active_sources = define_option downloads_expert_ini
    ["html_mods_vd_active_sources"] "Whether to display the Active Sources column in vd output" bool_option false

let html_mods_vd_age = define_option downloads_expert_ini
    ["html_mods_vd_age"] "Whether to display the Age column in vd output" bool_option true

let html_mods_vd_last = define_option downloads_expert_ini
    ["html_mods_vd_last"] "Whether to display the Last column in vd output" bool_option true

let html_mods_vd_prio = define_option downloads_expert_ini
    ["html_mods_vd_prio"] "Whether to display the Priority column in vd output" bool_option false

let html_mods_vd_queues = define_option downloads_expert_ini
    ["html_mods_vd_queues"] "Whether to display the Queues in vd # output" bool_option true

let html_mods_show_pending = define_option downloads_expert_ini
    ["html_mods_show_pending"] "Whether to display the pending slots in uploaders command" bool_option true

let html_mods_load_message_file = define_option downloads_expert_ini
    ["html_mods_load_message_file"] "Whether to load the mldonkey_messages.ini file (false=use internal settings)" bool_option false

let html_mods_max_messages = define_option downloads_expert_ini
    ["html_mods_max_messages"] "Maximum chat messages to log in memory" int_option 10
  
let use_html_mods o =
  o.CommonTypes.conn_output = CommonTypes.HTML && !!html_mods
  
let html_checkbox_file_list = define_option downloads_expert_ini
    ["html_checkbox_file_list"] "Whether to use checkboxes in the WEB interface" bool_option true
    
let display_downloaded_results = define_option downloads_expert_ini
    ["display_downloaded_results"] "Whether to display results already downloaded" bool_option true

    
let filter_table_threshold = define_option downloads_expert_ini
    ["filter_table_threshold"] "Minimal number of results for filter form to appear"
    int_option 50

let client_buffer_size = define_option downloads_expert_ini
    ["client_buffer_size"] "Maximal size of the buffers of a client"
    int_option 500000

let save_options_delay = 
  define_option downloads_expert_ini ["save_options_delay"] 
    "The delay between two saves of the 'downloads.ini' file (default is 4 minutes)" 
  float_option 900.0

let server_connection_timeout = define_option downloads_expert_ini
  ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 15.
  
let new_print_search = define_option downloads_expert_ini
    ["new_print_search"] "Use new display of search results (with tables,
    which might be slower for your browser to display)"
    bool_option false 
let _ =
  option_hook client_buffer_size (fun _ ->
      TcpBufferedSocket.max_buffer_size := maxi 50000 !!client_buffer_size
  )

let download_sample_rate = define_option downloads_expert_ini ["download_sample_rate"]
  "The delay between one glance at a file and another" float_option 1.
 
let download_sample_size = define_option downloads_expert_ini ["download_sample_size"]
    "How many samples go into an estimate of transfer rates" int_option 10

let calendar = define_option downloads_expert_ini ["calendar"]
  "This option defines a set of date at which some commands have to be executed"
    (list_option (tuple3_option (list_option int_option,list_option int_option,
      string_option)))
  []

let ip_cache_timeout = define_option downloads_expert_ini
    ["ip_cache_timeout"]
    "The time an ip address can be kept in the cache"
    int_option 3600

let verbosity = define_option downloads_expert_ini ["verbosity"] 
  "A space-separated list of keywords. Each keyword triggers
  printing information on the corresponding messages:
  mc : debug client messages
  ms : debug server messages
  net : debug net
  verb : debug other
  loc : debug source research
  sp : debug source propagation 
  sm : debug source management
  do : some download warnings
  up : some upload warnings
  unk : unknown messages
  ov : overnet
  share: debug sharing
  md4 : md4 computation
"
    string_option ""

let verbose_msg_servers = ref false
let verbose_msg_clients = ref false
let verbose_src_manager = ref false
let verbose_src_prop = ref false
let verbose = ref false
let verbose_download = ref false
let verbose_upload = ref false
let verbose_unknown_messages = ref false
let verbose_overnet = ref false
let verbose_location = ref false
let verbose_share = ref false
let verbose_md4 = ref false
  
let _ = 
  option_hook verbosity (fun _ ->
      
      verbose_msg_clients := false;
      verbose_msg_servers := false;
      BasicSocket.debug := false;
      verbose_src_prop := false;
      verbose_src_manager := false;
      verbose := false;
      verbose_download := false;
      verbose_upload := false;
      verbose_unknown_messages := false;
      verbose_overnet := false;
      verbose_location := false;
      verbose_share := false;
      verbose_md4 := false;
      
      List.iter (fun s ->
          match s with
          | "mc" -> verbose_msg_clients := true;
          | "ms" -> verbose_msg_servers := true;
          | "verb" -> verbose := true;
          | "sm" -> verbose_src_manager := true;
          | "net" -> BasicSocket.debug := true
          | "sp" -> verbose_src_prop := true
          | "do" -> verbose_download := true
          | "up" -> verbose_upload := true
          | "unk" -> verbose_unknown_messages := true
          | "ov" -> verbose_overnet := true
          | "loc" -> verbose_location := true
          | "share" -> verbose_share := true
          | "md4" -> verbose_md4 := true
              
          | "all" ->
              
              verbose_msg_clients := true;
              verbose_msg_servers := true;
              BasicSocket.debug := true;
              verbose_src_prop := true;
              verbose_src_manager := true;
              verbose := true;
              verbose_download := true;
              verbose_upload := true;
              verbose_unknown_messages := true;
              verbose_overnet := true;
              verbose_share := true;
              verbose_md4 := true;
              
          | _ -> ()
              
      ) (String2.split_simplify !!verbosity ' ')
  )
  
let compaction_overhead = define_option downloads_expert_ini 
    ["compaction_overhead"] 
    "The percentage of free memory before a compaction is triggered"
    int_option 25

let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )

let log_size = 
  define_option downloads_expert_ini ["log_size"]
    "size of log in number of records" int_option 300
  
let _ = 
  option_hook log_size (fun _ ->
      lprintf_max_size := !!log_size   
  )
    
let max_displayed_results = define_option downloads_expert_ini
    ["max_displayed_results"]
    "Maximal number of results displayed for a search"
    int_option 1000
  
let _ =
  option_hook filter_search_delay (fun _ ->
      if !!filter_search_delay < 1. then filter_search_delay =:= 1.)

let options_version = define_option downloads_expert_ini ["options_version"]
    "(internal option)"
    int_option 0  
  
let gui_options_panel = 
(*
  define_option downloads_expert_ini ["gui_options_panel"]
  "Which options are configurable in the GUI option panel, and in which
    sections. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple4_option (string_option, string_option, string_option, string_option)))
*)
  
  [
    "Identification", "Your client name", (shortname client_name), "T";
    (*
    "Identification", "Password", (shortname password), "T";
    "Identification", "HTTP login", (shortname http_login), "T";
"Identification", "HTTP password", (shortname http_password), "T";
  *)
    "Identification", "Allowed IPs", (shortname allowed_ips), "T";
    
    "Ports", "Client IP", (shortname set_client_ip), "T";
    "Ports", "Force Client IP", (shortname force_client_ip), "B";
    "Ports", "HTTP Port", (shortname http_port), "T";
    "Ports", "Telnet Port", (shortname telnet_port), "T";
    "Ports", "GUI Port", (shortname gui_port), "T";
    "Ports", "HTTP Bind Address", (shortname http_bind_addr), "T";
    "Ports", "Telnet Bind Address", (shortname telnet_bind_addr), "T";
    "Ports", "GUI Bind Address", (shortname gui_bind_addr), "T";
    
    "Bandwidth", "Maximal Download Bandwidth in kB/s", shortname max_hard_download_rate, "T";
    "Bandwidth", "Maximal Upload Bandwidth in kB/s", shortname max_hard_upload_rate, "T";
    "Bandwidth", "Maximal Number of Sockets Used", shortname max_opened_connections, "T";
    "Bandwidth", "Socket Buffer Size", shortname client_buffer_size, "T";

    "Delays", "Min Reask Delay", shortname min_reask_delay, "T";
    "Delays", "Max Reask Delay", shortname max_reask_delay, "T";
    "Delays", "Save Options Delay", shortname save_options_delay, "T";
    "Delays", "Server Connection Timeout", shortname server_connection_timeout, "T";
(*    "Delays", "Client Connection Timeout", shortname  *)
    "Delays", "Update GUI Delay", shortname update_gui_delay, "T";
    "Delays", "Download-rate Sample Period", shortname download_sample_rate, "T";
    "Delays", "Download-rate Samples Size", shortname download_sample_size, "T";
        
    "Chat", "External Chat Application Port", (shortname chat_app_port), "T";
    "Chat", "External Chat Application Address", (shortname chat_app_host), "T";
    "Chat", "Core Chat Port", (shortname chat_port), "T";
    "Chat", "Core Chat Nick", (shortname chat_console_id), "T";
    "Chat", "Chat Warning For Downloaded Files", (shortname chat_warning_for_downloaded), "B";
    
    "Files", "Temp directory", shortname temp_directory, "F";
    "Files", "Incoming directory", shortname incoming_directory, "F";
    "Files", "Previewer program", shortname previewer, "F";
    
    "Mail", "SMTP server", shortname smtp_server, "T";
    "Mail", "SMTP port", shortname smtp_port, "T";
    "Mail", "Your Email Address", shortname mail, "T";
    
    "Debug", "Verbosity", shortname verbosity, "T";
    
    "Startup", "Start mldonkey_gui at beginning", shortname start_gui, "B"; 
    "Startup", "Ask for mldonkey_gui start", shortname ask_for_gui, "B";
    "Startup", "Path of mldonkey binaries", shortname mldonkey_bin, "F";
    "Startup", "Name of MLdonkey GUI to start", shortname mldonkey_gui, "F";
    "Startup", "User MLdonkey should run as", shortname run_as_user, "T";
 ]  
  
let last_high_id = ref Ip.null

let client_ip sock =
  if !!force_client_ip then !!set_client_ip else
  if !last_high_id <> Ip.null then begin
    if !last_high_id <> Ip.localhost && !!set_client_ip <> !last_high_id then 
      set_client_ip =:= !last_high_id;
    !last_high_id 
  end else
  match sock with
    None -> !!set_client_ip
  | Some sock ->
      let ip = TcpBufferedSocket.my_ip sock in
      if ip <> Ip.localhost && !!set_client_ip <> ip then
	set_client_ip =:= ip;
      ip

      
let allowed_commands = define_option downloads_ini
    ["allowed_commands"]
  "Commands that you are allowed to be call from the interface. These
commands should short, so that the core is not blocked more than necessary."
    (list_option (tuple2_option (string_option, string_option)))
  [ "df", "df";
    "ls", "ls incoming"; 
  ]

let empty_password = Md4.string ""
  
let users = define_option downloads_ini ["users"]
  "The users that are defined on this core. The default user is
called 'admin', and uses an empty password. To create new users,
login as admin in mldonkey, and use the 'add_user' command."
    (list_option (tuple2_option (string_option, Md4.option)))
    [ "admin", empty_password ]

let empty_password user =
  try
    (List.assoc user !!users) = empty_password
  with _ -> false

let mlnet_redirector = define_option downloads_expert_ini ["redirector"]
    "IP:port of the network redirector"
    addr_option ("128.93.52.5", 3999)

let log_file = define_option downloads_expert_ini ["log_file"]
  "The file in which you want mldonkey to log its debug messages. If you
  set this option, mldonkey will log this info in the file until you use the
'close_log' command. The log file may become very large. You can
    also enable logging in a file after startup using the 'log_file' command."
  string_option ""
  

let max_upload_slots = define_option downloads_ini ["max_upload_slots"]
    "How many slots can be used for upload"
    int_option 5
  
let _ =  
  option_hook max_upload_slots (fun _ ->
      if !!max_upload_slots < 3 then
        max_upload_slots =:= 3)

let dynamic_slots = define_option downloads_ini ["dynamic_slots"] 
  "Set this to true if you want to have dynamic upload slot allocation (experimental)" bool_option false

    
let max_connections_per_second = define_option downloads_ini
    ["max_connections_per_second"] 
  "Maximal number of connections that can be opened per second
(will supersede max_clients_per_second in the future)"
  int_option 10
    
let delete_original = define_option downloads_ini ["delete_original"]
  "Should MLdonkey delete the file downloaded when splitting has been succesful"
    bool_option false
