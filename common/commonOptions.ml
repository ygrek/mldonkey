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
          (Filename.basename Sys.argv.(0))  "mldonkey_gui")
    then raise Exit;
    let chroot_dir = Sys.getenv "MLDONKEY_CHROOT" in
    try
      Unix.chdir chroot_dir;
      Unix.chroot chroot_dir;
      Printf.printf "mldonkey is now running in %s\n"  chroot_dir;
        ".", "."
        
    with e ->
        Printf.printf "Exception %s trying to chroot %s"
          (Printexc2.to_string e) chroot_dir;
        print_newline ();
        exit 2
  with _ ->  
      (try Sys.getenv "MLDONKEY_DIR" with _ -> 
            !!mldonkey_directory),
      home_dir
      
let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)
  
let downloads_ini = create_options_file (
    Filename.concat file_basedir "downloads.ini")
let expert_ini = downloads_ini

let servers_ini = create_options_file (
    Filename.concat file_basedir "servers.ini")
let searches_ini = create_options_file (
    Filename.concat file_basedir "searches.ini")
let clients_ini = create_options_file (
    Filename.concat file_basedir "files.ini")
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

let shared_directories = 
  define_option downloads_ini ["shared_directories" ] 
    "Directories where files will be shared"
  (list_option string_option) []

      
let gui_port = 
  define_option downloads_ini ["gui_port"] "port for user interaction" int_option 4001

let http_port = 
  define_option downloads_ini ["http_port"] "The port used to connect to your client with a WEB browser" int_option 4080

let telnet_port = define_option downloads_ini ["telnet_port"] "port for user interaction" int_option 4000

  
let http_login = 
  define_option downloads_ini ["http_login"] "Your login when using a WEB browser" string_option ""
  
let http_password = 
  define_option downloads_ini ["http_password"] "Your password when using a WEB browser" string_option ""
  
let max_hard_upload_rate = define_option downloads_ini ["max_hard_upload_rate"] 
  "The maximal upload rate you can tolerate on your link in kBytes/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
control and data messages." int_option 0
  
let max_hard_download_rate = define_option downloads_ini ["max_hard_download_rate"] 
  "The maximal download rate you can tolerate on your link in kBytes/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
control and data messages." int_option 0
  
let password = define_option downloads_ini ["password"] 
  "The password to access your client from the GUI (setting it disables
  the command-line client)" string_option ""

  
let allowed_ips = define_option downloads_ini ["allowed_ips"]
  "list of IP address allowed to connect to the core via telnet/GUI/WEB
list separated by spaces, wildcard=255 ie: use 192.168.0.255 for 192.168.0.* "
    (list_option Ip.option) [Ip.of_string "127.0.0.1"]
  
let enable_server = define_option downloads_ini
    ["enable_server"]
  "Set to true if you also want mldonkey to run as a server (experimental)"
    bool_option false

let enable_overnet = define_option downloads_ini
    ["enable_overnet"]
  "Set to true if you also want mldonkey to run as an overnet client"
    bool_option true

let enable_donkey = define_option downloads_ini
    ["enable_donkey"]
  "Set to true if you also want mldonkey to run as a donkey client"
    bool_option true
  
  
let enable_opennap = define_option downloads_ini
    ["enable_opennap"]
  "Set to true if you also want mldonkey to run as a napster client (experimental)"
    bool_option false
  
  
let enable_soulseek = define_option downloads_ini
    ["enable_soulseek"]
  "Set to true if you also want mldonkey to run as a soulseek client (experimental)"
    bool_option false
    

  
let enable_audiogalaxy = define_option downloads_ini
    ["enable_audiogalaxy"]
  "Set to true if you also want mldonkey to run as an audiogalaxy satellite (experimental)"
    bool_option false
  
let enable_limewire = define_option downloads_ini
    ["enable_limewire"]
  "Set to true if you also want mldonkey to run as a limewire sub node (experimental)"
    bool_option false
  
let enable_directconnect = define_option downloads_ini
    ["enable_directconnect"]
  "Set to true if you also want mldonkey to run as a direct-connect node (experimental)"
    bool_option false

let enable_openft = define_option downloads_ini
    ["enable_openft"]
  "Set to true if you also want mldonkey to run as a OpenFT sub node (experimental)"
    bool_option false

let _ =
  let bin_name = Filename.basename Sys.argv.(0) in
  List.iter (fun (prefix, option) ->
      if String2.starts_with bin_name prefix then
        option =:= true
  )
  [ "mldc", enable_directconnect;
    "mlnap", enable_opennap; 
    "mldonkey", enable_donkey;
    "mlslsk", enable_soulseek;
    "mlgnut", enable_limewire;]
  
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
    ("server.met", 1, "http://savannah.nongnu.org/download/mldonkey/network/servers.met");        
    ("ocl",1, "http://savannah.nongnu.org/download/mldonkey/network/peers.ocl");
    ("ocl",1, "http://members.lycos.co.uk/appbyhp2/FlockHelpApp/contact-files/contact.ocl" );

  ]

(********************

    EXPERT OPTIONS

*********************)
  
      
let shared_extensions = define_option expert_ini ["shared_extensions"]
  
  "A list of extensions of files that should be shared. Files with extensions
    not in the list will not be shared (except if the list is empty :)"
    (list_option string_option) []

  
let client_timeout = define_option expert_ini ["client_timeout"] 
  "Timeout on client connections when not queued" float_option 120.

let interface_buffer = define_option expert_ini ["interface_buffer"] 
  "The size of the buffer between the client and its GUI. Can be useful
to increase when the connection between them has a small bandwith" int_option
  1000000

let max_name_len = define_option expert_ini ["max_name_len"]
    "The size long names will be shorten to in the interface"
  int_option 50
  
let previewer = define_option expert_ini ["previewer"]
  "Name of program used for preview (first arg is local filename, second arg
    is name of file as searched on eDonkey" string_option
  "mldonkey_previewer"

let update_gui_delay = define_option expert_ini ["update_gui_delay"] 
  "Delay between updates to the GUI" float_option 1.
 
let temp_directory = define_option expert_ini ["temp_directory" ] 
    "The directory where temporary files should be put" 
    string_option (Filename.concat file_basedir "temp")
  
let incoming_directory = 
  define_option expert_ini ["incoming_directory" ] 
    "The directory where downloaded files should be moved after commit" 
    string_option (Filename.concat file_basedir "incoming")

let http_realm = 
  define_option expert_ini ["http_realm"] "The realm shown when connecting with a WEB browser" string_option "MLdonkey"

let initialized = define_option expert_ini ["initialized"] 
  "(not used)"
    bool_option false

let set_client_ip = define_option expert_ini ["client_ip"] 
    "The last IP address used for this client" Ip.option  
    (Ip.my ())
  
let force_client_ip = define_option expert_ini ["force_client_ip"] 
  "Use the IP specified by 'client_ip' instead of trying to determine it
    ourself. Don't set this option to true if you have dynamic IP."
    bool_option false
  
let use_html_frames = define_option expert_ini ["use_html_frames"] 
    "This option controls whether the WEB interface should use frames or not" bool_option true

let commands_frame_height = define_option expert_ini ["commands_frame_height"] "The height of the command frame in pixel (depends on your screen and browser sizes)" int_option 140

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

  
let strict_bandwidth = define_option expert_ini ["strict_bandwidth"]
    "Should the bandwidth be controled more strictly ? (count IP packets)"
    bool_option true
    
let ask_for_gui = define_option expert_ini ["ask_for_gui"]
    "Ask for GUI start"    bool_option true
    
let start_gui = define_option expert_ini ["start_gui"]
    "Automatically Start the GUI" bool_option false

let bin_dir = Filename.dirname Sys.argv.(0)
  
let mldonkey_bin = define_option expert_ini ["mldonkey_bin"]
    "Directory where mldonkey binaries are installed"
    string_option bin_dir

let mldonkey_gui = define_option expert_ini ["mldonkey_gui"]
    "Name of GUI to start" string_option 
    (Filename.concat bin_dir "mldonkey_gui")


let filter_search = define_option expert_ini ["filter_search"]
  "Should mldonkey filter results of searches
  (results are displayed only if they exactly match the request,
    and filtering is done every 'filter_search_delay'." 
    bool_option false

let filter_search_delay = define_option expert_ini ["filter_search_delay"]
  "Delay before two filtering on results (results
    are not displayed until filtered). Min is 1 second." float_option 20.
  
let tcpip_packet_size = define_option expert_ini ["tcpip_packet_size"]
  "The size of the header of a TCP/IP packet on your connection (ppp adds
    14 bytes sometimes, so modify to take that into account)"
    int_option 40
  
let _ =
  option_hook tcpip_packet_size (fun _ ->
      TcpBufferedSocket.ip_packet_size := !!tcpip_packet_size
  )

let mtu_packet_size = define_option expert_ini ["mtu_packet_size"]
  "The size of the MTU of a TCP/IP packet on your connection"
    int_option 1500

let _ =
  option_hook mtu_packet_size (fun _ ->
      TcpBufferedSocket.mtu_packet_size := !!mtu_packet_size
  )

let network_update_url = define_option expert_ini ["network_update_url"]
    "URL where mldonkey can download update information on the network"
    string_option "http://savannah.nongnu.org/download/mldonkey/network/"
  
let motd_html = define_option expert_ini ["motd_html"]
    "Message printed at startup (automatically downloaded from the previous
    URL directory" string_option "Welcome to MLdonkey"
  
let run_as_user = define_option expert_ini ["run_as_user"]
  "The login of the user you want mldonkey to run as, after the ports
  have been bound (can be use not to run with root priviledges when 
a port < 1024 is needed)" string_option ""

let run_as_useruid = define_option expert_ini ["run_as_useruid"]
  "The UID of the user (0=disabled) you want mldonkey to run as, after the ports
  have been bound (can be use not to run with root priviledges when 
a port < 1024 is needed)" int_option 0

let addr_option  =  define_option_class "Addr" 
    (fun value ->
      let s = value_to_string value in
      let addr, port = String2.cut_at s ':' in
      addr, int_of_string port)
      (fun (addr, port) -> string_to_value (Printf.sprintf "%s:%d" addr port))

let compaction_delay = define_option expert_ini ["compaction_delay"]
    "Force compaction every <n> hours (in [1..24])"
    int_option 2
  
let vd_reload_delay = define_option expert_ini ["vd_reload_delay"]
    "The delay between reloads of the vd output in the WEB interface"
    int_option 120
    
let http_bind_addr = define_option expert_ini ["http_bind_addr"]
    "The IP address used to bind the http server"
    Ip.option (Ip.any)
  
let gui_bind_addr = define_option expert_ini ["gui_bind_addr"]
    "The IP address used to bind the gui server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
  
let telnet_bind_addr = define_option expert_ini ["telnet_bind_addr"]
    "The IP address used to bind the telnet server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
  
(** {2 Chat} *)

let chat_app_port = 
  define_option expert_ini ["chat_app_port"] 
    "port of the external chat application" 
    int_option 5036

let chat_app_host = 
  define_option expert_ini ["chat_app_host"] 
    "hostname of the external chat application" 
    string_option "localhost"

let chat_port = 
  define_option expert_ini ["chat_port"] 
    "port used by the external chat application to use the core as a proxy"
    int_option 4002

let chat_bind_addr = define_option expert_ini ["chat_bind_addr"]
    "The IP address used to bind the chat server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let chat_console_id =
  define_option expert_ini ["chat_console_id"] 
    "the id to use for communicating with the core console through chat interface" 
    string_option "donkey console"

let chat_warning_for_downloaded = define_option expert_ini
    ["chat_warning_for_downloaded"]
    "use the chat to indicate when a file has been downloaded"
    bool_option true

let max_opened_connections = define_option expert_ini
    ["max_opened_connections"] "Maximal number of opened connections" 
  int_option (min MlUnix.max_sockets 200)

  (*
let web_header = define_option expert_ini
    ["web_header"] "The header displayed in the WEB interface"
    string_option
  "
  <h2>Connected to <a href=http://www.freesoftware.fsf.org/mldonkey/> MLdonkey </a> 
WEB server</h2>
  <br>
</table>
<table width=100% border=0>
<tr>
  <td><a href=/submit?q=vm $O> View Connected Servers </a></td>
  <td><a href=/submit?q=vma $O> View All Servers </a></td>
  <td><a href=/submit?q=c $O> Connect More Servers </a></td>
  <td><a href=/submit?q=view_custom_queries $O> Custom Searches </a></td>
  <td><a href=/submit?q=xs $O> Extended Search </a></td>
  <td><a href=/submit?q=upstats $O> Upload Statistics </a></td>
  </tr>
<tr>
<td><a href=/submit?q=vr $O> View Results </a></td>
<td><a href=/files $O> View Downloads </a></td>
<td><a href=/submit?q=commit $S> Commit Downloads </a></td>
<td><a href=/submit?q=vs $O> View Searches </a></td>
<td><a href=/submit?q=vo $O> View Options </a></td>
<td><a href=/submit?q=help $O> View Help </a></td>
  </tr>
  </table>
<br>
"
*)

let file_completed_cmd = define_option expert_ini 
    ["file_completed_cmd"] "A command that is called when a file is completely
    downloaded. Arguments are: <file_name on disk> <md4> <size>"
    string_option "" 

  

let minor_heap_size = define_option expert_ini
    ["minor_heap_size"] "Size of the minor heap in kB"
    int_option 32
      
let _ =
  option_hook minor_heap_size (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.minor_heap_size = 
        (!!minor_heap_size * 1024) };     
  )

let min_reask_delay = define_option expert_ini ["min_reask_delay"]
  "The minimal delay between two connections to the same client (in seconds)" 
    int_option 600
  
let max_reask_delay = define_option expert_ini ["max_reask_delay"]
    "The maximal delay between two connections to the same client" 
  int_option 3600

let html_mods = define_option expert_ini
    ["html_mods"] "Whether to use the modified WEB interface" bool_option false

  let html_mods_human_readable = define_option expert_ini
    ["html_mods_human_readable"] "Whether to use human readable GMk number format" bool_option true

let html_mods_use_relative_availability = define_option expert_ini
    ["html_mods_use_relative_availability"] "Whether to use relative availability in the WEB interface" bool_option true

let html_mods_vd_age = define_option expert_ini
    ["html_mods_vd_age"] "Whether to display the Age column in vd output" bool_option true

let html_mods_vd_last = define_option expert_ini
    ["html_mods_vd_last"] "Whether to display the Last column in vd output" bool_option true

let html_mods_vd_queues = define_option expert_ini
    ["html_mods_vd_queues"] "Whether to display the Queues in vd # output" bool_option true
  
let use_html_mods o =
  o.CommonTypes.conn_output = CommonTypes.HTML && !!html_mods
  
let html_checkbox_file_list = define_option expert_ini
    ["html_checkbox_file_list"] "Whether to use checkboxes in the WEB interface" bool_option true
    
let display_downloaded_results = define_option expert_ini
    ["display_downloaded_results"] "Whether to display results already downloaded" bool_option true

    
let filter_table_threshold = define_option expert_ini
    ["filter_table_threshold"] "Minimal number of results for filter form to appear"
    int_option 50

let client_buffer_size = define_option expert_ini
    ["client_buffer_size"] "Maximal size of the buffers of a client"
    int_option 500000

let save_options_delay = 
  define_option expert_ini ["save_options_delay"] 
    "The delay between two saves of the 'downloads.ini' file (default is 4 minutes)" 
  float_option 900.0

let server_connection_timeout = define_option expert_ini
  ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 5.
  
let new_print_search = define_option expert_ini
    ["new_print_search"] "Use new display of search results (with tables,
    which might be slower for your browser to display)"
    bool_option false 
let _ =
  option_hook client_buffer_size (fun _ ->
      TcpBufferedSocket.max_buffer_size := maxi 50000 !!client_buffer_size
  )

let download_sample_rate = define_option expert_ini ["download_sample_rate"]
  "The delay between one glance at a file and another" float_option 1.
 
let download_sample_size = define_option expert_ini ["download_sample_size"]
    "How many samples go into an estimate of transfer rates" int_option 10

let calendar = define_option expert_ini ["calendar"]
  "This option defines a set of date at which some commands have to be executed"
    (list_option (tuple3_option (list_option int_option,list_option int_option,
      string_option)))
  []

let ip_cache_timeout = define_option expert_ini
    ["ip_cache_timeout"]
    "The time an ip address can be kept in the cache"
    int_option 3600

  

let verbosity = define_option expert_ini ["verbosity"] 
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
              verbose_unknown_messages := true;
              verbose_overnet := true;
              verbose_share := true;
              verbose_md4 := true;
              
          | _ -> ()
              
      ) (String2.split_simplify !!verbosity ' ')
  )
  
let compaction_overhead = define_option expert_ini 
    ["compaction_overhead"] 
    "The percentage of free memory before a compaction is triggered"
    int_option 25

let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )
    
let max_displayed_results = define_option expert_ini
    ["max_displayed_results"]
    "Maximal number of results displayed for a search"
    int_option 1000
  
let _ =
  option_hook filter_search_delay (fun _ ->
      if !!filter_search_delay < 1. then filter_search_delay =:= 1.)

let options_version = define_option expert_ini ["options_version"]
    "(internal option)"
    int_option 0  
  
let gui_options_panel = define_option expert_ini ["gui_options_panel"]
  "Which options are configurable in the GUI option panel, and in which
    sections. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
    (list_option (tuple4_option (string_option, string_option, string_option, string_option)))
  [
    "Identification", "Your client name", (shortname client_name), "T";
    "Identification", "Password", (shortname password), "T";
    "Identification", "HTTP login", (shortname http_login), "T";
    "Identification", "HTTP password", (shortname http_password), "T";
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
  
let client_ip sock =
  if !!force_client_ip then !!set_client_ip else
  match sock with
    None -> !!set_client_ip
  | Some sock ->
      let ip = TcpBufferedSocket.my_ip sock in
      if ip <> Ip.localhost && !!set_client_ip <> ip then  set_client_ip =:= ip;
      ip

      
