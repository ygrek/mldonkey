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

open BasicSocket
open Options
open Unix

  
let file_basedir = 
  try
    Sys.getenv "MLDONKEY_DIR"
  with _ -> ""

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)
  
let downloads_ini = create_options_file (
    Filename.concat file_basedir "downloads.ini")
let shared_files_ini = create_options_file (
    Filename.concat file_basedir "shared_files.ini")
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
  
let initial_score = define_option downloads_ini ["initial_score"] "" int_option 5

  
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
  
let min_retry_delay = define_option downloads_ini ["min_retry_delay"] 
  "" float_option 150.

let client_timeout = define_option downloads_ini ["client_timeout"] 
  "Timeout on client connections when not queued" float_option 120.

let interface_buffer = define_option downloads_ini ["interface_buffer"] 
  "The size of the buffer between the client and its GUI. Can be useful
to increase when the connection between them has a small bandwith" int_option
  1000000

let max_name_len = define_option downloads_ini ["max_name_len"]
    "The size long names will be shorten to in the interface"
  int_option 50
  
let previewer = define_option downloads_ini ["previewer"]
  "Name of program used for preview (first arg is local filename, second arg
    is name of file as searched on eDonkey" string_option
  "mldonkey_previewer"
  
let gui_port = 
  define_option downloads_ini ["gui_port"] "port for user interaction" int_option 4001

let update_gui_delay = define_option downloads_ini ["update_gui_delay"] 
  "Delay between updates to the GUI" float_option 1.
 
let temp_directory = define_option downloads_ini ["temp_directory" ] 
    "The directory where temporary files should be put" 
    string_option (Filename.concat file_basedir "temp")
  
let incoming_directory = 
  define_option downloads_ini ["incoming_directory" ] 
    "The directory where downloaded files should be moved after commit" 
    string_option (Filename.concat file_basedir "incoming")

let shared_directories = 
  define_option downloads_ini ["shared_directories" ] 
    "Directories where files will be shared"
  (list_option string_option) []

let http_port = 
  define_option downloads_ini ["http_port"] "The port used to connect to your client with a WEB browser" int_option 4080
  
let http_login = 
  define_option downloads_ini ["http_login"] "Your login when using a WEB browser" string_option ""
  
let http_password = 
  define_option downloads_ini ["http_password"] "Your password when using a WEB browser" string_option ""

let initialized = define_option downloads_ini ["initialized"] 
  "(not used)"
    bool_option false
  
let max_hard_upload_rate = define_option downloads_ini ["max_hard_upload_rate"] 
  "The maximal upload rate you can tolerate on your link in kB/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
control and data messages." int_option 0
  
let max_hard_download_rate = define_option downloads_ini ["max_hard_download_rate"] 
  "The maximal download rate you can tolerate on your link in kB/s (0 = no limit)
  The limit will apply on all your connections (clients and servers) and both
control and data messages." int_option 0
  
  
let max_xs_packets = define_option downloads_ini ["max_xs_packets"] 
  "Max number of UDP packets per round for eXtended Search" int_option 30

let max_dialog_history = define_option downloads_ini ["max_dialog_history"]
    "Max number of messages of Chat remembered" int_option 30
    
  
let string_list_option = define_option_class "String"
    (fun v ->
      match v with
        List _ | SmallList _ -> ""
      | _ -> value_to_string v
  )
  string_to_value

  
let features = define_option downloads_ini ["features"] "" string_list_option ""
  
let password = define_option downloads_ini ["password"] 
  "The password to access your client from the GUI (setting it disables
  the command-line client)" string_option ""

let port = define_option downloads_ini ["port"] "The port used for connection by other donkey clients." int_option 4662

let save_options_delay = 
  define_option downloads_ini ["save_options_delay"] 
    "The delay between two saves of the 'downloads.ini' file (default is 4 minutes)" 
  float_option 240.0

let check_client_connections_delay = 
  define_option downloads_ini ["check_client_connections_delay"] 
  "Delay used to request file sources" float_option 180.0
    
let check_connections_delay = 
  define_option downloads_ini ["check_connections_delay"] 
  "The delay between server connection rounds" float_option 5.0
  
let max_connected_servers = define_option downloads_ini ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 10

let max_udp_sends = define_option downloads_ini ["max_udp_sends"] 
    "The number of UDP packets you send every check_client_connections_delay" 
  int_option 10

  (*
let _ =
  option_hook max_connected_servers (fun _ ->
      if !!max_connected_servers > 10 && !has_upload then
        max_connected_servers =:= 10)
  *)

let retry_delay = define_option downloads_ini ["retry_delay"] "" float_option 3600.
let server_connection_timeout = define_option downloads_ini ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 5.

let telnet_port = define_option downloads_ini ["telnet_port"] "port for user interaction" int_option 4000

let max_server_age = define_option downloads_ini ["max_server_age"] "max number of days after which an unconnected server is removed" int_option 7

let use_file_history = define_option downloads_ini ["use_file_history"] "keep seen files in history to allow local search (can be expensive in memory)" bool_option true
  
let save_file_history = define_option downloads_ini ["save_file_history"] "save the file history in a file and load it at startup" bool_option true

  
let filters = define_option downloads_ini ["filters"] 
    "filters on replies (replies will be kept)."
    string_list_option ""

let smtp_server = define_option downloads_ini ["smtp_server"] 
  "The mail server you want to use (must be SMTP). Use hostname or IP address"
    string_option "127.0.0.1"

let smtp_port = define_option downloads_ini ["smtp_port"] 
  "The port to use on the mail server (default 25)"
  int_option 25

let mail = define_option downloads_ini ["mail"]
  "Your e-mail if you want to receive mails when downloads are completed"
    string_option ""


let max_allowed_connected_servers () = 
  mini 5 !!max_connected_servers

let verbose = define_option downloads_ini ["verbose"] "Only for debug"
    bool_option false

  
let max_opened_connections = define_option downloads_ini
    ["max_opened_connections"] "Maximal number of opened connections" int_option (maxi (Unix32.fds_size - 100) (Unix32.fds_size / 2))

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
  ]

  (*
let web_header = define_option downloads_ini
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
<td><a href=/submit?q=vd $O> View Downloads </a></td>
<td><a href=/submit?q=commit $S> Commit Downloads </a></td>
<td><a href=/submit?q=vs $O> View Searches </a></td>
<td><a href=/submit?q=vo $O> View Options </a></td>
<td><a href=/submit?q=help $O> View Help </a></td>
  </tr>
  </table>
<br>
"
*)

let file_completed_cmd = define_option downloads_ini 
    ["file_completed_cmd"] "A command that is called when a file is completely
    downloaded. Arguments are: <file_name on disk> <md4> <size>"
    string_option "" 

let local_index_find_cmd = define_option downloads_ini 
    ["local_index_find_cmd"] "A command used locally to find more results
    during a search"
    string_option "" (* (cmd_basedir ^ "local_index_find")  *)

let local_index_add_cmd = define_option downloads_ini 
    ["local_index_add_cmd"] "A command used locally to add new results
    to a local index after a search"
    string_option "" (* (cmd_basedir ^ "local_index_add") *)
  
let compaction_overhead = define_option downloads_ini 
    ["compaction_overhead"] 
    "The percentage of free memory before a compaction is triggered"
    int_option 25

let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )
  

  
let client_ip = define_option downloads_ini ["client_ip"] 
    "The last IP address used for this client" Ip.option  
    (Ip.my ())
  
let force_client_ip = define_option downloads_ini ["force_client_ip"] 
  "Use the IP specified by 'client_ip' instead of trying to determine it
    ourself. Don't set this option to true if you have dynamic IP."
    bool_option false
  
let use_html_frames = define_option downloads_ini ["use_html_frames"] 
    "This option controls whether the WEB interface should use frames or not" bool_option true

let commands_frame_height = define_option downloads_ini ["commands_frame_height"] "The height of the command frame in pixel (depends on your screen and browser sizes)" int_option 140

let compute_md4_delay = define_option downloads_ini ["compute_md4_delay"]
    "The delay between computations of the md4 of chunks"
  float_option 10.
  
let web_common_header = define_option downloads_ini
    ["web_common_header"] "The header displayed in the WEB interface"
    string_option
  "
  <table width=100% border=0> <tr>
<td>
  <h2>Connected to <a href=http://www.freesoftware.fsf.org/mldonkey/ $P> MLdonkey </a> 
WEB server</h2>
  </td>
<td>
<form action=\"submit\" $O>
<table border=0>
<tr>
<td width=\"1%\"><input type=text name=q size=40 value=\"\"></td>
<td align=left><input type=submit value=\"Execute\"></td>
</tr>
</table>
</form>
  </td>
</table>
<table width=100% border=0>
<tr>
  <td><a href=/submit?q=vm $O> View Connected Servers </a></td>
  <td><a href=/submit?q=vma $O> View All Servers </a></td>
  <td><a href=/submit?q=c $O> Connect More Servers </a></td>
  <td><a href=/submit?q=view_custom_queries $S> Custom Searches </a></td>
  <td><a href=/submit?q=xs $O> Extended Search </a></td>
  <td><a href=/submit?q=upstats $O> Upload Statistics </a></td>
  $G
  </tr>
<tr>
<td><a href=/submit?q=vr $O> View Results </a></td>
<td><a href=/submit?q=vd $O> View Downloads </a></td>
<td><a href=/submit?q=commit $S> Commit Downloads </a></td>
<td><a href=/submit?q=vs $O> View Searches </a></td>
<td><a href=/submit?q=vo $O> View Options </a></td>
<td><a href=/submit?q=help $O> View Help </a></td>
  </tr>
  </table>
<br>
"

let server_black_list = define_option downloads_ini 
    ["server_black_list"] "A list of server IP to remove from server list.
    Servers on this list can't be added, and will eventually be removed"
    (list_option Ip.option) []
  
let master_server_min_users = define_option downloads_ini
    ["master_server_min_users"] "The minimal number of users for a server
    to be admitted as one of the 5 master servers"
    int_option 0
  
let update_server_list = define_option downloads_ini
    ["update_server_list"] "Set this option to false if you don't want auto
    update of servers list" bool_option true
  
let minor_heap_size = define_option downloads_ini
    ["minor_heap_size"] "Size of the minor heap in kB"
    int_option 32
  
let max_sources_age = define_option downloads_ini
    ["max_source_age"] "Sources that have not been connected for this number of days are removed"
    int_option 3
  
let max_clients_per_second = define_option downloads_ini
    ["max_clients_per_second"] "Maximal number of connections to sources per second"
    int_option 5
    
let _ =
  option_hook minor_heap_size (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.minor_heap_size = 
        (!!minor_heap_size * 1024) };     
  )

    
let html_checkbox_file_list = define_option downloads_ini
    ["html_checkbox_file_list"] "Whether to use checkboxes in the WEB interface" bool_option true
    
let display_downloaded_results = define_option downloads_ini
    ["display_downloaded_results"] "Whether to display results already downloaded" bool_option true

    
let filter_table_threshold = define_option downloads_ini
    ["filter_table_threshold"] "Minimal number of results for filter form to appear"
    int_option 50

let client_buffer_size = define_option downloads_ini
    ["client_buffer_size"] "Maximal size of the buffers of a client"
    int_option 500000
  
let new_print_search = define_option downloads_ini
    ["new_print_search"] "Use new display of search results (with tables,
    which might be slower for your browser to display)"
    bool_option false 
let _ =
  option_hook client_buffer_size (fun _ ->
      TcpBufferedSocket.max_buffer_size := maxi 50000 !!client_buffer_size
  )
  
(*  
let use_mp3_tags = define_option downloads_ini ["use_mp3_tags"] 
  "Use mp3 tag content to save mp3 files"
    bool_option false
    *)

let max_upload_slots = define_option downloads_ini ["max_upload_slots"]
    "How many slots can be used for upload"
    int_option 20
  
let _ =  
  option_hook max_upload_slots (fun _ ->
      if !!max_upload_slots < 10 then
        max_upload_slots =:= 10)

  
let compaction_delay = define_option downloads_ini ["compaction_delay"]
    "Force compaction every <n> hours (in [1..24])"
    int_option 2
  
let vd_reload_delay = define_option downloads_ini ["vd_reload_delay"]
    "The delay between reloads of the vd output in the WEB interface"
    int_option 120
  
let html_header = define_option downloads_ini ["html_header"]
  "The header used in the WEB interface (modify to add your CSS)"
    string_option  
  "<TITLE>\n
  MLdonkey WEB Interface\n
  </TITLE>\n
    "
  
let http_bind_addr = define_option downloads_ini ["http_bind_addr"]
    "The IP address used to bind the http server"
    Ip.option (Ip.any)
  
let gui_bind_addr = define_option downloads_ini ["gui_bind_addr"]
    "The IP address used to bind the gui server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
  
let telnet_bind_addr = define_option downloads_ini ["telnet_bind_addr"]
    "The IP address used to bind the telnet server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let donkey_bind_addr = define_option downloads_ini ["donkey_bind_addr"]
    "The IP address used to bind the donkey client"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
    
let propagate_sources = define_option downloads_ini ["propagate_sources"]
    "Allow mldonkey to propagate your sources to other donkey clients"
    bool_option true
  
let max_sources_per_file = define_option downloads_ini ["max_sources_per_file"]
    "Maximal number of sources for each file"
    int_option 1000
    
let max_displayed_results = define_option downloads_ini
    ["max_displayed_results"]
    "Maximal number of results displayed for a search"
    int_option 1000
  
let min_left_sources = define_option downloads_ini
    ["min_left_sources"]
    "Minimal number of sources for a file"
    int_option 100
  
let enable_server = define_option downloads_ini
    ["enable_server"]
  "Set to true if you also want mldonkey to run as a server (experimental)"
    bool_option false

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
  
(** {2 Chat} *)

let chat_app_port = 
  define_option downloads_ini ["chat_app_port"] 
    "port of the external chat application" 
    int_option 5036

let chat_app_host = 
  define_option downloads_ini ["chat_app_host"] 
    "hostname of the external chat application" 
    string_option "localhost"

let chat_port = 
  define_option downloads_ini ["chat_port"] 
    "port used by the external chat application to use the core as a proxy"
    int_option 4002

let chat_bind_addr = define_option downloads_ini ["chat_bind_addr"]
    "The IP address used to bind the chat server"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)

let chat_console_id =
  define_option downloads_ini ["chat_console_id"] 
    "the id to use for communicating with the core console through chat interface" 
    string_option "donkey console"

let chat_warning_for_downloaded = define_option downloads_ini
    ["chat_warning_for_downloaded"]
    "use the chat to indicate when a file has been downloaded"
    bool_option true

  
let allowed_ips = define_option downloads_ini ["allowed_ips"]
    "list of IP address allowed to control the client via telnet/GUI/WEB"
    (list_option Ip.option) [Ip.of_string "127.0.0.1"]

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

let client_md4 = define_option downloads_ini ["client_md4"]
    "The MD4 of this client" Md4.option (Md4.random ())

let ip_cache_timeout = define_option downloads_ini
    ["ip_cache_timeout"]
    "The time an ip address can be kept in the cache"
    float_option 3600.

    
let download_sample_rate = define_option downloads_ini ["download_sample_rate"]
  "The delay between one glance at a file and another" float_option 1.
 
let download_sample_size = define_option downloads_ini ["download_sample_size"]
    "How many samples go into an estimate of transfer rates" int_option 10

let calendar = define_option downloads_ini ["calendar"]
  "This option defines a set of date at which some commands have to be executed"
    (list_option (tuple3_option (list_option int_option,list_option int_option,
      string_option)))
  []
  
let shared_extensions = define_option downloads_ini ["shared_extensions"]
  
  "A list of extensions of files that should be shared. Files with extensions
    not in the list will not be shared (except if the list is empty :)"
    (list_option string_option) []

    
  
let debug_net = define_option downloads_ini ["debug_net"]
    "Set to true if you want some more information on low-level network layer"
    bool_option false
  
let ask_for_gui = define_option downloads_ini ["ask_for_gui"]
    "Ask for GUI start"    bool_option true
  
let start_gui = define_option downloads_ini ["start_gui"]
    "Automatically Start the GUI" bool_option false

let bin_dir = Filename.dirname Sys.argv.(0)
  
let mldonkey_bin = define_option downloads_ini ["mldonkey_bin"]
    "Directory where mldonkey binaries are installed"
    string_option bin_dir

let mldonkey_gui = define_option downloads_ini ["mldonkey_gui"]
    "Name of GUI to start" string_option 
    (Filename.concat bin_dir "mldonkey_gui")
  
let _ =
  option_hook debug_net (fun _ -> BasicSocket.debug := !!debug_net)


let gui_options_panel = define_option downloads_ini ["gui_options_panel"]
  "Which options are configurable in the GUI option panel, and in which
    sections. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
    (list_option (tuple4_option (string_option, string_option, string_option, string_option)))
  [
    "Identification", "Your client name", (shortname client_name), "T";
    "Identification", "Password", (shortname password), "T";
    "Identification", "HTTP login", (shortname http_login), "T";
    "Identification", "HTTP password", (shortname http_password), "T";
    "Identification", "Allowed IPs", (shortname allowed_ips), "T";
    
    "Ports", "Client IP", (shortname client_ip), "T";
    "Ports", "Force Client IP", (shortname force_client_ip), "B";
    "Ports", "HTTP Port", (shortname http_port), "T";
    "Ports", "Telnet Port", (shortname telnet_port), "T";
    "Ports", "GUI Port", (shortname gui_port), "T";
    "Ports", "HTTP Bind Address", (shortname http_bind_addr), "T";
    "Ports", "Telnet Bind Address", (shortname telnet_bind_addr), "T";
    "Ports", "GUI Bind Address", (shortname gui_bind_addr), "T";
    
    "Bandwidth", "Maximal Download Bandwidth in kB/s", shortname max_hard_download_rate, "T";
    "Bandwidth", "Maximal Upload Bandwidth in kB/s", shortname max_hard_upload_rate, "T";
    "Bandwidth", "Maximal Number of Connected Servers", shortname max_connected_servers, "T";
    "Bandwidth", "Socket Buffer Size", shortname client_buffer_size, "T";
    
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
    
    "Debug", "Verbose Mode", shortname verbose, "B";
    "Debug", "Network Verbose Mode", shortname debug_net, "B";
    
    "Startup", "Start mldonkey_gui at beginning", shortname start_gui, "B"; 
    "Startup", "Ask for mldonkey_gui start", shortname ask_for_gui, "B";
    "Startup", "Path of mldonkey binaries", shortname mldonkey_bin, "F";
    "Startup", "Name of MLdonkey GUI to start", shortname mldonkey_gui, "F";
 ]  

  
let client_ip sock =
  if !!force_client_ip then !!client_ip else
  match sock with
    None -> !!client_ip
  | Some sock ->
      let ip = TcpBufferedSocket.my_ip sock in
      if ip <> Ip.localhost then client_ip =:= ip;
      ip
