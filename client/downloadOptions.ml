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
open Mftp
open Options
open DownloadTypes
open Unix
open Gui_types

let file_basedir = ""
let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)
  
let downloads_ini = create_options_file (file_basedir ^ "downloads.ini")
let shared_files_ini = create_options_file (file_basedir ^ "shared_files.ini")
let servers_ini = create_options_file (file_basedir ^ "servers.ini")
let searches_ini = create_options_file (file_basedir ^ "searches.ini")
let files_ini = create_options_file (file_basedir ^ "files.ini")
let friends_ini = create_options_file (file_basedir ^ "friends.ini")
  
let initial_score = define_option downloads_ini ["initial_score"] "" int_option 5

  
let _ = Random.self_init ()

let random_letter () =
  char_of_int (97 + Random.int 26)
  
  
let client_name = define_option downloads_ini ["client_name"] "small name of client" string_option 
    (Printf.sprintf "mldonkey_%c%c%c%c%c" (random_letter ()) (random_letter ()) 
    (random_letter ()) (random_letter ()) (random_letter ()))

let _ =
  let in_hook = ref false in
  option_hook client_name (fun _ ->
      if not !in_hook &&
        (!!client_name = "mldonkey_rfrnx" || !!client_name = "mldonkey") then
        begin 
          in_hook := true;
          client_name =:= (Printf.sprintf "mldonkey_%c%c%c%c%c"
              (random_letter ()) (random_letter ()) 
            (random_letter ()) (random_letter ()) (random_letter ()));
        end
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
  "Delay between updates to the GUI" float_option 10.
 
let temp_directory = define_option downloads_ini ["temp_directory" ] 
    "The directory where temporary files should be put" 
  string_option "temp"
  
let incoming_directory = 
  define_option downloads_ini ["incoming_directory" ] 
    "The directory where downloaded files should be moved after commit" 
  string_option "incoming"

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

(*  
let max_upload_rate = define_option downloads_ini ["max_upload_rate"] 
  "The maximal upload rate you can tolerate (in kB/s)" int_option 3000
  
let max_download_rate = define_option downloads_ini ["max_download_rate"] 
  "The maximal download rate you can tolerate in kB/s(0 = no limit)" int_option 0
*)
  
  
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
    ["max_opened_connections"] "Maximal number of opened connections" int_option (Unix32.fds_size - 100)

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
  []

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
    "The last IP used for this client" Ip.option  
    (try
      let name = Unix.gethostname () in
      Ip.from_name name
    with _ -> Ip.localhost)
  
let force_client_ip = define_option downloads_ini ["force_client_ip"] 
  "Use the IP specified by 'client_ip' instead of trying to determine it
    ourself. Don't set this option to true if you have dynamic IP."
    bool_option false
  
let ip_verified = ref 0

    
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
    ["dont_update_server_list"] "Unset this option if you don't want auto
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
    int_option 100000
  
let new_print_search = define_option downloads_ini
    ["new_print_search"] "Use new display of search results (with tables,
    which might be slower for your browser to display)"
    bool_option false 
let _ =
  option_hook client_buffer_size (fun _ ->
      TcpBufferedSocket.max_buffer_size := maxi 50000 !!client_buffer_size
  )
  
  
let use_mp3_tags = define_option downloads_ini ["use_mp3_tags"] 
  "Use mp3 tag content to save mp3 files"
    bool_option false
  
let max_upload_slots = define_option downloads_ini ["max_upload_slots"]
    "How many slots can be used for upload"
    int_option 20
  
let _ =  
  option_hook max_upload_slots (fun _ ->
      if !!max_upload_slots < 20 then
        max_upload_slots =:= 20)

  
let compaction_delay = define_option downloads_ini ["compaction_delay"]
    "Force compaction every <n> hours (in [1..24])"
    int_option 6
  
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