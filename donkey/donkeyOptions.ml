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
open CommonOptions

let donkey_ini = create_options_file (
    Filename.concat file_basedir "edonkey.ini")

let donkey_ini = downloads_ini (* temporary aliased *)
  
let initial_score = define_option downloads_ini ["initial_score"] "" int_option 5

  
  
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


  
let port = define_option downloads_ini ["port"] "The port used for connection by other donkey clients." int_option 4662

let check_client_connections_delay = 
  define_option downloads_ini ["check_client_connections_delay"] 
  "Delay used to request file sources" float_option 180.0
    
let check_connections_delay = 
  define_option downloads_ini ["check_connections_delay"] 
  "The delay between server connection rounds" float_option 5.0
  
let max_connected_servers = define_option downloads_ini
  ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 3

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

let max_server_age = define_option downloads_ini ["max_server_age"] "max number of days after which an unconnected server is removed" int_option 2

let use_file_history = define_option downloads_ini ["use_file_history"] "keep seen files in history to allow local search (can be expensive in memory)" bool_option true
  
let save_file_history = define_option downloads_ini ["save_file_history"] "save the file history in a file and load it at startup" bool_option true

  
let filters = define_option downloads_ini ["filters"] 
    "filters on replies (replies will be kept)."
    string_list_option ""

let max_allowed_connected_servers () = 
  BasicSocket.mini 5 !!max_connected_servers

  
let local_index_find_cmd = define_option downloads_ini 
    ["local_index_find_cmd"] "A command used locally to find more results
    during a search"
    string_option "" (* (cmd_basedir ^ "local_index_find")  *)

let local_index_add_cmd = define_option downloads_ini 
    ["local_index_add_cmd"] "A command used locally to add new results
    to a local index after a search"
    string_option "" (* (cmd_basedir ^ "local_index_add") *)
  

  

let compute_md4_delay = define_option downloads_ini ["compute_md4_delay"]
    "The delay between computations of the md4 of chunks"
  float_option 10.
  
let server_black_list = define_option downloads_ini 
    ["server_black_list"] "A list of server IP to remove from server list.
    Servers on this list can't be added, and will eventually be removed"
    (list_option Ip.option) []
  
let master_server_min_users = define_option downloads_ini
    ["master_server_min_users"] "The minimal number of users for a server
    to be admitted as one of the 5 master servers"
    int_option 0
  
let force_high_id = define_option downloads_ini ["force_high_id"] 
    "immediately close connection to servers that don't grant a High ID"
    bool_option false

let update_server_list = define_option downloads_ini
    ["update_server_list"] "Set this option to false if you don't want auto
    update of servers list" bool_option true

let keep_best_server = define_option downloads_ini
    ["keep_best_server"] "Set this option to false if you don't want mldonkey
    to change the master servers it is connected to" bool_option true

let max_walker_servers = define_option downloads_ini
    ["max_walker_servers"] "Number of servers that can be used to walk
between servers" int_option 1
    
let max_sources_age = define_option downloads_ini
    ["max_source_age"] "Sources that have not been connected for this number of days are removed"
    int_option 2
  
let max_clients_per_second = define_option downloads_ini
    ["max_clients_per_second"] "Maximal number of connections to sources per second"
    int_option 5
  
(*  
let use_mp3_tags = define_option downloads_ini ["use_mp3_tags"] 
  "Use mp3 tag content to save mp3 files"
    bool_option false
    *)

let max_upload_slots = define_option downloads_ini ["max_upload_slots"]
    "How many slots can be used for upload"
    int_option 10
  
let _ =  
  option_hook max_upload_slots (fun _ ->
      if !!max_upload_slots < 10 then
        max_upload_slots =:= 10)

  
let donkey_bind_addr = define_option downloads_ini ["donkey_bind_addr"]
    "The IP address used to bind the donkey client"
    Ip.option (Ip.of_inet_addr Unix.inet_addr_any)
    
let propagate_sources = define_option downloads_ini ["propagate_sources"]
    "Allow mldonkey to propagate your sources to other donkey clients"
    bool_option true
  
let max_sources_per_file = define_option downloads_ini ["max_sources_per_file"]
    "Maximal number of sources for each file"
    int_option 500
    
let good_sources_threshold = define_option downloads_ini ["good_sources_threshold"]
    "What percentage of good sources is enough"
    int_option 75
  
let min_left_sources = define_option downloads_ini
    ["min_left_sources"]
    "Minimal number of sources for a file"
    int_option 100

open Md4  
    
let mldonkey_md4 md4 =
  let md4 = Md4.direct_to_string md4 in
  md4.[5] <- Char.chr 14;
  md4.[14] <- Char.chr 111;
  Md4.direct_of_string md4

let client_md4 = define_option downloads_ini ["client_md4"]
    "The MD4 of this client" Md4.option (mldonkey_md4 (Md4.random ()))
  
let _ =
  option_hook client_md4 (fun _ -> 
      let m = mldonkey_md4 !!client_md4 in
      if m <> !!client_md4 then
        client_md4 =:= m)

    

let black_list = define_option downloads_ini ["black_list"]
  ""    bool_option true
  
let redirector = define_option downloads_ini ["redirector"]
    "IP:port of the network redirector"
    addr_option ("128.93.52.5", 4665)
  
let port_black_list = define_option downloads_ini 
    ["port_black_list"] "A list of ports that specify servers to remove
    from server list. Servers with ports on this list can't be added, and
    will eventually be removed"
    (list_option int_option) []
   
let protocol_version = 
  define_option donkey_ini ["protocol_version"] 
    "The version of the protocol that should be sent to servers (need restart) "
    int_option 61
  
let queued_timeout = 
  define_option donkey_ini ["queued_timeout"] 
    "How long should we wait in the queue of another client"
    float_option 1800. 
      
let upload_timeout = 
  define_option donkey_ini ["upload_timeout"] 
    "How long can a silent client stay in the upload queue"
    float_option 1800. 
      
let random_order_download = 
  define_option donkey_ini ["random_order_download"] 
  "Should we try to download chunks in random order (false = linearly) ?"
    bool_option false
      
let verbose_overnet = 
  define_option donkey_ini ["verbose_overnet"] 
    "Should overnet be verbose ?"
    bool_option false

let connected_server_timeout = 
  define_option donkey_ini ["connected_server_timeout"]
    "How long can a silent server stay connected"
    float_option 1800.
  
let upload_power = define_option donkey_ini ["upload_power"]
  "The weight of upload on a donkey connection compared to upload on other
  peer-to-peer networks. Setting it to 5 for example means that a donkey 
  connection will be allowed to send 5 times more information per second than
  an Open Napster connection. This is done to favorise donkey connections
  over other networks, where upload is less efficient, without preventing
  upload from these networks." int_option 5

let reward_power = define_option donkey_ini ["reward_power"]
  "Maximum additional weight granted to good sources" int_option 0

let propagate_servers = define_option donkey_ini ["propagate_servers"]
  "Send an UDP packet to a central servers with the list of servers you
  are currently connected to, for the central server to be able to
    generate accurate server lists." bool_option true

let files_queries_per_minute = define_option donkey_ini
    ["files_queries_per_minute"] 
  "Maximal number of localisation queries that can be sent to
  one server per minute. Some servers kick clients when this
  value is greater than 1" int_option 1

let files_queries_initial_delay = define_option donkey_ini
    ["files_queries_initial_delay"] 
  "Initial delay after sending the first localisation queries to
  a server, before sending other localisation queries." int_option 20

let commit_in_subdir = define_option donkey_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option ""

let min_left_servers = define_option donkey_ini ["min_left_servers"]
  "Minimal number of servers remaining after remove_old_servers"
    int_option 200
  
let servers_walking_period = define_option donkey_ini ["servers_walking_period"]
  "How often should we check all servers (minimum 4 hours, 0 to disable)"
    int_option 6
  
let _ =
  option_hook servers_walking_period (fun _ ->
    if !!servers_walking_period > 0 &&
      !!servers_walking_period < 4 then
	servers_walking_period =:= 4)

let network_options_prefix = define_option donkey_ini
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option ""
  
let keep_cancelled_in_old_files = define_option donkey_ini
    ["keep_cancelled_in_old_files"] 
    "Are the cancelled files added to the old files list to prevent re-download ?"
    bool_option false
  
let new_upload_system = define_option donkey_ini
    ["new_upload_system"] "Should we use the new experimental upload system"
    bool_option true
  
let send_warning_messages = define_option donkey_ini
    ["send_warning_messages"] "true if you want your mldonkey to lose some
upload bandwidth sending messages to clients which are banned :)"
    bool_option false
  
let ban_queue_jumpers = define_option donkey_ini
    ["ban_queue_jumpers"] "true if you want your client to ban
    clients that try queue jumping (3 reconnections faster than 9 minutes)"
    bool_option true
  
let ban_period = define_option donkey_ini
    ["ban_period"] "Set the number of hours you want client to remain banned"
    int_option 6

let max_emule_slots = define_option donkey_ini
    ["max_emule_slots"] "Part of the queue that Emule clients are limited to
    (default is 33 (%) of the slots, set to 100 (%) for no limitation)"
  int_option 33
  
let _ =
  option_hook max_emule_slots (fun _ ->
      if !!max_emule_slots < 25 then max_emule_slots =:= 25)
  
let sources_per_chunk = 
  define_option donkey_ini ["sources_per_chunk"]
    "How many sources to use to download each chunk"
    int_option 1
  
let _ = 
(* Clients should never send more than 5 localisations queries
per minute. Greater values are bad for server ressources.  *)
  option_hook files_queries_per_minute (fun _ ->
      if !!files_queries_per_minute > 5 then 
        files_queries_per_minute =:= 5)
  
let gui_donkey_options_panel = 
  define_option donkey_ini ["gui_donkey_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
    eDonkey section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
    (list_option (tuple3_option (string_option, string_option, string_option)))
  [
    "Maximal Source Age", shortname max_sources_age, "T";
    "Maximal Server Age", shortname max_server_age, "T";
    "Min Left Servers After Clean", shortname min_left_servers, "T";
    "Update Server List", shortname update_server_list, "B";
    "Min Users on Master Servers", shortname master_server_min_users, "T";
    "Servers Walking Period", shortname servers_walking_period, "T";
    "Force High ID", shortname force_high_id, "B";
    "Max Number of Connected Servers", shortname max_connected_servers, "T";
    "Max Upload Slots", shortname max_upload_slots, "T";
    "Max Sources Per Download", shortname max_sources_per_file, "T";
    "Good Sources Threshold", shortname good_sources_threshold, "T";
    "Protocol Version", shortname protocol_version, "T";
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
    "Port", shortname port, "T";
    "Download Chunks in Random order", shortname random_order_download, "B";    
    "Sources Per Chunk", shortname sources_per_chunk, "T";
    "Prevent Re-download of Cancelled Files", shortname keep_cancelled_in_old_files, "B";
  ]

