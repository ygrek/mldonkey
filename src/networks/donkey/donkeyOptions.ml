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

let donkey_ini = create_options_file "donkey.ini"

let donkey_section = file_section donkey_ini ["Donkey"] "Donkey options"

let max_xs_packets = define_expert_option donkey_section ["max_xs_packets"]
  "Max number of UDP packets per round for eXtended Search"
    int_option 30

let donkey_port = define_option donkey_section ["port"]
  ~restart: true
  "The port used for TCP connection by other donkey clients. UDP port = port + 4."
    port_option (2000 + Random.int 20000)

let check_client_connections_delay = define_expert_option donkey_section ["check_client_connections_delay"]
  "Delay used to request file sources"
    float_option 180.0

let client_timeout = define_expert_option donkey_section ["client_timeout"]
  "Timeout on client connections when not queued"
    float_option 40.

let max_connected_servers = define_option donkey_section ["max_connected_servers"]
  "The number of servers you want to stay connected to, maximum allowable = 3"
    int_option 3

let max_allowed_connected_servers () =
  min (int_of_string (strings_of_option max_connected_servers).option_default)
      !!max_connected_servers

let reliable_sources = define_option donkey_section ["reliable_sources"]
  "Should mldonkey try to detect sources responsible for corruption and ban them, currently disabled"
    bool_option true

let ban_identity_thieves = define_option donkey_section ["ban_identity_thieves"]
  "Should mldonkey try to detect sources masquerading as others and ban them"
    bool_option true

let server_black_list = define_option donkey_section ["server_black_list"]
  "A list of server IP to remove from server list. Can contain single IPs, CIDR ranges, or begin-end ranges.
  Servers on this list can't be added, and will eventually be removed"
    CommonOptions.ip_range_list_option []

let server_black_list_set = ref Ip_set.bl_empty

let () =
  option_hook server_black_list (fun _ ->
    server_black_list_set := Ip_set.of_list !!server_black_list)

let force_high_id = define_option donkey_section ["force_high_id"]
  "immediately close connection to servers that don't grant a High ID"
    bool_option false

let force_client_high_id = define_option donkey_section ["force_client_high_id"]
  "send all clients your IP regardless of what ID was assigned by the server"
    bool_option false

let update_server_list_server = define_option donkey_section ["update_server_list_server"]
  "Set this option to false if you don't want to
  receive new servers from servers"
    bool_option false

let update_server_list_server_met = define_option donkey_section ["update_server_list_server_met"]
  "Set this option to false if you don't want to
  receive new servers from server.met"
    bool_option true

let update_server_list_client = define_option donkey_section ["update_server_list_client"]
  "Set this option to false if you don't want to
  receive new servers from clients"
    bool_option false

let keep_best_server = define_expert_option donkey_section ["keep_best_server"]
  "Set this option to false if you don't want mldonkey
  to change the master servers it is connected to"
    bool_option true

let connect_only_preferred_server = define_expert_option donkey_section ["connect_only_preferred_server"]
  "only servers which are set to 'preferred' will be connected, 
  please note that any server must be set to preferred to use this feature"
    bool_option false

let max_walker_servers = define_expert_option donkey_section ["max_walker_servers"]
  "Number of servers that can be used to walk
  between servers"
    int_option 1

let walker_server_lifetime = define_expert_option donkey_section ["walker_server_lifetime"]
  "The maximal delay a connection with a server should last when walking through the list"
    int_option 300

let log_clients_on_console = define_expert_option donkey_section ["log_clients_on_console"]
  ""
    bool_option false

let propagate_sources = define_expert_option donkey_section ["propagate_sources"]
  "Allow mldonkey to propagate your sources to other mldonkey clients.
   This function is superseded by eMule-style source exchange,
   this option is outdated"
    bool_option false

let max_sources_per_file = define_option donkey_section ["max_sources_per_file"]
  "Maximal number of sources for each file"
    int_option 5000

let keep_sources = define_expert_option donkey_section ["keep_sources"]
  "Save sources to file_sources.ini and reload them on core start."
    bool_option true

open Md4

let mldonkey_md4 md4 =
  (* not sure if mutation in-place is necessary, keeping behaviour as it was *)
  let md4 = Bytes.unsafe_of_string @@ Md4.direct_to_string md4 in
  md4.[5] <- Char.chr 14;
  md4.[14] <- Char.chr 111;
  Md4.direct_of_string @@ Bytes.unsafe_to_string md4

let client_md4 = define_option donkey_section ["client_md4"]
  "The MD4 of this client"
    Md4.option (mldonkey_md4 (Md4.random ()))

let client_private_key = define_option donkey_section ["client_private_key"]
  "The RSA private key of this client"
    string_option (if Autoconf.donkey_sui_works () then DonkeySui.SUI.create_key () else "")

let enable_sui = define_option donkey_section ["enable_sui"]
  "Enable secure user identification support"
    bool_option true

let _ =
  option_hook client_md4 (fun _ ->
      let m = mldonkey_md4 !!client_md4 in
      if m <> !!client_md4 then
        client_md4 =:= m)

let black_list = define_expert_option donkey_section ["black_list"]
  ""
    bool_option true

let port_black_list = define_expert_option donkey_section ["port_black_list"]
  "A list of ports that specify servers to remove
  from server list. Servers with ports on this list can't be added, and
  will eventually be removed"
    (list_option int_option) []

let queued_timeout = define_expert_option donkey_section ["queued_timeout"]
  "How long should we wait in the queue of another client"
    float_option 1800.

let upload_timeout = define_expert_option donkey_section ["upload_timeout"]
  "How long can a silent client stay in the upload queue"
    float_option 600.

let upload_lifetime = define_expert_option donkey_section ["upload_lifetime"]
  "How long a downloading client can stay in my upload queue (in minutes >5)"
    int_option 90

let upload_full_chunks = define_expert_option donkey_section ["upload_full_chunks"]
  "If true, each client is allowed to receive one chunk, this setting overrides upload_lifetime"
    bool_option true

let upload_complete_chunks = define_expert_option donkey_section ["upload_complete_chunks"]
  "If true, each client is allowed to complete only one chunk, independent, if it is empty or
   partial. this setting overrides upload_full_chunks and dynamic_upload_lifetime,
   but is, as a failsafe, limited by upload_lifetime (should be set reasonable high)"
    bool_option false

let dynamic_upload_lifetime = define_expert_option donkey_section ["dynamic_upload_lifetime"]
  "Each client upload lifetime depends on download-upload ratio"
    bool_option false

let dynamic_upload_threshold = define_expert_option donkey_section ["dynamic_upload_threshold"]
  "Uploaded zones (1 zone = 180 kBytes) needed to enable the dynamic upload lifetime"
    int_option 10

let connected_server_timeout = define_expert_option donkey_section ["connected_server_timeout"]
  "How long can a silent server stay connected"
    float_option 1800.

let upload_power = define_expert_option donkey_section ["upload_power"]
  "The weight of upload on a donkey connection compared to upload on other
  peer-to-peer networks. Setting it to 5 for example means that a donkey
  connection will be allowed to send 5 times more information per second than
  an Open Napster connection. This is done to favorise donkey connections
  over other networks, where upload is less efficient, without preventing
  upload from these networks."
    int_option 5

let max_server_age = define_expert_option donkey_section ["max_server_age"]
  "max number of days after which an unconnected server is removed"
    int_option 2

let remove_old_servers_delay = define_expert_option donkey_section ["remove_old_servers_delay"]
  ~restart: true
  "How often should remove old donkey servers (see max_server_age) be called
  (in seconds, 0 to disable)"
    float_option 900.

let min_left_servers = define_expert_option donkey_section ["min_left_servers"]
  "Minimal number of servers remaining after remove_old_servers"
    int_option 20

let servers_walking_period = define_expert_option donkey_section ["servers_walking_period"]
  "How often should we check all servers (minimum 4 hours, 0 to disable)"
    int_option 6

let _ =
  option_hook servers_walking_period (fun _ ->
    if !!servers_walking_period > 0 
      && !!servers_walking_period < 4 then
        servers_walking_period =:= 4
  )

let keep_cancelled_in_old_files = define_expert_option donkey_section ["keep_cancelled_in_old_files"]
  "Are the cancelled files added to the old files list to prevent re-download ?"
    bool_option false

let keep_downloaded_in_old_files = define_expert_option donkey_section ["keep_downloaded_in_old_files"]
  "Are the downloaded files added to the old files list to prevent re-download ?"
    bool_option false

let send_warning_messages = define_expert_option donkey_section ["send_warning_messages"]
  "true if you want your mldonkey to lose some
  upload bandwidth sending messages to clients which are banned :)"
    bool_option false

let ban_queue_jumpers = define_expert_option donkey_section ["ban_queue_jumpers"]
  "true if you want your client to ban
  clients that try queue jumping (3 reconnections faster than 9 minutes)"
    bool_option true

let use_server_ip = define_expert_option donkey_section ["use_server_ip"]
  "true if you want your client IP to be set from servers ID"
    bool_option true

let ban_period = define_expert_option donkey_section ["ban_period"]
  "Set the number of hours you want client to remain banned"
    int_option 1

let good_client_rank = define_expert_option donkey_section ["good_client_rank"]
  "Set the maximal rank of a client to be kept as a client"
    int_option 500

let min_users_on_server = define_option donkey_section ["min_users_on_server"]
  "disconnect if server users is smaller"
    int_option 0

let max_published_files = define_option donkey_section ["max_published_files"]
  "maximum number of files published to servers per minute, eMule default 200"
    int_option 200

let login = define_option donkey_section ["login"]
  "login of client on eDonkey network (nothing default to global one)"
    string_option ""

let overnet_options_section_name = "Overnet"

let overnet_section = file_section donkey_ini [ overnet_options_section_name ]
    "Overnet options"

let overnet_port = define_option overnet_section [overnet_options_section_name; "port"]
  ~restart: true
  "port for overnet"
    port_option (2000 + Random.int 20000)

let options_version = define_expert_option donkey_section ["options_version"]
  ~internal: true
  "(internal option)"
    int_option 4

let gui_donkey_options_panel =
  [
(*    "Maximal Source Age", shortname max_sources_age, "T"; *)
    "Maximal Server Age", shortname max_server_age, "T";
    "Min Left Servers After Clean", shortname min_left_servers, "T";
    "Update Server List Server", shortname update_server_list_server, "B";
    "Update Server List Server.met", shortname update_server_list_server_met, "B";
    "Update Server List Client", shortname update_server_list_client, "B";
    "Servers Walking Period", shortname servers_walking_period, "T";
    "Force High ID", shortname force_high_id, "B";
    "Max Number of Connected Servers", shortname max_connected_servers, "T";
    "Max Upload Slots", shortname max_upload_slots, "T";
    "Max Sources Per Download", shortname max_sources_per_file, "T";
    "Port", shortname donkey_port, "T";
    "Login", shortname login, "T";
    "Sources Per Chunk", shortname sources_per_chunk, "T";
    "Prevent Re-download of Cancelled Files", shortname keep_cancelled_in_old_files, "B";
    "Prevent Re-download of Downloaded Files", shortname keep_downloaded_in_old_files, "B";
    "Dynamic Slot Allocation", shortname dynamic_slots, "B";
  ]

let old_files = define_option donkey_section ["old_files"]
  "The files that were downloaded"
    (list_option Md4.option) []
