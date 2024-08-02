(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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
open CommonOptions
open Options

let bittorrent_ini = create_options_file "bittorrent.ini"
let bittorrent_section = file_section bittorrent_ini ["Bittorrent"] "Bittorrent options"

let client_port = define_option bittorrent_section ["client_port"]
  ~restart: true
  "The port to bind the client to"
    port_option 6882

(* Generate client_uid *)
(* current_version could be "2.6.6.CVS" (why send the dots?)
   perhaps of use: Autoconf.major_version Autoconf.minor_version Autoconf.sub_version 
*)
let generate_client_uid =
  let client_uid_from_version = "-ML" ^ Autoconf.current_version ^ "-" in
  let client_uid_random_tail = Bytes.create (20 - (String.length client_uid_from_version)) in
  let sl_client_uid_random_tail = Bytes.length client_uid_random_tail in
  if sl_client_uid_random_tail > 0  then
    for i = 0 to sl_client_uid_random_tail - 1 do
      client_uid_random_tail.[i] <- char_of_int (Random.int 256)
    done;
  client_uid_from_version ^ Bytes.unsafe_to_string client_uid_random_tail

let client_uid = define_option bittorrent_section ["client_uid"]
  "The UID of this client"
    Sha1.option (Sha1.direct_of_string generate_client_uid)

(* Check if the uid is in sync with Autoconf.current_version *)
let check_client_uid () =
  let s = Sha1.direct_to_string !!client_uid in
  if Autoconf.current_version <> String.sub s 3 ((
    try
      String.index_from s 1 s.[0]
    with
      Not_found -> 3
    ) - 3) then
    client_uid =:= Sha1.direct_of_string generate_client_uid

let shortname o =
  Printf.sprintf "BT-%s" (shortname o)

let gui_bittorrent_options_panel =
  [
    "Port", shortname client_port, "T";
  ]

let ask_tracker_threshold = define_option bittorrent_section ["ask_tracker_threshold"]
  "Ask the tracker for new sources only if you have fewer than that number of sources"
    int_option 20

   (** #4541 [egs]
     *       Some tracker implements load balancing
     * and failover using redirect
    **)
let max_tracker_redirect = define_option bittorrent_section ["max_tracker_redirect"]
  "Maximum number of HTTP redirects before reaching the tracker - maximum 10, 0 to disable"
    int_option 1


let send_key = define_option bittorrent_section ["send_key"]
  "Send client key to trackers"
    bool_option true

let max_bt_uploaders = define_option bittorrent_section ["max_bt_uploaders"]
  "Maximum number of uploaders for bittorrent, can not be higher than max_upload_slots"
    int_option 3

let max_uploaders_per_torrent = define_option bittorrent_section ["max_uploaders_per_torrent"]
  "Maximum number of uploaders for one torrent, can not be higher than max_bt_uploaders"
    int_option 3

(* numwant: Optional. Number of peers that the client would like to receive from the tracker.
This value is permitted to be zero. If omitted, typically defaults to 50 peers.   *)

let numwant = define_option bittorrent_section ["numwant"]
  "Number of peers to request from tracker (Negative # = let tracker decide)"
    int_option (-1)

let import_new_torrents_interval = define_option bittorrent_section ["import_new_torrents_interval"]
  ~restart: true
  "Interval in seconds 'torrents/incoming' is scanned for new torrent files to be downloaded, 0 to deactivate"
    float_option 60.

let tracker_retries = define_option bittorrent_section ["tracker_retries"]
  "Number of retries before a tracker is disabled, use 0 to not disable trackers"
    int_option 10

let check_bt_uploaders () =
  if !!max_bt_uploaders > !!max_upload_slots then
    max_bt_uploaders =:= !!max_upload_slots;
  if !!max_uploaders_per_torrent > !!max_bt_uploaders then
    max_uploaders_per_torrent =:= !!max_bt_uploaders

let _ =
  begin
    option_hook max_uploaders_per_torrent
      (fun _ ->
        let v = int_of_string (strings_of_option max_uploaders_per_torrent).option_default in
        if !!max_uploaders_per_torrent < 1 then max_uploaders_per_torrent =:= v;
        check_bt_uploaders ()
        );
    option_hook max_bt_uploaders
      (fun _ ->
        if !!max_bt_uploaders < 1 then
            max_bt_uploaders =:= int_of_string (strings_of_option max_bt_uploaders).option_default;
        check_bt_uploaders ()
        );
    option_hook max_tracker_redirect   (* #4541 [egs] **)
      (fun _ ->
        if !!max_tracker_redirect < 0 then max_tracker_redirect =:= 0
        else if !!max_tracker_redirect > 10 then max_tracker_redirect =:= 10) ;
  end

let min_tracker_reask_interval = define_option bittorrent_section ["min_tracker_reask_interval"]
  "Minimum time in seconds to wait between asking the tracker for sources"
    int_option 300

let client_timeout = define_option bittorrent_section ["client_timeout"]
  "Timeout on client connections"
    float_option 90.

let user_agent = define_option bittorrent_section ["user_agent"]
  "User agent string (default = \"default\")"
    string_option "default"

let options_version = define_option bittorrent_section ["options_version"]
  ~internal: true
  "(internal option)"
    int_option 0

let old_files = define_option bittorrent_section ["old_files"]
  ""
    (list_option (tuple2_option (string_option, int64_option))) []

let tracked_files_list = define_option bittorrent_section ["tracked_files"]
  ""
    (list_option Sha1.option) []

let get_user_agent () = 
  if !!user_agent = "default" then
    CommonOptions.get_user_agent ()
  else !!user_agent

let dht_port = define_option bittorrent_section ["dht_port"]
  "DHT port (UDP, set 0 to disable)"
  port_option (2000 + Random.int 20000)

let use_trackers = define_option bittorrent_section ["use_trackers"]
  "Send announces to trackers"
  bool_option true

let dht_bootstrap_nodes = define_option bittorrent_section ["dht_bootstrap_nodes"]
  "Addresses of nodes used to bootstrap DHT network. Tried in order until enough nodes are found."
  (list_option addr_option)
  [
    "service.ygrek.org.ua",6881;
    "router.bittorrent.com", 8991;
  ]

