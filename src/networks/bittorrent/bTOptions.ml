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
    "The port to bind the client to"
    int_option 6882
  
  
let client_uid = define_option bittorrent_section ["client_uid"]
    "The UID of this client" Sha1.option (Sha1.random ())

  
let shortname o =
  Printf.sprintf "BT-%s" (shortname o)
  
let gui_bittorrent_options_panel = 
  (*
  define_option bittorrent_section ["gui_bittorrent_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  bittorrent section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
*)
  
  [
    "Port", shortname client_port, "T";
  ]

  
    
(*
let torrent_files =
  define_option bittorrent_section ["torrent_files"]
  "The torrent files to serve on the tracker WEB server"
    (list_option (tuple2_option (string_option, filename_option))) []
    
let tracked_files = 
  define_option bittorrent_section ["tracked_files"]
  "The files tarcked on this tracker"
    (list_option filename_option) []

let shared_files = 
  define_option bittorrent_section ["shared_files"]
  "The files shared on this torrent server (pair .torrent file, and path to shared file or directory)"
    (list_option (tuple2_option (filename_option, filename_option))) []
*)  
    
let ask_tracker_threshold = define_option bittorrent_section ["ask_tracker_threshold"]
    "Ask the tracker for new sources only if you have fewer than that number of sources"
    int_option 20

let max_uploaders_per_torrent = define_option bittorrent_section ["max_uploaders_per_torrent"]
    "Maximum number of uploaders for one torrent"
    int_option 5

let max_bt_uploaders = define_option bittorrent_section ["max_bt_uploaders"]
    "Maximum number of uploaders for bittorrent"
    int_option 5

(* numwant: Optional. Number of peers that the client would like to receive from the tracker. 
This value is permitted to be zero. If omitted, typically defaults to 50 peers.   *)

let numwant = define_option bittorrent_section ["numwant"]
    "Number of peers to request from tracker (Negative # = let tracker decide)"
    int_option (-1)

let min_tracker_reask_interval = define_option bittorrent_section ["min_tracker_reask_interval"]
    "Minimum time in seconds to wait between asking the tracker for sources"
    int_option 300

let _ =
  begin
    option_hook max_uploaders_per_torrent
      (fun _ ->
        if !!max_uploaders_per_torrent < 1 then max_uploaders_per_torrent =:= 5);
    option_hook max_bt_uploaders
      (fun _ ->
        if !!max_bt_uploaders < 0 then max_bt_uploaders =:= 5)
  end
  
  
let cookies = define_option bittorrent_section ["cookies"]
    "Cookies send with http request to get .torrent file"
    (list_option (tuple2_option (string_option, list_option (tuple2_option (string_option, string_option))))) []

let referers = define_option bittorrent_section ["referers"]
    "Referer sent with http request to get .torrent file" 
    (list_option (tuple2_option (string_option, string_option))) [(".*suprnova.*", "http://www.suprnova.org/")]
    
