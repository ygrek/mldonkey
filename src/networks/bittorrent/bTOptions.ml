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

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let bittorrent_ini = create_options_file (
    Filename.concat file_basedir "bittorrent.ini")
let bittorrent_section = file_section bittorrent_ini [] ""
  
let client_port = define_option bittorrent_section ["client_port"]
    "The port to bind the client to"
    int_option 6882
  
let commit_in_subdir = define_option bittorrent_section ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "BT"
  
let client_uid = define_option bittorrent_section ["client_uid"]
    "The UID of this client" Sha1.option (Sha1.random ())

let network_options_prefix = define_option bittorrent_section
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "BT-"
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
  
let gui_bittorrent_options_panel = 
  (*
  define_option bittorrent_section ["gui_bittorrent_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  bittorrent section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
*)
  
  [
    "Port", shortname client_port, "T";
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
    "Delete Original", shortname delete_original, "B";
  ]

  
    
let tracker_port = define_option bittorrent_section ["tracker_port"]
  "The port to bind the tracker to"
    int_option 6881

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
