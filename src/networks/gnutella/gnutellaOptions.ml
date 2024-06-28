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
open Options

let gnutella_ini = create_options_file GnutellaNetwork.config_file

let gnutella_section = file_section gnutella_ini ["Gnutella"] "Gnutella options"  

let max_ultrapeers = define_option gnutella_section
    ["max_ultrapeers"]
  "Maximal number of ultrapeers connected on Gnutella"
    int_option 5
  
let _ = 
  option_hook max_ultrapeers 
    (fun _ ->
      if !!max_ultrapeers > 10 then max_ultrapeers =:= 10)
  
let client_port = define_option gnutella_section ["client_port"]
    ~restart: true
    "The port to bind the client to"
    port_option GnutellaNetwork.port
  
let gnutella_experimental = define_option gnutella_section
    ["experimental"]
    "Activate this option if you want to test experimental features."
    bool_option false

let gnutella_hostfiles = define_option gnutella_section 
    ["hostfiles"]
  "A list of GWCache urls"
    (list_option string_option)
  [
    "http://cache.kicks-ass.net:8000/";
    "http://g2cache.theg2.net/gwcache/lynnx.asp";
    "http://gwcrab.sarcastro.com:8001";
    "http://gwebcache.bearshare.net/";
    "http://gwebcache.bearshare.net/gcache.php";
    "http://pokerface.ibiza.bishopston.net:3558/";
    "http://t.az.is.teh.r0x0r.gtkg.de/";
    "http://tribaldance.ibiza.bishopston.net:3558/";
  ]

let urlfiles = define_option gnutella_section 
    ["urlfiles"]
  "A list of urlfile urls"
    (list_option string_option)
  [
(*
    "http://loot.alumnigroup.org/";
*)
  ]
  
let redirectors = define_option gnutella_section 
    ["redirectors"]
    "The hosts to connect to to get a list of peers"
    (list_option string_option)
  GnutellaNetwork.redirectors
(* (Ip.of_string "64.61.25.171")   *)
  
  
let max_known_ultrapeers = define_option gnutella_section ["max_known_ultrapeers"]
  "Maximal number of ultrapeers remembered"
    int_option 500

let max_known_peers = define_option gnutella_section ["max_known_peers"]
  "Maximal number of peers remembered"
  int_option GnutellaNetwork.max_known_peers_default
    
let server_connection_timeout = 
  define_option gnutella_section ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 30.

let client_uid = define_option gnutella_section ["client_uid"]
    "The UID of this client" Md4.option (Md4.random ())
  
  let _  =
  option_hook client_uid (fun _ ->
     let s = Bytes.of_string (Md4.direct_to_string !!client_uid) in
     s.[8] <- '\255';
     s.[15] <- '\000';
  )
  
  (*
let verbose_clients = 
  define_option gnutella_section ["verbose_clients"] 
  "level of verbosity when communicating with clients" 
    int_option 0
    
let verbose_servers = 
  define_option gnutella_section ["verbose_servers"] 
    "level of verbosity when communicating with servers" int_option 0
    *)
  
let max_available_slots = define_option gnutella_section
    ["max_available_slots"] "The maximal number of slots for upload by Gnutella clients"
    int_option 5
  
let shortname o =
  Printf.sprintf "%s%s" GnutellaNetwork.options_prefix (shortname o)

let sha1_verification_threshold = define_option gnutella_section
    ["sha1_verification_threshold"]
  "Under this threshold, files are verified using the SHA1 digest
(always available), without using the tiger-tree (experimental)."
    int64_option Int64ops.zero
  
let deflate_connections = define_option gnutella_section 
  ["deflate_connections"]
  "(only for development tests)"  
    bool_option false
  
let keep_alive = define_option gnutella_section 
    ["keep_alive"]
  "(only for development tests)"  
    bool_option true

let dont_connect = define_option gnutella_section 
    ["dont_connect"]
  "(only for development tests)"  
    bool_option false

let supernode_enabled = define_option gnutella_section 
  ["supernode_enabled"]
  "(only for development tests)"  
    bool_option false

let incoming_data_log = define_option gnutella_section
    ["incoming_data_log"]
    "(only for development tests)"
    string_option ""

let options_version = define_option gnutella_section ["options_version"]
    ~internal: true
    "(internal option)"
    int_option 0
  
let gui_gnutella_options_panel = 
  (*
  define_option gnutella_section ["gui_gnutella_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  gnutella section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Port", shortname client_port, "T";
(*    "Max Connected Ultrapeers", shortname max_ultrapeers, "T"; 
    "Max Known Ultrapeers", shortname max_known_ultrapeers, "T";
    "Max Known Peers", shortname max_known_peers, "T";    *)
    "Max Available Slots", shortname max_available_slots, "T";
    
    "          Max Connected Ultrapeers", shortname max_ultrapeers, "T"; 
  ]

let old_files =
  define_option gnutella_section ["old_files"]
    "" (list_option (tuple2_option (string_option, int64_option))) []

