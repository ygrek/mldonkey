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
open CommonOptions
open Options

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let gnutella_ini = create_options_file (
    Filename.concat file_basedir "gnutella.ini")

let gnutella_section = file_section gnutella_ini [] "Gnutella"  
let gnutella2_section = file_section gnutella_ini [] "Gnutella2"  

let g1_max_ultrapeers = define_option gnutella_section
    ["g1_max_ultrapeers"]
  "Maximal number of ultrapeers connected on Gnutella1"
    int_option 5
  
let g2_max_ultrapeers = define_option gnutella2_section
    [ "g2_max_ultrapeers"]
  "Maximal number of ultrapeers connected on Gnutella2"
    int_option 5

let _ = 
  option_hook g1_max_ultrapeers 
    (fun _ ->
      if !!g1_max_ultrapeers > 10 then g1_max_ultrapeers =:= 10);
  option_hook g2_max_ultrapeers 
    (fun _ ->
      if !!g2_max_ultrapeers > 10 then g2_max_ultrapeers =:= 10)
  
let client_port = define_option gnutella_section ["client_port"]
    "The port to bind the client to"
    int_option 6346
  
let g1_enabled = define_option gnutella_section
    ["gnutella1_enabled"]
    "Do you want to support Gnutella1 protocol"
    bool_option true
  
let g2_enabled = define_option gnutella2_section
    ["gnutella2_enabled"]
    "Do you want to support Gnutella2 protocol (not yet supported)"
    bool_option true

let gnutella1_hostfiles = define_option gnutella_section 
    ["gnutella1"; "hostfiles"]
  "A list of GWCache urls"
    (list_option string_option)
  [
    "http://gwebcache.bearshare.net/gcache.php"
  ]

let g1_urlfiles = define_option gnutella_section 
    ["gnutella1"; "urlfiles"]
  "A list of GWCache urls"
    (list_option string_option)
  [
    "http://gwebcache.bearshare.net/gcache.php"
  ]
  
let g1_redirectors = define_option gnutella_section 
    ["gnutella1"; "redirectors"]
    "The hosts to connect to to get a list of peers"
    (list_option string_option)
  [
    "public.bearshare.net"; 
    "gnotella.fileflash.com";
    "gnutella-again.hostscache.com";
    "connect1.bearshare.net"; 
    "connect1.gnutellanet.com";
    "gnutella.hostscache.com";
    "connect2.gnutellanet.com";
    "connect3.gnutellanet.com"; 
    "router4.gnutella.com";
  ]
(* (Ip.of_string "64.61.25.171")   *)

let g2_redirectors = define_option gnutella2_section
    ["gnutella2"; "redirectors"]
    "The URLs where hosts on gnutella2 can be downloaded, you can find a
list here: http://gwebcache.jonatkins.com/?urlcache"
    (list_option string_option)
  [
    "http://dlaikar.de/cgi-bin/gcache2-cgi/gcache.cgi";
    "http://fast.papajema.com/cgi-bin/gcache.cgi";
    "http://gwc.fspn.cryptnet.net/gcache.cgi";
    "http://gwc.gnewsgroups.com/cgi-bin/gcache.cgi";
    "http://gwc.mamarazzi.net/";
    "http://gwebcache.jonatkins.com/cgi-bin/gwebcache.cgi";
  ]

let gnutella2_cache = define_option gnutella_section ["gnutella2_cache"]
    "The known gnutella2 hosts"
    (list_option (tuple2_option (Ip.option, int_option)))
  []
  
  
let commit_in_subdir = define_option gnutella_section ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "Gnutella"

let user_agent = Printf.sprintf "MLDonkey %s" Autoconf.current_version

let max_known_ultrapeers = define_option gnutella_section ["max_known_ultrapeers"]
  "Maximal number of ultrapeers remembered"
    int_option 100

let max_known_peers = define_option gnutella_section ["max_known_peers"]
  "Maximal number of peers remembered"
  int_option 20

    
let server_connection_timeout = 
  define_option gnutella_section ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 10.

let client_uid = define_option gnutella_section ["client_uid"]
    "The UID of this client" Md4.option (Md4.random ())
  
  let _	 =
  option_hook client_uid (fun _ ->
     let s = Md4.direct_to_string !!client_uid in
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

let network_options_prefix = define_option gnutella_section
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "GNUT-"
  
let max_available_slots = define_option gnutella_section
    ["max_available_slots"] "The maximal number of slots for upload by Gnutella clients"
    int_option 5
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
  
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
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
    "Max Available Slots", shortname max_available_slots, "T";
    
    "Gnutella: Enabled", shortname g1_enabled, "B";
    "          Max Connected Ultrapeers", shortname g1_max_ultrapeers, "T"; 
    "Gnutella2: Enabled", shortname g2_enabled, "B";
    "           Max Connected Ultrapeers", shortname g2_max_ultrapeers, "T"; 
  ]
  
