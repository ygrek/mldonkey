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
    Filename.concat file_basedir "gnutella2.ini")
  
let g2_max_ultrapeers = define_option gnutella_ini
    [ "max_ultrapeers"]
  "Maximal number of ultrapeers connected"
    int_option 5

let _ = 
  option_hook g2_max_ultrapeers 
    (fun _ ->
      if !!g2_max_ultrapeers > 10 then g2_max_ultrapeers =:= 10)
  
let client_port = define_option gnutella_ini ["client_port"]
    "The port to bind the client to"
    int_option 6346
  
let g2_redirectors = define_option gnutella_ini
    ["redirectors"]
    "The URLs where hosts on gnutella2 can be downloaded"
    (list_option string_option)
  [
    "http://cache.shareaza.com/cache.aspx";
    "http://g2cache.theg2.net/gwcache/lynnx.asp";
    "http://user1.7host.com/dgwc2/lynnx.asp";
    "http://www20.brinkster.com/dgc2/lynnx.asp";
    "http://ptzldd1.ath.cx/perlgcache.cgi";
    "http://www25.brinkster.com/dcache/dcache.asp";
    "http://gwebcache4.jonatkins.com/cgi-bin/perlgcache.cgi";
    "http://gwebcache2.jonatkins.com/cgi-bin/gwebcache.cgi";
    "http://gwebcache5.jonatkins.com/cgi-bin/perlgcache.cgi";
  ]
  
  
let commit_in_subdir = define_option gnutella_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "Gnutella2"
  
  
let deflate_connections = define_option gnutella_ini ["deflate_connections"]
    "(only for development tests)"  
    bool_option true

let user_agent = Printf.sprintf "MLDonkey %s" Autoconf.current_version

let max_known_ultrapeers = define_option gnutella_ini ["max_known_ultrapeers"]
  "Maximal number of ultrapeers remembered"
    int_option 100

let max_known_peers = define_option gnutella_ini ["max_known_peers"]
  "Maximal number of peers remembered"
  int_option 20

    
let server_connection_timeout = 
  define_option gnutella_ini ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 10.

let client_uid = define_option gnutella_ini ["client_uid"]
    "The UID of this client" Md4.option (Md4.random ())
  
  let _	 =
  option_hook client_uid (fun _ ->
     let s = Md4.direct_to_string !!client_uid in
     s.[8] <- '\255';
     s.[15] <- '\000';
  )
  
  (*
let verbose_clients = 
  define_option gnutella_ini ["verbose_clients"] 
  "level of verbosity when communicating with clients" 
    int_option 0
    
let verbose_servers = 
  define_option gnutella_ini ["verbose_servers"] 
    "level of verbosity when communicating with servers" int_option 0
    *)

let network_options_prefix = define_option gnutella_ini
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "GNUT-"
  
let max_available_slots = define_option gnutella_ini
    ["max_available_slots"] "The maximal number of slots for upload by Gnutella clients"
    int_option 5
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
  
let gui_gnutella_options_panel = 
  (*
  define_option gnutella_ini ["gui_gnutella_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  gnutella section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Port", shortname client_port, "T";
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
    "Max Available Slots", shortname max_available_slots, "T";    
    "Max Connected Ultrapeers", shortname g2_max_ultrapeers, "T"; 
  ]
  
