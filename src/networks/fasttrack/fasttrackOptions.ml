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

let fasttrack_ini = create_options_file (
    Filename.concat file_basedir "fasttrack.ini")

  
let max_ultrapeers = define_option fasttrack_ini
    [ "max_ultrapeers"]
  "Maximal number of ultrapeers connected on Fasttrack2"
    int_option 5

let _ = 
  option_hook max_ultrapeers 
    (fun _ ->
      if !!max_ultrapeers > 10 then max_ultrapeers =:= 10)
  
let client_port = define_option fasttrack_ini ["client_port"]
    "The port to bind the client to"
    int_option 6346
    
let enabled = define_option fasttrack_ini
    ["fasttrack2_enabled"]
    "Do you want to support Fasttrack2 protocol (not yet supported)"
    bool_option true
  
  
let commit_in_subdir = define_option fasttrack_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "Fasttrack"

let user_agent = Printf.sprintf "MLDonkey %s" Autoconf.current_version

let max_known_ultrapeers = define_option fasttrack_ini ["max_known_ultrapeers"]
  "Maximal number of ultrapeers remembered"
    int_option 100

let max_known_peers = define_option fasttrack_ini ["max_known_peers"]
  "Maximal number of peers remembered"
  int_option 20

    
let server_connection_timeout = 
  define_option fasttrack_ini ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 10.

let client_uid = define_option fasttrack_ini ["client_uid"]
    "The UID of this client" Md4.option (Md4.random ())
  
  let _	 =
  option_hook client_uid (fun _ ->
     let s = Md4.direct_to_string !!client_uid in
     s.[8] <- '\255';
     s.[15] <- '\000';
  )
  
  (*
let verbose_clients = 
  define_option fasttrack_ini ["verbose_clients"] 
  "level of verbosity when communicating with clients" 
    int_option 0
    
let verbose_servers = 
  define_option fasttrack_ini ["verbose_servers"] 
    "level of verbosity when communicating with servers" int_option 0
    *)

let network_options_prefix = define_option fasttrack_ini
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "FT-"
  
let max_available_slots = define_option fasttrack_ini
    ["max_available_slots"] "The maximal number of slots for upload by Fasttrack clients"
    int_option 5
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
  
let gui_fasttrack_options_panel = 
  (*
  define_option fasttrack_ini ["gui_fasttrack_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  fasttrack section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Port", shortname client_port, "T";
(*    "Max Connected Ultrapeers", shortname max_ultrapeers, "T"; 
    "Max Known Ultrapeers", shortname max_known_ultrapeers, "T";
    "Max Known Peers", shortname max_known_peers, "T";    *)
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
    "Max Available Slots", shortname max_available_slots, "T";
    "Max Connected Ultrapeers", shortname max_ultrapeers, "T"; 
  ]
  
let network_name = "KaZaA"
