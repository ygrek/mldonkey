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

open CommonOptions
open CommonTypes
open DcTypes
open Options

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let directconnect_ini = create_options_file (
    Filename.concat file_basedir "directconnect.ini")
  
  
let max_connected_servers = define_option directconnect_ini
  ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 5

let login_messages = define_option directconnect_ini
    ["login_messages"]
    "Some more messages to send to the server when connecting"
    (list_option string_option)
  ["$Version 1,0091"; "$GetNickList"]
  

let search_timeout = define_option directconnect_ini
    ["search_timeout"]
  "The time a search is active"
    int_option 60

let load_hublist = define_option directconnect_ini ["load_hublist"]
    "Download a list of servers"
    bool_option true
  
let firewalled = define_option directconnect_ini ["firewalled"]
  "Is this client firewalled (use passive searches)"
    bool_option false
  
let shared_offset = define_option directconnect_ini
    ["shared_offset"]
    "An amount of bytes to add to the shared total (can help to connect)"
    float_option (1024. *. 1024. *. 11.)

  
let dc_port = define_option directconnect_ini ["client_port"]
  "The port to bind the client to"
    int_option 4444
  
let login = define_option directconnect_ini ["login"]
    "Your login on DC (no spaces !!!)" string_option ""
    
let max_known_servers = define_option directconnect_ini
    ["query_hublist_limit"] 
    "The limit on the number of servers to avoid asking for a new list" 
  int_option 100
    
let commit_in_subdir = define_option directconnect_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "DC"

let servers_list_url = define_option directconnect_ini ["servers_list_url"]
    "The URL from which the first server list is downloaded"
    string_option  "http://www.neo-modus.com/PublicHubList.config"
  
let client_description = define_option directconnect_ini
    ["client_description"] "The description sent in the MyINFO message"
    string_option "mldc client"
  
(* We should probably only have one common option for all networks for the
  speed. However, it might be interesting to cheat on some networks... *)
  
let client_speed = define_option directconnect_ini
    ["client_speed"] "The line speed sent in the MyINFO message"
    string_option "DSL"
  
let client_keyinfo = define_option directconnect_ini
    ["client_keyinfo"] "The key info sent in the handshake message"
    string_option "Pk=mldc"
  
let network_options_prefix = define_option directconnect_ini
    ["network_options_prefix"] 
  "The prefixes used before Direct-Connect options"
    string_option "DC-"

let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
    
let gui_dc_options_panel = 
  (*
  define_option directconnect_ini ["gui_dc_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
  dc section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Login (nothing for global one)", shortname login, "T";
    "Description", shortname client_description, "T";
    "Port", shortname dc_port, "T";
    "Hub List URL", shortname servers_list_url, "T";
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
    "Search Timeout", shortname search_timeout, "T";
    ("Max Connected Servers", shortname max_connected_servers, "T");
    ("Firewalled", shortname firewalled, "B");
    ("Client Speed", shortname client_speed, "T");
    ("Shared Offset", shortname shared_offset, "T");
  ]

let client_options_version = define_option directconnect_ini
    ["client_options_version"] "To automatically update your options"
    int_option 0
