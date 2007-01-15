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
open Options

let soulseek_ini = create_options_file "soulseek.ini"
let soulseek_section = file_section soulseek_ini ["Soulseek"] "Soulseek options"
  
  
let max_connected_servers = define_option soulseek_section
  ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 10

(* let ip_cache_timeout = define_option soulseek_section
    ["ip_cache_timeout"]
    "The time an ip address can be kept in the cache"
    float_option 3600. *)

let load_serverlist = define_option soulseek_section ["load_serverlist"]
  "Download a list of servers"
    bool_option true
  
(*
let main_server_name = define_option soulseek_section ["main_server_name"]
    "The main server to connect to" string_option "mail.soulseek.org"

let main_server_port = define_option soulseek_section ["main_server_port"]
  "The main server to connect to" int_option 2242
*)
  
let slsk_port = define_option soulseek_section ["client_port"]
  ~restart: true
  "The port to bind the client to"
    int_option 2234
  
let login = define_option soulseek_section ["login"]
  "Your login on SoulSeek" string_option ""

let password = define_option soulseek_section ["password"]
    "Your password on SoulSeek" string_option "mldonkey"
  
let max_known_servers = define_option soulseek_section
    ["query_serverlist_limit"] 
  "The limit on the number of servers to avoid asking for a new list" int_option 100
  

let verbose_clients = 
  define_option soulseek_section ["verbose_clients"] 
  "level of verbosity when communicating with clients" 
    int_option 0
    
let verbose_servers = 
  define_option soulseek_section ["verbose_servers"] 
    "level of verbosity when communicating with servers" int_option 0

let next_token = 
  define_option soulseek_section ["next_token"]
    "the last token used for a query is saved here" int_option 1

let options_version = define_option soulseek_section ["options_version"]
    ~internal: true
    "(internal option)"
    int_option 0

let shortname o =
  Printf.sprintf "SLSK-%s" (shortname o)
    
let gui_soulseek_options_panel = 
  (*
  define_option soulseek_section ["gui_soulseek_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
  soulseek section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Port", shortname slsk_port, "T";
(*
    "Main Server Name", shortname main_server_name, "T";
    "Main Server Port", shortname main_server_port, "T";
  *)
    "Login (nothing for global one)", shortname login, "T";
    "Password", shortname password, "T";
  ]

