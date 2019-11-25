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

  
let directconnect_ini = create_options_file "directconnect.ini"
let directconnect_section = file_section directconnect_ini ["DirectConnect"] "DirectConnect options"
  
let dc_port = define_option directconnect_section ["client_port"]
    "The port to bind the client to"
    port_option 4444
  
let dc_open_slots = define_option directconnect_section ["dc_open_slots"]
    "How many slots are open to other clients"
    int_option 2

let login = define_option directconnect_section ["login"]
    "Your login on DC (no spaces !!!)" string_option ""

let hubs_passwords = define_option directconnect_section ["hubs_passwords"]
    "Define here a list of address/passwords/nick triples for hubs in form:
    [ (\"hubserver.ip.com\", \"nick1\", \"pass1\");
     (\"somehub.somewhere.org\", \"nick2\", \"pass2\");
     (\"11.22.333.444\", \"nick3\", \"pass3\");
    ]"
    (list_option (tuple3_option (string_option, string_option, string_option)))
    []

let default_encoding = define_option directconnect_section ["default_encoding"]
    "Default encoding to use for communications with hubs (CP1251,UTF-8,ISO-8859-1,etc)"
    string_option "CP1252"

let search_timeout = define_option directconnect_section
    ["search_timeout"]
    "The time a search is active"
    int_option 60
  
let file_search_time = define_option directconnect_section
    ["file_search_time"]
    "Minimum time between automated file tth searches in minutes"
    int_option 60
  
let client_timeout = define_option directconnect_section
    ["client_timeout"]
    "In initialization, the time in seconds we wait connection responses from client"
    int_option 90

let client_read_timeout = define_option directconnect_section
    ["client_read_timeout"]
    "The maximum time in seconds we wait data from client in downloading"
    int_option 60
    
let client_write_timeout = define_option directconnect_section
    ["client_write_timeout"]
    "The maximum time in seconds we wait to continue pushing data to client in uploading"
    int_option 60
    
let wait_for_next_upload = define_option directconnect_section
    ["wait_for_next_upload"]
    "How many seconds we wait a client to continue slot before closing and freeing the slot"
    int_option 30

let firewalled = define_option directconnect_section ["firewalled"]
    "Is this client firewalled (use passive searches)"
    bool_option false
  
let autosearch_by_tth = define_option directconnect_section ["autosearch_by_tth"]
    "Automatically find alternative sources, if file is unavailable. Also add sources
     from searches automatically"
    bool_option true
  
let max_sources_file = define_option directconnect_section ["max_sources_file"]
    "Maximal number of sources to single file"
    int_option 5
  
let client_speed = define_option directconnect_section
    ["client_speed"]
    "The line speed sent in the MyINFO message
     eg. Modem, Cable, DSL, Satellite, LAN(T1), LAN(T3)"
    string_option "DSL"
  
let options_version = define_option directconnect_section ["options_version"]
    ~internal: true
    "(internal option)"
    int_option 0

let shortname o =
  Printf.sprintf "DC-%s" (shortname o)
    
let gui_dc_options_panel = 
  (*
  define_option directconnect_section ["gui_dc_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
  dc section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Login (nothing for global one)", shortname login, "T";
    "Port", shortname dc_port, "T";
    "Search Timeout", shortname search_timeout, "T";
    "Firewalled", shortname firewalled, "B";
    "Connection Type", shortname client_speed, "T";
  ]
