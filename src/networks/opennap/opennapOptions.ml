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
open OpennapTypes
open CommonTypes
open Options

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let opennap_ini = create_options_file (
    Filename.concat file_basedir "opennap.ini")

let opennap_section = file_section opennap_ini ["Opennap"] "Opennap options"
  
let client_port = define_option opennap_section ["client_port"]
    "The port to bind the client to"
    int_option 6699
  
let max_connected_servers = define_option opennap_section ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 5

let _ = 
  option_hook max_connected_servers (fun _ ->
      if !!max_connected_servers > 10 then
        max_connected_servers =:= 10)
  
let client_password = define_option opennap_section ["client_password"]
    "The password used to log on the napster server"
    string_option "nopass"
  
let client_info = define_option opennap_section ["client_info"]
  "The info on this client"
    string_option "mldonkey v1.99beta3"

let use_napigator = define_option opennap_section ["use_napigator"]
    "Download a list of servers from www.napigator.com"
    bool_option true

let servers_list_url = define_option opennap_section ["servers_list_url"]
    "The URL from which servers list is downloaded"
    string_option  "http://beta34.napigator.com/servers/?sort=network&dir=asc"
  
let server_connection_timeout = 
  define_option opennap_section ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 10.
    
let max_shared_files = define_option opennap_section ["max_shared_files"]
  "The maximal number of files to share on a server"
    int_option 400
  
      
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
    
let gui_opennap_options_panel = 
  (*
  define_option opennap_section ["gui_opennap_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
    dc section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Password", shortname client_password, "T";
    "Description", shortname client_info, "T";
    "Port", shortname client_port, "T";
    "Max Connected Servers (<10)", shortname max_connected_servers, "T";
    "Use Napigator", shortname use_napigator, "B";
    "Napigator List URL", shortname servers_list_url, "T";
  ]
