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

  
let client_port = define_option opennap_ini ["client_port"]
    "The port to bind the client to"
    int_option 9999

let max_connected_servers = define_option opennap_ini ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 5

let _ = 
  option_hook max_connected_servers (fun _ ->
      if !!max_connected_servers > 10 then
        max_connected_servers =:= 10)
  
let client_password = define_option opennap_ini ["client_password"]
    "The password used to log on the napster server"
    string_option "nopass"
  
let client_port = define_option opennap_ini ["client_port"] 
  "The data port for napster uploads" int_option 6699
  
let client_info = define_option opennap_ini ["client_info"]
  "The info on this client"
    string_option "mldonkey v1.99beta3"

let use_napigator = define_option opennap_ini ["use_napigator"]
    "Download a list of servers from www.napigator.com"
    bool_option true

let servers_list_url = define_option opennap_ini ["servers_list_url"]
    "The URL from which servers list is downloaded"
    string_option  "http://www.napigator.com/servers/"
  
let server_connection_timeout = 
  define_option opennap_ini ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 10.
    
let max_shared_files = define_option opennap_ini ["max_shared_files"]
  "The maximal number of files to share on a server"
    int_option 400
  
let commit_in_subdir = define_option opennap_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "Napster"
      
let network_options_prefix = define_option opennap_ini
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "OpenNap-"
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
    
let gui_opennap_options_panel = 
  (*
  define_option opennap_ini ["gui_opennap_options_panel"]
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
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
  ]
