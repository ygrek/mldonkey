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

let donkey_ini = downloads_ini
  
  
let protocol_version = 
  define_option donkey_ini ["protocol_version"] 
    "The version of the protocol that should be sent to servers (need restart) "
    int_option 59
  
let queued_timeout = 
  define_option donkey_ini ["queued_timeout"] 
    "How long should we wait in the queue of another client"
    float_option 1800. 
      
let upload_timeout = 
  define_option donkey_ini ["upload_timeout"] 
    "How long can a silent client stay in the upload queue"
    float_option 1800. 

let connected_server_timeout = 
  define_option donkey_ini ["connected_server_timeout"]
    "How long can a silent server stay connected"
    float_option 1800.
  
let upload_power = define_option donkey_ini ["upload_power"]
  "The weight of upload on a donkey connection compared to upload on other
  peer-to-peer networks. Setting it to 5 for example means that a donkey 
  connection will be allowed to send 5 times more information per second than
  an Open Napster connection. This is done to favorise donkey connections
  over other networks, where upload is less efficient, without preventing
  upload from these networks." int_option 5

let propagate_servers = define_option donkey_ini ["propagate_servers"]
  "Send an UDP packet to a central servers with the list of servers you
  are currently connected to, for the central server to be able to
    generate accurate server lists." bool_option true

let files_queries_per_minute = define_option donkey_ini
    ["files_queries_per_minute"] 
  "Maximal number of localisation queries that can be sent to
  one server per minute. Some servers kick clients when this
  value is greater than 1" int_option 1

let files_queries_initial_delay = define_option donkey_ini
    ["files_queries_initial_delay"] 
  "Initial delay after sending the first localisation queries to
  a server, before sending other localisation queries." int_option 20

let commit_in_subdir = define_option donkey_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option ""
  
let network_options_prefix = define_option donkey_ini
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option ""
  
  
let _ = 
(* Clients should never send more than 5 localisations queries
per minute. Greater values are bad for server ressources.  *)
  option_hook files_queries_per_minute (fun _ ->
      if !!files_queries_per_minute > 5 then 
        files_queries_per_minute =:= 5)
  
let gui_donkey_options_panel = 
  define_option donkey_ini ["gui_donkey_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
    eDonkey section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
    (list_option (tuple3_option (string_option, string_option, string_option)))
  [
    "Maximal Source Age", shortname max_sources_age, "T";
    "Maximal Server Age", shortname max_server_age, "T";
    "Update Server List", shortname update_server_list, "B";
    "Min Users on Master Servers", shortname master_server_min_users, "T";
    "Max Upload Slots", shortname max_upload_slots, "T";
    "Max Sources Per Download", shortname max_sources_per_file, "T";
    "Protocol Version", shortname protocol_version, "T";
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
  ]
