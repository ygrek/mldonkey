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

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let openft_ini = create_options_file (
    Filename.concat file_basedir "openft.ini")

let max_ultrapeers = define_option openft_ini ["max_ultrapeers"]
    "Maximal number of ultrapeers connected"
  int_option 5

let port = define_option openft_ini ["client_port"]
    "The port to bind the client to"
    int_option 1215

let http_port = define_option openft_ini ["http_port"]
    "The port to bind the client to for downloads"
    int_option 1216
  
  
let commit_in_subdir = define_option openft_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "openFT"
  
      
let network_options_prefix = define_option openft_ini
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "OpenFT-"
  
let max_known_ultrapeers = define_option openft_ini ["max_known_ultrapeers"]
  "Maximal number of ultrapeers remembered"
    int_option 100

let max_known_peers = define_option openft_ini ["max_known_peers"]
  "Maximal number of peers remembered"
  int_option 20

    
let server_connection_timeout = 
  define_option openft_ini ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 10.
    
let verbose_clients = 
  define_option openft_ini ["verbose_clients"] 
  "level of verbosity when communicating with clients" 
    int_option 0
    
let verbose_servers = 
  define_option openft_ini ["verbose_servers"] 
    "level of verbosity when communicating with servers" int_option 0
