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

let soulseek_ini = create_options_file (
    Filename.concat file_basedir "soulseek.ini")
  
  
let max_connected_servers = define_option soulseek_ini
  ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 10

let ip_cache_timeout = define_option soulseek_ini
    ["ip_cache_timeout"]
    "The time an ip address can be kept in the cache"
    float_option 3600.

let load_serverlist = define_option soulseek_ini ["load_serverlist"]
  "Download a list of servers"
    bool_option true
  
let main_server_name = define_option soulseek_ini ["main_server_name"]
    "The main server to connect to" string_option "sk.nikita.cx"

let main_server_port = define_option soulseek_ini ["main_server_port"]
  "The main server to connect to" int_option 2242

  
let slsk_port = define_option soulseek_ini ["client_port"]
  "The port to bind the client to"
    int_option 2234
  
let login = define_option soulseek_ini ["login"]
  "Your login on SoulSeek" string_option ""

let password = define_option soulseek_ini ["password"]
    "Your password on SoulSeek" string_option "mldonkey"
  
let max_known_servers = define_option soulseek_ini
    ["query_serverlist_limit"] 
  "The limit on the number of servers to avoid asking for a new list" int_option 100
  
let commit_in_subdir = define_option soulseek_ini ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "SoulSeek"
