(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open Unix
open TcpBufferedSocket
open Mftp
open Options
open Mftp_comm
open ServerTypes
  
let server_ini = create_options_file "server.ini"
  
let server_port = define_option server_ini ["port"] "port to bind on"
  int_option 4661
let server_name = define_option server_ini ["name"] "small name of server"
    string_option  "mldonkey"
let server_desc = define_option server_ini ["description"] 
  "small description of server" string_option "mldonkey general server"
let welcome_messages = define_option server_ini ["welcome"]
  "list of welcome messages"
  (list_option string_option) [ "Welcome to my mldonkey server" ]
let seed_ip = define_option server_ini ["seedIP"] "IP of server to register"
    (option_option Ip.option) None
let seed_port = define_option server_ini ["seedPort"]
  "Port of server to register" int_option 4661
let max_clients = define_option server_ini ["maxClients"] 
  "Maximal number of clients connected" int_option 200

  
let server_ip = define_option server_ini ["server_ip"] 
    "The last IP used for this server" Ip.option  
    (try
      let name = Unix.gethostname () in
      Ip.from_name name
    with _ -> Ip.localhost)
