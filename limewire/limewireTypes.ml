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

open CommonTypes

type server = {
    server_server : server CommonServer.server_impl;
    server_ip : Ip.t;
    server_port : int;
    mutable server_agent : string;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_nfiles : int;
    mutable server_nkb : int;
    
    mutable server_ping_last : Md4.t;
    mutable server_nfiles_last : int;
    mutable server_nkb_last : int;
  }
    
type local_search = {
    search_search : search;
    search_uid : Md4.t;
  }

and user = {
    user_user : user CommonUser.user_impl;
    mutable user_kind  : location_kind;
(*    mutable user_files : (result * int) list; *)
    mutable user_speed : int;
    mutable user_uid : Md4.t;
  }
  
and client = {
    client_client : client CommonClient.client_impl;
    mutable client_downloads : (file * int) list;
    mutable client_connection_control : connection_control;
    mutable client_sock : TcpBufferedSocket.t option;
    mutable client_user : user;
    mutable client_file : (file, client) CommonDownloads.download option;
    mutable client_all_files : file list option;
  }
  
and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int64;
    mutable result_sources : (user * int) list;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    file_name : string;
    mutable file_clients : client list;
  }
