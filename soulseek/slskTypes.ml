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
    server_server: server CommonServer.server_impl;
    mutable server_name : string;
    mutable server_addr : addr;
    mutable server_info : string;
    mutable server_nusers : int;
    server_connection_control : CommonTypes.connection_control;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_port : int;
    mutable server_nick : int;
    mutable server_last_nick : string;
    mutable server_search : search option;
    mutable server_search_timeout : float;
    mutable server_users : user list;
  }
  
and room = {
    room_room: room CommonRoom.room_impl;
    room_name : string;
    mutable room_users : user list;
    mutable room_nusers : int;
    mutable room_messages : (int * room_message) list;
  }

and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int64;
    mutable result_sources : (user * string) list;
  }
  
and user = {
    user_user : user CommonUser.user_impl; 
    user_nick : string;
    mutable user_rooms : room list;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    mutable file_clients : client list;
  }

and client = {
    client_client : client CommonClient.client_impl;
    client_name : string;
    mutable client_addr : (Ip.t * int) option;
    mutable client_peer_sock : TcpBufferedSocket.t option;
    mutable client_downloads : CommonDownloads.download list;
    mutable client_result_socks : TcpBufferedSocket.t list;
    client_connection_control : CommonTypes.connection_control;
    mutable client_files : (file * string) list; 
    mutable client_all_files : (string * result) list option;
    mutable client_receiving : int64;
    mutable client_user : user;
    mutable client_requests : (int * file) list;
  }
