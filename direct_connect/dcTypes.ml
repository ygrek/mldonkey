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
    server_room: server CommonRoom.room_impl;
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
    mutable server_search_timeout : int;
    mutable server_users : user list;
    mutable server_messages : (int * room_message) list;
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
    mutable user_servers : server list;
    mutable user_link : string;
    mutable user_data : float;
    mutable user_admin : bool;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_name : string;
    file_id : Md4.t;
    mutable file_clients : client list;
  }

and client = {
    client_client : client CommonClient.client_impl;
    client_name : string;
    mutable client_addr : (Ip.t * int) option;
    mutable client_sock : TcpBufferedSocket.t option;
    mutable client_files : (file * string) list;
    mutable client_download : download_type;
    mutable client_pos : int64;
    mutable client_all_files : (string * result) list option;
    mutable client_receiving : int64;
    mutable client_user : user;
    mutable client_connection_control : connection_control;
  }

and shared_file = {
    shared_fullname : string;
    shared_codedname : string;
    shared_size : int64;
    shared_fd : Unix32.t;
  }

and shared_tree =
  { 
    shared_dirname : string;
    mutable shared_files : shared_file list;
    mutable shared_dirs : (string * shared_tree) list;
  }
  
and download_type =
  DcIdle
| DcUpload of shared_file
| DcDownload of file
| DcUploadList of string
| DcDownloadList of Buffer.t
  
type sizelimit = 
  AtLeast of int64
| AtMost of int64
| NoLimit
