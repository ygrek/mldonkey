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

open CommonTypes

type addr =
  AddrIp of Ip.t
| AddrName of string

type server = {
    server_server: server CommonServer.server_impl;
    server_room: server CommonChatRoom.room_impl;
    mutable server_name : string;
    mutable server_addr : addr;
    mutable server_ip_cached : (Ip.t * float) option;
    mutable server_info : string;
    mutable server_nusers : int;
    server_connection_control : CommonTypes.connection_control;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_port : int;
    mutable server_nick : int;
    mutable server_last_nick : string;
    mutable server_searches : search list;
    mutable server_users : user list;
    mutable server_messages : room_message list;
  }

and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int32;
    mutable result_sources : (source * string) list;
  }
  
and source = {
    source_nick : string;
    source_server : server;
  }
  
and user = {
    user_user : user CommonUser.user_impl; 
    user_nick : string;
    user_server : server;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_name : string;
    file_size : int32;  
    file_id : Md4.t;
    mutable file_downloaded : int32;
    mutable file_temp : string;
    mutable file_fd : Unix32.t;
    mutable file_clients : client list;
  }

and client = {
    client_client : client CommonClient.client_impl;
    client_name : string;
    mutable client_sock : TcpBufferedSocket.t option;
    mutable client_server : server option;
    mutable client_files : (file * string) list;
    mutable client_download : download_type;
    mutable client_pos : int32;
    mutable client_all_files : result list option;
    mutable client_receiving : int32;
  }

and download_type =
  DcIdle
| DcUpload of file
| DcDownload of file
| DcUploadList of string
| DcDownloadList of Buffer.t
  
type sizelimit = 
  AtLeast of int32
| AtMost of int32
| NoLimit

open CommonNetwork
  
let network = new_network "Direct Connect"  
      
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
let (room_ops : server CommonChatRoom.room_ops) = 
  CommonChatRoom.new_room_ops network
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network
