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

type server_connection_state =
  Not_connected
| Connecting_to_redirector
| Connected_to_redirector
| Connecting_to_server
| Connected

  
type file_transfer_state =  
    FTS_DOWNLOAD_COMPLETE
  | FTS_INTERNAL_ERROR
  | FTS_TIMEOUT
  | FTS_SETTING_UP
  | FTS_CONNECTION_ERROR
  | FTS_CONNECTION_CLOSED
  | FTS_PORT_NOT_AVAILABLE
  | FTS_STOPPED
  | FTS_RESET_CONNECTION

  
type connect_flag = Listen | Connect
type direction_flag = SendFile | ReceiveFile

    
type file = {
    file_file : file CommonFile.file_impl;
    file_hash : Md4.t;
    file_name : string;
    file_temp : string;
    mutable file_client : client option;
  }

and client = {
    client_client : client CommonClient.client_impl;
    client_ip  : Ip.t;
    client_port : int;
    client_file : file;
    client_direction : direction_flag;
    client_id : int32;
    mutable client_sock : TcpBufferedSocket.t option;
    mutable client_server : TcpServerSocket.t option;
    mutable client_file_pos : int32;
    client_file_id : Md4.t;
  }

