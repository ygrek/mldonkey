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

open  CommonTypes

type link_type = 
| LinkUnknown
| Link14_4
| Link28_8
| Link33_6
| Link56_7
| Link64K_ISDN
| Link128K_ISDN
| LinkCable
| LinkDSL
| LinkT1
| LinkT3

  
type server = {
    server_server : server CommonServer.server_impl;
    server_ip : Ip.t;
    server_port : int;
    mutable server_desc : string;
    mutable server_net : string;
    mutable server_sock : tcp_connection; 
    mutable server_connection_control : connection_control;
    mutable server_size : int;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_nick_num : int;
    mutable server_last_nick : string;
    mutable server_pending_searches : (
      search_handler * (search_handler -> unit)) list;
    mutable server_searches : search_handler option;
    mutable server_users : user list;
    mutable server_browse_queue : client list;
  }
  
and search_handler =
  Recover_file of string list
| Normal_search of search
  

and user = {
    user_user : user CommonUser.user_impl;
    mutable user_addr : (Ip.t * int) option;
    user_nick : string;
    mutable user_link : link_type;
    mutable user_servers : server list;
  }

and client = {
    client_client : client CommonClient.client_impl;
    mutable client_user : user;
    client_name : string;
    mutable client_addr : (Ip.t * int) option;
    mutable client_sock : tcp_connection;
    mutable client_pos : int64;
    mutable client_error : bool;
    mutable client_files : (file * string) list;
    mutable client_file : file option;
    mutable client_connection_control : connection_control;
    mutable client_all_files : result list option;
  }
  
  (*
and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int64;
    result_info : CommonTypes.tag list;
    mutable result_sources : (user * string) list;
  }
*)
  
and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    file_name : string;
    mutable file_clients : client list;
  }

and shared_file = {
    shared_fullname : string;
    shared_codedname : string;
    shared_size : int64;
    shared_fd : Unix32.t;
    shared_format : Mp3tag.Id3v1.tag * Mp3tag.info;
  }

and shared_tree =
  { 
    shared_dirname : string;
    mutable shared_files : shared_file list;
    mutable shared_dirs : (string * shared_tree) list;
  }
  
and client_state =
  Client_not_connected
| Client_waiting_for_1
| Client_waiting_for_GET_sent
| Client_waiting_for_size

  