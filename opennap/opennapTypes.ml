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
    mutable server_sock : TcpBufferedSocket.t option; 
    mutable server_connection_control : connection_control;
    mutable server_size : int;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_nick_num : int;
    mutable server_last_nick : string;
    mutable server_pending_searches : (
      search_handler * (search_handler -> unit)) list;
    mutable server_searches : search_handler option;
    mutable server_sources : (string * source) list;
  }

  (*
and search = {
    search_search : CommonTypes.search;
    search_num : int;
    search_query : Mftp.query; 
    mutable search_results : result Intmap.t;
    mutable search_waiting_for : int;
  }
*)
  
and search_handler =
  Recover_file of string list
| Normal_search of search
  
and file_key = {
    file_name : string;
    file_size : int32;
  }

and source = {
    source_uid : int * string;
    source_port : int option;
    source_nick : string;
    mutable source_ip : Ip.t;
    mutable source_link : link_type;
    source_server : server;
    mutable source_files : (result * string) list;
    mutable source_client : client option;
    mutable source_downloads : file list;
    mutable source_connection_control : connection_control;
  }

and client = {
    mutable source_sock : TcpBufferedSocket.t option;
    mutable source_pos : int32;
    mutable source_error : bool;
    mutable source : source option;
    mutable source_file : file option;
  }
  
and result = {
    result_result : result CommonResult.result_impl;
    result_file : file_key;
    result_info : CommonTypes.tag list;
    mutable result_sources : source list;
  }
  
and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    file_result : result;
    mutable file_downloaded : int32;
    mutable file_temp : string;
    mutable file_fd : Unix32.t;
  }
  
and client_state =
  Client_not_connected
| Client_waiting_for_1
| Client_waiting_for_GET_sent
| Client_waiting_for_size
  
open CommonNetwork
  
let network = new_network "Open Napster"
    
let (file_ops : file CommonFile.file_ops) = CommonFile.new_file_ops network
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
  
(*
  let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network
*)
  