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

and source = {
    mutable source_ip  : Ip.t;
    mutable source_port : int;
    mutable source_files : (result * int) list;    
    mutable source_speed : int;
    mutable source_push : bool;
    mutable source_uid : Md4.t;
    mutable source_client : client option;
    mutable source_downloads : file list;
    mutable source_connection_control : connection_control;
  }
  
and client = {
    mutable source_sock : TcpBufferedSocket.t;
    mutable source_pos : int32;
    mutable source_error : bool;
    mutable source : source option;
    mutable source_file : file option;
  }
  
and file_key = {
    file_name : string;
    file_size : int32;
  }
  
and result = {
    result_result : result CommonResult.result_impl;
    result_file : file_key;
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
  

open CommonNetwork
  
let network = new_network "LimeWire"  
let (file_ops : file CommonFile.file_ops) = CommonFile.new_file_ops network
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
(*
  let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network
*)

let (result_ops : result CommonResult.result_ops) = CommonResult.new_result_ops network
