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

open Queues
open Md4

open GuiTypes
  
open CommonTypes
open CommonSwarming

type client = {
    client_client : client CommonClient.client_impl;
    mutable client_port : int;
    mutable client_hostname : string;
    mutable client_downloads : download list;
    mutable client_in_queues : file list;
    mutable client_connection_control : connection_control;
    mutable client_sock : tcp_connection;
    mutable client_requests : download list;
    mutable client_reconnect : bool;
    mutable client_connected_for : file option;
    mutable client_proto : tp_proto;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    mutable file_name : string;
    file_swarmer : Int64Swarmer.t;
    mutable file_clients : client list;
    mutable file_filenames : (string * ips_list) list;
    mutable file_clients_queue : client  Queues.Queue.t;
    mutable file_nconnected_clients : int;
  }

and download = {
    download_file : file;
    download_url : string;
    mutable download_chunks : (int64 * int64) list;
    mutable download_uploader : Int64Swarmer.uploader option;
    mutable download_ranges : (int64 * int64 * Int64Swarmer.range) list;
    mutable download_block : Int64Swarmer.block option;
  }

and tp_proto = {
    proto_send_range_request : (client -> (int64 * int64) ->
        TcpBufferedSocket.t -> download -> unit);    
    proto_set_sock_handler : (client -> TcpBufferedSocket.t -> unit);
    proto_string : string;
    proto_check_size : Url.url -> string -> 
       (Url.url -> string -> int64 -> unit) -> unit;
    proto_connect : TcpBufferedSocket.token ->
       client -> (TcpBufferedSocket.t -> unit) -> TcpBufferedSocket.t
  }
  