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

open CommonTypes
open CommonDownloads


type query_key =
  NoUdpSupport
| GuessSupport
| UdpSupport of Md4.t
| UdpQueryKey of int64

  (*
type host = {
    host_num : int;
    mutable host_server : server option;
    host_ip : Ip.t;
    host_port : int;
    mutable host_age : int;
    mutable host_udp_request : int;
    mutable host_tcp_request : int;
    mutable host_connected : int;
(* 0 -> gnutella1 or gnutella2, 1 -> gnutella1, 2 -> gnutella2 *)
    mutable host_kind : int;
    mutable host_ultrapeer : bool;
    
    mutable host_queues : host Queue.t list;
  }
    *)
    
type request = 
| Tcp_Connect
| Udp_Connect

type host = (server, request, Ip.t) CommonHosts.host
  
and server = {
    server_server : server CommonServer.server_impl;
    mutable server_agent : string;
    mutable server_sock : tcp_connection;
    mutable server_nfiles : int64;
    mutable server_nkb : int;

    mutable server_nusers : int64;
    
    mutable server_need_qrt : bool;
    mutable server_ping_last : Md4.t;
    mutable server_nfiles_last : int64;
    mutable server_nkb_last : int;
    mutable server_vendor : string;
    mutable server_connected : int64;
    
(*    mutable server_gnutella2 : bool; *)
    mutable server_host : host;
    mutable server_query_key : query_key;
  }

type search_type =
  UserSearch of search * string * string
| FileUidSearch of file * Uid.t
| FileWordSearch of file * string
  
and local_search = {
    search_search : search_type;
    search_uid : Md4.t;
    mutable search_hosts : Intset.t;
  }

and user = {
    user_user : user CommonUser.user_impl;
    mutable user_kind  : location_kind;
(*    mutable user_files : (result * int) list; *)
    mutable user_speed : int;
    mutable user_uid : Md4.t;
    mutable user_vendor : string;
    mutable user_gnutella2 : bool;
    mutable user_nick : string;
  }

(* In a client structure, we only have on socket, whereas in gnutella,
client connections are directed, ie we could need two sockets if we
want both upload and download from the same client. We could maybe use
two different tables to look up for clients ? *)
and client = {
    client_client : client CommonClient.client_impl;
    mutable client_downloads : download list;
    mutable client_connection_control : connection_control;
    mutable client_sock : tcp_connection;
    mutable client_user : user;
    mutable client_all_files : file list option;
    mutable client_requests : download list;
    mutable client_host : (Ip.t * int) option;
  }

and file_uri =
  FileByIndex of int * string
| FileByUrl of string
  
and upload_client = {
    uc_sock : TcpBufferedSocket.t;
    uc_file : CommonUploads.shared_file;
    mutable uc_chunk_pos : int64;
    uc_chunk_len : int64;
    uc_chunk_end : int64;
  }

  (*
and result = {
    result_result : result_info;
    mutable result_sources : (user * file_uri) list;
    mutable result_uids : Uid.t list;
  }
*)
  
and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    mutable file_name : string;
    mutable file_swarmer : Int64Swarmer.t option;
    mutable file_clients : client list;
    mutable file_uids : Uid.t list; 
    mutable file_searches : local_search list;
  }

and download = {
    download_file : file;
    download_uri : file_uri;
    mutable download_chunks : (int64 * int64) list;
    mutable download_uploader : Int64Swarmer.uploader option;
    mutable download_ranges : (int64 * int64 * Int64Swarmer.range) list;
    mutable download_block : Int64Swarmer.block option;
  }

  