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
open CommonDownloads

(* any = 0 *)
let name_of_tag = 
  [
    "year", 1;
    "filename", 2;
    "hash", 3;
    "title", 4;
    "time", 5;
    "artist", 6;
    "album", 8;
    "language", 10;
    "keywords", 12;
    "resolution", 13;
    "genre", 14;
    "OS", 16;
    "bitdepth", 17;
    "quality", 21;
    "version", 24;
    "comment", 26;
    "codec", 28; (* "divx" *)
    "rating", 29;
    "size", 33;
    "type", 34; (* "movie", "video clip",... *)
  ]

type host_kind =
  Ultrapeer
| Peer
| IndexServer

type cipher

type ciphers = {
    in_cipher : cipher;
    out_cipher : cipher;
    mutable in_xinu : int64;
    mutable out_xinu : int64;
  }
  
type host = {
    host_num : int;
    mutable host_server : server option;
    host_addr : Ip.addr;
    host_port : int;
    mutable host_age : int;
    mutable host_udp_request : int;
    mutable host_tcp_request : int;
    mutable host_connected : int;
    mutable host_kind : host_kind;
    
    mutable host_queues : host Queue.t list;
  }

and server = {
    server_server : server CommonServer.server_impl;
    mutable server_agent : string;
    mutable server_sock : tcp_connection;
    mutable server_ciphers : ciphers option;
    mutable server_nfiles : int;
    mutable server_nusers : int;
    mutable server_nkb : int;

    mutable server_need_qrt : bool;
    mutable server_ping_last : Md4.t;
    mutable server_nfiles_last : int;
    mutable server_nkb_last : int;
    mutable server_vendor : string;
    mutable server_connected : int64;
    
    mutable server_host : host;
    
    mutable server_searches : local_search Fifo.t;
  }

(*
typedef enum
{	QUERY_REALM_EVERYTHING	= 0x3f,
	QUERY_REALM_AUDIO		= 0x21,
	QUERY_REALM_VIDEO		= 0x22,
	QUERY_REALM_IMAGES		= 0x23,
	QUERY_REALM_DOCUMENTS	= 0x24,
	QUERY_REALM_SOFTWARE	= 0x25
} FSTQueryRealm;

typedef enum
{
	QUERY_CMP_EQUALS=0x00,
	QUERY_CMP_ATMOST=0x02,
	QUERY_CMP_APPROX=0x03,
	QUERY_CMP_ATLEAST=0x04,
	QUERY_CMP_SUBSTRING=0x05
} FSTQueryCmp;

/*****************************************************************************/
  *)

and query_term =
  AtMost of string * int64
| AtLeast of string * int64
| Substring of string * string

and local_search = {
    search_search : search_type;
    search_id : int;
  }
  
and search_type =
  UserSearch of search * string * string * query_term list
| FileSearch of file
| UserBrowse

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
    mutable client_in_queues : file list;
    mutable client_connection_control : connection_control;
    mutable client_sock : tcp_connection;
    mutable client_user : user;
    mutable client_all_files : file list option;
    mutable client_requests : download list;
    mutable client_host : (Ip.t * int) option;
    mutable client_reconnect : bool;
    mutable client_connected_for : file option;
    mutable client_support_head_request : bool;
  }
  
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
  }
*)
  
and file = {
    file_file : file CommonFile.file_impl;
    file_id : Md4.t;
    mutable file_name : string;
    mutable file_swarmer : Int64Swarmer.t option;
    mutable file_clients : client list;
    mutable file_search : local_search;
    mutable file_hash : Md5Ext.t;
    mutable file_filenames : (string * ips_list) list;
    mutable file_clients_queue : client Queues.Queue.t;
    mutable file_nconnected_clients : int;
  }

and download = {
    download_file : file;
    mutable download_chunks : (int64 * int64) list;
    mutable download_uploader : Int64Swarmer.uploader option;
    
(* TODO: remove these two fields as they are already inside 
Int64Swarmer.uploader. Redundant information might be a problem if
consistency is not guaranteed. Do the same for BT *)
    mutable download_ranges : (int64 * int64 * Int64Swarmer.range) list;
    mutable download_block : Int64Swarmer.block option;
    mutable download_head : head_request;
  }

and head_request =
  HeadNotRequested
| HeadRequested
| Head of string
  
external create_cipher : unit -> cipher = "ml_create_cipher"
external apply_cipher : cipher -> string -> int -> int -> unit
  = "ml_apply_cipher"
external init_cipher : cipher -> int32 -> int -> unit = "ml_init_cipher"
external set_cipher : cipher -> int32 -> int -> unit = "ml_set_cipher"
external cipher_packet_get : string -> int -> cipher -> cipher -> unit
  = "ml_cipher_packet_get"
external cipher_packet_set : cipher -> string -> int -> unit
  = "ml_cipher_packet_set"
external cipher_free : cipher -> unit = "ml_cipher_free"

(*

This should be used to check whether the encryption is OK on your platform.
Since pointers are placed in integers, it might be dangerous to call this 
function on 64-bits computers ?

*)
