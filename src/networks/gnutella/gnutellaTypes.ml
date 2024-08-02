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

    
type request = 
| Tcp_Connect
| Udp_Connect

type host = (server, request, Ip.addr) CommonHosts.host
  
and server = {
    server_server : server CommonServer.server_impl;
    mutable server_agent : string;
    mutable server_description : string;
    mutable server_sock : tcp_connection;
    mutable server_ciphers : GnutellaNetwork.ciphers option;
    mutable server_nfiles : int64;
    mutable server_nusers : int64;
    mutable server_maxnusers : int64;
    mutable server_nkb : int;
    
    mutable server_need_qrt : bool;
    mutable server_ping_last : Md4.t;
    mutable server_last_lni : int;
    mutable server_nfiles_last : int64;
    mutable server_nkb_last : int;
    mutable server_vendor : string;
    mutable server_connected : int64;
    
    mutable server_host : host;
    mutable server_country_code : int option;
    mutable server_query_key : GnutellaNetwork.query_key;
    mutable server_searches : local_search Fifo.t;
    
    mutable server_shared : Intset.t;
  }

and search_type =
  UserSearch of search * string * GnutellaNetwork.search_extension
| FileUidSearch of file * GnutellaNetwork.file_uid
(*| FileWordSearch of file * string *)
  
and local_search = {
    search_search : search_type;
    search_uid : GnutellaNetwork.search_uid;
    mutable search_hosts : Intset.t;
  }

and user = {
    user_user : user CommonUser.user_impl;
    mutable user_kind  : location_kind;
(*    mutable user_files : (result * int) list; *)
    mutable user_speed : int;
    mutable user_uid : Md4.t;
    mutable user_vendor : string;
    mutable user_software : string;
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
    mutable client_country_code : int option;
    mutable client_reconnect : bool;
    mutable client_connected_for : file option;
    mutable client_support_head_request : bool;
  }
  
and upload_client = {
    uc_sock : TcpBufferedSocket.t;
    uc_partial : bool;
    uc_reader : (int64 -> bytes -> int -> int -> unit);
    mutable uc_chunk_pos : int64;
    uc_chunk_len : int64;
    uc_chunk_end : int64;
    uc_size : int64;
    uc_header : string;
  }

and download_request = 
  RANGEReq of int64 * int64 * CommonSwarming.range
| HEADReq
| TTRReq of int
  
  
and file = {
    file_file : file CommonFile.file_impl;
    file_temp : string;
    mutable file_name : string;
    mutable file_swarmer : CommonSwarming.t option;
    mutable file_clients : client list;
    mutable file_uids : Uid.t list; 
    mutable file_searches : local_search list;
    mutable file_clients_queue : client Queues.Queue.t;
    mutable file_nconnected_clients : int;
    mutable file_ttr : TigerTree.t array option;
  }

and download = {
    download_file : file;
    mutable download_uri : GnutellaNetwork.file_uri;
    mutable download_chunks : (int64 * int64) list;
    mutable download_uploader : CommonSwarming.uploader option;
    mutable download_ranges : download_request list;
    mutable download_blocks : CommonSwarming.uploader_block list;
    mutable download_head_requested : bool;
    mutable download_ttr_requested : bool;
  }

and headers = (string * (string * string)) list

and head_request =
  HeadNotRequested
| HeadRequested
| Head of string * headers

type ghandler =
  HttpReader of 
  int * (* Number of bytes that need to be read to find the connection type *)
  (string * 
    (gconn -> TcpBufferedSocket.t -> string * headers -> unit)) list *
  (gconn -> TcpBufferedSocket.t -> unit) (* default connection *)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)
| CipherReader of (GnutellaNetwork.cipher * (gconn -> TcpBufferedSocket.t -> unit))
  
and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;
    mutable gconn_file_info_sent : int list;
    mutable gconn_client_info_sent : bool;
    mutable gconn_verbose : bool ref;
  }
  
open Printf2
  
let print_head first_line headers =
  lprintf "HEADER: %s\n" first_line;
  List.iter (fun (header, (value,_)) ->
      lprintf "   %s = %s\n" header value;
  ) headers;
  lprintf "\n\n"
  
let make_http_header code headers =
  let buf = Buffer.create 100 in
  Printf.bprintf buf "%s\r\n" code;
  List.iter (fun (field, value) ->
      Printf.bprintf buf "%s: %s\r\n" field value
  ) headers;
  Buffer.add_string buf "\r\n";
  Buffer.contents buf


let plugin_enable_hooks = ref ([] : (bool ref -> unit) list)
let plugin_disable_hooks = ref [(fun () -> ())]
