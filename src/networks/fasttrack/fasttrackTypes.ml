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
open CommonSwarming
open CommonHosts
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

type host = (server, host_kind, unit, Ip.addr) CommonHosts.host

and server = {
    server_server : server CommonServer.server_impl;
    mutable server_agent : string;
    mutable server_sock : tcp_connection;
    mutable server_ciphers : ciphers option;
    mutable server_nfiles : int;
    mutable server_nkb : int;
    
    mutable server_need_qrt : bool;
    mutable server_ping_last : Md4.t;
    mutable server_nfiles_last : int;
    mutable server_nkb_last : int;
    mutable server_vendor : string;
    mutable server_connected : int32;
    
    mutable server_host : host;
    
    mutable server_searches : local_search Fifo.t;
  }

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
(* FOR SOURCES 
    mutable client_source : source option;
    mutable client_score : int;
    mutable client_files : file_request list;
    mutable client_next_queue : int;
mutable client_requests_sent: int;    
  *)
  }

and source = {
    source_num : int;
    source_addr : Ip.t * int;
    mutable source_client: client_kind;     
(* This field is not kept up-to-date when source_client = SourceClient c,
  change c.client_source_for *)
    mutable source_files : file_request list;
    mutable source_overnet : bool;
    mutable source_score : int;
    mutable source_age : int;
    mutable source_in_queues : file list;
  }


and file_request = {
    request_file : file;
    mutable request_time : int;
    mutable  request_result : request_result;
  }

and request_result = 
| File_possible (* we asked, but didn't know *)
| File_not_found (* we asked, the file is not there *)
| File_expected (* we asked, because it was announced *)
| File_new_source (* we never asked *)
| File_found    (* the file was found *)
| File_chunk    (* the file has chunks we want *)
| File_upload   (* we uploaded from this client *)

and client_kind = 
  SourceClient of client
| SourceLastConnection of 
  int *
  int * (* last connection attempt *)
  int   (* booked client num *)

and upload_client = {
    uc_sock : TcpBufferedSocket.t;
    uc_file : CommonUploads.shared_file;
    mutable uc_chunk_pos : int64;
    uc_chunk_len : int64;
    uc_chunk_end : int64;
  }

and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int64;
    mutable result_tags : tag list;
    mutable result_sources : user list;
    mutable result_hash : Md5Ext.t;
  }

and file = {
(* The part of the file structure which is used for all networks *)
    file_shared : MultinetTypes.file;

(* The part of the file structure which is only needed for this network *)
    file_partition : CommonSwarming.Int64Swarmer.partition;
    mutable file_clients : client list;  
    mutable file_search : local_search;
    mutable file_hash : Md5Ext.t;
    mutable file_clients_queue : client Queues.Queue.t;
    mutable file_nconnected_clients : int;
(*    
    mutable file_locations : client Intmap.t; 
    mutable file_clients : (client * int) Fifo.t;
    mutable file_sources : source Queue.t array;
*)    
  }

and download = {
    download_file : file;
    mutable download_chunks : (int64 * int64) list;
    mutable download_blocks : Int64Swarmer.block list;
    mutable download_ranges : (int64 * int64 * Int64Swarmer.range) list;
    mutable download_block : Int64Swarmer.block option;
  }

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

open CommonFile
open CommonClient
open CommonServer
open CommonResult 

  
let set_client_state c s =
  let cc = as_client c.client_client in
  set_client_state cc s
        
let set_client_disconnected c =
  let cc = as_client c.client_client in
  set_client_disconnected cc
   
let client_state client =
  CommonClient.client_state (as_client client.client_client)
    
let client_new_file client r =
  client_new_file (as_client client.client_client) ""
  (as_result r.result_result)
    

module SourcesQueueCreate = struct  

    let lifo = lifo 
    let fifo = fifo
    
    module SourcesSet = Set.Make (
        struct
          type t = int * source
          let compare (t1,s1) (t2,s2) = 
            if s1.source_addr = s2.source_addr then begin
                0 end else              
            let x = compare t1 t2 in
            if x = 0 then compare s1.source_addr s2.source_addr else x
        end
      )

    let oldest_first () = 
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ -> try SourcesSet.min_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try 
              let x = SourcesSet.min_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

    let oldest_last () = 
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ ->
            try SourcesSet.max_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try
              let x = SourcesSet.max_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

      (*
    let max_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.max_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.max_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }

    let min_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ -> SourcesSet.min_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.min_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }
*)
  end
  

  
let file_state file = MultinetTypes.file_state file.file_shared
let client_num c = client_num (as_client c.client_client)
let server_state s = server_state (as_server s.server_server)
let server_num s = server_num (as_server s.server_server)
let set_server_state s t = set_server_state (as_server s.server_server) t
  