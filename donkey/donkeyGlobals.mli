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

open Mftp_comm

  (* hooks *)
  
val server_must_update : DonkeyTypes.server -> unit
val client_must_update : DonkeyTypes.client -> unit
  
val say_hook : (DonkeyTypes.client -> string -> unit) ref
val server_is_connected_hook :
  (DonkeyTypes.server -> server_sock -> unit) ref
val received_from_server_hook :
  (DonkeyTypes.server -> server_sock -> Mftp_server.t -> unit) ref
val server_is_disconnected_hook : (DonkeyTypes.server -> unit) ref
val friend_change_hook : (DonkeyTypes.client -> unit) ref
val file_change_hook : (DonkeyTypes.file -> unit) ref
  
(* constants *)
  
val page_size : int32
val zone_size : int32
val block_size : int32

(* simple variables *)
  
val client_port : int ref
val client_tags  : CommonTypes.tag list ref
val servers_ini_changed : bool ref
val sleeping : bool ref
val upload_credit : int ref
val has_upload : int ref
val remaining_time_for_clients : int ref
val download_counter : int ref
val upload_counter : int ref
val download_credit : int ref
val nshared_files :  int ref
val udp_sock : UdpSocket.t option ref
val last_xs : int ref
val nservers : int ref
val queue_timeout : float ref
val nclients : int ref
val clients_list_len : int ref

(* complex variables *)

(* files not yet shared *)
val shared_files_info : (string, DonkeyTypes.shared_file_info) Hashtbl.t
  
(* donkey searches *)
val local_searches : DonkeyTypes.local_search list ref
  
(* servers *)
val servers_list : DonkeyTypes.server list ref  
val udp_servers_list : DonkeyTypes.server list ref
val servers_by_key : (Ip.t * int, DonkeyTypes.server) Hashtbl2.t
val udp_servers_replies : (Md4.t, DonkeyTypes.server) Hashtbl.t

(* clients *)
val clients_list : (DonkeyTypes.client * (DonkeyTypes.file list)) list ref  
val upload_clients : DonkeyTypes.client Fifo.t  
val clients_by_kind : (CommonTypes.location_kind, DonkeyTypes.client) Hashtbl.t
  
(* files *)
val new_shared : DonkeyTypes.file list ref
val files_by_md4 : (Md4.t, DonkeyTypes.file) Hashtbl.t
val current_files : DonkeyTypes.file list ref
val file_groups : (Md4.t,DonkeyTypes.file_group) Hashtbl.t
  
(* shared files *)
val shared_files : DonkeyTypes.file_to_share list ref

(* result *)
val last_search : DonkeyTypes.result Intmap.t ref

(* UDP clients *)
val udp_clients : (CommonTypes.location_kind,DonkeyTypes.udp_client) Hashtbl.t
  
(* functions *)

val check_useful_client : DonkeyTypes.client -> unit
val set_client_name : DonkeyTypes.client -> string -> Md4.t -> unit
val find_client_by_name : string -> DonkeyTypes.client
  
val find_file : Md4.t -> DonkeyTypes.file  
val new_file : CommonTypes.file_state -> string -> Md4.t -> int32 -> bool -> DonkeyTypes.file
val new_server : Ip.t -> int -> int -> DonkeyTypes.server
val find_server : Ip.t -> int -> DonkeyTypes.server
val new_client : CommonTypes.location_kind -> DonkeyTypes.client
  
val remove_server :  Ip.t -> int -> unit
val remove_client : DonkeyTypes.client -> unit

val info_change_file : DonkeyTypes.file -> unit
val avail_change_file : DonkeyTypes.file -> unit
  
val remove_client_chunks : 
  DonkeyTypes.file -> DonkeyTypes.availability -> unit
val add_client_chunks :   
  DonkeyTypes.file -> DonkeyTypes.availability -> unit
  
val change_hardname : DonkeyTypes.file -> string -> unit
val first_name : DonkeyTypes.file -> string
val set_client_state : 
  DonkeyTypes.client -> CommonTypes.host_state -> unit
  
val set_server_state : 
  DonkeyTypes.server -> CommonTypes.host_state -> unit

val file_state :   DonkeyTypes.file -> CommonTypes.file_state

val file_must_update :   DonkeyTypes.file -> unit
  
val client_state : 
  DonkeyTypes.client -> CommonTypes.host_state
  
val server_state : 
  DonkeyTypes.server -> CommonTypes.host_state

val mem_stats : Buffer.t -> unit
  
val remove_file_clients : DonkeyTypes.file -> unit
  
val new_source : DonkeyTypes.file -> DonkeyTypes.client -> unit

val client_num : DonkeyTypes.client -> int
val file_num : DonkeyTypes.file -> int
val server_num : DonkeyTypes.server -> int
  
val add_connected_server : DonkeyTypes.server -> unit
val remove_connected_server : DonkeyTypes.server -> unit
val connected_servers : unit -> DonkeyTypes.server list

val friend_add : DonkeyTypes.client -> unit
val friend_remove : DonkeyTypes.client -> unit
val current_friends : unit -> DonkeyTypes.client Intmap.t
  
(* indexation *)
  
val results_by_md4 :  (Md4.t, DonkeyTypes.result) Hashtbl.t
val doc_value : Store.index -> CommonTypes.result_info
val store : CommonTypes.result_info Store.t
  

val comments : (Md4.t,string) Hashtbl.t
val comment_filename : string
    
module Document : sig
    type t = Store.index
      
    val num : Store.index -> int
    val filtered : Store.index -> bool 
    val filter : Store.index -> bool -> unit
  end

module DocIndexer : sig 
    type index
    val add : index -> string -> Document.t -> int -> unit
    val clear : index -> unit
    val clear_filter : index -> unit
    val filter_words : index -> string list -> unit
    val filtered : Document.t -> bool 
    val query : index -> Document.t Indexer.query -> Document.t array
  end
  
val index : DocIndexer.index
val history_file_oc : out_channel option ref
val history_file : string
  
val artist_bit : int
val album_bit : int
val title_bit : int
val format_bit : int
val media_bit : int
  
val client_is_friend : DonkeyTypes.client -> bool
