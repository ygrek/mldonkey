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
type event =
    WRITE_DONE
  | CAN_REFILL
  | CONNECTED
  | BUFFER_OVERFLOW
  | READ_DONE of int
  | BASIC_EVENT of BasicSocket.event

  
and buf = {
  mutable buf : string;
  mutable pos : int;
  mutable len : int;
  mutable max_buf_size : int;
  } 
  
type t
  
type bandwidth_controler (* = {
    mutable remaining_bytes : int;
    mutable total_bytes : int;
    mutable nconnections : int;
    mutable connections : t list;
    allow_io : bool ref;
    mutable last_remaining : int;
    mutable moved_bytes : int64;
  }
*)
  
  
and handler = t -> event -> unit

val max_buffer_size : int ref

val sock: t -> BasicSocket.t
val create : string -> Unix.file_descr -> handler -> t
val create_simple : string -> Unix.file_descr -> t
val create_blocking : string -> Unix.file_descr -> handler -> t
val buf : t -> buf
val set_reader : t -> (t -> int -> unit) -> unit
val sock_used : t -> int -> unit
val buf_create : int -> buf
val buf_used : buf -> int -> unit
val buf_add : t -> buf -> string -> int -> int -> unit
val set_handler : t -> event -> (t -> unit) -> unit
val set_refill : t -> (t -> unit) -> unit
val write: t -> string -> int -> int -> unit
val write_string: t -> string -> unit
val connect: string -> Unix.inet_addr -> int -> handler -> t
val close : t -> BasicSocket.close_reason -> unit
val closed : t -> bool
val shutdown : t -> BasicSocket.close_reason -> unit
val error: t -> BasicSocket.close_reason
val tcp_handler: t -> BasicSocket.t -> BasicSocket.event -> unit
val set_closer : t -> (t -> BasicSocket.close_reason -> unit) -> unit
val nread : t -> int
val set_max_write_buffer : t -> int -> unit  
val can_write : t -> bool  
val can_write_len : t -> int -> bool  
val set_monitored : t -> unit
  
val close_after_write : t -> unit

val create_read_bandwidth_controler : int -> bandwidth_controler
val create_write_bandwidth_controler : int -> bandwidth_controler
val set_read_controler : t -> bandwidth_controler -> unit
val set_write_controler : t -> bandwidth_controler -> unit
val change_rate : bandwidth_controler -> int -> unit

  
val my_ip : t -> Ip.t
val peer_ip : t -> Ip.t
val host : t -> Ip.t * int
  
val stats :  Buffer.t -> t -> unit
val buf_size : t -> int * int
val can_fill : t -> bool
  
(* val if_possible : bandwidth_controler -> int -> bool *)
  
val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit

val internal_buf : Buffer.t
val value_send : t -> 'a -> unit
val value_handler : ('a -> t -> unit) -> t -> int -> unit

val set_write_power : t -> int -> unit
val set_read_power : t -> int -> unit
  
val remaining_to_write : t -> int
  
val set_lifetime : t -> float -> unit
  
val tcp_uploaded_bytes : int64 ref
val tcp_downloaded_bytes : int64 ref
val moved_bytes : bandwidth_controler -> int64
  
val set_remaining_bytes_user : bandwidth_controler ->
  (int -> int -> unit) -> unit
val set_lost_bytes : bandwidth_controler -> int -> int -> unit
val compute_lost_byte : bandwidth_controler -> int
  
  
val exec_command : string -> string array -> handler -> t * t
  
val not_buffer_more : t -> int -> bool
  
val nread : t -> int
val nwritten : t -> int
  
val accept_connection_bandwidth :
  bandwidth_controler ->  bandwidth_controler -> unit 
  
val ip_packet_size : int ref
val mtu_packet_size : int ref
val minimal_packet_size : int ref

val remove_ip_packet : bandwidth_controler -> unit

val get_rtimeout : t -> float * float
  
val http_proxy : (string * int) option ref
  
