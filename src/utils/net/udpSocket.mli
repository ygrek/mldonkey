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
  | READ_DONE
  | BASIC_EVENT of BasicSocket.event
type udp_packet = { 
    udp_ping: bool; udp_content : bytes; udp_addr : Unix.sockaddr; } 
type t
(*
  type t = {
    mutable sock : BasicSocket.t;
    mutable rlist : udp_packet list;
    mutable wlist : (udp_packet * float) list;
    mutable revwlist : (udp_packet * float) list;
    mutable event_handler : handler;
    mutable write_controler : TcpBufferedSocket.bandwidth_controler option;
  } *)
and handler = t -> event -> unit
val read : t -> udp_packet
val set_handler : t -> event -> (t -> unit) -> unit
val set_refill : t -> (t -> unit) -> unit
val set_reader : t -> (t -> unit) -> unit
val sock : t -> BasicSocket.t
val closed : t -> bool
val close : t -> BasicSocket.close_reason -> unit
val write : t -> bool -> bytes -> Ip.t -> int -> unit
val create : Unix.inet_addr -> int -> handler -> t
val create_sendonly : unit -> t
val can_write : t -> bool
val debug : bool ref  
  
val udp_uploaded_bytes : int64 ref
val udp_downloaded_bytes : int64 ref
val read_packets : t -> (udp_packet -> unit) -> unit
  
type bandwidth_controler
val set_write_controler : t -> bandwidth_controler -> unit
val new_bandwidth_controler : TcpBufferedSocket.bandwidth_controler ->
  bandwidth_controler
  
val remaining_bytes : bandwidth_controler -> int
val use_remaining_bytes : bandwidth_controler -> int -> unit
  
  
val get_latencies : bool ref -> string
val declare_pong : Ip.t -> unit
