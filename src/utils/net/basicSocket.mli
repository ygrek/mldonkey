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

type close_reason =
  Closed_for_timeout    (* timeout exceeded *)
| Closed_for_lifetime   (* lifetime exceeded *)
| Closed_by_peer        (* end of file *)
| Closed_for_error of string
| Closed_by_user         (* the operation was completed *)
| Closed_for_overflow
| Closed_connect_failed
| Closed_for_exception of exn

type event = 
  CLOSED of close_reason
| RTIMEOUT | WTIMEOUT | LTIMEOUT | CAN_READ | CAN_WRITE

type t
type handler = t -> event -> unit

val infinite_timeout : float
val last_time : unit -> int
val update_time : unit -> unit
  
val fd : t -> Unix.file_descr
val must_write : t -> bool -> unit
val must_read : t -> bool -> unit
val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit
val set_lifetime : t -> float -> unit
val set_handler : t -> handler -> unit
val handler : t -> handler
val closed : t -> bool

(* val set_before_select : t -> (t -> unit) -> unit *)
    
val create : string -> Unix.file_descr -> handler -> t
val create_blocking : string -> Unix.file_descr -> handler -> t
val set_printer : t -> (t -> string) -> unit
val close : t -> close_reason -> unit
val shutdown : t -> close_reason -> unit

val set_before_select_hook : (unit -> unit) -> unit
val set_after_select_hook : (unit -> unit) -> unit
  
(* val add_timer: float -> (unit -> unit) -> unit *)
  
type timer
val add_timer : float -> (timer -> unit) -> unit
val add_infinite_timer : float -> (unit -> unit) -> unit
val add_session_timer : bool ref -> float -> (unit -> unit) -> unit
val add_session_option_timer : bool ref -> float Options.option_record -> (unit -> unit) -> unit
val add_infinite_option_timer : float Options.option_record -> (unit -> unit) -> unit
  
val reactivate_timer : timer -> unit
val loop : unit -> unit

val nb_sockets : unit -> int
  
val stats : Buffer.t -> t -> unit
  
val minf : float -> float -> float
val maxf : float -> float -> float
  
val set_allow_write : t -> bool ref -> unit
val set_allow_read : t -> bool ref -> unit
  
val print_sockets : Buffer.t -> unit
val print_socket : Buffer.t -> t -> unit
  
val info : t -> string
  
val debug : bool ref
  
val set_dump_info : t -> (Buffer.t -> unit) -> unit
  
external use_poll : bool -> unit = "ml_use_poll"
  
val prevent_close : t -> unit
val close_all : unit -> unit
  
val start_time : int
  
external setsock_iptos_throughput: Unix.file_descr -> int = "ml_setsock_iptos_throughput"
 
val sock_num : t -> int
val string_of_date : int -> string
val date_of_int : int -> float
val normalize_time : int -> int
  
val get_rtimeout : t -> float * float
val int64_time : unit -> int64
val current_time : unit -> float

val string_of_reason : close_reason -> string
val string_of_basic_event : event -> string

val loop_delay : float ref

val socket_keepalive : bool ref

(*
0 : not verbose
1 : summary every second
2 : system calls
3 : internal values
*)
val verbose_bandwidth :  int ref
val add_bandwidth_second_timer : (unit -> unit) -> unit

