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
type event = CLOSED of string | RTIMEOUT | WTIMEOUT | LTIMEOUT | CAN_READ | CAN_WRITE

type t
type handler = t -> event -> unit

val infinite_timeout : float
val last_time : unit -> float

  
val fd : t -> Unix.file_descr
val must_write : t -> bool -> unit
val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit
val set_lifetime : t -> float -> unit
val set_handler : t -> handler -> unit
val handler : t -> handler
val closed : t -> bool
  
  
val create : Unix.file_descr -> handler -> t
val create_blocking : Unix.file_descr -> handler -> t
val close : t -> string -> unit
val shutdown : t -> string -> unit

  
(* val add_timer: float -> (unit -> unit) -> unit *)
  
type timer
val add_timer : float -> (timer -> unit) -> unit
val reactivate_timer : timer -> unit
val loop : unit -> unit
