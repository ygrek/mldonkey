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

(*
val const_int32_255 : int32
val output_int32_8 : out_channel -> int32 -> unit
val output_int32_32 : out_channel -> int32 -> unit
val output_int8 : out_channel -> int -> unit
*)

val output_int : out_channel -> int -> unit
val rev_assoc : 'a -> ('b * 'a) list -> 'b
val buf_string : Buffer.t -> string -> unit
val buf_port : Buffer.t -> int -> unit
val buf_addr : Buffer.t -> Ip.t * int -> unit
val buf_tags :
  Buffer.t -> CommonTypes.tag list -> (string * string) list -> unit
(*val read_uint8 : in_channel -> int32
val read_uint32 : in_channel -> int32 *)
val read_request : in_channel -> string
val output_request : out_channel -> string -> unit
val get_port : string -> int -> int
val get_string : string -> int -> string * int
val get_tags :
  string -> int -> (string * string) list -> CommonTypes.tag list * int
val get_peer : string -> int -> (Ip.t * int) * int
module type Request =
  sig
    type t
    val parse : int -> string -> t
    val print : t -> unit
    val write : Buffer.t -> t -> unit
  end
val find_tag :  string ->
    CommonTypes.tag list -> CommonTypes.tag_value
