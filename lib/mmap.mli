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

type t

val mmap : string -> Unix.file_descr -> int32 -> int32 -> t
val truncate : t -> int32 -> unit
val blit_from_string : string -> int32 -> t -> int32 -> int32 -> unit
val blit_to_string :  t -> int32 -> string -> int32 -> int32 -> unit
val munmap : t -> unit
val md4_sub : t -> int32 -> int32 -> Md4.t
val msync: t -> unit
