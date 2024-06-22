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

val get_from_client : DonkeyTypes.client -> unit
val request_slot : DonkeyTypes.client -> unit
val check_files_downloaded : unit -> unit
  val block_received :
    DonkeyTypes.client -> Md4.t -> int64 -> string -> int -> int -> unit
val add_client_chunks : DonkeyTypes.client -> DonkeyTypes.file -> Bitv.t -> unit

val unshare_file : DonkeyTypes.file -> unit
val declare_completed_file : DonkeyTypes.file -> unit
val remove_client_slot : DonkeyTypes.client -> unit
  
val search_found : bool -> 
  CommonTypes.search -> Md4.t -> CommonTypes.tag list -> unit
