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

val find_client_zone : DonkeyTypes.client -> unit
val update_zone : DonkeyTypes.file -> 
  int32 -> int32 -> DonkeyTypes.zone -> unit
val client_state : CommonTypes.host_state -> string
val client_file : DonkeyTypes.client -> DonkeyTypes.file
val find_client_block : DonkeyTypes.client -> unit
val verify_chunks : DonkeyTypes.file -> unit 
val set_file_size : DonkeyTypes.file -> int32 -> unit
val start_download : DonkeyTypes.client -> unit
val next_file : DonkeyTypes.client -> unit
  
val remove_file : Md4.t -> unit

val download_engine : unit -> unit
val check_files_md4s : unit -> unit
val clean_client_zones : DonkeyTypes.client -> unit
  
val search_found : CommonTypes.search -> Md4.t -> CommonTypes.tag list -> unit
