(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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
open Options

val add_server : Ip.t -> int -> DonkeyTypes.server
val check_add_server : Ip.t -> int -> DonkeyTypes.server
val safe_add_server : Ip.t -> int -> unit

val remove_server : Ip.t -> int -> unit
  
val known_shared_files : DonkeyTypes.shared_file_info 
  list Options.option_record

val old_files : Md4.t list Options.option_record

  
val value_to_addr : Options.option_value -> Ip.t * int
val value_to_md4 : Options.option_value -> Md4.t
val file_to_value :  DonkeyTypes.file -> (string * Options.option_value) list

val load_sources : unit -> unit
  
val gstats_by_brand : DonkeyTypes.brand_stat array Options.option_record
val gstats_by_brand_mod : DonkeyTypes.brand_mod_stat array Options.option_record
val guptime : unit -> int
  