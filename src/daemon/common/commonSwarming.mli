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

open CommonTypes

val verbose_swarming : bool ref 
  
module type Integer = sig
    type t
    val add : t -> t -> t
    val sub : t -> t -> t
    val zero : t 
    val of_int : int -> t
    val to_int : t -> int
    val to_string : t -> string
  end
  
module type Swarmer = sig
    type pos
    type t
    type block
    type range
    type partition
    
    val create : unit -> t
    val set_writer : t -> (pos -> string -> int -> int -> unit) -> unit

    val set_size : t -> pos -> unit
    val set_present : t -> (pos * pos) list -> unit
    val partition : t -> (pos -> pos) -> partition
      
    val set_verifier : partition -> (block -> bool) -> unit
    val verified_bitmap : partition -> string
    val set_verified_bitmap : partition -> string -> unit
      
    val register_uploader :     partition -> (pos * pos) list -> block list
    val unregister_uploader : t -> (pos * pos) list -> block list -> unit
    
    val register_uploader_bitmap : partition -> string -> block list
    val unregister_uploader_bitmap : partition -> string -> unit
    
    val get_block: block list -> block
    val find_range: block -> (pos * pos) list -> range list -> pos -> range
    val find_range_bitmap: block -> range list -> pos -> range
      
    val alloc_range : range -> unit
    val free_range : range -> unit
    val received : t -> pos -> string -> int -> int -> unit
    val sort_chunks :  (pos * pos) list ->  (pos * pos) list

    val print_t : string -> t -> unit
    val print_block : block -> unit
      
    val range_range: range ->  pos * pos
    val block_block: block -> int * pos * pos
    val availability : partition -> string
      
    val downloaded : t -> pos
    val present_chunks : t -> (pos * pos) list
    val partition_size : partition -> int

    val debug_print : Buffer.t -> t -> unit
    val compute_bitmap : partition -> unit
      
  end
  
module Make(I: Integer) : Swarmer with type pos = I.t

module Int64Swarmer : Swarmer with type pos = int64
  
val fixed_partition : Int64Swarmer.t -> int64 -> Int64Swarmer.partition
  