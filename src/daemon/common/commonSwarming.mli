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

module Make (M : sig
      
      module CommonTypes : sig
          type file
          type client
          type uid_type
        end
      
      open CommonTypes
      module CommonClient : sig
          val client_num : client -> int          
        end
      
      module CommonFile : sig
          val file_num : file -> int          
          val set_file_last_seen : file -> int -> unit
          val file_size : file -> int64
          val file_best_name : file -> string
          val file_state : file -> file_state
          val file_fd : file -> Unix32.t
          val add_file_downloaded : file -> int64 -> unit
        end
    
    end) : sig
    
    open M
    open CommonTypes
    
    
    type strategy =
      LinearStrategy    (* one after the other one *)
    | AdvancedStrategy  (* first chunks first, rarest chunks second, 
     complete first third, and random final *)
    
    exception VerifierNotReady
      
      
    type t
    and range
    and uploader
    and block
      
    type chunks =
      AvailableRanges of (int64 * int64) list
(* A bitmap is encoded with '0' for empty, '1' for present *)
    | AvailableCharBitmap of string 
(* A bitmap encoded as an array of boolean *)
    | AvailableBoolBitmap of bool array
    
    val create : file -> int64 -> int64 -> t
    val set_writer : t -> (int64 -> string -> int -> int -> unit) -> unit
    val set_verifier : t -> (int -> int64 -> int64 -> bool) -> unit
    
    val set_present : t -> (int64 * int64) list -> unit
    val set_absent : t -> (int64 * int64) list -> unit
    
    val verified_bitmap : t -> string
    val set_verified_bitmap : t -> string -> unit
    val set_verified_block : t -> int -> unit
    
    val register_uploader : t -> client -> chunks -> uploader
    val update_uploader : uploader -> chunks -> unit
    val clear_uploader_block : uploader -> unit      
    val clear_uploader_ranges : uploader -> unit      
    val unregister_uploader : uploader -> unit      
    
    val find_block : uploader -> block
    val find_range : uploader -> int64 * int64 * range
    val current_block : uploader -> block
    val current_ranges : uploader -> (int64 * int64 * range) list
      
    val received : uploader -> int64 -> string -> int -> int -> unit
    val print_t : string -> t -> unit
    val range_range : range -> int64 * int64
      
      
    val print_block : block -> unit
    val block_block : block -> int * int64 * int64 
    val availability : t -> string
    val downloaded : t -> int64
    val present_chunks : t -> (int64 * int64) list
    val partition_size : t -> int
    val compute_bitmap : t -> unit
    val is_interesting : uploader -> bool	
    val print_uploader : uploader -> unit

(* In these two function, the 'bool' is if verification should take place
  immediatly or not. *)
    val must_verify_block : t -> int -> bool -> unit
    val verify_all_blocks : t -> bool -> unit
      
(* raise Exit if one block checksum has been computed *)
    val verify_one_block : t -> unit
    val set_checksums : t option -> CommonTypes.uid_type array -> unit
      
    val compute_last_seen : t -> int array
    val print_uploaders : t -> unit    
      
    val uploader_swarmer : uploader -> t
      
    val swarmer_to_value : t ->
      (string * Options.option_value) list ->
      (string * Options.option_value) list
      
    val value_to_swarmer : t -> (string * Options.option_value) list -> unit
  end
