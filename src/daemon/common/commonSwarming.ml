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

open Int64ops
open Options
open Printf2
  
open CommonOptions

  
let check_swarming = false
let debug_present_chunks = false
let debug_all = false

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         MODULE Make                                   *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

open CommonTypes

module type CommonEnv = sig
    
    module CommonTypes : sig
        type file
        type client
      end
    
    open CommonTypes
    module CommonClient : sig
        val client_num : client -> int
        val client_has_bitmap : client -> file -> string -> unit
      end
    
    module CommonFile : sig
        val file_num : file -> int          
        val set_file_last_seen : file -> int -> unit
        val file_size : file -> int64
        val file_best_name : file -> string
        val file_state : file -> file_state
        val file_downloaded : file -> int64
        val add_file_downloaded : file -> int64 -> unit
        val file_write : file -> int64 -> string -> int -> int -> unit
        val file_verify : file -> uid_type -> int64 -> int64 -> bool
        val file_mtime : file -> float
        val file_copy : file -> file -> int64 -> int64 -> int64 -> unit
        val file_fd : file -> Unix32.t
        val set_file_fd : file -> Unix32.t -> unit
        val file_disk_name : file -> string
      end
  end

type strategy =
  LinearStrategy    (* one after the other one *)
| AdvancedStrategy  (* first chunks first, rarest chunks second, 
     complete first third, and random final *)

exception VerifierNotReady

type chunks =
  AvailableRanges of (int64 * int64) list
(* A bitmap is encoded with '0' for empty, '1' for present *)
| AvailableCharBitmap of string 
(* A bitmap encoded as an array of boolean *)
| AvailableBoolBitmap of bool array

type verification =
  NoVerification
| VerificationNotAvailable
| ForceVerification
| Verification of uid_type array
  
module type Maker = functor (M : CommonEnv) -> sig
  
  type t
  type swarmer
  and range
  and uploader
  and block
  
  val create_swarmer : string -> int64 -> int64 -> swarmer
  val create : swarmer -> M.CommonTypes.file -> int64 -> t
    
  val set_verifier : t -> verification -> unit
  val set_verified : t -> (int -> int -> unit) -> unit
  
  val set_present : t -> (int64 * int64) list -> unit
  val set_absent : t -> (int64 * int64) list -> unit
  
  val verified_bitmap : t -> string
  val set_verified_bitmap : t -> string -> unit
  val set_verified_chunk : t -> int -> unit
  
  val register_uploader : t -> M.CommonTypes.client -> chunks -> uploader
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
  val block_num : t -> block -> int
  val availability : t -> string
  val downloaded : t -> int64
  val present_chunks : t -> (int64 * int64) list
  val partition_size : t -> int
  val compute_bitmap : t -> unit
  val is_interesting : uploader -> bool	
  val print_uploader : uploader -> unit

(* In these two function, the 'bool' is if verification should take place
  immediatly or not. *)
  val verify_all_chunks : t -> bool -> unit

(* raise Exit if one block checksum has been computed *)
  val verify_one_chunk : t -> unit
  val verify_some_chunks : unit -> unit
  val get_checksums : t -> CommonTypes.uid_type array
  
  val compute_last_seen : t -> int array
  val print_uploaders : t -> unit    
  
  val uploader_swarmer : uploader -> t
  
  val swarmer_to_value : t ->
    (string * Options.option_value) list ->
    (string * Options.option_value) list
  
  val value_to_swarmer : t -> (string * Options.option_value) list -> unit
    
  val has_multinet : bool
  val merge : M.CommonTypes.file -> M.CommonTypes.file -> unit
  val subfiles : t -> M.CommonTypes.file list
    
  val check_finished : t -> bool
end
