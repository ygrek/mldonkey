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

module Basic :
  sig
    type ('a, 'b) download = {
      download_file : 'a;
      download_client : 'b;
      mutable download_min_read : int;
      mutable download_pos : int64;
      mutable download_sock : TcpBufferedSocket.t option;
    }
    module Make :
      functor
        (M : sig
               type f
               type c
               val file : f -> CommonTypes.file
               val client : c -> CommonTypes.client
               val client_disconnected : (f, c) download -> unit
               val download_finished : (f, c) download -> unit
             end) ->
        sig
          val disconnect_download :
            (M.f, M.c) download -> BasicSocket.close_reason -> unit
          val file_complete : (M.f, M.c) download -> unit
          val download_reader :
            (M.f, M.c) download -> TcpBufferedSocket.t -> int -> unit
          val new_download :
            TcpBufferedSocket.t -> M.c -> M.f -> int -> (M.f, M.c) download
        end
  end
  
  
open CommonSwarming
module Int64Swarmer :
  sig
    type t
    type swarmer
    and range
    and uploader
    and block
    val create_swarmer : string -> int64 -> int64 -> swarmer
    val remove_swarmer : t option -> unit
    val create : swarmer -> CommonTypes.file -> int64 -> t
    val set_verifier : t -> CommonSwarming.verification -> unit
    val set_verified : t -> (int -> int -> unit) -> unit
    val set_present : t -> (int64 * int64) list -> unit
    val set_absent : t -> (int64 * int64) list -> unit
    val verified_bitmap : t -> string
    val set_verified_bitmap : t -> string -> unit
    val set_verified_chunk : t -> int -> unit
    val register_uploader : t -> CommonTypes.client -> chunks -> uploader
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
    val verify_all_chunks : t -> bool -> unit
    val verify_one_chunk : t -> unit
    val verify_some_chunks : unit -> unit
      val get_checksums : t -> CommonTypes.uid_type array 
    val compute_last_seen : t -> int array
    val print_uploaders : t -> unit
    val uploader_swarmer : uploader -> t
    val swarmer_to_value :
      t ->
      (string * Options.option_value) list ->
      (string * Options.option_value) list
    val value_to_swarmer : t -> (string * Options.option_value) list -> unit
    val has_multinet : bool
    val merge : CommonTypes.file -> CommonTypes.file -> unit
    val subfiles : t -> CommonTypes.file list
    val check_finished : t -> bool
  end
