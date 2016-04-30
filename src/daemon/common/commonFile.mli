(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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

type 'a file_impl = {
    mutable impl_file_owner : CommonTypes.userdb;
    mutable impl_file_group : CommonTypes.groupdb option;
    mutable impl_file_update : int;
    mutable impl_file_state : CommonTypes.file_state;
    
    mutable impl_file_comment : string;
  mutable impl_file_num : int;
  mutable impl_file_val : 'a;
  mutable impl_file_ops : 'a file_ops;
  mutable impl_file_size : int64;
  mutable impl_file_age : int;
  mutable impl_file_fd : Unix32.t option;
  mutable impl_file_downloaded : int64;
  mutable impl_file_received : int64;
  impl_file_last_received : (int64 * int) Queue.t;
  mutable impl_file_last_rate : float;
  mutable impl_file_best_name : string;
  mutable impl_file_filenames : string list;
  mutable impl_file_magic : string option;
  mutable impl_file_priority : int;
  mutable impl_file_release : bool;
    mutable impl_file_last_seen : int;
    mutable impl_file_probable_name : string option;
} 
and 'a file_ops = {
  mutable op_file_network : CommonTypes.network;
  mutable op_file_commit : 'a -> string -> unit;
  mutable op_file_save_as : 'a -> string -> unit;
  mutable op_file_shared : 'a -> CommonTypes.shared option;
  mutable op_file_to_option : 'a -> (string * Options.option_value) list;
  mutable op_file_cancel : 'a -> unit;
  mutable op_file_pause : 'a -> unit;
  mutable op_file_queue : 'a -> unit;
  mutable op_file_resume : 'a -> unit;
  mutable op_file_download_order : 'a -> CommonTypes.swarming_strategy option -> CommonTypes.swarming_strategy option;
  mutable op_file_info : 'a -> GuiTypes.file_info;
  mutable op_file_set_format : 'a -> CommonTypes.format -> unit;
  mutable op_file_check : 'a -> unit;
  mutable op_file_recover : 'a -> unit;
  mutable op_file_all_sources : 'a -> CommonTypes.client list;
  mutable op_file_active_sources : 'a -> CommonTypes.client list;
  mutable op_file_comment : 'a -> string;
    mutable op_file_print: 'a -> CommonTypes.ui_conn -> unit;
    mutable op_file_print_sources : 'a -> CommonTypes.ui_conn -> unit;
    mutable op_file_files : ('a -> 'a file_impl -> CommonTypes.file list);    
    mutable op_file_debug : 'a -> string;
    mutable op_file_proposed_filenames : 'a -> string list;
} 
val as_file : 'a file_impl -> CommonTypes.file
val as_file_impl : CommonTypes.file -> 'a file_impl
val file_num : CommonTypes.file -> int
val dummy_file_impl : unit -> int file_impl
val dummy_file : CommonTypes.file
val file_counter : int ref
val ni : CommonTypes.network -> string -> string
val fni : CommonTypes.network -> string -> 'a
val ni_ok : CommonTypes.network -> string -> unit
val file_must_update : CommonTypes.file -> unit
val file_must_update_downloaded : CommonTypes.file -> unit
val update_file_num : 'a file_impl -> unit
val update_file_state : 'a file_impl -> CommonTypes.file_state -> unit
val file_to_option : CommonTypes.file -> (string * Options.option_value) list
val file_save_as : CommonTypes.file -> string -> unit
val file_shared : CommonTypes.file -> CommonTypes.shared option
val file_comment : CommonTypes.file -> string
val file_network : CommonTypes.file -> CommonTypes.network
val file_info : CommonTypes.file -> GuiTypes.file_info
val file_pause : CommonTypes.file -> CommonTypes.userdb -> unit
val file_queue : CommonTypes.file -> unit
val file_resume : CommonTypes.file -> CommonTypes.userdb -> unit
val set_file_release : CommonTypes.file -> bool -> CommonTypes.userdb -> unit
val file_release : CommonTypes.file -> bool
val set_file_state : CommonTypes.file -> CommonTypes.file_state -> unit
val file_best_name : CommonTypes.file -> string
val set_file_best_name : CommonTypes.file -> string -> ?fs:Unix32.fstype -> int -> unit
val add_file_filenames : CommonTypes.file -> string -> unit
val shorten_all_file_filenames : int -> unit
val set_file_format : CommonTypes.file -> CommonTypes.format -> unit
val file_check : CommonTypes.file -> unit
val file_recover : CommonTypes.file -> unit
val file_preview : CommonTypes.file -> unit
val file_all_sources : CommonTypes.file -> CommonTypes.client list
val file_active_sources : CommonTypes.file -> CommonTypes.client list
val file_print_sources : CommonTypes.file -> CommonTypes.ui_conn -> unit
val files_ops : (int file_ops * int file_ops) list ref
val new_file_ops : CommonTypes.network -> 'a file_ops
val check_file_implementations : unit -> unit
val file_find : int -> CommonTypes.file
val file_state : CommonTypes.file -> CommonTypes.file_state
val file_add_source : CommonTypes.file -> CommonTypes.client -> unit
val file_remove_source : CommonTypes.file -> CommonTypes.client -> unit
val sample_timer : unit -> unit
val file_download_rate : 'a file_impl -> float
val add_file_downloaded : CommonTypes.file -> Int64.t -> unit
val file_downloaders : CommonTypes.file -> CommonTypes.ui_conn -> int -> bool
val colored_chunks : VerificationBitmap.t -> string
val file_print : CommonTypes.file -> CommonTypes.ui_conn -> unit
val file_print_ed2k_link : string -> int64 -> Md4.Md4.t -> string
val file_size : CommonTypes.file -> int64
val file_disk_name : CommonTypes.file -> string
val file_fd : CommonTypes.file -> Unix32.t
val set_file_fd : CommonTypes.file -> Unix32.t -> unit
val set_file_disk_name : CommonTypes.file -> string -> unit
val file_downloaded : CommonTypes.file -> int64
val file_network : CommonTypes.file -> CommonTypes.network
val file_priority : CommonTypes.file -> int
val file_download_order : CommonTypes.file -> CommonTypes.swarming_strategy option -> CommonTypes.swarming_strategy option
val file_print_download_order : CommonTypes.file -> string
val set_file_priority : CommonTypes.file -> int -> unit
val set_file_last_seen : CommonTypes.file -> int -> unit
val file_debug : CommonTypes.file -> string
val set_file_comment : CommonTypes.file -> string -> unit
val file_comment : CommonTypes.file -> string
val file_magic : CommonTypes.file -> string option
val check_magic : CommonTypes.file -> unit  
val recover_bytes : CommonTypes.file -> (int64 * int64) list
val file_write : CommonTypes.file -> int64 -> string -> int -> int -> unit
val file_verify : CommonTypes.file ->
  CommonTypes.uid_type -> int64 -> int64 -> bool
val file_mtime : CommonTypes.file -> float
  val file_copy :
  CommonTypes.file -> CommonTypes.file -> int64 -> int64 -> int64 -> unit
val file_files : CommonTypes.file -> CommonTypes.file list
val propose_filenames : unit -> unit
val propose_filename : CommonTypes.file -> unit

val forceable_download : CommonTypes.result_info list ref
val impl_file_info : 'a file_impl -> GuiTypes.file_info

val user2_filter_files : CommonTypes.file list -> CommonTypes.userdb -> CommonTypes.file list
val user2_num_user_dls : CommonTypes.userdb -> int
val user2_num_group_dls : CommonTypes.groupdb -> int
val user2_allow_file_admin : CommonTypes.file -> CommonTypes.userdb -> bool
val set_file_owner : CommonTypes.file -> CommonTypes.userdb -> unit
val file_owner : CommonTypes.file -> CommonTypes.userdb
val set_file_group : CommonTypes.file -> CommonTypes.groupdb option -> unit
val file_group : CommonTypes.file -> CommonTypes.groupdb option
val lprintf_file_nl : ?exn:exn -> CommonTypes.file -> ('a, unit, string, unit) Pervasives.format4 -> 'a

(** [concat_file dir filename] sanitizes [filename] and appends it to [dir] *)
val concat_file : string -> string -> string

