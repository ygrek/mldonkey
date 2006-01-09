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

val verbose : bool ref

val external_start : string -> unit
val external_exit : unit -> unit
val uname : unit -> string

(* CryptoPP *)
val create_key : unit -> string
val load_key : string -> string
val create_signature : string -> int -> int64 -> int -> int64 -> string
val verify_signature : string -> int -> string -> int -> int64 -> int -> int64 -> bool

val create_dir_mask : string ref
val close : t -> unit
(* val force_fd : t -> Unix.file_descr *)
  
(* val seek64 : t -> int64 -> Unix.seek_command -> int64  *)
val getsize : string -> int64
val getsize64 : t -> int64
(* size, sparse flag *)
val ftruncate64 : t -> int64 -> bool -> unit

val close_all : unit -> unit

val fds_size : int
val filename : t -> string
val rename : t -> string -> unit 
val max_cache_size : int ref
val mtime : string -> float
val mtime64 : t -> float
  
val flush : unit -> unit
val flush_fd : t -> unit
val buffered_write : t -> int64 -> string -> int -> int -> unit
val buffered_write_copy : t -> int64 -> string -> int -> int -> unit
val write : t -> int64 -> string -> int -> int -> unit
val max_buffered : int64 ref
val remove : t -> unit
  
val read : t -> int64 -> string -> int -> int -> unit
(*val allocate_chunk :  t -> int64 -> int -> unit*)
  
val copy_chunk : t -> t -> int64 -> int64 -> int -> unit
    
val file_exists : string -> bool

val apply_on_chunk : t -> int64 -> int64 -> 
  (Unix.file_descr -> int64 -> 'a) -> 'a
  

val create_diskfile : string -> bool -> t
val create_ro : string -> t
val create_rw : string -> t
  
(* 
[create_multifile dirname writable files]: create a directory
[dirname] containing the files in [files]. [files] is a list of tuples
[(filename, size)] where [filename] is relative inside the directory
[dirname] and [size] is the size of the files. 
  *)
  
val create_multifile :     
  string -> bool -> (string * int64) list -> t

val create_sparsefile :  string -> bool -> t

val ro_flag :  Unix.open_flag list
val rw_flag :  Unix.open_flag list
  
val destroy : t -> unit
  
val dummy : t

val destroyed : t -> bool

val bsize : string -> int64 option
val blocks : string -> int64 option
val bfree : string -> int64 option
val bavail : string -> int64 option
val fnamelen : string -> int64 option
val disktotal : string -> int64 option
val diskfree : string -> int64 option
val diskused : string -> int64 option
val percentused : string -> int option
val percentfree : string -> int option
val filesystem : string -> string
