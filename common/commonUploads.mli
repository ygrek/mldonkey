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

type shared_file = {
    shared_fullname : string;
    shared_codedname : string;
    shared_size : int64;
    shared_fd : Unix32.t;
    shared_id : int;
    shared_format : CommonTypes.format;
    shared_impl : shared_file CommonShared.shared_impl;
} 
and shared_tree = {
  shared_dirname : string;
  mutable shared_files : shared_file list;
  mutable shared_dirs : (string * shared_tree) list;
} 
and upload = {
  upload_file : CommonTypes.file;
  upload_client : CommonTypes.client;
  mutable upload_pos : int64;
  mutable upload_end : int64;
  mutable upload_sock : TcpBufferedSocket.t option;
  mutable upload_on_close : upload -> unit;
  upload_subdir : string Options.option_record;
  upload_on_finish : upload -> unit;
} 

val new_shared_dir : string -> shared_tree
val shared_tree : shared_tree
val add_shared_file : shared_tree -> shared_file -> string list -> unit
val add_shared : string -> string -> int64 -> unit
val query: CommonTypes.query -> shared_file array
val find_by_name : string -> shared_file
val find_by_num : int -> shared_file
  
val filesize_field : string