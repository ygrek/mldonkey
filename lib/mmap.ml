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

type t = {
    mutable fd : Unix.file_descr;
    mutable addr: int option;
    mutable filename : string;
    mutable len : int32;
    mutable pos : int32;
  }

external mmap_open_file : t -> unit = "mmap_mmap"
external truncate : t -> int32 -> unit = "mmap_truncate"
external blit_from_string : string -> int32 -> t -> int32 -> int32 -> unit = "mmap_blit_from_string"
external blit_to_string :  t -> int32 -> string -> int32 -> int32 -> unit = "mmap_blit_to_string"
external munmap : t -> unit = "mmap_munmap"
external mmap_md4_sub : t -> int32 -> int32 -> Md4.t -> unit = "mmap_md4_sub"
external msync : t -> unit = "mmap_msync"
  
let mmap filename fd pos len =
  let t = { fd = fd; 
      addr = None; 
      filename = filename; 
      len = len;
      pos = pos;
      } in
  mmap_open_file t;
  t
  
let md4_sub t pos len =
  let md4 = Md4.create () in
  mmap_md4_sub t pos len md4;
  md4
