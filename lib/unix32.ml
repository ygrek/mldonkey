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
    mutable fd : Unix.file_descr option;
    mutable access : int;
    mutable rights : Unix.open_flag list;
    filename : string;
  }

external seek32 : Unix.file_descr -> int32 -> Unix.seek_command -> int32 =
  "ml_lseek32"
external getsize32 : string -> int32 = "ml_getsize32"
external ftruncate32 : Unix.file_descr -> int32 -> unit = "ml_truncate32"
external getdtablesize : unit -> int = "ml_getdtablesize"

let fd_size = getdtablesize ()
  
let _ =
  Printf.printf "Your system supports %d file descriptors" fd_size;
  print_newline () 
  
let max_cache_size = 50
let cache_size = ref 0
let cache = Fifo.create ()
  
let create f r a = {
    filename = f;
    fd = None;
    rights = r;
    access = a;
  }

let rec close_one () =
  if not (Fifo.empty cache) then
    let t = Fifo.take cache in
    match t.fd with
      None -> 
        close_one ()
    | Some fd -> Unix.close fd; t.fd <- None
        
let force_fd t =
  match t.fd with
    None ->
      if !cache_size = max_cache_size then
        close_one ()
      else incr cache_size;
      let fd = Unix.openfile t.filename t.rights t.access in
      Fifo.put cache t;
      t.fd <- Some fd;
      fd
  | Some fd -> fd
  
let seek32 t pos com =
  seek32 (force_fd t) pos com
  
let ftruncate32 t len =
  ftruncate32 (force_fd t) len
  
let close t =
  match t.fd with
  | Some fd -> Unix.close fd; decr cache_size
  | None -> ()
      