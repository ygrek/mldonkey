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
    mutable filename : string;
  }

external seek64 : Unix.file_descr -> int64 -> Unix.seek_command -> int64 =
  "ml_lseek64" 
external getsize64 : string -> int64 = "ml_getsize64"
external ftruncate64 : Unix.file_descr -> int64 -> unit = "ml_truncate64"
external getdtablesize : unit -> int = "ml_getdtablesize"

let fds_size = getdtablesize ()
  
let _ =
  Printf.printf "Your system supports %d file descriptors" fds_size;
  print_newline () 

(* at most 50 files can be opened simultaneously *)
  
let max_cache_size = ref 50
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
    | Some fd -> 
        (try Unix.close fd with _ -> ()); 
        t.fd <- None
        
let force_fd t =
  match t.fd with
    None ->
      if !cache_size >= !max_cache_size then
        close_one ()
      else incr cache_size;
      let fd = Unix.openfile t.filename t.rights t.access in
      Fifo.put cache t;
      t.fd <- Some fd;
      fd
  | Some fd -> fd
  
let seek64 t pos com =
  seek64 (force_fd t) pos com
  
let ftruncate64 t len =
  ftruncate64 (force_fd t) len
  
let close t =
  match t.fd with
  | Some fd -> 
      (try Unix.close fd with _ -> ());
      t.fd <- None;
      decr cache_size
  | None -> ()
      
let close_all () =
  while not (Fifo.empty cache) do
    close_one ()
  done
  
let filename t = t.filename
      
let set_filename t f = 
  t.filename <- f;
  close t

let mtime64 filename =
  let st = Unix.LargeFile.stat filename in
  st.Unix.LargeFile.st_mtime
  
  