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

open Printf2

let verbose = false
  
type t = {
    mutable fd : Unix.file_descr option;
    mutable access : int;
    mutable rights : Unix.open_flag list;
    mutable filename : string;
    mutable error : exn option;
    mutable buffers : (string * int * int * int64 * int64) list;
  }

(*
For chunked files, we should read read the meta-file instead of the file.
*)
let getsize64 = Unix2.c_getsize64
  
let fds_size = Unix2.c_getdtablesize ()

let buffered_bytes = ref Int64.zero
let modified_files = ref []
  
let _ =
  lprintf "Your system supports %d file descriptors" fds_size;
  lprint_newline () 

(* at most 50 files can be opened simultaneously *)
  
let max_cache_size = ref 50
let cache_size = ref 0
let cache = Fifo.create ()
  
let create f r a = {
    filename = f;
    fd = None;
    rights = r;
    access = a;
    error = None;
    buffers = [];
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
  Unix2.c_seek64 (force_fd t) pos com
  
let ftruncate64 t len =
  Unix2.c_ftruncate64 (force_fd t) len
  
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

let buffer = Buffer.create 65000

let (++) = Int64.add
let (--) = Int64.sub

let flush_buffer t offset = 
  if verbose then lprintf "flush_buffer\n";
  let s = Buffer.contents buffer in
  Buffer.clear buffer;
  let len = String.length s in
  try
    if verbose then lprintf "seek64 %Ld\n" offset;
    let final_pos = seek64 t offset Unix.SEEK_SET in
    let fd =  force_fd t in
    if verbose then lprintf "really_write %d\n" len;
    Unix2.really_write fd s 0 len;
    buffered_bytes := !buffered_bytes -- (Int64.of_int len);
    if verbose then lprintf "written %d bytes (%Ld)\n" len !buffered_bytes;
  with e ->
      lprintf "exception %s in flush_buffer\n" (Printexc2.to_string e);
      t.buffers <- (s, 0, len, offset, Int64.of_int len) :: t.buffers;
      raise e
  
let flush_fd t = 
  if t.buffers = [] then () else
  let list = 
    List.sort (fun (_, _, _, o1, l1) (_, _, _, o2, l2) ->
        let c = compare o1 o2 in
        if c = 0 then compare l2 l1 else c) 
    t.buffers
  in
  if verbose then lprintf "flush_fd\n";
  t.buffers <- list;
  let rec iter_out () =
    match t.buffers with
      [] -> ()
    | (s, pos_s, len_s, offset, len) :: tail ->
        Buffer.clear buffer;
        Buffer.add_substring buffer s pos_s len_s;
        t.buffers <- tail;
        iter_in offset len
        
  and iter_in offset len =
    match t.buffers with
      [] -> flush_buffer t offset
    | (s, pos_s, len_s, offset2, len2) :: tail ->
        let in_offset = offset ++ len -- offset2 in
        if in_offset = Int64.zero then begin
            Buffer.add_substring buffer s pos_s len_s;
            t.buffers <- tail;
            iter_in offset (len ++ len2);
          end else
        if in_offset < Int64.zero then begin
            flush_buffer t offset;
            iter_out ()
          end else 
        let keep_len = len2 -- in_offset in
        if verbose then lprintf "overlap %Ld\n" keep_len;
        t.buffers <- tail;
        if keep_len <= Int64.zero then begin
            buffered_bytes := !buffered_bytes -- len2;                
            iter_in offset len
          end else begin
            let new_pos = len2 -- keep_len in
            Buffer.add_substring buffer s 
              (pos_s + Int64.to_int new_pos) (Int64.to_int keep_len);
            buffered_bytes := !buffered_bytes -- new_pos;
            iter_in offset (len ++ keep_len)
          end
  in
  iter_out ()

let max_buffered = ref (Int64.of_int (1024*1024))
      
let flush _ = 
  if verbose then lprintf "flush all\n";
  let rec iter list =
    match list with
      [] -> []
    | t :: tail ->
        try
          flush_fd t;
          t.error <- None;
          iter tail
        with e -> 
            t.error <- Some e;
            t :: (iter tail)
  in
  modified_files := iter !modified_files;
  if !buffered_bytes <> Int64.zero then
    lprintf "[ERROR] remaining bytes after flush\n"
  
let buffered_write t offset s pos_s len_s = 
  let len = Int64.of_int len_s in
  match t.error with
    None -> 
      if len > Int64.zero then begin
          if not (List.memq t !modified_files) then
            modified_files := t:: !modified_files;
          t.buffers <- (s, pos_s, len_s, offset, len) :: t.buffers;
          buffered_bytes := !buffered_bytes ++ len;
          if verbose then 
            lprintf "buffering %Ld bytes (%Ld)\n" len !buffered_bytes;

(* Don't buffer more than 1 Mo *)
          if !buffered_bytes > !max_buffered then flush ()
        end
  | Some e -> 
      t.error <- None; 
      raise e

let write t offset s pos_s len_s =  
  let final_pos = seek64 t offset Unix.SEEK_SET in
  if final_pos <> offset then begin
      lprintf "BAD LSEEK in Unix32.write %Ld/%Ld\n" final_pos offset;
      raise Not_found
    end;

(* if !verbose then begin
      lprintf "{%d-%d = %Ld-%Ld}\n" (t.Q.bloc_begin)
      (t.Q.bloc_len) (begin_pos) 
      (end_pos);
    end; *)
  let fd = try force_fd t with e -> 
        lprintf "In force_fd\n"; 
        raise e
  in
  Unix2.really_write fd s pos_s len_s
  
let fd_of_chunk t pos_s len_s =
  let fd = force_fd t in
  fd, pos_s
  
let read t offset s pos_s len_s =
  let fd, offset = fd_of_chunk t offset (Int64.of_int len_s) in
  let final_pos = seek64 t offset Unix.SEEK_SET in
  
  if final_pos <> offset then begin
      lprintf "BAD LSEEK in Unix32.read %Ld/%Ld\n" final_pos offset;
      raise Not_found
    end;
  Unix2.really_read fd s pos_s len_s

let mini (x: int) (y: int) =
  if x > y then y else x

(* zero_chunk is only useful on sparse file systems, not on Windows 
  for example *)
    
let allocate_chunk t offset len64 =
  let len = Int64.to_int len64 in
  let fd, offset = fd_of_chunk t offset len64 in
  let final_pos = seek64 t offset Unix.SEEK_SET in
  if final_pos <> offset then begin
      lprintf "BAD LSEEK in Unix32.zero_chunk %Ld/%Ld\n"
        final_pos offset; 
      raise Not_found
    end;
  let buffer_size = 128 * 1024 in
  let buffer = String.make buffer_size '\001' in
  let remaining = ref len in
  while !remaining > 0 do
    let len = mini !remaining buffer_size in
    Unix2.really_write fd buffer 0 len;
    remaining := !remaining - len;
  done;
  