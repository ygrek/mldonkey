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

let (++) = Int64.add
let (--) = Int64.sub
let chunk_min_size = ref (Int64.of_int 65000)

let verbose = false
let max_cache_size = ref 50

module FDCache = struct
    
    type t = {
        mutable fd : Unix.file_descr option;
        mutable access : int;
        mutable rights : Unix.open_flag list;
        mutable filename : string;
      }
    
    let cache_size = ref 0
    let cache = Fifo.create ()
    
    let create f r a = {
        filename = f;
        fd = None;
        rights = r;
        access = a;
      }
    
    let close t =
      match t.fd with
      | Some fd -> 
(*          lprintf "close_one: closing %d\n" (Obj.magic fd); *)
          (try Unix.close fd with _ -> ());
          t.fd <- None;
          decr cache_size
      | None -> ()
    
    
    let rec close_one () =
      if not (Fifo.empty cache) then
        let t = Fifo.take cache in
        match t.fd with
          None -> 
            close_one ()
        | Some fd -> 
            close t
    
    
    let local_force_fd t =
      let fd = 
        match t.fd with
          None ->
            if !cache_size >= !max_cache_size then
              close_one ()
            else incr cache_size;
            let fd = Unix.openfile t.filename t.rights t.access in
(*            lprintf "local_force: opening %d\n" (Obj.magic fd); *)
            Fifo.put cache t;
            t.fd <- Some fd;
            fd
        | Some fd -> fd
      in
(*      lprintf "local_force_fd %d\n" (Obj.magic fd); *)
      fd
    
    
    let close_all () =
      while not (Fifo.empty cache) do
        close_one ()
      done
    
    let rename t f = 
      close t;
      Unix2.rename t.filename f;  
      t.filename <- f
    
    let ftruncate64 t len =
      Unix2.c_ftruncate64 (local_force_fd t) len
    
    let getsize64 t = Unix2.c_getsize64 t.filename
    
    
    let mtime64 t =
      let st = Unix.LargeFile.stat t.filename in
      st.Unix.LargeFile.st_mtime

    let exists t = Sys.file_exists t.filename

    let remove t = Sys.remove t.filename
  end

module type File =   sig
    type t
    val create : string -> Unix.open_flag list -> int -> t
    val fd_of_chunk : t -> int64 -> int64 -> Unix.file_descr * int64
    val close : t -> unit
    val rename : t -> string -> unit
    val ftruncate64 : t -> int64 -> unit
    val getsize64 : t -> int64
    val mtime64 : t -> float
    val exists : t -> bool
    val remove : t -> unit
  end

  
module DiskFile : File = struct
    
    type t = FDCache.t 
    
    let create = FDCache.create
    
    let fd_of_chunk t pos_s len_s =
      let fd = FDCache.local_force_fd t in
      fd, pos_s
    
    let close = FDCache.close

    let rename = FDCache.rename
      
    let ftruncate64 = FDCache.ftruncate64

    let getsize64 = FDCache.getsize64
    let mtime64 = FDCache.mtime64
    let exists = FDCache.exists
    let remove = FDCache.remove
  end

    
module SparseFile : File = struct
    
    type chunk = {
        mutable dirname : string;
        mutable pos : int64;
        mutable len : int64;
        mutable created : bool;
        mutable fd : FDCache.t;
        mutable next : chunk option;
      }
    
    type t = {
        mutable filename : string;
        mutable size : int64;
        mutable chunks : chunk;
      }
    
    let rights = 0o664
    let access = [Unix.O_CREAT; Unix.O_RDWR]
    
    let create filename rights access = 
      let dirname = Printf.sprintf "%s.chunks" filename in
      Unix.mkdir dirname  0o755;
      {
        filename = filename;
        size = Int64.zero;
        chunks = {
          dirname = dirname;
          pos = Int64.zero; 
          len = Int64.zero; 
          created = false;
          fd = FDCache.create filename rights access;
          next = None;
        }; 
      }
    
    let (++) = Int64.add
    let (--) = Int64.sub
    
    let append_chunk dest src = 
      failwith "SparseFile.append_chunk not implemented"
    let remove_chunk c = 
      failwith "SparseFile.remove_chunk not implemented"
    
    let ftruncate_chunk c size = 
      FDCache.ftruncate64 c.fd size;
      c.created <- true
    let open_chunk c = FDCache.local_force_fd c.fd
    
    let split_chunk c pos =
      let cc = {
          dirname = c.dirname;
          fd = FDCache.create (Filename.concat c.dirname 
              (Int64.to_string pos)) access rights;
          pos = pos;
          len = c.len -- pos;
          next = c.next;
          created = false;
        } in
      c.next <- Some cc;
      c.len <- (pos -- c.pos)
    
    let rec extend_chunk c size = 
      match c.next with
        None -> 
          lprintf "Cannot extend last chunk\n";
          assert false
      | Some cc ->
          if cc.created then
            begin
              append_chunk c cc;
              remove_chunk cc;
              c.next <- cc.next;
              c.len <- (cc.pos ++ cc.len) -- c.pos;
            end
          else
          if cc.len >= size ++ !chunk_min_size then 
            begin
              split_chunk cc (cc.pos ++ size);
              extend_chunk c size;
            end 
          else
            begin
              c.next <- cc.next;
              c.len <- (cc.pos ++ cc.len) -- c.pos;
              ftruncate_chunk c (c.len ++ cc.len);
            end
    
    
    let get_chunk t pos len = 
      let c = t.chunks in
      let rec iter c =
        if c.created && c.pos <= pos && c.pos ++ c.len >= pos ++ len then
          (open_chunk c, pos -- c.pos)
        else
        if c.created && pos <= c.pos ++ c.len ++ !chunk_min_size then
          begin
            extend_chunk c (max !chunk_min_size 
              (pos ++ len -- c.pos -- c.len ++ !chunk_min_size));
            iter c
          end
        else
        if c.pos ++ c.len <= pos then
          match c.next with
          | None -> 
              lprintf "Invalid access in file pos %Ld is after last chunk\n" pos;
              assert false
          | Some c -> iter c
        else
        if c.pos ++ !chunk_min_size < pos then 
          begin
            split_chunk c pos;
            iter c     
          end
        else
        if c.pos ++ c.len > pos ++ (max len !chunk_min_size) ++ !chunk_min_size then
          begin
            split_chunk c (pos ++ (max len !chunk_min_size)++ !chunk_min_size);
            iter c
          end
        else 
          begin
            ftruncate_chunk c c.len;
            iter c
          end
      in
      iter c
    
    let build t =
      let c = t.chunks in
      if not c.created then ftruncate_chunk c c.len;
      while c.next <> None do
        extend_chunk c t.size
      done;
      t
    
    
    let fd_of_chunk = get_chunk 
    
    let close t = 
      failwith "SparseFile.close not implemented"
    
    let rename t f =
      failwith "SparseFile.rename not implemented"
    
    let ftruncate64 t size = 
      t.size <- size;
      t.chunks.len <- size;
      assert (t.chunks.next = None)
    
    let getsize64 t =
      failwith "SparseFile.getsize64 not implemented"
    
    let mtime64 t =
      failwith "SparseFile.mtime64 not implemented"
    
    let exists t = 
      failwith "SparseFile.exists not implemented"
    
    let remove t = 
      failwith "SparseFile.remove not implemented"
  
  
  end
  
  
type file_kind = 
(*  SparseFile of SparseFile.t *)
| DiskFile of DiskFile.t  
| SparseFile of SparseFile.t
  
type t = {
    file_kind : file_kind;
    mutable filename : string;
    mutable error : exn option;
    mutable buffers : (string * int * int * int64 * int64) list;    
  }

module H = Weak2.Make(struct
      type old_t = t
      type t = old_t
      let hash t = Hashtbl.hash t.filename
      
      let equal x y  = x.filename = y.filename
    end)

let dummy =  {
    file_kind = DiskFile (DiskFile.create "" [] 0);
    filename = "";
    error = None;
    buffers = [];
  } 

let table = H.create 100

let create f r a = 
  try
    H.find table { dummy with filename = f }
  with _ ->
      let t =  {
          file_kind = DiskFile (DiskFile.create f r a);
          filename = f;
          error = None;
          buffers = [];
        } in
      H.add table t;
      t

let fd_of_chunk t pos len = 
  match t.file_kind with
  | DiskFile t -> DiskFile.fd_of_chunk t pos len
  | SparseFile t -> SparseFile.fd_of_chunk t pos len

let ftruncate64 t len =
  match t.file_kind with
  | DiskFile t -> DiskFile.ftruncate64 t len
  | SparseFile t -> SparseFile.ftruncate64 t len

let mtime64 t =
  match t.file_kind with
  | DiskFile t -> DiskFile.mtime64 t 
  | SparseFile t -> SparseFile.mtime64 t 

let getsize64 t =
  match t.file_kind with
  | DiskFile t -> DiskFile.getsize64 t 
  | SparseFile t -> SparseFile.getsize64 t 

(*
For chunked files, we should read read the meta-file instead of the file.
*)

let fds_size = Unix2.c_getdtablesize ()

let buffered_bytes = ref Int64.zero
let modified_files = ref []

let _ =
  lprintf "Your system supports %d file descriptors" fds_size;
  lprint_newline () 

(* at most 50 files can be opened simultaneously *)

(*
 let seek64 t pos com =
  Unix2.c_seek64 (force_fd t) pos com
  
  
*)


let filename t = t.filename



let buffer = Buffer.create 65000


let flush_buffer t offset = 
  if verbose then lprintf "flush_buffer\n";
  let s = Buffer.contents buffer in
  Buffer.clear buffer;
  let len = String.length s in
  try
    if verbose then lprintf "seek64 %Ld\n" offset;
    let fd, offset =  fd_of_chunk t offset (Int64.of_int len) in
    let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
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
  try
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
  with e ->
      lprintf "[ERROR] Exception %s in Unix32.flush\n"
        (Printexc2.to_string e)
      
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
      raise e

let write t offset s pos_s len_s =  
  let fd, offset = fd_of_chunk t offset (Int64.of_int len_s) in
  let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
  if final_pos <> offset then begin
      lprintf "BAD LSEEK in Unix32.write %Ld/%Ld\n" final_pos offset;
      raise Not_found
    end;

(* if !verbose then begin
      lprintf "{%d-%d = %Ld-%Ld}\n" (t.Q.bloc_begin)
      (t.Q.bloc_len) (begin_pos) 
      (end_pos);
    end; *)
  Unix2.really_write fd s pos_s len_s

let read t offset s pos_s len_s =
  flush_fd t;
  let fd, offset = fd_of_chunk t offset (Int64.of_int len_s) in
  let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
  
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
  let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
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
  done

let copy_chunk t1 t2 pos1 pos2 len =
(* Close two file descriptors *)
  assert (!max_cache_size > 2);
  flush_fd t1;
  flush_fd t2;
  FDCache.close_one ();
  FDCache.close_one ();
(*  lprintf "Copying chunk\n"; *)
  let file_in,pos1 = fd_of_chunk t1 pos1 len in
  let file_out,pos2 = fd_of_chunk t2 pos2 len in
  let pos1_after = Unix2.c_seek64 file_in pos1 Unix.SEEK_SET in
  let pos2_after = Unix2.c_seek64 file_out pos2 Unix.SEEK_SET in
(*  lprintf "Seek %Ld/%Ld %Ld/%Ld\n" pos1_after pos1 pos2_after pos2; *)
  let buffer_len = 32768 in
  let len = Int64.to_int len in
  let buffer = String.create buffer_len in
  let rec iter file_in file_out len =
    if len > 0 then
      let can_read = mini buffer_len len in
      let nread = Unix.read file_in buffer 0 buffer_len in
(*      lprintf "Unix2.really_write %d\n" nread; *)
      Unix2.really_write file_out buffer 0 nread;
      iter file_in file_out (len-nread)
  in
  iter file_in file_out len;
(*  lprintf "Chunk copied\n"; *)
  ()

let close_all = FDCache.close_all
let close t = 
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.close t
  | SparseFile t -> SparseFile.close t

let create_ro filename = create filename [Unix.O_RDONLY] 0o666

let exists t = 
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.exists t
  | SparseFile t -> SparseFile.exists t

let remove t = 
  close t;
  match t.file_kind with
  | DiskFile t -> DiskFile.remove t
  | SparseFile t -> SparseFile.remove t

let getsize64 s =  getsize64 (create_ro s)
let mtime64 s =  mtime64 (create_ro s)

let file_exists s = exists (create_ro s)

let rename t f = 
  close t;
  t.filename <- f;
  match t.file_kind with
  | DiskFile t -> DiskFile.rename t f
  | SparseFile t -> SparseFile.rename t f
      