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

open Int64
open Printf2

let (++) = Int64.add
let (--) = Int64.sub
let chunk_min_size = ref (Int64.of_int 65000)

let max_buffered = ref (Int64.of_int (1024*1024))

let verbose = false
let max_cache_size = ref 50

let mini (x: int) (y: int) =
  if x > y then y else x

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
            if !cache_size >= !max_cache_size then  close_one ();
            let fd = Unix.openfile t.filename t.rights t.access in
            incr cache_size;
(*            lprintf "local_force: opening %d\n" (Obj.magic fd);  *)
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
    
    let read file file_pos string string_pos len =
      let fd = local_force_fd file in
      let final_pos = Unix2.c_seek64 fd file_pos Unix.SEEK_SET in
      if verbose then lprintf "really_read %d\n" len;
      Unix2.really_read fd string string_pos len
    
    let write file file_pos string string_pos len =
      let fd = local_force_fd file in
      let final_pos = Unix2.c_seek64 fd file_pos Unix.SEEK_SET in
      if verbose then lprintf "really_write %d\n" len;
      Unix2.really_write fd string string_pos len
    
    
    let copy_chunk t1 t2 pos1 pos2 len =
      let buffer_size = 128 * 1024 in
      let buffer = String.make buffer_size '\001' in
      let rec iter remaining pos1 pos2 =
        let len = mini remaining buffer_size in
        if len > 0 then begin
            read t1 pos1 buffer 0 len;
            write t2 pos2 buffer 0 len;
            let len64 = Int64.of_int len in
            iter (remaining - len) (pos1 ++ len64) (pos2 ++ len64)
          end
      in
      iter len pos1 pos2
      
  end

module type File =   sig
    type t
    val create : string -> Unix.open_flag list -> int -> t
    val fd_of_chunk : t -> int64 -> int64 -> 
      Unix.file_descr * int64 * string option
    val close : t -> unit
    val rename : t -> string -> unit
    val ftruncate64 : t -> int64 -> unit
    val getsize64 : t -> int64
    val mtime64 : t -> float
    val exists : t -> bool
    val remove : t -> unit
    val read : t -> int64 -> string -> int -> int -> unit
    val write : t -> int64 -> string -> int -> int -> unit
  end

  
module DiskFile = struct
    
    type t = FDCache.t 
    
    let create = FDCache.create
    
    let fd_of_chunk t pos_s len_s =
      let fd = FDCache.local_force_fd t in
      fd, pos_s, None
    
    let close = FDCache.close

    let rename = FDCache.rename
      
    let ftruncate64 = FDCache.ftruncate64

    let getsize64 = FDCache.getsize64
    let mtime64 = FDCache.mtime64
    let exists = FDCache.exists
    let remove = FDCache.remove
    let read = FDCache.read
    let write = FDCache.write
  end


  
module SparseFile = struct
    
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
          (open_chunk c, pos -- c.pos, None)
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
    
    
    let fd_of_chunk t pos len = get_chunk t pos len
    
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
  
    let read file file_pos string string_pos len = ()

    let write file file_pos string string_pos len = ()

  
  end
  
module MultiFile = struct
    
    type file = {
        mutable filename : string;
        mutable pos : int64;
        mutable len : int64;
        mutable fd : FDCache.t;
        mutable tail : file list;
      }
    
    type t = {
        mutable dirname : string;
        mutable size : int64;
        mutable files : file list;
        mutable tree : btree;
      }
    
    and btree = 
      Node of int64 * btree * btree
    | Leaf of file
    
    let rec make_tree files =
      match files with
        [||] -> assert false
      | [| file |] -> Leaf file
      | _ ->
          let len = Array.length files in
          let middle = len / 2 in
          let pos = files.(middle).pos in
          Node (pos, 
            make_tree (Array.sub files 0 middle),
            make_tree (Array.sub files middle (len - middle)))
    
    let make_tree files =
      make_tree (Array.of_list (
          List.filter (fun file -> file.len > zero) files))
    
    let rec find_file tree file_pos =
      match tree with
        Leaf file -> file
      | Node (pos, tree1, tree2) ->
          find_file
            (if file_pos < pos then tree1 else tree2) file_pos
    
    let rec print_tree indent tree =
      match tree with
        Leaf file -> lprintf "%s  - %s (%Ld,%Ld)\n" 
            indent file.filename file.pos file.len
      | Node (pos, tree1, tree2) ->
          lprintf "%scut at %Ld\n" indent pos;
          print_tree (indent ^ "  ") tree1;
          print_tree (indent ^ "  ") tree2
    
    let rights = 0o664
    let access = [Unix.O_CREAT; Unix.O_RDWR]
    
    let create dirname rights access files = 
      Unix2.safe_mkdir dirname;
      let rec iter files pos files2 = 
        match files with
          [] -> 
            let rec reverse_files files tail =
              match files with
                [] -> tail
              | file :: others ->
                  file.tail <- tail;
                  reverse_files others (file :: tail)
            in
            let files = reverse_files files2 [] in
            {
              dirname = dirname;
              size = pos;
              files = List.rev files2;
              tree = make_tree files;
            }
        | (filename, size) :: tail ->
            Unix2.safe_mkdir (Filename.dirname filename);
            iter tail (pos ++ size)
            ({ 
                filename = filename;
                pos = pos;
                len = size;
                fd = FDCache.create (Filename.concat dirname filename)
                rights access;
                tail = [];
              } :: files2)
      
      in
      iter files zero []
    
    let build t = ()

(*
TODO:
  We should probably use another structure (b-tree) to easily find the file
  corresponding to a particular position in the global file.
*)

      (*
    let find_file t chunk_begin =
      let rec iter list =
        match list with
          [] -> assert false
        | file :: tail ->
            let file_begin = file.pos in
            let file_end = file_begin ++ file.len in
            if file_end > chunk_begin then 
              (file, tail)
            else
              iter tail
      in
      iter t.files
*)
      
    let find_file t chunk_begin =
      let file = find_file t.tree chunk_begin in
      file, file.tail
    
    let do_on_remaining tail arg remaining f =
      
      let rec iter_files list arg remaining =
        match list with
          [] -> assert false
        | file :: tail ->
            
            let len = min remaining file.len in
            let arg = f file arg len in            
            let remaining = remaining -- len in
            if remaining > zero then
              iter_files tail arg remaining
      
      in
      iter_files tail arg remaining
    
    let fd_of_chunk t chunk_begin chunk_len = 
      let (file, tail) = find_file t chunk_begin in
      let chunk_end = chunk_begin ++ chunk_len in
      let file_begin = file.pos in
      let file_end = file_begin ++ file.len in
      if file_end > chunk_end then
        let fd = FDCache.local_force_fd file.fd in
        fd, chunk_begin -- file_begin, None
      else
      let temp_file = Filename.temp_file "chunk" ".tmp" in
      let file_out = FDCache.create temp_file access rights in

(* first file *)
      let in_pos = chunk_begin -- file_begin in
      let in_len = file_end -- in_pos in
      FDCache.copy_chunk file.fd file_out zero 
        in_pos (Int64.to_int in_len);
(* other files *)
      do_on_remaining tail in_len  (chunk_len -- in_len)
      (fun file file_pos len ->          
          FDCache.copy_chunk file.fd file_out
            zero file_pos (Int64.to_int len);
          file_pos ++ len
      );
      FDCache.close file_out;
      
      let fd = FDCache.local_force_fd file_out in
      fd, zero, Some temp_file
    
    
    let close t = 
      List.iter (fun file -> FDCache.close file.fd) t.files
    
    let rename t f =
      close t;
      Sys.rename t.dirname f;
      List.iter (fun file ->
          file.fd.FDCache.filename <- Filename.concat t.dirname file.filename
      ) t.files 
    
    let ftruncate64 t size = 
      t.size <- size
    
    let getsize64 t = t.size 
    
    let mtime64 t =
      let st = Unix.LargeFile.stat t.dirname in
      st.Unix.LargeFile.st_mtime
    
    let exists t = 
      Sys.file_exists t.dirname
    
    let remove t = 
      close t;
      Unix2.remove_all_directory t.dirname
    
    let read t chunk_begin string string_pos len = 
      let (file, tail) = find_file t chunk_begin in
      
      let chunk_len = Int64.of_int len in
      let chunk_end = chunk_begin ++ chunk_len in
      let file_begin = file.pos in
      let file_end = file_begin ++ file.len in
      if file_end > chunk_end then
        FDCache.read file.fd (chunk_begin -- file_begin)
        string string_pos len
      else

(* first file *)
      let in_pos = chunk_begin -- file_begin in
      let first_write = file_end -- in_pos in
      let in_len = Int64.to_int first_write in
      FDCache.read file.fd in_pos string 0 in_len;
(* other files *)
      do_on_remaining tail in_len  (chunk_len -- first_write)
      (fun file string_pos len ->          
          let len = Int64.to_int len in
          FDCache.read file.fd zero string string_pos len;
          string_pos + len
      )
    
    let write t chunk_begin string string_pos len = 
      begin
        let (file, tail) = find_file t chunk_begin in
        
        let chunk_len = Int64.of_int len in
        let chunk_end = chunk_begin ++ chunk_len in
        let file_begin = file.pos in
        let file_end = file_begin ++ file.len in
        if file_end > chunk_end then
          FDCache.write file.fd (chunk_begin -- file_begin)
          string string_pos len
        else

(* first file *)
        let in_pos = chunk_begin -- file_begin in
        let first_write = file_end -- in_pos in
        let in_len = Int64.to_int first_write in
        FDCache.write file.fd in_pos string 0 in_len;
(* other files *)
        do_on_remaining tail in_len  (chunk_len -- first_write)
        (fun file string_pos len ->          
            let len = Int64.to_int len in
            FDCache.write file.fd zero string string_pos len;
            string_pos + len
        );
      
      end;        
      let time = Unix.time () in
      Unix.utimes t.dirname time time;
      ()

            
      
    (*
      
    type node =
      NTree of int64 * node array
    | NNode of file
    
    let rec make_tree files offset file_size =  
      match files with
        [file] -> NNode file
      | _ ->
          let array = Array.create 11 [] in
          let divisor = file_size // (Int64.of_int 10) in
          List.iter (fun file ->
              let n = 
                if file.len = zero then 0 else
                  Int64.to_int ( (file.pos -- offset) // divisor) 
              in
              array.(n) <- file :: array.(n);
          ) files;
          let all_empty = ref true in
          for i = 1 to 10 do
            all_empty := !all_empty && array.(i) = []
          done;
          if !all_empty then
            make_tree files offset divisor
          else
            NTree (divisor, 
              Array.mapi (fun i files ->
                  make_tree (List.rev files) 
                  (offset ++ divisor ** (Int64.of_int i)) divisor
              ) array)
    
    let rec find_file tree offset file_pos =
      match tree with
        NNode file -> file
      | NTree (divisor, files) ->
          let nn =  (file_pos -- offset) // divisor in
          let n = Int64.to_int nn in
          find_file (files.(n)) (offset ++ divisor ** nn) file_pos
*)

          (*
    let _ =
      let files = ref [] in
      let size = ref 0 in
      for i = 0 to 20 do
        let len = (Random.int 5)+1 in
        files := (Printf.sprintf "file_%d" i,
          Int64.of_int len) :: !files;
        size := len + !size
      done;
      
      List.iter (fun (filename, size) ->
          lprintf "%s : %Ld\n" filename size;
      ) !files;

      let t = create "test" access rights !files in
      
      let tree = make_tree t.files in
      print_tree "" tree;      
      for i = 0 to !size do
        let file = find_file tree (Int64.of_int i) in
        lprintf "%d is in %s\n" i file.filename
      done;
      
      exit 2
*)
          
  end
  
  
type file_kind = 
  MultiFile of MultiFile.t
| DiskFile of DiskFile.t  
| SparseFile of SparseFile.t
  
type t = {
    file_kind : file_kind;
    file_rights : Unix.open_flag list;
    mutable filename : string;
    mutable error : exn option;
    mutable buffers : (string * int * int * int64 * int64) list;    
  }

module H = Weak2.Make(struct
      type old_t = t
      type t = old_t
      let hash t = Hashtbl.hash t.filename
      
      let equal x y  = 
        x.filename = y.filename && x.file_rights = y.file_rights
    end)

let dummy =  {
    file_kind = DiskFile (DiskFile.create "" [] 0);
    filename = "";
    error = None;
    buffers = [];
    file_rights = [];
  } 

let table = H.create 100

let create f r a creator = 
  try
    H.find table { dummy with filename = f; file_rights = r }
  with _ ->
      let t =  {
          file_kind = creator f r a;
          filename = f;
          error = None;
          buffers = [];
          file_rights = r;
        } in
      H.add table t;
      t

let create_diskfile filename r a =
  create filename r a (fun f r a -> DiskFile (DiskFile.create f r a))
  
let create_multifile filename r a files =
  create filename r a (fun f r a -> MultiFile (MultiFile.create f r a files))
  
let fd_of_chunk t pos len = 
  match t.file_kind with
  | DiskFile t -> DiskFile.fd_of_chunk t pos len
  | MultiFile t -> MultiFile.fd_of_chunk t pos len
  | SparseFile t -> SparseFile.fd_of_chunk t pos len

let ftruncate64 t len =
  match t.file_kind with
  | DiskFile t -> DiskFile.ftruncate64 t len
  | MultiFile t -> MultiFile.ftruncate64 t len
  | SparseFile t -> SparseFile.ftruncate64 t len

let mtime64 t =
  match t.file_kind with
  | DiskFile t -> DiskFile.mtime64 t 
  | MultiFile t -> MultiFile.mtime64 t 
  | SparseFile t -> SparseFile.mtime64 t 

let getsize64 t =
  match t.file_kind with
  | DiskFile t -> DiskFile.getsize64 t 
  | MultiFile t -> MultiFile.getsize64 t 
  | SparseFile t -> SparseFile.getsize64 t 

(*
For chunked files, we should read read the meta-file instead of the file.
*)

let fds_size = Unix2.c_getdtablesize ()

let buffered_bytes = ref Int64.zero
let modified_files = ref []

let _ =
  lprintf "Your system supports %d file descriptors" fds_size;
  lprint_newline ();
  lprintf "You can download files up to %s\n" 
    ( match Unix2.c_sizeofoff_t () with 
	|  4 -> "2GB"
	|  _ -> Printf.sprintf "2^%d-1 bits (do the maths ;-p)" 
	     ((Unix2.c_sizeofoff_t () *8)-1)			
    );
  lprint_newline () 

(* at most 50 files can be opened simultaneously *)

(*
 let seek64 t pos com =
  Unix2.c_seek64 (force_fd t) pos com
  
  
*)


let filename t = t.filename

let write file file_pos string string_pos len =
  match file.file_kind with
  | DiskFile t -> DiskFile.write t file_pos string string_pos len
  | MultiFile t -> MultiFile.write t file_pos string string_pos len
  | SparseFile t -> SparseFile.write t file_pos string string_pos len

let buffer = Buffer.create 65000


let flush_buffer t offset = 
  if verbose then lprintf "flush_buffer\n";
  let s = Buffer.contents buffer in
  Buffer.clear buffer;
  let len = String.length s in
  try
    if verbose then lprintf "seek64 %Ld\n" offset;
    write t offset s 0 len;
(*
    let fd, offset =  fd_of_chunk t offset (Int64.of_int len) in
    let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
    if verbose then lprintf "really_write %d\n" len;
    Unix2.really_write fd s 0 len;
*)
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

let read t file_pos string string_pos len =
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.read t file_pos string string_pos len
  | MultiFile t -> MultiFile.read t file_pos string string_pos len
  | SparseFile t -> SparseFile.read t file_pos string string_pos len

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

let buffered_write_copy t offset s pos_s len_s = 
  buffered_write t offset (String.sub s pos_s len_s) 0 len_s

(*      
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
*)

(* zero_chunk is only useful on sparse file systems, not on Windows 
  for example *)

let allocate_chunk t offset len64 =
(*
  let len = Int64.to_int len64 in
  let fd, offset = fd_of_chunk t offset len64 in
  let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
  if final_pos <> offset then begin
      lprintf "BAD LSEEK in Unix32.zero_chunk %Ld/%Ld\n"
        final_pos offset; 
      raise Not_found
    end; *)
  let buffer_size = 128 * 1024 in
  let buffer = String.make buffer_size '\001' in
  let rec iter remaining offset =
    let len = mini remaining buffer_size in
    if len > 0 then begin
      write t offset buffer 0 len;
      iter (remaining - len) (offset ++ (Int64.of_int len))
    end
  in
  iter len64 offset

let copy_chunk t1 t2 pos1 pos2 len =
  flush_fd t1;
  flush_fd t2;
  let buffer_size = 128 * 1024 in
  let buffer = String.make buffer_size '\001' in
  let rec iter remaining pos1 pos2 =
    let len = mini remaining buffer_size in
    if len > 0 then begin
      read t1 pos1 buffer 0 len;
      write t2 pos2 buffer 0 len;
      let len64 = Int64.of_int len in
      iter (remaining - len) (pos1 ++ len64) (pos2 ++ len64)
    end
  in
  iter len pos1 pos2

(*
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
      let nread = Unix.read file_in buffer 0 can_read in
(*      lprintf "Unix2.really_write %d\n" nread; *)
      Unix2.really_write file_out buffer 0 nread;
      iter file_in file_out (len-nread)
  in
  iter file_in file_out len;
(*  lprintf "Chunk copied\n"; *)
  ()
*)

  
let close_all = FDCache.close_all
let close t = 
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.close t
  | MultiFile t -> MultiFile.close t
  | SparseFile t -> SparseFile.close t

let create_ro filename = create_diskfile filename [Unix.O_RDONLY] 0o666
let create_rw filename = create_diskfile filename 
    [Unix.O_CREAT; Unix.O_RDWR] 0o666

let exists t = 
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.exists t
  | MultiFile t -> MultiFile.exists t
  | SparseFile t -> SparseFile.exists t

let remove t = 
  flush_fd t;
  close t;
  match t.file_kind with
  | DiskFile t -> DiskFile.remove t
  | MultiFile t -> MultiFile.remove t
  | SparseFile t -> SparseFile.remove t

let getsize64 s =  getsize64 (create_ro s)
let mtime64 s =  mtime64 (create_ro s)

let file_exists s = exists (create_ro s)

let rename t f = 
  flush_fd t;
  close t;
  match t.file_kind with
  | DiskFile t -> DiskFile.rename t f
  | MultiFile t -> MultiFile.rename t f
  | SparseFile t -> SparseFile.rename t f
      

(* module MultiFile_Test = (MultiFile : File) *)
module DiskFile_Test = (DiskFile : File)
module SparseFile_Test = (SparseFile : File)
  
