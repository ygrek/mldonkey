
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


open Int64ops
open Printf2

let log_prefix = "[Ux32]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt
  
let chunk_min_size = ref 65000L
  
let max_buffered = ref (Int64.of_int (1024 * 1024))
  
let create_file_mode = ref 0o664
let create_dir_mode = ref 0o755
let verbose = ref false
let max_cache_size_default = 50
let max_cache_size = ref max_cache_size_default
  
let mini (x: int) (y: int) =
  if x > y then y else x
    
let ro_flag =  [Unix.O_RDONLY]
let rw_flag =  [Unix.O_RDWR]
let rw_creat_flag =  [Unix.O_CREAT; Unix.O_RDWR]
  
external external_start : unit -> unit = "external_start"
external external_exit : unit -> unit = "external_exit"
external uname : unit -> string = "ml_uname"
external os_supported : unit -> bool = "ml_os_supported"
  
(* let really_write fd s pos len =
  try
    Unix2.really_write fd s pos len
  with e -> raise e *)
let really_write = Unix2.really_write

module FDCache = struct
    
    type t = {
        mutable fd : Unix.file_descr option;
        mutable filename : string;
        mutable writable : bool;
        mutable destroyed : bool; (* could we get rid of this ? *)
    }

    let cache_size = ref 0
    let cache = Fifo.create ()
      
    let close t =
      if not t.destroyed then
        match t.fd with
          | Some fd ->
              if !verbose then lprintf_nl "Close %s" t.filename;
              (try Unix.close fd with e -> 
                 lprintf_nl "Exception in FDCache.close %s: %s" 
                   t.filename
                   (Printexc2.to_string e);
                 raise e);
              t.fd <- None;
              decr cache_size
          | None -> ()

    let is_closed t =
      if t.destroyed then
        true
      else match t.fd with
        | Some fd -> false
        | None -> true

    let rec close_one () =
      if not (Fifo.empty cache) then
        let t = Fifo.take cache in
        match t.fd with
          | None ->
              close_one ()
          | Some fd ->
              try
                close t
              with _ -> close_one ()

    let check_destroyed t =
      if t.destroyed then
        failwith (Printf.sprintf 
          "Unix32.check_destroyed %s: Cannot use destroyed FD" t.filename)
        
    let destroy t =
      if not t.destroyed then begin
        (try close t with _ -> ());
        t.destroyed <- true
      end
        
    let _local_force_fd creat t =
      check_destroyed t;
      let fd =
        match t.fd with
          | Some fd -> fd
          | None ->
              if !cache_size >= !max_cache_size then close_one ();
              let fd = Unix2.tryopen_umask 0 (fun _old_umask ->
                (* disable umask restrictions temporarily; 
                   rights on downloaded files should be dictated 
                   by create_file_mode instead *)
                try
                  if t.writable then
                    Unix.openfile t.filename 
                      (if creat then rw_creat_flag else rw_flag) 
                      !create_file_mode
                  else
                    Unix.openfile t.filename ro_flag 0o400
                with e ->
                  if !verbose then lprintf_nl "Exception in FDCache._local_force_fd %s (%s): %s" 
                    t.filename 
                    (if t.writable then "rw" else "ro") 
                    (Printexc2.to_string e);
                  raise e)
              in
                incr cache_size;
(*              lprintf "local_force: opening %d\n" (Obj.magic fd_rw);  *)
                Fifo.put cache t;
                t.fd <- Some fd;
                fd
      in
(*      lprintf "local_force_fd %d\n" (Obj.magic fd); *)
        fd

    let create f writable =
      if !verbose then lprintf_nl "Open %s (%s)" f (if writable then "rw" else "ro");
      let t =
        {
         filename = f;
         writable = writable;
         fd = None;
         destroyed = false;
        }
      in
      try
      let _fd = _local_force_fd true t in
      t
      with
        Unix.Unix_error (Unix.EISDIR, _, _) -> t
      | Unix.Unix_error (Unix.EACCES, _, _) when Autoconf.windows -> t
      | e -> raise e
      
    let local_force_fd t = _local_force_fd false t

    let close_all () =
      while not (Fifo.empty cache) do
        close_one ()
      done

    let rename t f =
      try
      check_destroyed t;
      close t;
      Unix2.rename t.filename f;
      destroy t
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.rename %s %s: %s"
        t.filename
        f
        (Printexc2.to_string e);
      raise e

    let ftruncate64 t len sparse =
      try
      check_destroyed t;
      Unix2.c_ftruncate64 (local_force_fd t) len sparse
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.ftruncate64 %s %Ld (%s): %s"
        t.filename
        len
        (if sparse then "sparse" else "not sparse")
        (Printexc2.to_string e);
      raise e

    let getsize64 t =
      try
      check_destroyed t;
      Unix2.c_getfdsize64 (local_force_fd t)
      with
        Unix.Unix_error (Unix.EISDIR, _, _) -> 0L
      | Unix.Unix_error (Unix.EACCES, _, _) when Autoconf.windows -> 0L
      | e ->
      if !verbose then lprintf_nl "Exception in FDCache.getsize64 %s: %s"
        t.filename
        (Printexc2.to_string e);
      raise e

    let owner t =
      let module U = Unix.LargeFile in
      try
      check_destroyed t;
      let s =
        try
          U.fstat (local_force_fd t)
        with
 (* Unix.fstat is not supported on MinGW, Unix.stat is supported *)
        | Invalid_argument _ -> U.stat t.filename
      in
      let user = Unix.getpwuid s.U.st_uid in
      let group = Unix.getgrgid s.U.st_gid in
      user.Unix.pw_name, group.Unix.gr_name
      with
      | Not_found -> "", ""
      | e -> if !verbose then
            lprintf_nl "Exception in FDCache.owner %s: %s"
              t.filename (Printexc2.to_string e);
      raise e

    let mtime64 t =
      try
      check_destroyed t;
      let st = Unix.LargeFile.stat t.filename in
      st.Unix.LargeFile.st_mtime
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.mtime64 %s: %s"
        t.filename
        (Printexc2.to_string e);
      raise e

    let exists t =
      try
      check_destroyed t;
      Sys.file_exists t.filename
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.exists %s: %s"
        t.filename
        (Printexc2.to_string e);
      raise e

    let remove t =
      try
      check_destroyed t;
      if exists t then
        Sys.remove t.filename;
      destroy t
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.remove %s: %s"
        t.filename
        (Printexc2.to_string e);
      raise e

    let multi_rename t f file =
      try
      check_destroyed t;
      close t;
      (let d = (Filename.dirname (Filename.concat f file)) in
       Unix2.safe_mkdir d;
       Unix2.chmod d !create_dir_mode;
       Unix2.can_write_to_directory d);
      (try
        Unix2.rename t.filename (Filename.concat f file);
      with
        Unix.Unix_error (Unix.EACCES, _, _) when
          Autoconf.windows && (Unix2.list_directory t.filename = [])
        -> remove t);
      destroy t
      with e ->
      lprintf_nl "Exception in FDCache.multi_rename %s %s: %s"
        t.filename
        (Filename.concat f file)
        (Printexc2.to_string e);
      raise e

    let read file file_pos string string_pos len =
      try
      let fd = local_force_fd file in
      ignore(Unix2.c_seek64 fd file_pos Unix.SEEK_SET);
      if !verbose then
        lprintf_nl "really_read %s %Ld %d"
          file.filename
          file_pos
          len;
      Unix2.really_read fd string string_pos len
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.read %s %Ld %d: %s"
        file.filename
        file_pos
        len
        (Printexc2.to_string e);
      raise e

    let write file file_pos string string_pos len =
      try
      assert (file.writable);
      let fd = local_force_fd file in
      ignore(Unix2.c_seek64 fd file_pos Unix.SEEK_SET);
      if !verbose then
        lprintf_nl "really_write %s %Ld %d"
          file.filename
          file_pos
          len;
          really_write fd string string_pos len
      with e ->
      if !verbose then lprintf_nl "Exception in FDCache.write file %s file_pos=%Ld len=%d string_pos=%d, string length=%d: %s"
        file.filename
        file_pos
        len
        string_pos
        (Bytes.length string)
        (Printexc2.to_string e);
      raise e

    let copy_chunk t1 t2 pos1 pos2 len64 =
      check_destroyed t1;
      check_destroyed t2;
      let buffer_len = 128 * 1024 in
      let buffer_len64 = Int64.of_int buffer_len in
      let buffer = Bytes.make buffer_len '\001' in
      let rec iter remaining pos1 pos2 =
        let len64 = min remaining buffer_len64 in
        let len = Int64.to_int len64 in
        if len > 0 then begin
            read t1 pos1 buffer 0 len;
            write t2 pos2 buffer 0 len;
            iter (remaining -- len64) (pos1 ++ len64) (pos2 ++ len64)
          end
      in
      try
        iter len64 pos1 pos2
      with e ->
        if !verbose then lprintf_nl "Exception in FDCache.copy_chunk %s %Ld to %s %Ld (%Ld): %s"
          t1.filename 
          pos1
          t2.filename
          pos2
          len64
          (Printexc2.to_string e);
        raise e

  end

module type File =   sig
    type t
    val create : string -> bool -> t
    val apply_on_chunk : t -> int64 -> int64 ->
      (Unix.file_descr -> int64 -> 'a) -> 'a
    val close : t -> unit
    val rename : t -> string -> unit
    val ftruncate64 : t -> int64 -> bool -> unit
    val getsize64 : t -> int64
    val mtime64 : t -> float
    val exists : t -> bool
    val remove : t -> unit
    val read : t -> int64 -> bytes -> int -> int -> unit
    val write : t -> int64 -> bytes -> int -> int -> unit
    val destroy : t -> unit
    val is_closed : t -> bool
  end


module DiskFile = struct

    type t = FDCache.t

    let create = FDCache.create

    let apply_on_chunk t pos_s len_s f =
      let fd = FDCache.local_force_fd t in
      f fd pos_s

    let close = FDCache.close

    let rename = FDCache.rename

    let ftruncate64 = FDCache.ftruncate64

    let getsize64 = FDCache.getsize64
    let owner = FDCache.owner
    let mtime64 = FDCache.mtime64
    let exists = FDCache.exists
    let remove = FDCache.remove
    let read = FDCache.read
    let write = FDCache.write
    let destroy = FDCache.destroy
    let is_closed = FDCache.is_closed
  end

let zero_chunk_len = 65536L
let zero_chunk_name = "zero_chunk"
let zero_chunk_fd_option = ref None
let zero_chunk_fd () =
  match !zero_chunk_fd_option with
    Some fd -> fd
  | None ->
      let fd = FDCache.create zero_chunk_name true in
      FDCache.ftruncate64 fd zero_chunk_len false;
      zero_chunk_fd_option := Some fd;
      fd

module MultiFile = struct

    type file = {
        mutable filename : string;
        mutable pos : int64;
        mutable len : int64;
        mutable current_len : int64;
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
      | [||] -> failwith "make_tree: no files"
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

    let rec subfile_tree_map indent tree f=
      match tree with
        | Leaf file -> 
            f file.filename file.pos file.len file.current_len
        | Node (pos, tree1, tree2) ->
            subfile_tree_map (indent ^ "  ") tree1 f;
            subfile_tree_map (indent ^ "  ") tree2 f
              
    let rec print_tree indent tree =
      match tree with
        Leaf file -> lprintf_nl "%s  - %s (%Ld,%Ld)"
            indent file.filename file.pos file.len
      | Node (pos, tree1, tree2) ->
          lprintf_nl "%scut at %Ld" indent pos;
          print_tree (indent ^ "  ") tree1;
          print_tree (indent ^ "  ") tree2

    let create dirname writable files =
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
            let temp_filename = Filename.concat dirname filename in
            Unix2.safe_mkdir (Filename.dirname temp_filename);
            let fd = FDCache.create temp_filename writable in
            iter tail (pos ++ size)
            ({
                filename = filename;
                pos = pos;
                len = size;
                current_len = FDCache.getsize64 fd;
                fd = fd;
                tail = [];
              } :: files2)

      in
      iter files zero []

    let build t = ()

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

    let rec fill_zeros file_out file_pos max_len =
      if max_len > zero then
        let max_possible_write = min max_len zero_chunk_len in
        FDCache.copy_chunk (zero_chunk_fd ()) file_out
          zero file_pos max_possible_write;
        fill_zeros file_out (file_pos ++ max_possible_write)
        (max_len -- max_possible_write)


    let apply_on_chunk t chunk_begin chunk_len f =
      let (file, tail) = find_file t chunk_begin in
      let chunk_end = chunk_begin ++ chunk_len in
      let file_begin = file.pos in
      let max_current_pos = file_begin ++ file.current_len in
      if max_current_pos >= chunk_end then
        let fd = FDCache.local_force_fd file.fd in
        f fd (chunk_begin -- file_begin)
      else
        let temp_file = Filename2.temp_file "chunk" ".tmp" in
        let file_out = FDCache.create temp_file true in

        try
(* first file *)
          let in_pos = chunk_begin -- file_begin in
          let in_len = max_current_pos -- chunk_begin in
          FDCache.copy_chunk file.fd file_out
            in_pos  zero in_len;
          let zeros =
            min (chunk_len -- in_len) (file.len -- file.current_len) in
          fill_zeros file_out in_len zeros;
          let in_len = in_len ++ zeros in

(* other files *)
          do_on_remaining tail in_len  (chunk_len -- in_len)
            (fun file file_pos len ->
               let max_current_pos = min len file.current_len in
               FDCache.copy_chunk file.fd file_out
                 zero file_pos max_current_pos;
               let file_pos = file_pos ++ max_current_pos in
               let zeros = len -- max_current_pos in
               fill_zeros file_out file_pos zeros;
               file_pos ++ zeros
            );
          FDCache.close file_out;
          let fd = FDCache.local_force_fd file_out in
          let v = f fd zero in
          FDCache.close file_out;
          Sys.remove temp_file;
          v
        with e ->
          if !verbose then lprintf_nl "Exception in MultiFile.apply_on_chunk %s %Ld %Ld: %s"
            t.dirname
            chunk_begin
            chunk_len
            (Printexc2.to_string e);
          (try FDCache.close file_out with _ -> ());
          (try Sys.remove temp_file with _ -> ());
          raise e

    let close t =
      List.iter (fun file -> FDCache.close file.fd) t.files

    let destroy t =
      List.iter (fun file -> FDCache.destroy file.fd) t.files

    let rename t f =
      close t;
      List.iter (fun file -> FDCache.multi_rename file.fd f file.filename) 
        t.files

    let ftruncate64 t size sparse =
      t.size <- size

    let getsize64 t = t.size

    let mtime64 t =
      try
        let st = Unix.LargeFile.stat t.dirname in
        st.Unix.LargeFile.st_mtime
      with e ->
        if !verbose then lprintf_nl "Exception in MultiFile.mtime64 %s: %s" 
          t.dirname
          (Printexc2.to_string e);
        raise e

    let exists t =
      Sys.file_exists t.dirname

    let remove t =
      try
        close t;
        if Sys.file_exists t.dirname then
          Unix2.remove_all_directory t.dirname
      with e ->
        if !verbose then lprintf_nl "Exception in MultiFile.remove %s: %s" 
          t.dirname
          (Printexc2.to_string e);
        raise e

    let file_write file in_file_pos s in_string_pos len =
      (* prevent write to zero-byte files so BT downloads finish *)
      if len <> 0 then
        begin
          let len64 = Int64.of_int len in
          let in_file_len = in_file_pos ++ len64 in
          FDCache.write file.fd in_file_pos s in_string_pos len;
          file.current_len <- max file.current_len in_file_len
        end

    let file_read file in_file_pos s in_string_pos len =
      let len64 = Int64.of_int len in
      let in_file_len = in_file_pos ++ len64 in
      if in_file_len <= file.current_len then
        FDCache.read file.fd in_file_pos s in_string_pos len
      else
      let possible_len64 = max zero (file.current_len -- in_file_pos) in
      let possible_len = Int64.to_int possible_len64 in
      if possible_len64 > zero then
        FDCache.read file.fd in_file_pos s in_string_pos possible_len;
      String.fill s (in_string_pos + possible_len) (len - possible_len) '\000'

    let io f t chunk_begin string string_pos len =
      let (file, tail) = find_file t chunk_begin in

      let chunk_len = Int64.of_int len in
      let chunk_end = chunk_begin ++ chunk_len in
      let file_begin = file.pos in
      let file_end = file_begin ++ file.len in
      if file_end >= chunk_end then
        f file (chunk_begin -- file_begin)
        string string_pos len
      else

(* first file *)
      let in_pos = chunk_begin -- file_begin in
      let first_read = file_end -- chunk_begin in
      let in_len = Int64.to_int first_read in
      f file in_pos string string_pos in_len;

(* other files *)
      do_on_remaining tail (string_pos + in_len)
        (chunk_len -- first_read)
        (fun file string_pos len64 ->
           let len = Int64.to_int len64 in
           f file zero string string_pos len;
           string_pos + len
        )

    let read t chunk_begin string string_pos len =
      io file_read t chunk_begin string string_pos len

    let write t chunk_begin string string_pos len =
      io file_write t chunk_begin string string_pos len ;
      let len64 = Int64.of_int len in
      t.size <- max t.size (chunk_begin ++ len64);
      let time = Unix.time () in
      (* this does not work on win32 because of
         file locking i guess, therefore ignore
         all exceptions *)
      (try Unix.utimes t.dirname time time with _ -> ());
      ()

    let is_closed _ = false
  end

module SparseFile = struct
    
    type chunk = {
        mutable chunkname : string;
        mutable pos : int64;
        mutable len : int64;
        mutable fd : FDCache.t;
      }
    
    type t = {
        mutable filename : string;
        mutable dirname : string;
        mutable size : int64;
        mutable chunks : chunk array;
        mutable writable : bool;
      }
    
    let zero_chunk () =
      {
        chunkname = zero_chunk_name;
        pos = zero;
        len = zero_chunk_len;
        fd = zero_chunk_fd ();
      }

    let is_closed _ = false
    
    let create filename writable =
(*      lprintf_nl "SparseFile.create %s" filename; *)
      let dirname = filename ^ ".chunks" in
(*      lprintf_nl "Creating directory %s" dirname; *)
      Unix2.safe_mkdir dirname;
      Unix2.can_write_to_directory dirname;
      {
        filename = filename;
        dirname = dirname;
        chunks = [||];
        size = zero;
        writable = writable;
      }
    
    let find_read_pos t pos =
      let nchunks = Array.length t.chunks in
      let rec iter t start len =
        if len <= 0 then start else
          let milen = len/2 in
          let med = start + milen in
          let chunk = t.chunks.(med) in
          if chunk.pos <= pos && pos -- chunk.pos < chunk.len then med 
          else
            if chunk.pos > pos then
              if med = start then start 
              else
                iter t start milen
            else
              let next = med+1 in
              iter t next (start+len-next)
      in
        iter t 0 nchunks
    
    let find_write_pos t pos =
      let nchunks = Array.length t.chunks in
      let rec iter t start len =
        if len <= 0 then start 
        else
          let milen = len/2 in
          let med = start + milen in
          let chunk = t.chunks.(med) in
          if chunk.pos <= pos && pos -- chunk.pos <= chunk.len then med 
          else
            if chunk.pos > pos then
              if med = start then start 
              else
                iter t start milen
            else
              let next = med+1 in
              iter t next (start+len-next)
      in
        iter t 0 nchunks

(*  (*** debugging code ***)

    let find_read_pos2 t pos =
      let rec iter t i len =
        if i = len then i 
        else
          let chunk = t.chunks.(i) in
          if chunk.pos ++ chunk.len > pos then i 
          else
            iter t (i+1) len
      in
        iter t 0 (Array.length t.chunks)

    let find_write_pos2 t pos =
      let rec iter t i len =
        if i = len then i 
        else
          let chunk = t.chunks.(i) in
          if chunk.pos ++ chunk.len >= pos then i 
          else
            iter t (i+1) len
      in
        iter t 0 (Array.length t.chunks)

    let _ =
      let one = 1L in
      let two = 2L in
      let three = 3L in
      for i = 0 to 3 do
        let chunks = Array.init i (fun i ->
          let name = string_of_int i in
          let pos = 3 * i + 1 in
          let fd = FDCache.create name writable in
          lprintf " %d [%d - %d]" i pos (pos+2);
          {
            chunkname = name;
            pos = Int64.of_int pos;
            len = two;
            fd = fd;
          }
        ) in
        lprintf_nl "";
        let t = {
          filename = "";
          dirname = "";
          size = zero;
          chunks = chunks;
        } in
        for j = 0 to 3 * i + 3 do
          let pos = (Int64.of_int j) in
          let i1 = find_write_pos t pos in
          lprintf "(%d -> %d) " j i1;
          let i2 = find_write_pos2 t pos in
          assert (i1 = i2)
        done;
        lprintf_nl "";
      done;
      exit 0
*)
    
    let build t = ()
    
    let apply_on_chunk t chunk_begin chunk_len f =
      
      let index = find_read_pos t chunk_begin in
      let nchunks = Array.length t.chunks in
      
      if index < nchunks &&
        ( let chunk = t.chunks.(index) in
          chunk.pos <= chunk_begin &&
          chunk.len >= (chunk_len -- (chunk_begin -- chunk.pos))
        ) then
        
        let chunk = t.chunks.(index) in
        let in_chunk_pos = chunk_begin -- chunk.pos in
        let fd = FDCache.local_force_fd chunk.fd in
        f fd in_chunk_pos
      
      else
      
        let temp_file = Filename2.temp_file "chunk" ".tmp" in
        let file_out = FDCache.create temp_file t.writable in
      
        let rec iter pos index chunk_begin chunk_len =
          if chunk_len > zero then
            let chunk =
              if index >= nchunks then 
                let z = zero_chunk () in
                z.pos <- chunk_begin;
                z
              else
                let chunk = t.chunks.(index) in
                let next_pos = chunk.pos in
                if next_pos > chunk_begin then 
                  let z = zero_chunk () in
                  z.pos <- chunk_begin;
                  z.len <- min zero_chunk_len (next_pos -- chunk_begin);
                  z
                else
                  chunk
            in
          
            let in_chunk_pos = chunk_begin -- chunk.pos in
            let max_len = min chunk_len (chunk.len -- in_chunk_pos) in
          
            FDCache.copy_chunk chunk.fd file_out
              in_chunk_pos pos max_len;
            iter (pos ++ max_len) (index+1) (chunk_begin ++ max_len)
              (chunk_len -- max_len)
      
        in
          try
            iter zero (find_read_pos t chunk_begin) chunk_begin chunk_len;
      
            FDCache.close file_out;
            let fd = FDCache.local_force_fd file_out in
            let v = f fd zero in
            Sys.remove temp_file;
            v
          with e -> 
            if !verbose then lprintf_nl "Exception in SparseFile.apply_on_chunk %s %Ld %Ld: %s"
              t.dirname
              chunk_begin
              chunk_len
              (Printexc2.to_string e);
            (try FDCache.close file_out with _ -> ());
            (try Sys.remove temp_file with _ -> ());
            raise e
    
    let close t =
      Array.iter (fun file -> FDCache.close file.fd) t.chunks

    let destroy t =
      Array.iter (fun file -> FDCache.destroy file.fd) t.chunks
      
    let rename t f =
      close t;
      
      let chunk_begin = zero in
      let chunk_len = t.size in
      let nchunks = Array.length t.chunks in
      
      let file_out = FDCache.create f true in
      
      let rec iter pos index chunk_begin chunk_len =
        if chunk_len > zero then
          let chunk =
            if index >= nchunks then 
              let z = zero_chunk () in
              z.pos <- chunk_begin;
              z
            else
              let chunk = t.chunks.(index) in
              let next_pos = chunk.pos in
              if next_pos > chunk_begin then 
                let z = zero_chunk () in
                z.pos <- chunk_begin;
                z.len <- min zero_chunk_len (next_pos -- chunk_begin);
                z
              else
                chunk
          in

          let in_chunk_pos = chunk_begin -- chunk.pos in
          let max_len = min chunk_len (chunk.len -- in_chunk_pos) in

          FDCache.copy_chunk chunk.fd file_out
            in_chunk_pos pos max_len;

          if chunk.fd != zero_chunk_fd () then FDCache.remove chunk.fd;

          iter (pos ++ max_len) (index+1) (chunk_begin ++ max_len)
            (chunk_len -- max_len)

      in
        iter zero 0 chunk_begin chunk_len;
        FDCache.close file_out;
        ()
(* (* why is that commented off ? Does SparseFile.rename actually work ? *)
        Sys.rename t.dirname f;
        List.iter (fun file ->
            file.fd.FDCache.filename <- Filename.concat t.dirname file.filename
        ) t.files
*)

    let ftruncate64 t size sparse =
      t.size <- size

    let getsize64 t = t.size

    let mtime64 t =
      try
        let st = Unix.LargeFile.stat t.dirname in
        st.Unix.LargeFile.st_mtime
      with e ->
        if !verbose then lprintf_nl "Exception in SparseFile.mtime64 %s: %s"
          t.dirname
          (Printexc2.to_string e);
        raise e

    let exists t =
      Sys.file_exists t.dirname

    let remove t =
      try
        close t;
(*      lprintf "Removing %s\n" t.dirname; *)
        Unix2.remove_all_directory t.dirname
      with e ->
        if !verbose then lprintf_nl "Exception in SparseFile.remove %s: %s" 
          t.dirname
          (Printexc2.to_string e);
        raise e

    let read t chunk_begin string string_pos chunk_len =
      let chunk_len64 = Int64.of_int chunk_len in

      let nchunks = Array.length t.chunks in
      let rec iter string_pos index chunk_begin chunk_len64 =

        if chunk_len64 > zero then
          let chunk =
            if index >= nchunks then
              let z = zero_chunk () in
              z.pos <- chunk_begin;
              z
            else
              let chunk = t.chunks.(index) in
              let next_pos = chunk.pos in
              if next_pos > chunk_begin then 
                let z = zero_chunk () in
                z.pos <- chunk_begin;
                z.len <- min zero_chunk_len (next_pos -- chunk_begin);
                z
              else
                chunk
          in

          let in_chunk_pos = chunk_begin -- chunk.pos in
          let max_len64 = min chunk_len64 (chunk.len -- in_chunk_pos) in
          let max_len = Int64.to_int max_len64 in

          FDCache.read chunk.fd in_chunk_pos string string_pos max_len;
          iter (string_pos + max_len) (index+1) (chunk_begin ++ max_len64)
            (chunk_len64 -- max_len64)

      in
        iter string_pos (find_read_pos t chunk_begin) chunk_begin chunk_len64

    let write t chunk_begin string string_pos len =
      let index = find_write_pos t chunk_begin in
      let len64 = Int64.of_int len in
      let nchunks = Array.length t.chunks in

      if index = Array.length t.chunks then begin
(*          lprintf "Adding chunk at end\n"; *)
        let chunk_name = Int64.to_string chunk_begin in
        let chunk_name = Filename.concat t.dirname chunk_name in
        let fd = FDCache.create chunk_name t.writable in
        let chunk = {
          chunkname = chunk_name;
          pos = chunk_begin;
          len = zero;
          fd = fd;
        } in
        let new_array = Array.make (nchunks+1) chunk in
        Array.blit t.chunks 0 new_array 0 nchunks;
        t.chunks <- new_array

      end else
        if t.chunks.(index).pos > chunk_begin then begin
(*          lprintf "Inserting chunk\n"; *)
          let chunk_name = Int64.to_string chunk_begin in
          let chunk_name = Filename.concat t.dirname chunk_name in
          let fd = FDCache.create chunk_name t.writable in
          let chunk = {
            chunkname = chunk_name;
            pos = chunk_begin;
            len = zero;
            fd = fd;
          } in
          let new_array = Array.make (nchunks+1) chunk in
          Array.blit t.chunks 0 new_array 0 index;
          Array.blit t.chunks index new_array (index+1) (nchunks-index);
          t.chunks <- new_array;
        end;

      let nchunks = Array.length t.chunks in
      let rec iter index chunk_begin string_pos len =
        if len > 0 then
          let next_index, max_len, max_len64 =
            if index = nchunks-1 then
              index, len, Int64.of_int len
            else
              let max_pos = t.chunks.(index+1).pos in
              let max_possible_len64 = max_pos -- chunk_begin in
              let len64 = Int64.of_int len in
              let max_len64 = min max_possible_len64 len64 in
              let max_len = Int64.to_int max_len64 in
              if max_len64 = max_possible_len64 then
                index+1, max_len, max_len64
              else
                index, max_len, max_len64
          in

          let chunk = t.chunks.(index) in
          let begin_pos = chunk_begin -- chunk.pos in
          FDCache.write chunk.fd begin_pos string string_pos max_len;
          chunk.len <- chunk.len ++ max_len64;

          iter next_index (chunk_begin ++ max_len64)
            (string_pos + max_len) (len - max_len)
      in
        iter index chunk_begin string_pos len;

      t.size <- max t.size (chunk_begin ++ len64);

      let time = Unix.time () in
      (* this does not work on win32 because of
         file locking i guess, therefore ignore
         all exceptions *)
      (try Unix.utimes t.dirname time time with _ -> ())
  end


type file_kind =
| MultiFile of MultiFile.t
| DiskFile of DiskFile.t
| SparseFile of SparseFile.t
| Destroyed
  
type file = {
    mutable file_kind : file_kind;
    mutable filename : string;
    mutable writable : bool;
    mutable error : exn option;
    mutable buffers : (bytes * int * int * int64 * int64) list;
  }
  
module H = Weak.Make(struct
      type old_t = file
      type t = old_t
      let hash t = Hashtbl.hash (t.filename, t.writable)

      let equal x y  = x.filename = y.filename && x.writable = y.writable
    end)

let dummy =  {
    file_kind = Destroyed;
    filename = "";
    writable = false;
    error = None;
    buffers = [];
  }

let table = H.create 100

let destroyed t = t.file_kind = Destroyed
  
let fd_exists f =
  try
    let fd = H.find table { dummy with filename = f } in
    match fd.file_kind with
    | DiskFile fd -> not (DiskFile.is_closed fd)
    | MultiFile fd -> not (MultiFile.is_closed fd)
    | SparseFile fd -> not (SparseFile.is_closed fd)
    | Destroyed -> false
  with Not_found -> false

let create f writable creator =
  try
    let fd = H.find table { dummy with filename = f; writable = writable } in
(*    lprintf "%s already exists\n" f; *)
    fd
  with Not_found ->
    let t = {
      file_kind = creator f;
      filename = f;
      writable = writable;
      error = None;
      buffers = [];
    } in
    H.add table t;
    t

(* check if a writable descriptor on the same file exists *)
let find_writable fd =
  if fd.writable then Some fd
  else
    try
      Some (H.find table { fd with writable = true })
    with Not_found -> None
      
let create_diskfile filename writable =
  create filename writable (fun f -> DiskFile (DiskFile.create f writable))

let create_multifile filename writable files =
  create filename writable (fun f ->
    MultiFile (MultiFile.create f writable files))

let create_sparsefile filename writable =
  create filename writable (fun f ->
    SparseFile (SparseFile.create f writable))

let ftruncate64 t len sparse =
  match t.file_kind with
  | DiskFile t -> DiskFile.ftruncate64 t len sparse
  | MultiFile t -> MultiFile.ftruncate64 t len sparse
  | SparseFile t -> SparseFile.ftruncate64 t len sparse
  | Destroyed -> failwith "Unix32.ftruncate64 on destroyed FD"
      
let mtime64 t =
  match t.file_kind with
  | DiskFile t -> DiskFile.mtime64 t
  | MultiFile t -> MultiFile.mtime64 t
  | SparseFile t -> SparseFile.mtime64 t
  | Destroyed -> failwith "Unix32.mtime64 on destroyed FD"
      
let getsize64 t =
  match t.file_kind with
  | DiskFile t -> DiskFile.getsize64 t
    (* only avoid opening rw on shared files, shared files can only be DiskFile *)
  | MultiFile t -> MultiFile.getsize64 t
  | SparseFile t -> SparseFile.getsize64 t
  | Destroyed -> failwith "Unix32.getsize64 on destroyed FD"

let fds_size = Unix2.c_getdtablesize ()

let buffered_bytes = ref Int64.zero
let modified_files = ref []

let filename t = t.filename

let write file file_pos string string_pos len =
  if len > 0 then
    match file.file_kind with
      | DiskFile t -> DiskFile.write t file_pos string string_pos len
      | MultiFile t -> MultiFile.write t file_pos string string_pos len
      | SparseFile t -> SparseFile.write t file_pos string string_pos len
      | Destroyed -> failwith "Unix32.write on destroyed FD"
  else
    lprintf_nl "Unix32.write: error, invalid argument len = 0"
        
let buffer = Buffer.create 65000

let flush_buffer t offset =
  if !verbose then lprintf_nl "flush_buffer";
  let s = Buffer.to_bytes buffer in
  Buffer.reset buffer;
  let len = Bytes.length s in
  try
    if !verbose then lprintf_nl "seek64 %Ld" offset;
    if len > 0 then write t offset s 0 len;
(*
    let fd, offset =  fd_of_chunk t offset (Int64.of_int len) in
    let final_pos = Unix2.c_seek64 fd offset Unix.SEEK_SET in
    if verbose then lprintf_nl "really_write %d" len;
    Unix2.really_write fd s 0 len;
*)
    buffered_bytes := !buffered_bytes -- (Int64.of_int len);
    if !verbose then lprintf_nl "written %d bytes (%Ld)" len !buffered_bytes;
  with e ->
    if !verbose then lprintf_nl "exception %s in flush_buffer" (Printexc2.to_string e);
    t.buffers <- (s, 0, len, offset, Int64.of_int len) :: t.buffers;
    raise e

let flush_fd t =
  match find_writable t with
  | None -> ()
  | Some t ->
      if t.buffers = [] then () else
        let list =
          List.sort (fun (_, _, _, o1, l1) (_, _, _, o2, l2) ->
            let c = compare o1 o2 in
            if c = 0 then compare l2 l1 else c)
            t.buffers
        in
        if !verbose then lprintf_nl "flush_fd";
        t.buffers <- list;
        let rec iter_out () =
          match t.buffers with
          | [] -> ()
          | (s, pos_s, len_s, offset, len) :: tail ->
              Buffer.reset buffer;
              Buffer.add_subbytes buffer s pos_s len_s;
              t.buffers <- tail;
              iter_in offset len

        and iter_in offset len =
          match t.buffers with
          | [] -> flush_buffer t offset
          | (s, pos_s, len_s, offset2, len2) :: tail ->
              let in_offset = offset ++ len -- offset2 in
              if in_offset = Int64.zero then begin
                Buffer.add_subbytes buffer s pos_s len_s;
                t.buffers <- tail;
                iter_in offset (len ++ len2);
              end else
                if in_offset < Int64.zero then begin
                  flush_buffer t offset;
                  iter_out ()
                end else
                  let keep_len = len2 -- in_offset in
                  if !verbose then lprintf_nl "overlap %Ld" keep_len;
                  t.buffers <- tail;
                  if keep_len <= 0L then begin
                    buffered_bytes := !buffered_bytes -- len2;
                    iter_in offset len
                  end else begin
                    let new_pos = len2 -- keep_len in
                    Buffer.add_subbytes buffer s
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
  | Destroyed -> failwith 
        (Printf.sprintf "Unix32.read on destroyed FD %s"
          (filename t))
      
let flush _ =
  try
    if !verbose then lprintf_nl "flush all";
    let rec iter list =
      match list with
      | [] -> []
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
      if !buffered_bytes <> 0L then
        lprintf_nl "[ERROR] remaining bytes after flush"
  with e ->
      if !verbose then lprintf_nl "[ERROR] Exception %s in Unix32.flush"
        (Printexc2.to_string e)

let buffered_write t offset s pos_s len_s =
  let len = Int64.of_int len_s in
  match t.error with
  | None ->
      if len > Int64.zero then begin
        if not (List.memq t !modified_files) then
          modified_files := t :: !modified_files;
        t.buffers <- (s, pos_s, len_s, offset, len) :: t.buffers;
        buffered_bytes := !buffered_bytes ++ len;
        if !verbose then
          lprintf_nl "buffering %Ld bytes (%Ld)" len !buffered_bytes;

(* Don't buffer more than 1 Mo *)
        if !buffered_bytes > !max_buffered then flush ()
      end
  | Some e ->
      raise e

let buffered_write_copy t offset s pos_s len_s =
  buffered_write t offset (Bytes.sub s pos_s len_s) 0 len_s

let copy_chunk t1 t2 pos1 pos2 len =
  flush_fd t1;
  flush_fd t2;
  let buffer_size = 128 * 1024 in
  let buffer = Bytes.make buffer_size '\001' in
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

let mega = megabytes 1

let rec copy t1 t2 pos1 pos2 len64 = 
  if len64 > mega then begin
    copy_chunk t1 t2 pos1 pos2 (Int64.to_int mega);
    copy t1 t2 (pos1 ++ mega) (pos2 ++ mega) (len64 -- mega)
  end else
    copy_chunk t1 t2 pos1 pos2 (Int64.to_int len64)

let close_all = FDCache.close_all

let close t =
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.close t
  | MultiFile t -> MultiFile.close t
  | SparseFile t -> SparseFile.close t
  | Destroyed -> () (* failwith "Unix32.close on destroyed FD" *)

let destroy t =
  if t.file_kind <> Destroyed then begin
    H.remove table t;
    (match t.file_kind with
     | DiskFile t -> DiskFile.destroy t
     | MultiFile t -> MultiFile.destroy t
     | SparseFile t -> SparseFile.destroy t
     | Destroyed -> ());
    t.file_kind <- Destroyed
  end
    
let create_rw filename = create_diskfile filename true
let create_ro filename = create_diskfile filename false

let apply_on_chunk t pos len f =
  match t.file_kind with
  | DiskFile t -> DiskFile.apply_on_chunk t pos len f
  | MultiFile tt ->
      flush_fd t;
      MultiFile.apply_on_chunk tt pos len f
  | SparseFile t -> SparseFile.apply_on_chunk t pos len f
  | Destroyed -> failwith "Unix32.apply_on_chunk on destroyed FD"
      
let exists t =
  flush_fd t;
  match t.file_kind with
  | DiskFile t -> DiskFile.exists t
  | MultiFile t -> MultiFile.exists t
  | SparseFile t -> SparseFile.exists t
  | Destroyed -> failwith "Unix32.exists on destroyed FD"
      
let remove t =
  flush_fd t;
  close t;
  match t.file_kind with
  | DiskFile t -> DiskFile.remove t
  | MultiFile t -> MultiFile.remove t
  | SparseFile t -> SparseFile.remove t
  | Destroyed -> failwith "Unix32.remove on destroyed FD"
      
let getsize s =
  let old_fd_exists = fd_exists s in
  let fd = create_ro s in
  let size = getsize64 fd in
  if not old_fd_exists then close fd;
  size

let mtime s =
  let old_fd_exists = fd_exists s in
  let fd = create_ro s in
  let time = mtime64 fd in
  if not old_fd_exists then close fd;
  time

let file_exists s =
  let old_fd_exists = fd_exists s in
    try
      let fd = create_ro s in
      let exists =
        try
          exists fd
        with Unix.Unix_error (Unix.ENOENT, _, _) -> false
      in
      if not old_fd_exists then close fd;
      exists
    with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let owner_fd t =
  match t.file_kind with
  | DiskFile t -> DiskFile.owner t
  | MultiFile t -> "", ""
  | SparseFile t -> "", ""
  | Destroyed -> "", ""

let owner s =
    let old_fd_exists = fd_exists s in
    let fd = create_ro s in
  let user,pass =
    try
      owner_fd fd
  with _ -> "", ""
  in
  if not old_fd_exists then close fd;
  user, pass

let rename t f =
  flush_fd t;
  close t;
  match t.file_kind with
  | DiskFile t -> DiskFile.rename t f
  | MultiFile t -> MultiFile.rename t f
  | SparseFile t -> SparseFile.rename t f
  | Destroyed -> failwith "Unix32.rename on destroyed FD"

(* module MultiFile_Test = (MultiFile : File) *)
module DiskFile_Test = (DiskFile : File)
module SparseFile_Test = (SparseFile : File)

module SharedParts = struct 

(*************************************************************************)
(*                                                                       *)
(*                      Files sharing parts                              *)
(*                                                                       *)
(*************************************************************************)

(* TODO: share file parts:
The following functions have to be rewritten:
* rename, remove: copy the shared parts out before destroying the file.
* write, buffered_write, buffered_write_copy, read: split
* allocate_chunk: remove this
* copy_chunk: remove this
*)
    
  type t = {
    file : file;
    mutable file_parts : part list;
  }
    
  and part = {
    mutable part_file : file;
    mutable part_begin : int64;
    mutable part_len : int64;
    mutable part_end : int64;
    mutable part_shared : t list;
  }
    
  let copy_shared_parts_out file parts =
    List2.tail_map (fun part ->
      if part.part_file == file then 
        match part.part_shared with
        | [] -> part
        | t :: tail ->
            lprintf_nl "Copy shared part to another file";
            copy part.part_file t.file part.part_begin part.part_begin
              part.part_len;
            lprintf_nl "   Copy done.";
            part.part_file <- t.file;
            part.part_shared <- tail;
            { part with part_file = file; part_shared = [] }
      else part
    ) parts
    
  let copy_shared_parts_in file parts =
    List2.tail_map (fun part ->
      if part.part_file != file then begin
        lprintf_nl "Copy shared part to another file";
        copy part.part_file file part.part_begin part.part_begin
          part.part_len;
        lprintf_nl "   Copy done.";
        part.part_shared <- List.filter (fun t -> t.file != file) 
          part.part_shared;
        { part with part_file = file; part_shared = [] }
      end else 
        part
    ) parts
    
  let remove t =
    t.file_parts <- copy_shared_parts_out t.file t.file_parts;
    remove t.file
    
  let destroy t = 
    t.file_parts <- copy_shared_parts_out t.file t.file_parts;
    destroy t.file
    
  let old_close = close
    
  let close t = close t.file
  let getsize64 t  = getsize64 t.file
  let filename t = filename t.file
    
  let rename t file_name = 
    t.file_parts <- copy_shared_parts_in t.file t.file_parts;
    t.file_parts <- copy_shared_parts_out t.file t.file_parts;
    rename t.file file_name
    
  let mtime64 t = mtime64 t.file
  let flush_fd t = flush_fd t.file
    
  let apply_on_parts f t file_pos s pos len =
    List.iter (fun part ->
      f part.part_file file_pos s pos len) t.file_parts
    
  let buffered_write t file_pos s pos len =
    apply_on_parts buffered_write t file_pos s pos len
    
  let buffered_write_copy t file_pos s pos len =
    apply_on_parts buffered_write_copy t file_pos s pos len
    
  let write t file_pos s pos len =
    apply_on_parts write t file_pos s pos len
    
  let read t file_pos s pos len =
    apply_on_parts read t file_pos s pos len



(* TODO: there is no need to create a temporary file when the wanted chunk
 * overlaps different parts, but these parts are on the same physical file. *)
  let apply_on_chunk t chunk_begin chunk_len f =
    let chunk_end = chunk_begin ++ chunk_len in
    let rec iter list =
      match list with 
      | [] -> assert false
      | part :: tail ->
          if part.part_begin <= chunk_begin &&
            part.part_end >= chunk_end then
              apply_on_chunk part.part_file chunk_begin chunk_len f
          else
            if part.part_end > chunk_begin then
              make_temp_file list
            else
              iter tail
      
    and make_temp_file list = 
      let temp_file = Filename2.temp_file "chunk" ".tmp" in
      let file_out = create_rw temp_file in
        
      let rec fill pos chunk_begin chunk_len list =
        if chunk_len > zero then
          match list with
          | [] -> ()
          | part :: tail ->
                
              let tocopy = min chunk_len (part.part_end -- chunk_begin) in
              copy_chunk part.part_file file_out chunk_begin pos 
                (Int64.to_int tocopy);
              fill (pos ++ tocopy) (chunk_begin ++ tocopy)
                (chunk_len -- tocopy) tail
      in
        fill zero chunk_begin chunk_len list;
        old_close file_out;
        try
          let v = apply_on_chunk file_out zero chunk_len f in
          Sys.remove temp_file;
          v
        with e ->
            (try Sys.remove temp_file with _ -> ());
            raise e

      in
        iter t.file_parts

    let ftruncate64 t len sparse =
      ftruncate64 t.file len sparse

    let maxint64 = megabytes 1000000

    let create file =
      let part = { 
          part_file = file;
          part_begin = zero;
          part_end = maxint64;
          part_len = maxint64;
          part_shared = []
        } in
      { file = file; file_parts = [part] }
    
    let create_diskfile file_name writable =
      create (create_diskfile file_name writable)
    
    let create_multifile file_name writable files =
      create (create_multifile file_name writable files)
    
    let create_sparsefile file_name writable =
      create (create_sparsefile file_name writable)
    
    let create_ro filename = 
      create (create_ro filename)
    
    let create_rw filename = 
      create (create_rw filename)

    let is_closed = FDCache.is_closed

(*
(* the new part (shared_begin, shared_len) is shared between t1 and t2.
It will be kept inside t1, and used by t2. The problem is what happens
when these two files have already been partially downloaded ? This 
  problem has to be solved at a upper level. *)
    let shared_part t1 t2 shared_begin shared_len = 
      let shared_end = shared_begin ++ shared_len in
      ()
*)      
  end

(* subfile tree map function*)
let subfile_tree_map t f =
  match t.file_kind with
    | MultiFile t -> MultiFile.subfile_tree_map "" t.MultiFile.tree f; ()
    | _ -> ()
        

let find_file t chunk_begin = 
  match t.file_kind with
    | MultiFile t ->  
        let (sf, tail) = (MultiFile.find_file t chunk_begin) in
          (sf.MultiFile.filename, sf.MultiFile.pos , sf.MultiFile.len)
    | _ -> ("unimplemeted" , 0L, 0L)

let find_file_index t index = 
  match t.file_kind with
    | MultiFile t ->  
        let sf = List.nth t.MultiFile.files index in
          (sf.MultiFile.filename, sf.MultiFile.pos , sf.MultiFile.len)
    | _ -> ("unimplemeted" , 0L, 0L)

type t = file

(*
let bad_fd = 
  let t = create_rw "/dev/null" in
  t.file_kind <- Destroyed;
  t
*)

type statfs = {
  f_type : int64;   (* type of filesystem *)
  f_bsize : int64;  (* optimal transfer block size *)
  f_blocks : int64; (* total data blocks in file system *)
  f_bfree : int64;  (* free blocks in fs *)
  f_bavail : int64; (* free blocks avail to non-superuser *)
  f_files : int64;  (* total file nodes in file system *)
  f_ffree : int64;  (* free file nodes in fs *)
  f_fsid : unit;  (* See note in statfs(2) *)
  f_fnamelen : int64; (* maximum length of filenames, maximum Sys.max_string_length *)
  f_basetype : string; (* type of filesystem - Solaris, (-1) on other systems, use f_type there *)
  f_frsize : int64;  (* Fundamental file system block size, (-1) if not provided by system *)
}

exception Not_supported
exception Error

let _ = Callback.register_exception "not supported" Not_supported
let _ = Callback.register_exception "error" Error

external statfs : string -> statfs = "statfs_statfs"

let _bsize sf =
  try
    if sf.f_frsize = Int64.zero || sf.f_frsize = -1L then
      Some sf.f_bsize
    else
      Some sf.f_frsize
  with e -> None


let bsize dir =
  try
    _bsize (statfs dir)
  with e -> None

let blocks dir =
  try
    Some (statfs dir).f_blocks
  with e -> None

let bfree dir =
  try
    Some (statfs dir).f_bfree
  with e -> None

let bavail dir =
  try
    Some (statfs dir).f_bavail
  with e -> None

let fnamelen dir =
  try
    Some (Int64.to_int (min (statfs dir).f_fnamelen (Int64.of_int Sys.max_string_length)))
  with e -> None

let disktotal dir =
(* total disk space in bytes *)
  try
    let s = statfs dir in
    match _bsize s with
    | Some bsize ->
        Some (bsize ** s.f_blocks)
    | None -> None
  with e -> None

let diskfree dir =
(* free disk space in bytes *)
  try
    let s = statfs dir in
    match _bsize s with
    | Some bsize ->
        Some (bsize ** s.f_bavail)
    | None -> None
  with e -> None

let diskused dir =
(* used disk space in bytes *)
  try
    let s = statfs dir in
    match _bsize s with
    | Some bsize ->
        Some (bsize ** (s.f_blocks -- s.f_bavail))
    | None -> None
  with e -> None

let percentused dir =
(* percentage of used disk space *)
  match diskfree dir, disktotal dir with
  | Some dfree, Some dtotal ->
      begin
        try
          Some (Int64.to_int (100L -- (dfree ** 100L // dtotal)))
        with Division_by_zero -> None
      end
  | _ -> None

let percentfree dir =
(* percentage of free disk space *)
  match diskfree dir, disktotal dir with
  | Some dfree, Some dtotal ->
      begin
        try
          Some (Int64.to_int (dfree ** 100L // dtotal))
        with Division_by_zero -> None
      end
  | _ -> None

type fstype = [ `Win | `Posix | `Mac | `Unknown ]

let filesystem' dir =
  try
    let s = statfs dir in
    match s.f_type with
(* values copied from statfs(2) manpage *)
    | 0xadf5L -> "ADFS_SUPER_MAGIC", `Unknown
    | 0xADFFL -> "AFFS_SUPER_MAGIC", `Unknown
    | 0x42465331L -> "BEFS_SUPER_MAGIC", `Unknown
    | 0x1BADFACEL -> "BFS_MAGIC", `Unknown
    | 0xFF534D42L -> "CIFS_MAGIC_NUMBER", `Win
    | 0x73757245L -> "CODA_SUPER_MAGIC", `Unknown
    | 0x012FF7B7L -> "COH_SUPER_MAGIC", `Unknown
    | 0x28cd3d45L -> "CRAMFS_MAGIC", `Unknown
    | 0x1373L -> "DEVFS_SUPER_MAGIC", `Unknown
    | 0x00414A53L -> "EFS_SUPER_MAGIC", `Unknown
    | 0x137DL -> "EXT_SUPER_MAGIC", `Posix
    | 0xEF51L -> "ext2", `Posix (* EXT2_OLD_SUPER_MAGIC *)
    | 0xEF53L -> "ext2/3/4", `Posix (* EXT2/3/4_SUPER_MAGIC *)
    | 0x4244L -> "HFS_SUPER_MAGIC", `Mac
    | 0xF995E849L -> "HPFS_SUPER_MAGIC", `Unknown
    | 0x958458f6L -> "HUGETLBFS_MAGIC", `Unknown
    | 0x9660L -> "ISOFS_SUPER_MAGIC", `Win
    | 0x4000L -> "ISOFS_SUPER_MAGIC_WIN", `Win (* from coreutils-5.2.1, stat.c *)
    | 0x4004L -> "ISOFS_SUPER_MAGIC_R_WIN", `Win (* from coreutils-5.2.1, stat.c *)
    | 0x72b6L -> "JFFS2_SUPER_MAGIC", `Posix
    | 0x3153464aL -> "JFS_SUPER_MAGIC", `Posix
    | 0x137FL -> "MINIX_SUPER_MAGIC", `Posix
    | 0x138FL -> "MINIX_SUPER_MAGIC2", `Posix
    | 0x2468L -> "MINIX2_SUPER_MAGIC", `Posix
    | 0x2478L -> "MINIX2_SUPER_MAGIC2", `Posix
    | 0x4d44L -> "msdos", `Win (* MSDOS_SUPER_MAGIC *)
    | 0x4006L -> "fat", `Win (* from coreutils-5.2.1, stat.c *)
    | 0x564cL -> "NCP_SUPER_MAGIC", `Unknown
    | 0x6969L -> "NFS_SUPER_MAGIC", `Unknown
    | 0x5346544eL -> "ntfs", `Win (* NTFS_SB_MAGIC *)
    | 0x9fa1L -> "OPENPROM_SUPER_MAGIC", `Unknown
    | 0x9fa0L -> "PROC_SUPER_MAGIC", `Posix
    | 0x002fL -> "QNX4_SUPER_MAGIC", `Posix
    | 0x52654973L -> "reiserfs", `Posix (* REISERFS_SUPER_MAGIC *)
    | 0x52345362L -> "reiser4", `Posix
    | 0x7275L -> "ROMFS_MAGIC", `Unknown
    | 0x517BL -> "smb", `Win (* SMB_SUPER_MAGIC *)
    | 0x012FF7B6L -> "SYSV2_SUPER_MAGIC", `Posix
    | 0x012FF7B5L -> "SYSV4_SUPER_MAGIC", `Posix
    | 0x01021994L -> "tmpfs", `Posix (* TMPFS_MAGIC *)
    | 0x15013346L -> "UDF_SUPER_MAGIC", `Win
    | 0x00011954L -> "UFS_MAGIC", `Win
    | 0x9fa2L -> "USBDEVICE_SUPER_MAGIC", `Unknown
    | 0xa501FCF5L -> "VXFS_SUPER_MAGIC", `Unknown
    | 0x012FF7B4L -> "XENIX_SUPER_MAGIC", `Posix
    | 0x58465342L -> "xfs", `Posix (* XFS_SUPER_MAGIC *)
    | 0x012FD16DL -> "_XIAFS_SUPER_MAGIC", `Unknown
    | 0x9123683EL -> "btrfs", `Posix
    | 5L -> "iso9660", `Win (* Cygwin *)
    | 6L -> "fat", `Win (* Cygwin *)
    | 0x700FFL -> "ntfs", `Win (* Cygwin *)
    | 0xC3L -> "ext2/3", `Win (* Cygwin *)
    | _ -> if s.f_basetype <> "-1" then
             s.f_basetype, `Unknown
           else
             Printf.sprintf "unknown (%LX)" s.f_type, `Unknown
  with e -> "not supported", `Unknown

let filesystem dir = fst (filesystem' dir)
let filesystem_type dir = snd (filesystem' dir)

let set_max_cache_size v =
  max_cache_size := v;
  while !FDCache.cache_size > !max_cache_size do FDCache.close_one () done

let get_max_cache_size () = !max_cache_size

let _ =
  Heap.add_memstat "Unix32" (fun level buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) table;
      Printf.bprintf buf "  table: %d\n" !counter;
      Printf.bprintf buf "  modified_files: %d\n" (List.length !modified_files);
      Printf.bprintf buf "  max cache_size: %d\n" !max_cache_size;
      Printf.bprintf buf "  fd cache_size: %d\n" !FDCache.cache_size
  )
