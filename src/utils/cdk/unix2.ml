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
open Unix

let tryopen openf closef filename f =
  let descr = openf filename in
  let result =
    try
      f descr
    with e ->
      (try closef descr with _ -> ());
      raise e in
  closef descr;
  result

let exn_drop f x = try f x with _ -> ()
let with_remove fn f = tryopen (fun fn -> fn) (fun fn -> exn_drop Sys.remove fn) fn f

let tryopen_read fn f = tryopen open_in close_in fn f
let tryopen_write fn f = tryopen open_out close_out fn f
let tryopen_read_bin fn f = tryopen open_in_bin close_in fn f
let tryopen_write_bin fn f = tryopen open_out_bin close_out fn f
let tryopen_read_gen fn flags perm f = 
  tryopen (open_in_gen flags perm) close_in fn f
let tryopen_write_gen fn flags perm f = 
  tryopen (open_out_gen flags perm) close_out fn f
let tryopen_openfile fn flags perm f =
  tryopen (fun fn -> Unix.openfile fn flags perm) Unix.close fn f
let tryopen_dir dir f = tryopen opendir closedir dir f
let tryopen_read_zip fn f = tryopen Zip.open_in Zip.close_in fn f
let tryopen_write_zip fn f = tryopen Zip.open_out Zip.close_out fn f
let tryopen_read_tar fn f = 
  tryopen Tar.open_in Tar.close_in fn f
let tryopen_write_tar ?compress fn f = 
  tryopen (Tar.open_out ?compress) Tar.close_out fn f
let tryopen_read_gzip fn f = 
  tryopen Gzip.open_in_file Gzip.close_in fn f
let tryopen_write_gzip ?level fn f = 
  tryopen (Gzip.open_out_file ?level) Gzip.close_out fn f
let tryopen_umask temp_umask f =
  (* Unix.umask is not implemented on MinGW *)
  let safe_umask umask = try Unix.umask umask with Invalid_argument _ -> 0 in
  tryopen safe_umask (fun oldumask -> ignore(safe_umask oldumask)) temp_umask f

let list_directory filename =
  let list = ref [] in
  tryopen_dir filename (fun dir ->
    try
      while true do
        let file = readdir dir in 
        if file <> "." && file <> ".." &&
          not (file = ".DS_Store" || String2.check_prefix file "._" ||
               file = "Thumbs.db" || file = "desktop.ini") then
            list := file :: !list 
      done
    with End_of_file -> ());
  !list
  
let iter_directory f dirname =
  tryopen_dir dirname (fun dir ->
    try
      while true do
        let file = readdir dir in 
        if file <> "." && file <> ".." then
          f (Filename.concat dirname file)
      done
    with End_of_file -> ())

let is_directory filename =
  try let s = Unix.LargeFile.stat filename in s.LargeFile.st_kind = S_DIR with _ -> false

let is_link filename =
  try let s = Unix.LargeFile.lstat filename in s.LargeFile.st_kind = S_LNK with _ -> false

let chmod f o = 
  try 
    Unix.chmod f o 
  with e -> 
    lprintf_nl "warning: chmod failed on %s: %s" f (Printexc2.to_string e)

let rec safe_mkdir ?(mode = 0o775) dir =  
  if Sys.file_exists dir then begin
    if not (is_directory dir) then 
      failwith (Printf.sprintf "%s already exists but is not a directory" dir)
  end
  else 
    if is_link dir then
      try
        tryopen_dir dir ignore
      with
        | Unix.Unix_error (EACCES, _, _) -> 
            lprintf_nl "access denied for directory %s" dir; 
            exit 73
        | Unix.Unix_error (ENOENT, _, _) -> 
            lprintf_nl "directory %s not found, orphaned link?" dir; 
            exit 73
        | e -> 
            lprintf_nl "error %s for directory %s" (Printexc2.to_string e) dir;
            exit 73
    else 
      let predir = Filename.dirname dir in
      if predir <> dir then safe_mkdir predir;
      try
        Unix.mkdir dir mode
      with
        Unix.Unix_error (EEXIST, _, _) -> ()
      | e -> lprintf_nl "error %s for directory %s" (Printexc2.to_string e) dir; exit 73

(* same as in downloadClient.ml *)
let rec really_write fd s pos len =
  if len = 0 then begin
(*      lprintf "really_write 0 BYTES !!!!!!!!!\n";  *)
      raise End_of_file
    end else
  let nwrite = Unix.write fd s pos len in
  if nwrite = 0 then raise End_of_file else
  if nwrite < len then 
    really_write fd s (pos + nwrite) (len - nwrite)
      
let rec really_read fd s pos len =
  let nread = Unix.read fd s pos len in
  if nread = 0 then raise End_of_file else
  if nread < len then
    really_read fd s (pos + nread) (len - nread)

let copy oldname newname =
  tryopen_read_bin oldname (fun ic ->
    let stats =
      try Some (Unix.LargeFile.fstat (Unix.descr_of_in_channel ic)) with _ -> None
    in
    tryopen_write_bin newname (fun oc ->
      (match stats with
      | None -> ()
      | Some stats ->
      let descr = Unix.descr_of_out_channel oc in
      (try Unix.fchown descr stats.Unix.LargeFile.st_uid stats.Unix.LargeFile.st_gid 
       with e -> lprintf_nl "copy: failed to preserve owner : %s" (Printexc.to_string e));
      (try Unix.fchmod descr stats.Unix.LargeFile.st_perm
       with e -> lprintf_nl "copy: failed to preserve mode : %s" (Printexc.to_string e)));
      let buffer_len = 8192 in
      let buffer = String.create buffer_len in
      let rec copy_file () =
        let n = input ic buffer 0 buffer_len in
        if n = 0 then () else begin 
          output oc buffer 0 n; 
          copy_file () 
        end in
      copy_file ()))
  
let rename oldname newname =
  if oldname <> newname then
  try Unix.rename oldname newname with
    Unix_error(EXDEV,_,_) ->
(* renaming is not enough, we must COPY *)
      lprintf_nl "COPY %s TO %s" oldname newname; 
      let copied = ref false in
      try
        copy oldname newname; 
        copied := true;
        Sys.remove oldname 
      with 
        _ -> 
          if not !copied then
            Sys.remove newname
      
external c_seek64 : Unix.file_descr -> int64 -> Unix.seek_command -> int64 =
  "unix_lseek_64" 
external c_getsize64 : string -> int64 = "ml_getsize64"
external c_getfdsize64 : Unix.file_descr -> int64 = "ml_getfdsize64"
(* c_ftruncate64 sets size, optionally using a sparse file *)
external c_ftruncate64 : Unix.file_descr -> int64 -> bool -> unit =
  "mld_ftruncate_64"
external c_getdtablesize : unit -> int = "ml_getdtablesize"
external c_sizeofoff_t : unit -> int = "ml_sizeofoff_t"

external endianness : unit -> string = "ml_check_endianness"

let rec remove_all_directory dirname =
  let files = list_directory dirname in
  List.iter (fun file ->
      let filename = Filename.concat dirname file in
      if is_directory filename then
        remove_all_directory filename
      else
        Sys.remove filename
  ) files;
  Unix.rmdir dirname

let random () =
  let s = Bytes.create 7 in
  for i = 0 to 6 do
    s.[i] <- char_of_int (97 + Random.int 26)
  done;
  (Bytes.to_string s)

let can_write_to_directory dirname =
  let temp_file = Filename.concat dirname "tmp_" ^ random () ^ "_mld.tmp" in
  let check () = with_remove temp_file (fun _ ->
    tryopen_openfile temp_file [O_WRONLY; O_CREAT] 0o600 (fun fd ->
      let test_string = "mldonkey accesstest - this file can be deleted\n" in
      really_write fd (Bytes.of_string test_string) 0 (String.length test_string)))
  in
  try
    check ()
  with
  | Unix.Unix_error (Unix.EACCES, _, _) ->
      lprintf_nl "can not create files in directory %s, check rights..." dirname; 
      exit 73
  | Unix.Unix_error (Unix.ENOSPC,_,_) ->
      lprintf_nl "directory %s is full..." dirname; 
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      (try
        safe_mkdir dirname;
        check ()
      with _ ->
        lprintf_nl "%s does not exist and can not be created,  exiting..." dirname; 
        exit 73)
  | Unix.Unix_error (error, func, what) -> 
      lprintf_nl "%s(%s) : %s for directory %s" func what (error_message error) dirname;
      exit 73
  | e ->
      lprintf_nl "%s for directory %s" (Printexc2.to_string e) dirname; 
      exit 73

(** The resource type to query or set with [getrlimit] or [setrlimit] *)
type rlimit_resource = RLIMIT_CPU (** CPU time in seconds *)
                       | RLIMIT_FSIZE (** Maximum file size *)
                       | RLIMIT_DATA (** Max data size *)
                       | RLIMIT_STACK (** Max stack size *)
                       | RLIMIT_CORE (** Max core file size *)
                       | RLIMIT_RSS (** Max resident set size *)
                       | RLIMIT_NPROF (** Max number of processes *)
                       | RLIMIT_NOFILE (** Max number of open files *)
                       | RLIMIT_MEMLOCK (** Max locked-in-memory address space *)
                       | RLIMIT_AS (** Address space limit *)

type rlimit = {
  rlim_cur: int;
  rlim_max: int
}

let dummy_rlimit = {
  rlim_cur = -1;
  rlim_max = -1
}

external getrlimit: rlimit_resource -> rlimit = "ml_getrlimit"
external setrlimit: rlimit_resource -> rlimit -> unit = "ml_setrlimit"

let ml_getrlimit resource =
  try
    getrlimit resource
  with _ -> dummy_rlimit

let ml_setrlimit resource n =
  let new_rlimit = {
    rlim_cur = n;
    rlim_max = n
  } in
  try
    setrlimit resource new_rlimit
  with _ -> ()

external fsync : Unix.file_descr -> unit = "ml_fsync"

