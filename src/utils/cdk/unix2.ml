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
  
let list_directory filename =
  let dir = opendir filename in
  let list = ref [] in
  try
    while true do
      let file = readdir dir in 
      if file <> "." && file <> ".." then begin
          list := file :: !list 
        end;
    done;
    assert false
  with _ -> 
      closedir dir;
      !list
  
let iter_directory f dirname =
  let dir = opendir dirname in
  try
    while true do
      let file = readdir dir in 
      if file <> "." && file <> ".." then begin
          f (Filename.concat dirname file)
        end;
    done;
    assert false
  with _ -> 
      closedir dir

let is_directory filename =
  try let s = Unix.stat filename in s.st_kind = S_DIR with _ -> false

let is_link filename =
  try let s = Unix.lstat filename in s.st_kind = S_LNK with _ -> false

let rec safe_mkdir dir =  
  if Sys.file_exists dir then begin
      if not (is_directory dir) then 
        failwith (Printf.sprintf "%s not a directory" dir)
    end
  else 
  if is_link dir then
    failwith (Printf.sprintf "%s is an orphan symbolic link" dir)
  else begin
      let predir = Filename.dirname dir in
      if predir <> dir then safe_mkdir predir;
(*      if dir <> "." then *) Unix.mkdir dir 0o775
    end    
    
    
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
  let ic = open_in_bin oldname in
  let oc = open_out_bin newname in
  let buffer_len = 8192 in
  let buffer = String.create buffer_len in
  let rec copy_file () =
    let n = input ic buffer 0 buffer_len in
    if n = 0 then () else begin output oc buffer 0 n; copy_file () end in
  copy_file ();
  close_in ic;
  close_out oc
  
  
let rename oldname newname =
  if oldname <> newname then
  try Unix.rename oldname newname with
    Unix_error(EXDEV,_,_) as e ->
(* renaming is not enough, we must COPY *)
      lprintf "COPY %s TO %s\n" oldname newname; 
      let copied = ref false in
      try
        copy oldname newname; 
        copied := true;
        Sys.remove oldname 
      with 
        e -> 
          if not !copied then
            Sys.remove newname
      
external c_seek64 : Unix.file_descr -> int64 -> Unix.seek_command -> int64 =
  "unix_lseek_64" 
external c_getsize64 : string -> int64 = "ml_getsize64"
external c_ftruncate64 : Unix.file_descr -> int64 -> unit =
  "mld_ftruncate_64"
external c_getdtablesize : unit -> int = "ml_getdtablesize"
external c_sizeofoff_t : unit -> int = "ml_sizeofoff_t"

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