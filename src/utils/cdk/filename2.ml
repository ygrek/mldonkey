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

open String2

  
let win32 = Sys.os_type = "Win32"
let slash = if win32 then '\\' else '/'
let slash_s = String.make 1 slash
  
let normalize filename =
  let l = split filename slash in
  let is_absolute = match l with
      "" :: _ -> true
    | _ -> false
  in
  let rec iter l =
    match l with
      [] -> [], false
    | "" :: l -> iter l
    | "." :: l -> iter l
    | ".." :: l -> let l,_ = iter l in ("..":: l), false
    | _ :: ".." :: l -> 
        let l,_ = iter l in l, true
    | x :: l -> 
        let l, redo = iter l in if redo then iter (x :: l) else (x :: l), false
  in
  let l, _ = iter l in
  let l = 
    if is_absolute then
      let rec iter_abs l =
        match l with
          ".." :: l -> iter_abs l
        | _ -> l
      in
      "" :: (iter_abs l)
    else l
  in
  let file = match l with
      [] -> "."
    | [""] -> slash_s
    | _ -> unsplit l slash
  in
(*  if file <> filename then begin
      lprintf "[%s] normalized to [%s]" filename file; lprint_newline ();
    end; *)
  file
;;

let dirname name =
  let name = normalize name in
  try
    match String.rindex name slash with
      0 -> slash_s
    | n -> String.sub name 0 n
  with Not_found -> "."

let last_extension file =
  try
    let pos = String.rindex file '.' in
    let pos2 = try String.rindex file slash with Not_found -> 0 in 
    if pos < pos2 then raise Not_found;
    String2.after file pos
  with Not_found -> ""

let last_extension2 file =
  try
    let pos = String.rindex file '.' in
    let pos2 = try String.rindex file slash with Not_found -> 0 in 
    if pos < pos2 then raise Not_found;
    String2.after file (pos + 1)
  with Not_found -> ""

let extension file =
  try
    let pos2 = try String.rindex file slash with _ -> 0 in 
    let pos = String.index_from file pos2 '.' in
    let len = String.length file in
    String.sub file pos (len -pos)
  with _ -> ""
      
let extensions file =
  let ext = extension file in
  let len = String.length ext in
  if len > 0 then
    String2.split_simplify (String.sub ext 1 (len-1)) '.'
  else []
      
let from_strings = ref []
let to_strings = ref []

let register_conversions from_string to_string =
  from_strings := from_string :: !from_strings;
  to_strings := to_string :: !to_strings

let from_string filename =
  List.fold_left (fun file f -> f file) filename !from_strings
  
let to_string filename =
  List.fold_left (fun file f -> f file) filename !to_strings

let path_of_filename filename =
  let len = String.length filename in
  let filename = Bytes.of_string filename in
  for i = 0 to len - 1 do
    if Bytes.get filename i = '\\' then Bytes.set filename i '/';
  done;
  let filename = 
    if len > 2 && Bytes.get filename 1 = ':' &&
      match Bytes.get filename 0 with 
        'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false then
      Printf.sprintf "%s/%s" (Bytes.sub_string filename 0 2) 
      (Bytes.sub_string filename 2 (len-2))
    else Bytes.unsafe_to_string filename
  in
  split_simplify filename '/'

let basename filename =
  let rec iter list name =
    match list with [] -> name | name :: tail -> iter tail name
  in
  iter (path_of_filename filename) filename

let filesystem_compliant name fstype namemax =
  (* replace all illegal characters with a valid one.
     assumes all filesystems accept '_'s in filenames *)
  let escape_chars p filename =
    let s = Bytes.of_string filename in
    for i = 0 to String.length filename - 1 do
      if p (Bytes.get s i) then Bytes.set s i '_'
    done;
    Bytes.unsafe_to_string s
  in

  (* remove all illegal characters at the beginning of filename *)
  let trim_left p filename =
    let len = String.length filename in
    let left =
      let rec aux i =
        if i < len && p filename.[i] then aux (i+1) else i in
      aux 0 in
    if left = 0 then filename
    else
      String.sub filename left (len - left) in

  (* remove all illegal characters at the end of filename *)
  let trim_right p filename =
    let len = String.length filename in
    let right =
      let rec aux i =
        if i > 0 && p filename.[i-1] then aux (i-1) else i in
      aux len in
    if right = len then filename
    else
      String.sub filename 0 right in

  let minimal_filter c =
    match c with
      | '/' | '\\' | '<' | '>' | '"' -> true
      | _ -> false in

  let posix_compliant name =
    escape_chars minimal_filter name in

  let windows_compliant name =
    (* http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/creating__deleting__and_maintaining_files.asp *)
    let windows_filter c = 
      minimal_filter c ||
        match c with
          | '*' | '?' | '|' | ':' | '"' -> true
          | _ -> false in

    (* Windows has additional restrictions:
       - filenames cannot start with a '.' 
       - filenames cannot end with '.' or space *)
    let name = trim_left (fun c -> c = '.') name in
    let name = trim_right (fun c -> c = '.' || c = ' ') name in
    escape_chars windows_filter name in

  let macosx_compliant name =
  (* ':' is directory seperator on Mac OS X: http://www.comentum.com/File-Systems-HFS-FAT-UFS.html *)
    let macosx_filter c = 
      minimal_filter c || c = ':' in
    escape_chars macosx_filter name in

  let sys_checked_name =
    match fstype with
    | `Win -> windows_compliant name
    | `Mac -> macosx_compliant name
    | `Posix
    | `Unknown -> posix_compliant name
  in

  let fs_checked_name =
    let remove_last_spaces s =
      let len = String.length s in
      let rec aux n =
        if n = 0 then n
        else
          let n1 = n - 1 in
          if s.[n1] = ' ' then aux n1
          else n in
      let last_space = aux len in
      if last_space = len then s
      else String.sub s 0 last_space
    in
(* FAT filesystems do not allow files with space as last char *)
    match fstype with
    | `Win -> remove_last_spaces sys_checked_name
    | _ -> sys_checked_name
  in

  let length_checked_name =
    if namemax < 1 || String.length sys_checked_name < namemax then
      fs_checked_name
    else
      let ext = extension fs_checked_name in
        if String.length ext > namemax then
          String.sub fs_checked_name 0 namemax
        else
          String.sub fs_checked_name 0 (namemax - (String.length ext)) ^ ext
  in
  length_checked_name

let temp_dir_name () =
  try
    Sys.getenv "MLDONKEY_TEMP"
  with Not_found ->
(* kept for compatibility with Filename.temp_dir_name, this code
   is never reached because $MLDONKEY_TEMP is filled in commonOptions.ml *)
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
      (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
    | _ ->
      (try Sys.getenv "TEMP" with Not_found -> ".")

(* this code is copied from OCaml stdlib/filename.ml but
   extended to respect runtime changes to $MLDONKEY_TEMP,
   OCaml uses variable $TMPDIR/$TEMP instead *)
external open_desc: string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc: int -> unit = "caml_sys_close"

let prng = Random.State.make_self_init ();;

let temp_file_name prefix suffix =
  let rnd = (Random.State.bits prng) land 0xFFFFFF in
  Filename.concat (temp_dir_name ()) (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let temp_file prefix suffix =
  let rec try_name counter =
    let name = temp_file_name prefix suffix in
    try
      close_desc (open_desc name [Open_wronly; Open_creat; Open_excl] 0o600);
      name
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0
  
let _ = (* some assertions on these functions *)
  assert (basename "c:\\Program Files\\Toto history.exe" = "Toto history.exe");
  assert (path_of_filename 
      "c:\\Program Files\\Toto history.exe" = 
    [ "c:"; "Program Files"; "Toto history.exe"] );
  assert (path_of_filename 
      "/home/bidule/mldonkey folder/toto" = 
    [ "home"; "bidule"; "mldonkey folder"; "toto"] );
  assert (path_of_filename 
      "/home//bidule" = ["home"; "bidule"])
  
