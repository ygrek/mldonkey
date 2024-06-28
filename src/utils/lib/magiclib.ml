(* File: magic.ml

   Copyright (C) 2005

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://www.umh.ac.be/math/an/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* 	$Id$	 *)



exception Failure of string

let () = Callback.register_exception "Magiclib.Failure" (Failure "message")

type t (* hold magic_t *)

external magic_open : int -> t = "ocaml_magic_open"
external close : t -> unit = "ocaml_magic_close"
external magic_file : t -> string -> string = "ocaml_magic_file"
external magic_buffer : t -> string -> int -> string = "ocaml_magic_buffer"
external magic_setflags : t -> int -> unit = "ocaml_magic_setflags"
external magic_check_default : t -> bool = "ocaml_magic_check_default"
external magic_check : t -> string -> bool = "ocaml_magic_check"
external magic_compile_default : t -> unit = "ocaml_magic_compile_default"
external magic_compile : t -> string -> unit = "ocaml_magic_compile"
external magic_load_default : t -> unit = "ocaml_magic_load_default"
external magic_load : t -> string -> unit = "ocaml_magic_load"

type flag =
  | Symlink
  | Compress
  | Devices
  | Mime
  | Continue
  | Check    (* => flush stderr for all funs.  FIXME *)
  | Preserve_atime
  | Raw

(* WARNING: Keep in sync with magic.h *)
let int_of_flag = function
  | Symlink	-> 0x002
  | Compress	-> 0x004
  | Devices	-> 0x008
  | Mime	-> 0x010
  | Continue	-> 0x020
  | Check	-> 0x040
  | Preserve_atime -> 0x080
  | Raw		-> 0x100

let int_of_flags flags =
  List.fold_left (fun fs f -> fs lor (int_of_flag f)) 0x000 flags


(* FIXME: is this escaping correct for libmagic??? *)
let escape_colon s =
  let len = String.length s in
  let n = ref 0 in
  for i = 0 to len - 1 do if s.[i] = ':' then incr n done;
  if !n = 0 then s else
    let s' = Bytes.create (len + !n) in
    let j = ref 0 in
    for i = 0 to len - 1 do
      if s.[i] = ':' then (s'.[!j] <- '\\'; incr j);
      s'.[!j] <- s.[i]; incr j
    done;
    Bytes.unsafe_to_string s'

(* Concatenate the filenames with ":".  If ":" is present in a
   filename, escape it. *)
let concat filenames =
  String.concat ":" (List.map escape_colon filenames)

let load cookie = function
  | [] -> magic_load_default cookie
  | filenames -> magic_load cookie (concat filenames)

let compile cookie = function
  | [] -> magic_compile_default cookie
  | filenames -> magic_compile cookie (concat filenames)

let check cookie = function
  | [] -> magic_check_default cookie
  | filenames -> magic_check cookie (concat filenames)


let create ?(flags=[]) filenames =
  let cookie = magic_open(int_of_flags flags) in
  load cookie filenames;
  cookie

let setflags cookie flags =
  magic_setflags cookie (int_of_flags flags)

let file cookie filename =
  (* FIXME: For a strange reason the toplevel loops with an error "I/O
     error: Bad file descriptor" when querying an unexisting file or a
     char device,...  (this works fine when compiled). *)
  magic_file cookie filename

let buffer cookie ?len s =
  let len = match len with
  | None -> String.length s
  | Some l ->
      if 0 <= l && l <= String.length s then l
      else invalid_arg(Printf.sprintf "Magiclib.buffer: len=%i not in [0, %i]"
                         l (String.length s)) in
  magic_buffer cookie s len
