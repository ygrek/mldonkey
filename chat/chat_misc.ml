(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Misc functions. *)


let remove_blanks str =
  let len = String.length str in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match str.[i] with
      '\000' | ' ' | '\t' | '\r' | '\n' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let remove_newlines str =
  let len = String.length str in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match str.[i] with
      '\r' | '\n' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let buf_get_line buf =
  let s = Buffer.contents buf in
  try
    let n = String.index s '\n' in
    let s1 = String.sub s 0 n in
    let s2 = String.sub s n ((String.length s) - n) in
    Buffer.clear buf;
    Buffer.add_string buf s2;
    match s1 with
      "" -> raise End_of_file
    | _ -> s1
  with
    Not_found ->
      Buffer.clear buf;
      s

let buf_input buf str pos len =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  let s1 = 
    try String.sub s 0 len
    with _ -> raise End_of_file
  in
  let s2 = String.sub s len ((String.length s) - len) in
  Buffer.add_string buf s2;
  for i = 0 to len - 1 do
    str.[pos+i] <- s1.[i]
  done
