(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

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
