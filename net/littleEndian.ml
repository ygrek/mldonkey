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

(* opposite of network order

Big Endian:    strongest byte first (network order)
Little Endian: weakest byte first

  *)

open Autoconf
open Int32ops

  (*
let check_string s pos =
  if check_bounds && pos >= String.length s then
    raise outofboundsaccess
  
let check_array s pos =
  if check_bounds && pos >= Array.length s then
    raise outofboundsaccess
      *)

let const_int32_255 = Int32.of_int 255
let const_int64_255 = Int64.of_int 255

(* int 8 bits *)
  
let buf_int32_8 buf i =
  Buffer.add_char buf (char_of_int (Int32.to_int (
        and32 i const_int32_255)))
  
let buf_int64_8 buf i =
  Buffer.add_char buf (char_of_int (Int64.to_int (
        Int64.logand i const_int64_255)))
      
let get_int32_8 s pos =
  check_string s pos;
  Int32.of_int (int_of_char s.[pos])
  
let get_int64_8 s pos =
  check_string s pos;
  Int64.of_int (int_of_char s.[pos])

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))

let get_int8 s pos = 
  check_string s pos;
  int_of_char s.[pos]

(* int 16 bits *)
  
let buf_int16 buf i =
  Buffer.add_char buf (char_of_int (i land 255));
  Buffer.add_char buf (char_of_int ((i land 65535) lsr 8))

let str_int16 s pos i =
  s.[pos] <- char_of_int (i land 255);
  s.[pos+1] <- char_of_int ((i lsr 8) land 255)

let get_int16 s pos =
  check_string s (pos+1);
  let c1 = int_of_char s.[pos] in
  let c2 = int_of_char s.[pos+1] in
  c1 lor (c2 lsl 8)

(* int 32 bits *)
  
let buf_int32 oc i =
  buf_int32_8 oc i;
  buf_int32_8 oc (right32 i  8);
  buf_int32_8 oc (right32 i  16);
  buf_int32_8 oc (right32 i  24)

let buf_int32_32 = buf_int32
  
let buf_int64 oc i =
  buf_int64_8 oc i;
  buf_int64_8 oc (right64 i  8);
  buf_int64_8 oc (right64 i  16);
  buf_int64_8 oc (right64 i  24);
  
  buf_int64_8 oc (right64 i  32);
  buf_int64_8 oc (right64 i  40);
  buf_int64_8 oc (right64 i  48);
  buf_int64_8 oc (right64 i  56)
  
let get_int32_32 s pos = 
  let c1 = get_int32_8 s pos in
  let c2 = get_int32_8 s (pos+1) in
  let c3 = get_int32_8 s (pos+2) in
  let c4 = get_int32_8 s (pos+3) in
  or32 c1 
    (or32 (left32 c2 8)
    (or32 (left32 c3 16) 
       (left32 c4 24)))

let get_int32 =  get_int32_32

let bits32_64 = Int64.of_string "0xffffffff"
  
let get_int64 s pos = 
  let i1 = get_int32 s pos in
  let i2 = get_int32 s (pos+4) in
  or64 (and64 (Int64.of_int32 i1) bits32_64) (left64 (Int64.of_int32 i2) 32)
  
(* int 31 bits *)
  
let buf_int buf i = 
  buf_int8 buf i;
  buf_int8 buf (i lsr 8);
  buf_int8 buf (i lsr 16);
  buf_int8 buf (i lsr 24)

let str_int s pos i =
  s.[pos] <- char_of_int (i land 255);
  s.[pos+1] <- char_of_int ((i lsr 8) land 255);
  s.[pos+2] <- char_of_int ((i lsr 16) land 255);
  s.[pos+3] <- char_of_int ((i lsr 24) land 255)

let get_int s pos =
  Int32.to_int (get_int32_32 s pos)

(* IP addresses *)
  
let get_ip s pos =
  check_string s (pos+3);
  let c1 = int_of_char s.[pos] in
  let c2 = int_of_char s.[pos+1] in
  let c3 = int_of_char s.[pos+2] in
  let c4 = int_of_char s.[pos+3] in
  Ip.of_ints (c1, c2, c3, c4)

let buf_ip buf ip =
  let (ip0,ip1,ip2,ip3) = Ip.to_ints ip in
  buf_int8 buf ip0;
  buf_int8 buf ip1;
  buf_int8 buf ip2;
  buf_int8 buf ip3

  
let rec get_list_rec get_item s pos len left =
  if len = 0 then List.rev left, pos else
  let (item,pos) = get_item s pos in
  get_list_rec get_item s pos (len-1) (item :: left)
  
let get_list32 get_item s pos =
  let len = get_int s pos in
  get_list_rec get_item s (pos+4) len []

let buf_list32 buf_item b list =
  let len = List.length list in
  buf_int b len;
  List.iter (buf_item b) list

let buf_list = buf_list32
  
let get_list = get_list32

let get_list16 get_item s pos =
  let len = get_int16 s pos in
  get_list_rec get_item s (pos+2) len []

let buf_list16 buf_item b list =
  let len = List.length list in
  buf_int16 b len;
  List.iter (buf_item b) list

let get_list8 get_item s pos =
  let len = get_int8 s pos in
  get_list_rec get_item s (pos+1) len []

let buf_list8 buf_item b list =
  let len = List.length list in
  buf_int8 b len;
  List.iter (buf_item b) list

(* md4 *)
  
let buf_md4 buf s = Buffer.add_string buf (Md4.direct_to_string s)

let get_md4 s pos =
  try Md4.direct_of_string (String.sub s pos 16)  with e ->
      Printf.printf "exception in get_md4 %d" pos; print_newline ();
      raise e

      
let dump_ascii s =
  let len = String.length s in
  Printf.printf "ascii: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      Printf.printf " %c" c
    else
      Printf.printf "(%d)" n
  done;
  Printf.printf "]\n"
      
let dump s =
  let len = String.length s in
  Printf.printf "ascii: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      Printf.printf " %c" c
    else
      Printf.printf "(%d)" n
  done;
  Printf.printf "]\n";
  Printf.printf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    Printf.printf "(%d)" n            
  done;
  Printf.printf "]\n"

let dump_sub s pos len =
  Printf.printf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[pos+i] in
    let n = int_of_char c in
    Printf.printf "(%d)" n            
  done;
  Printf.printf "]\n";
  print_newline ()
