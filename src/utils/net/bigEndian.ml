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

(***** Network order ******)

open Autoconf
open Int64ops
  
let const_int32_255 = Int32.of_int 255
let const_int64_255 = Int64.of_int 255

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))

    
let buf_int16 buf i =
  Buffer.add_char buf (char_of_int ((i land 65535) lsr 8));
  Buffer.add_char buf (char_of_int (i land 255))
    
let buf_int24 buf i =
  Buffer.add_char buf (char_of_int ((i lsr 16) land 255));
  Buffer.add_char buf (char_of_int ((i lsr 8) land 255));
  Buffer.add_char buf (char_of_int (i land 255))
  
let buf_int32_8 buf i =
  Buffer.add_char buf (char_of_int (Int32.to_int (
        Int32.logand i const_int32_255)))

  (*
let buf_int32 oc i =
  buf_int32_8 oc (right32 i  24);
  buf_int32_8 oc (right32 i  16);
  buf_int32_8 oc (right32 i  8);
  buf_int32_8 oc i
    *)

let str_int16 s pos i =
  s.[pos+1] <- char_of_int (i land 255);
  s.[pos] <- char_of_int ((i lsr 8) land 255)

let get_int16 s pos =
  check_string s (pos+1);  
  let c1 = int_of_char s.[pos+1] in
  let c2 = int_of_char s.[pos] in
  c1 + c2 lsl 8

let str_int24 s pos i =
  s.[pos+2] <- char_of_int (i land 255);
  s.[pos+1] <- char_of_int ((i lsr 8) land 255);
  s.[pos] <- char_of_int ((i lsr 16) land 255)

let get_int24 s pos =
  check_string s (pos+1);  
  let c0 = int_of_char s.[pos+2] in
  let c1 = int_of_char s.[pos+1] in
  let c2 = int_of_char s.[pos] in
  c0 + c1 lsl 8 + c2 lsl 16

  
let buf_int buf i = 
  buf_int8 buf (i lsr 24);
  buf_int8 buf (i lsr 16);
  buf_int8 buf (i lsr 8);
  buf_int8 buf i

let str_int s pos i =
  s.[pos+3] <- char_of_int (i land 255);
  s.[pos+2] <- char_of_int ((i lsr 8) land 255);
  s.[pos+1] <- char_of_int ((i lsr 16) land 255);
  s.[pos] <- char_of_int ((i lsr 24) land 255)

(*      
let get_int32_8 s pos =
  check_string s pos;
  Int32.of_int (int_of_char s.[pos])

let internal_get_int32_32 s pos = 
  let c4 = get_int32_8 s pos in
  let c3 = get_int32_8 s (pos+1) in
  let c2 = get_int32_8 s (pos+2) in
  let c1 = get_int32_8 s (pos+3) in

  *)

let get_int8 s pos =  
  check_string s pos;
  int_of_char s.[pos]
    
let get_int s pos = 
  let c4 = get_int8 s pos in
  let c3 = get_int8 s (pos+1) in
  let c2 = get_int8 s (pos+2) in
  let c1 = get_int8 s (pos+3) in
  c1 + (c2 lsl 8) + (c3 lsl 16) + (c4 lsl 24)           

  
let buf_int64_8 buf i =
  Buffer.add_char buf (char_of_int (Int64.to_int (
        Int64.logand i const_int64_255)))

let get_uint64_32 s pos =
  let c4 = get_int8 s pos in
  let c3 = get_int8 s (pos+1) in
  let c2 = get_int8 s (pos+2) in
  let c1 = get_int8 s (pos+3) in
  let start = Int64.of_int (c1 + (c2 lsl 8) + (c3 lsl 16)) in
  (left64 (Int64.of_int c4) 24) ++ start
  
     
let buf_int64_32 oc i =
  buf_int64_8 oc (right64 i  24);
  buf_int64_8 oc (right64 i  16);
  buf_int64_8 oc (right64 i  8);  
  buf_int64_8 oc i

  
let get_ip s pos =
  check_string s (pos+3);
  let c1 = int_of_char s.[pos] in
  let c2 = int_of_char s.[pos+1] in
  let c3 = int_of_char s.[pos+2] in
  let c4 = int_of_char s.[pos+3] in
  Ip.of_ints (c4, c3, c2, c1)
