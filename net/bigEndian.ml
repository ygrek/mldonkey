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


open Int32ops
  
let const_int32_255 = Int32.of_int 255
let const_int64_255 = Int64.of_int 255

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))

    
let buf_int16 buf i =
  Buffer.add_char buf (char_of_int ((i land 65535) lsr 8));
  Buffer.add_char buf (char_of_int (i land 255))
  
let buf_int32_8 buf i =
  Buffer.add_char buf (char_of_int (Int32.to_int (
        Int32.logand i const_int32_255)))

let buf_int32 oc i =
  buf_int32_8 oc (right32 i  24);
  buf_int32_8 oc (right32 i  16);
  buf_int32_8 oc (right32 i  8);
  buf_int32_8 oc i

let str_int16 s pos i =
  s.[pos+1] <- char_of_int (i land 255);
  s.[pos] <- char_of_int ((i lsr 8) land 255)

let get_int16 s pos =
  let c1 = int_of_char s.[pos+1] in
  let c2 = int_of_char s.[pos] in
  c1 + c2 * 256

  
let buf_int buf i = 
  buf_int8 buf (i lsr 24);
  buf_int8 buf (i lsr 16);
  buf_int8 buf (i lsr 8);
  buf_int8 buf i

      
let get_int32_8 s pos =
  Int32.of_int (int_of_char s.[pos])
    
let get_int32_32 s pos = 
  let c4 = get_int32_8 s pos in
  let c3 = get_int32_8 s (pos+1) in
  let c2 = get_int32_8 s (pos+2) in
  let c1 = get_int32_8 s (pos+3) in
  c1 +. (left32 c2 8) +. (left32 c3 16) +. (left32 c4 24)           
    
let get_int32 = get_int32_32
      
let get_int8 s pos =  int_of_char s.[pos]
    
let get_int s pos = 
  let c4 = get_int8 s pos in
  let c3 = get_int8 s (pos+1) in
  let c2 = get_int8 s (pos+2) in
  let c1 = get_int8 s (pos+3) in
  c1 + (c2 lsl 8) + (c3 lsl 16) + (c4 lsl 24)           
