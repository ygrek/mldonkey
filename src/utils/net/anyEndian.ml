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
(* opposite of network order

Big Endian:    strongest byte first (network order)
Little Endian: weakest byte first

  *)
open Md4
open Autoconf

    
external get_byte: string -> int -> int = "%string_safe_get"
external set_byte: string -> int -> int -> unit = "%string_safe_set"

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))

let get_uint8 s pos = 
  check_string s pos;
  int_of_char s.[pos]

let get_uint8_bytes b pos =
  int_of_char (Bytes.get b pos)
  (*
let buf_int32_8 buf i =
  Buffer.add_char buf (char_of_int (Int32.to_int (
        and32 i 0xffl)))
      
let get_int32_8 s pos =
  check_string s pos;
  Int32.of_int (int_of_char s.[pos])
  *)

let buf_int64_8 buf i =
  Buffer.add_char buf (char_of_int (Int64.to_int (
        Int64.logand i 0xffL)))
  
let get_int64_8 s pos =
  check_string s pos;
  Int64.of_int (int_of_char s.[pos])


let rec get_list_rec get_item s pos len left =
  if len = 0 then List.rev left, pos else
  let (item,pos) = get_item s pos in
  get_list_rec get_item s pos (len-1) (item :: left)
  
let get_list8 get_item s pos =
  let len = get_uint8 s pos in
  get_list_rec get_item s (pos+1) len []

let buf_list8 buf_item b list =
  let len = List.length list in
  buf_int8 b len;
  List.iter (buf_item b) list

let buf_sha1 buf s = Buffer.add_string buf (Sha1.direct_to_string s)

let get_sha1 s pos =
  try Sha1.direct_of_string (String.sub s pos 20)  
  with e ->
(*      lprintf "exception in get_sha1 %d s=%s\n" pos (String.escaped s); *)
      raise e
      
let buf_md4 buf s = Buffer.add_string buf (Md4.direct_to_string s)

let get_md4 s pos =
  try Md4.direct_of_string (String.sub s pos 16)  
  with e ->
(*    lprintf "exception in get_md4 %d s=%s\n" pos (String.escaped s); *)
    raise e

let buf_string8 buf s =
  buf_int8 buf (String.length s);
  Buffer.add_string buf s

let get_string8 s pos =
  let len = get_uint8 s pos in
  String.sub s (pos+1) len, pos+1+len

let bdump_hex buf s =
  let len = String.length s in
  let asc = Buffer.create 16 in
  let hex = Buffer.create 50 in
  let rec iter i =
    if i = len then begin
      if Buffer.length asc > 0 then begin
        let fill = String.make (50 - (Buffer.length hex )) ' ' in
        Printf.bprintf buf "%s%s|%-16s|\n" (Buffer.contents hex) fill (Buffer.contents asc);
      end
    end
    else begin
      if i mod 16 = 0 then begin
        if i > 0
          then Printf.bprintf buf "%s|%-16s|\n" (Buffer.contents hex) (Buffer.contents asc);
        Printf.bprintf buf "%08x: " i;
        Buffer.clear asc;
        Buffer.clear hex;
      end;
      let c = s.[i] in
      let ioc = int_of_char c in
      let bc = if ioc > 32 && ioc < 127 then c else '.' in
      Buffer.add_char asc bc;
      Buffer.add_string hex (Printf.sprintf "%02x " ioc);
      if (i + 1) mod 8 = 0 then Buffer.add_char hex ' ';
      iter (i + 1);
    end;
  in
  iter 0

let dump_hex_s s =
  let buf = Buffer.create (String.length s * 4) in
  bdump_hex buf s;
  Buffer.contents buf

let dump_hex s =
  lprintf "%s" (dump_hex_s s)

let bdump_ascii buf s =
  let len = String.length s in
  Printf.bprintf buf "ascii: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      Printf.bprintf buf " %c" c
    else
      Printf.bprintf buf "(%d)" n
  done;
  Printf.bprintf buf "]\n"

let dump_ascii s =
  let buf = Buffer.create 1000 in
  bdump_ascii buf s;
  lprintf "%s" (Buffer.contents buf)

let bdump_dec buf s = 
  let len = String.length s in
  Printf.bprintf buf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    Printf.bprintf buf "(%d)" n            
  done;
  Printf.bprintf buf "]\n"
        
let dump_dec s =
  let buf = Buffer.create 1000 in
  bdump_dec buf s;
  lprintf "%s" (Buffer.contents buf)

let dump s =
  dump_ascii s;
  dump_dec s;
  dump_hex s

let bdump_sub buf s pos len =
  Printf.bprintf buf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[pos+i] in
    let n = int_of_char c in
    Printf.bprintf buf "(%d)" n            
  done;
  Printf.bprintf buf "]\n\n"

let dump_sub s pos len =
  let buf = Buffer.create 1000 in
  bdump_sub buf s pos len;
  lprintf "%s" (Buffer.contents buf)

let bdump buf s =
  bdump_ascii buf s;
  bdump_dec buf s;
  bdump_hex buf s

let sdump s =
  let buf = Buffer.create 1000 in
  bdump buf s;
  Buffer.contents buf

let dump s =
  lprintf "%s" (sdump s)
