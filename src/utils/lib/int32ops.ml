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

let ( *.. ) x y = Int64.mul x (Int64.of_int y)
let ( ** ) x y = Int64.mul x y
let ( ++ ) x y = Int64.add x y
let ( -- ) x y = Int64.sub x y
let left64 x y = Int64.shift_left x y
let right64 x y = Int64.shift_right_logical x y
let or64 x y = Int64.logor x y
let and64 x y = Int64.logand x y

let zero = Int64.zero
let one = Int64.one
let ( // ) x y = Int64.div x y

  
let const_int32_255 = Int32.of_int 255
let const_int64_255 = Int64.of_int 255

let bits32_64 = Int64.of_string "0xffffffff"
  
let int64_of_uint32 v =
  and64 (Int64.of_int32 v) bits32_64
  
let int32_of_int64 v = Int64.to_int32
  
let megabyte = Int64.of_int (1024 * 1024)
let kilobyte = Int64.of_int 1024
let kilobytes256 = Int64.of_int (256 * 1024)
let kilobytes x = kilobyte *.. x
let megabytes x = megabyte *.. x
let kilobytes64 = kilobytes 64
  