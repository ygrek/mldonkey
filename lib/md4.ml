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
open Misc

type t = string
  
let null = String.make 16 '\000'
let one = String.make 16 '\001'
  
let to_string s =
  let p = String.create 32 in
  for i = 0 to 15 do
    let c = s.[i] in
    let n = int_of_char c in
    let i0 = (n/16) land 15 in
    let i1 = n land 15 in
    p.[2 * i] <- hexa_digit i0;
    p.[2 * i+1] <- hexa_digit i1;
  done;
  p

let i_a = int_of_char 'a'  
let i_A = int_of_char 'A'  
let i_f = int_of_char 'f'  
let i_F = int_of_char 'F'  
let i_0 = int_of_char '0'
let i_9 = int_of_char '9'

let digit_hexa c =
  let i = int_of_char c in
  if i >= i_a && i <= i_f then i - i_a + 10 else
  if i >= i_A && i <= i_F then i - i_A + 10 else
  if i >= i_0 && i <= i_9 then i - i_0 else
    failwith "Bad hexa char"
    
let of_string s =
  assert (String.length s = 32);
  let p = String.create 16 in
  for i = 0 to 15 do
    let c0 = s.[2*i] in
    let c1 = s.[2*i+1] in
    p.[i] <- char_of_int ((16 * digit_hexa c0) + digit_hexa c1);
  done;
  p

external unsafe_string : string -> string -> int -> unit = "md4_unsafe_string"
external unsafe_file : string -> string -> unit = "md4_unsafe_file"
external digest_subfile : t -> Unix.file_descr -> int32 -> int32 -> unit =
  "md4_unsafe_fd"
  
let string s =
  let len = String.length s in
  let digest = String.create 16 in
  unsafe_string digest s len;
  digest

external xor_c : t -> t -> t -> unit = "md4_xor" "noalloc"
  
let xor m1 m2 =
  let m3 = String.create 16 in
  xor_c m1 m2 m3;
  m3
  
let file s =
  let digest = String.create 16 in
  unsafe_file digest s;
  digest
  
let digest_subfile fd pos len =
  Printf.printf "MD4 OF %ld+%ld" pos len; print_newline ();
  let digest = String.create 16 in
  digest_subfile digest (Unix32.force_fd fd) pos len;
  digest
  
let create () =
  String.create 16
  
let direct_to_string s = s
let direct_of_string s = s
  
let random () =
  let s = create () in
  for i = 0 to 15 do
    s.[i] <- char_of_int (Random.int 256)
  done;
  s
  
open Options
  
module Md4Option = struct
    
    let value_to_md4 v = (*
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          
          let file_md4_name = 
            try
              get_value "file_md4" value_to_string
            with _ -> failwith "Bad file_md4"
          in
          Md4.of_string file_md4_name
          
      | _ -> *) of_string (value_to_string v)
          
    let md4_to_value v = string_to_value (to_string v)
    
    let t =
      define_option_class "Md4" value_to_md4 md4_to_value
    ;;
  end

let option = Md4Option.t