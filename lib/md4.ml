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


module type Digest = sig
    type t
    
    val null : t
    val one : t
    val two : t
    val to_string : t -> string
    val of_string : string -> t
    
    val string : string -> t
    val file : string -> t
    val create : unit -> t
    val direct_of_string : string -> t
    val direct_to_string : t -> string
    val random : unit -> t
    
    val digest_subfile : Unix32.t -> int64 -> int64 -> t
    
    val option : t Options.option_class
    
    val xor : t -> t -> t
    val value_to_hash : Options.option_value -> t
    val hash_to_value : t -> Options.option_value
      
    val up : t -> int
    val up2 : t -> int
    val up3 : t -> int

  end


module Make(M: sig
      val hash_length : int
      val hash_name : string
        
      val unsafe_string : string -> string -> int -> unit
      val unsafe_file : string -> string -> unit
      val digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit 

    end) = struct
    open M
open Misc

type t = string
  
let null = String.make hash_length '\000'
let one = String.make hash_length '\001'
let two =  String.make hash_length '\002'
  
let to_string s =
  let p = String.create (hash_length * 2) in
  for i = 0 to hash_length - 1 do
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
  assert (String.length s = hash_length*2);
  let p = String.create hash_length in
  for i = 0 to hash_length - 1 do
    let c0 = s.[2*i] in
    let c1 = s.[2*i+1] in
    p.[i] <- char_of_int ((16 * digit_hexa c0) + digit_hexa c1);
  done;
  p
  
let string s =
  let len = String.length s in
  let digest = String.create hash_length in
  unsafe_string digest s len;
  digest

external xor_c : t -> t -> t -> unit = "md4_xor" "noalloc"
  
let xor m1 m2 =
  let m3 = String.create hash_length in
  xor_c m1 m2 m3;
  m3
  
let file s =
  let digest = String.create hash_length in
  unsafe_file digest s;
  digest
  
let digest_subfile fd pos len =
  let digest = String.create hash_length in
  digest_subfile digest (Unix32.force_fd fd) pos len;
  digest
  
let create () =  String.create hash_length
  
let direct_to_string s = s
let direct_of_string s = s
  
let random () =
  let s = create () in
  for i = 0 to hash_length - 1 do
    s.[i] <- char_of_int (Random.int 256)
  done;
  s
  
open Options

let value_to_hash v = of_string (value_to_string v)

let hash_to_value v = string_to_value (to_string v)
  
let option =
  define_option_class hash_name value_to_hash hash_to_value
    
 
  let up s = int_of_char s.[0]
  let up2 s = (int_of_char s.[0])*256+(int_of_char s.[1])
  let up3 s = (int_of_char s.[0])*65536+(int_of_char s.[1])*256+(int_of_char s.[2])
  
  
end

module Md4 = Make(struct
      let hash_length = 16
      let hash_name = "Md4"        
      
      external unsafe_string : string -> string -> int -> unit = "md4_unsafe_string"
      external unsafe_file : string -> string -> unit = "md4_unsafe_file"
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
        "md4_unsafe64_fd"
    
    end)
  
module Md5 = Make(struct
      let hash_length = 16
      let hash_name = "Md5"        
      
      external unsafe_string : string -> string -> int -> unit = "md5_unsafe_string"
      external unsafe_file : string -> string -> unit = "md5_unsafe_file"
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
        "md5_unsafe64_fd"
    
    end)
  