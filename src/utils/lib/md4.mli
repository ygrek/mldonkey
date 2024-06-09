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

module type Base = sig 
    val to_string : int -> string -> string
    val to_string_case : bool -> int -> string -> string
    val of_string : int -> string -> string
  end
  
module type Digest = sig
    type t

    val null : t
    val one : t
    val two : t

    val equal : t -> t -> bool
      
    val to_string : t -> string
(* [to_string_case upper hash] *)
    val to_string_case : bool -> t -> string
    val of_string : string -> t
      
    val to_bits : t -> string      
      
    val to_hexa : t -> string
    val to_hexa_case : bool -> t -> string
    val of_hexa : string -> t
      
    val to_base32 : t -> string
    val to_base32_case : bool -> t -> string
    val of_base32 : string -> t
    
    val string : string -> t
(*    val file : string -> t *)
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

    val length : int
    val enabled : bool
  end

module Md4 : Digest    
module Md5 : Digest
module Sha1 : Digest
module TigerTree : Digest  
module Tiger : Digest  
module Md5Ext : Digest
  
module Base16 : Base
module Base32 : Base
module Base6427 : Base
  
