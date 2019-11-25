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

type 'a variable
type 'a arrow
 
val text_option : 'a arrow ->
  ('a, unit, string) format Options.option_class
  
(*  
let nshared = 
  (arrow_variable int_variable int32_variable) 
  "Shared: %d/%ld"
    *)

val save_strings : unit -> unit
val set_strings_file : string -> unit

val _b : string -> ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4
val _s : string -> string -> string

type 'a _string

val bb_ : string -> ('a, 'b, 'c) format -> ('a, 'b, 'c) format _string
val _bb : ('a, 'b, 'c) format _string -> ('a, 'b, 'c) format

val ss_ : string -> string -> string _string    
val _ss : string _string -> string      

  
module T :
  sig
    val int : 'b arrow -> (int -> 'b) arrow
    val char : 'b arrow -> (char -> 'b) arrow
    val string :  'b arrow -> (string -> 'b) arrow
    val float :  'b arrow -> (float -> 'b) arrow
    val bool :  'b arrow -> (bool -> 'b) arrow
    val int32 :  'b arrow -> (int32 -> 'b) arrow
    val int64 :  'b arrow -> (int64 -> 'b) arrow
    val nativeint :  'b arrow -> (nativeint -> 'b) arrow
    val format : string arrow
    val bformat : unit arrow
    val option : 'a arrow ->
      ('a, unit, string) format Options.option_class
    val boption : 'a arrow ->
      ('a, Buffer.t, unit) format Options.option_class
  end

val gettext : ('a, unit, string) format Options.option_record -> 'a
val buftext : Buffer.t -> ('a, Buffer.t, unit) format Options.option_record -> 'a
