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

val dirname : string -> string 
(*d [dirname filename] returns the dirname of [filename] after normalization *)

val normalize : string -> string
(*d [normalize filename] returns a normalized name for [filename], where no
  "//", "<name>/.." or "/./" are present. *)

val extension : string -> string
(*d [extension filename] returns the longest extension of [filename], 
which is the substring after the first "." in [filename] (including
  the ".") *)

val last_extension : string -> string
(*d [last_extension filename] returns the last extension of [filename], 
which is the substring after the last "." in [filename] 
  (including the "."). If no extension is present, returns "". *)

val extensions : string -> string list
(*d [extensions filename] returns the list of extensions (without the dot)
  found at the end of filename. *)
  
val register_conversions: (string -> string) -> (string -> string) -> unit
(*d [register_conversions from_string to_string] registers two conversion
functions for filenames. The first [from_string] one converts strings 
  (from the user) to 
filenames (for system use) , whereas the second [to_string]
  converts filenames to strings.
*)
  
val from_string : string -> string
(*d [from_string str] converts the [str] string to a filename using reistered
conversion functions. *)
  
val to_string : string -> string 
(*d [to_string filename] converts [filename] to a string using registered
conversion functions. *)
  
val shorten : int -> string -> string
  
(* transform a filename in a list of dirname *)
val path_of_filename : string -> string list
  
val basename : string -> string
  