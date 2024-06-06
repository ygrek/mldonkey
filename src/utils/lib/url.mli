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


type url = {
    proto : string;
    server : string;
    port : int;
    full_file : string;
    short_file : string;
    user : string;
    passwd : string;
    args : (string * string) list;
    
    string : string;
  }
  
  (*
val create  :    ?proto:string ->
    ?server:string ->
    ?port:int -> ?user:string -> ?pass:string -> string -> url
    (*d create an already parsed url *)
*)
  
val of_string : ?args:(string * string) list -> string -> url
    (*d [of_string s] returns the url corresponding to the given [s].
       this string should start by ["http://"]. [raise Invalid_argument]
       if the string is not an url *)

val to_string : url -> string
(*d [to_string u] returns a string representing u.  *)

val to_string_no_args : url -> string
(*d [to_string u] returns a string representing u.  *)

val cut_args : string -> (string * string) list
val put_args : string -> (string * string) list -> string

  
val encode : string -> bytes
val encode_to_string: string -> string
val decode : ?raw:bool -> string -> string

val option : url Options.option_class
  