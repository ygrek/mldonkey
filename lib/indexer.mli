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
type 'a  index
and 'a doc

val print : 'a index -> unit
val create : unit -> 'a index
val find_docs : 'a index -> string -> unit
val make_doc : 'a index -> 'a -> 'a doc
  
type request = (string * int) list
val complex_request :
  'a index -> request -> ('a doc -> bool) -> 'a doc list
    
val add : 'a index -> string -> 'a doc -> int -> unit
val value : 'a doc -> 'a

val clear : 'a index -> unit

val filter_words : 'a index -> string list -> unit
val clear_filter : 'a index -> unit
val filtered : 'a doc -> bool