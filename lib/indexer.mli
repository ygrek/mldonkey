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

  
type 'a query =
  And of 'a query * 'a query
| Or of 'a query * 'a query
| AndNot of 'a query * 'a query
| HasWord of string
| HasField of int * string
| Predicate of ('a -> bool)
  
module Make(Doc : sig
      type t
      
      val num : t -> int
      val filtered : t -> bool
      val filter : t -> bool -> unit
    end) : sig
    
    type   index
    
    val print :  index -> unit
    val create : unit ->  index
    val find_docs :  index -> string -> unit
    
    val add :  index -> string ->  Doc.t -> int -> unit
    
    val clear :  index -> unit
    
    val filter_words :  index -> string list -> unit
    val clear_filter :  index -> unit
    val filtered :  Doc.t -> bool
    val query : index -> Doc.t query -> Doc.t array
end