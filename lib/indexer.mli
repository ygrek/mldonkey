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


module type Doc = sig
      type t
      
      val num : t -> int
      val filtered : t -> bool
      val filter : t -> bool -> unit
    end

  
module type Make = functor (Doc: Doc) ->
  sig    
    type   index
    val create : unit ->  index
    
    val add :  index -> string ->  Doc.t -> int -> unit
    
    val clear :  index -> unit
    
    val filter_words :  index -> string list -> unit
    val clear_filter :  index -> unit
    val filtered :  Doc.t -> bool

    type doc = Doc.t
    type node
    val or_get_fields : Doc.t Intmap.t ref -> node -> int -> Doc.t Intmap.t
    val find : index -> string -> node
    val and_get_fields : node -> int -> Doc.t Intmap.t -> Doc.t Intmap.t

  end


type 'a query =
  And of 'a query * 'a query
| Or of 'a query * 'a query
| AndNot of 'a query * 'a query
| HasWord of string
| HasField of int * string
| Predicate of ('a -> bool)

module type Index = sig
    type index
    type node
    type doc
    val or_get_fields : doc Intmap.t ref -> node -> int -> doc Intmap.t
    val find : index -> string -> node
    val and_get_fields : node -> int -> doc Intmap.t -> doc Intmap.t
  
  end
  
module QueryMake ( Index : Index) : sig
    val query : Index.index -> Index.doc query -> Index.doc array      
  end
  
module FullMake (Doc : Doc) (Make : Make) : sig
    
    type   index
    val create : unit ->  index
    
    val add :  index -> string ->  Doc.t -> int -> unit
    
    val clear :  index -> unit
    
    val filter_words :  index -> string list -> unit
    val clear_filter :  index -> unit
    val filtered :  Doc.t -> bool
    val query : index -> Doc.t query -> Doc.t array          
  end
  
