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


module type Doc = sig
      type t
      
      val num : t -> int
      val filtered : t -> bool
      val filter : t -> bool -> unit
    end


module type Index = sig
    type index
    type node
    type doc
    val or_get_fields : doc Intmap.t ref -> node -> int -> doc Intmap.t
    val find : index -> string -> node
    val and_get_fields : node -> int -> doc Intmap.t -> doc Intmap.t
  
  end


(****************   SEARCH AND COMPLEX QUERIES ****************)      


module QueryMake(Index: Index) = struct

(* AND entre plusieurs mots, avec un champ specifie par mot *)
    
    open Index
    
    let get_fields tree fields =
(*  Printf.printf "get_fields"; print_newline (); *)
      let map = ref Intmap.empty in 
      or_get_fields map tree fields

(*
      
    let complex_request idx req = 
      try
        let  trees = List.map (fun (s,fields) ->
(*          Printf.printf "FIND [%s]" s; print_newline (); *)
              s, find idx s, fields
          ) req in
        match trees with
          [] ->  []
        | (s, tree, fields) :: tail -> 
            let hits = get_fields tree fields in
            let hits = List.fold_left (fun hits (s, tree, fields) ->
(*              Printf.printf "AND GET FIELD on [%s]" s; print_newline (); *)
                  and_get_fields tree fields hits) hits tail in
            let list = ref [] in
            Intmap.iter (fun _ doc -> 
                if not (Doc.filtered doc) then
                  list := doc :: !list) hits;
(*        Printf.printf "FOUND %d documents" (List.length !list); 
        print_newline (); *)
            !list
      with Not_found -> 
(*      Printf.printf "Not found"; print_newline (); *)
          []
*)
    
    let purge_map f map =
      let new_map = ref Intmap.empty in
      Intmap.iter (fun num doc -> 
          if f doc then
            new_map := Intmap.add num doc !new_map) map;
      !new_map
    
    let merge_maps map1 map2 =
      if map1 = Intmap.empty then map2 else
      let new_map = ref map1 in
      Intmap.iter (fun num doc -> 
          if not (Intmap.mem num !new_map) then
            new_map := Intmap.add num doc !new_map) map2;
      !new_map
    
    let substract_map map map_not =
      if map_not = Intmap.empty then map else
      let new_map = ref Intmap.empty in
      Intmap.iter (fun num doc -> 
          if not (Intmap.mem num map_not) then
            new_map := Intmap.add num doc !new_map) map;
      !new_map
    
    let rec query idx q =
      try
        match q with
        | HasWord s -> 
            let tree = find idx s in
            get_fields tree (-1) 
        | HasField (fields, s) -> 
            let tree = find idx s in
            get_fields tree fields
        | And (q1, q2) -> 
            let map = query idx q1 in
            and_query idx q2 map
        | Or (q1, q2) -> 
            let map = query idx q1 in
            or_query idx q2 map
        | AndNot (q1, q2) -> 
            let map = query idx q1 in
            andnot_query idx q2 map
        | Predicate f -> 
            Intmap.empty
      with _ -> Intmap.empty          
          (*
          let map = query idx q in
          purge_map f map
*)
          
    and and_query idx q map =
      try
      if map = Intmap.empty then map else
      match q with
      | HasWord s -> 
          let tree = find idx s in
          and_get_fields tree (-1) map
      | HasField (fields, s) -> 
          let tree = find idx s in
          and_get_fields tree fields map
      | And (q1, q2) -> 
          let map = and_query idx q1 map in
          and_query idx q2 map
      | Or (q1, q2) -> 
          let map1 = and_query idx q1 map in
          let map2 = and_query idx q2 map in
          merge_maps map1 map2
      | AndNot (q1, q2) -> 
          let map = and_query idx q1 map in
          andnot_query idx q2 map
      | Predicate f -> 
          Intmap.empty
          (*
          let map = and_query idx q map in
          purge_map f map
*)
      with _ -> Intmap.empty
          
    and  or_query idx q map =
      try
      match q with
      | HasWord s -> 
          let tree = find idx s in
          or_get_fields (ref map) tree (-1)
      | HasField (fields, s) -> 
          let tree = find idx s in
          or_get_fields (ref map) tree fields
      | And (q1, q2) -> 
          let map_and = query idx q1 in
          let map_and = and_query idx q2 map_and in
          merge_maps map map_and
      | Or (q1, q2) -> 
          let map = or_query idx q1 map in
          or_query idx q2 map
      | AndNot (q1, q2) -> 
          let map_andnot = query idx q1 in
          let map_andnot = andnot_query idx q2 map_andnot in
          merge_maps map map_andnot
      | Predicate f -> 
          Intmap.empty
(*
  let map_f = query idx q in
          let map_f = purge_map f map_f in
          merge_maps map map_f
*)
      with _ -> Intmap.empty
    and andnot_query idx q map =
      match q with
      | HasWord s -> 
          let tree = find idx s in
          let map_not = get_fields tree (-1) in
          substract_map map map_not
      | HasField (fields, s) -> 
          let tree = find idx s in
          let map_not = get_fields tree fields in
          substract_map map map_not
      | And (q1, q2) -> 
          let map_not = query idx q1 in
          let map_not = and_query idx q2 map_not in
          substract_map map map_not
      | Or (q1, q2) -> 
          let map_not = query idx q1 in
          let map = substract_map map map_not in
          let map_not = query idx q2 in
          substract_map map map_not
      | AndNot (q1, q2) -> 
          let map_not = query idx q1 in
          let map_not = andnot_query idx q2 map_not in
          substract_map map map_not
      | Predicate f -> 
          Intmap.empty
(*
          let map_not = query idx q in
          let map_not = purge_map f map_not in
          substract_map map map_not
*)
          
    let query idx q =
      let map = query idx q in
      let count = ref 0 in
      let ele = ref None in
      Intmap.iter (fun num doc ->
          incr count;
          if !count = 1 then ele := Some doc
      ) map;
      match !ele with
        None -> [||] 
      | Some doc ->
      let array = Array.create !count doc in
      count := 0;
      Intmap.iter (fun num doc ->
          array.(!count) <- doc;
          incr count;
      ) map;
      array
      
          
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

  
module FullMake (Doc : Doc)(Make:Make) =
    struct
      module Index = Make(Doc) 
      module Query = QueryMake(Index)
      
      type index = Index.index
      
      let query = Query.query
      let filtered = Index.filtered
      let clear_filter = Index.clear_filter
      let filter_words = Index.filter_words
      let clear = Index.clear
      let add = Index.add
      let create = Index.create
    end
