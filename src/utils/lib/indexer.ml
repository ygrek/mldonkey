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

(* We need another indexer that would be able to compute lazily
the results of a search, as in Haskell. We would return the first 200
hits, then compute the other ones only if the client asks for. *)

let max_search_results = 1000

type 'a query =
  And of 'a query * 'a query
| Or of 'a query * 'a query
| AndNot of 'a query * 'a query
| HasWord of string
| HasField of int * string
(*
| HasMinField of int * int32
| HasMaxField of int * int32
*)
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
    val size : node -> int
      
    val stats : index -> int
(* 
val min_field : int -> int32 -> doc Intmap.t
val max_field : int -> int32 -> doc Intmap.t
*)  
  end


(****************   SEARCH AND COMPLEX QUERIES ****************)      


module QueryMake(Index: Index) = struct

(* AND entre plusieurs mots, avec un champ specifie par mot *)
    
    open Index
    
    let get_fields tree fields =
(*  lprintf "get_fields"; lprint_newline (); *)
      let map = ref Intmap.empty in 
      or_get_fields map tree fields

(*
      
    let complex_request idx req = 
      try
        let  trees = List.map (fun (s,fields) ->
(*          lprintf "FIND [%s]" s; lprint_newline (); *)
              s, find idx s, fields
          ) req in
        match trees with
          [] ->  []
        | (s, tree, fields) :: tail -> 
            let hits = get_fields tree fields in
            let hits = List.fold_left (fun hits (s, tree, fields) ->
(*              lprintf "AND GET FIELD on [%s]" s; lprint_newline (); *)
                  and_get_fields tree fields hits) hits tail in
            let list = ref [] in
            Intmap.iter (fun _ doc -> 
                if not (Doc.filtered doc) then
                  list := doc :: !list) hits;
(*        lprintf "FOUND %d documents" (List.length !list); 
        lprint_newline (); *)
            !list
      with Not_found -> 
(*      lprintf "Not found"; lprint_newline (); *)
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
    
    let rec query_map idx q =
      try
        match q with
        | HasWord s -> 
            let tree = find idx s in
            get_fields tree (-1) 
        | HasField (fields, s) -> 
(*            lprintf "query_map %d %s" fields s; lprint_newline (); *)
            let tree = find idx s in
            get_fields tree fields
        | And ((Predicate _) as q2, q1) 
        | And (q1, q2) ->
            let map = query_map idx q1 in
            and_query idx q2 map
        | Or (q1, q2) -> 
            let map = query_map idx q1 in
            or_query idx q2 map
        | AndNot (q1, q2) -> 
            let map = query_map idx q1 in
            andnot_query idx q2 map
        | Predicate f -> 
            Intmap.empty
      with _ -> Intmap.empty          
    
    and and_query idx q map =
      try
        if map = Intmap.empty then map else
        match q with
        | HasWord s -> 
            let tree = find idx s in
            and_get_fields tree (-1) map
        | HasField (fields, s) -> 
(*            lprintf "and_query %d %s" fields s; lprint_newline (); *)
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
            purge_map f map
      
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
        | And (Predicate _ as q2, q1) 
        | And (q1, q2) -> 
            let map_and = query_map idx q1 in
            let map_and = and_query idx q2 map_and in
            merge_maps map map_and
        | Or (q1, q2) -> 
            let map = or_query idx q1 map in
            or_query idx q2 map
        | AndNot (q1, q2) -> 
            let map_andnot = query_map idx q1 in
            let map_andnot = andnot_query idx q2 map_andnot in
            merge_maps map map_andnot
        | Predicate f -> 
            map
      
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
      | And (Predicate _ as q2, q1)
      | And (q1, q2) -> 
          let map_not = query_map idx q1 in
          let map_not = and_query idx q2 map_not in
          substract_map map map_not
      | Or(q, Predicate f)
      | Or(Predicate f, q) ->
          let map = purge_map f map in
          andnot_query idx q map
      | Or (q1, q2) -> 
          let map_not = query_map idx q1 in
          let map = substract_map map map_not in
          let map_not = query_map idx q2 in
          substract_map map map_not
      | AndNot (q1, q2) -> 
          let map_not = query_map idx q1 in
          let map_not = andnot_query idx q2 map_not in
          substract_map map map_not          
      | Predicate f ->          
          purge_map (fun x -> not (f x)) map
    
    let exit_exn= Exit
    
    let rec simplify q =
      match q with
        And (q1, q2) -> 
          let q1 = simplify q1 in
          let q2 = simplify q2 in
          simplify1 (And (q1, q2))
      | Or (q1, q2) ->
          let q1 = simplify q1 in
          let q2 = simplify q2 in
          simplify1 (Or (q1, q2))
      | AndNot (q1, q2) ->
          let q1 = simplify q1 in
          let q2 = simplify q2 in
          simplify1 (AndNot (q1, q2))          
      | Predicate _ 
      | HasField _ 
      | HasWord _ -> q
          
    and simplify1 q =
      match q with
        And (Predicate f1, Predicate f2) ->
          Predicate (fun x -> f1 x && f2 x)
      | AndNot (Predicate f1, Predicate f2) ->
          Predicate (fun x -> f1 x && not (f2 x))
      | And (AndNot (q1, q2), q3) ->
          simplify1 (AndNot ( simplify1 (And (q1, q3)), q2))
      | And (Predicate f, q) -> And (q, Predicate f)
      | Or(q1, Or(q2,q3)) -> 
          simplify1 (Or (simplify1 (Or(q1,q2)), q3))
      | And (q1, And(q2, q3)) -> 
          simplify1 (And (simplify1 (And(q1, q2)), q3))
      | And (Or(q1,q2), q3) -> 
          simplify1 (Or(simplify1 (And(q1,q3)), simplify1 (And(q2,q3))))
      | _ -> q
    
    let query idx q =
      let map = query_map idx (simplify q) in
      let count = ref 0 in
      let ele = ref None in
      Intmap.iter (fun num doc ->
          incr count;
          if !count = 1 then ele := Some doc
      ) map;
      
      match !ele with
        None -> [||] 
      | Some doc ->
          let max_docs = min !count max_search_results in
          let array = Array.create max_docs doc in
          count := 0;
          (try
              Intmap.iter (fun num doc ->
                  if !count = max_docs then raise exit_exn;
                  array.(!count) <- doc;
                  incr count;
              ) map;
            with _ -> ());
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
  val size : node -> int  
  
  val stats :  index -> int
end

  
module FullMake (Doc : Doc)(Make:Make) =
  struct
    module Index = Make(Doc) 
    module Query = QueryMake(Index)
    
    type index = Index.index
    
    let query = Query.query
    let query_map = Query.query_map
    let filtered = Index.filtered
    let clear_filter = Index.clear_filter
    let filter_words = Index.filter_words
    let clear = Index.clear
    let add = Index.add
    let create = Index.create
    let stats = Index.stats
  end
