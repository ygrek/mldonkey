(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Sets over ordered types *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val split: elt -> t -> t * bool * t
  end

module Make(Ord: OrderedType) =
  struct
    type elt = Ord.t
    type btree = Empty | Node of btree * elt * btree * int
    type t = {
        tree : btree;
        card : int Lazy.t
      }

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

    let height = function
        Empty -> 0
      | Node(_, _, _, h) -> h

    let rec cardinal_tree = function
        Empty -> 0
      | Node(l, v, r, _) -> cardinal_tree l + 1 + cardinal_tree r

    let t_of_tree t =
      { tree = t;
        card = lazy (cardinal_tree t); }

    let cardinal t =
      Lazy.force t.card

    (* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

    let create l v r =
      let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
      Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

    let bal l v r =
      let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Set2.bal"
        | Node(ll, lv, lr, _) ->
            if height ll >= height lr then
              create ll lv (create lr v r)
            else begin
              match lr with
                Empty -> invalid_arg "Set2.bal"
              | Node(lrl, lrv, lrr, _)->
                  create (create ll lv lrl) lrv (create lrr v r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Set2.bal"
        | Node(rl, rv, rr, _) ->
            if height rr >= height rl then
              create (create l v rl) rv rr
            else begin
              match rl with
                Empty -> invalid_arg "Set2.bal"
              | Node(rll, rlv, rlr, _) ->
                  create (create l v rll) rlv (create rlr rv rr)
            end
      end else
        Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Insertion of one element *)

    let rec add_tree x = function
        Empty -> Node(Empty, x, Empty, 1)
      | Node(l, v, r, _) as t ->
          let c = Ord.compare x v in
          if c = 0 then t else
          if c < 0 then bal (add_tree x l) v r else bal l v (add_tree x r)

    let add x t =
      let tree = add_tree x t.tree in
      {
       tree = tree;
       card = if Lazy.lazy_is_val t.card then
         Lazy.lazy_from_val ((Lazy.force_val t.card) + 1)
       else
         lazy (cardinal_tree tree)
     }

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v r =
      match (l, r) with
        (Empty, _) -> add_tree v r
      | (_, Empty) -> add_tree v l
      | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
          if lh > rh + 2 then bal ll lv (join lr v r) else
          if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)

    let rec min_elt_tree = function
        Empty -> raise Not_found
      | Node(Empty, v, r, _) -> v
      | Node(l, v, r, _) -> min_elt_tree l
            
    let min_elt t =
      min_elt_tree t.tree

    let rec max_elt_tree = function
        Empty -> raise Not_found
      | Node(l, v, Empty, _) -> v
      | Node(l, v, r, _) -> max_elt_tree r

    let max_elt t =
      max_elt_tree t.tree

    (* Remove the smallest element of the given set *)

    let rec remove_min_elt = function
        Empty -> invalid_arg "Set2.remove_min_elt"
      | Node(Empty, v, r, _) -> r
      | Node(l, v, r, _) -> bal (remove_min_elt l) v r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. *)

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> bal t1 (min_elt_tree t2) (remove_min_elt t2)

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> join t1 (min_elt_tree t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x. *)

    let rec split_tree x = function
        Empty ->
          (Empty, false, Empty)
      | Node(l, v, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, true, r)
          else if c < 0 then
            let (ll, pres, rl) = split_tree x l in (ll, pres, join rl v r)
          else
            let (lr, pres, rr) = split_tree x r in (join l v lr, pres, rr)

    let split e t =
      let (ll, pres, rl) = split_tree e t.tree in
      (t_of_tree ll, pres, t_of_tree rl)

    (* Implementation of the set operations *)

    let empty = { tree = Empty; card = Lazy.lazy_from_val 0; }

    let is_empty_tree = function Empty -> true | _ -> false

    let is_empty t = is_empty_tree t.tree

    let rec mem_tree x = function
        Empty -> false
      | Node(l, v, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem_tree x (if c < 0 then l else r)
            
    let mem x t = mem_tree x t.tree

    let singleton x = 
      { tree = Node(Empty, x, Empty, 1);
        card = Lazy.lazy_from_val 1; }

    let rec remove_tree x = function
        Empty -> (Empty, false)
      | Node(l, v, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (merge l r, true) else
          if c < 0 then
            let tree, found = remove_tree x l in
            (bal tree v r, found) 
          else 
            let tree, found = remove_tree x r in
            (bal l v tree, found)

    let remove x t =
      let tree, found = remove_tree x t.tree in
      if found then
        { tree = tree;
          card = if Lazy.lazy_is_val t.card then
            Lazy.lazy_from_val ((Lazy.force_val t.card) - 1)
          else
            lazy (cardinal_tree tree) }
      else t

    let rec union_tree s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
          if h1 >= h2 then
            if h2 = 1 then add_tree v2 s1 else begin
              let (l2, _, r2) = split_tree v1 s2 in
              join (union_tree l1 l2) v1 (union_tree r1 r2)
            end
          else
            if h1 = 1 then add_tree v1 s2 else begin
              let (l1, _, r1) = split_tree v2 s1 in
              join (union_tree l1 l2) v2 (union_tree r1 r2)
            end

    let union s1 s2 =
      t_of_tree (union_tree s1.tree s2.tree)

    let rec inter_tree s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, v1, r1, _), t2) ->
          match split_tree v1 t2 with
            (l2, false, r2) ->
              concat (inter_tree l1 l2) (inter_tree r1 r2)
          | (l2, true, r2) ->
              join (inter_tree l1 l2) v1 (inter_tree r1 r2)

    let inter s1 s2 =
      t_of_tree (inter_tree s1.tree s2.tree)

    let rec diff_tree s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, _), t2) ->
          match split_tree v1 t2 with
            (l2, false, r2) ->
              join (diff_tree l1 l2) v1 (diff_tree r1 r2)
          | (l2, true, r2) ->
              concat (diff_tree l1 l2) (diff_tree r1 r2)

    let diff s1 s2 =
      t_of_tree (diff_tree s1.tree s2.tree)

    type enumeration = End | More of elt * btree * enumeration

    let rec cons_enum s e =
      match s with
        Empty -> e
      | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

    let rec compare_aux e1 e2 =
        match (e1, e2) with
        (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, r1, e1), More(v2, r2, e2)) ->
          let c = Ord.compare v1 v2 in
          if c <> 0
          then c
          else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

    let compare_tree s1 s2 =
      compare_aux (cons_enum s1 End) (cons_enum s2 End)

    let compare s1 s2 =
      compare_tree s1.tree s2.tree

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset_tree s1 s2 =
      match (s1, s2) with
        Empty, _ ->
          true
      | _, Empty ->
          false
      | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
          let c = Ord.compare v1 v2 in
          if c = 0 then
            subset_tree l1 l2 && subset_tree r1 r2
          else if c < 0 then
            subset_tree (Node (l1, v1, Empty, 0)) l2 && subset_tree r1 t2
          else
            subset_tree (Node (Empty, v1, r1, 0)) r2 && subset_tree l1 t2

    let subset s1 s2 =
      subset_tree s1.tree s2.tree

    let rec iter_tree f = function
        Empty -> ()
      | Node(l, v, r, _) -> iter_tree f l; f v; iter_tree f r

    let iter f t =
      iter_tree f t.tree

    let rec fold_tree f s accu =
      match s with
        Empty -> accu
      | Node(l, v, r, _) -> fold_tree f r (f v (fold_tree f l accu))

    let fold f s accu =
      fold_tree f s.tree accu

    let rec for_all_tree p = function
        Empty -> true
      | Node(l, v, r, _) -> p v && for_all_tree p l && for_all_tree p r

    let for_all p t =
      for_all_tree p t.tree

    let rec exists_tree p = function
        Empty -> false
      | Node(l, v, r, _) -> p v || exists_tree p l || exists_tree p r

    let exists p t =
      exists_tree p t.tree

    let filter_tree p s =
      let rec filt accu = function
        | Empty -> accu
        | Node(l, v, r, _) ->
            filt (filt (if p v then add_tree v accu else accu) l) r in
      filt Empty s

    let filter p s =
      t_of_tree (filter_tree p s.tree)

    let partition_tree p s =
      let rec part (t, f as accu) = function
        | Empty -> accu
        | Node(l, v, r, _) ->
            part (part (if p v then (add_tree v t, f) else (t, add_tree v f)) l) r in
      part (Empty, Empty) s

    let partition p s =
      let tree1, tree2 = partition_tree p s.tree in
      (t_of_tree tree1, t_of_tree tree2)

    let rec elements_aux accu = function
        Empty -> accu
      | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

    let elements s =
      elements_aux [] s.tree

    let choose = min_elt 

  end
