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

type key = int

type 'a t =
  Empty
| Node of 'a t * key * 'a * 'a t * int

let empty = Empty

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node(ll, lv, ld, lr, _) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
    end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let rec add x data = function
    Empty ->
      Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
      if x = v then
        Node(l, x, data, r, h)
      else if x < v then
        bal (add x data l) v d r
      else
        bal l v d (add x data r)

let rec find x = function
    Empty ->
      raise Not_found
  | Node(l, v, d, r, _) ->
      if x = v then d
      else find x (if x < v then l else r)

let rec mem x = function
    Empty ->
      false
  | Node(l, v, _d, r, _) ->
      x = v || mem x (if x < v then l else r)

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, d1, r1, _h1), Node(l2, v2, d2, r2, _h2)) ->
      bal l1 v1 d1 (bal (merge r1 l2) v2 d2 r2)

let rec remove x = function
    Empty ->
      Empty
  | Node(l, v, d, r, _h) ->
      if x = v then
        merge l r
      else if x < v then
        bal (remove x l) v d r
      else
        bal l v d (remove x r)

let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      iter f l; f v d; iter f r

let rec map f = function
    Empty               -> Empty
  | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

let rec mapi f = function
    Empty               -> Empty
  | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
      fold f l (f v d (fold f r accu))

      
let rec length_aux len = function
    Empty -> len
  | Node(l, _v, _d, r, _) ->
      length_aux (length_aux (1+len) l) r

let length map = 
  length_aux 0 map
  
let top = function
    Empty -> raise Not_found
  | Node(_l, v, d, _r, _) -> v, d

let rec infix_nth map res =
  match res, map with
    (_count, Some _), _ -> res
  | (_count, None), Empty -> res
  | (_count, None), Node(l, _v, d, r, _) ->
      infix_nth r (match infix_nth l res with
          (_count, Some _) as res -> res
        | (count, None) -> 
            if count = 0 then (count, Some d)
            else (count - 1, None)) 

let nth map n =
  match infix_nth map (n, None) with
    (_, None) -> raise Not_found
  | (_, Some node) -> node
      
let to_list map =
  let list = ref [] in
  iter (fun _ v -> list := v :: !list) map;
  !list
  
