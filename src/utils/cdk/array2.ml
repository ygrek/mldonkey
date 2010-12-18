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

let min a =
  if a = [||] then raise Not_found;
  let min_val = ref a.(0) in
  for i = 1 to Array.length a - 1 do
    if a.(i) < !min_val then min_val := a.(i)
  done;
  !min_val
  
let max a =
  if a = [||] then raise Not_found;
  let max_val = ref a.(0) in
  for i = 1 to Array.length a - 1 do
    if a.(i) > !max_val then max_val := a.(i)
  done;
  !max_val
  
let exists p a =
  let l = Array.length a in
  let rec aux i = (i < l) && (p a.(i) || aux (i+1)) in
  aux 0

let existsi p a =
  let l = Array.length a in
  let rec aux i = (i < l) && (p i a.(i) || aux (i+1)) in
  aux 0

(* based on Array.fold_left code *)
let fold_lefti f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f !r i (Array.unsafe_get a i)
  done;
  !r

let subarray_fold_lefti f x a firstidx lastidx =
  let len = Array.length a in
  assert(firstidx >= 0 && firstidx < len);
  assert(lastidx >= 0 && lastidx < len);
  let r = ref x in
  for i = firstidx to lastidx do
    r := f !r i (Array.unsafe_get a i)
  done;
  !r

(** Fisher-Yates shuffle *)
let shuffle a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i+1) in
    let tmp = a.(j) in
    a.(j) <- a.(i);
    a.(i) <- tmp
  done

