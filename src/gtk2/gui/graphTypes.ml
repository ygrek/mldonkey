(* Copyright 2004 b8_bavard, INRIA *)
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

open CommonTypes

module GStack = struct

  type t = {
    mutable gstack_queue   : (float * float) array;
    mutable gstack_empty   : bool;
    mutable gstack_max_pts : int;
    mutable gstack_inpos   : int;
    mutable gstack_outpos  : int;
    mutable gstack_size    : int;
  }

  type index = {
    nextpos     : bool;
    mutable pos : int;
  }

  exception Empty

  let make max_pts =
    let size = max 1 (max_pts / 5) in {
      gstack_queue   = Array.make size (0.,0.);
      gstack_empty   = true;
      gstack_max_pts = max_pts;
      gstack_inpos   = 0;
      gstack_outpos  = 0;
      gstack_size    = size;
    }

  let shrink q =
    if (q.gstack_inpos - q.gstack_outpos) <= q.gstack_size / 2
    then begin
      let new_size = max 1 ((q.gstack_inpos - q.gstack_outpos) * 140 / 100) in
      let new_size = min q.gstack_max_pts new_size in
      let new_stack = Array.make new_size (0.,0.) in
      Array.blit q.gstack_queue q.gstack_outpos new_stack 0 (q.gstack_inpos - q.gstack_outpos);
      q.gstack_queue <- new_stack;
      q.gstack_inpos <- q.gstack_inpos - q.gstack_outpos;
      q.gstack_outpos <- 0;
      q.gstack_size <- new_size
    end

  let reallocate q =
    if q.gstack_outpos > 0
    then begin
      Array.blit q.gstack_queue q.gstack_outpos q.gstack_queue 0 (q.gstack_inpos - q.gstack_outpos);
      q.gstack_inpos <- q.gstack_inpos - q.gstack_outpos;
      q.gstack_outpos <- 0;
    end;
    if q.gstack_size = q.gstack_max_pts && q.gstack_size = q.gstack_inpos
    then begin
      Array.blit q.gstack_queue 1 q.gstack_queue 0 (q.gstack_inpos - 1);
      q.gstack_inpos <- q.gstack_inpos - 1
    end else begin
      if q.gstack_size - q.gstack_inpos <= q.gstack_size / 5
      then begin
        let new_size = min q.gstack_max_pts (q.gstack_size + q.gstack_max_pts / 5) in
        let new_stack = Array.make new_size (0.,0.) in
        Array.blit q.gstack_queue q.gstack_outpos new_stack 0 (q.gstack_inpos - q.gstack_outpos);
        q.gstack_queue <- new_stack;
        q.gstack_size <- new_size
      end
    end

  let clear q =
    let size = max 1 (q.gstack_max_pts / 5) in
    let new_queue = Array.make size (0.,0.) in
    q.gstack_queue <- new_queue;
    q.gstack_empty <- true;
    q.gstack_inpos <- 0;
    q.gstack_outpos <- 0;
    q.gstack_size <- size

  let put (t, r) q =
    reallocate q;
    q.gstack_queue.(q.gstack_inpos) <- (t,r);
    q.gstack_inpos <- q.gstack_inpos + 1;
    if q.gstack_empty then q.gstack_empty <- false

  let add r q =
    let t = BasicSocket.current_time () in
    put (t, r) q

  let take q =
    if q.gstack_empty then raise Empty;
    let res = q.gstack_queue.(q.gstack_outpos) in
    q.gstack_outpos <- q.gstack_outpos + 1;
    if q.gstack_outpos = q.gstack_inpos then clear q;
    shrink q;
    res

  let peek q =
    if q.gstack_empty
    then raise Empty
    else q.gstack_queue.(q.gstack_outpos)

  let last q =
    if q.gstack_empty
    then raise Empty
    else q.gstack_queue.(q.gstack_inpos - 1)

  let copy q = {
    gstack_queue   = Array.copy q.gstack_queue;
    gstack_empty   = q.gstack_empty;
    gstack_max_pts = q.gstack_max_pts;
    gstack_inpos   = q.gstack_inpos;
    gstack_outpos  = q.gstack_outpos;
    gstack_size    = q.gstack_size;
  }

  let is_empty q = q.gstack_empty

  let length q = q.gstack_inpos - q.gstack_outpos

  let iter f q =
    for i = q.gstack_outpos to q.gstack_inpos - 1 do
      f (Array.unsafe_get q.gstack_queue i)
    done

  let rev_iter f q =
    for i = q.gstack_inpos - 1 downto q.gstack_outpos do
      f (Array.unsafe_get q.gstack_queue i)
    done

  let iteri f q =
    for i = q.gstack_outpos to q.gstack_inpos - 1 do
      f (i - q.gstack_outpos) (Array.unsafe_get q.gstack_queue i)
    done

  let rev_iteri f q =
    for i = q.gstack_inpos - 1 downto q.gstack_outpos do
      f (i - q.gstack_outpos) (Array.unsafe_get q.gstack_queue i)
    done

  let map f q =
    if q.gstack_empty
    then [||]
    else begin
      let r = Array.create (q.gstack_inpos - q.gstack_outpos) (f(Array.unsafe_get q.gstack_queue q.gstack_outpos)) in
      for i = q.gstack_outpos to q.gstack_inpos - 1 do
        Array.unsafe_set r (i - q.gstack_outpos) (f(Array.unsafe_get q.gstack_queue i))
      done;
      r
    end

  let rev_map f q =
    if q.gstack_empty
    then [||]
    else begin
      let r = Array.create (q.gstack_inpos - q.gstack_outpos) (f(Array.unsafe_get q.gstack_queue (q.gstack_inpos - 1))) in
      for i = q.gstack_inpos - 1 downto q.gstack_outpos do
        Array.unsafe_set r (i - q.gstack_outpos) (f(Array.unsafe_get q.gstack_queue i))
      done;
      r
    end

  let mapi f q =
    if q.gstack_empty
    then [||]
    else begin
      let r = Array.create (q.gstack_inpos - q.gstack_outpos) (f 0 (Array.unsafe_get q.gstack_queue q.gstack_outpos)) in
      for i = q.gstack_outpos to q.gstack_inpos - 1 do
        Array.unsafe_set r (i - q.gstack_outpos) (f (i - q.gstack_outpos) (Array.unsafe_get q.gstack_queue i))
      done;
      r
    end

  let rev_mapi f q =
    if q.gstack_empty
    then [||]
    else begin
      let r = Array.create (q.gstack_inpos - q.gstack_outpos) (f 0 (Array.unsafe_get q.gstack_queue (q.gstack_inpos - 1))) in
      for i = q.gstack_inpos - 1 downto q.gstack_outpos do
        Array.unsafe_set r (i - q.gstack_outpos) (f (i - q.gstack_outpos) (Array.unsafe_get q.gstack_queue i))
      done;
      r
    end

  let fold_left f accu q =
    let r = ref accu in
    for i = q.gstack_outpos to q.gstack_inpos - 1 do
      r := f !r (Array.unsafe_get q.gstack_queue i)
    done;
    !r

  let fold_right f accu q =
    let r = ref accu in
    for i = q.gstack_inpos - 1 downto q.gstack_outpos do
      r := f !r (Array.unsafe_get q.gstack_queue i)
    done;
    !r

  (* ordered from the most recently entered to the least recently entered *)
  let to_list_lif q =
    let rec iter l i n =
      if i = n
      then l
      else iter ((Array.unsafe_get q.gstack_queue i) :: l) (i+1) n
    in
    iter [] q.gstack_outpos q.gstack_inpos

  (* ordered from the least recently entered to the most recently entered *)
  let to_list_fif q =
    let rec iter l i n =
      if i < n
      then l
      else iter ((Array.unsafe_get q.gstack_queue i) :: l) (i-1) n
    in
    iter [] (q.gstack_inpos - 1) q.gstack_outpos

  (* l shall be ordered from the least recently entered to the most recently entered *)
  let from_list nbpts l =
    let pos = max 0 (List.length l - nbpts) in
    let _, _l = List2.cut pos l in
    let q = make nbpts in
    List.iter (fun (t,r) ->
      put (t,r) q
    ) _l;
    q

  let get_index_first q =
    if q.gstack_empty
    then None
    else Some {pos = q.gstack_outpos; nextpos = true}

  let get_index_last q =
    if q.gstack_empty
    then None
    else Some {pos = q.gstack_inpos - 1; nextpos = false}

  let iter_next index q =
    let nextpos = index.pos + (match index.nextpos with true -> 1 | false -> (-1)) in
    if nextpos >= q.gstack_outpos && nextpos < q.gstack_inpos
    then begin
      index.pos <- nextpos;
      true
    end else false

  let get_at_index index q =
    if index.pos >= q.gstack_outpos && index.pos < q.gstack_inpos
    then Array.unsafe_get q.gstack_queue index.pos
    else failwith "GStack.t index out of bound"

end

type graph = {
  mutable quarter : GStack.t;
  mutable hour    : GStack.t;
  mutable halfday : GStack.t;
  mutable day     : GStack.t;
  mutable week    : GStack.t;
  mutable month   : GStack.t;
  mutable year    : GStack.t;
}

type graph_record =
  GraphDownloads
| GraphUploads
| GraphFile of (uid_type * graph_record)

type graph_time =
  GraphQuarter
| GraphHour
| GraphHalfDay
| GraphDay
| GraphWeek
| GraphMonth
| GraphYear
