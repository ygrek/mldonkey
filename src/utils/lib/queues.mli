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

(* The Queue.t implements a queue where objects are stored with a timestamp.
The module provides different queue behaviors:
  - fifo: First In, First Out
  - lifo: Last In, Fist Out
  - oldest_first: Output the object with the smallest timestamp  first
  - oldest_last: Output the object with the greatest timestamp  first

  *)


module Queue :
  sig
    type 'a t
    val head : 'a t -> int * 'a
    val put : 'a t -> int * 'a -> unit
    val iter : ('a -> unit) -> 'a t -> unit
    val length : 'a t -> int
    val take : 'a t -> int * 'a
    val put_back : 'a t -> int * 'a -> unit
  end
val fifo : unit -> 'a Queue.t
val lifo : unit -> 'a Queue.t
module Make :
  functor (M : sig type t end) ->
  sig
    val oldest_first : unit -> M.t Queue.t
    val oldest_last : unit -> M.t Queue.t
  end

(* [workflow delayed] returns a queue containing objects such that:
- Queue.take returns an object only if [delayed timestamp] returns true
- Queue.put puts the objects with non nul timestamps in a fifo, and
nul timestamps in a lifo. Objects from the lifo are outputed before
objects from the fifo.

This queue can be used to queue objects that have to be examined from
time to time, putting new objects (that need to be examined first) in
the lifo, and old objects (with the last exam timestamp) in the fifo.
The delayed function then returns false if enough time has been spent
after the last exam.
*)
  
val workflow : (int -> bool) -> 'a Queue.t
  
(*  These are useful if you want to implement your own queues, as it
provides the internal type of queues, and a function to translate
the internal type to the Queue.t type. *)
    
type 'a impl = {
    head : (unit -> int * 'a);
    put : (int * 'a -> unit);
    length : (unit -> int);
    take : (unit -> int * 'a);
    iter : ( ('a -> unit) -> unit);
    put_back : (int * 'a -> unit);
  }
  
val of_impl : 'a impl -> 'a Queue.t
  