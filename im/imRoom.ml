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

open ImTypes
open ImProtocol
  
type 'a room_impl = {
    mutable impl_room_num : int;
    mutable impl_room_val : 'a;
    mutable impl_room_ops : 'a room_ops;
  }
  
and 'a room_ops = {
    mutable op_room_protocol : protocol; 
    
(* Send a message to this roomentity *)
    mutable op_room_send : ('a -> string -> unit); 
    
(* Remove this room from my contacts *)
    mutable op_room_remove : ('a -> unit);
    
(* Get the name of this room *)
    mutable op_room_name : ('a -> string);
    
(* Is this room online ? *)
    mutable op_room_status : ('a -> bool);
  }
  
  
let as_room  (room : 'a room_impl) =
  let (room : room) = Obj.magic room in
  room
  
let as_room_impl  (room : room) =
  let (room : 'a room_impl) = Obj.magic room in
  room
  
let room_num room =
  let impl = as_room_impl  room in
  impl.impl_room_num

let dummy_room_impl = {
    impl_room_num = 0;
    impl_room_val = 0;
    impl_room_ops = Obj.magic 0;
  }
  
let dummy_room = as_room dummy_room_impl  

module H = Weak2.Make(struct
      type t = room
      let hash room = Hashtbl.hash (room_num room)
      
      let equal x y  = (room_num x) = (room_num y)
    end)

let room_counter = ref 0
let rooms_by_num = H.create 1027
  
let update_room_num impl =
  if impl.impl_room_num = 0 then begin
      incr room_counter;
      impl.impl_room_num <- !room_counter;
      H.add rooms_by_num (as_room impl);
    end

let room_send room msg =
  let room = as_room_impl room in
  room.impl_room_ops.op_room_send room.impl_room_val msg

let room_remove room =
  let room = as_room_impl room in
  room.impl_room_ops.op_room_remove room.impl_room_val

let room_name room =
  let room = as_room_impl room in
  room.impl_room_ops.op_room_name room.impl_room_val

let room_status room =
  let room = as_room_impl room in
  room.impl_room_ops.op_room_status room.impl_room_val
  
  
let new_room_ops protocol = {
    op_room_protocol = protocol; 
    op_room_send = fni2 protocol "op_room_send";
    op_room_remove = fni protocol "op_room_remove";
    op_room_name = fni protocol "op_room_name";
    op_room_status = fni protocol "op_room_status";
  }
