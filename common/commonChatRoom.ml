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

open Options
open CommonTypes
  
type 'a room_impl = {
    mutable impl_room_update : bool;
    mutable impl_room_state : room_state;
    mutable impl_room_val : 'a;
    mutable impl_room_num : int;
    mutable impl_room_ops : 'a room_ops;
  }
  
and 'a room_ops = {
    mutable op_room_close : ('a -> unit);
    mutable op_room_pause : ('a -> unit);
    mutable op_room_resume : ('a -> unit);
    mutable op_room_messages : ('a -> room_message list);
    mutable op_room_users : ('a -> user list);
    mutable op_room_name : ('a -> string);
    mutable op_room_info : ('a -> Gui_proto.room_info);
    mutable op_room_send_message : ('a -> room_message -> unit);
  }
  
let room_counter = ref 0
let rooms_by_num = Hashtbl.create 1027
  
let ni n m = 
  let s = Printf.sprintf "Room.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m =   failwith (ni n m)
let ni_ok n m = ignore (ni n m)

let as_room  (room : 'a room_impl) =
  let (room : room) = Obj.magic room in
  room
  
let as_room_impl  (room : room) =
  let (room : 'a room_impl) = Obj.magic room in
  room

  let rooms_update_list = ref []
  
  
let room_must_update room =
  let impl = as_room_impl room in
  if not impl.impl_room_update then
    begin
      impl.impl_room_update <- true;
      rooms_update_list := room :: !rooms_update_list
    end

let room_add (room : 'a room_impl) =
  incr room_counter;
  room.impl_room_num <- !room_counter;
  let (room : room) = Obj.magic room in
  Hashtbl.add rooms_by_num !room_counter room;
  room_must_update room
    
let room_num room =
  let impl = as_room_impl  room in
  impl.impl_room_num
  
let new_room_ops network = {
    op_room_pause = (fun _ -> ni_ok network "room_pause");
    op_room_resume = (fun _ -> ni_ok network "room_resume");
    op_room_close = (fun _ -> ni_ok network "room_close");  
    op_room_messages = (fun _ -> fni network "room_messages");
    op_room_users = (fun _ -> fni network "room_users");
    op_room_name = (fun _ -> fni network "room_name");
    op_room_info = (fun _ -> fni network "room_info");
    op_room_send_message = (fun _ _ -> ni_ok network "room_message");
}

let room_find num = 
  Hashtbl.find rooms_by_num num
    
let room_state c =
  let impl = as_room_impl c in
  impl.impl_room_state
  
let set_room_state c state =
  let impl = as_room_impl c in
  if impl.impl_room_state <> state then begin
      impl.impl_room_state <- state;
      room_must_update c
    end
  
let room_pause (room : room) =
  let impl = as_room_impl room in
  if not (impl.impl_room_state = RoomPaused) then begin
      set_room_state room RoomPaused;
      impl.impl_room_ops.op_room_pause impl.impl_room_val;
    end
  
let room_resume (room : room) =
  let impl = as_room_impl room in
  if impl.impl_room_state = RoomPaused then begin
      set_room_state room RoomOpened;
      room_must_update room;
      impl.impl_room_ops.op_room_resume impl.impl_room_val;
    end
  
let room_send_message (room : room) msg =
  let impl = as_room_impl room in
  impl.impl_room_ops.op_room_send_message impl.impl_room_val msg
  
let room_messages (room : room) =
  let impl = as_room_impl room in
  impl.impl_room_ops.op_room_messages impl.impl_room_val
  
let room_users (room : room) =
  let impl = as_room_impl room in
  impl.impl_room_ops.op_room_users impl.impl_room_val
  
let room_info (room : room) =
  let impl = as_room_impl room in
  impl.impl_room_ops.op_room_info impl.impl_room_val
  
let room_name (room : room) =
  let impl = as_room_impl room in
  impl.impl_room_ops.op_room_name impl.impl_room_val
  
let room_close (room : room) =
  let impl = as_room_impl room in
  if not (impl.impl_room_state = RoomClosed) then begin
      set_room_state room RoomClosed;
      room_must_update room;
      impl.impl_room_ops.op_room_close impl.impl_room_val;
      Hashtbl.remove rooms_by_num impl.impl_room_num
    end

  
let com_rooms_by_num = rooms_by_num
let rooms_by_num = ()

  
let room_new_users = ref []
    
let room_new_user room c =
  room_new_users := (room_num room, (c : user)) :: !room_new_users  
