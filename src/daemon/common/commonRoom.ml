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

open Printf2
open CommonTypes
open CommonEvent
  
type 'a room_impl = {
    mutable impl_room_update : int;
    mutable impl_room_state : room_state;
    mutable impl_room_val : 'a;
    mutable impl_room_num : int;
    mutable impl_room_ops : 'a room_ops;
  }
  
and 'a room_ops = {
    mutable op_room_close : ('a -> unit);
    mutable op_room_pause : ('a -> unit);
    mutable op_room_resume : ('a -> unit);
    mutable op_room_messages : ('a -> int -> room_message list);
    mutable op_room_users : ('a -> user list);
    mutable op_room_name : ('a -> string);
    mutable op_room_info : ('a -> GuiTypes.room_info);
    mutable op_room_send_message : ('a -> room_message -> unit);
  }


let as_room  (room : 'a room_impl) =
  let (room : room) = Obj.magic room in
  room
  
let as_room_impl  (room : room) =
  let (room : 'a room_impl) = Obj.magic room in
  room

let dummy_room_impl = {
    impl_room_update = 1;
    impl_room_state = RoomClosed;
    impl_room_val = 0;
    impl_room_num = 0;
    impl_room_ops = Obj.magic None;
  }
  
let dummy_room  = as_room dummy_room_impl
module H = Weak.Make(struct
      type t = room
      let hash r = Hashtbl.hash (as_room_impl r).impl_room_num
      
      let equal x y = 
        (as_room_impl x).impl_room_num = (as_room_impl y).impl_room_num
    end)

let room_counter = ref (-1)
let rooms_by_num = H.create 1027
    
let _ = 
  Heap.add_memstat "CommonRoom" (fun level buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) rooms_by_num;
      Printf.bprintf buf "  rooms: %d\n" !counter;
  )

let ni n m = 
  let s = Printf.sprintf "Room.%s not implemented by %s" 
      m n.network_name in
  lprintf "%s\n" s; 
  s
  
let fni n m =   failwith (ni n m)
let ni_ok n m = ignore (ni n m)

  
let room_must_update room =
  let impl = as_room_impl room in
  if impl.impl_room_update <> 0 then begin
      events_list := Room_info_event room :: !events_list
    end;
  impl.impl_room_update <- 0

let room_updated room = 
  let impl = as_room_impl room in
  impl.impl_room_update <- 1
  
let room_add (room : 'a room_impl) =
  incr room_counter;
  room.impl_room_num <- !room_counter;
  let (room : room) = Obj.magic room in
  H.add rooms_by_num room;
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

  H.find rooms_by_num (as_room { dummy_room_impl with
    impl_room_num = num })

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
  try impl.impl_room_ops.op_room_users impl.impl_room_val with _ -> []
  
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
    end


let rooms_iter (f : CommonTypes.room -> unit) =
  H.iter f rooms_by_num
  
let com_rooms_by_num = rooms_by_num
let rooms_by_num = ()

  
let room_add_user room c =
  add_event (Room_add_user_event (room, c))

let room_remove_user room c =
  add_event (Room_remove_user_event (room, c))

let message_counter = ref 0
let room_new_message room msg =
  incr message_counter;
  events_list := Room_message_event (!message_counter, 
    (room : room), (msg : room_message)) :: !events_list;
  (!message_counter, msg)

let extract_messages list age =
  let rec iter list passed =
    match list with
      [] -> passed
    | (old, m) :: tail ->
        if old >= age then
          iter tail (m :: passed)
        else passed
  in
  iter list []
  
module Private = struct  

(* Private messages are implemented with the following trick:
A special room "Private" is created. This room is always opened in
the core, but is sent as Closed to the GUI so that it should not display it
  in the room panel.  It also has number 0.  Sending a message 
Public/Private Message to this room is transformed into sending a
receiving a private message from this client in driver/driverInterface.ml. 
  *)
    
    
    open CommonNetwork  
    let network = "Private"
    
    type private_room = {
        room_impl : private_room room_impl;
        mutable messages : (client * string) list; 
      }    
    
    let private_room_ops = 
      {
        op_room_pause = (fun _ -> ni_ok network "room_pause");
        op_room_resume = (fun _ -> ni_ok network "room_resume");
        op_room_close = (fun room -> 
            set_room_state (as_room room.room_impl) RoomOpened);  
        op_room_messages = (fun _ -> fni network "room_messages");
        op_room_users = (fun _ -> failwith "not implemented");
        op_room_name = (fun _ -> fni network "room_name");
        op_room_info = (let module P = GuiTypes in
          fun r -> 
            { 
              P.room_name = "Default Room";
              P.room_num = r.room_impl.impl_room_num;
              P.room_network = 0;
              P.room_state = RoomClosed;
              P.room_users = [];
              P.room_messages = [];
              P.room_nusers = 0;
            }
        );
        op_room_send_message = (fun _ _ -> ni_ok network "room_message");
      }
    
    let private_room = 
      let rec room = {
          messages = [];
          room_impl = room_impl;
        } and
        room_impl = {
          dummy_room_impl with
          impl_room_val = room;
          impl_room_ops = private_room_ops;
          impl_room_state = RoomOpened;
        } in
      room_add room_impl;
      room
      
  end
  
let private_room_ops = Private.private_room_ops
let private_room = as_room Private.private_room.Private.room_impl
let private_history = Private.private_room.Private.messages

let private_message_from c s =
  room_send_message private_room
    (PrivateMessage (CommonClient.client_num c, s))

  
