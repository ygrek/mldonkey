type 'a room_impl = {
  mutable impl_room_update : int;
  mutable impl_room_state : CommonTypes.room_state;
  mutable impl_room_val : 'a;
  mutable impl_room_num : int;
  mutable impl_room_ops : 'a room_ops;
} 
and 'a room_ops = {
  mutable op_room_close : 'a -> unit;
  mutable op_room_pause : 'a -> unit;
  mutable op_room_resume : 'a -> unit;
  mutable op_room_messages : 'a -> int -> CommonTypes.room_message list;
  mutable op_room_users : 'a -> CommonTypes.user list;
  mutable op_room_name : 'a -> string;
  mutable op_room_info : 'a -> GuiTypes.room_info;
  mutable op_room_send_message : 'a -> CommonTypes.room_message -> unit;
} 
val as_room : 'a room_impl -> CommonTypes.room
val as_room_impl : CommonTypes.room -> 'a room_impl
val dummy_room_impl : int room_impl
val dummy_room : CommonTypes.room
val room_counter : int ref
val ni : CommonTypes.network -> string -> string
val fni : CommonTypes.network -> string -> 'a
val ni_ok : CommonTypes.network -> string -> unit
val room_must_update : CommonTypes.room -> unit
val room_updated : CommonTypes.room -> unit
val room_add : 'a room_impl -> unit
val room_num : CommonTypes.room -> int
val new_room_ops : CommonTypes.network -> 'a room_ops
val room_find : int -> CommonTypes.room
val room_state : CommonTypes.room -> CommonTypes.room_state
val set_room_state : CommonTypes.room -> CommonTypes.room_state -> unit
val room_pause : CommonTypes.room -> unit
val room_resume : CommonTypes.room -> unit
val room_send_message : CommonTypes.room -> CommonTypes.room_message -> unit
val room_messages : CommonTypes.room -> int -> CommonTypes.room_message list
val room_users : CommonTypes.room -> CommonTypes.user list
val room_info : CommonTypes.room -> GuiTypes.room_info
val room_name : CommonTypes.room -> string
val room_close : CommonTypes.room -> unit
val rooms_iter : (CommonTypes.room -> unit) -> unit
val rooms_by_num : unit
val room_add_user : CommonTypes.room -> CommonTypes.user -> unit
val room_remove_user : CommonTypes.room -> CommonTypes.user -> unit
val message_counter : int ref
val room_new_message :
  CommonTypes.room ->
  CommonTypes.room_message -> int * CommonTypes.room_message
val extract_messages : ('a * 'b) list -> 'a -> 'b list
module Private :
  sig
    val network : string
    type private_room = {
      room_impl : private_room room_impl;
      mutable messages : (CommonTypes.client * string) list;
    } 
    val private_room_ops : private_room room_ops
    val private_room : private_room
  end
val private_room_ops : Private.private_room room_ops
val private_room : CommonTypes.room
val private_history : (CommonTypes.client * string) list
val private_message_from : CommonTypes.client -> string -> unit
