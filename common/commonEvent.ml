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

open CommonTypes

type event_kind = 
  Room_info_event of room
| Room_add_user_event of room * user
| Room_remove_user_event of room * user
| Room_message_event of int * room * room_message
  
let events_list = ref ([] : event_kind list)

let gui_option = ref (None : gui_record option)
  
let add_event event =
  events_list := event :: !events_list
      
  
  
    
let with_gui f =
  match !gui_option with
    None -> ()
  | Some gui -> f gui
      