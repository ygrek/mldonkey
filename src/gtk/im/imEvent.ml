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
  
type event =
| Account_event of account
| Account_friend_event of identity 
  
| Chat_open_event of chat
| Chat_my_message of chat * string
| Chat_message_event of chat * identity * string
| Chat_close_event of  chat

| Room_join of room
| Room_leave of room
| Room_message of room * identity * string
| Room_user_join of room * identity
| Room_user_leave of room * identity
| Room_public_message of room * string
  
  (*
  
| Chat_open_event of chat
| Chat_close_event of chat
| Chat_login_event of chat * identity
| Chat_logout_event of chat * identity
*)
  
let event_handler = ref None
let pending_events = ref []
  
let set_event_handler f =
  event_handler := Some f;
  let old_events = List.rev !pending_events in
  pending_events := [];
  List2.safe_iter f old_events

let add_event (e : event) =
  match !event_handler with
    None -> pending_events := e :: !pending_events
  | Some f -> try f e with _ -> ()
      
          