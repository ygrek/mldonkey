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

type identity
type protocol
type chat
type room  
type account

type online_status =
| Online_available
| Online_away
    
type status =
  Status_offline
| Status_connecting
| Status_online of online_status

type from_record = 
| FromString of (string -> unit)
| FromBool of (bool -> unit)
| FromInt of (int -> unit)
  
type to_record = 
| ToString of (unit -> string)
| ToBool of (unit -> bool)
| ToInt of (unit -> int)
  
type config_record =
  (string * string * bool * from_record * to_record) list
  
open Options

exception Optional
  
let from_record record assocs =
  List.iter (fun (option, name, optional, from_record, to_record) ->
      try
        let value = 
          try
            List.assoc option assocs 
          with _ -> 
              if not optional then 
                failwith (Printf.sprintf "Required field %s missing" option)
              else
                raise Optional
        in
        match from_record with
          FromString f -> f (value_to_string value)
        | FromBool f -> f (value_to_bool value)
        | FromInt f -> f (value_to_int value)
      with Optional -> ()
  ) record

let to_record record =
  List.map (fun  (option, name, optional, from_record, to_record) ->
      match to_record with
        ToString f -> option,string_to_value (f ())
      | ToBool f -> option, bool_to_value (f ())
      | ToInt f -> option, int_to_value (f ())
  ) record
  
(*
(* The abstract type we manipulate for contacts *)
type contact = {
    contact_name : string; (* his name *)
    contact_identities : identity list; (* all his identities *)
    contact_online : identity list; (* online identities *)
  }
  
type protocol = {
    im_name : string;
    im_login : (unit -> unit);
    im_send : (string -> string -> unit);
    im_add_buddy : (string -> string -> unit);
    im_remove_buddy : (string -> string -> unit);
    im_keepalive : (unit -> unit);
    im_set_idle : (bool -> unit);
  }
*)

(*********

  A very simple interface for the Instant Messaging facility:

*********)

(*  
module type Main_Interface = sig
      
(* An Instant-Messaging Network. *)
    type protocol

(* A chat session: a chat may be:
- a simple discussion with one guy
- a conference between several guys
- a room with discussion with multiple guys
 *)
    type chat

(* An identity: some of them may be one of my accounts *)
    type identity
      
(* A room *)
    type room
      
(* [protocols ()] returns the list of available protocols *)
(* [protocol_name p] returns the name of the protocol *)
(* [protocol_login p] connect the given network *)
(* [protocol_logout p] disconnect from the given network *)
  val protocols : unit -> protocol list      
  val protocol_name : protocol -> string      
  val account_login : identity -> unit      
  val account_logout : identity -> unit

(* The list of opened chats for a given protocol. *) 
  val protocol_chats : protocol -> chat list
  val protocol_rooms : protocol -> room list
  val protocol_accounts : protocol -> identity list

(* [new_contact name] returns a new contact of name [name] *)
(* [contact_name contact] returns the name of the contact [contact] *)
(* [contact_protocols contact] returns the list of networks on which [contact] 
   has a known identity. *)
(* [contact_available contact] returns if the contact [contact] is online *)
(* [contacts_merge contact list] adds the list of identities [list] as other
   identities for the contact [contact], that can be used to contact it. *)
  val contact_name : identity -> string
  val contact_online : identity -> bool

  val room_name : room -> string
  val room_protocol : room -> protocol
  val room_open : room -> chat

  val identity_is_me : identity -> bool

(* [protocol_contacts p] returns the list of known contacts on the given 
  network. *)
(* [protocol_add_contact contact p url] add a new identity on network [p] for 
  contact [contact] using info from [url]. *)
(* [protocol_remove_contact p contact] removes [contact] from the list of 
  contacts  on the given network [p] *)
  val new_account : protocol -> string -> identity
  val account_contacts : identity -> identity list
  val account_enter_contact : identity -> string -> identity
  val account_add_contact : identity -> identity -> unit
  val account_remove_contact : identity -> identity -> unit
      
  val chat_open : identity list -> chat
  val chat_close : chat -> unit
  val chat_identities : chat -> identity list
  val chat_send : chat -> string -> unit
      
  val identity_protocol : identity -> protocol
      
(* [set_available p online] change my online status to [online] on
   network [p] *)
  val set_available : identity -> bool -> unit
(* [send_message contact msg] sends a simple message [msg] to contact [contact]
  using one of the network he is online on. *)

  val add_event_handler : (event -> unit) -> unit
    
end
  *)

