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

type event = {
    mutable next_event : event;
    mutable prev_event : event;
    mutable event_kind : event_kind;
  }
  
and event_kind = 
  FirstEvent
  
let new_event_loop () = 
  let rec e =
    {
      next_event = e;
      prev_event = e;
      event_kind = FirstEvent;
    }
  in e
  
let insert_event e kind = 
  let new_event = {
      next_event = e.next_event;
      prev_event = e;
      event_kind = kind;
    } in
  e.next_event <- new_event;
  e.next_event.prev_event <- new_event;
  new_event
  
let remove_event e =
  e.next_event.prev_event <- e.prev_event;
  e.prev_event.next_event <- e.next_event

let history = new_event_loop ()
  
  