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


(*
How the events should work ?

When an object is added or modified, it is tagged with 0 and
added to the list of events [events_list]. If the event modification is
minor, the object is only tagged with -1 (and still added to the list
of events).

Every [gui_update_delay] seconds, all the events are checked.
For each event, dependencies are examined. Infos on objects are first
added to the guis, and then, the remaining events are added to the
guis, using a FIFO order ([gui.gui_old_events] and [gui.gui_new_events]
lists.

Note that, since [events_list] is in reverse order (most recent event
first), [events_list] must be reversed first, and the remaining events
added in head of [gui.gui_new_events].

The update field: positive, it indicates which GUI has received a description
of the object. Negative, it indicates that the modification is limited.
  
*)

open CommonTypes
open GuiProto
  
let events_list = ref ([] : event list)

let guis = ref ([]: gui_record list)
  
let add_event event =
  events_list := event :: !events_list
          
let with_guis f =
  List.iter f !guis
    
let create_events () =
  { num_map = Intmap.empty; num_list = []; }
    
let addevent events num update =
  try
    let prev_update = Intmap.find num events.num_map in
    if prev_update < update then
      events.num_map <- Intmap.add num update events.num_map
  with _ ->
      events.num_map <- Intmap.add num update events.num_map;
      events.num_list <- num :: events.num_list

let rec getevents context list g =
  match list with
    [] -> g context
  | (events, f) :: tail ->
      match events.num_list with
        [] -> getevents context tail g
      | num :: tail ->
          events.num_list <- tail;
          let update = Intmap.find num events.num_map in
          events.num_map <- Intmap.remove num events.num_map ;
          try f context num update; true with _ -> true
