(* Copyright 2001, 2002 Simon, INRIA *)
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

val sources_entry : MultinetTypes.source Fifo.t
  
val new_source : CommonTypes.location_kind -> MultinetTypes.file_network
    -> MultinetTypes.file -> MultinetTypes.source
  
val second_timer : unit -> unit
val minute_timer : unit -> unit
val print : Buffer.t -> unit
val add_source_request : 
    MultinetTypes.source ->
  MultinetTypes.file -> int -> MultinetTypes.request_result -> unit
  
(* Must be called when:
- a new download is added
- file priorities have changed
*)
val resort_files : unit -> unit