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

val old_source : int -> int -> Ip.t * int -> DonkeyTypes.file -> 
  DonkeyTypes.source
val iter : (DonkeyTypes.source -> unit) -> unit
val source_of_client : DonkeyTypes.client -> unit    
val reschedule_sources : DonkeyTypes.file -> unit
val new_source : Ip.t * int -> DonkeyTypes.file -> DonkeyTypes.source
val need_new_sources : DonkeyTypes.file -> bool
val check_sources :     (DonkeyTypes.client -> unit) -> unit
val print_sources : Buffer.t -> unit
val recompute_ready_sources : unit -> unit
val client_connected : DonkeyTypes.client -> unit
val add_source_request : DonkeyTypes.source ->
  DonkeyTypes.file -> int -> DonkeyTypes.request_result -> unit
val init : unit -> unit  

val query_file : DonkeyTypes.client -> DonkeyTypes.file -> unit
val add_file_location : DonkeyTypes.file -> DonkeyTypes.client -> unit
val set_request_result :
  DonkeyTypes.client ->
  DonkeyTypes.file -> DonkeyTypes.request_result -> unit
val indirect_connections : int ref
val create_source : int -> int -> Ip.t * int -> DonkeyTypes.source
