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

open DonkeyTypes

val add_source_request : DonkeyTypes.source ->
    DonkeyTypes.file -> int -> DonkeyTypes.request_result -> unit
val set_request_result : DonkeyTypes.client ->
    DonkeyTypes.file -> DonkeyTypes.request_result -> unit  
  
val query_file :
  TcpBufferedSocket.t -> DonkeyTypes.client -> DonkeyTypes.file -> unit
val add_file_location : DonkeyTypes.file -> DonkeyTypes.client -> unit
val remove_file_location : DonkeyTypes.file -> DonkeyTypes.client -> unit

  
(* Specific to the source management used *)
module S : sig
    val old_source : int -> Ip.t * int -> DonkeyTypes.file -> source
    val iter : (DonkeyTypes.source -> unit) -> unit
    val source_of_client : DonkeyTypes.client -> unit    
    val reschedule_sources : DonkeyTypes.file -> unit
    val new_source : Ip.t * int -> DonkeyTypes.file -> source
    val need_new_sources : unit -> bool
    val check_sources :     (DonkeyTypes.client -> unit) -> unit
    val print_sources : Buffer.t -> unit
  end
  