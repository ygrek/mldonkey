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

open Md4

val query_id :
  DonkeyTypes.server -> TcpBufferedSocket.t -> Ip.t -> 
  DonkeyTypes.file option -> unit
  
val query_locations_reply : DonkeyTypes.server ->
  DonkeyProtoServer.QueryLocationReply.t -> unit

val reconnect_client : DonkeyTypes.client -> unit
val client_connection_handler : bool -> 'a -> TcpServerSocket.event -> unit

val query_files : DonkeyTypes.client -> TcpBufferedSocket.t -> unit

val client_wants_file : DonkeyTypes.client -> Md4.t -> unit
  
val clean_groups : unit -> unit
  
val client_send_if_possible : DonkeyTypes.client -> TcpBufferedSocket.t -> 
  DonkeyProtoClient.t -> unit

  
val clean_requests : unit -> unit

val disconnect_client : DonkeyTypes.client -> unit
  
