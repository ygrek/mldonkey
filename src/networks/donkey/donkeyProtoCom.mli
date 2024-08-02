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

open DonkeyTypes
(*
type server_msg = DonkeyProtoServer.t
type client_msg =  DonkeyProtoClient.t

type server_sock = TcpBufferedSocket.t
type client_sock = TcpBufferedSocket.t
    *)

val server_send : TcpBufferedSocket.t -> DonkeyProtoServer.t -> unit
val client_send : client -> DonkeyProtoClient.t -> unit
(*val emule_send : TcpBufferedSocket.t -> DonkeyProtoClient.t -> unit *)
val servers_send : TcpBufferedSocket.t list -> DonkeyProtoServer.t -> unit


  (*
val client_handler :
  (DonkeyProtoClient.t -> TcpBufferedSocket.t -> unit) ->
TcpBufferedSocket.t -> int -> unit
*)

val cut_messages : (int -> string -> 'a) ->
    ('a -> TcpBufferedSocket.t -> 'b) -> TcpBufferedSocket.t -> int -> unit

val client_handler2 :
  client option ref ->
    (DonkeyProtoClient.t -> TcpBufferedSocket.t -> client option) ->
  (client -> DonkeyProtoClient.t -> TcpBufferedSocket.t -> unit) ->
  TcpBufferedSocket.t -> int -> unit

  (*
val server_handler :
  (DonkeyProtoServer.t -> TcpBufferedSocket.t -> unit) ->
  TcpBufferedSocket.t -> int -> unit
*)

val udp_send:  UdpSocket.t -> Ip.t -> int -> DonkeyProtoUdp.t -> unit
val udp_handler :
  (DonkeyProtoUdp.t -> UdpSocket.udp_packet -> unit) ->
  UdpSocket.t -> UdpSocket.event -> unit

(* val propagate_working_servers : (Ip.t * int) list -> (Ip.t * int) list -> unit *)
val udp_basic_handler :
  (string -> UdpSocket.udp_packet -> unit) -> UdpSocket.t ->
  UdpSocket.event -> unit

val server_msg_to_string : DonkeyProtoServer.t -> string
val client_msg_to_string : emule_proto -> DonkeyProtoClient.t -> string

val direct_client_sock_send : emule_proto -> TcpBufferedSocket.t -> DonkeyProtoClient.t -> unit

val server_send_share :
  bool -> TcpBufferedSocket.t  -> file list -> unit
val client_send_files :
    TcpBufferedSocket.t -> file list -> unit
val client_send_dir :
    TcpBufferedSocket.t -> string -> file list -> unit

val new_string :  DonkeyProtoClient.t -> bytes -> unit

val tag_file : file -> CommonTypes.tag list

val udp_server_send : server -> DonkeyProtoUdp.t -> unit
val udp_server_send_ping : server -> DonkeyProtoUdp.t -> unit
