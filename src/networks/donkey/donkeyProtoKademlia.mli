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
module P :
  sig
    val write : Buffer.t -> DonkeyOvernet.t -> unit
    val udp_buf : Buffer.t
    val kademlia_header_code : char
    val kademlia_packed_header_code : char
    val kademlia_header : string
    val kademlia_packed_header : string
    val udp_send : UdpSocket.t -> Ip.t -> int -> DonkeyOvernet.t -> unit
    val parse : 'a -> 'b
    val udp_handler :
      ('a -> UdpSocket.udp_packet -> unit) ->
      UdpSocket.t -> UdpSocket.event -> unit
    val redirector_section : string
    val options_section_name : string
    val enable_overnet : bool Options.option_record
    val overnet_section : Options.options_section
    val overnet_port : int Options.option_record
    val overnet_tcpport : int Options.option_record
    val command_prefix : string
  end
  *)
module Kademlia :
  sig
    val overnet_search : CommonTypes.search -> unit
    val recover_file : DonkeyTypes.file -> unit
    val enable : bool ref -> unit
    val disable : unit -> unit
    val gui_overnet_options_panel : (string * string * string) list
    val bootstrap : Ip.t -> int -> unit
    val cancel_recover_file : DonkeyTypes.file -> unit
  end
