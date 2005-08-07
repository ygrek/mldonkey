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
   This code is translated mainly from Markus Kern work on giFT-Fasttrack
*)

open Queues
open CommonDownloads
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open FasttrackTypes
open FasttrackGlobals
open FasttrackOptions
open FasttrackProtocol
open FasttrackComplexOptions

open FasttrackProto
(*

let send_query ss =
  let f s =
    server_send_query s ss in
  List.iter f !connected_servers
    *)

  (*
let udp_handler ip port buf =
  FasttrackHandler.udp_packet_handler ip port
  (FasttrackProto.parse_udp_packet  ip  port buf)
*)
