(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

(* An open protocol that can be used between mldonkey clients *)

open Md4
let udp_magic = 212

type udp_messages = 
  StoreRecommandation of Md4.t
(* Send a recommandation for the client with that Md4 to a server, so that
the client can ask the server for recommandation. *)
| HasRecommandation of Ip.t * int
(* The client must know that there is a new location to ask for 
recommandation. *)
| Recommandations of Md4.t * (Ip.t * int) list
(* When the client wants to download, it can send a list of recommandations. *)
| QueryRecommandation of Md4.t
(* For each recommandation, the uploader can ask for credits. *)
| Recommandation of Md4.t
(* This message is sent to recommand the client *)
