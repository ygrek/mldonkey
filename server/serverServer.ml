(* Fonction of server interation in the relais protocol *)
(* Copyright 2002 vernin, INRIA *)
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

open CommonTypes

open BasicSocket
open TcpBufferedSocket
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open Mftp_comm
open ServerTypes  
open ServerOptions        
open ServerGlobals
open ServerMessages

module M = ServerMessages

  
let null_ip = Ip.of_int32 (Int32.of_int 0)


let server_to_server s t sock = 
  Printf.printf "server_to_server"; print_newline ();
  M.print t;
  print_newline ();
  (*match t with
    M.ServerConnectReq t ->
      (*s.peer_md4 <- t.M.ServerConnect.md4;
      s.peer_tags <- t.M.ServerConnect.tags;*)      
   _ -> ()*)
        
  

