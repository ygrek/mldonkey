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
open Options
open Mftp
open BasicSocket
open TcpClientSocket  
open Mftp_comm

let client_ip = Ip.of_ints [|127; 0; 0; 1|]
let client_port = 4663
let client_name = "mldonkey"
let  client_tags =
  [
    { tag_name = "name"; tag_value =  String client_name };
    { tag_name = "version"; tag_value =  Uint32 (Int32.of_int 57) };
    { tag_name = "port"; tag_value =  Uint32 (Int32.of_int client_port) };
  ]

  
let client_handler sock event = 
  match event with
    BASIC_EVENT (CLOSED s) -> exit 0
  | _ -> ()

let client_to_client t sock =
  let module M = Mftp_client in
  match t with
    M.ConnectReplyReq t ->  
      client_send sock 
        (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq (Md4.random ()));
      client_send sock 
        (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq (Md4.of_string "EC8F3298E60C44F6851E8AEEC7D1F506"));
      client_send sock 
        (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq (Md4.random ()));
      client_send sock 
        (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq (Md4.random ()));
      client_send sock 
        (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq (Md4.random ()));
      client_send sock 
        (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq (Md4.random ()));
  | _ -> 
      Printf.printf "Other message"; print_newline ()
      
let _ =
  let sock = connect (Ip.to_inet_addr (client_ip)) 4662 client_handler
  in
  set_reader sock (Mftp_comm.client_handler client_to_client);
  client_send sock (
    let module M = Mftp_client in
    let module C = M.Connect in
    M.ConnectReq {
      C.md4 = Md4.random (); (* we want a different id each conn *)
      C.ip = client_ip;
      C.port = client_port;
      C.tags = client_tags;
      C.version = 16;
      C.ip_server = client_ip;
      C.port_server = 4700;
    });

  loop ()
