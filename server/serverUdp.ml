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

open Unix
open TcpClientSocket
open Mftp
open Options
open Mftp_comm
open ServerTypes
open ServerGlobals  
open ServerOptions
  
(* reponse (propagation de la liste des serveurs connus):
227: header
161: opcode
(127)(0)(0)(1)(53)(18): mon adresse
2: nbre de serveurs connus  
servers_connus (IP + port)
  *)
  
let rec add_new_servers servers other_servers to_add =
  let module Q = Mftp_server.QueryServersReply in
  match servers with 
    [] -> other_servers @ to_add
  | s :: tail ->
      let s = { 
          server_ip = s.Q.ip;
          server_port = s.Q.port;
        } in
      add_new_servers tail other_servers 
        (if List.mem s other_servers then to_add else (s :: to_add))
        

let rec find_servers servers n left =
  if n = 0 then left else
  match servers with
    [] -> left
  | s :: tail ->
      find_servers tail (n-1) (
        let module Q = Mftp_server.QueryServersReply in
        { Q.ip = s.server_ip; Q.port = s.server_port; } :: left)
          
    
      
let udp_handler sock event =
  let module M = Mftp_server in
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p -> 
          let pbuf = p.UdpSocket.content in
          let len = String.length pbuf in
          if len = 0 || 
            int_of_char pbuf.[0] <> 227 then begin
              Printf.printf "Received unknown UDP packet"; print_newline ();
              dump pbuf;
              print_newline ();
            end else begin
              let t = M.parse (String.sub pbuf 1 (len-1)) in
              M.print t;
              print_newline ();
              match t with
                M.QueryServersReplyUdpReq t -> 
                  let module Q = M.QueryServersReply in
                  other_servers := add_new_servers t.Q.servers 
                    !other_servers []
                  
              | M.QueryServersUdpReq t -> 
                  let module Q = M.QueryServers in
                  let to_addr = p.UdpSocket.addr in
                  let servers = find_servers !other_servers 200 [] in
                  udp_send sock to_addr (
                    let module M = Mftp_server in
                    M.QueryServersReplyUdpReq (
                      let module Q = M.QueryServersReply in
                      {
                        Q.server_ip = !!server_ip;
                        Q.server_port = !!server_port;
                        Q.servers = servers;
                      }));
                
                  (*
              | M.Message150UdpReq (t1, t2,t3) ->            
                  let to_addr = p.UdpSocket.addr in
                  udp_send sock to_addr (M.Message151UdpReq
                    (t1, t2, t3, Int32.of_int 1, Int32.of_int 0, 0))
*)                    
                  
              | _ -> 
                  Printf.printf "UNKNOWN"; print_newline ();
            end
      ) sock.UdpSocket.rlist;
      sock.UdpSocket.rlist <- []
  | _ -> ()
