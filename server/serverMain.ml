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
open ServerOptions
        
let _ =
  begin try
      Options.load server_ini;
    with e ->
        Printf.printf "Exception %s while loading options" (Printexc.to_string e);
        print_newline ();
        Options.save_with_help server_ini
  end;
  Options.save_with_help server_ini;
  ignore(TcpServerSocket.create !!server_port ServerClients.handler);
  let udp_sock = UdpSocket.create (!!server_port + 4) ServerUdp.udp_handler in
  begin
    match !!seed_ip with
      None -> ()
    | Some ip ->
        udp_send udp_sock (Unix.ADDR_INET (Ip.to_inet_addr ip,
          !!seed_port + 4)) (
          let module M = Mftp_server in
          M.QueryServersUdpReq (
            let module Q = M.QueryServers in
            {
              Q.ip = !!server_ip;
              Q.port = !!server_port;
            }));
    end;
  Printf.printf "server started at %d" !!server_port; print_newline ();
  BasicSocket.loop ()
