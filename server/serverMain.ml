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

open CommonOptions
open CommonTypes
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open Mftp_comm
open ServerTypes
open ServerOptions

  
let load_simple_args () =
    begin try
      Options.load server_ini;
    with e ->
        Printf.printf "Exception %s while loading options"
          (Printexc.to_string e);
        print_newline ();
        Options.save_with_help server_ini
  end

let enable () =
  
  if not !!enable_server then enable_server =:= true;
  
  ignore(TcpServerSocket.create "server server"
    Unix.inet_addr_any !!server_port ServerClients.handler);
  let udp_sock = UdpSocket.create Unix.inet_addr_any (!!server_port + 4) 
    ServerUdp.udp_handler in
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
  Printf.printf "server started at %d" !!server_port; print_newline ()

let _ =
  network.op_network_config_file <- (fun _ -> server_ini);
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_server);
  network.op_network_load_simple_options <- load_simple_args;
  network.op_network_save_simple_options <- save_config;
  network.op_network_enable <- enable;
  network.op_network_prefixed_args <- (fun _ ->
      prefixed_args "server" server_ini  
  );

  
