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
open ServerLog
open BasicSocket
open ServerClients
open ServerGlobals

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
    
    if !!save_log then ServerLog.initialized(); 

    (*Printf.printf "COOOOOOOOOL %d\n" (List.length !!known_server); *)  

   
      other_servers := List.map (fun (ip,port) ->
				   {server_ip = ip; server_port = port}) !!known_server;
   
	

    
    (*Printf.printf "BOOOOOOOL %d\n" (List.length !other_servers);  *)

    (*ServerUdp.print !other_servers;*)

    (*List.iter (fun s -> server_print s stdout) !!known_servers;*)

    (*let tmp = List.hd !!known_servers in
    let (ip,port) = get_address tmp in
      Printf.printf "yes//////\n";
      Printf.printf "%s:%d" (Ip.to_string ip) port*)

   
	
   
   (* let serv  = (List.hd !!known_servers) in
      Printf.printf "%s" serv;*)
    
    (*other_servers := List.map (fun serv -> serv.server_addr) !!known_servers;*)
    
    add_infinite_option_timer send_server_stat_delay ServerClients.send_stat_to_clients;
    
    add_infinite_option_timer save_option_delay (fun timer -> 
				     ServerUdp.save_servers_liste();
				     Options.save_with_help server_ini;
    );

    add_infinite_option_timer ping_knowed_servers ServerUdp.ping_servers;

    add_infinite_timer 15. (fun timer ->
		      Printf.printf "SERVER STAT\n";
		      Printf.printf "nb_client:%d\nnb_files:%d\n" !nconnected_clients !nshared_files;
		      Printf.printf "nb_know_alive_server:%d\n" (List.length !alive_servers);
		      Printf.printf "nb_know_other_server:%d\n" (List.length !other_servers);
		       Printf.printf "client_counter:%d\n" !client_counter;
		      Printf.printf "nb udp requets:%d\n" !nb_udp_req;
	              Printf.printf "nb udp Location:%d\n nb udp Query:%d\n " !nb_udp_loc !nb_udp_query;
	              Printf.printf "nb tcp requets:%d\n" !nb_tcp_req;
		      (*ServerUdp.print !alive_servers;*)
		      Pervasives.flush Pervasives.stdout
		
		   );

    ServerUdp.hello_world();

    Printf.printf "server started at %d" !!server_port; print_newline ()

let _ =
  network.network_config_file <- Some server_ini;
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_server);
  network.op_network_enable <- enable;
  network.network_prefixes <- [ "server" ]  

  
