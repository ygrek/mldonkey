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

open ServerServer
open Gui_proto
open CommonNetwork
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

    if !!relais_cooperation_protocol then
      begin
	if !!relais_master then
	  ignore(TcpServerSocket.create "server group"
		   Unix.inet_addr_any (!!server_port+2) ServerServer.handler)
	else
	  (*join a group*)
	  ();
	add_infinite_option_timer notify_time_out notify_server;
      end;
    
    if !!save_log then ServerLog.initialized(); 

    (*Printf.printf "COOOOOOOOOL %d\n" (List.length !!known_server); *)  

    (*let t = Unix.time() in*)
      other_servers := List.map (fun (ip,port) ->
				   {server_ip = ip; server_port = port; last_message = Unix.time()}) !!known_server;
   
	

    
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

    (*add_infinite_timer 15. (fun timer ->
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
		
		   );*)

    add_infinite_timer 5. (fun timer ->
			     nb_udp_query_sec := !nb_udp_query_count;
			     nb_udp_query_count := 0;
			     nb_udp_loc_sec := !nb_udp_loc_count;
			     nb_udp_loc_count := 0;
			     nb_udp_req_sec := !nb_udp_req_count;
			     nb_udp_req_count := 0;
			     nb_tcp_req_sec := !nb_tcp_req_count;
			     nb_tcp_req_count := 0;
			     
			     
			     nb_udp_reply_sec := !nb_udp_reply_count;
			     nb_udp_reply_count := 0;
			     
				  );

    add_infinite_option_timer ping_knowed_servers (fun timer -> 
					      nb_udp_ping_server_sec := !nb_udp_ping_server_count;
					      nb_udp_ping_server_count := 0 
					   );
					      


    ServerUdp.hello_world();


    Printf.printf "server started at %d" !!server_port; print_newline ()

let _ =
  register_commands [
    "stat", Arg_none(fun o -> 
			let buf = o.conn_buf in
			  Printf.printf "SERVER STAT\n";
			  Printf.bprintf buf "nb_client:%d\nnb_files:%d\n" !nconnected_clients !nshared_md4;
			  Printf.bprintf buf "nb_know_alive_server:%d\n" (List.length !alive_servers);
			  Printf.bprintf buf "nb_know_other_server:%d\n" (List.length !other_servers);
			  Printf.bprintf buf "client_counter:%d\n" !client_counter;
			  ""
		     ),"Print server's stat";
    "packets", Arg_none(fun o -> 
			  let buf = o.conn_buf in
			    Printf.bprintf buf "nb tcp requets:%d\n" (!nb_tcp_req_sec /5);
			    Printf.bprintf buf "nb udp requets/second :%d\n" (!nb_udp_req_sec /5);
			    Printf.bprintf buf "nb udp Location/second:%d\n nb udp Query/second:%d\n " (!nb_udp_loc_sec/5) (!nb_udp_query_sec/5);
			    Printf.bprintf buf "nb udp rely send/second : %d\n" (!nb_udp_reply_sec/5);
			    Printf.bprintf buf "nb udp ping server during %fs : %d\n" !!ping_knowed_servers (!nb_udp_ping_server_count);
			    ""
		       ), "Print number of udp and tcp packet rec";
  ];

  
  network.network_config_file <- Some server_ini;
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_server);
  network.op_network_enable <- enable;
  network.network_prefixes <- [ "server" ];  
  network.op_network_info <- (fun n ->
      { 
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            None -> "" | Some opfile -> options_file_name opfile);
        network_netname = network.network_name;
        network_enabled = network.op_network_is_enabled ();
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
      })
