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
open GuiTypes
open CommonNetwork
open CommonOptions
open CommonTypes
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open DonkeyProtoCom
open ServerTypes
open ServerOptions
open ServerLog
open BasicSocket
open ServerClients
open ServerGlobals


let enable () =
  
  if not !!enable_server then enable_server =:= true;
  
  ignore(TcpServerSocket.create "server client bind socket"
    Unix.inet_addr_any !!server_port ServerClients.handler);

  let udp_server_sock = 
    UdpSocket.create Unix.inet_addr_any (!!server_port + 4) 
    ServerUdp.udp_handler in
  udp_sock := Some udp_server_sock;
  begin
    match !!seed_ip with
      None -> ()
    | Some ip ->
        udp_send udp_server_sock (Unix.ADDR_INET (Ip.to_inet_addr ip,
          !!seed_port + 4)) (
          let module M = DonkeyProtoServer in
          M.QueryServersUdpReq (
            let module Q = M.QueryServers in
            {
              Q.ip = !!server_ip;
              Q.port = !!server_port;
            }));
    end;
    

    if !!relais_cooperation_protocol then
      begin
	ignore(TcpServerSocket.create "server group bind socket"
		 Unix.inet_addr_any ((!!server_port)+5) ServerServer.handler);
	if !!relais_master then
          begin
            Printf.printf "Your server create a new group bind on %d \n" (!!server_port+5);
            group_id := !!server_md4;
            incr server_counter
          end 
	else
	  begin
	    let (ip,port)= List.hd !!known_master in
	      Printf.printf "I connect to master %s:%d\n" (Ip.to_string ip) port;  
              ServerServer.connect_a_group();
          end;
	add_infinite_option_timer notify_time_out action_notify_servers;
	add_infinite_option_timer connect_time_out action_connect_servers;
      end;
   

    if !!save_log then ServerLog.initialized(); 

    add_infinite_option_timer send_server_stat_delay ServerClients.send_stat_to_clients;
   

    add_infinite_timer 60. (fun timer ->
			     nb_udp_query_sec := float !nb_udp_query_count;
			     nb_udp_query_count := 0;
			     nb_udp_loc_sec := float !nb_udp_loc_count;
			     nb_udp_loc_count := 0;
			     nb_udp_req_sec := float !nb_udp_req_count;
			     nb_udp_req_count := 0;
			     nb_tcp_req_sec := float !nb_tcp_req_count;
			     nb_tcp_req_count := 0;
			     
			     
			     nb_udp_reply_sec := float !nb_udp_reply_count;
			     nb_udp_reply_count := 0;
			     
				  );

    add_infinite_option_timer ping_known_servers (fun timer -> 
						    nb_udp_ping_server_sec := float !nb_udp_ping_server_count;
						    nb_udp_ping_server_count := 0 
						 );
					      
    (*ServerUdp.test();*)

    add_infinite_timer 60. ServerClients.check_notifications;
  

    add_infinite_option_timer ping_known_servers ServerUdp.ping_servers;
     
    ServerUdp.hello_world();


    Printf.printf "server started at %d" !!server_port; print_newline ()

let _ =
  register_commands [
    "stat", Arg_none(fun o -> 
        let n_alive = ref 0 in
        let n_dead = ref 0 in
        Hashtbl.iter (fun _ s ->
            if s.DonkeyTypes.server_last_message > last_time () -. 1800. then
              incr n_alive else incr n_dead
        ) DonkeyGlobals.servers_by_key;
        let buf = o.conn_buf in
        Printf.printf "SERVER STAT\n";
        Printf.bprintf buf "nb_client:%d\nnb_files:%d\n" !nconnected_clients !nshared_md4;
        Printf.bprintf buf "nb_know_alive_server:%d\n" !n_alive;
        Printf.bprintf buf "nb_know_other_server:%d\n" !n_dead;
        Printf.bprintf buf "client_counter:%d\n" !client_counter;
        ""
    ),"Print server's stat";
    "packets", Arg_none(fun o -> 
			  let buf = o.conn_buf in
                            Printf.printf "Messages received during the last min\n";
			    Printf.bprintf buf "nb tcp requets:%f\n" (!nb_tcp_req_sec /. 60.);
			    Printf.bprintf buf "nb udp requets/second :%f\n" (!nb_udp_req_sec /. 60.);
			    Printf.bprintf buf "nb udp Location/second:%f\n nb udp Query/second:%f\n" (!nb_udp_loc_sec /. 60.) (!nb_udp_query_sec /. 60.);
			    Printf.bprintf buf "nb udp rely send/second : %f\n" (!nb_udp_reply_sec /. 60.);
			    ""
		       ), "Print number of udp and tcp packet rec";
    "clients", Arg_one (fun arg o -> 
			  let buf = o.conn_buf in
			    ServerClients.bprint buf clients_by_id arg;
			    ""
		       ),"Print clients general info";

    "sources", Arg_none (fun o ->
			   let buf =  o.conn_buf in
			   let lst = ServerLocate.get_liste_of_md4() in
			     List.iter (fun (md4,nb_source) -> 
					  try
					    let tags = ServerIndexer.get_def md4 in
					      DonkeyMftp.bprint_tags buf tags.DonkeyMftp.f_tags;
					      Printf.bprintf buf "-->Actualy %d sources\n" nb_source 
					  with _ ->
					    Printf.bprintf buf "NO file def for %s\n" (Md4.to_string md4);
					    Printf.bprintf buf "-->Actualy %d sources\n" nb_source 
				       ) lst;
			     ""
			),"Print files shared on the server";
			
    "server_group_info", Arg_none (fun o ->
        let buf = o.conn_buf in
        bprint_server_info buf "";
        ""
    ),"Print info about servers in the group";
    "jg", Arg_two (fun ip port o -> 
                               let buf = o.conn_buf in
                                  Printf.bprintf buf "try to connect %s %s\n" ip port;
                               ServerServer.join_a_group (Ip.of_string ip) (int_of_string port);
                               ""
                  ),"Try to join a server master";
    "qg",Arg_none (fun o ->
                           let buf = o.conn_buf in
                                  Printf.bprintf buf "diconnect of the group %s\n" (Md4.to_string !group_id);
                          ServerServer.disconnect();
                          ""
                   ),"Diconnect of the group";

  
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
  
