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

open CommonGlobals
open Unix
open TcpBufferedSocket
open DonkeyMftp
open BasicSocket
open UdpSocket
open Options
open Mftp_comm
open ServerTypes
open ServerGlobals  
open ServerOptions
open ServerLog
open DonkeyComplexOptions
  
(* reponse (propagation de la liste des serveurs connus):
227: header
161: opcode
(127)(0)(0)(1)(53)(18): mon adresse
2: nbre de serveurs connus  
servers_connus (IP + port)
  *)

let new_alive_servers = ref []
let time_out = ref true
  
let rec add_new_servers servers other_servers to_add =
  let module Q = Mftp_server.QueryServersReply in
  match servers with 
    [] -> other_servers @ to_add
  | s :: tail ->
      let s = { 
          server_ip = s.Q.ip;
          server_port = s.Q.port;
	  last_message = Unix.time();
        } in
      add_new_servers tail other_servers 
        (if List.mem s other_servers then to_add else (s :: to_add))
        

let rec find_servers servers n left =
  if n = 0 then left else
  match servers with
    [] -> left
  | s :: tail -> (*Printf.printf "Add to liste\n";*)
      find_servers tail (n-1) (
        let module Q = Mftp_server.QueryServersReply in
        { Q.ip = s.server_ip; Q.port = s.server_port; } :: left)
          
    
let rec print liste = 
        match liste with
        [] -> ()
        | hd :: tail -> Printf.printf "%s:%d\n" (Ip.to_string hd.server_ip)
        hd.server_port;
                        print tail;
                        ()
        
        
let udp_handler sock event =
  let module M = Mftp_server in
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p -> 
          try
          let pbuf = p.UdpSocket.content in
          let len = String.length pbuf in
          if len = 0 || 
            int_of_char pbuf.[0] <> 227 then begin
              Printf.printf "Received unknown UDP packet\n";
              BigEndian.dump pbuf;
              print_newline ();
            end else begin
              let t = M.parse (String.sub pbuf 1 (len-1)) in
		(*M.print t;
		print_newline ();*)
		incr nb_udp_req_count;
		if (!!save_log) then
                  begin 
                    match p.UdpSocket.addr with
                      | ADDR_INET (ip,port) ->
			  ServerLog.new_log_req (Ip.of_inet_addr ip) Md4.null t;
			  ServerLog.add_to_liste ()
                      |_-> ()
		  end;
              match t with
                M.QueryServersReplyUdpReq t -> 
		  (*Printf.printf "-_-_-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-";*)
                  let module Q = M.QueryServersReply in
		  if (!time_out) then
		    begin 
		      (*Printf.printf "-_-_-_-_-YES TIME";*)
                      other_servers :=  add_new_servers t.Q.servers
			!other_servers []
		    end
		  else
		    begin
		      (*Printf.printf "-_-_-_-_-NO TIME";*)
                      let bob =  
			{
			  server_ip = t.Q.server_ip; 
			  server_port = t.Q.server_port;
			  last_message = Unix.time();
			} in
		      if not (List.mem bob !new_alive_servers) then 
			new_alive_servers:= bob :: !new_alive_servers;
			(*new_alive_servers:= add_new_servers t.Q.servers
		       !new_alive_servers [];*)
		      other_servers :=  add_new_servers t.Q.servers
		       !other_servers []
		    end;
                  
              | M.QueryServersUdpReq t -> 
                  (*Printf.printf "SEND SERVER LISTE\n";*)
                  (*print !other_servers;*) 
		  incr nb_udp_reply_count;
                  let module Q = M.QueryServers in 
                  let to_addr = Unix.ADDR_INET((Ip.to_inet_addr t.Q.ip),t.Q.port+4) in
                  let servers = find_servers !alive_servers 50 [] in
                  udp_send sock to_addr (
                    let module M = Mftp_server in
                    M.QueryServersReplyUdpReq (
                      let module Q = M.QueryServersReply in
                      {
                        Q.server_ip = !!server_ip;
                        Q.server_port = !!server_port;
                        Q.servers = servers;
                      }));
		    let bob =  
		      {
			server_ip = t.Q.ip; 
			server_port = t.Q.port;
			last_message = Unix.time();
		      } in
		      if not (List.mem bob !alive_servers) then
			begin
			  alive_servers:= bob :: !alive_servers; 
			  if not (List.mem bob !other_servers) then 
			    other_servers:= bob :: !other_servers
			end
                
                  
              (*| M.PingServerUdpReq (t1, t2,t3) ->            
                  let to_addr = p.UdpSocket.addr in
                  udp_send sock to_addr (M.PingServerReplyUdpReq
                    (t1, t2, t3, 0,Int32.of_int !nconnected_clients, Int32.of_int
                    !nshared_files)) *)                   
                  
              | M.PingServerUdpReq t ->            
                  let to_addr = p.UdpSocket.addr in
                  udp_send sock to_addr (M.PingServerReplyUdpReq
                    (t,Int32.of_int !nconnected_clients, Int32.of_int
                    !nshared_md4))  
		    
	      | M.QueryUdpReq t ->
                 incr nb_udp_query_count;       
                 (*Printf.printf "QUERYUDPREQ";*)
                 let module R = M.Query in
                 let q = ServerIndexer.query_to_query t in
                 let docs = ServerIndexer.find q in
                 (* send back QueryReplyReq *)
                 let list = ServerIndexer.get docs 1 in
		   if (List.length list) <> 0 then
		     begin
		       if (!!save_log) then
			 ServerLog.put_results list;
		       let to_addr = p.UdpSocket.addr in 
			 udp_send sock to_addr (M.QueryReplyUdpReq (List.hd list));
		     end;
		    ()   
                  
              | M.QueryLocationUdpReq t ->
                begin 
                try   
		  incr nb_udp_loc_count;
                    (*Printf.printf "QUERYLOCATIONUDPREQ";*)
                  let module R = M.QueryLocationReply in
                  let peer_list = ServerLocate.get t in
                  if (!!save_log) then
                     ServerLog.add_note  ("Number of Location:\n"^(string_of_int (List.length peer_list.R.locs))^"\n");
		  if ((List.length peer_list.R.locs) <> 0) then
                    let to_addr = p.UdpSocket.addr in
                      udp_send sock to_addr (M.QueryLocationReplyUdpReq peer_list);
                with Not_found -> ()
                end  
		  
		| M.QueryCallUdpReq t ->
		    (*begin
		      try
			let cc = Hashtbl.find clients_by_id t in
			  match cc.client_kind, sock, c.client_kind, cc.client_sock with
			    | KnownLocation (ip, port), sock, _, _
				->  let to_addr = p.UdpSocket.addr in
				  udp_send sock to_addr (M.QueryLocationReplyUdpReq {);
			    | Firewalled_client, _, KnownLocation (ip, port), Some sock 
				->
				let module QI = M.QueryIDReply in
				  direct_server_send sock (M.QueryIDReplyReq {
							     QI.ip = ip;
							     QI.port = port;
							   })
			    | _ ->
				(*Printf.printf "QueryIDReq can't return reply";*)
				raise Not_found
			  with Not_found -> (); 
		    end;*)()
                  
              | _ ->
		  begin
		    match p.UdpSocket.addr with
		      | ADDR_INET (ip,port) -> 
			  Printf.printf "UNKNOWN UDP from %s:%d\n" (string_of_inet_addr ip) port
		      | _ ->  ()
		  end;
		  Mftp_server.print t;
		  print_newline ();
            end
         with e ->
              Printf.printf "******* EXCEPTION %s in List.iter on UDP packets"
                (Printexc.to_string e); print_newline ();
              Printf.printf "This could prevent discarding UDP messages ...";
              print_newline ();
      ) sock.UdpSocket.rlist;
      sock.UdpSocket.rlist <- []
  | _ -> ()


let wait_ping timer = 
  (*Printf.printf "FIN PING\n";
  print !new_alive_servers;
  Printf.printf "///////////\n";*)
  alive_servers := [];
  alive_servers := !new_alive_servers;
  time_out := true;
()
  

let ping_servers timer =
  new_alive_servers := [];
  time_out := false;  
  let sock = UdpSocket.create_sendonly () in 
  let module M = Mftp_server in 
  let module Q = M.QueryServers in
  let msg = {
    Q.ip = !!server_ip;
    Q.port = !!server_port;
  } in
    if (List.length !alive_servers) > 60 then
	for i = 0 to 60 do
	  let s = List.nth !alive_servers i in
	  let to_addr = Unix.ADDR_INET((Ip.to_inet_addr s.server_ip),s.server_port+4) in 
	    udp_send sock to_addr (M.QueryServersUdpReq  msg)
	done
    else
      begin 
	List.iter (fun serv ->
		     (*Printf.printf "PING %s %d \n" (Ip.to_string serv.server_ip) serv.server_port;*)
		     let to_addr = Unix.ADDR_INET((Ip.to_inet_addr serv.server_ip),serv.server_port+4) in 
		       udp_send sock to_addr (M.QueryServersUdpReq  msg)
		  ) !alive_servers;
	if  (List.length !other_servers) > 100 then
	  for i = 0 to 100 do
	    let s = List.hd !other_servers in
	    let to_addr = Unix.ADDR_INET((Ip.to_inet_addr s.server_ip),s.server_port+4) in 
	      udp_send sock to_addr (M.QueryServersUdpReq  msg);
	      other_servers := (List.tl !other_servers) @ [s] 
	      
	  done
	else
	  List.iter (fun serv ->
		     (*Printf.printf "PING %s %d \n" (Ip.to_string serv.server_ip) serv.server_port;*)
		     let to_addr = Unix.ADDR_INET((Ip.to_inet_addr serv.server_ip),serv.server_port+4) in 
		       udp_send sock to_addr (M.QueryServersUdpReq  msg)
		    ) !other_servers;
	  
      end;
   add_infinite_timer 3. wait_ping      

let hello_world () =
  let sock = UdpSocket.create_sendonly () in 
  let module M = Mftp_server in 
  let module Q = M.QueryServers in
  let msg = {
    Q.ip = !!server_ip;
    Q.port = !!server_port;
  } in 
   
  List.iter (fun serv ->
	       (*Printf.printf "PING %s %d \n" (Ip.to_string serv.server_ip) serv.server_port;*)
	       let to_addr = Unix.ADDR_INET((Ip.to_inet_addr serv.server_ip),serv.server_port+4) in 
		 udp_send sock to_addr (M.QueryServersUdpReq  msg)
	    ) !other_servers;
  add_infinite_timer 3. wait_ping
  
  


let save_servers_liste () = 
  known_server =:= List.map (fun temp ->
			       (temp.server_ip,temp.server_port)) (!other_servers) 
			  
