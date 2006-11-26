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

open Md4
open CommonGlobals
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
open ServerSubscriptions


let enable () =
  
  if not !!enable_server then enable_server =:= true;
  
  ignore(TcpServerSocket.create "server client bind socket"
      Unix.inet_addr_any !!server_port ServerClients.handler);
  
  let udp_server_sock = 
    UdpSocket.create (Ip.to_inet_addr !!client_bind_addr) (!!server_port + 4) 
    ServerUdp.udp_handler in
  udp_sock := Some udp_server_sock;
  UdpSocket.set_write_controler udp_server_sock udp_write_controler;
  begin
    match !!seed_ip with
      None -> ()
    | Some ip ->
        udp_send udp_server_sock ip  (!!seed_port + 4) (
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
          lprintf "Your server create a new group bind on %d \n" (!!server_port+5);
          group_id := !!server_md4;
          incr server_counter
        end 
      else
        begin
          let (ip,port)= List.hd !!known_master in
          lprintf "I connect to master %s:%d\n" (Ip.to_string ip) port;  
          ServerServer.connect_a_group();
        end;
      add_infinite_option_timer notify_time_out action_notify_servers;
      add_infinite_option_timer connect_time_out action_connect_servers;
    end;
  
  
  if !!save_log then ServerLog.initialized(); 
  
  add_infinite_option_timer send_server_stat_delay ServerClients.send_stat_to_clients;


(* Subs timer *)
  add_infinite_timer 60. (fun timer -> Subs.remove_timeout_subs ());
  
  add_infinite_timer 60. (fun timer ->
      nb_udp_query_sec := float !nb_udp_query_count;
      nb_udp_query_count := 0;
      nb_udp_loc_sec := float !nb_udp_loc_count;
      nb_udp_loc_count := 0;
      
      nb_udp_loc_reply_sec :=  float !nb_udp_loc_reply_count;
      nb_udp_loc_reply_count := 0;
      
      nb_udp_req_sec := float !nb_udp_req_count;
      nb_udp_req_count := 0;
      if ((!nb_udp_req_sec /. 60.) > !!limite_udp_received_sec) then
        stop_udp := true;
      
      if ((!nb_udp_req_sec /. 60.) < !!limite_udp_received_sec) then
        stop_udp := false;
      
      
      nb_tcp_req_sec := float !nb_tcp_req_count;
      nb_tcp_req_count := 0;
      
      
      nb_udp_reply_sec := float !nb_udp_reply_count;
      nb_udp_reply_count := 0;
      
      info_percent := !nb_notifs,((float !nb_info) /. (float !nb_notifs) *. 100.);
      nb_info := 0;
      nb_notifs := 0;
  
  );
  
  add_infinite_option_timer ping_known_servers (fun timer -> 
      nb_udp_ping_server_sec := float !nb_udp_ping_server_count;
      nb_udp_ping_server_count := 0 
  );

(*add_infinite_timer	1000. ServerUdp.get_info_servers;	*)	      
(*ServerUdp.test();*)

(* add_infinite_timer 1. ServerClients.check_notifications; *)
  
  
  add_infinite_option_timer ping_known_servers ServerUdp.ping_servers;
  
  ServerUdp.hello_world();
  
  
  lprintf "server started at %d" !!server_port; lprint_newline ()
  
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
        Printf.bprintf buf "SERVER STAT\n";
        Printf.bprintf buf "nb_client:%d\nnb_files:%d\n" !nconnected_clients !nshared_md4;
        let real = ref 0 in
        let remote = ref 0 in
        let ml_user = ref 0 in
        Hashtbl.iter (fun _ c ->
            match c with 
              LocalClient c -> 
                incr real;
                if c.client_mldonkey >= 2 then
                  incr ml_user
            | RemoteClient _ -> incr remote
        ) clients_by_id;
        Printf.bprintf buf "nb_real_client:%d and remote:%d \n" !real !remote;
        Printf.bprintf buf "%d of your client use mldonkey\n" !ml_user;
        let local = ref 0 in
        let remote = ref 0 in
        let remote_local = ref 0 in
        let lst = ServerLocate.get_locate_table() in
        List.iter (fun (md4,md4_list) ->
            if (List.exists (
                  fun loc ->
                    loc.loc_local) md4_list) then
              begin
                if (List.exists (
                      fun loc ->
                        not loc.loc_local) md4_list) then
                  incr remote_local
                else
                  incr local
              end
            else
              begin	       
                incr remote
              end
        ) lst;
        Printf.bprintf buf "nb_real_file %d local:%d , remote: %d , remote&local: %d\n" (!local + !remote + !remote_local) !local !remote !remote_local;   
        Printf.bprintf buf "nb_know_alive_server:%d\n" !n_alive;
        Printf.bprintf buf "nb_know_other_server:%d\n" !n_dead;
        Printf.bprintf buf "client_counter:%d\n" !client_counter;
        ""
    ),":\t\t\t\t\tprint server's stat";
    
    "cl", Arg_none(fun o -> 
        let buf = o.conn_buf in
        let lst = ServerLocate.get_locate_table() in
        List.iter (fun (md4,md4_list) -> 
            List.iter (fun loc -> 
                try
                  let c = Hashtbl.find clients_by_id loc.loc_ip in
                  (match c with 
                      RemoteClient c ->  
                        if loc.loc_local then
                          Printf.bprintf buf "WARNING: remote locality incoherence\n";
                        if not (List.mem md4 c.remote_client_files) then
                          Printf.bprintf buf "WARNING: Remote client %s doesn't reference the file %s\n" 
                            (Ip.to_string c.remote_client_remote_id) (Md4.to_string md4);
                        ()
                    | LocalClient c ->
                        if not loc.loc_local then
                          Printf.bprintf buf "WARNING: local locality incoherence\n";
                        if not (List.mem md4 c.client_files) then
                          Printf.bprintf buf "WARNING: Local client %s doesn't reference the file %s\n" 
                            (Ip.to_string c.client_id) (Md4.to_string md4);
                        ()
                  )
                with _ ->  Printf.bprintf buf "WARNING: File %s have a invalid client id %s\n"
                      (Md4.to_string md4) (Ip.to_string loc.loc_ip);
                    ()
            ) md4_list
        ) lst;
        ""
    ),":\t\t\t\t\tprint server's coherence failure";
    
    "cc", Arg_none(fun o -> 
        let buf = o.conn_buf in
        Hashtbl.iter (fun id c ->
            match c with 
              RemoteClient c ->
                List.iter (fun md4 ->
                    try
                      if not (ServerLocate.exist md4 id) then
                        Printf.bprintf buf "WARNING: Remote client %s reference the file %s who not\n" 
                          (Ip.to_string c.remote_client_remote_id) (Md4.to_string md4)
                    with _ ->  Printf.bprintf buf "WARNING: Remote client %s reference the file %s who not\n" 
                          (Ip.to_string c.remote_client_remote_id) (Md4.to_string md4)
                ) c.remote_client_files
            | LocalClient c ->
                List.iter (fun md4 ->
                    try
                      if not (ServerLocate.exist md4 id) then
                        Printf.bprintf buf "WARNING: Local client %s reference the file %s who not\n" 
                          (Ip.to_string c.client_id) (Md4.to_string md4)
                    with _ ->  Printf.bprintf buf "WARNING: Remote client %s reference the file %s who not\n" 
                          (Ip.to_string c.client_id) (Md4.to_string md4)
                )  c.client_files
        ) clients_by_id; 
        ""
    ),":\t\t\t\t\tprint server's coherence failure";
    
    
    
    
    "packets", Arg_none(fun o -> 
        let buf = o.conn_buf in
        Printf.bprintf buf "Messages received during the last min per second\n";
        Printf.bprintf buf "nb tcp requets:%f\n" (!nb_tcp_req_sec /. 60.);
        Printf.bprintf buf "nb udp requets/second :%f\n" (!nb_udp_req_sec /. 60.);
        Printf.bprintf buf "nb udp Location/second:%f, %f is return \nnb udp Query/second:%f\n" (!nb_udp_loc_sec /. 60.) (!nb_udp_loc_reply_sec /. !nb_udp_loc_sec *. 100.) (!nb_udp_query_sec /. 60.);
        if (!stop_udp) then
          Printf.bprintf buf "I received too many queries and i stop all udp reply to server\n"
        else
          Printf.bprintf buf "I reply server and client udp message\n";
        Printf.bprintf buf "nb udp rely send/second : %f\n" (!nb_udp_reply_sec /. 60.);
        ""
    ), ":\t\t\t\tprint number of udp and tcp packet rec";
    
    "clients", Arg_one (fun arg o -> 
        let buf = o.conn_buf in
        ServerClients.bprint buf clients_by_id arg;
        ""
    ),":\t\t\t\tprint clients general info";
    
    "sources", Arg_none (fun o ->
        let buf =  o.conn_buf in
        let lst = ServerLocate.get_list_of_md4() in
        List.iter (fun (md4,nb_source) -> 
            try
              let tags = ServerIndexer.get_def md4 in
              bprint_tags buf tags.f_tags;
              Printf.bprintf buf "-->Actualy %d sources\n" nb_source 
            with _ ->
                Printf.bprintf buf "NO file def for %s\n" (Md4.to_string md4);
                Printf.bprintf buf "-->Actualy %d sources\n" nb_source 
        ) lst;
        ""
    ),":\t\t\t\tprint files shared on the server";
    
    "ginfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf buf "GROUP :%d servers for %d remote sources for %d remote clients\n" !nconnected_servers !nshared_remote_md4 !nremote_clients;
        let x,y = !info_percent in
        Printf.bprintf buf "for %d notif nees %f info \n" x y;
        bprint_server_info buf "";
        ""
    ),":\t\t\t\t\tprint info about servers in the group";
    
    "jg", Arg_two (fun ip port o -> 
        let buf = o.conn_buf in
        Printf.bprintf buf "try to connect %s %s\n" ip port;
        ServerServer.join_a_group (Ip.of_string ip) (int_of_string port);
        ""
    ),":\t\t\t\t\ttry to join a server master";
    
    "qg",Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf buf "diconnect of the group %s\n" (Md4.to_string !group_id);
        ServerServer.disconnect();
        ""
    ),":\t\t\t\t\tdisconnect of the group";
    
    "tags",Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf buf "known tag name = ";
        List.iter (fun tagname -> 
            Printf.bprintf buf "\"%s\" " tagname)
        !tag_name_list;
        Printf.bprintf buf "\n";
        ""
    ),":\t\t\t\t\tgives the list of known tag name";
  
  ];
  
  
  network.network_config_file <- Some server_ini;
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_server);
  option_hook enable_server (fun _ ->
      if !CommonOptions.start_running_servers then
        if !!enable_server then network_enable network
      else network_disable network);
  network.op_network_enable <- enable;
  network.op_network_info <- (fun n ->
      { 
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            None -> "" | Some opfile -> options_file_name opfile);
        network_netname = network.network_name;
        network_enabled = network_is_enabled network;
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
      })
  
