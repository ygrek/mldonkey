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

open Printf2
open CommonNetwork

open CommonHosts
open GnutellaClients
open CommonOptions
open BasicSocket
open Options
open GnutellaOptions
open GnutellaGlobals
open GnutellaTypes
open CommonTypes

let is_enabled = ref false
   
let disable enabler () =
  if !enabler then begin
      enabler := false;
      
      List.iter (fun f -> 
          try f () with e ->
              lprintf "Exception %s in plugin_enable_hooks\n"
                (Printexc2.to_string e)
      ) !plugin_disable_hooks;
      
      Hashtbl2.safe_iter (fun h -> 
          match h.host_server with
            None -> ()
          | Some s -> GnutellaServers.disconnect_server s Closed_by_user)
      H.hosts_by_key;
      Hashtbl2.safe_iter (fun c -> disconnect_client c Closed_by_user) clients_by_uid;
      (match !listen_sock with None -> ()
        | Some sock -> 
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      if !!enable_gnutella then enable_gnutella =:= false;
      match !udp_sock with
        None -> ()
      | Some sock -> 
          udp_sock := None;
          UdpSocket.close sock Closed_by_user
    end

let udp_handler sock event =
(*  lprintf "Gnutella: udp_handler called\n"; *)
  match event with
    UdpSocket.READ_DONE ->
(*      lprintf "Gnutella: udp read_packets...\n"; *)
      UdpSocket.read_packets sock (fun p -> 
          try
(*            lprintf "Gnutella: udp one packet...\n"; *)
            let (ip,port) = match p.UdpSocket.udp_addr with
              | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
              | _ -> raise Not_found
            in
(*            lprintf "Gnutella: calling udp_client_handler %s:%d\n"
              (Ip.to_string ip) port; *)
            let buf = Bytes.to_string p.UdpSocket.udp_content in
            GnutellaHandler.udp_client_handler ip port buf 
          with e ->
              lprintf "Error %s in udp_handler\n"
                (Printexc2.to_string e); 
      ) ;
  | _ -> ()
    
let enable () =
  if not !is_enabled then
    let enabler = ref true in
    GnutellaProto.check_primitives ();
    is_enabled := true;
    network.op_network_disable <- disable enabler;
    
    if not !!enable_gnutella then enable_gnutella =:= true;
    
    List.iter (fun f -> 
        try f enabler with e ->
            lprintf "Exception %s in plugin_enable_hooks\n"
              (Printexc2.to_string e)
    ) !plugin_enable_hooks;
    
    add_session_timer enabler 1.0 (fun timer ->
        GnutellaServers.manage_hosts ();
        GnutellaProto.resend_udp_packets ();
    );
    
    GnutellaServers.ask_for_files ();

    add_session_timer enabler 60.0 (fun timer ->
        ( try 
        GnutellaServers.ask_for_files ();
          with e -> lprintf_nl "ASK_FOR_FILE FAILED: %s" (Printexc2.to_string e));

        ( try
        GnutellaServers.send_pings ();
        with e -> lprintf_nl "SEND_PINGS FAILED: %s" (Printexc2.to_string e));  

        (* Connect only every 60 seconds to new servers to prevent
           hammering on them. It would be better to remember the
           last time of a connection attempt to each and only
           retry after a certain time. *)
        GnutellaServers.connect_servers GnutellaServers.connect_server;


        if !should_update_shared_files then begin
            should_update_shared_files := false;
            GnutellaHandler.update_shared_files ()
          end;

    );

    GnutellaInteractive.recover_files ();
    add_session_timer enabler GnutellaProto.recover_files_delay (fun timer ->
        GnutellaInteractive.recover_files ();
    );

    add_session_timer enabler 1800. (fun _ ->
        GnutellaClients.clean_sources ());
    
    GnutellaClients.listen ();
(* Check the port returned by listen, and put the udp socket on it *)
    let sock = (UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
        !!client_port udp_handler ) in
    if !verbose then
      lprintf_nl "UDP socket bound on port %d" !!client_port;
    udp_sock := Some sock;
    
    UdpSocket.set_write_controler sock CommonGlobals.udp_write_controler;
    ()
    
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_gnutella);
  option_hook enable_gnutella (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_gnutella then network_enable network
      else network_disable network);
(*  network.op_network_save_complex_options <- GnutellaComplexOptions.save_config; *)
  (*
  network.op_network_load_simple_options <- 
    (fun _ -> 
      try
        Options.load gnutella_ini;
      with Sys_error _ ->
          GnutellaComplexOptions.save_config ()
);
  *)
  network.op_network_enable <- enable;
  network.network_config_file <- [gnutella_ini];
  network.op_network_recover_temp <- (fun s -> ());

  network.op_network_info <- (fun n ->
      { 
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            [] -> "" | opfile :: _ -> options_file_name opfile);
        network_netflags = network.network_flags;
        network_netname = network.network_name;
        network_enabled = network_is_enabled network;
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
        network_connected_servers = List.length !connected_servers;
      });
  CommonInteractive.register_gui_options_panel "Gnutella" 
  gui_gnutella_options_panel
  
  
let main (toto: int) = ()
    
