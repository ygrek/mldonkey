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

open GnutellaClients
open CommonOptions
open CommonFile
open CommonComplexOptions
open BasicSocket
open Options
open GnutellaComplexOptions
open GnutellaOptions
open GnutellaGlobals
open GnutellaTypes
open CommonTypes
open GnutellaServers

let is_enabled = ref false
   
let disable enabler () =
  if !enabler then begin
      enabler := false;
      Hashtbl2.safe_iter (fun h -> 
          match h.host_server with
            None -> ()
          | Some s -> GnutellaServers.disconnect_server s Closed_by_user)
      hosts_by_key;
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
    
let enable () =
  if not !is_enabled then
    let enabler = ref true in
    is_enabled := true;
  network.op_network_disable <- disable enabler;
  
  if not !!enable_gnutella then enable_gnutella =:= true;

  List.iter (fun (ip,port) -> 
      ignore (new_host ip port true 1)) !!g1_ultrapeers;

  List.iter (fun (ip,port) -> 
      ignore (new_host ip port true 2)) !!g2_ultrapeers;

  List.iter (fun (ip,port) -> 
      ignore (new_host ip port false 1)) !!g1_peers;

  List.iter (fun (ip,port) -> 
      ignore (new_host ip port false 2)) !!g2_peers;
  
  add_session_timer enabler 1.0 (fun timer ->
      GnutellaServers.manage_hosts ();
      Gnutella2Proto.resend_udp_packets ();
      if !!g1_enabled then
        Gnutella1.connect_servers GnutellaServers.connect_server;      
      if !!g2_enabled then
        Gnutella2.connect_servers GnutellaServers.connect_server;      
      );

  GnutellaServers.ask_for_files ();
  
  add_session_timer enabler 60.0 (fun timer ->
      GnutellaServers.ask_for_files ();
      Gnutella1.send_pings ();
      Gnutella2.send_pings ();
  );

  GnutellaServers.recover_files ();
  add_session_timer enabler 3600.0 (fun timer ->
      GnutellaServers.recover_files ();
  );

  GnutellaClients.listen ();
  let sock = (UdpSocket.create Unix.inet_addr_any
        !!client_port (GnutellaProtocol.udp_handler 
        GnutellaServers.udp_handler)) in
  udp_sock := Some sock;
  
  UdpSocket.set_write_controler sock CommonGlobals.udp_write_controler;
  ()
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_gnutella);
  option_hook enable_gnutella (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_gnutella then network_enable network
      else network_disable network);
  network.op_network_save_complex_options <- GnutellaComplexOptions.save_config;
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
  network.op_network_info <- (fun n ->
      { 
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            [] -> "" | opfile :: _ -> options_file_name opfile);
        network_netname = network.network_name;
        network_enabled = network.op_network_is_enabled ();
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
      });
  CommonInteractive.register_gui_options_panel "Gnutella" 
  gui_gnutella_options_panel
  
  
let main (toto: int) = ()
    
