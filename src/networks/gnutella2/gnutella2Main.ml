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
open BasicSocket
open Options
  
open CommonOptions
open CommonFile
open CommonComplexOptions
open CommonTypes
open CommonNetwork
open CommonHosts

open Gnutella2Clients
open Gnutella2ComplexOptions
open Gnutella2Options
open Gnutella2Globals
open Gnutella2Types
open Gnutella2Servers

let is_enabled = ref false
   
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      Hashtbl2.safe_iter (fun h -> 
          match h.host_server with
            None -> ()
          | Some s -> Gnutella2Servers.disconnect_server s Closed_by_user)
      H.hosts_by_key;
      Hashtbl2.safe_iter (fun c -> disconnect_client c Closed_by_user) clients_by_uid;
      (match !listen_sock with None -> ()
        | Some sock -> 
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      if !!enable_gnutella2 then enable_gnutella2 =:= false;
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
    
    if not !!enable_gnutella2 then enable_gnutella2 =:= true;
    
    List.iter (fun (ip,port) -> 
        ignore (H.new_host ip port (true))) !!g2_ultrapeers;
    
    List.iter (fun (ip,port) -> 
        ignore (H.new_host ip port (false))) !!g2_peers;
    
    add_session_timer enabler 1.0 (fun timer ->
        H.manage_hosts ();
        Gnutella2Proto.resend_udp_packets ();
        Gnutella2.connect_servers Gnutella2Servers.connect_server;      
    );
    
    Gnutella2Servers.ask_for_files ();
    
    add_session_timer enabler 60.0 (fun timer ->
        Gnutella2Servers.ask_for_files ();
        Gnutella2.send_pings ();
    );
    
    Gnutella2Servers.recover_files ();
    add_session_timer enabler 3600.0 (fun timer ->
        Gnutella2Servers.recover_files ();
    );
    
    Gnutella2Clients.listen ();
    let sock = (UdpSocket.create Unix.inet_addr_any
          !!client_port (Gnutella2Protocol.udp_handler 
            Gnutella2Servers.udp_handler)) in
    udp_sock := Some sock;
    
    UdpSocket.set_write_controler sock CommonGlobals.udp_write_controler;
    ()
    
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_gnutella2);
  option_hook enable_gnutella2 (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_gnutella2 then network_enable network
      else network_disable network);
  network.op_network_save_complex_options <- Gnutella2ComplexOptions.save_config;
  (*
  network.op_network_load_simple_options <- 
    (fun _ -> 
      try
        Options.load gnutella_ini;
      with Sys_error _ ->
          Gnutella2ComplexOptions.save_config ()
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
        network_netflags = network.network_flags;
        network_enabled = network.op_network_is_enabled ();
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
        network_connected = List.length !g2_connected_servers;
      });
  CommonInteractive.register_gui_options_panel "Gnutella2" 
  gui_gnutella_options_panel
  
  
let main (toto: int) = ()
    
