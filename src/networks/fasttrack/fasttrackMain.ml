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

open FasttrackClients
open CommonOptions
open CommonFile
open CommonComplexOptions
open BasicSocket
open Options
open FasttrackComplexOptions
open FasttrackOptions
open FasttrackGlobals
open FasttrackTypes
open CommonTypes
open FasttrackServers
  
let disable enabler () =
  enabler := false;
  Hashtbl2.safe_iter (fun h -> 
      match h.host_server with
        None -> ()
      | Some s -> FasttrackServers.disconnect_server s) hosts_by_key;
  Hashtbl2.safe_iter (fun c -> disconnect_client c) clients_by_uid;
  (match !listen_sock with None -> ()
    | Some sock -> 
        listen_sock := None;
        TcpServerSocket.close sock "");
  if !!enable_fasttrack then enable_fasttrack =:= false;
  match !udp_sock with
    None -> ()
  | Some sock -> 
      udp_sock := None;
      UdpSocket.close sock "disabled"
      
let enable () =

  let enabler = ref true in
  network.op_network_disable <- disable enabler;
  
  if not !!enable_fasttrack then enable_fasttrack =:= true;

  List.iter (fun (ip,port) -> 
      ignore (new_host ip port Ultrapeer)) !!ultrapeers;

  List.iter (fun (ip,port) -> 
      ignore (new_host ip port Peer)) !!peers;
  
  add_session_timer enabler 1.0 (fun timer ->
      FasttrackServers.manage_hosts ();
      Fasttrack.connect_servers FasttrackServers.connect_server;      
  );

  FasttrackServers.ask_for_files ();
  
  add_session_timer enabler 60.0 (fun timer ->
      FasttrackServers.ask_for_files ();
      Fasttrack.send_pings ();
  );

  FasttrackServers.recover_files ();
  add_session_timer enabler 600.0 (fun timer ->
      FasttrackServers.recover_files ();
  );

  FasttrackClients.listen ();
(*  let sock = (UdpSocket.create Unix.inet_addr_any
        !!client_port (FasttrackProtocol.udp_handler 
        FasttrackServers.udp_handler)) in
  udp_sock := Some sock;
  
  UdpSocket.set_write_controler sock CommonGlobals.udp_write_controler;
*)
  ()
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_fasttrack);
  network.op_network_save_complex_options <- FasttrackComplexOptions.save_config;
  (*
  network.op_network_load_simple_options <- 
    (fun _ -> 
      try
        Options.load fasttrack_ini;
      with Sys_error _ ->
          FasttrackComplexOptions.save_config ()
);
  *)
  network.op_network_enable <- enable;
  network.network_config_file <- [fasttrack_ini];
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
  CommonInteractive.register_gui_options_panel "Fasttrack" 
  gui_fasttrack_options_panel
  
  
let main (toto: int) = ()
    
