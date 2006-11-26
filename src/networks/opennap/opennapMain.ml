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

open CommonNetwork
open Printf2
open OpennapClients
open OpennapServers
open CommonOptions
open CommonTypes
open CommonComplexOptions
open CommonFile
open Options
open BasicSocket
open OpennapTypes
open OpennapGlobals
open OpennapOptions  

let is_enabled = ref false
  
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      Hashtbl2.safe_iter (fun s -> disconnect_server s Closed_by_user) servers_by_addr;
      Hashtbl2.safe_iter (fun c -> disconnect_client c Closed_by_user) clients_by_name;
      (match !listen_sock with None -> ()
        | Some sock -> 
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      if !!enable_opennap then enable_opennap =:= false
    end
    
let enable () =
  if not !is_enabled then 
    let enabler = ref true in
    is_enabled := true;
    network.op_network_disable <- disable enabler;
    if not !!enable_opennap then enable_opennap =:= true;
    if !!use_napigator then
      Napigator.load_servers_list !!servers_list_url
        (fun list -> 
          lprintf "LIST OF %d SERVERS DOWNLOADED"
            (List.length list)
          ; lprint_newline ();
          List.iter 
            (fun (desc, ip, port, network) ->
              let s = new_server ip port in
              s.server_desc <- desc;
              s.server_net <- network) list);
    
    add_session_timer enabler 1.0 (fun timer ->
        OpennapServers.connect_servers ());
    
    add_session_timer enabler 60.0 (fun timer ->
        OpennapServers.ask_for_files ();
    );
    
    add_session_timer enabler 300.0 (fun timer ->
        OpennapServers.recover_files ());
    
    OpennapClients.listen ();
    
  ()

    
open CommonTypes
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_opennap);
  option_hook enable_opennap (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_opennap then network_enable network
      else network_disable network);
  network.op_network_enable <- enable;
  network.network_config_file <- [opennap_ini];
  network.op_network_info <- (fun n ->
      { 
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            [] -> "" | opfile :: _ -> options_file_name opfile);
        network_netname = network.network_name;
        network_netflags = network.network_flags;
        network_enabled = network_is_enabled network;
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
        network_connected = List.length !connected_servers;
      });
  network.op_network_share <- add_shared;
  CommonInteractive.register_gui_options_panel 
    "Napster" gui_opennap_options_panel;
