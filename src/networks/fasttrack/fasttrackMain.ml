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
  
let is_enabled = ref false
 
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      Hashtbl2.safe_iter (fun h -> 
          match h.host_server with
            None -> ()
          | Some s -> FasttrackServers.disconnect_server s Closed_by_user) 
      hosts_by_key;
      Hashtbl2.safe_iter (fun c -> disconnect_client c Closed_by_user) 
      clients_by_uid;
      (match !listen_sock with None -> ()
        | Some sock -> 
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      if !!enable_fasttrack then enable_fasttrack =:= false;
      match !udp_sock with
        None -> ()
      | Some sock -> 
          udp_sock := None;
          UdpSocket.close sock Closed_by_user
    end
    
let enable () =
  if not !is_enabled then
    let enabler = ref true in
    
    (try
        let cipher = create_cipher () in
        init_cipher cipher (Int32.of_int 123456789) 0x29;
        let s = String.create 12 in
        cipher_packet_set cipher s 0;
        assert (s = "ú\000¶+\007[Í\021né\135»");
        lprintf "cipher_packet_set s = \"%s\"\n" (String.escaped s);
        let s = "123456789abcdefghijklm\233\234\235" in
        apply_cipher cipher s 0 (String.length s);
        assert (s = "\016Òõñ\144Ug\028ZåÀ8°§À\008\139\019\018ZÁ7âúi");
        lprintf "apply_cipher s = \"%s\"\n" (String.escaped s);
        cipher_free cipher;
      with _ ->
          lprintf "The Fasttrack plugin will not work on your computer, since\n";
          lprintf "the encryption algorithm does not work correctly.\n";
          lprintf "You can try to solve this problem by hacking the C files in\n";
          lprintf "   mldonkey/src/networks/fasttrack/*.c \n");
      
    
    is_enabled := true;
    network.op_network_disable <- disable enabler;
    
    if not !!enable_fasttrack then enable_fasttrack =:= true;
    
    List.iter (fun (ip,port) -> 
        ignore (new_host ip port Ultrapeer)) !!ultrapeers;
    
    List.iter (fun (ip,port) -> 
        ignore (new_host ip port Peer)) !!peers;
    
    add_session_timer enabler 1.0 (fun timer ->
        FasttrackServers.manage_hosts ();
        FasttrackServers.connect_servers FasttrackServers.connect_server;      
    );
    
    FasttrackServers.ask_for_files ();
    
    add_session_timer enabler 60.0 (fun timer ->
        FasttrackServers.ask_for_files ();
    );
    
    FasttrackServers.recover_files ();
    add_session_timer enabler 600.0 (fun timer ->
        FasttrackServers.recover_files ();
    );
    
(*    FasttrackClients.listen (); *)
(*  let sock = (UdpSocket.create Unix.inet_addr_any
        !!client_port (FasttrackProtocol.udp_handler 
        FasttrackServers.udp_handler)) in
  udp_sock := Some sock;
  
  UdpSocket.set_write_controler sock CommonGlobals.udp_write_controler;
*)
  ()
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_fasttrack);
  option_hook enable_fasttrack (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_fasttrack then network_enable network
      else network_disable network);
  network.op_network_save_complex_options <- FasttrackComplexOptions.save_config;
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
    
