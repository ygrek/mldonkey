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
open Options

open BasicSocket
open TcpBufferedSocket
  
open CommonSwarming
open CommonNetwork
open CommonInteractive
open CommonClient
open CommonFile
open CommonComplexOptions
open CommonTypes
open CommonOptions
open CommonGlobals
  
open DonkeyMftp
open DonkeyProtoCom
open DonkeyServers
open DonkeyComplexOptions
open DonkeyOneFile
open DonkeyFiles
open DonkeyTypes  
open DonkeyGlobals
open DonkeyClient
open DonkeyThieves
open DonkeyOptions


let _ =
  network.op_network_is_enabled <- (fun _ -> !!enable_donkey);
  option_hook enable_donkey (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_donkey then network_enable network
        else network_disable network);
  network.network_config_file <- [
    donkey_ini]

let hourly_timer timer =
  DonkeyClient.clean_groups ();
  DonkeyClient.clean_requests ();
  Hashtbl.clear udp_servers_replies;
  DonkeyThieves.clean_thieves ();
  DonkeyNeighbours.recover_downloads !current_files
    
let quarter_timer timer =
  clean_join_queue_tables ()

let fivemin_timer timer =
  DonkeyShare.send_new_shared ();
  DonkeyChunks.duplicate_chunks ();
  DonkeySources.clean_sources ();
  clients_root := []

let second_timer timer =
  (try
      DonkeySources.connect_sources connection_manager;
    with e ->
        if !verbose_sources then 
          lprintf "Exception %s while checking sources\n" 
            (Printexc2.to_string e)
  )

let halfmin_timer timer =
  DonkeyServers.update_master_servers ()
(*  DonkeyIndexer.add_to_local_index_timer () *)


let local_login () =
  if !!login = "" then !!global_login else !!login
  
let is_enabled = ref false
  
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      if !!enable_donkey then enable_donkey =:= false;
      Hashtbl2.safe_iter (fun s -> disconnect_server s Closed_by_user)
      servers_by_key;
      H.iter (fun c -> DonkeyClient.disconnect_client c Closed_by_user) 
      clients_by_kind;
      (match !listen_sock with None -> ()
        | Some sock -> 
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      (match !reversed_sock with None -> ()
        | Some sock -> 
            reversed_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      (match !udp_sock with None -> ()
        | Some sock -> 
            udp_sock := None;
            UdpSocket.close sock Closed_by_user);
      servers_list := [];
      if !!enable_donkey then enable_donkey =:= false;
      DonkeyOvernet.disable ()
    end

let reset_tags () =
  client_to_client_tags :=
  [
    string_tag "name" (local_login ());
    int_tag "version" protocol_version;
    int_tag "port" !client_port;
  ];      
  client_to_server_tags :=
  [
    string_tag "name" (local_login ());
    int_tag "version" protocol_version;
    int_tag "port" !client_port;
  ];      
  if Autoconf.has_zlib then
    client_to_server_tags := (int_tag "extended" 1)::!client_to_server_tags;
  emule_info_tags := [
    int_tag "compression" 0;
    int_tag "udpport" (!!port+4);
    int_tag "sourceexchange" 1;
    int_tag "comments" 1;
    int_tag "compatableclient" 10; 
    int_tag "extendedrequest" 1;
    int_tag "udpver" 1;
  ];
  overnet_connect_tags :=
  [
    string_tag "name" (local_login ());
    int_tag "version" !!DonkeyOvernet.overnet_protocol_connect_version; 
  ];
  overnet_connectreply_tags :=
  [
    string_tag "name" (local_login ());
    int_tag "version" !!DonkeyOvernet.overnet_protocol_connectreply_version; 
  ]
  
let enable () =
  if not !is_enabled then 
    let enabler = ref true in
    is_enabled := true;
    network.op_network_disable <- disable enabler;
    
    if not !!enable_donkey then enable_donkey =:= true;
    
    try
(*  DonkeyClient.verbose := true; *)

(**** LOAD OTHER OPTIONS ****)
      
      DonkeyIndexer.load_comments comment_filename;
      DonkeyIndexer.install_hooks ();
      CommonGlobals.do_at_exit (fun _ -> DonkeyIndexer.save_history ());

(*
    BasicSocket.add_timer 10. (fun timer ->
        BasicSocket.reactivate_timer timer;
        match !indexer with
          None -> ()
        | Some (t_in, t_out) ->
            if TcpBufferedSocket.can_write t_out then begin
                TcpBufferedSocket.close t_out "timed";
                indexer := None;
              end)
*)
      
      Hashtbl.iter (fun _ file ->
          try
            if file_state file <> FileDownloaded then begin
                current_files := file :: !current_files;
(*                set_file_size file (file_size file) *)
              end else begin
                try
                  let file_disk_name = file_disk_name file in
                  if Unix32.file_exists file_disk_name &&
                    Unix32.getsize file_disk_name <> Int64.zero then begin
                      lprintf "FILE DOWNLOADED"; lprint_newline ();
                      DonkeyShare.remember_shared_info file file_disk_name;
                      lprintf "REMEMBERED"; lprint_newline ();
                      file_completed (as_file file);
                      let file_id = Filename.basename file_disk_name in
                      ignore (CommonShared.new_shared "completed" 0 (
                          file_best_name file )
                        file_disk_name);
                      
                      lprintf "COMPLETED"; lprint_newline ();
                      (try
                          let format = CommonMultimedia.get_info file_disk_name
                          in
                          file.file_format <- format
                        with _ -> ());        
                    end
                  else raise Not_found
                with _ -> 
                    file_commit (as_file file)
              end
          with e ->
              lprintf "Exception %s while recovering download %s"
                (Printexc2.to_string e) (file_disk_name file); lprint_newline ();
      ) files_by_md4;
      
      let list = ref [] in
(* Normally, we should check that downloaded files are still there.
  
  *)    
      let list = ref [] in
      List.iter (fun file ->
          if Unix32.file_exists file.sh_name then begin
              Hashtbl.add shared_files_info file.sh_name file;
              list := file :: !list
            end
      ) !!known_shared_files;
      known_shared_files =:= !list;

(**** CREATE WAITING SOCKETS ****)
      let rec find_port new_port =
        try
          let sock = TcpServerSocket.create 
              "donkey client server"
              (Ip.to_inet_addr !!client_bind_addr)
            !!port (client_connection_handler false) in
          
          TcpServerSocket.set_accept_controler sock connections_controler;
          listen_sock := Some sock;
          port =:= new_port;
          
          begin try
              let sock =
                (UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
                  (!!port + 4) 
                  (udp_handler DonkeyUdp.udp_client_handler))
              in
              udp_sock := Some sock;
              UdpSocket.set_write_controler sock udp_write_controler;
            with e ->
                lprintf "Exception %s while binding UDP socket"
                  (Printexc2.to_string e);
                lprint_newline ();
          end;
          sock
        with e ->
            if !find_other_port then find_port (new_port+1)
            else  raise e
      in
      let sock = find_port !!port in
      DonkeyOvernet.enable enabler;
      
      begin
        match Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock)) with
          Unix.ADDR_INET (ip, port) ->
            client_port :=  port
        | _ -> failwith "Bad socket address"
      end;
      
      let port = !client_port in
      
      reset_tags ();
      
      Options.option_hook global_login reset_tags;
      Options.option_hook login reset_tags;

(**** START TIMERS ****)
      add_session_option_timer enabler check_client_connections_delay 
        DonkeyUdp.force_check_locations;
      
      add_session_option_timer enabler buffer_writes_delay 
        (fun _ -> Unix32.flush ());
      
      add_session_option_timer enabler check_connections_delay 
        DonkeyServers.check_server_connections;
      add_session_option_timer enabler compute_md4_delay 
        (fun _ ->
          DonkeyOneFile.check_files_downloaded ();
          DonkeyShare.check_shared_files ()
      );
      add_session_timer enabler 5.0 DonkeyServers.walker_timer;
      add_session_timer enabler 1.0 DonkeyServers.udp_walker_timer;
      
      add_session_timer enabler 3600. hourly_timer;
      add_session_timer enabler 30. halfmin_timer;
      add_session_timer enabler 300. fivemin_timer;
      add_session_timer enabler 900. quarter_timer;
      add_session_timer enabler 1. second_timer;
      add_session_timer enabler 60. (fun _ ->
          (try
              List.iter (fun file ->
                  match file.file_swarmer with
                    None -> ()
                  | Some swarmer ->
                      Int64Swarmer.verify_one_block swarmer
              ) !current_files
            with _ -> ()
          );
          (try
              DonkeyServers.query_locations_timer ();
            with _ -> ());
          List.iter (fun file -> DonkeyShare.must_share_file file) 
          !new_shared_files;  
          new_shared_files := [];
      );
      add_session_option_timer enabler remove_old_servers_delay 
          DonkeyServers.remove_old_servers;
      
    
    DonkeyComplexOptions.load_sources ();
    
(**** START PLAYING ****)  
    (try DonkeyUdp.force_check_locations () with _ -> ());
    (try force_check_server_connections true with _ -> ());

  with e ->
      lprintf "Error: Exception %s during startup\n"
        (Printexc2.to_string e)

let _ =

(*  DonkeyFiles.install_hooks (); *)
  DonkeyIndexer.init ();

(*
  file_ops.op_file_commit <- (fun file ->
      DonkeyInteractive.save_file file 
        (DonkeyInteractive.saved_name file);
      lprintf "SAVED"; lprint_newline ();
);
  *)
  network.op_network_enable <- enable;
(*  network.network_config_file <- []; *)
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
        network_connected = List.length (connected_servers ());
      });
  CommonInteractive.register_gui_options_panel "eDonkey" 
    gui_donkey_options_panel;
  CommonInteractive.register_gui_options_panel "Overnet" 
    DonkeyOvernet.gui_overnet_options_panel

  
let _ =
  CommonInteractive.add_main_options
    
    [
    "-dump", Arg.String (fun file -> 
        DonkeyImport.dump_file file), " <filename> : dump file";
    "-known", Arg.String (fun file ->
        let module K = DonkeyImport.Known in
        let s = File.to_string file in
        let t = K.read s in
        K.print t;
        lprint_newline ();
    ), " <filename> : print a known.met file";
    "-part", Arg.String (fun file ->
        let module K = DonkeyImport.Part in
        let s = File.to_string file in
        let t = K.read s in
        K.print t;
        lprint_newline ();
    ), " <filename> : print a .part.met file";
    "-server", Arg.String (fun file ->
        let module K = DonkeyImport.Server in
        let s = File.to_string file in
        let t = K.read s in
        K.print t;
        lprint_newline ();
        exit 0
    ), " <filename> : print a server.met file";
    "-pref", Arg.String (fun file ->
        let module K = DonkeyImport.Pref in
        let s = File.to_string file in
        let t = K.read s in
        K.print t;
        lprint_newline ();
    ), " <filename> : print a server.met file";
  ]
