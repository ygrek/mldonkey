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

open CommonClient
open CommonFile
open CommonComplexOptions
open CommonTypes
open Options
open DonkeyMftp
open DonkeyProtoCom
open DonkeyServers
open BasicSocket
open DonkeyComplexOptions
open TcpBufferedSocket
open DonkeyOneFile
open DonkeyFiles
open DonkeyTypes  
open DonkeyGlobals
open DonkeyClient
open DonkeyOptions
open CommonOptions
open CommonGlobals


let _ =
  network.op_network_is_enabled <- (fun _ -> !!enable_donkey)

let hourly_timer timer =
  DonkeyServers.remove_old_servers ();
  DonkeyFiles.remove_old_clients ();
  DonkeyClient.clean_groups ();
  DonkeyProtoCom.propagate_working_servers 
    (List.map (fun s -> s.server_ip, s.server_port) (connected_servers()));
  Hashtbl.clear udp_servers_replies
    
let quarter_timer timer =
  ()

let fivemin_timer timer =
  DonkeyFiles.fill_clients_list ();
  DonkeyShare.send_new_shared ()

let second_timer timer =
  DonkeyFiles.check_clients ();
  DonkeyFiles.reset_upload_timer ()

let halfmin_timer timer =
  DonkeyServers.update_master_servers ();
  DonkeyFiles.upload_credit_timer ();
  DonkeyIndexer.add_to_local_index_timer ()

  
let disable enabler () =
  enabler := false;
  if !!enable_donkey then enable_donkey =:= false;
  Hashtbl2.safe_iter (fun s -> disconnect_server s) servers_by_key;
  Hashtbl2.safe_iter (fun c -> disconnect_client c) clients_by_kind;
  (match !listen_sock with None -> ()
    | Some sock -> 
        listen_sock := None;
        TcpServerSocket.close sock "");
  (match !reversed_sock with None -> ()
    | Some sock -> 
        reversed_sock := None;
        TcpServerSocket.close sock "");
  (match !udp_sock with None -> ()
    | Some sock -> 
        udp_sock := None;
        UdpSocket.close sock "");
  clients_list := [];
  servers_list := [];
  if !!enable_donkey then enable_donkey =:= false
  
let enable () =

  let enabler = ref true in
  network.op_network_disable <- disable enabler;
  
  if not !!enable_donkey then enable_donkey =:= true;
  
  try
(*  DonkeyClient.verbose := true; *)

(**** LOAD OTHER OPTIONS ****)
    
    (try Options.load shared_files_ini with _ -> ());
    

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
    
    features =:= !!features;  
    
    Hashtbl.iter (fun _ file ->
        if file_state file <> FileDownloaded then begin
            current_files := file :: !current_files;
            set_file_size file (file_size file)
          end else begin
            try
              if Sys.file_exists file.file_hardname &&
                Unix32.getsize32 file.file_hardname <> Int32.zero then begin
                  file_completed (as_file file.file_file);
                  (try
                      let format = CommonMultimedia.get_info file.file_hardname
                        in
                      file.file_format <- format
                      with _ -> ());        
                end
              else raise Not_found
            with _ -> 
                file_commit (as_file file.file_file)
          end
    ) files_by_md4;
    let list = ref [] in
(* Normally, we should check that downloaded files are still there.
  
  *)    
    let list = ref [] in
    List.iter (fun file ->
        if Sys.file_exists file.sh_name then begin
            Hashtbl.add shared_files_info file.sh_name file;
            list := file :: !list
          end
    ) !!known_shared_files;
    known_shared_files =:= !list;
    Options.save shared_files_ini;

(**** CREATE WAITING SOCKETS ****)
    
    begin try
        udp_sock := Some (UdpSocket.create (Ip.to_inet_addr !!donkey_bind_addr)
          (!!port + 4) 
          (udp_handler DonkeyFiles.udp_client_handler));
      with e ->
          Printf.printf "Exception %s while binding UDP socket"
            (Printexc.to_string e);
          print_newline ();
    end;
    
    let sock = TcpServerSocket.create 
        "donkey client server"
      (Ip.to_inet_addr !!donkey_bind_addr)
      !!port client_connection_handler in
    
    listen_sock := Some sock;
    
    begin
      match Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock)) with
        Unix.ADDR_INET (ip, port) ->
          client_port :=  port
      | _ -> failwith "Bad socket address"
    end;
    
    let port = !client_port in
    let sport = (port mod 256) * 256 + (port / 256) in
    
    begin
      try
        reversed_sock := Some (TcpServerSocket.create "donkey client server"
            (Ip.to_inet_addr !!donkey_bind_addr)
          sport client_connection_handler);
      with _ ->
          Printf.printf "Unable to open Second listening port\n";
          Printf.printf "mldonkey will be enable to receive indirect\n";
          Printf.printf "connections from linux clients due to a bug\n";
          Printf.printf "in these clients.";
          print_newline ();
    end;

    let reset_tags () =
      client_tags :=
      [
        { tag_name = "name"; tag_value =  String !!client_name };
        { tag_name = "version"; tag_value =  Uint32 (Int32.of_int 
              !!DonkeyOptions.protocol_version) };
        { tag_name = "port"; tag_value =  Uint32 (Int32.of_int !client_port) };
      ]
    in
    reset_tags ();

    Options.option_hook DonkeyOptions.protocol_version reset_tags;
    Options.option_hook client_name reset_tags;
    
    DonkeyFiles.fill_clients_list ();
    

(**** START TIMERS ****)
    add_session_option_timer enabler check_client_connections_delay 
      DonkeyFiles.force_check_locations;

    add_session_option_timer enabler check_connections_delay 
      DonkeyServers.check_server_connections;
    add_session_option_timer enabler compute_md4_delay DonkeyOneFile.check_files_md4s;  
    add_session_timer enabler 5.0 DonkeyServers.walker_timer;
    
    add_session_timer enabler 3600. hourly_timer;
    add_session_timer enabler 30. halfmin_timer;
    add_session_timer enabler 300. fivemin_timer;
    add_session_timer enabler 900. quarter_timer;
    add_session_timer enabler 1. second_timer;
    add_session_timer enabler 0.1 DonkeyFiles.upload_timer;
    add_session_timer enabler 60. DonkeyServers.query_locations_timer;

(**** START PLAYING ****)  
    (try force_check_locations () with _ -> ());
    (try force_check_server_connections true with _ -> ());

  with e ->
      Printf.printf "Error: Exception %s during startup"
        (Printexc.to_string e); print_newline ()

let _ =
  
(*  DonkeyFiles.install_hooks (); *)
  DonkeyIndexer.init ();
  
  file_ops.op_file_commit <- (fun file ->
      DonkeyInteractive.save_file file 
        (DonkeyInteractive.saved_name file);
      Printf.printf "SAVED"; print_newline ();
  );
  network.op_network_enable <- enable;
  network.network_prefixes <- ["donkey"];
  network.network_config_file <- None;
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
  
  
  
