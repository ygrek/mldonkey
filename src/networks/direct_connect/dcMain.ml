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
open CommonTypes
open CommonNetwork
open CommonOptions
open CommonServer

open DcTypes
open DcGlobals
open DcOptions  
open DcServers

let log_prefix = "[dcM]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let do_once_after_start () =
  if not !once_create_filelist then begin
    once_create_filelist := true;
    DcShared.create_filelist ()
  end;
  if not !once_connect_to_servers then begin
    once_connect_to_servers := true;
    DcServers.autoconnect_to_servers ()
  end

let hash_timer () =
  if !dc_config_files_loaded then begin
    DcShared.dc_check_hashed_files ();
  end

let five_sec_timer () =
  if !!autosearch_by_tth then begin
    DcClients.create_autosearch ()
  end

let one_hour_timer () =
  DcShared.create_filelist ()

let half_hour_timer () =
  DcServers.send_myinfo_connected_servers ()

let one_min_timer () =
  DcClients.try_to_resume_files ()

let five_min_timer () =
  DcGlobals.check_all_passive_users ();
  DcServers.autoconnect_to_servers ()
  
let is_enabled = ref false
  
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      Hashtbl2.safe_iter (fun s -> disconnect_server s Closed_by_user) servers_by_ip;
      List.iter (fun c ->
        match c.client_sock with
        | Connection sock -> TcpBufferedSocket.close sock Closed_by_user
        | _ -> () 
      )  !clients_list;
      (match !dc_tcp_listen_sock with
        | None -> ()
        | Some sock -> 
            dc_tcp_listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      (match !dc_udp_sock with
        | None -> ()
        | Some sock -> 
            dc_udp_sock := None;
            UdpSocket.close sock Closed_by_user);
      if !!enable_directconnect then enable_directconnect =:= false
    end
    
let enable () =
  if not !is_enabled then
  let enabler = ref true in
  is_enabled := true;
  network.op_network_disable <- disable enabler;
  
  if not !!enable_directconnect then enable_directconnect =:= true;
  
  Unix2.safe_mkdir directconnect_directory;
  Unix2.safe_mkdir filelist_directory;
  Unix2.can_write_to_directory directconnect_directory;
  Unix2.can_write_to_directory filelist_directory;
  
  (match  DcClients.create_udp_socket (); with        (* UDP listening socket *)
  | Some sock -> ()
  | None -> failwith "Could not create udp socket" ); (* TCP listening socket *) 
  (match DcClients.create_tcp_socket () with
  | Some sock -> ()
  | None -> failwith "Could not create tcp socket" );

  add_session_timer enabler 60. one_min_timer;
  add_session_timer enabler 300. five_min_timer;
  add_session_timer enabler 1800. half_hour_timer;
  add_session_timer enabler 3600. one_hour_timer;
  add_session_timer enabler 0.2 hash_timer;
  add_session_timer enabler 5.0 five_sec_timer;


(* list of todos here... *)

  
  add_timer 60.0 (fun timer -> do_once_after_start ());

(*  add_session_timer enabler 300. (fun timer ->
      DcServers.recover_files_clients ()
  );

  add_session_timer enabler 20. (fun timer ->
      DcServers.recover_files_searches ()
  );
*)


    ()

  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_directconnect);
  option_hook enable_directconnect (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_directconnect then network_enable network
      else network_disable network);
  network.network_config_file <- [directconnect_ini];
  network.op_network_enable <- enable;
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
      network_connected_servers = List.length !connected_servers;
      });
  network.op_network_search <- (fun q buf -> (* DC search function *)
    let query = q.search_query in
    let module S = DcProtocol.Search in
    let words = ref [] in
    let filetype = ref 1 in
    let sizelimit = ref NoLimit in
    let rec iter q =
      (match q with
      | QOr (q1,q2)
      | QAnd (q1,q2) -> iter q1; iter q2
      | QAndNot (q1,q2) -> iter q1
      | QHasWord w -> words := w :: !words
      | QHasField (field, w) ->
          (match field with
          | Field_Type ->
           (* 1 for any file type
              2 for audio files ("mp3", "mp2", "wav", "au", "rm", "mid", "sm")
              3 for compressed files ("zip", "arj", "rar", "lzh", "gz", "z", "arc", "pak")
              4 for documents ("doc", "txt", "wri", "pdf", "ps", "tex")
              5 for executables ("pm", "exe", "bat", "com")
              6 for pictures ("gif", "jpg", "jpeg", "bmp", "pcx", "png", "wmf", "psd")
              7 for video ("mpg", "mpeg", "avi", "asf", "mov")
              8 for folders
              9 for TTH  *)
              (match w with
              | "Audio" -> filetype := 2
              | "Video" -> filetype := 7
              | "Doc" -> filetype := 4
              | "TTH" -> filetype := 9
              | _ -> if !verbose_msg_clients then lprintf_nl "Unknown search type [%s]" w )
          | _ -> () (*words := w :: !words*) )
      | QHasMinVal (field, value) ->
          (match field with
          | Field_Size -> sizelimit := AtLeast value
          | _ -> () )
     | QHasMaxVal (field, value) ->
          (match field with
          | Field_Size -> sizelimit := AtMost value
          | _ -> () )
      | QNone ->
          if !verbose_unexpected_messages then
            lprintf_nl "DcInteractive.start_search: QNone in query";
          () )
    in
    iter query;

    let words = String2.unsplit !words ' ' in
    dc_with_connected_servers (fun s -> DcClients.server_send_search s q 1 words);
  );
  network.op_network_connected <- (fun _ ->
    !connected_servers <> []
  );
  network.op_network_parse_url <- (fun url user group -> DcInteractive.parse_url url user group);
  network.op_network_download <- DcInteractive.start_result_download;
  network.op_network_ports <- (fun _ ->
    [
    !!dc_port, "client_port TCP+UDP";
    ]);
  network.op_network_recover_temp <- (fun _ -> ());
  network.op_network_save_sources <- (fun _ -> ());
  network.op_network_update_options <- (fun _ -> ());
  network.op_network_add_server <- (fun addr port ->
      as_server (new_server addr (Ip.ip_of_addr addr) port).server_server (*DNS *)
  );
  network.op_network_connected_servers <- (fun _ ->
    List2.tail_map (fun s -> as_server s.server_server) !connected_servers
  );
  network.op_network_private_message <- (fun _ _ -> ());
  network.op_network_connect_servers <- (fun _ -> ());
  network.op_network_forget_search <- (fun _ -> ());
  network.op_network_close_search <- (fun _ -> ());
  network.op_network_extend_search <- (fun _ _ -> ());
  network.op_network_clean_servers <- (fun _ -> ());
  network.op_network_gui_message <- (fun _ _ -> ());
  network.op_network_display_stats <- (fun _ -> ());
(*  network.op_network_clean_exit <- (fun _ -> lprintf_nl "Received (op_network_clean_exit)"; ()); *)
  network.op_network_reset <- (fun _ -> ());
  network.op_network_porttest_result <- (fun _ -> PorttestNotAvailable);
  network.op_network_check_upload_slots <- (fun _ -> ());

  CommonInteractive.register_gui_options_panel "DC" gui_dc_options_panel; 
  
(*
let _ =
  network.op_network_porttest_result <- (fun _ -> PorttestNotAvailable);
  network.op_network_save_sources <- (fun _ -> ());
      as_server (new_server ip port).server_server
  )
*)

