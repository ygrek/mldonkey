(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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
open BTClients
open CommonOptions
open BasicSocket
open Options
open BTComplexOptions
open BTOptions
open BTGlobals
open BTTypes
open CommonTypes

let log_prefix = "[bTM]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let is_enabled = ref false

let stop_dht () =
  match !bt_dht with
  | None -> ()
  | Some dht ->
    if !verbose then lprintf_nl "stopping DHT";
    BT_DHT.stop dht;
    bt_dht := None

let start_dht () =
  let already = match !bt_dht with Some dht -> dht.BT_DHT.M.dht_port = !!dht_port | None -> false in
  if not already && !!dht_port > 0 then
  begin
    stop_dht ();
    lprintf_nl "starting DHT on port %d" !!dht_port;
    let dht = BT_DHT.start !!dht_routing_table !!dht_port CommonGlobals.udp_write_controler in
    BT_DHT.bootstrap dht ~routers:!!dht_bootstrap_nodes;
    bt_dht := Some dht
  end

let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      List.iter (fun file ->
          Hashtbl2.safe_iter (fun c -> disconnect_client c Closed_by_user)
          file.file_clients) !current_files;
      (match !listen_sock with None -> ()
        | Some sock ->
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      BTTracker.stop_tracker ();
      stop_dht ();
      if !!enable_bittorrent then enable_bittorrent =:= false
    end

let enable () =
  if not !is_enabled then
    let enabler = ref true in
    List.iter (fun dir ->
      Unix2.safe_mkdir dir;
      Unix2.can_write_to_directory dir;
    )  [torrents_directory; seeded_directory;
        tracked_directory; downloads_directory;
        old_directory; new_torrents_directory];
    is_enabled := true;
    if !!BTTracker.tracker_port = !!client_port then
      begin
        lprint_newline();
        lprintf_nl "BT-client_port and BT-tracker_port can not be the same.";
        lprintf_nl "Change one of the settings and restart MLDonkey, exiting...\n";
        Pervasives.exit 69
      end;
    if !!BTTracker.tracker_port > 0 then (
        try BTTracker.start_tracker !!tracked_files_list
        with e ->
            lprintf "Exception in BTTracker.start_tracker: %s\n"
              (Printexc2.to_string e));
    start_dht ();
    if !!share_scan_interval <> 0 then
    add_session_timer enabler (float_of_int (!!share_scan_interval * 60))
      (fun _ -> BTInteractive.share_files ();
    );
    if !!import_new_torrents_interval <> 0. then
    add_session_timer enabler !!import_new_torrents_interval (fun _ ->
        BTInteractive.scan_new_torrents_directory ();
    );
    add_session_timer enabler 60. (fun _ -> BTInteractive.announce_shared_files (); );
    add_timer 5. (fun _ -> BTInteractive.scan_new_torrents_directory ());
    add_timer 10. BTInteractive.share_files;
    add_session_timer enabler 600. BTInteractive.retry_all_ft;
    network.op_network_disable <- disable enabler;

    if not !!enable_bittorrent then enable_bittorrent =:= true;

    BTClients.recover_files ();
    add_session_timer enabler 60.0 (fun timer ->
        BTClients.recover_files ();
    );

    add_session_timer enabler 120.0 (fun timer ->
        BTClients.send_pings ();
    );

  add_session_timer enabler 10.0 (fun timer ->
      BTClients.recompute_uploaders());

  CommonGlobals.do_at_exit ( fun _ ->
    List.iter (fun file ->
                 BTClients.file_stop file
              ) !current_files;
  );

  BTClients.listen ();
  ()

let _ =
  CommonOptions.verbose_dht := Kademlia.verb;
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_bittorrent);
  option_hook enable_bittorrent (fun _ ->
      if !CommonOptions.start_running_plugins then
      if !!enable_bittorrent then network_enable network
      else network_disable network);
  option_hook dht_port (fun _ ->
    if !is_enabled then
    begin
      if !!dht_port = 0 then stop_dht () else start_dht ()
    end);
(*
  network.op_network_save_simple_options <- BTComplexOptions.save_config;
  network.op_network_load_simple_options <-
    (fun _ ->
      try
        Options.load bittorrent_ini;
      with Sys_error _ ->
          BTComplexOptions.save_config ()
);
  *)
  network.op_network_enable <- enable;
  network.network_config_file <- [bittorrent_ini];
  check_client_uid ();
  network.op_network_info <- (fun n ->
      {
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            [] -> "" | opfile :: _ -> options_file_name opfile);
        network_netflags = network.network_flags;
        network_netname = network.network_name;
        network_enabled = network_is_enabled network;
        network_uploaded = !bt_upload_counter;
        network_downloaded = !bt_download_counter;
        network_connected_servers = 0;
      });
  CommonInteractive.register_gui_options_panel "BitTorrent"
  gui_bittorrent_options_panel

let main (toto: int) = ()
