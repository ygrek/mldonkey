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
open FileTPClients
open CommonOptions
open BasicSocket
open Options
open FileTPOptions
open FileTPGlobals
open CommonTypes

let is_enabled = ref false

let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      Hashtbl2.safe_iter (fun c -> disconnect_client c Closed_by_user)
      clients_by_uid;
      (match !listen_sock with None -> ()
        | Some sock ->
            listen_sock := None;
            TcpServerSocket.close sock Closed_by_user);
      if !!enable_fileTP then enable_fileTP =:= false;
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

    if not !!enable_fileTP then enable_fileTP =:= true;

    add_session_timer enabler 1.0 (fun timer ->
        FileTPClients.manage_hosts ();
    );

    add_timer 10. (fun _ -> FileTPClients.ask_for_files ());
    add_session_timer enabler 60.0 (fun timer ->
        FileTPClients.ask_for_files ();
    );
  ()

let _ =

  Hashtbl.add protos_by_name "http" FileTPHTTP.proto;
  Hashtbl.add protos_by_name "ftp" FileTPFTP.proto;
  Hashtbl.add protos_by_name "ssh" FileTPSSH.proto;

  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_fileTP);
  option_hook enable_fileTP (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_fileTP then network_enable network
      else network_disable network);
  network.op_network_save_complex_options <- FileTPComplexOptions.save_config;
  network.op_network_enable <- enable;
  network.network_config_file <- [fileTP_ini];
  network.op_network_info <- (fun n ->
      {
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            [] -> "" | opfile :: _ -> options_file_name opfile);
        network_netflags = network.network_flags;
        network_netname = network.network_name;
        network_enabled = network.op_network_is_enabled ();
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
        network_connected_servers = 0;
      });
  CommonInteractive.register_gui_options_panel "FileTP"
  gui_fileTP_options_panel

let main (toto: int) = ()
