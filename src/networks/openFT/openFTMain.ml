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

open OpenFTClients
open CommonOptions
open CommonFile
open CommonComplexOptions
open BasicSocket
open Options
open OpenFTComplexOptions
open OpenFTOptions
open OpenFTGlobals
open OpenFTTypes
open CommonTypes
open OpenFTServers

let is_enabled = ref false
    
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      enabler := false;
      Hashtbl2.safe_iter (fun s -> disconnect_from_server s) servers_by_key;
      Hashtbl2.safe_iter (fun c -> disconnect_client c) clients_by_num;
      (match !listen_sock with None -> ()
        | Some sock -> 
            listen_sock := None;
            TcpServerSocket.close sock "");
      if !!enable_openft then enable_openft =:= false
    end
    
let enable () =
  if not !is_enabled then
    let enabler = ref true in
    is_enabled := true;
    network.op_network_disable <- disable enabler;
    
    if not !!enable_openft then enable_openft =:= true;
    
  (*
  Hashtbl.iter (fun _ file ->
      if file_state file <> FileDownloaded then
        current_files := file :: !current_files
  ) files_by_key;
*)
  List.iter (fun s -> Fifo.put ultrapeers_queue s) !!ultrapeers;
  
  add_session_timer enabler 1.0 (fun timer ->
      OpenFTServers.connect_servers ());

  add_session_timer enabler 60.0 (fun timer ->
      OpenFTServers.ask_for_files ();
      OpenFTServers.send_pings ());
  
  add_session_timer enabler 300.0 (fun timer ->
      OpenFTServers.recover_files ());
  
  OpenFTClients.listen ();
  ()
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_openft);
  option_hook enable_ (fun _ ->
      if !!enable_ then network_enable network
      else network_disable network);
(*
  network.op_network_save_simple_options <- OpenFTComplexOptions.save_config;
  network.op_network_load_simple_options <- 
    (fun _ -> 
      try
        Options.load openft_ini;
      with Sys_error _ ->
          OpenFTComplexOptions.save_config ()
);
  *)
  network.op_network_enable <- enable;
  network.network_config_file <- Some openft_ini;
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
  
  
let main (toto: int) = ()
  