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

open CommonOptions
open CommonFile
open CommonComplexOptions
open BasicSocket
open Options
open LimewireComplexOptions
open LimewireOptions
open LimewireGlobals
open LimewireTypes
open CommonTypes

  
let enable () =
  
  if not !!enable_limewire then enable_limewire =:= true;
  List.iter (fun s ->
      try
        let ip = Ip.from_name s in
        redirectors_ips := ip :: !redirectors_ips
      with _ -> ()
  ) !!redirectors;

  (*
  Hashtbl.iter (fun _ file ->
      if file_state file <> FileDownloaded then
        current_files := file :: !current_files
  ) files_by_key;
*)
  
  add_timer 1.0 (fun timer ->
      reactivate_timer timer;
      LimewireServers.connect_servers ());

  add_timer 60.0 (fun timer ->
      reactivate_timer timer;
      LimewireServers.ask_for_files ();
      LimewireServers.send_pings ());
  
  add_timer 300.0 (fun timer ->
      reactivate_timer timer;
      LimewireServers.recover_files ());
  
  LimewireClients.listen ();
  ()
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_limewire);
  network.op_network_save_simple_options <- LimewireComplexOptions.save_config;
  network.op_network_load_simple_options <- 
    (fun _ -> 
      try
        Options.load limewire_ini;
        List.iter (fun s -> Fifo.put ultrapeers_queue s) !!ultrapeers      
      with Sys_error _ ->
          LimewireComplexOptions.save_config ()
      );
  network.op_network_enable <- enable;
  network.op_network_prefixed_args <- (fun _ ->
      prefixed_args "limewire" LimewireOptions.limewire_ini  
  )
  
let main (toto: int) = ()
  