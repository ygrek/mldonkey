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
open CommonServer
open CommonTypes
open Options
open BasicSocket
open DcTypes
open DcGlobals
open DcOptions  

  
let enable () =
  
  if not !!enable_directconnect then enable_directconnect =:= true;
  
  add_timer 1.0 (fun timer ->
      reactivate_timer timer;
      DcServers.connect_servers ());

  DcClients.listen ();
  if !!load_hublist then  DcServers.load_servers_list "";
(*  network.command_vm <- DcInteractive.print_connected_servers *)
    ()

  
let _ =
  network.op_network_save_simple_options <- DcComplexOptions.save_config;
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_directconnect);
  network.op_network_load_simple_options <- (fun _ -> 
      try Options.load directconnect_ini
        with Sys_error _ ->
          DcComplexOptions.save_config ()
  );
  network.op_network_prefixed_args <- (fun _ -> []);
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server) !connected_servers   
  );
  network.op_network_enable <- enable
  

  