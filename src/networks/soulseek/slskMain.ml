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

open CommonInteractive
open CommonGlobals
open CommonOptions
open CommonServer
open CommonTypes
open Options
open BasicSocket
open SlskTypes
open SlskGlobals
open SlskOptions  


let disable enabler () =
  enabler := false;
  Hashtbl2.safe_iter (fun s -> SlskServers.disconnect_server s)
  servers_by_addr;
(*  List.iter (fun file -> ()) !current_files; *)
  if !!enable_soulseek then enable_soulseek =:= false

    
let enable () =

  let enabler = ref true in
  network.op_network_disable <- disable enabler;

(*  load_url slsk_kind "http://www.slsk.org/slskinfo2"; *)

  (*
  let main_server = new_server (new_addr_name !!main_server_name)
    !!main_server_port in
*)  
  if not !!enable_soulseek then enable_soulseek =:= true;

  List.iter (fun (server_name, server_port) ->
      ignore (new_server (new_addr_name server_name) server_port)
  ) !!SlskComplexOptions.servers;
  
  add_session_timer enabler 5.0 (fun timer ->
      SlskServers.connect_servers ());

  add_session_timer enabler 300. (fun timer ->
      SlskServers.recover_files ()
  );

  add_session_timer enabler 60. (fun timer ->
      SlskServers.ask_for_files ()
  );
  
  SlskClients.listen ();
(*  SlskServers.connect_server main_server; *)
(*  network.command_vm <- SlskInteractive.print_connected_servers *)
    ()
  
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !! CommonOptions.enable_soulseek);
  network.op_network_save_complex_options <- SlskComplexOptions.save_config;
(*
  network.op_network_load_simple_options <- (fun _ -> 
      try Options.load soulseek_ini
        with Sys_error _ ->
          SlskComplexOptions.save_config ()
);
  *)
  network.network_config_file <- Some soulseek_ini;
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server) !connected_servers   
  );
  network.op_network_enable <- enable;
  network.op_network_info <- (fun n ->
      { 
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            None -> "" | Some opfile -> options_file_name opfile);
        network_netname = network.network_name;
        network_enabled = network.op_network_is_enabled ();
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
      });
  CommonInteractive.register_gui_options_panel "Soulseek" gui_soulseek_options_panel;
  
  

  (*
Download the server list from:
  
http://www.soulseek.org/slskinfo
*)