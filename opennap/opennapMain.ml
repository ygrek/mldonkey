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
open CommonTypes
open CommonComplexOptions
open CommonFile
open Options
  open BasicSocket
open OpennapTypes
open OpennapGlobals
open OpennapOptions  

let enable () =

  if not !!enable_opennap then enable_opennap =:= true;
  if !!use_napigator then
    Napigator.load_servers_list "http://www.napigator.com/servers/"
      (fun list -> 
      Printf.printf "LIST OF %d SERVERS DOWNLOADED"
          (List.length list)
        ; print_newline ();
        List.iter 
          (fun (desc, ip, port, network) ->
          let s = new_server ip port in
            s.server_desc <- desc;
            s.server_net <- network) list);

  (*
  Hashtbl.iter (fun _ file ->
      if file_state file <> FileDownloaded then
        current_files := file :: !current_files
  ) files_by_key;
*)
  
  add_timer 1.0 (fun timer ->
      reactivate_timer timer;
      OpennapServers.connect_servers ());

  add_timer 30.0 (fun timer ->
      reactivate_timer timer;
      Printf.printf "SAVE FILES"; print_newline ();
      OpennapComplexOptions.save_config ());
  
  add_timer 60.0 (fun timer ->
      reactivate_timer timer;
      OpennapServers.ask_for_files ();
  );
  
  add_timer 300.0 (fun timer ->
      reactivate_timer timer;
      OpennapServers.recover_files ());

  OpennapClients.listen ();

  ()

    
open CommonTypes
  
let _ =
  network.op_network_is_enabled <- (fun _ -> !!CommonOptions.enable_opennap);
  network.op_network_save_simple_options <- OpennapComplexOptions.save_config;
  network.op_network_load_simple_options <- (fun _ -> 
      try
        Options.load opennap_ini
      with Sys_error _ ->
          OpennapComplexOptions.save_config ()        
  );
  network.op_network_enable <- enable;
  network.op_network_prefixed_args <- (fun _ ->
      prefixed_args "opennap" OpennapOptions.opennap_ini  
  )

  