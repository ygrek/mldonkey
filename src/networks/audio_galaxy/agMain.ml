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
open CommonNetwork
open CommonComplexOptions
open CommonFile
open TcpBufferedSocket
open Options
open BasicSocket
open AgTypes
open AgGlobals
open AgOptions

let is_enabled = ref false
  
let disable enabler () =
  if !enabler then begin
      is_enabled := false;
      if !!enable_audiogalaxy then enable_audiogalaxy =:= false;
      enabler := false
    end
    
let enable  () =
  if not !is_enabled then
  let enabler = ref true in
  is_enabled := true;
  network.op_network_disable <- disable enabler;
  if not !!enable_audiogalaxy then enable_audiogalaxy =:= true;
  
  Hashtbl.iter (fun _ file ->
      if file_state   file <> FileDownloaded then
        current_files := file :: !current_files
  ) files_by_key;

  
  (try
      redirection_server_ip =:= Ip.from_name !!redirection_server
    with e ->
        lprintf "Could not get IP of %s (%s). Using last one." 
        !!redirection_server
          (Printexc2.to_string e);
        lprint_newline ());
  
  lprintf "redirection_server_ip for %s: %s" 
  !!redirection_server (Ip.to_string !!redirection_server_ip);
  lprint_newline ();
  (try
      gold_redirection_server_ip =:= Ip.from_name !!gold_redirection_server
    with e ->
        lprintf "Could not get IP of %s (%s). Using last one." 
        !!gold_redirection_server
          (Printexc2.to_string e);
        lprint_newline ());
  
  AgServers.connect_server ();
  add_timer (*enabler*) 5.0 (fun timer ->
      reactivate_timer timer;
      AgServers.connect_server ());
  

  AgHttpForward.start ();
(* Faire un relais d'un port particulier vers http://www.audiogalaxy.com,
pour faire croire a audiogalaxy que tout vient d'mldonkey ... 
Relais bete: on forward simplement la requete sans la modifier !
  *)
  ()

  
let _ =
  network.op_network_is_enabled <- (
    fun _ -> !!CommonOptions.enable_audiogalaxy);
  option_hook enable_audiogalaxy (fun _ ->
      if !!enable_audiogalaxy then network_enable network
      else network_disable network);
      
  (*
  network.op_network_save_simple_options <- AgOptions.save_config;
  network.op_network_load_simple_options <- 
    (fun _ -> 
      try
        Options.load audiogal_ini;      
      with Sys_error _ ->
          AgOptions.save_config ()
);
  *)
  network.op_network_enable <- enable;
  network.network_config_file <- Some audiogal_ini;
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
  ;
  
  CommonNetwork.register_escape_char 'G' (fun _ ->
      Printf.sprintf "<td><a href=\"http://%s:%d/\" $O> Audio Gallaxy </a>" 
        (Ip.to_string (if !!CommonOptions.http_bind_addr = Ip.any then
            client_ip None
          else
            !!http_bind_addr)) !!AgOptions.http_port)
  
let main (toto: int) = ()
  
