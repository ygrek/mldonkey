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

open Queues
open Printf2
open Md4
open Options
open BasicSocket
  
open CommonSwarming
open CommonTypes
open CommonFile
open CommonHosts
open CommonDownloads.SharedDownload
  
open GnutellaTypes
open GnutellaOptions
open GnutellaGlobals

let g1_ultrapeers = define_option gnutella_ini
    ["cache"; "gnutella1"; "ultrapeers"]
    "Known ultrapeers" (list_option (tuple2_option (Ip.option, int_option)))
  []

let g1_peers = define_option gnutella_ini
    ["cache"; "gnutella1"; "peers"]
    "Known Peers" (list_option (tuple2_option (Ip.option, int_option)))
  []

module ClientOption = struct
    
    let value_to_client v = 
      match v with
      | Module assocs ->
          
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          let client_ip = get_value "client_ip" (from_value Ip.option)
          in
          let client_port = get_value "client_port" value_to_int in
          let client_uid = get_value "client_uid" (from_value Md4.option) in
          let c = new_client (Known_location(client_ip, client_port)) in
          
          (try
              c.client_user.user_speed <- get_value "client_speed" value_to_int 
            with _ -> ());
          (try
              if get_value "client_push" value_to_bool then
                c.client_user.user_kind <- Indirect_location ("", client_uid)
            with _ -> ());
          c
      | _ -> failwith "Options: Not a client"
    
    
    let client_to_value c =
      let u = c.client_user in
      match c.client_user.user_kind with
        Known_location (ip, port) ->
          Options.Module [
            "client_uid", to_value Md4.option u.user_uid;
            "client_ip", to_value Ip.option ip;
            "client_port", int_to_value port;
            "client_speed", int_to_value u.user_speed;
            "client_push", bool_to_value false;
          ]
      | Indirect_location _ ->
          Options.Module [
            "client_uid", to_value Md4.option u.user_uid;
            "client_ip", to_value Ip.option Ip.null;
            "client_port", int_to_value 0;
            "client_speed", int_to_value u.user_speed;
            "client_push", bool_to_value true;
          ]
    
    let t =
      define_option_class "Client" value_to_client client_to_value
  
  end

let value_to_file file_shared assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in  
  
  let file = new_file file_shared in
  
  (try
      ignore (get_value "file_sources" (value_to_list (fun v ->
              match v with
              | SmallList [c; index; name] | List [c;index; name] ->
                  let s = ClientOption.value_to_client c in
                  add_download file s (
                    FileByIndex (value_to_int index, value_to_string name))
    
              | SmallList [c; index] | List [c;index] ->
                  let s = ClientOption.value_to_client c in
                  add_download file s (
                    FileByUrl (value_to_string index))
              | _ -> failwith "Bad source"
          )))
    with e -> 
        lprintf "Exception %s while loading source\n"
          (Printexc2.to_string e); 
  )
  
let file_to_value file =
  [
    "file_sources", 
    list_to_value "Gnutella Sources" (fun c ->
        match (find_download file c.client_downloads).download_uri with
          FileByIndex (i,n) -> 
            SmallList [ClientOption.client_to_value c; int_to_value i; 
              string_to_value n]
        | FileByUrl s -> 
            SmallList [ClientOption.client_to_value c; 
              string_to_value s]
    ) file.file_clients
    ;
  ]
  
let old_files = 
  define_option gnutella_ini ["old_files"]
    "" (list_option (tuple2_option (string_option, int64_option))) []
    
let exit = Exit
  
let save_config () =
  g1_ultrapeers =:= [];
  g1_peers =:= [];
  
  Queue.iter (fun h -> 
      try
      let o = match h.host_kind with
        | (true) -> g1_ultrapeers
        | _ -> g1_peers
      in
(* Don't save hosts that are older than 1 hour, and not responding *)
      if max h.host_connected h.host_age > last_time () - 3600 then
          o =:= (h.host_addr, h.host_port) :: !!o
      with _ -> ()) 
  H.workflow;
  
  ()

let _ =
  network_file_ops.op_download_of_value <- value_to_file;
  file_ops.op_download_to_value <- file_to_value;
  network.op_network_file_of_option <- 
    CommonDownloads.SharedDownload.value_to_file
