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

open CommonOptions
open CommonHosts
open CommonTypes
open CommonFile
open CommonSwarming
  
open GnutellaNetwork
open GnutellaTypes
open GnutellaOptions
open GnutellaGlobals

let ultrapeers = define_option gnutella_section
    ["cache"; "ultrapeers"]
    "Known ultrapeers" (list_option (tuple2_option (Ip.addr_option, int_option)))
  []

let peers = define_option gnutella_section
    ["cache"; "peers"]
    "Known Peers" (list_option (tuple2_option (Ip.addr_option, int_option)))
  []

module ClientOption = struct
    
    let value_to_client v = 
      match v with
      | Module assocs ->
          
          let get_value name conv = conv (List.assoc name assocs) in
          let client_ip = get_value "client_ip" (from_value Ip.option)
          in
          let client_port = get_value "client_port" value_to_int in
          let client_uid = get_value "client_uid" (from_value Md4.option) in
          let c = new_client (Known_location(client_ip, client_port)) in
          c.client_user.user_nick <- (Md4.to_string client_uid);
          
          (try
              c.client_user.user_speed <- get_value "client_speed" value_to_int 
            with _ -> ());
          (try
              if get_value "client_push" value_to_bool then
                c.client_user.user_kind <- Indirect_location ("", client_uid, client_ip, client_port)
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

let value_to_int32pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int64 v1, value_to_int64 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"

let value_to_file file_size file_state user group assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let file_name = get_value "file_name" value_to_string in
  let file_temp = 
    try
      Printf.sprintf "GNUT-%s" (get_value "file_id" value_to_string)
    with _ -> 
        get_value "file_temp" value_to_string
  in
  
  let file_uids = ref [] in
  let uids_option = try
      value_to_list value_to_string (List.assoc "file_uids" assocs) 
    with _ -> []
  in
  List.iter (fun v ->
      file_uids := (Uid.of_string v) :: !file_uids) uids_option;


(* recover the hash stored by versions <= 2.5.28 *)
  (try
      let hash = 
        Uid.create (Md5Ext (
            Md5Ext.of_string (get_value "file_hash" value_to_string)))
      in 
      file_uids := hash :: !file_uids;
    with _ -> ());
  
  let file = new_file file_temp file_name file_size !file_uids user group in
  
  (try
      file.file_ttr <- Some (get_value "file_ttr" (value_to_array 
            (fun v -> TigerTree.of_string (value_to_string v))));      
    with _ -> ());
  
    
  (match file.file_swarmer with
      None -> ()
    | Some swarmer ->
        CommonSwarming.value_to_frontend swarmer assocs;
        CommonSwarming.set_verifier swarmer (
          match file.file_ttr with
            None -> ForceVerification
          | Some ttr ->
              lprintf "[TTR] set_verifier\n";
              Verification (Array.map (fun ttr -> TigerTree ttr) ttr))
  );
  
  (try
      ignore (get_value "file_sources" (value_to_list (fun v ->
              match v with
              | SmallList (c :: tail) | List (c :: tail) ->
                  let s = ClientOption.value_to_client c in
                  add_download file s (GnutellaNetwork.value_to_index tail)
              | _ -> failwith "Bad source"
          )))
    with e -> 
        lprintf "Exception %s while loading source\n"
          (Printexc2.to_string e); 
  );
  as_file file
  
let file_to_value file =
  let assocs =
    [
      "file_name", string_to_value file.file_name;
      "file_temp", string_to_value file.file_temp;
      
      "file_sources", 
      list_to_value (fun c ->
          let d = find_download file c.client_downloads in
          let tail = index_to_value d.download_uri in
          SmallList (ClientOption.client_to_value c :: tail)      
      ) file.file_clients
      ;
      "file_uids", list_to_value (fun uid ->
          string_to_value  (Uid.to_string uid))
      file.file_uids;
(*      "file_present_chunks", List
        (List.map (fun (i1,i2) -> 
            SmallList [int64_to_value i1; int64_to_value i2])
        (CommonSwarming.present_intervals file.file_swarmer));
*)  
    ]
  in
  let assocs =
    match file.file_ttr with
      None -> assocs
    | Some ttr ->
        ("file_ttr", array_to_value TigerTree.hash_to_value ttr) :: assocs
  in
    match file.file_swarmer with
    None -> assocs 
  | Some swarmer ->
      CommonSwarming.frontend_to_value swarmer assocs
      

  (*
let save_config () =
  lprintf "GnutellaComplexOptions: save_config\n";
  ultrapeers =:= [];
  peers =:= [];
  
  let files = !!old_files in
  old_files =:= [];
  List.iter (fun file ->
      if not (List.mem file !!old_files) then 
        old_files =:= file :: !!old_files
  ) files;
  
  ()
  *)

let _ =
  network.op_network_file_of_option <- value_to_file;
  file_ops.op_file_to_option <- file_to_value;
  (* Shut up message "Network.save/load_complex_options not
     implemented by Gnutella" *)
  network.op_network_load_complex_options <- (fun _ -> ());
  network.op_network_save_complex_options <- (fun _ -> ());
  network.op_network_save_sources <- (fun _ -> ());
  network.op_network_update_options <- (fun _ -> ());
  set_after_load_hook gnutella_ini (fun _ ->
      
      List.iter (fun (ip,port) -> 
          ignore (H.new_host ip port Ultrapeer)) !!ultrapeers;
      
      List.iter (fun (ip,port) -> 
          ignore (H.new_host ip port Peer)) !!peers;      
      
      ultrapeers =:= [];
      peers =:= [];
  );
  
  set_before_save_hook gnutella_ini (fun _ ->

      let ultrapeers_list = ref [] in
      let peers_list = ref [] in

      let next_obsolete = last_time () + 3600 in
      Queue.iter (fun (_,h) -> 
          try
            let list = match h.host_kind with 
                Ultrapeer -> ultrapeers_list
              | _ -> peers_list
            in
            if h.host_kind <> IndexServer then
              list := ( (h.host_addr, h.host_port), 
                if h.host_obsolete = 0 then next_obsolete else h.host_obsolete
              ) :: !list
          with _ -> ())
      H.workflow;
      
      let ultrapeers_list = List.sort (fun (_, a1) (_, a2) -> 
            compare a2 a1) !ultrapeers_list in
      let ultrapeers_list, _ = List2.cut !!max_known_ultrapeers ultrapeers_list
      in
      ultrapeers =:= List2.tail_map fst ultrapeers_list;
      
      let peers_list, _ = List2.cut !!max_known_peers ultrapeers_list
      in
      peers =:= List2.tail_map fst peers_list;

      
      List.iter (fun s ->
          let h = s.server_host in
          let o = ultrapeers in
          if h.host_kind <> IndexServer  then
            let key = (h.host_addr, h.host_port) in
            if not (List.mem key !!o) then
              o =:= key :: !!o
      ) !connected_servers
      
  );
  
  set_after_save_hook gnutella_ini (fun _ ->
      ultrapeers =:= [];
      peers =:= [];
  )
  
