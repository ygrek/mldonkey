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


open Options
open Printf2
open Md4

open CommonOptions
open CommonTypes
open CommonFile
open CommonSwarming

      
open MultinetTypes
open MultinetFunctions
open MultinetComplexOptions


open BTTypes
open BTOptions
open BTGlobals

module ClientOption = struct
    
    let value_to_client file v = 
      match v with
      | Module assocs ->
          
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          let client_ip = get_value "client_ip" (from_value Ip.option)
          in
          let client_port = get_value "client_port" value_to_int in
          let client_uid = get_value "client_uid" (from_value Sha1.option) in
          let c = new_client file client_uid (client_ip, client_port) in
          
          c
      | _ -> failwith "Options: Not a client"
    
    
    let client_to_value c =
      let (ip,port) = c.client_host in
          Options.Module [
            "client_uid", to_value Sha1.option c.client_uid;
            "client_ip", to_value Ip.option ip;
            "client_port", int_to_value port;
          ]

    let to_value = client_to_value
    let of_value = value_to_client
  
  end

let value_to_int32pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int64 v1, value_to_int64 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"

let value_to_file file_shared assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_id = 
    try
      Sha1.of_string (get_value "file_id" value_to_string)
    with _ -> failwith "Bad file_id"
  in

  let file_piece_size = try
      value_to_int64 (List.assoc "file_piece_size" assocs) 
    with _ -> failwith "Bad file size"
  in
  
  let file_tracker = 
    try
      get_value "file_tracker" value_to_string
    with _ -> failwith "Bad file_tracker"
  in
  let file_chunks = get_value "file_hashes" (value_to_array
        (from_value Sha1.option)) in

  let file_info = {
      file_info_name = file_best_name file_shared;
      file_info_id = file_id;
      file_info_piece_size = file_piece_size;
      file_info_tracker = file_tracker;
      file_info_size = file_size file_shared;
      file_info_chunks = file_chunks;
      file_info_files = file_shared.file_files;
    } in

  Hashtbl.add infos_by_uid file_id file_info;
  
  let file = new_file file_shared file_id in
  
  (try
      Int64Swarmer.set_verified_bitmap file.file_partition
        (get_value  "file_chunks" value_to_string)
    with e -> 
        lprintf "Exception %s while loading bitmap\n"
          (Printexc2.to_string e); 
  );
  
  (try
      ignore
        (get_value  "file_sources" (
          value_to_list (ClientOption.of_value file)))
    with e -> 
        lprintf "Exception %s while loading sources\n"
          (Printexc2.to_string e); 
  )
  
let file_to_value file =
  let sources = Hashtbl2.to_list file.file_clients in
  let info = file.file_info in  
  [
    "file_piece_size", int64_to_value (info.file_info_piece_size);
    "file_id", string_to_value (Sha1.to_string info.file_info_id);
    "file_tracker", string_to_value info.file_info_tracker;
    "file_chunks", string_to_value 
      (Int64Swarmer.verified_bitmap file.file_partition);
    "file_sources", 
    list_to_value "BT Sources" (fun c -> ClientOption.to_value c) sources;
    "file_hashes", array_to_value (to_value Sha1.option) info.file_info_chunks;
  ]


let old_value_to_file is_done assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_name = get_value "file_name" value_to_string in
  let file_id = 
    try
      Sha1.of_string (get_value "file_id" value_to_string)
    with _ -> failwith "Bad file_id"
  in
  let file_size = try
      value_to_int64 (List.assoc "file_size" assocs) 
    with _ -> failwith "Bad file size"
  in
  let file_piece_size = try
      value_to_int64 (List.assoc "file_piece_size" assocs) 
    with _ -> failwith "Bad file size"
  in
  
  let file_tracker = 
    try
      get_value "file_tracker" value_to_string
    with _ -> failwith "Bad file_tracker"
  in
  
  let file_files = 
    try
      (get_value "file_files" 
        (value_to_list (fun v ->
              match v with
                SmallList [name; p1;p2]
              | List [name; p1;p2] ->
                  value_to_string name, value_to_int64 p1, value_to_int64 p2
              | _ -> assert false
          )))
    with _ -> []
  in
  
  let file_chunks = get_value "file_hashes" (value_to_array
        (from_value Sha1.option)) in

  
  let file_info = {
      file_info_name = file_name;
      file_info_id = file_id;
      file_info_piece_size = file_piece_size;
      file_info_tracker = file_tracker;
      file_info_size = file_size;
      file_info_chunks = file_chunks;
      file_info_files = file_files;
    } in

  Hashtbl.add infos_by_uid file_id file_info;
  
  let old_file = Filename.concat !!temp_directory (
      Printf.sprintf "BT-%s" (Sha1.to_string file_id)) in
  
  let file_uid = BTUrl ("",file_id) in
  
  let file_shared = new_download None [file_name] file_size (Some old_file)
    [file_uid] in

  let file = 
    let file = ref None in
    Hashtbl.iter (fun _ f ->
        if f.file_shared == file_shared then 
          file := Some f
    ) files_by_uid;
    match !file with
      None -> raise Not_found
    | Some file -> file
  in

  (try 
      Int64Swarmer.set_present file_shared.file_swarmer 
        (get_value "file_present_chunks" 
          (value_to_list value_to_int32pair));
      lprintf "add_file_downloaded %Ld\n" (Int64Swarmer.downloaded file_shared.file_swarmer);
      add_file_downloaded file_shared.file_file
        (Int64Swarmer.downloaded file_shared.file_swarmer)
    with e ->
        lprintf "Exception %s while set present\n"
          (Printexc2.to_string e); 
    
        );
  
  
  (try
      Int64Swarmer.set_verified_bitmap file.file_partition
        (get_value  "file_chunks" value_to_string)
    with e -> 
        lprintf "Exception %s while loading bitmap\n"
          (Printexc2.to_string e); 
  );
  
  (try
      ignore
        (get_value  "file_sources" (
          value_to_list (ClientOption.of_value file)))
    with e -> 
        lprintf "Exception %s while loading sources\n"
          (Printexc2.to_string e); 
  );
  as_file file_shared

  
let _ =
  network_file_ops.op_download_of_value <- value_to_file;
  file_ops.op_download_to_value <- file_to_value;
  network.op_network_file_of_option <- old_value_to_file; 
  ()

let old_files = 
  define_option bittorrent_section ["old_files"]
    "" (list_option (tuple2_option (string_option, int64_option))) []
    
    
let save_config () =
  Options.save_with_help bittorrent_ini

