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

open Int64ops
open Printf2
open Md4
open Options
  
open CommonGlobals
open CommonDownloads
open CommonTypes
open CommonFile

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

let value_to_file file_size file_state assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_trackers = 
    try
      get_value "file_trackers" (value_to_list value_to_string)
    with _ -> 
        try
          [get_value "file_tracker"  value_to_string]
        with _ -> failwith "Bad file_tracker"
  in
  
  let file_id, torrent, torrent_diskname =
    try
      let torrent_diskname = get_value "file_torrent_name" value_to_string in
      let s = File.to_string torrent_diskname in
      let file_id, torrent = BTTorrent.decode_torrent s in
      file_id, torrent, torrent_diskname
    with _ ->
        
        let file_name = get_value "file_name" value_to_string in
        let file_id = 
          try
            Sha1.of_string (get_value "file_id" value_to_string)
          with _ -> failwith "Bad file_id"
        in
        let file_piece_size = try
            value_to_int64 (List.assoc "file_piece_size" assocs) 
          with _ -> failwith "Bad file size"
        in
        let file_chunks =  
          get_value "file_hashes" (value_to_array
              (from_value Sha1.option))
        in
        let file_size = get_value "file_size" value_to_int64 in
        let file_files =  
          try
            let file_files = (get_value "file_files" 
                  (value_to_list (fun v ->
                      match v with
                        SmallList [name; p1]
                      | List [name; p1] ->
                          value_to_string name, value_to_int64 p1
                      | _ -> assert false
                  ))) in
            file_files
          with _ -> []
        in
        let torrent = {
            torrent_name = file_name;
            torrent_pieces = file_chunks;
            torrent_piece_size = file_piece_size;
            torrent_files = file_files;
            torrent_length = file_size;
            torrent_announce = 
            (
              try
                (List.hd file_trackers)
              with _ -> ""
            );
          } in
        let torrent_diskname = Filename.concat downloads_directory 
            (file_name ^ ".torrent") in
        file_id, torrent, torrent_diskname
  
  in
  let file_temp = try
      get_value "file_temp" value_to_string
    with Not_found ->
        let file_temp = Filename.concat !!DO.temp_directory 
            (Printf.sprintf "BT-%s" (Sha1.to_string file_id)) in
        file_temp        
  in
  let file = new_file file_id torrent torrent_diskname file_temp file_state in
    
  let file_uploaded = try
      value_to_int64 (List.assoc "file_uploaded" assocs) 
    with _ -> zero
  in
  file.file_uploaded <- file_uploaded;
  
  (match file.file_swarmer with
      None -> ()
    | Some swarmer ->
        Int64Swarmer.value_to_swarmer swarmer assocs;
  );

  (try
      ignore
        (get_value  "file_sources" (
          value_to_list (ClientOption.of_value file)))
    with e -> 
        lprintf "Exception %s while loading sources\n"
          (Printexc2.to_string e); 
  );

  as_file file
  
let file_to_value file =
  try
    let sources = Hashtbl2.to_list file.file_clients in
    let assocs =
      [
        "file_temp", string_to_value (Unix32.filename (file_fd file));
        "file_piece_size", int64_to_value (file.file_piece_size);
        "file_name", string_to_value file.file_name;
        "file_uploaded", int64_to_value  (file.file_uploaded);
        "file_id", string_to_value (Sha1.to_string file.file_id);
        "file_trackers", (list_to_value string_to_value) 
        (List.map (fun t -> t.tracker_url) file.file_trackers);
(* OK, but I still don't like the idea of forgetting all the clients.
We should have a better strategy, ie rating the clients and connecting
to them depending on the results of our last connections. And then,
if we could not download enough in the last interval, ask the tracker to
send us more clients.
  
    "file_sources", 
    list_to_value "BT Sources" (fun c ->
        ClientOption.to_value c) sources
;
  *)
      ]
    in
    let assocs =
      ("file_torrent_name", string_to_value file.file_torrent_diskname) ::
      ("file_hashes", array_to_value 
          (to_value Sha1.option) file.file_chunks) ::
      ("file_files", list_to_value
          (fun (name, p1) ->
            SmallList [string_to_value name; int64_to_value p1])
        file.file_files) ::
      assocs
    in
    match file.file_swarmer with
      None -> assocs 
    | Some swarmer ->
        Int64Swarmer.swarmer_to_value swarmer assocs
  with
    e ->
      lprintf "BTComplexOptions: exception %s in file_to_value\n"
        (Printexc2.to_string e); raise e
      
let old_files = 
  define_option bittorrent_section ["old_files"]
    "" (list_option (tuple2_option (string_option, int64_option))) []
    
    
let save_config () =
  Options.save_with_help bittorrent_ini

let _ =
  network.op_network_file_of_option <- value_to_file;
  file_ops.op_file_to_option <- file_to_value;
  (* Shut up message "Network.save/load_complex_options not implemented by BitTorrent" *)
  network.op_network_load_complex_options <- (fun _ -> ());
  network.op_network_save_complex_options <- (fun _ -> ());
