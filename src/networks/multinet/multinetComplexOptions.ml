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

open Md4
open Printf2
open Options
open BasicSocket
open TcpBufferedSocket

open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open CommonGlobals
open CommonOptions
open CommonSwarming
  
open MultinetTypes
open MultinetGlobals
open MultinetFunctions

  
  
let value_to_int32pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int64 v1, value_to_int64 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"

let value_to_state v =
  match v with
  | StringValue "Paused" -> FilePaused
  | StringValue "Downloading" -> FileDownloading
  | StringValue "Downloaded" -> FileDownloaded
  | _ -> raise Not_found

let state_to_value s = 
  match s with
  | FilePaused | FileAborted _ -> StringValue "Paused"
  | FileDownloaded -> StringValue "Downloaded"
  | _ -> StringValue "Downloading"

let value_to_file is_done assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let filenames = get_value_nil "file_filenames" 
      (value_to_list value_to_string) 
  in

  let file_id = 
    try
      get_value "file_id" value_to_string
    with _ -> failwith "Bad file_id"
  in
  let file_size = try
      value_to_int64 (List.assoc "file_size" assocs) 
    with _ -> failwith "Bad file size"
  in
  let file_uids = try
      value_to_list (fun v ->
          uid_of_string (value_to_string v)  
      ) (List.assoc "file_hashes" assocs)
    with _ -> 
        lprintf "[ERROR]: Could not load hash for %s\n"
          file_id;
        []
  in
  
  let file = new_download (Some file_id) filenames file_size None file_uids in
  
  (try
      let file_state = get_value "file_state" value_to_state in
      set_file_state file file_state;  
    with _ -> ());
  
  (try
      file.file_file.impl_file_age <- 
        normalize_time (get_value "file_age" value_to_int)
    with _ -> ());
  
  let _ = 
    try
      List.iter (fun v ->
          file.file_files <- v :: file.file_files
      )
      (get_value "file_files" 
          (value_to_list (fun v ->
              match v with
                SmallList [name; p1;p2]
              | List [name; p1;p2] ->
                  value_to_string name, value_to_int64 p1, value_to_int64 p2
              | _ -> assert false
          )))
    with _ -> ()
  in
  (try
      let s = get_value "file_bitzi_ticket"  value_to_string in
      if s <> "" then
        file.file_bitzi_ticket <- Bitzi_ticket s
    with _ -> ());
  
  (try 
      Int64Swarmer.set_present file.file_swarmer 
        (get_value "file_present_chunks" 
          (value_to_list value_to_int32pair));
      add_file_downloaded file.file_file
        (Int64Swarmer.downloaded file.file_swarmer)      
    with _ -> ()                
  );
  (try
      match (List.assoc "file_on_networks" assocs) with
        List l | SmallList l ->
          List.iter (fun v ->
              match v with
                Module assocs ->
                  let netname = value_to_string (List.assoc "network" assocs) in
                  List.iter (fun n ->
                      if n.op_download_netname = netname then
                        n.op_download_of_value file assocs
                  ) !networks
              | _ -> ()
          ) l
      | _ -> ()
    with _ -> ());
  
  as_file file

let file_to_value file =
  [
    "file_size", int64_to_value (file_size file);
    "file_downloaded", int64_to_value (file_downloaded file);
    "file_id", string_to_value file.file_id;
    "file_state", state_to_value (file_state file);
    "file_hashes", list_to_value "Hashes" (fun uid ->
        string_to_value (string_of_uid uid)
    ) file.file_uids;
    "file_filenames", List
      (List.map (fun s -> string_to_value s) file.file_filenames);        
    "file_present_chunks", List
      (List.map (fun (i1,i2) -> 
          SmallList [int64_to_value i1; int64_to_value i2])
      (Int64Swarmer.present_chunks file.file_swarmer));
    "file_files", list_to_value ""
      (fun (name, p1,p2) ->
        SmallList [string_to_value name; int64_to_value p1; int64_to_value p2])
    file.file_files;
    
    "file_on_networks", 
    List (List.map (fun (n,v) ->
          Module (
            ("network", string_to_value n.op_download_network.op_download_netname) ::
            (n.op_download_to_value v)
          )) file.file_impls);
    "file_mtime", float_to_value (
      try Unix32.mtime64 (file_disk_name file) with _ -> 0.0);
    "file_age", IntValue (Int64.of_int (file_age file));
    "file_bitzi_ticket", string_to_value (match file.file_bitzi_ticket with
        Bitzi_ticket s -> s | _ -> "");
  ]
