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

open Printf2
open Md4
open CommonClient
open CommonUser
open CommonTypes
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonFile
open BasicSocket
open CommonGlobals
open Options
open BTTypes
open BTOptions
open CommonSwarming  
open CommonNetwork
  
let network = new_network "BitTorrent"  
    (fun _ -> !!network_options_prefix)
  (fun _ -> !!commit_in_subdir)
(*  network_options_prefix commit_in_subdir *)
  (*
     op_result_network : network;
     op_result_download : ('a -> string list -> unit);
     op_result_info : ('a -> CommonTypes.result_info);
  *)
      
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
  
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network

let (room_ops : server CommonRoom.room_ops) = 
  CommonRoom.new_room_ops network
  
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
  
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network
    
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)
    
module DO = CommonOptions

let current_files = ref ([] : BTTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let files_by_uid = Hashtbl.create 13
      
let new_file file_id file_name file_size file_tracker piece_size = 
  let file_temp = Filename.concat !!DO.temp_directory 
      (Printf.sprintf "BT-%s" (Sha1.to_string file_id)) in
  let t = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
  let swarmer = Int64Swarmer.create () in
  let partition = fixed_partition swarmer piece_size in
  let rec file = {
      file_file = file_impl;
      file_piece_size = piece_size;
      file_id = file_id;
      file_name = file_name;
      file_clients = Hashtbl.create 113;
      file_swarmer = swarmer;
      file_partition = partition;
      file_tracker = file_tracker;
      file_chunks = [||];
      file_tracker_connected = false;
      file_tracker_last_conn = 0;
      file_tracker_interval = 600;
      file_files = [];
      file_blocks_downloaded = [];
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = t;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();          
      impl_file_best_name = file_name;
    }
  in
  Int64Swarmer.set_size swarmer file_size;  
  Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
      if !!CommonOptions.buffer_writes then 
        Unix32.buffered_write t offset s pos len
      else
        Unix32.write  t offset s pos len
  );
  Int64Swarmer.set_verifier partition (fun b ->
      if file.file_chunks = [||] then raise Not_found;
      let num, begin_pos, end_pos = Int64Swarmer.block_block b in
      let sha1 = Sha1.digest_subfile (file_fd file) 
        begin_pos (end_pos -- begin_pos) in
      let result = sha1 = file.file_chunks.(num) in
      lprintf "Sha1 computed: %s against %s = %s\n"
        (Sha1.to_string sha1) (Sha1.to_string file.file_chunks.(num))
      (if result then "VERIFIED" else "CORRUPTED");
      if result then begin
          file.file_blocks_downloaded <- b :: file.file_blocks_downloaded;
          file_must_update (as_file file.file_file)
        end;
      result
  );
  current_files := file :: !current_files;
  Hashtbl.add files_by_uid file_id file;
  file_add file_impl FileDownloading;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file
  
let new_file file_id file_name file_size file_tracker piece_size =
  try
    Hashtbl.find files_by_uid file_id
  with Not_found -> 
      new_file file_id file_name file_size file_tracker piece_size
  
let new_client file peer_id kind =
  try
    let c = Hashtbl.find file.file_clients peer_id in
    c.client_host <- kind;
    c
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = None;
          client_upload_requests = [];
          client_connection_control = new_connection_control (());
          client_file = file;
          client_host = kind;
          client_chocked = true;
          client_interested = false;
          client_blocks = [];
          client_chunks = [];
          client_ranges = [];
          client_block = None;
          client_uid = peer_id;
          client_bitmap = 
          String.make (Int64Swarmer.partition_size file.file_partition) '\000';
          client_allowed_to_write = zero;
          client_uploaded = zero;
          client_downloaded = zero;
          client_blocks_sent = [];
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
        } in
      new_client impl;
      Hashtbl.add file.file_clients peer_id c;
      c
  
let file_state file =
  file_state (as_file file.file_file)
  
let file_num file =
  file_num (as_file file.file_file)
  
let file_must_update file =
  file_must_update (as_file file.file_file)

let client_type c = client_type (as_client c.client_client)

let set_client_state client state =
  CommonClient.set_client_state (as_client client.client_client) state
  
let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client.client_client) 
  
  
let remove_file file = 
  Hashtbl.remove files_by_uid file.file_id;
  current_files := List2.removeq file !current_files
  
  
let as_client c = as_client c.client_client
let client_num c = client_num (as_client c)
