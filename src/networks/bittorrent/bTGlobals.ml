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
open BasicSocket
open Options

open CommonClient
open CommonUser
open CommonTypes
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonFile
open CommonSwarming  
open CommonNetwork
open CommonGlobals
      
open MultinetTypes
open MultinetFunctions
open MultinetComplexOptions

open BTTypes
open BTOptions
open BTProtocol
  
    (*
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)
    
let file_state file =
  file_state (as_file file.file_file)
  
let file_num file =
  file_num (as_file file.file_file)
  
  *)

  
let network = new_network "BitTorrent"  
  [ 
    NetworkHasMultinet; 
    NetworkHasUpload;
  ]

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
  
let (network_file_ops: file_network) = new_file_network "BitTorrent"
let (file_ops: file network_file) = new_network_file network_file_ops
    
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network
    
module DO = CommonOptions

let current_files = ref ([] : BTTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)

let infos_by_uid = Hashtbl.create 13
  
let files_by_uid = Hashtbl.create 13

let new_file file_shared file_id =
  
  let file_info = Hashtbl.find infos_by_uid file_id in
  let partition = fixed_partition file_shared.file_swarmer network.network_num
      file_info.file_info_piece_size in
  let rec file = {
      file_shared = file_shared;
      file_info = file_info;
      file_clients = Hashtbl.create 113;
      file_partition = partition;
      file_tracker_connected = false;
      file_tracker_last_conn = 0;
      file_tracker_interval = 600;
      file_blocks_downloaded = [];
    }
  in
(*  file_shared.file_verified_partition <- Some partition; *)
  Int64Swarmer.set_verifier partition (fun b ->
      if file_info.file_info_chunks <> [||] then 
        let num, begin_pos, end_pos = Int64Swarmer.block_block b in
        lprintf "Sha1 to compute: %d %Ld-%Ld\n" num begin_pos end_pos;
        Unix32.flush_fd (file_fd file_shared);
        let sha1 = Sha1.digest_subfile (file_fd file_shared) 
          begin_pos (end_pos -- begin_pos) in
        let result = sha1 = file_info.file_info_chunks.(num) in
        lprintf "Sha1 computed: %s against %s = %s\n"
          (Sha1.to_string sha1) 
        (Sha1.to_string file_info.file_info_chunks.(num))
        (if result then "VERIFIED" else "CORRUPTED");
        if result then 
          Int64Swarmer.loaded_block b
        else 
          Int64Swarmer.reload_block b;
        
        if result then begin
            file.file_blocks_downloaded <- b :: file.file_blocks_downloaded;
            file_must_update file_shared;
(*Automatically send Have to ALL clients once a piece is verified
             NB : will probably have to check if client can be interested*)
            Hashtbl.iter (fun _ c ->
                match c.client_sock with
                | Connection sock -> 
                    send_client c (Have (Int64.of_int num));
                | _ -> ()
            ) file.file_clients
          end;
  );
  file_shared.file_files <- file_info.file_info_files;
  current_files := file :: !current_files;
  Hashtbl.add files_by_uid file_id file;
  file
  
let _ = 
  register_network network_file_ops;
  network_file_ops.op_download_start <- 
    (fun file_shared ->      
      List.iter (fun uid ->          
          match uid with
          | BTUrl (_, file_id) ->
              if not (Hashtbl.mem files_by_uid file_id) then
                ignore (new_file file_shared file_id)
              
          | _ ->()
      ) file_shared.file_uids
  ) 
  
let new_client file peer_id kind =
  try
    let c = Hashtbl.find file.file_clients peer_id in
    c.client_host <- kind;
    c
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
          client_upload_requests = [];
          client_connection_control = new_connection_control (());
          client_file = file;
          client_host = kind;
          client_choked = true;
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
	  client_downloaded_rate = zero;
          client_optimist_time=0;
          client_blocks_sent = [];
          client_new_chunks = [];
          client_good = false;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
	  impl_client_upload = None;
        } in
      c.client_connection_control.control_min_reask <- 120;
      new_client impl;
      Hashtbl.add file.file_clients peer_id c;
      file_add_source file.file_shared (as_client c);
      c
  
let remove_file file = 
  Hashtbl.remove files_by_uid file.file_info.file_info_id;
  current_files := List2.removeq file !current_files
