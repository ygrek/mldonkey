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

open CommonInteractive
open Int64ops
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
open BTRate
open BTTypes
open BTOptions
open BTProtocol
open CommonSwarming  
open CommonNetwork

let as_file file = as_file file.file_file        
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file)
let set_file_disk_name file = set_file_disk_name (as_file file)
let file_state file = file_state (as_file file)  
let file_num file = file_num (as_file file)
let file_must_update file = file_must_update (as_file file)

let set_file_state file state = 
 CommonFile.set_file_state (as_file file) state

let as_client c = as_client c.client_client
let client_type c = client_type (as_client c)

let set_client_state client state =
  CommonClient.set_client_state (as_client client) state
  
let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client) 

let client_num c = client_num (as_client c)

  
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
let connection_manager = network.network_connection_manager
    
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
    
module DO = CommonOptions

let current_files = ref ([] : BTTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let files_by_uid = Hashtbl.create 13
  
let max_range_len = Int64.of_int (1 lsl 14)
  
let check_if_interesting file c =
  
  if not c.client_alrd_sent_notinterested then
    let up = match c.client_uploader with
        None -> assert false
      | Some up -> up 
    in
    let swarmer = Int64Swarmer.uploader_swarmer up in
    let must_send = 
(* The client has nothing to propose to us *)
      (not (Int64Swarmer.is_interesting up )) &&
(* All the requested ranges are useless *)
      (List.filter (fun (_,_,r) ->
            let x,y = Int64Swarmer.range_range r in
            x < y) c.client_ranges = []) &&
(* The current block is also useless *)
      (match c.client_block with
          None -> true
        | Some b -> 
            let (block_num,_,_) = Int64Swarmer.block_block b in
            let bitmap = Int64Swarmer.verified_bitmap swarmer in
            bitmap.[block_num] <> '3')
    in
    if must_send then
      begin
        c.client_interesting <- false;
        c.client_alrd_sent_notinterested <- true;
        send_client c NotInterested
      end
      
let new_file file_id file_name file_size file_tracker piece_size file_u file_state = 
(*  let t = Unix32.create_rw file_temp in*)
  let rec file = {
      file_file = file_impl;
      file_piece_size = piece_size;
      file_id = file_id;
      file_name = file_name;
      file_clients_num = 0;
      file_clients = Hashtbl.create 113;
      file_swarmer = None;
      file_tracker = file_tracker;
      file_chunks = [||];
      file_tracker_connected = false;
      file_tracker_last_conn = 0;
      file_tracker_interval = 600;
      file_files = [];
      file_blocks_downloaded = [];
      file_uploaded = Int64.zero;
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = file_u;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();          
      impl_file_best_name = file_name;
    }
  in
  let swarmer = Int64Swarmer.create (as_file file) piece_size 
      (min max_range_len piece_size)  in
  file.file_swarmer <- Some swarmer;
  Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
      if !!CommonOptions.buffer_writes then 
        Unix32.buffered_write_copy file_u offset s pos len
      else
        Unix32.write file_u offset s pos len
  );
  Int64Swarmer.set_verifier swarmer (fun num begin_pos end_pos ->
      if file.file_chunks = [||] then raise Not_found;
      lprintf "Sha1 to compute: %d %Ld-%Ld\n" num begin_pos end_pos;
      Unix32.flush_fd (file_fd file);
      let sha1 = Sha1.digest_subfile (file_fd file) 
        begin_pos (end_pos -- begin_pos) in
      let result = sha1 = file.file_chunks.(num) in
      lprintf "Sha1 computed: %s against %s = %s\n"
        (Sha1.to_string sha1) (Sha1.to_string file.file_chunks.(num))
      (if result then "VERIFIED" else "CORRUPTED");
      if result then begin
          file.file_blocks_downloaded <- (num, begin_pos, end_pos) :: 
          file.file_blocks_downloaded;
          file_must_update file;
(*Automatically send Have to ALL clients once a piece is verified
            NB : will probably have to check if client can be interested*)
          Hashtbl.iter (fun _ c ->
              
              if c.client_registered_bitfield then
                begin
                  match c.client_bitmap with
                    None -> ()
                  | Some bitmap ->
                      if (bitmap.[num] <> '1') then
                        send_client c (Have (Int64.of_int num));
                      check_if_interesting file c                          
              end				
          ) file.file_clients
        end;
      result
  );
  current_files := file :: !current_files;
  Hashtbl.add files_by_uid file_id file;
  file_add file_impl file_state;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file
  
let new_file file_id 
    file_name file_size file_tracker piece_size file_files file_temp 
  file_state =
  try
    Hashtbl.find files_by_uid file_id;
  with Not_found -> 
      let file_u = 
        if file_files <> [] then
          Unix32.create_multifile file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666 file_files
        else
          Unix32.create_rw file_temp 
      in
      new_file file_id file_name file_size file_tracker piece_size file_u
        file_state
      
let new_download file_id 
  file_name file_size file_tracker piece_size file_files =
  let file_temp = Filename.concat !!DO.temp_directory 
      (Printf.sprintf "BT-%s" (Sha1.to_string file_id)) in
  new_file file_id 
    file_name file_size file_tracker piece_size file_files file_temp
      
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
          client_sent_choke = false;
          client_interested = false;
          client_uploader = None;
          client_chunks = [];
          client_ranges = [];
          client_block = None;
          client_uid = peer_id;
          client_bitmap = None;
          client_allowed_to_write = zero;
          client_uploaded = zero;
          client_downloaded = zero;
	  client_upload_rate = Rate.new_rate ();
	  client_downloaded_rate = Rate.new_rate ();
          client_optimist_time=0;
          client_blocks_sent = [];
          client_new_chunks = [];
          client_good = false;
          client_num_try = 0;
          client_alrd_sent_interested = false;
          client_alrd_sent_notinterested = false;
          client_interesting = false;
          client_incoming = false;
	  client_registered_bitfield = false;
	  client_last_optimist = 0;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
	  impl_client_upload = None;
        } in
      c.client_connection_control.control_min_reask <- 120;
      new_client impl;
      Hashtbl.add file.file_clients peer_id c;
      file.file_clients_num <- file.file_clients_num + 1;
      file_add_source (as_file file) (as_client c);
      c
  
let remove_file file = 
  Hashtbl.remove files_by_uid file.file_id;
  current_files := List2.removeq file !current_files

let remove_client c = 
    Hashtbl.remove c.client_file.file_clients c.client_uid ;
    c.client_file.file_clients_num <- c.client_file.file_clients_num  - 1;
    file_remove_source (as_file c.client_file) (as_client c)

let downloads_directory = Filename.concat "torrents" "downloads"
let tracked_directory = Filename.concat "torrents" "tracked"
let seeded_directory = Filename.concat "torrents" "seeded"
