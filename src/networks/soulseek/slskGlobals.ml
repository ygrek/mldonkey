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

open CommonInteractive
open Printf2
open Md4
open Int64ops
open CommonOptions
open CommonResult
open BasicSocket
open CommonGlobals
open CommonTypes
open CommonClient
open CommonComplexOptions
open GuiProto
open Options
open CommonFile
open CommonUser
open CommonRoom
open CommonTypes
open CommonShared
open CommonServer
open CommonSwarming
open SlskOptions
open SlskTypes

    
open CommonNetwork
  
let network = new_network "SLSK" "Soulseek"  
    [ 
    NetworkHasServers; 
    NetworkHasSearch;
    NetworkHasRooms;
    NetworkHasChat;
  ]
  
let connection_manager = network.network_connection_manager      
  
  (*
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
    *)

let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network

let (room_ops : room CommonRoom.room_ops) = 
  CommonRoom.new_room_ops network
  
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
  
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

  

let local_login () = 
  if !!login = "" then !!CommonOptions.global_login else !!login
  
let set_server_state s state =
  set_server_state (as_server s.server_server) state
let set_client_state s state =
  set_client_state (as_client s.client_client) state
let set_room_state s state =
  set_room_state (as_room s.room_room) state
let server_num s = server_num (as_server s.server_server)
let file_num s = file_num (as_file s.file_file)
let server_state s = server_state (as_server s.server_server)
let file_state s = file_state (as_file s.file_file)
let server_must_update s = server_must_update (as_server s.server_server)
let file_must_update s = file_must_update (as_file s.file_file)
let user_num  u = user_num  (as_user u.user_user)
  
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file_fd (as_file file.file_file)
  
let client_type c =
  client_type (as_client c.client_client)

let nknown_servers = ref 0
let connected_servers = ref ([] : server list)

let servers_by_addr = Hashtbl.create 13
  
let new_server addr port =
  try
    Hashtbl.find servers_by_addr (addr, port) 
  with _ ->
      incr nknown_servers;
      let rec h = { 
          server_server = server_impl;
          server_name = "<unknown>";
          server_addr = addr;
          server_nusers = Int64.zero;
          server_info = "";
          server_connection_control = new_connection_control ();
          server_sock = NoConnection;
          server_port = port;
          server_nick = 0;
          server_last_nick = "";
          server_search = None;
          server_search_timeout = 0.0;
          server_users = [];
        } and 
        server_impl = {
          dummy_server_impl with
          impl_server_val = h;
          impl_server_ops = server_ops;
        }       in
      server_add server_impl;
      Hashtbl.add servers_by_addr (addr, port) h;
      h

let searches = ref ([] :  (int * CommonTypes.search) list)

let clients_by_name = Hashtbl.create 113

let users_by_name = Hashtbl.create 113

let results_by_file = Hashtbl.create 111

let new_user name =
  try
    Hashtbl.find users_by_name name
  with _ ->
      let rec user = {
          user_nick = name;
          user_user = user_impl;
          user_rooms = [];
        } and user_impl = {
          dummy_user_impl with
          impl_user_ops = user_ops;
          impl_user_val = user;
        }
      in
      Hashtbl.add users_by_name name user;
      user_add user_impl;
      user      
      
let new_client name =
  try
    Hashtbl.find clients_by_name name 
  with _ ->
      let u = new_user name in
      let rec c = {
          client_client = impl;
          client_peer_sock = NoConnection;
          client_downloads = [];
          client_result_socks = [];
          client_name = name;
          client_addr = None;
          client_files = [];
          client_all_files = None;
          client_receiving = Int64.zero;
          client_connection_control = new_connection_control ();
          client_user = u;
          client_requests = [];
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_upload = None;
        } in
      new_client impl;
      Hashtbl.add clients_by_name name c;
      c

  
let result_sources = Hashtbl.create 1000
  
let add_result_source r (s : user) (index : string) =
  let ss = 
    try
      Hashtbl.find result_sources r.stored_result_num
    with _ ->
        let ss = ref [] in
        Hashtbl.add result_sources r.stored_result_num ss;
        ss
  in
  let key = (s, index) in
  if not (List.mem key !ss) then begin
      ss := key :: !ss
    end
      
let new_result filename filesize =
  let basename = Filename2.basename filename in
  let key = (basename, filesize) in  
  try
    Hashtbl.find results_by_file key
  with _ ->
      let rec r = {
          dummy_result with
          result_names = [basename];
          result_size = filesize;
        } in
      let rs = update_result_num r in
      Hashtbl.add results_by_file key rs;
      rs
            
let rooms_by_name = Hashtbl.create 13
  
let new_room name =
  try 
    Hashtbl.find rooms_by_name name
  with _ ->
      let rec room = {
          room_room = room_impl;
          room_name = name;
          room_nusers = 0;
          room_users = [];
          room_messages = [];
        } and 
        room_impl = {
          dummy_room_impl with
          impl_room_val = room;
          impl_room_ops = room_ops;
          impl_room_state = RoomPaused;
        }         
      in 
      room_add room_impl;
      Hashtbl.add rooms_by_name name room;
      room
      
      
let files_by_key = Hashtbl.create 47

let current_files = ref []

let min_range_size = megabyte

let new_file file_id name file_size =
  let file_chunk_size =
    max megabyte (
      1L ++ file_size // (max 5L (1L ++ file_size // (megabytes 5)))
    )
  in
  let file_temp = Filename.concat !!temp_directory 
      (Printf.sprintf "SK-%s" (Md4.to_string file_id)) in
  let current_size =
    try
      Unix32.getsize file_temp
    with e -> Int64.zero
  in

  let t = Unix32.create_rw file_temp in
  let rec file = {
      file_file = file_impl;
      file_id = file_id;
      file_clients = [];
      file_swarmer = None;
    } and file_impl =  {
      (dummy_file_impl ()) with
      impl_file_fd = Some t;
      impl_file_size = file_size;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();
      impl_file_best_name = name;
    }
  in
  let state =
    if current_size = file_size then
      FileDownloaded
    else
      begin
        let kernel = CommonSwarming.create_swarmer file_temp file_size in
        let swarmer = CommonSwarming.create kernel (as_file file.file_file) file_chunk_size in
          file.file_swarmer <- Some swarmer;
        CommonSwarming.set_verifier swarmer ForceVerification;
        CommonSwarming.set_verified swarmer (fun _ _ -> file_must_update file);
        current_files := file :: !current_files;
        FileDownloading
      end
  in
  file_add file_impl state;
  file

let new_file file_id name file_size =
  let key = String.lowercase name in
  try
    Hashtbl.find files_by_key key
  with _ ->
      let file = new_file file_id key file_size  in
      Hashtbl.add files_by_key key file;
      file

let find_file file_name file_size =
  Hashtbl.find files_by_key (String.lowercase file_name)

  
      
let add_file_client file user filename = 
  let  c = new_client user.user_nick in
  if not (List.memq c file.file_clients) then begin
      file.file_clients <- c :: file.file_clients;
      c.client_files <- (file, filename) :: c.client_files;
      file_add_source (as_file file.file_file) (as_client c.client_client)
    end;
  c
