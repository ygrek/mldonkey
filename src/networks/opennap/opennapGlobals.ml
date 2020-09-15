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

open Printf2
open Md4

open CommonInteractive
open OpennapOptions
open CommonClient
open CommonUser
open CommonComplexOptions
open CommonTypes
open CommonServer
open CommonResult
open CommonFile
open CommonGlobals
open Options
open BasicSocket
open OpennapTypes

module DG = CommonGlobals
module DO = CommonOptions
  
open CommonNetwork
  
let network = new_network "ON" "OpenNapster"
    [ 
    NetworkHasServers; 
    NetworkHasRooms;
    NetworkHasChat;
    NetworkHasSearch;
  ]
  
  
let connection_manager = network.network_connection_manager
    
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

let current_servers = ref ([] : server list)
let connected_servers = ref ([] : server list)
let servers_list = ref ([] : server list)
let servers_by_addr = Hashtbl.create 127
let nservers = ref 0  
  
let files_by_key = Hashtbl.create 13
let clients_by_name = Hashtbl.create 127
let users_by_name = Hashtbl.create 127
let results_by_file = Hashtbl.create 127

let listen_sock = ref (None : TcpServerSocket.t option)
  
let (current_files : OpennapTypes.file list ref) = ref []
  
let client_type c =
  client_type (as_client c.client_client)
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)

  
      
let exit_exn = Exit
let basename filename =
  let s =
    let len = String.length filename in
    try
      let pos = String.rindex_from filename (len-1) '\\' in
      String.sub filename (pos+1) (len-pos-1)
    with _ ->      
        try
          if len > 2 then
            let c1 = Char.lowercase filename.[0] in
            let c2 = filename.[1] in
            match c1,c2 with
              'a'..'z', ':' ->
                String.sub filename 2 (len -2 )
            | _ -> raise exit_exn
          else raise exit_exn
        with _ -> Filename.basename filename
  in
  String.lowercase s

let new_server ip port = 
  let key = (ip,port) in
  try
    Hashtbl.find servers_by_addr key
  with _ ->
      let rec s = {
          server_server = server_impl;
          server_ip = ip;
          server_port = port;
          server_desc = "";
          server_net = "";
          server_sock = NoConnection;
          server_nusers = Int64.zero;
          server_nfiles = Int64.zero;
          server_size = 0;
          server_connection_control = DG.new_connection_control ( ());
          server_nick_num = -1;
          server_last_nick = "";
          server_searches = None;
          server_users = [];
          server_pending_searches = [];
          server_browse_queue = [];
        } and 
        server_impl = {
          dummy_server_impl with
          impl_server_val = s;
          impl_server_ops = server_ops;
        } 
      in
      server_add server_impl;
      servers_list := s :: !servers_list;
      current_servers := s :: !current_servers;
      Hashtbl.add servers_by_addr key s;
      s
      
let set_server_state s state =
  set_server_state (as_server s.server_server) state

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

let new_result file_name file_size =
  let basename = basename file_name in
  let key = (basename, file_size) in
  try
    Hashtbl.find results_by_file key
  with _ ->
      let r = {
          dummy_result with
          result_names = [basename];
          result_size = file_size;
        }
      in
      let r = update_result_num r in
      Hashtbl.add results_by_file key r;
      r
  
let new_file file_id file_name file_size =
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key key  
  with _ ->
      let file_temp = Filename.concat !!DO.temp_directory 
          (Printf.sprintf "ON-%s" (Md4.to_string file_id)) in
      let t = Unix32.create_rw file_temp in
      let current_size = try
          Unix32.getsize file_temp
        with e ->
            lprintf "Exception %s in current_size\n" (Printexc2.to_string e); 
            Int64.zero
      in
      
      let rec file = {
          file_file = file_impl;
          file_id = file_id;
          file_name = file_name;
          file_clients = [];
        } 
      and file_impl = {
          (dummy_file_impl ()) with
          impl_file_ops = file_ops;
          impl_file_val = file; 
          impl_file_fd = Some t;
          impl_file_size = file_size;
          impl_file_downloaded = current_size;
          impl_file_age = last_time ();          
          impl_file_best_name = file_name;
        }
      in
      let state = if current_size = file_size then FileDownloaded else
          FileDownloading in
      file_add file_impl state;
      Hashtbl.add files_by_key key file;
      file
  
let find_file file_name file_size =
  let key = (file_name, file_size) in
    Hashtbl.find files_by_key key  


let new_user server name =
  let u =
    try
      let user = Hashtbl.find users_by_name name in
      user
    with _ ->
        let rec user = {
            user_nick = name;
            user_servers = [];
            user_user = user_impl;
            user_link = LinkUnknown;
            user_addr = None;
          } and user_impl = {
            dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
          }
        in
        user_add user_impl;
        Hashtbl.add users_by_name name user;
        user      
  in
  match server with
    None -> u
  | Some s ->
      if not (List.memq s u.user_servers) then begin
          lprintf 
          "User %s(%d) is on %s(%d)" 
            u.user_nick
            u.user_user.impl_user_num 
            s.server_desc 
            s.server_server.impl_server_num;
          lprint_newline ();
          u.user_servers <- s :: u.user_servers;
        end;
      u
  
let new_client name =
  try
    Hashtbl.find clients_by_name name 
  with _ ->
      let user = new_user None name in
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
          client_name = name;
          client_addr = None;
          client_files = [];
          client_file = None;
          client_pos = Int64.zero;
          client_all_files = None;
          client_user = user;
          client_error = false;
          client_connection_control = new_connection_control (());
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_upload = None;
        } in
      new_client impl;
      Hashtbl.add clients_by_name name c;
      c

let add_source r user filename =
  let ss =
    try
      Hashtbl.find result_sources r.stored_result_num
    with _ ->
        let ss = ref [] in
        Hashtbl.add result_sources r.stored_result_num ss;
        ss
  in
  let key = (user, filename) in
  if not (List.memq key !ss) then begin
      ss := key :: !ss
    end

let add_file_client file user filename = 
  let  c = new_client user.user_nick in
  if not (List.memq c file.file_clients) then begin
      file.file_clients <- c :: file.file_clients;
      c.client_files <- (file, filename) :: c.client_files
    end;
  c
      
let file_state file =
  file_state (as_file file.file_file)
  
let server_num s =
  server_num (as_server s.server_server)
  
    
let server_state s =
  server_state (as_server s.server_server)
  
let server_remove s =
  server_remove (as_server s.server_server);
  Hashtbl.remove servers_by_addr (s.server_ip, s.server_port);
  current_servers := List2.removeq s !current_servers;
  servers_list := List2.removeq s !servers_list
  
    
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file_fd (as_file file.file_file)
  
  
let shared_counter = ref (Int64.zero)
let shared_files = Hashtbl.create 13 

  
let new_shared_dir dirname = {
    shared_dirname = dirname;
    shared_files = [];
    shared_dirs = [];
  }

let shared_tree = new_shared_dir ""

let rec add_shared_file node sh dir_list =
  match dir_list with
    [] -> assert false
  | [filename] ->
      node.shared_files <- sh :: node.shared_files
  | dirname :: dir_tail ->
      let node =
        try
          List.assoc dirname node.shared_dirs
        with _ ->
            let new_node = new_shared_dir dirname in
            node.shared_dirs <- (dirname, new_node) :: node.shared_dirs;
            new_node
      in
      add_shared_file node sh dir_tail
  
let add_shared full_name codedname size =
  try
    match CommonMultimedia.get_info full_name with
      MP3 (tags, info) ->
        let sh = {
            shared_fullname = full_name;
            shared_codedname = String2.replace codedname '/' "\\";
            shared_size = size;
            shared_fd=  Unix32.create_ro full_name;
            shared_format = (tags, info);
          } in
        Hashtbl.add shared_files codedname sh;
        add_shared_file shared_tree sh (String2.split codedname '/');
        lprintf "Total shared : %s" (Int64.to_string !shared_counter);
        lprint_newline () 
    | _ -> ()
  with _ -> ()
      
