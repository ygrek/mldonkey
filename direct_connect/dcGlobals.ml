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

open CommonClient
open CommonComplexOptions
open Gui_proto
open Options
open CommonFile
open CommonUser
open CommonChatRoom
open CommonTypes
open CommonShared
open CommonServer
open DcTypes
open DcOptions
  
module DO = CommonOptions
  
let nservers = ref 0
        
let servers_list = ref ([] : server list)
let connected_servers = ref ([]: server list)

let set_server_state s state =
  set_server_state (as_server s.server_server) state
let set_room_state s state =
  set_room_state (as_room s.server_room) state
let server_num s = server_num (as_server s.server_server)
let file_num s = file_num (as_file s.file_file)
let server_state s = server_state (as_server s.server_server)
let file_state s = file_state (as_file s.file_file)
let server_must_update s = server_must_update (as_server s.server_server)
let file_must_update s = file_must_update (as_file s.file_file)
let user_remove user = user_remove (as_user user.user_user)  
  
let shared_files = ref []
let shared_total = ref 0.0
  
let add_shared s =
  shared_files  := s :: !shared_files;
  let size = Unix32.getsize32 (shared_filename s) in
  shared_total :=  !shared_total +. (Int32.to_float size);
  Printf.printf "Total shared : %f" !shared_total;
  print_newline () 
  
let _ =
  network.op_network_share <- add_shared
 
exception Found of user  
  
let user_add server name =
  try
    List.iter (fun user -> if user.user_nick = name then raise (Found user)) 
    server.server_users;
    let rec user = {
        user_nick = name;
        user_server = server;
        user_user = user_impl;
      } and user_impl = {
        impl_user_state = NewHost;
        impl_user_num = 0;
        impl_user_ops = user_ops;
        impl_user_val = user;
      }
    in
    user_add user_impl;
    server_new_user (as_server server.server_server) (as_user user_impl);
    room_new_user (as_room server.server_room) (as_user user_impl);
    server.server_users <- user :: server.server_users;
    user
  with Found user -> user
    
let files_by_key = Hashtbl.create 47
  
let new_file file_id name size =
  let key = (name, size) in
  try
    Hashtbl.find files_by_key key
  with _ ->
      let file_temp = Filename.concat !!DO.temp_directory 
          (Printf.sprintf "DC-%s" (Md4.to_string file_id)) in
      let current_size = try
          Unix32.getsize32 file_temp
        with e ->
            Printf.printf "Exception %s in current_size" (Printexc.to_string e); 
            print_newline ();
            Int32.zero
      in
      
      let rec file = {
          file_file = impl;
          file_name = name;
          file_size = size;
          file_id = file_id;
          file_downloaded = current_size;
          file_temp = file_temp;
          file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
        } and impl = {
          impl_file_num = 0;
          impl_file_state = FileNew;
          impl_file_val = file;
          impl_file_ops = file_ops;
        } in
      file_add impl FileDownloading;
      Hashtbl.add files_by_key key file;
      file
      
let add_source file src filename = 
  raise Not_found

let clients_by_name = Hashtbl.create 113
  
let new_client name =
  try
    Hashtbl.find clients_by_name name 
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = None;
          client_name = name;
          client_server = None;
          client_files = [];
          client_download = DcIdle;
          client_pos = Int32.zero;
          client_all_files = None;
          client_receiving = Int32.zero;
        } and impl = {
          impl_client_state = NotConnected;
          impl_client_type = NormalClient;
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_num = 0;          
        } in
      new_client impl;
      Hashtbl.add clients_by_name name c;
      c

let remove_client c =
  Hashtbl.remove clients_by_name c.client_name;
  client_remove (as_client c.client_client)
      
let set_client_state c state =
  set_client_state (as_client c.client_client) state
              
let login () =
  if !!login = "" then !!CommonOptions.client_name else !!login
    
let login s =
  let nick = 
    if s.server_nick = 0 then login () else 
      Printf.sprintf "%s_%d" (login ()) s.server_nick 
  in
  s.server_nick <- s.server_nick + 1;
  s.server_last_nick <- nick;
  nick

let client_type c =
  client_type (as_client c.client_client)