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
open SlskOptions
open SlskTypes

    
open CommonNetwork

(*
    mutable op_network_connected_servers : (unit -> server list);
    mutable op_network_config_file : (unit -> Options.options_file);
    mutable op_network_is_enabled : (unit -> bool);
    mutable op_network_save_simple_options : (unit -> unit);
    mutable op_network_load_simple_options : (unit -> unit);
    mutable op_network_save_complex_options : (unit -> unit);
    mutable op_network_load_complex_options : (unit -> unit);
    mutable op_network_enable : (unit -> unit);
    mutable op_network_disable : (unit -> unit);    
    mutable op_network_add_server : 
      ((string * Options.option_value) list -> server);
    mutable op_network_add_file : 
      bool -> ((string * Options.option_value) list -> file);
    mutable op_network_add_client : 
      bool -> ((string * Options.option_value) list -> client);
    mutable op_network_prefixed_args : 
      (unit -> (string * Arg.spec * string) list);    
    mutable op_network_search : (search -> Buffer.t -> unit);
    mutable op_network_share : (shared -> unit);
    mutable op_network_private_message : (string -> string -> unit);
    mutable op_network_connect_servers : (unit -> unit);
    mutable op_network_add_server_id : (Ip.t -> int -> unit);
    mutable op_network_forget_search : (search -> unit);
    mutable op_network_close_search : (search -> unit);
    mutable op_network_extend_search : (unit -> unit);
    mutable op_network_clean_servers : (unit -> unit);
    mutable op_network_add_friend_id : (Ip.t -> int -> unit);

*)
  
let network = new_network "Soulseek"  
    network_options_prefix commit_in_subdir
  (*
     op_result_network : network;
     op_result_download : ('a -> string list -> unit);
     op_result_info : ('a -> CommonTypes.result_info);
  *)
      
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
  
(*
     op_server_network : network;
     op_server_to_option : ('a -> (string * option_value) list);
     op_server_remove : ('a -> unit);
     op_server_info : ('a -> GuiProto.server_info);
     op_server_sort : ('a -> float);
     op_server_connect : ('a -> unit);
     op_server_disconnect : ('a -> unit);
     op_server_users : ('a -> user list);
     op_server_query_users : ('a -> unit);
     op_server_find_user : ('a -> string -> unit);
     op_server_new_messages : (unit -> (int * int * string) list);
*)
  
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
  
(*
  
     op_room_close : ('a -> unit);
     op_room_pause : ('a -> unit);
     op_room_resume : ('a -> unit);
     op_room_messages : ('a -> room_message list);
     op_room_users : ('a -> user list);
     op_room_name : ('a -> string);
     op_room_info : ('a -> GuiProto.room_info);
     op_room_send_message : ('a -> room_message -> unit);

*)
let (room_ops : room CommonRoom.room_ops) = 
  CommonRoom.new_room_ops network
  
(*
     op_user_network : network;
     op_user_commit : ('a -> unit);
     op_user_save_as : ('a -> string -> unit);
     op_user_print : ('a -> CommonTypes.connection_options -> unit);
     op_user_to_option : ('a -> (string * option_value) list);
     op_user_remove : ('a -> unit);
     op_user_info : ('a -> GuiProto.user_info);
     op_user_set_friend : ('a -> unit);
     op_user_browse_files : ('a -> unit);
*)
  
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
  
(*
     op_file_network : network;
     op_file_commit : ('a -> unit);
     op_file_save_as : ('a -> string -> unit);
     op_file_to_option : ('a -> (string * option_value) list);
     op_file_cancel : ('a -> unit);
     op_file_pause : ('a -> unit);
     op_file_resume : ('a -> unit);
     op_file_info : ('a -> GuiProto.file_info);
     op_file_disk_name : ('a -> string);
     op_file_best_name : ('a -> string);
     op_file_state : ('a -> CommonTypes.file_state);
     op_file_set_format : ('a -> CommonTypes.format -> unit);
     op_file_check : ('a -> unit);
     op_file_recover : ('a -> unit);
     op_file_sources : ('a -> client list);
*)
  
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
  
(*
     op_client_network : network;
     op_client_commit : ('a -> unit);
     op_client_connect : ('a -> unit);
     op_client_save_as : ('a -> string -> unit);
     op_client_to_option : ('a -> (string * option_value) list);
     op_client_cancel : ('a -> unit);
     op_client_info : ('a -> GuiProto.client_info);
     op_client_say : ('a -> string -> unit);
     op_client_files : ('a -> (string * result) list);
     op_client_set_friend : ('a -> unit);
     op_client_remove_friend : ('a -> unit);
*)
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

  

let login () = 
  if !!login = "" then !!CommonOptions.client_name else !!login
  
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
let file_downloaded file = file.file_file.impl_file_downloaded
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
  
let client_type c =
  client_type (as_client c.client_client)

let nknown_servers = ref 0
let connected_servers = ref ([] : server list)

let servers_by_addr = Hashtbl.create 13
  
let new_server addr port=
  try
    Hashtbl.find servers_by_addr (addr, port) 
  with _ ->
      incr nknown_servers;
      let rec h = { 
          server_server = server_impl;
          server_name = "<unknown>";
          server_addr = addr;
          server_nusers = 0;
          server_info = "";
          server_connection_control = new_connection_control ();
          server_sock = None;
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
          client_peer_sock = None;
          client_downloads = [];
          client_result_socks = [];
          client_name = name;
          client_addr = None;
          client_files = [];
          client_all_files = None;
          client_receiving = Int32.zero;
          client_connection_control = new_connection_control ();
          client_user = u;
          client_requests = [];
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
        } in
      new_client impl;
      Hashtbl.add clients_by_name name c;
      c

      
let new_result filename filesize =
  let basename = Filename2.basename filename in
  let key = (basename, filesize) in  
  try
    Hashtbl.find results_by_file key
  with _ ->
      let rec result = {
          result_result = result_impl;
          result_name = basename;
          result_size = filesize;
          result_sources = [];
        } and
        result_impl = {
          dummy_result_impl with
          impl_result_val = result;
          impl_result_ops = result_ops;
        } in
      new_result result_impl;
      Hashtbl.add results_by_file key result;
      result
        
        
let add_result_source r u filename =
  if not (List.mem_assoc u r.result_sources) then begin
      r.result_sources <- (u, filename) :: r.result_sources
    end
      
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

    
let new_file file_id name file_size =
  let key = (name, file_size) in
  try
    Hashtbl.find files_by_key key
  with _ ->
      let file_temp = Filename.concat !!temp_directory 
          (Printf.sprintf "SK-%s" (Md4.to_string file_id)) in
      let current_size = try
          Unix32.getsize32 file_temp
        with e ->
            Printf.printf "Exception %s in current_size" (Printexc.to_string e); 
            print_newline ();
            Int32.zero
      in
      
      let rec file = {
          file_file = impl;
          file_id = file_id;
          file_clients = [];
        } and impl = {
          dummy_file_impl with
          impl_file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
          impl_file_size = file_size;
          impl_file_downloaded = current_size;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_best_name = name;
        } in
      let state = if current_size = file_size then FileDownloaded else begin
            current_files := file :: !current_files;
            FileDownloading
          end
      in
      file_add impl state;
      Hashtbl.add files_by_key key file;
      file

let find_file file_name file_size =
  Hashtbl.find files_by_key (file_name, file_size)

  
      
let add_file_client file user filename = 
  let  c = new_client user.user_nick in
  if not (List.memq c file.file_clients) then begin
      file.file_clients <- c :: file.file_clients;
      c.client_files <- (file, filename) :: c.client_files;
      file_new_source (as_file file.file_file) (as_client c.client_client)
    end;
  c
