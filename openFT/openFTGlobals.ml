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
open CommonUser
open CommonTypes
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonFile
open BasicSocket
open CommonGlobals
open Options
open OpenFTTypes
open OpenFTOptions
    
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
  
let network = new_network "OpenFT"  
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
let (room_ops : server CommonRoom.room_ops) = 
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

  
  
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file.file_file.impl_file_downloaded
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)

    
let file_state file =
  file_state (as_file file.file_file)
  
let file_num file =
  file_num (as_file file.file_file)
  
let file_must_update file =
  file_must_update (as_file file.file_file)

let server_num s =
  server_num (as_server s.server_server)
      
let server_state s =
  server_state (as_server s.server_server)
      
let set_server_state s state =
  set_server_state (as_server s.server_server) state

let client_type c = client_type (as_client c.client_client)

module DO = CommonOptions

let current_files = ref ([] : OpenFTTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let connected_servers = ref ([] : server list)
let servers_by_key = Hashtbl.create 103

let (searches_by_uid : (int, local_search) Hashtbl.t) = Hashtbl.create 11
let (ultrapeers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
let (peers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
let nservers = ref 0
let redirector_connected = ref false

let redirectors_ips = ref ( [] : Ip.t list)
let redirectors_to_try = ref ( [] : Ip.t list)
  
let files_by_md5 = Hashtbl.create 13

let (users_by_num : (int, user) Hashtbl.t) = Hashtbl.create 127
let (clients_by_num : (int, client) Hashtbl.t) = Hashtbl.create 127
let results_by_key = Hashtbl.create 127

let nsearches = ref 0
  
let new_server ip port =
  let key = (ip,port) in
  try
    Hashtbl.find servers_by_key key
  with _ ->
      let rec s = {
          server_server = server_impl;
          server_ip = ip;
          server_port = port;
          server_sock = None;
          server_agent = "<unknown>";
          server_nfiles = 0;
          server_nkb = 0;
          server_http_port = 0;
          server_caps = [];
          server_version = "";
          server_type = Index_node; (* we don't know *)
          server_connection_control = new_connection_control (last_time ());
      
          server_ping_last = Md4.random ();
          server_nfiles_last = 0;
          server_nkb_last = 0;
        } and
        server_impl = {
          dummy_server_impl with
          impl_server_val = s;
          impl_server_ops = server_ops;
        } in
      server_add server_impl;
      Hashtbl.add servers_by_key key s;
      s

let new_result file_md5 file_name file_size =
  try
    Hashtbl.find results_by_key file_md5
  with _ ->
      let rec result = {
          result_result = result_impl;
          result_md5 = file_md5;
          result_name = file_name;
          result_size = file_size;
          result_sources = [];
        } and
        result_impl = {
          dummy_result_impl with
          impl_result_val = result;
          impl_result_ops = result_ops;
        } in
      new_result result_impl;
      Hashtbl.add results_by_key file_md5 result;
      result
      
  
  
let new_file file_id file_name file_size =
  try
    Hashtbl.find files_by_md5 file_id
  with _ ->
      let file_temp = Filename.concat !!DO.temp_directory 
          (Printf.sprintf "LW-%s" (Md4.to_string file_id)) in
      let current_size = try
          Unix32.getsize32 file_temp
        with e ->
            Printf.printf "Exception %s in current_size" (Printexc.to_string e); 
            print_newline ();
            Int32.zero
      in
      
      let rec file = {
          file_file = file_impl;
          file_md5 = file_id;
          file_name = file_name;
          file_clients = [];
        } and file_impl =  {
          dummy_file_impl with
          impl_file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
          impl_file_size = file_size;
          impl_file_downloaded = current_size;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_best_name = file_name;
          }
      in
      
      let state = if current_size = file_size then FileDownloaded else
          FileDownloading in
      
      if state = FileDownloading then begin
          Printf.printf "ADDING FILE %s" file_name; print_newline ();
          current_files := file :: !current_files
        end;
      file_add file_impl state;
(*      Printf.printf "ADD FILE TO DOWNLOAD LIST"; print_newline (); *)
      Hashtbl.add files_by_md5 file_id file;
      file


let new_user ip port http_port =
  let s = new_server ip port in
  s.server_http_port <- http_port;
  try
    let u = Hashtbl.find users_by_num (server_num s) in
    u
  with _ ->
      let rec user = {
          user_user = user_impl;
          user_server = s;
        }  and user_impl = {
          dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
          } in
      user_add user_impl;
      Hashtbl.add users_by_num (server_num s) user;
      user
  
let new_client ip port http_port =
  let u = new_user ip port http_port in
  try
    Hashtbl.find clients_by_num (server_num u.user_server)
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = None;
(*          client_name = name; 
          client_kind = None; *)
          client_file = None;
(*          client_pos = Int32.zero; *)
          client_all_files = None;
          client_user = u;
          client_connection_control = new_connection_control (last_time());
          client_downloads = [];
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
        } in
      new_client impl;
      Hashtbl.add clients_by_num (server_num u.user_server) c;
      c

let add_source r s index =
  let key = (s, index) in
  if not (List.mem key r.result_sources) then begin
      r.result_sources <- key :: r.result_sources
    end
    
let add_download file c index =
(*  let r = new_result file.file_md5 file.file_name (file_size file) in *)
(*  add_source r c.client_user index; *)
  Printf.printf "Adding file to client"; print_newline ();
  if not (List.memq c file.file_clients) then begin
      c.client_downloads <- (file, index) :: c.client_downloads;
      file.file_clients <- c :: file.file_clients;
      file_new_source (as_file file.file_file) (as_client c.client_client)
    end
    
      
