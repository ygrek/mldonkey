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

open CommonFile
open CommonResult
open BasicSocket
open CommonGlobals
open CommonTypes
open CommonClient
open CommonComplexOptions
open GuiProto
open Options
open CommonUser
open CommonRoom
open CommonTypes
open CommonShared
open CommonServer
open DcTypes
open DcOptions
open CommonInteractive
  
module DO = CommonOptions

open CommonNetwork

(*
OK   op_network_connected_servers : (unit -> server list);
OK   op_network_is_enabled : (unit -> bool);
     op_network_save_complex_options : (unit -> unit);
     op_network_load_complex_options : (unit -> unit);
OK   op_network_enable : (unit -> unit);
     op_network_disable : (unit -> unit);    
OK   op_network_add_server:((string * Options.option_value) list -> server);
OK   op_network_add_file:bool -> ((string * Options.option_value) list -> file);
     op_network_add_client:bool -> ((string * Options.option_value) list -> client);
OK   op_network_prefixed_args:(unit -> (string * Arg.spec * string) list);    
OK   op_network_search : (search -> Buffer.t -> unit);
OK   op_network_share : (shared -> unit);
     op_network_private_message : (string -> string -> unit);
     op_network_connect_servers : (unit -> unit);
     op_network_forget_search : (search -> unit);
     op_network_close_search : (search -> unit);
     op_network_extend_search : (unit -> unit);
     op_network_clean_servers : (unit -> unit);
OK   op_network_parse_url
*)
  
let network = new_network "Direct Connect"  
    [ 
    NetworkHasServers; 
    NetworkHasRooms;
    NetworkHasChat;
    NetworkHasSearch;
    NetworkHasUpload;
    ]
    (fun _ -> !!network_options_prefix)
  (fun _ -> !!commit_in_subdir)

let connection_manager = network.network_connection_manager
  
  (*
OK   op_result_download : ('a -> string list -> unit);
OK   op_result_info : ('a -> CommonTypes.result_info);
  *)
  
  (*
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
    *)

(*
OK   op_server_to_option : ('a -> (string * option_value) list);
OK   op_server_remove : ('a -> unit);
OK   op_server_info : ('a -> GuiProto.server_info);
OK   op_server_sort : ('a -> float);
OK   op_server_connect : ('a -> unit);
OK   op_server_disconnect : ('a -> unit);
OK   op_server_users : ('a -> user list);
OK   op_server_query_users : ('a -> unit);
     op_server_find_user : ('a -> string -> unit);
*)
  
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
  
(*
     op_room_close : ('a -> unit);
     op_room_pause : ('a -> unit);
     op_room_resume : ('a -> unit);
OK   op_room_messages : ('a -> room_message list);
     op_room_users : ('a -> user list);
OK   op_room_name : ('a -> string);
OK   op_room_info : ('a -> GuiProto.room_info);
OK   op_room_send_message : ('a -> room_message -> unit);

*)
let (room_ops : server CommonRoom.room_ops) = 
  CommonRoom.new_room_ops network
  
(*
OK   op_user_remove : ('a -> unit);
OK   op_user_info : ('a -> GuiProto.user_info);
OK   op_user_set_friend : ('a -> unit);
OK   op_user_browse_files : ('a -> unit);
*)
  
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
  
(*
OK   op_file_commit : ('a -> unit);
OK   op_file_save_as : ('a -> string -> unit);
OK   op_file_to_option : ('a -> (string * option_value) list);
     op_file_cancel : ('a -> unit);
     op_file_pause : ('a -> unit);
     op_file_resume : ('a -> unit);
OK   op_file_info : ('a -> GuiProto.file_info);
OK   op_file_disk_name : ('a -> string);
OK   op_file_best_name : ('a -> string);
     op_file_set_format : ('a -> CommonTypes.format -> unit);
     op_file_check : ('a -> unit);
     op_file_recover : ('a -> unit);
OK   op_file_sources : ('a -> client list);
*)
  
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
  
(*
     op_client_connect : ('a -> unit);
     op_client_to_option : ('a -> (string * option_value) list);
OK   op_client_info : ('a -> GuiProto.client_info);
     op_client_say : ('a -> string -> unit);
     op_client_files : ('a -> (string * result) list);
OK   op_client_set_friend : ('a -> unit);
     op_client_remove_friend : ('a -> unit);
*)
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

    
let (shared_ops : CommonUploads.shared_file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network

let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)

let nservers = ref 0

  
let listen_sock = ref (None : TcpServerSocket.t option)

let servers_list = ref ([] : server list) 
let connected_servers = ref ([]: server list)
let servers_by_addr = Hashtbl.create 100
let nknown_servers = ref 0  
  
let users_by_name = Hashtbl.create 113
    
let files_by_key = Hashtbl.create 47

let current_files = ref []
      
let clients_by_name = Hashtbl.create 113

let results_by_file = Hashtbl.create 111


(**)
(**)
(*             FUNCTIONS          *)
(**)
(**)
  
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
  
exception Found of user  

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
            user_link = "";
            user_data = 0.0;
            user_admin = false;
          } and user_impl = {
            dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
          }
        in
        Hashtbl.add users_by_name name user;
        user_add user_impl;
        user      
  in
  match server with
    None -> u
  | Some s ->
      if not (List.memq s u.user_servers) then begin

          (*lprintf "%s(%d) is on %s" u.user_nick u.user_user.impl_user_num s.server_name; lprint_newline (); *)
          u.user_servers <- s :: u.user_servers;
        end;
      u
        
let user_add s name =
  try
    StringMap.find name s.server_users
  with _ ->
      let user = new_user (Some s) name in
      server_new_user (as_server s.server_server) (as_user user.user_user);
      room_add_user (as_room s.server_room) (as_user user.user_user);
      s.server_users <- StringMap.add name user s.server_users;
      user
      
let user_remove s name = 
  try
    let user = StringMap.find name s.server_users in
    s.server_users <- StringMap.remove user.user_nick s.server_users;
(* server_remove_user (as_server s.server_server) (as_user user.user_user); *)
    room_remove_user (as_room s.server_room) (as_user user.user_user);
    
  with _ -> ()
      
      
let new_file file_id name file_size =
  let key = (name, file_size) in
  try
    Hashtbl.find files_by_key key
  with _ ->
      let file_temp = Filename.concat !!DO.temp_directory 
          (Printf.sprintf "DC-%s" (Md4.to_string file_id)) in
      let t = Unix32.create_rw file_temp in
      let current_size = try
          Unix32.getsize file_temp
        with e ->
            lprintf "Exception %s in current_size of %s" (Printexc2.to_string e) file_temp; 
            lprint_newline ();
            Int64.zero
      in
      
      let rec file = {
          file_file = impl;
          file_name = name;
          file_id = file_id;
          file_clients = [];
        } and impl = {
          dummy_file_impl with
          impl_file_fd = t;
          impl_file_size = file_size;
          impl_file_downloaded = current_size;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_best_name = name;
        } in
      
      lprintf "FILE %s: %Ld =? %Ld" name current_size file_size;
      lprint_newline ();
      
      let state = if current_size = file_size then begin
            lprintf "File %s downloaded " name; lprint_newline ();
            FileDownloaded
          end else begin
            current_files := file :: !current_files;
            FileDownloading
          end
      in
      file_add impl state;
      Hashtbl.add files_by_key key file;
      file

let find_file file_name file_size =
  Hashtbl.find files_by_key (file_name, file_size)
      
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
          client_download = DcIdle;
          client_pos = Int64.zero;
          client_all_files = None;
          client_receiving = Int64.zero;
          client_user = user;
          client_connection_control = new_connection_control ();
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
	  impl_client_upload = None;
        } in
      new_client impl;
      Hashtbl.add clients_by_name name c;
      c

      
let add_file_client file user filename = 
  let  c = new_client user.user_nick in
  if not (List.memq c file.file_clients) then begin
      file.file_clients <- c :: file.file_clients;
      c.client_files <- (file, filename) :: c.client_files;
      file_add_source (as_file file.file_file) (as_client c.client_client)
    end;
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

    (*
let add_result_source r u filename =
  if not (List.mem_assoc u r.result_sources) then begin
      r.result_sources <- (u, filename) :: r.result_sources
    end
*)      
let remove_client c =
  Hashtbl.remove clients_by_name c.client_name;
  List.iter (fun (file,_) ->
      file.file_clients <- List2.removeq c file.file_clients
  ) c.client_files;
  client_remove (as_client c.client_client)
  
      
let set_client_state c state =
  set_client_state (as_client c.client_client) state

let set_client_disconnected c =
  set_client_disconnected (as_client c.client_client)
              
let local_login () =
  if !!login = "" then !!CommonOptions.global_login else !!login
    
let login s =
  let nick = 
    if s.server_nick = 0 then local_login () else 
      Printf.sprintf "%s_%d" (local_login ()) s.server_nick 
  in
  s.server_nick <- s.server_nick + 1;
  s.server_last_nick <- nick;
  nick

let client_type c =
  client_type (as_client c.client_client)
    
let new_server addr port=
  try
    Hashtbl.find servers_by_addr (Ip.string_of_addr addr, port) 
  with _ ->
      incr nknown_servers;
      let rec h = { 
          server_server = server_impl;
          server_room = room_impl;
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
          server_search_timeout = 0;
          server_users = StringMap.empty;
          server_messages = [];
          server_searches = Fifo.create ();
        } and 
        server_impl = {
          dummy_server_impl with
          impl_server_val = h;
          impl_server_ops = server_ops;
        } and 
        room_impl = {
          dummy_room_impl with
          impl_room_val = h;
          impl_room_ops = room_ops;
        }         
      in
      server_add server_impl;
      room_add room_impl;
      Hashtbl.add servers_by_addr (Ip.string_of_addr addr, port) h;
      h
  
let new_result filename filesize =
  let basename = Filename2.basename filename in
  let key = (basename, filesize) in
  try
    Hashtbl.find results_by_file key
  with _ ->
      let rec r = {
          dummy_result with
          result_names = [filename];
          result_size = filesize;
        } in
      let r = update_result_num r in
      Hashtbl.add results_by_file key r;
      r
      
let server_remove s =
  server_remove (as_server s.server_server);
  Hashtbl.remove servers_by_addr (Ip.string_of_addr s.server_addr, s.server_port);
  decr nknown_servers;
  servers_list := List2.removeq s !servers_list

  
  
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
  