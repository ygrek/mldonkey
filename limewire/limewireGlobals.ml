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
open LimewireTypes

  
module DO = CommonOptions

let current_files = ref ([] : LimewireTypes.file list)
  
let connected_servers = ref ([] : server list)
let servers_by_key = Hashtbl.create 103

let (searches_by_uid : (Md4.t, local_search) Hashtbl.t) = Hashtbl.create 11
let (ultrapeers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
let (peers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
let nservers = ref 0
let redirector_connected = ref false

let redirectors_ips = ref ( [] : Ip.t list)
let redirectors_to_try = ref ( [] : Ip.t list)
  
let files_by_key = Hashtbl.create 13

let (users_by_uid : (Md4.t, user) Hashtbl.t) = Hashtbl.create 127
let (clients_by_uid : (Md4.t, client) Hashtbl.t) = Hashtbl.create 127
let results_by_key = Hashtbl.create 127

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

let new_result file_name file_size =
  let key = (file_name, file_size) in
  try
    Hashtbl.find results_by_key key
  with _ ->
      let rec result = {
          result_result = result_impl;
          result_name = file_name;
          result_size = file_size;
          result_sources = [];
        } and
        result_impl = {
          dummy_result_impl with
          impl_result_val = result;
          impl_result_ops = result_ops;
        } in
      CommonResult.new_result result_impl;
      Hashtbl.add results_by_key key result;
      result
      
  
  
let new_file file_id file_name file_size =
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key key  
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
          file_id = file_id;
          file_name = file_name;
          file_size = file_size;
          file_downloaded = current_size;
          file_temp = file_temp;
          file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
          file_clients = [];
        } and file_impl =  {
          dummy_file_impl with

          impl_file_val = file;
          impl_file_ops = file_ops;
        }
      in
      
      let state = if current_size = file_size then FileDownloaded else
          FileDownloading in
      
      file_add file_impl FileDownloading;
(*      Printf.printf "ADD FILE TO DOWNLOAD LIST"; print_newline (); *)
      Hashtbl.add files_by_key key file;
      file


let new_user uid kind =
  try
    let s = Hashtbl.find users_by_uid uid in
    s.user_kind <- kind;
    s
  with _ ->
      let rec user = {
          user_user = user_impl;
          user_uid = uid;
          user_kind = kind;
          user_files = [];
          user_speed = 0;
        }  and user_impl = {
          dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
          } in
      user_add user_impl;
      Hashtbl.add users_by_uid uid user;
      user
  
let new_client uid kind =
  try
    Hashtbl.find clients_by_uid uid 
  with _ ->
      let user = new_user uid kind in
      let rec c = {
          client_client = impl;
          client_sock = None;
(*          client_name = name; 
          client_kind = None; *)
          client_file = None;
          client_pos = Int32.zero;
          client_all_files = None;
          client_user = user;
          client_error = false;
          client_connection_control = new_connection_control (last_time());
          client_downloads = [];
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
        } in
      new_client impl;
      Hashtbl.add clients_by_uid uid c;
      c

let add_source r s index =
  if not (List.memq s r.result_sources) then begin
      s.user_files <- (r,index) :: s.user_files;
      r.result_sources <- s :: r.result_sources
    end
    
let add_download file c index =
  let r = new_result file.file_name file.file_size in
(*  add_source r c.client_user index; *)
  if not (List.memq c file.file_clients) then begin
      c.client_downloads <- (file, index) :: c.client_downloads;
      file.file_clients <- c :: file.file_clients
    end
    
      
  
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
  
let server_remove s =
  connected_servers := List2.removeq s !connected_servers;    
  Hashtbl.remove servers_by_key (s.server_ip, s.server_port)

let client_type c = client_type (as_client c.client_client)
  