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
          impl_server_state = NewHost;
          impl_server_sort = 0.0;
          impl_server_val = s;
          impl_server_ops = server_ops;
          impl_server_num = 0;
        } in
      server_add server_impl;
      Hashtbl.add servers_by_key key s;
      s

(*      
let searches = ref ([] : search list)
  *)
let (searches_by_uid : (Md4.t, local_search) Hashtbl.t) = Hashtbl.create 11

let (ultrapeers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
let (peers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
let nservers = ref 0
let redirector_connected = ref false

let redirectors_ips = ref ( [] : Ip.t list)
let redirectors_to_try = ref ( [] : Ip.t list)

  
  
let files_by_key = Hashtbl.create 13

let (sources_by_uid : (Md4.t, source) Hashtbl.t) = Hashtbl.create 127
let results_by_file = Hashtbl.create 127

let new_result file_name file_size =
  let file = {
      file_name = file_name;
      file_size = file_size;
    } in
  try
    Hashtbl.find results_by_file file
  with _ ->
      let rec result = {
          result_result = result_impl;
          result_file = file;
          result_sources = [];
        } and
        result_impl = {
          impl_result_val = result;
          impl_result_ops = result_ops;
          impl_result_num = 0;
        } in
      CommonResult.new_result result_impl;
      Hashtbl.add results_by_file file result;
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
          file_result = new_result file_name file_size;
          file_downloaded = current_size;
          file_temp = file_temp;
          file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
        } and file_impl =  {
          impl_file_state = FileNew;
          impl_file_num = 0;
          impl_file_val = file;
          impl_file_ops = file_ops;
        }
      in
      file_add file_impl FileDownloading;
      Hashtbl.add files_by_key key file;
      file


let new_source uid ip port =
  try
    let s = Hashtbl.find sources_by_uid uid in
    s.source_ip <- ip;
    s.source_port <- port;
    s
  with _ ->
      let src = {
          source_uid = uid;
          source_ip = ip;
          source_port = port;
          source_files = [];
          source_speed = 0;
          source_client = None;
          source_downloads = [];
          source_connection_control = 
          new_connection_control (last_time ());
          source_push = true;
        } in
      Hashtbl.add sources_by_uid uid src;
      src


let add_source r s index =
  if not (List.memq s r.result_sources) then begin
      s.source_files <- (r,index) :: s.source_files;
      r.result_sources <- s :: r.result_sources
    end
    
let add_download file s index =
  let r = file.file_result in
  add_source r s index;
  if not (List.memq file s.source_downloads) then begin
      s.source_downloads <- file :: s.source_downloads;
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
  
