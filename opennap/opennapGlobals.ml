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

let current_servers = ref ([] : server list)
let connected_servers = ref ([] : server list)
let servers_list = ref ([] : server list)
let servers_by_addr = Hashtbl.create 127
let nservers = ref 0  
  
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
          server_sock = None;
          server_nusers = 0;
          server_nfiles = 0;
          server_size = 0;
          server_connection_control = DG.new_connection_control (last_time ());
          server_nick_num = -1;
          server_last_nick = "";
          server_searches = None;
          server_sources = [];
          server_pending_searches = [];
        } and 
        server_impl = {
          impl_server_update = false;
          impl_server_state = NewHost;
          impl_server_sort = 0.0;
          impl_server_val = s;
          impl_server_ops = server_ops;
          impl_server_num = 0;
        } 
      in
      server_add server_impl;
      servers_list := s :: !servers_list;
      current_servers := s :: !current_servers;
      Hashtbl.add servers_by_addr key s;
      s
      
let set_server_state s state =
  set_server_state (as_server s.server_server) state

  
let files_by_key = Hashtbl.create 13

let sources_by_uid = Hashtbl.create 127
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
          result_info = [];
          result_sources = [];
        } and 
        result_impl = {
          impl_result_val = result;
          impl_result_ops = result_ops;
          impl_result_num = 0;
        } in
      new_result result_impl;
      Hashtbl.add results_by_file file result;
      result

let find_result file_name file_size =
  let file = {
      file_name = file_name;
      file_size = file_size;
    } in
  Hashtbl.find results_by_file file
      
  
  
let new_file file_id file_name file_size =
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key key  
  with _ ->
      let file_temp = Filename.concat !!DO.temp_directory 
          (Printf.sprintf "ON-%s" (Md4.to_string file_id)) in
      let current_size = try
          Unix32.getsize32 file_temp
        with e ->
            Printf.printf "Exception %s in current_size" (Printexc.to_string e); 
            print_newline ();
            Int32.zero
      in
      
      let rec download = {
          file_file = file_impl;
          file_id = file_id;
          file_result = new_result file_name file_size;
          file_downloaded = current_size;
          file_temp = file_temp;
          file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
        } and file_impl = {
          impl_file_update = false;
          impl_file_state = FileNew;
          impl_file_num = 0; 
          impl_file_ops = file_ops;
          impl_file_val = download; 
        }
      in
      file_add file_impl FileDownloading;
      Hashtbl.add files_by_key key download;
      download
  
let find_file file_name file_size =
  let key = (file_name, file_size) in
    Hashtbl.find files_by_key key  


let new_source server nick ip =
  let uid = (server.server_server.impl_server_num, nick) in
  try
    let s = Hashtbl.find sources_by_uid uid in
    s.source_ip <- ip;
    s
  with _ ->
      let src = {
          source_nick = nick;
          source_server = server;
          source_uid = uid;
          source_ip = ip;
          source_port = None;
          source_files = [];
          source_link = LinkUnknown;
          source_client = None;
          source_downloads = [];
          source_connection_control = 
          new_connection_control (last_time ());
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

let (current_files : OpennapTypes.file list ref) = ref []
  
let file_state file =
  file_state (as_file file.file_file)
  
let server_num s =
  server_num (as_server s.server_server)
  
    
let server_state s =
  server_state (as_server s.server_server)
  
  