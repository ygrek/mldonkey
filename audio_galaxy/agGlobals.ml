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

open CommonServer
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open AgTypes
open AgOptions
  
module DO = CommonOptions
  
open CommonNetwork
  
let network = new_network "Audio Galaxy"
    network_options_prefix commit_in_subdir
  
let (file_ops : file CommonFile.file_ops) = CommonFile.new_file_ops network
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network
  
let server_connection_state = ref Not_connected
let server_sock = ref (None: TcpBufferedSocket.t option)
let message_counter = ref 0

let checked_file_transfer = ref false
  
    
let files_by_key = Hashtbl.create 127

let new_file file_id file_name file_size =
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key (file_name, file_size)
  with _ ->
      let file_temp = Filename.concat !!DO.temp_directory 
          (Printf.sprintf "AG-%s" (Md4.to_string file_id)) in
      let current_size = try
          Unix32.getsize32 file_temp
        with e ->
            Printf.printf "Exception %s in current_size" (Printexc2.to_string e); 
            print_newline ();
            Int32.zero
      in
      let rec file = {
          file_file = file_impl;
          file_hash = file_id;
          file_name = file_name;
          file_temp = file_temp;
          file_client = None;
        } and file_impl = {
          dummy_file_impl with
          impl_file_fd = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666;
          impl_file_size = file_size;
          impl_file_downloaded = current_size;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = BasicSocket.last_time ();          
          impl_file_best_name = file_name;
          }
        in
      file_add file_impl FileDownloading;
      Hashtbl.add files_by_key  key file;
      file

let (current_files: AgTypes.file list ref) = ref []
  
  
let file_state file =
  file_state (as_file file.file_file)
  
let file_num file =
  file_num (as_file file.file_file)

  (*
let server_num s =
  server_num (as_server s.server_server)
  
    
let server_state s =
  server_state (as_server s.server_server)
*)  
  
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file.file_file.impl_file_downloaded
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
