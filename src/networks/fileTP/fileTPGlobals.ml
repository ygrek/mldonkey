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
open Int64ops
open Queues
open Printf2
open Md4
open BasicSocket
open Options

open CommonOptions
open CommonClient
open CommonTypes
open CommonFile
open CommonGlobals
open CommonNetwork

open FileTPTypes
open FileTPOptions

let log_prefix = "[FileTP]"

let lprintf_nl ?exn fmt =
  lprintf_nl2 ?exn log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let network = new_network "FTP" "FileTP"
    [ ]

let connection_manager = network.network_connection_manager

  (*
let (result_ops : result CommonResult.result_ops) =
  CommonResult.new_result_ops network
    *)

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

let as_client c = as_client c.client_client
let as_file file = as_file file.file_file
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file_fd (as_file file)
let file_disk_name file = file_disk_name (as_file file)
let file_best_name file = file_best_name (as_file file)

let current_files = ref ([] : FileTPTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)

  (*
let redirector_connected = ref false
(* let redirectors_ips = ref ( [] : Ip.t list) *)
let redirectors_to_try = ref ( [] : string list)
  *)

let files_by_uid = Hashtbl.create 13

let clients_by_uid = Hashtbl.create 127
let protos_by_name = Hashtbl.create 13


let _ =
Heap.add_memstat "FileTPGlobals" (fun level buf ->
      Printf.bprintf buf "  current_files: %d\n" (List.length !current_files);
      Printf.bprintf buf "  file_by_uid: %d\n" (Hashtbl.length files_by_uid);
      Printf.bprintf buf "  clients_by_uid: %d\n" (Hashtbl.length clients_by_uid);
      Printf.bprintf buf "  protos_by_name: %d\n" (Hashtbl.length protos_by_name);
  )

let find_proto (name : string) =
  (Hashtbl.find protos_by_name name : tp_proto)

(***************************************************************

             HOST SCHEDULER

****************************************************************)

(*
let min_range_size = megabyte
*)
let min_range_size file = 
  if !!chunk_size = 0 
    then (file_size file) 
    else Int64.of_int !!chunk_size

let set_file_size file size =
  if file_size file = 0L && size <> 0L then begin
      file.file_file.impl_file_size <- size;
      let file_chunk_size = min_range_size file in
      let file_temp = Unix32.filename (file_fd file) in
      let kernel = CommonSwarming.create_swarmer file_temp size in
      let swarmer = CommonSwarming.create kernel (as_file file) file_chunk_size in
      file.file_swarmer <- Some swarmer;
      CommonSwarming.set_verified swarmer (fun _ _ ->
          file_must_update (as_file file);
      );
      file_must_update (as_file file);
    end

let new_file file_id file_name file_size user group =
  let file_temp = Filename.concat !!temp_directory
      (Printf.sprintf "FileTP-%s" (Md4.to_string file_id)) in
  let t = Unix32.create_rw file_temp in
  let rec file = {
      file_file = file_impl;
      file_id = file_id;
(*      file_name = file_name; *)
      file_clients = [];
      file_swarmer = None;
      file_clients_queue = Queues.workflow (fun _ -> false);
      file_nconnected_clients = 0;
    } and file_impl =  {
      (dummy_file_impl ()) with
      impl_file_owner = user;
      impl_file_group = group;
      impl_file_fd = Some t;
      impl_file_size = zero;
      impl_file_downloaded = zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();
      impl_file_best_name = Filename.basename file_name;
      impl_file_filenames = [Filename.basename file_name];
    }
  in
  set_file_size file file_size;
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file

let new_file file_id file_name file_size user group =
  try
    Hashtbl.find files_by_uid file_id
  with _ ->
      let file = new_file file_id file_name file_size user group in
      Hashtbl.add files_by_uid file_id file;
      file

let check_client_country_code c =
  if Geoip.active () then
    match c.client_country_code with
    | None ->
        c.client_country_code <-
          Geoip.get_country_code_option (Ip.from_name c.client_hostname)
    | _ -> ()

let new_client proto hostname port referer =
  let key = (hostname,port) in
  try
    Hashtbl.find clients_by_uid key
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
          client_requests = [];
          client_connection_control = new_connection_control (());
          client_downloads = [];
          client_hostname = hostname;
          client_referer = referer;
          client_port = port;
          client_country_code = None;
          client_total_downloaded = zero;
          client_session_downloaded = zero;
          client_reconnect = false;
          client_in_queues = [];
          client_connected_for = None;
          client_proto = proto;
          client_software = "";
          client_failed_attempts = 0;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_upload = None;
        } in
      new_client impl;
      check_client_country_code c;
      Hashtbl.add clients_by_uid key c;
      c
    
let add_download file c url =
(*  let r = new_result file.file_name (file_size file) in *)
(*  add_source r c.client_user index; *)
  if not (List.memq c file.file_clients) then begin
      let chunks = [ Int64.zero, file_size file ] in
      (*
      let bs = CommonSwarming.register_uploader file.file_swarmer
        (CommonSwarming.AvailableRanges chunks) in *)
      c.client_downloads <- c.client_downloads @ [{
          download_file = file;
          download_url = url;
          download_chunks = chunks;
          download_uploader = None;
          download_ranges = [];
          download_blocks = [];
        }];
      file.file_clients <- c :: file.file_clients;
      file_add_source (as_file file) (as_client c);
      if not (List.memq file c.client_in_queues) then begin
          Queue.put file.file_clients_queue (0,c);
          c.client_in_queues <- file :: c.client_in_queues
        end;
    end

let rec find_download file list =
  match list with
    [] -> raise Not_found
  | d :: tail ->
      if d.download_file == file then d else find_download file tail


let rec find_download_by_index index list =
  match list with
    [] -> raise Not_found
  | d :: tail ->
      if d.download_url = index then
        d
      else
        find_download_by_index index tail

let remove_download file list =
  let rec iter file list rev =
    match list with
      [] -> List.rev rev
    | d :: tail ->
        if d.download_file == file then
          iter file tail rev else
          iter file tail (d :: rev)
  in
  iter file list []

let file_state file = file_state (as_file file)

let file_num file =  file_num (as_file file)

let file_must_update file =
  file_must_update (as_file file)

let client_type c = client_type (as_client c)

let set_client_state client state =
  CommonClient.set_client_state (as_client client) state

let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client)

let client_remove c =
  let key = (c.client_hostname, c.client_port) in
  Hashtbl.remove clients_by_uid key;
  CommonClient.client_remove (as_client c)

let remove_file file =
  Hashtbl.remove files_by_uid file.file_id;
  current_files := List2.removeq file !current_files

let udp_sock = ref (None : UdpSocket.t option)

let client_ip sock =
  CommonOptions.client_ip
  (match sock with Connection sock -> Some sock | _ -> None)

let old_client_name = ref ""
let ft_client_name = ref ""

let client_name () =
  let name = !!global_login in
  if name != !old_client_name then  begin
      let len = String.length name in
      old_client_name := name;
      let name' = String.sub name 0 (min 32 len) in
      ft_client_name := String2.replace_char name' ' ' '_';
    end;
  !ft_client_name

