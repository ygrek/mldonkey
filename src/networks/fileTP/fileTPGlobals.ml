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
open TcpBufferedSocket

open CommonOptions
open CommonClient
open CommonUser
open CommonTypes
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonFile
open CommonGlobals
open CommonDownloads
open CommonNetwork

open FileTPTypes
open FileTPOptions

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[FileTP] "
    (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[FileTP] "
    (log_time ()); lprintf

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

let find_proto (name : string) =
  (Hashtbl.find protos_by_name name : tp_proto)

(***************************************************************

             HOST SCHEDULER

****************************************************************)

let min_range_size = megabyte

let set_file_size file size =
  if file_size file = zero && size <> zero then begin
      let file_chunk_size =
        max megabyte (
          size // (max 5L (size // (megabytes 5)))
        )
      in
      file.file_file.impl_file_size <- size;
      let file_temp = Unix32.filename (file_fd file) in
      let kernel = Int64Swarmer.create_swarmer file_temp size min_range_size in
      let swarmer = Int64Swarmer.create kernel (as_file file)
          file_chunk_size in
      file.file_swarmer <- Some swarmer;
      Int64Swarmer.set_verified swarmer (fun _ _ ->
          file_must_update (as_file file);
      );
      (*
      Int64Swarmer.set_writer swarmer (fun offset s pos len ->

(*
      lprintf "DOWNLOADED: %d/%d/%d\n" pos len (String.length s);
      AnyEndian.dump_sub s pos len;
*)

          if !!CommonOptions.buffer_writes then
            Unix32.buffered_write_copy (file_fd file) offset s pos len
          else
            Unix32.write  (file_fd file) offset s pos len
      ) *)
    end

let new_file file_id file_name file_size =
  let file_temp = Filename.concat !!temp_directory
      (Printf.sprintf "FileTP-%s" (Md4.to_string file_id)) in
  let t = Unix32.create_rw file_temp in
  let rec file = {
      file_file = file_impl;
      file_id = file_id;
(*      file_name = file_name; *)
      file_clients = [];
      file_swarmer = None;
      file_filenames = [Filename.basename file_name, GuiTypes.noips()];
      file_clients_queue = Queues.workflow (fun _ -> false);
      file_nconnected_clients = 0;
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = Some t;
      impl_file_size = zero;
      impl_file_downloaded = zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();
      impl_file_best_name = Filename.basename file_name;
    }
  in
  set_file_size file file_size;
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file

exception FileFound of file

let new_file file_id file_name file_size =
  try
    Hashtbl.find files_by_uid file_id
  with _ ->
      let file = new_file file_id file_name file_size  in
      Hashtbl.add files_by_uid file_id file;
      file

let new_client proto hostname port =
  let key = (hostname,port) in
  try
    Hashtbl.find clients_by_uid key
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
(*          client_name = name;
          client_kind = None; *)
          client_requests = [];
(*
client_pos = Int32.zero;
client_error = false;
  *)

          client_connection_control = new_connection_control (());
          client_downloads = [];
          client_hostname = hostname;
          client_port = port;
          client_reconnect = false;
          client_in_queues = [];
          client_connected_for = None;
          client_proto = proto;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
	  impl_client_upload = None;
        } in
      new_client impl;
      Hashtbl.add clients_by_uid key c;
      c
    
let add_download file c url referer =
(*  let r = new_result file.file_name (file_size file) in *)
(*  add_source r c.client_user index; *)
  if !verbose then lprintf "Adding file to client\n";
  if not (List.memq c file.file_clients) then begin
      let chunks = [ Int64.zero, file_size file ] in
      (*
      let bs = Int64Swarmer.register_uploader file.file_swarmer
        (Int64Swarmer.AvailableRanges chunks) in *)
      c.client_downloads <- c.client_downloads @ [{
          download_file = file;
          download_url = url;
          download_referer = referer;
          download_chunks = chunks;
          download_uploader = None;
          download_ranges = [];
          download_block = None;
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
      ft_client_name := String.sub name 0 (min 32 len);
      old_client_name := name;
      String2.replace_char !ft_client_name ' ' '_';
    end;
  !ft_client_name

let file_chunk_size = 307200
