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
open CommonSwarming  
open CommonNetwork
  
open FasttrackTypes
open FasttrackOptions

let search_num = ref 0
                  
let extension_list = [
    "mp3" ; "avi" ; "jpg" ; "jpeg" ; "txt" ; "mov" ; "mpg" 
]
      
let rec remove_short list list2 =
  match list with
    [] -> List.rev list2
  | s :: list -> 
      if List.mem s extension_list then 
        remove_short list (s :: list2) else 
      
      if String.length s < 5 then (* keywords should had list be 5 bytes *)
        remove_short list list2
      else
        remove_short list (s :: list2)

let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' '
  done;
  lprintf "STEM %s\n" s;
  remove_short (String2.split s ' ') []

let get_name_keywords file_name =
  match stem file_name with 
    [] | [_] -> 
      lprintf "Not enough keywords to recover %s\n" file_name;
      [file_name]
  | l -> l

  
  
let network = new_network "Fasttrack"  
         [ 
    NetworkHasSupernodes; 
    NetworkHasRooms;
    NetworkHasChat;
    NetworkHasSearch;
  ]
    (fun _ -> !!network_options_prefix)
  (fun _ -> !!commit_in_subdir)
      
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
  
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

    
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)

let current_files = ref ([] : FasttrackTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let hosts_by_key = Hashtbl.create 103

let (searches_by_uid : (int, local_search) Hashtbl.t) = Hashtbl.create 11

  (*
let redirector_connected = ref false
(* let redirectors_ips = ref ( [] : Ip.t list) *)
let redirectors_to_try = ref ( [] : string list)
  *)

let files_by_uid = Hashtbl.create 13

let (users_by_uid ) = Hashtbl.create 127
let (clients_by_uid ) = Hashtbl.create 127
let results_by_uid = Hashtbl.create 127
  
(***************************************************************


             HOST SCHEDULER


****************************************************************)
  
  
  
(* Hosts are first injected in workflow. The workflow ensures that any
host object is inspected every two minutes. *)
let (workflow : host Queue.t) = 
  Queues.workflow (fun time -> time + 120 > last_time ())

let ready _ = false
  
(* From the main workflow, hosts are moved to these workflows when they
are ready to be connected. They will only be connected when connections
will be available. We separate g1/g2, and g0 (unknown kind). *)
let (g0_ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready
let (ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* peers are only tested when no ultrapeers are available... *)
let (g0_peers_waiting_queue : host Queue.t) = Queues.workflow ready
let (peers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that we should try to contact by UDP *)
let (waiting_udp_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that have replied to our UDP requests *)
let (active_udp_queue : host Queue.t) = Queues.fifo ()

let nservers = ref 0

let connected_servers = ref ([] : server list)
  
  

let host_queue_add q h time =
  if not (List.memq q h.host_queues) then begin
      Queue.put q (time, h);
      h.host_queues <- q :: h.host_queues
    end

let host_queue_take q =
  let (time,h) = Queue.take q in
  if List.memq q h.host_queues then begin
      h.host_queues <- List2.removeq q h.host_queues 
    end;
  h
      
let hosts_counter = ref 0
  
let new_host ip port ultrapeer = 
  let key = (ip,port) in
  try
    let h = Hashtbl.find hosts_by_key key in
    h.host_age <- last_time ();
    h
  with _ ->
      incr hosts_counter;
      let host = {
          host_num = !hosts_counter;
          host_server = None;
          host_addr = ip;
          host_port = port;
          
          host_age = last_time ();
          host_tcp_request = 0;
          host_udp_request = 0;
          host_connected = 0;
          
          host_kind = ultrapeer;
          host_queues = [];
        } in
      Hashtbl.add hosts_by_key key host;
      host_queue_add workflow host 0;
      host
      
let new_server ip port =
  let h = new_host ip port Ultrapeer in
  match h.host_server with
    Some s -> s
  | None ->
      let rec s = {
          server_server = server_impl;
          server_host = h;
          server_sock = NoConnection;
          server_ciphers = None;
          server_agent = "<unknown>";
          server_nfiles = 0;
          server_nkb = 0;
          
          server_need_qrt = true;
          server_ping_last = Md4.random ();
          server_nfiles_last = 0;
          server_nkb_last = 0;
          server_vendor = "";
          
          server_connected = Int32.zero;
          server_searches = Fifo.create ();
        } and
        server_impl = {
          dummy_server_impl with
          impl_server_val = s;
          impl_server_ops = server_ops;
        } in
      server_add server_impl;
      h.host_server <- Some s;
      s

let add_source r s index =
  let key = (s, index) in
  if not (List.mem key r.result_sources) then begin
      r.result_sources <- key :: r.result_sources
    end

let new_result file_name file_size tags hash =
  
  let r = 
    try
      Hashtbl.find results_by_uid hash
    with _ -> 
        let rec result = {
            result_result = result_impl;
            result_name = file_name;
            result_size = file_size;
            result_sources = [];
            result_tags = tags;
            result_hash = hash;
          } and
          result_impl = {
            dummy_result_impl with
            impl_result_val = result;
            impl_result_ops = result_ops;
          } in
        new_result result_impl;
        Hashtbl.add results_by_uid hash result;
        result
  in
  r
  
let megabyte = Int64.of_int (1024 * 1024)
      
let new_file file_id file_name file_size file_hash = 
  let file_temp = Filename.concat !!temp_directory 
      (Printf.sprintf "FT-%s" (Md4.to_string file_id)) in
  let t = Unix32.create_rw file_temp in
  let swarmer = Int64Swarmer.create () in
  let partition = fixed_partition swarmer megabyte in
    let keywords = get_name_keywords file_name in
  let words = String2.unsplit keywords ' ' in
  let rec file = {
      file_file = file_impl;
      file_id = file_id;
      file_name = file_name;
      file_clients = [];
      file_swarmer = swarmer;
      file_partition = partition;
      file_search = search;
      file_hash = file_hash;
      file_filenames = [file_name];
      file_clients_queue = Queues.workflow (fun _ -> false);
      file_nconnected_clients = 0;
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = t;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();          
      impl_file_best_name = file_name;
    } and search = {
      search_search = FileSearch file;
      search_id = !search_num;
    } 
  in
  incr search_num;
  Hashtbl.add searches_by_uid search.search_id search;
  lprintf "SET SIZE : %Ld\n" file_size;
  Int64Swarmer.set_size swarmer file_size;  
  Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
      
      (*
      lprintf "DOWNLOADED: %d/%d/%d\n" pos len (String.length s);
      AnyEndian.dump_sub s pos len;
*)
      
      if !!CommonOptions.buffer_writes then 
        Unix32.buffered_write_copy t offset s pos len
      else
        Unix32.write  t offset s pos len
  );
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file

exception FileFound of file
  
let new_file file_id file_name file_size file_hash =
  try
    Hashtbl.find files_by_uid file_hash 
  with _ ->
    let file = new_file file_id file_name file_size file_hash in
    file    
              
let new_user kind =
  try
    let s = Hashtbl.find users_by_uid kind in
    s.user_kind <- kind;
    s
  with _ ->
      let rec user = {
          user_user = user_impl;
          user_uid = (match kind with
              Known_location _ -> Md4.null
            | Indirect_location (_, uid) -> uid);
          user_kind = kind;
(*          user_files = []; *)
          user_speed = 0;
          user_vendor = "";
          user_gnutella2 = false;
          user_nick = "";
        }  and user_impl = {
          dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
          } in
      user_add user_impl;
      Hashtbl.add users_by_uid kind user;
      user
  
let new_client kind =
  try
    Hashtbl.find clients_by_uid kind 
  with _ ->
      let user = new_user kind in
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
  
          client_all_files = None;
          client_user = user;
          client_connection_control = new_connection_control (());
          client_downloads = [];
          client_host = None;
          client_reconnect = false;
          client_in_queues = [];
          client_connected_for = None;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
	  impl_client_upload = None;
        } in
      new_client impl;
      Hashtbl.add clients_by_uid kind c;
      c
    
let add_download file c index =
(*  let r = new_result file.file_name (file_size file) in *)
(*  add_source r c.client_user index; *)
  lprintf "Adding file to client\n";
  if not (List.memq c file.file_clients) then begin
      let chunks = [ Int64.zero, file_size file ] in
      let bs = Int64Swarmer.register_uploader file.file_partition chunks in
      c.client_downloads <- c.client_downloads @ [{
          download_file = file;
          download_uri = index;
          download_chunks = chunks;
          download_blocks = bs;
          download_ranges = [];
          download_block = None;
        }];
      file.file_clients <- c :: file.file_clients;
      file_add_source (as_file file.file_file) (as_client c.client_client);
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
      match d.download_uri with
        FileByIndex (i,_) when i = index -> d
      | _ -> find_download_by_index index tail

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

  (*
let server_remove s =
  connected_servers := List2.removeq s !connected_servers;    
(*  Hashtbl.remove servers_by_key (s.server_ip, s.server_port)*)
  ()
  *)

let client_type c = client_type (as_client c.client_client)

let set_client_state client state =
  CommonClient.set_client_state (as_client client.client_client) state
  
let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client.client_client) 
  
  
let remove_file file = 
  Hashtbl.remove files_by_uid file.file_hash;
  current_files := List2.removeq file !current_files  

let udp_sock = ref (None : UdpSocket.t option)

let client_ip sock =
  CommonOptions.client_ip
  (match sock with Connection sock -> Some sock | _ -> None)

let free_ciphers s =
  match s.server_ciphers with
    None -> ()
  | Some ciphers ->
      cipher_free ciphers.in_cipher;
      cipher_free ciphers.out_cipher;
      s.server_ciphers <- None
  
let disconnect_from_server nservers s reason =
  match s.server_sock with
  | Connection sock ->
      let h = s.server_host in
      (match server_state s with 
          Connected _ ->
            let connection_time = Int32.to_int (
                Int32.sub (int32_time ()) s.server_connected) in
            lprintf "DISCONNECT FROM SERVER %s:%d after %d seconds\n" 
              (Ip.string_of_addr h.host_addr) h.host_port
              connection_time 
            ;
        | _ -> ()
      );
      (try close sock reason with _ -> ());
      s.server_sock <- NoConnection;
      free_ciphers s;            
      set_server_state s (NotConnected (reason, -1));
      s.server_need_qrt <- true;
      decr nservers;
      if List.memq s !connected_servers then
        connected_servers := List2.removeq s !connected_servers
  | _ -> ()

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
  
  