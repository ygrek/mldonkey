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
open GnutellaTypes
open GnutellaOptions
open CommonSwarming  
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
  
let network = new_network "Gnutella"  
    (fun _ -> !!network_options_prefix)
  (fun _ -> !!commit_in_subdir)
(*  network_options_prefix commit_in_subdir *)
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
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)
    
module DO = CommonOptions

let current_files = ref ([] : GnutellaTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let connected_servers = ref ([] : server list)
let hosts_by_key = Hashtbl.create 103

let (searches_by_uid : (Md4.t, local_search) Hashtbl.t) = Hashtbl.create 11
let (peers_waiting_queue : (host * int) Fifo.t) = Fifo.create ()
let (ultrapeers_waiting_queue : (host * int) Fifo.t) = Fifo.create ()
let (ultrapeers_old_queue : (host * int) Fifo.t) = Fifo.create ()
let (ultrapeers_recent_queue : (host * int) Fifo.t) = Fifo.create ()
let nservers = ref 0
let redirector_connected = ref false

(* let redirectors_ips = ref ( [] : Ip.t list) *)
let redirectors_to_try = ref ( [] : string list)
  
let files_by_uid = Hashtbl.create 13
let files_by_key = Hashtbl.create 13

let (users_by_uid ) = Hashtbl.create 127
let (clients_by_uid ) = Hashtbl.create 127
let results_by_key = Hashtbl.create 127
let results_by_uid = Hashtbl.create 127

let host_queue_add q h time =
  if not (List.memq q h.host_queues) then begin
      Fifo.put q (h, time);
      h.host_queues <- q :: h.host_queues
    end

let queue_take q =
  let (h,time) = Fifo.take q in
  if List.memq q h.host_queues then begin
      h.host_queues <- List2.removeq q h.host_queues 
    end
    
  
let new_host ip port ultrapeer kind = 
  let key = (ip,port) in
  try
    let h = Hashtbl.find hosts_by_key key in
    h.host_age <- last_time ();
    h
  with _ ->
      let host = {
          host_server = None;
          host_ip = ip;
          host_port = port;
          host_age = last_time ();
          host_kind = kind;
          host_queues = [];
        } in
      Hashtbl.add hosts_by_key key host;
      if ultrapeer then begin
          host_queue_add ultrapeers_waiting_queue host 0;
(*
      while Fifo.length ultrapeers_waiting_queue > !!max_known_ultrapeers do
        ignore (queue_take ultrapeers_waiting_queue)
      done;
*)
        end else begin
          host_queue_add peers_waiting_queue host 0;
          while Fifo.length peers_waiting_queue > !!max_known_peers do
            ignore (queue_take peers_waiting_queue)
          done;        
        end;
      host
      
let new_server ip port =
  let h = new_host ip port true 0 in
  match h.host_server with
    Some s -> s
  | None ->
      let rec s = {
          server_server = server_impl;
          server_host = h;
          server_sock = NoConnection;
          server_agent = "<unknown>";
          server_nfiles = 0;
          server_nkb = 0;
          
          server_need_qrt = true;
          server_ping_last = Md4.random ();
          server_nfiles_last = 0;
          server_nkb_last = 0;
          server_vendor = "";
          
          server_connected = Int32.zero;
          server_gnutella2 = false;
          server_query_key = NoUdpSupport;
        } and
        server_impl = {
          dummy_server_impl with
          impl_server_val = s;
          impl_server_ops = server_ops;
        } in
      server_add server_impl;
      h.host_server <- Some s;
      s
      
let extract_uids arg = 
  match String2.split (String.lowercase arg) ':' with
  | "urn" :: "sha1" :: sha1_s :: _ ->
      let sha1 = Sha1.of_string sha1_s in
      let sha1_s = Sha1.to_string sha1 in
      [Sha1 (Printf.sprintf "urn:sha1:%s" sha1_s, sha1) ]
  | "urn" :: "bitprint" :: bitprint :: _ ->
      let sha1_s = String.sub bitprint 0 32 in
      let sha1 = Sha1.of_string sha1_s in
      let sha1_s = Sha1.to_string sha1 in
      let tiger_s = String.sub bitprint 33 39 in
      let tiger = Tiger.of_string tiger_s in
      let tiger_s = Tiger.to_string tiger in
      [ Sha1 (Printf.sprintf "urn:sha1:%s" sha1_s, sha1);
        Bitprint (
          Printf.sprintf "urn:bitprint:%s.%s" sha1_s tiger_s,
          sha1, tiger)]
| _ -> []

let add_source r s index =
  let key = (s, index) in
  if not (List.mem key r.result_sources) then begin
      r.result_sources <- key :: r.result_sources
    end

let new_result file_name file_size uids =
  match uids with
    [] -> (
        lprintf "New result by key\n";
        let key = (file_name, file_size) in
        try
          Hashtbl.find results_by_key key
        with _ ->
            let rec result = {
                result_result = result_impl;
                result_name = file_name;
                result_size = file_size;
                result_sources = [];
                result_uids = [];
              } and
              result_impl = {
                dummy_result_impl with
                impl_result_val = result;
                impl_result_ops = result_ops;
              } in
            new_result result_impl;
            Hashtbl.add results_by_key key result;
            result)
  | uid :: other_uids ->
      lprintf "New result by UID\n";
      let r = 
        try
          Hashtbl.find results_by_uid uid
        with _ -> 
            let rec result = {
                result_result = result_impl;
                result_name = file_name;
                result_size = file_size;
                result_sources = [];
                result_uids = [uid];
              } and
              result_impl = {
                dummy_result_impl with
                impl_result_val = result;
                impl_result_ops = result_ops;
              } in
            new_result result_impl;
            Hashtbl.add results_by_uid uid result;
            result
      in
      let rec iter_uid uid =
        if not (List.mem uid r.result_uids) then begin
            r.result_uids <- uid :: r.result_uids;
            (try
                let rr = Hashtbl.find results_by_uid uid in
                if r != rr then 
                  let result_uids = rr.result_uids in
                  rr.result_uids <- [];
                  List.iter (fun uid -> 
                      Hashtbl.remove results_by_uid uid) result_uids;
                  List.iter (fun uid -> iter_uid uid) result_uids;
                  List.iter (fun (s, index) ->
                      add_source r s index
                  ) rr.result_sources;
                  rr.result_sources <- [];
              with _ -> ());
            
            Hashtbl.add results_by_uid uid r;
          end
      in
      List.iter iter_uid other_uids;
      r

let megabyte = Int64.of_int (1024 * 1024)
      
let new_file file_id file_name file_size = 
  let file_temp = Filename.concat !!DO.temp_directory 
      (Printf.sprintf "GNUT-%s" (Md4.to_string file_id)) in
  let t = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
  let swarmer = Int64Swarmer.create () in
  let partition = fixed_partition swarmer megabyte in
  let rec file = {
      file_file = file_impl;
      file_id = file_id;
      file_name = file_name;
      file_clients = [];
      file_uids = [];
      file_swarmer = swarmer;
      file_partition = partition;
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = t;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();          
      impl_file_best_name = file_name;
    }
  in
  lprintf "SET SIZE : %Ld\n" file_size;
  Int64Swarmer.set_size swarmer file_size;  
  Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
      if !!CommonOptions.buffer_writes then 
        Unix32.buffered_write t offset s pos len
      else
        Unix32.write  t offset s pos len
  );
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file

exception FileFound of file
  
let new_file file_id file_name file_size file_uids =
  if file_uids = [] then 
    try Hashtbl.find files_by_key (file_name, file_size) with
      _ -> 
        let file = new_file file_id file_name file_size in
        Hashtbl.add files_by_key (file_name, file_size) file;
        file
  else
  try
    List.iter (fun uid ->
        try  raise (FileFound (Hashtbl.find files_by_uid uid))
        with Not_found -> ()
    ) file_uids;
    let file = new_file file_id file_name file_size in
    file.file_uids <- file_uids;
    List.iter (fun uid -> Hashtbl.add files_by_uid uid file) file_uids;
    file    
  with FileFound file ->
      List.iter (fun uid ->
          if not (List.mem uid file.file_uids) then begin
              file.file_uids <- uid :: file.file_uids;
              Hashtbl.add files_by_uid uid file;
            end
      ) file_uids;
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
      file_add_source (as_file file.file_file) (as_client c.client_client)
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
  
let server_remove s =
  connected_servers := List2.removeq s !connected_servers;    
(*  Hashtbl.remove servers_by_key (s.server_ip, s.server_port)*)
  ()

let client_type c = client_type (as_client c.client_client)

let set_client_state client state =
  CommonClient.set_client_state (as_client client.client_client) state
  
let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client.client_client) 
  
  
let remove_file file = 
  if file.file_uids = [] then
    Hashtbl.remove files_by_key (file.file_name, file.file_file.impl_file_size)
  else
    List.iter (fun uid ->
        Hashtbl.remove files_by_uid uid
    ) file.file_uids;
  current_files := List2.removeq file !current_files  

let udp_sock = ref (None : UdpSocket.t option)

let verbose_udp = ref true

let client_ip sock =
  CommonOptions.client_ip
  (match sock with Connection sock -> Some sock | _ -> None)
  
  