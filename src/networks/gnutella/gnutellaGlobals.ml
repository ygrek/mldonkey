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
  
open GnutellaTypes
open GnutellaOptions

                  
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

  
  
let network = new_network "Gnutella"  
      [ 
    NetworkHasSupernodes; 
    NetworkHasSearch;
    NetworkHasUpload;
    NetworkHasMultinet;
  ] (fun _ -> !!network_options_prefix)
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

let current_files = ref ([] : GnutellaTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let hosts_by_key = Hashtbl.create 103

let (searches_by_uid : (Md4.t, local_search) Hashtbl.t) = Hashtbl.create 11

  (*
let redirector_connected = ref false
(* let redirectors_ips = ref ( [] : Ip.t list) *)
let redirectors_to_try = ref ( [] : string list)
  *)

let files_by_uid = Hashtbl.create 13
let files_by_key = Hashtbl.create 13

let (users_by_uid ) = Hashtbl.create 127
let (clients_by_uid ) = Hashtbl.create 127
let results_by_key = Hashtbl.create 127
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
let (g1_ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready
let (g2_ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* peers are only tested when no ultrapeers are available... *)
let (g0_peers_waiting_queue : host Queue.t) = Queues.workflow ready
let (g1_peers_waiting_queue : host Queue.t) = Queues.workflow ready
let (g2_peers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that we should try to contact by UDP *)
let (g1_waiting_udp_queue : host Queue.t) = Queues.workflow ready
let (g2_waiting_udp_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that have replied to our UDP requests *)
let (g1_active_udp_queue : host Queue.t) = Queues.fifo ()
let (g2_active_udp_queue : host Queue.t) = Queues.fifo ()

let g1_nservers = ref 0
let g2_nservers = ref 0

let g1_connected_servers = ref ([] : server list)
let g2_connected_servers = ref ([] : server list)
  
  

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
  
let new_host ip port ultrapeer kind = 
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
          host_ip = ip;
          host_port = port;
          
          host_age = last_time ();
          host_tcp_request = 0;
          host_udp_request = 0;
          host_connected = 0;
          
          host_kind = kind;
          host_ultrapeer = ultrapeer;
          host_queues = [];
        } in
      Hashtbl.add hosts_by_key key host;
      host_queue_add workflow host 0;
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
          server_nusers = 0;
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

let extract_uids arg = expand_uids [uid_of_string arg]
  
let add_source r s index =
  let key = (s, index) in
  if not (List.mem key r.result_sources) then begin
      r.result_sources <- key :: r.result_sources
    end

let new_result file_name file_size tags uids =
  match uids with
    [] -> (
(*        lprintf "New result by key\n"; *)
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
                result_tags = tags;
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
(*      lprintf "New result by UID\n"; *)
      let r = 
        try
          Hashtbl.find results_by_uid uid
        with _ -> 
            let rec result = {
                result_result = result_impl;
                result_name = file_name;
                result_size = file_size;
                result_sources = [];
                result_tags = tags;
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
  let file_temp = Filename.concat !!temp_directory 
      (Printf.sprintf "GNUT-%s" (Md4.to_string file_id)) in
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
      file_uids = [];
      file_swarmer = swarmer;
      file_partition = partition;
      file_searches = [search];
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
      search_search = FileWordSearch (file, words);
      search_hosts = Intset.empty;
      search_uid = Md4.random ();
    } 
  in
  Hashtbl.add searches_by_uid search.search_uid search;
  lprintf "SET SIZE : %Ld\n" file_size;
  Int64Swarmer.set_size swarmer file_size;  
  Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
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
  if file.file_uids = [] then
    Hashtbl.remove files_by_key (file.file_name, file.file_file.impl_file_size)
  else
    List.iter (fun uid ->
        Hashtbl.remove files_by_uid uid
    ) file.file_uids;
  current_files := List2.removeq file !current_files  

let udp_sock = ref (None : UdpSocket.t option)

let client_ip sock =
  CommonOptions.client_ip
  (match sock with Connection sock -> Some sock | _ -> None)

let disconnect_from_server nservers s r =
  match s.server_sock with
  | Connection sock ->
      let h = s.server_host in
      (match server_state s with 
          Connected _ ->
            let connection_time = Int32.to_int (
                Int32.sub (int32_time ()) s.server_connected) in
            lprintf "DISCONNECT FROM SERVER %s:%d after %d seconds\n" 
              (Ip.to_string h.host_ip) h.host_port
              connection_time
            ;
        | _ -> ()
      );
      (try close sock r with _ -> ());
      s.server_sock <- NoConnection;
      set_server_state s (NotConnected (r, -1));
      s.server_need_qrt <- true;
      decr nservers;
      if s.server_gnutella2 then
        (      
          if List.memq s !g2_connected_servers then
            g2_connected_servers := List2.removeq s !g2_connected_servers)
      else
        (
          if List.memq s !g1_connected_servers then
            g1_connected_servers := List2.removeq s !g1_connected_servers)
  | _ -> ()


      (*
let parse_magnet url =
  let url = Url.of_string url in
  if url.Url.file = "magnet:" then 
    let uids = ref [] in
    let name = ref "" in
    List.iter (fun (value, arg) ->
        if String2.starts_with value "xt" then
          uids := (extract_uids arg) @ !uids
        else 
        if String2.starts_with value "dn" then
          name := Url.decode arg
        else 
        if arg = "" then
(* This is an error in the magnet, where a & has been kept instead of being
  url-encoded *)
          name := Printf.sprintf "%s&%s" !name value
        else
          lprintf "MAGNET: unused field %s = %s\n"
            value arg
    ) url.Url.args;
    !name, !uids
  else raise Not_found
*)    
    
let clean_file s =
  String2.replace_char s '\r' '\n';
  String2.replace_char s ' ' '\n'

let local_login () =
  let name = !!global_login in
  let len = String.length name in
  if len > 32 then name else  String.sub name 0 32
    