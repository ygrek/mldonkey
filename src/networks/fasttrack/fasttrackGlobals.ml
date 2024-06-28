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

open Int64ops
open Queues
open Printf2
open Md4
open BasicSocket
open Options
open TcpBufferedSocket

open CommonInteractive
open CommonSwarming
open CommonHosts
open CommonOptions
open CommonClient
open CommonUser
open CommonTypes
open CommonServer
open CommonResult
open CommonFile
open CommonGlobals
open CommonNetwork

open FasttrackNetwork
open FasttrackTypes
open FasttrackOptions

let log_prefix = "[Fasttrack]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let search_num = ref 0

let should_update_shared_files = ref false

let network = new_network "FT" "Fasttrack"
         [
    NetworkHasSupernodes;
    NetworkHasRooms;
    NetworkHasChat;
    NetworkHasSearch;
  ]

let connection_manager = network.network_connection_manager

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
let file_state file =
  file_state (as_file file)
let file_num file =
  file_num (as_file file)
let file_must_update file =
  file_must_update (as_file file)
let server_num s =
  server_num (as_server s.server_server)
let server_must_update s =
  server_must_update (as_server s.server_server)
let server_state s =
  server_state (as_server s.server_server)
let set_server_state s state =
  set_server_state (as_server s.server_server) state
let client_type c = client_type (as_client c)
let set_client_state client state =
  CommonClient.set_client_state (as_client client) state
let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client)
let client_must_update client = client_must_update (as_client client)

(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)

let nservers = ref 0
let ready _ = false
let hosts_counter = ref 0
let old_client_name = ref ""
let ft_client_name = ref ""
let file_chunk_size = 307200

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let current_files = ref ([] : FasttrackTypes.file list)
let listen_sock = ref (None : TcpServerSocket.t option)
let udp_sock = ref (None: UdpSocket.t option)
let result_sources = Hashtbl.create 1011
(* let hosts_by_key = Hashtbl.create 103 *)
let (searches_by_uid : (int, local_search) Hashtbl.t) = Hashtbl.create 11
let files_by_uid = Hashtbl.create 13
let (users_by_uid ) = Hashtbl.create 127
let (clients_by_uid ) = Hashtbl.create 127
let results_by_uid = Hashtbl.create 127
let connected_servers = ref ([] : server list)

  (*
let (workflow : host Queue.t) =
  Queues.workflow (fun time -> time + 120 > last_time ())
  *)

(* From the main workflow, hosts are moved to these workflows when they
are ready to be connected. *)
let (ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready

(* peers are only tested when no ultrapeers are available... *)
let (peers_waiting_queue : host Queue.t) = Queues.workflow ready

(* These are the peers that we should try to contact by UDP *)
let (waiting_udp_queue : host Queue.t) = Queues.workflow ready

(* These are the peers that have replied to our UDP requests *)
let (active_udp_queue : host Queue.t) = Queues.fifo ()

(*************************************************************************)
(*                                                                       *)
(*                         Global functions                              *)
(*                                                                       *)
(*************************************************************************)

module H = CommonHosts.Make(struct
      include FasttrackTypes
      type ip = Ip.addr

      let requests =
        [
          Tcp_Connect,
          (600, (fun kind ->
                [ match kind with
                  | Ultrapeer -> ultrapeers_waiting_queue
                  | (_) -> peers_waiting_queue
                ]
            ));
          Udp_Connect,
          (600, (fun kind ->
                   [waiting_udp_queue]
            ))]

      let default_requests kind = [Tcp_Connect,0; Udp_Connect,0]

      let max_ultrapeers = max_known_ultrapeers
      let max_peers = max_known_peers
    end)

let check_server_country_code s =
  if Geoip.active () then
    match s.server_country_code with
    | None ->
        s.server_country_code <-
          Geoip.get_country_code_option (Ip.ip_of_addr s.server_host.host_addr)
    | _ -> ()

let new_server ip port =
  let h = H.new_host ip port Ultrapeer in
  match h.host_server with
    Some s -> s
  | None ->
      let rec s = {
          server_server = server_impl;
          server_host = h;
          server_country_code = None;
          server_sock = NoConnection;
          server_ciphers = None;
          server_agent = "<unknown>";
          server_description = "";
          server_nfiles = Int64.zero;
          server_nusers = Int64.zero;
          server_maxnusers = 0L;
          server_nkb = 0;

          server_need_qrt = true;
          server_ping_last = Md4.random ();
          server_nfiles_last = zero;
          server_nkb_last = 0;
          server_vendor = "";
          server_last_lni = 0;

          server_connected = zero;
          server_query_key = ();
          server_searches = Fifo.create ();
          server_shared = Intset.empty;
        } and
        server_impl = {
          dummy_server_impl with
          impl_server_val = s;
          impl_server_ops = server_ops;
        } in
      server_add server_impl;
      h.host_server <- Some s;
      check_server_country_code s;
      s

let add_source r (user : user) =
  let ss =
    try
      Hashtbl.find result_sources r.stored_result_num
    with _ ->
        let ss = ref [] in
        Hashtbl.add result_sources r.stored_result_num ss;
        ss
  in
  if not (List.mem_assq user !ss) then begin
      ss := (user, last_time ()) :: !ss
    end

let new_result file_name file_size tags hashes _ =
 
  match hashes with
  | [ hash ] ->
      let r =
        try
          let r = Hashtbl.find results_by_uid hash in
          increment_avail r
        with _ ->
            let tags = update_or_create_avail tags in
            let r = { dummy_result with
                result_names = [file_name];
                result_size = file_size;
                result_tags = tags;
                result_uids = [Uid.create (Md5Ext hash)];
                result_source_network = network.network_num;
              }
            in
            let r = update_result_num r in
            Hashtbl.add results_by_uid hash r;
            r
      in
      r
  | _ -> assert false

let min_range_size = megabyte

let new_file file_temporary file_name file_size file_hash user group =
  let file_temp = Filename.concat !!temp_directory file_temporary in
(*      (Printf.sprintf "FT-%s" (Md4.to_string file_id)) in *)
  let t = Unix32.create_rw file_temp in
  let file_chunk_size =
    max megabyte (
      1L ++ file_size // (max 5L (1L ++ file_size // (megabytes 5)))
    )
  in
  let uid = Uid.create (Md5Ext file_hash) in
  let rec file = {
      file_file = file_impl;
      file_temp = file_temporary;
      file_name = file_name;
      file_clients = [];
      file_swarmer = None;
      file_searches = [search];
      file_uids = [uid];
      file_clients_queue = Queues.workflow (fun _ -> false);
      file_nconnected_clients = 0;
      file_ttr = None;
    } and file_impl =  {
      (dummy_file_impl ()) with
      impl_file_fd = Some t;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_owner = user;
      impl_file_group = group;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();
      impl_file_best_name = file_name;
      impl_file_filenames = [file_name];
    } and search = {
      search_search = FileUidSearch (file, file_hash);
      search_uid = !search_num;
      search_hosts = Intset.empty;
    }
  in
  incr search_num;
  let kernel = CommonSwarming.create_swarmer file_temp file_size in
  let swarmer = CommonSwarming.create kernel (as_file file)
      file_chunk_size in
  file.file_swarmer <- Some swarmer;
  Hashtbl.add searches_by_uid search.search_uid search;
(*  lprintf "SET SIZE : %Ld\n" file_size;*)
  CommonSwarming.set_verifier swarmer ForceVerification;
  CommonSwarming.set_verified swarmer (fun _ _ ->
      file_must_update file;
  );
  (*
  CommonSwarming.set_writer swarmer (fun offset s pos len ->

      (*
      lprintf "DOWNLOADED: %d/%d/%d\n" pos len (String.length s);
      AnyEndian.dump_sub s pos len;
*)

      if !!CommonOptions.buffer_writes then
        Unix32.buffered_write_copy t offset s pos len
      else
        Unix32.write  t offset s pos len
  ); *)
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file

exception FileFound of file

let new_file file_id file_name file_size file_uids user group =
  let file = ref None in
  List.iter (fun uid ->
      match Uid.to_uid uid with
        Md5Ext file_hash ->
          file := Some (try
              Hashtbl.find files_by_uid file_hash
            with _ ->
                let file = new_file file_id file_name file_size file_hash user group in
                Hashtbl.add files_by_uid file_hash file;
                file)
      | _ -> ()
  ) file_uids;
  match !file with
    None -> assert false
  | Some file -> file

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
            | Indirect_location (_, uid, _, _) -> uid);
          user_kind = kind;
(*          user_files = []; *)
          user_speed = 0;
          user_vendor = "";
          user_software = "";
          user_nick = "";
        }  and user_impl = {
          dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
          } in
      user_add user_impl;
      Hashtbl.add users_by_uid kind user;
      user

let check_client_country_code c =
  if Geoip.active () then
    match c.client_country_code with
    | None ->
        (match c.client_host with
        | Some (ip,port) -> c.client_country_code <- Geoip.get_country_code_option ip
        | _ -> ())
    | _ -> ()

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
          client_country_code = None;
          client_reconnect = false;
          client_in_queues = [];
          client_connected_for = None;
          client_support_head_request = true;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
    impl_client_upload = None;
        } in
      new_client impl;
      Hashtbl.add clients_by_uid kind c;
      c

let add_download file c () =
(*  let r = new_result file.file_name (file_size file) in *)
(*  add_source r c.client_user index; *)
  if !verbose then lprintf "Adding file to client\n";
  if not (List.memq c file.file_clients) then begin
      let chunks = [ Int64.zero, file_size file ] in
      let d = {
          download_file = file;
(*          download_uri = index; *)
          download_chunks = chunks;
          download_uploader = None;
          download_ranges = [];
          download_blocks = [];
          download_uri = "";
          download_head_requested = false;
          download_ttr_requested = false;
        } in
      c.client_downloads <- c.client_downloads @ [d];
      List.iter (fun uid ->
          match Uid.to_uid uid with
            Md5Ext hash -> d.download_uri <- Md5Ext.to_hexa_case false hash
          | _ -> ()
      ) file.file_uids;
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

let remove_file file =
  List.iter (fun uid ->
      match Uid.to_uid uid with
        Md5Ext hash ->  Hashtbl.remove files_by_uid hash
      | _ -> ()
  ) file.file_uids;
  current_files := List2.removeq file !current_files

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
            let connection_time = Int64.to_int (
                (int64_time ()) -- s.server_connected) in
              if !verbose then lprintf "DISCONNECT FROM SERVER %s:%d after %d seconds [%s]\n"
              (Ip.string_of_addr h.host_addr) h.host_port
              connection_time
            (string_of_reason reason)
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

let client_name () =

  let name = !!global_login in
  if name != !old_client_name then  begin
      let len = String.length name in
      old_client_name := name;
      let name' = String.sub name 0 (min 32 len) in
      ft_client_name := String2.replace_char name' ' ' '_';
    end;
  !ft_client_name

(*************************************************************
 Define a function to be called when the "mem_stats" command
   is used to display information on structure footprint.
**************************************************************)

let _ =
  Heap.add_memstat "FasttrackGlobals" (fun level buf ->
     Printf.bprintf buf "Number of old files: %d\n" (List.length !!old_files))
