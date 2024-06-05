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
open TcpBufferedSocket
open Printf2
open Md4
open Options
open BasicSocket

open CommonSwarming
open CommonInteractive
open CommonFile
open CommonServer
open CommonClient
open CommonTypes
open CommonGlobals

open DonkeyTypes
open DonkeyOptions
open CommonOptions

let log_prefix = "[EDK]"

let lprintf_nl ?exn fmt =
  lprintf_nl2 ?exn log_prefix fmt

(*************************************************************

Define the instances of the plugin classes, that we be filled
later with functions defining the specialized methods for this
plugin.

**************************************************************)

let network = CommonNetwork.new_network "ED2K" "Donkey"
  ~comment:(if Autoconf.donkey_sui_works () then "SUI" else "noSUI")
  [
    NetworkHasServers;
    NetworkHasSearch;
    NetworkHasUpload;
    NetworkHasMultinet;
    NetworkHasChat;
    NetworkHasStats;
  ]

let connection_manager = network.network_connection_manager
let connections_controler = TcpServerSocket.create_connections_controler
    "Edonkey" (fun _ _ -> true)

let (shared_ops : file CommonShared.shared_ops) =
  CommonShared.new_shared_ops network

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

let (pre_shared_ops : file_to_share CommonShared.shared_ops) =
  CommonShared.new_shared_ops network

let (shared_ops : file CommonShared.shared_ops) =
  CommonShared.new_shared_ops network

let client_must_update c =
  client_must_update (as_client c.client_client)

let server_must_update s =
  server_must_update (as_server s.server_server)

let as_client c = as_client c.client_client
let as_file file = as_file file.file_file
let file_priority file = file.file_file.impl_file_priority
let file_size file = file.file_file.impl_file_size
let file_is_largefile f = file_size f > old_max_emule_file_size
let file_downloaded file = file_downloaded (as_file file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file_fd (as_file file)
let file_disk_name file = file_disk_name (as_file file)
let file_best_name file = file_best_name (as_file file)

let client_num c = client_num (as_client c)
let file_num c = file_num (as_file c)
let server_num c = server_num (as_server c.server_server)


(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)


let tag_client = 200
let tag_server = 201
let tag_file   = 202

let donkey_download_counter = ref Int64.zero
let donkey_upload_counter = ref Int64.zero

let client_to_client_tags = ref ([] : tag list)
let client_to_server_tags = ref ([] : tag list)
let client_to_server_reply_tags = ref ([] : tag list)
let emule_info =
  let module E = DonkeyProtoClient.EmuleClientInfo in
  {
    E.version = 66;
    E.protversion = 66;
    E.tags = [];
  }

let sec_ident_enabled () = !!enable_sui && (Autoconf.donkey_sui_works ())

let overnet_connectreply_tags = ref ([] :  tag list)
let overnet_connect_tags = ref ([] :  tag list)

let overnet_md4 = Md4.random()
let nservers = ref 0
let xs_last_search = ref (-1)

let zone_size = Int64.of_int (180 * 1024)
let block_size = 9728000L

(* Old value: *)
(* let nchunks = Int64.to_int (Int64.pred file_size // block_size) + 1 in *)

(* New value: *)
(* From Emule: KnownFile.cpp 
// File size       Data parts      ED2K parts      ED2K part hashs
// ---------------------------------------------------------------
// 1..PARTSIZE-1   1               1               0(!)
// PARTSIZE        1               2(!)            2(!)
// PARTSIZE+1      2               2               2
// PARTSIZE*2      2               3(!)            3(!)
// PARTSIZE*2+1    3               3               3
*)

let get_nchunks size = Int64.to_int (size // block_size) + 1 

let get_nchunk_hashes size = 
    let nchunk_hashes = Int64.to_int (size // block_size) in
    let nchunk_hashes = if nchunk_hashes <> 0 
      then nchunk_hashes + 1 
      else nchunk_hashes in
    nchunk_hashes

let queue_timeout = ref (60. *. 10.) (* 10 minutes *)

let files_queries_per_minute = 3 (* queries for 3 files cost 3*16=48 server-credits; we did get 60 (1 each second) *)

let nclients = ref 0

let protocol_version = 62
let max_file_groups = 1000
let master_server = ref (None: DonkeyTypes.server option)
let udp_sock = ref (None: UdpSocket.t option)
let listen_sock = ref (None : TcpServerSocket.t option)
let porttest_sock = ref (None : TcpBufferedSocket.t option)

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

module H = Weak.Make(struct
      type t = client
      let hash c = Hashtbl.hash c.client_kind

      let equal x y = x.client_kind = y.client_kind
    end)

let clients_by_kind = H.create 127
let clients_root = ref []
let servers_by_key = Hashtbl.create 127
let servers_list = ref ([] : server list)
let walker_list = ref ([] : server list)
let delayed_list = ref ([] : server list)

(* let remaining_time_for_clients = ref (60 * 15) *)

let current_files = ref ([] : file list)
let xs_servers_list = ref ([] : server list)
let connected_server_list = ref ([]  : server list)
let connecting_server_list = ref ([] : server list)

let (banned_ips : (Ip.t, int) Hashtbl.t) = Hashtbl.create 113
let (old_requests : (int * int, request_record) Hashtbl.t) =
  Hashtbl.create 13013

let (file_groups_fifo : Md4.t Fifo.t) = Fifo.create ()
let (connected_clients : (Md4.t, client) Hashtbl.t) = Hashtbl.create 130

let udp_servers_list = ref ([] : server list)
let interesting_clients = ref ([] : client list)

let files_by_md4 = Hashtbl.create 127
let find_file md4 = Hashtbl.find files_by_md4 md4

(* changed 2.5.24: we store directly the size and the modification time
in the shared_files_info *)
let shared_files_info = (Hashtbl.create 127
    : (string * int64 * float, shared_file_info) Hashtbl.t)
let shared_files = ref ([] : file_to_share list)

let udp_servers_replies = (Hashtbl.create 127 : (Md4.t, server) Hashtbl.t)

let file_groups = (Hashtbl.create 1023 : (Md4.t, file_group) Hashtbl.t)

module UdpClientWHashtbl = Weak.Make(struct
      type t = udp_client
      let hash c = Hashtbl.hash (c.udp_client_ip, c.udp_client_port)

      let equal x y = x.udp_client_port = y.udp_client_port
        && x.udp_client_ip = y.udp_client_ip
    end)

let udp_clients = UdpClientWHashtbl.create 1023

let join_queue_by_md4 = Hashtbl.create 13
let join_queue_by_id  = Hashtbl.create 13

(*************************************************************************)
(*                                                                       *)
(*                         Global functions                              *)
(*                                                                       *)
(*************************************************************************)

let _ =
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server) !connected_server_list
  )


let hashtbl_remove table key v =
  try
    let vv = Hashtbl.find table key in
    if vv == v then
      Hashtbl.remove table key
  with _ -> ()

let add_connecting_server c =
  connecting_server_list := c :: !connecting_server_list

let remove_connecting_server c =
  connecting_server_list := List2.removeq c !connecting_server_list

let connecting_server_ips () =
  List.rev_map (fun s -> s.server_ip) !connecting_server_list

let add_connected_server c =
  connected_server_list := c :: !connected_server_list

let remove_connected_server c =
  connected_server_list := List2.removeq c !connected_server_list

let connected_servers () = !connected_server_list

let logged_in_servers () =
List.filter (fun s -> 
  match server_state s with
  | Connected _ -> true
  | _ -> false) !connected_server_list

let get_udp_sock () =
  match !udp_sock with
    None -> failwith "No UDP socket"
  | Some sock -> sock

let md4_of_array md4s =
  let s = String.create ((Array.length md4s) * 16) in
  Array.iteri (fun i v -> 
    String.blit (Md4.direct_to_string v) 0 s (i*16) 16
  ) md4s;
  Md4.string (Bytes.to_string s)

(* compute the name used to save the file *)

let update_best_name file =

  let best_name = file_best_name file in
(*  lprintf "update_best_name: %s\n" best_name; *)
  if best_name = file_string_of_uid (Ed2k file.file_md4)
     || best_name = string_of_uid (Ed2k file.file_md4)
     || best_name = Md4.to_string file.file_md4
    then
    try
      let file = as_file file in
(*      lprintf "Propose filename...\n"; *)
      CommonFile.propose_filename file;
      let impl = as_file_impl file in
      match impl.impl_file_probable_name with
        None -> ()
      | Some best_name ->
          set_file_best_name file best_name 0
    with Not_found -> ()

let new_file file_diskname file_state md4 file_size filename writable user group =
  try
      let file = find_file md4 in
      if file.file_diskname <> file_diskname then
        begin
          if not (Sys.file_exists file.file_diskname)
              && Sys.file_exists file_diskname
              && file.file_shared = None
              && Unix32.destroyed (file_fd file)
            then
              begin
                if !verbose_share then
                  lprintf_nl "New file with changed filename %s to %s"
                    file.file_diskname file_diskname;
                file.file_diskname <- file_diskname;
              end
          else
            if !verbose_share then
              lprintf_nl "New file with not changed different filename %s and %s"
                file.file_diskname file_diskname;
        end;
      if Unix32.destroyed (file_fd file)
          && not writable
          && file.file_diskname = file_diskname
        then
          file.file_file.impl_file_fd <-
            Some (Unix32.create_diskfile file.file_diskname true);
      if Unix32.destroyed (file_fd file) then
          lprintf_nl "New Edonkey file with %b && %b remaining destroyed fd %s"
            (not writable) (file.file_diskname = file_diskname) file.file_diskname;
      file
  with _ ->
      if !verbose_share then
        lprintf_nl "New file with md4: %s" (Md4.to_string md4);

      let t =
(*  emulate_sparsefiles does not work, temporarily disabled
        if
(* Don't use this for shared files ! *)
          writable &&
(* Only if the option is set *)
          !!emulate_sparsefiles &&
(* Only if the file does not already exists *)
          not (Unix32.file_exists file_diskname)
        then
          Unix32.create_sparsefile file_diskname writable
        else
*)
          try
            Unix32.create_diskfile file_diskname writable
          with e ->
            failwith (Printf.sprintf "Error: %s" (Printexc2.to_string e))
      in
      let file_size =
        if file_size = Int64.zero then
          try
            Unix32.getsize file_diskname
          with _ ->
              failwith "Zero length file ?"
        else file_size
      in

      if file_size <> zero && writable then (* do not truncate if not writable *)
        begin
    try
            Unix32.ftruncate64 t file_size !!create_file_sparse
    with e ->
      (try
        Unix32.remove t
       with e ->
         lprintf_nl "Unix32.remove %s exception %s"
           (file_diskname) (Printexc2.to_string e));
      Unix32.destroy t;
      failwith (Printf.sprintf "file size %s is too big, exception: %s"
        (size_of_int64 file_size) (Printexc2.to_string e))
  end;

      let md4s = if file_size < block_size then [md4] else [] in
      let rec file = {
          file_diskname = file_diskname;
          file_file = file_impl;
          file_shared = None;
          file_md4 = md4;
          file_swarmer = None;
          file_nchunks = get_nchunks file_size;
          file_nchunk_hashes = get_nchunk_hashes file_size;
          file_computed_md4s = Array.of_list md4s;
          file_format = FormatNotComputed 0;
          file_sources = DonkeySources.create_file_sources_manager
            (Md4.to_string md4);
          file_comments = [];
        }
      and file_impl = {
          (dummy_file_impl ()) with
          impl_file_owner = user;
          impl_file_group = group;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();
          impl_file_size = file_size;
          impl_file_fd = Some t;
          impl_file_best_name = Filename.basename file_diskname;
          impl_file_filenames = (if filename = "" then [] else [filename]);
          impl_file_last_seen = last_time () - 100 * Date.day_in_secs;
        }
      in

      file.file_sources.DonkeySources.manager_file <- (fun () -> as_file file);

      (match file_state with
          FileShared -> ()
        | _ ->
            let kernel = CommonSwarming.create_swarmer file_diskname file_size in
            let swarmer = CommonSwarming.create kernel (as_file file) block_size
            in
            file.file_swarmer <- Some swarmer;
            CommonSwarming.set_verifier swarmer
              (if md4s = [] then VerificationNotAvailable else
                Verification (Array.of_list (List.map (fun md4 -> Ed2k md4) md4s))
            );
            CommonSwarming.set_verified swarmer (fun nblocks num ->
                if nblocks = 1 then file_must_update file)
      );

      update_best_name file;
      file_add file_impl file_state;
      Heap.set_tag file tag_file;
      Hashtbl.add files_by_md4 md4 file;
      file

  (*

  for i = 0 to file.file_nchunks - 1 do
    if client_chunks.(i) then
      let new_n = file.file_available_chunks.(i) + 1 in
      if new_n  < 11 then  file_must_update file;
      file.file_available_chunks.(i) <- new_n;
  done

let remove_client_chunks file client_chunks =
  for i = 0 to file.file_nchunks - 1 do
    if client_chunks.(i) then
      let new_n = file.file_available_chunks.(i) - 1 in
      if new_n < 11 then file_must_update file;
      file.file_available_chunks.(i) <- new_n;
      client_chunks.(i) <- false
  done
    *)

let low_id ip =
  match Ip.to_ints ip with
    | _, _, _, 0 -> true
    | _ -> false

let is_black_address ip port cc =
  !!black_list && not (low_id ip) && (
(* lprintf "is black ="; *)
    not (Ip.reachable ip) || 
    (Ip_set.match_ip !server_black_list_set ip) || 
    (List.mem port !!port_black_list) ||
    (match !Ip.banned (ip, cc) with
        None -> false
      | Some reason ->
          if !verbose_connect then
            lprintf_nl "%s:%d blocked: %s" (Ip.to_string ip) port reason;
          true))

let check_server_country_code s =
  if Geoip.active () then
    match s.server_country_code with
    | None -> s.server_country_code <- Geoip.get_country_code_option s.server_ip
    | _ -> ()

let new_server ip port =
  let key = (ip) in
  try
    let found = Hashtbl.find servers_by_key key in
(* Is updating port to the most recent value the correct thing to do ?
   PlasmaHH says they're legitimate servers switching ports :( *)
    found.server_port <- port;
    found
  with Not_found ->
      let rec s = {
        server_server = server_impl;
        server_next_udp = last_time ();
        server_ip = ip;
        server_cid = None (* client_ip None *);
        server_port = port;
        server_realport = None;
        server_country_code = None;
        server_sock = NoConnection;
        server_search_queries = Fifo.create ();
        server_users_queries = Fifo.create ();
        server_connection_control = new_connection_control ();
        server_score = 5;
        server_tags = [];
        server_nfiles = None;
        server_nusers = None;
        server_name = "";
        server_description = "";
        server_banner = "";
        server_users = [];
        server_master = false;
        server_preferred = false;
        server_queries_credit = 0;
        server_waiting_queries = [];
        server_sent_all_queries = false;
        server_id_requests = Fifo.create ();
        server_flags = 0;
        server_has_zlib = false;
        server_has_newtags = false;
        server_has_unicode = false;
        server_has_related_search = false;
        server_has_tag_integer = false;
        server_has_largefiles = false;
        server_version = "";
        server_lowid_users = None;
        server_soft_limit = None;
        server_hard_limit = None;
        server_obfuscation_tcp = None;
        server_obfuscation_udp = None;
        server_sent_shared = [];
        server_max_users = None;
        server_last_ping = 0.;
        server_next_ping = 0.;
        server_descping_counter = 0;
        server_ping = 0;
        server_failed_count = 0;
        server_udp_ping_challenge = None;
        server_udp_desc_challenge = None;
        server_has_get_sources = false;
        server_has_get_files = false;
        server_has_get_sources2 = false;
        server_dynip = "";
        server_auxportslist = "";

      }
      and server_impl =
        {
          dummy_server_impl with
          CommonServer.impl_server_val = s;
          CommonServer.impl_server_ops = server_ops;
        }
      in
      server_add server_impl;
      Heap.set_tag s tag_server;
      Hashtbl.add servers_by_key key s;
      check_server_country_code s;
      server_must_update s;
      s

let find_server ip port =
  let key = (ip) in
  Hashtbl.find servers_by_key key

let remove_server ip port =
  let key = (ip) in
  let s = Hashtbl.find servers_by_key key in
  try
    Hashtbl.remove servers_by_key key;
    servers_list := List2.removeq s !servers_list ;
    walker_list := List2.removeq s !walker_list;
    delayed_list := List2.removeq s !delayed_list;
    (match s.server_sock with
        NoConnection -> ()
      | ConnectionWaiting token -> cancel_token token
      | Connection sock ->
          TcpBufferedSocket.shutdown sock Closed_by_user)
  with _ -> ()

let check_client_country_code c =
  if Geoip.active () then
    match c.client_country_code with
    | None ->
        (match c.client_kind with
        | Direct_address (ip,port) ->
            c.client_country_code <- Geoip.get_country_code_option ip
        | Indirect_address (_,_,_,_,real_ip) ->
            c.client_country_code <- Geoip.get_country_code_option real_ip
        | _ -> ())
    | _ -> ()

let dummy_client =
  let module D = DonkeyProtoClient in
  let rec c = {
      client_client = client_impl;
      client_upload = None;
      client_kind = Direct_address (Ip.null, 0);
      client_source = DonkeySources.dummy_source;
      client_ip = Ip.null;
      client_country_code = None;
      client_md4 = Md4.null;
      client_download = None;
      client_file_queue = [];
      client_tags = [];
      client_name = "";
      client_all_files = None;
      client_rating = 0;
      client_brand = Brand_unknown;
      client_brand_mod = Brand_mod_unknown;
      client_osinfo = None;
      client_checked = false;
      client_connected = false;
      client_session_downloaded = Int64.zero;
      client_session_uploaded = Int64.zero;
      client_total_downloaded = Int64.zero;
      client_total_uploaded = Int64.zero;
      client_banned = false;
      client_rank = 0;
      client_connect_time = 0;
      client_requests_sent = 0;
      client_requests_received = 0;
      client_slot = SlotNotAsked;
      client_debug = false;
      client_pending_messages = [];
      client_emule_proto = emule_proto ();
      client_comp = None;
      client_connection_time = 0;
      client_req_challenge = Int64.zero;
      client_sent_challenge = Int64.zero;
      client_public_key = None;
      client_sui_verified = None;
      client_last_file_req_md4 = None;
      client_osinfo_sent = false;
      } and
    client_impl = {
      dummy_client_impl with
      impl_client_val = c;
      impl_client_ops = client_ops;
      impl_client_upload = None;
    }
  in
  c

let create_client key cc =
  let module D = DonkeyProtoClient in
  let s = DonkeySources.create_source_by_uid (match key with
      Indirect_address (server_ip, server_port, id, port, real_ip) -> Indirect_address (server_ip, server_port, id, 0, Ip.null) 
      | _ -> key) cc in
  let rec c = {
      client_client = client_impl;
      client_kind = key;
      client_upload = None;
      client_source = s;
      client_ip = Ip.null;
      client_country_code = cc;
      client_md4 = Md4.null;
      client_download = None;
      client_file_queue = [];
      client_tags = [];
      client_name = "";
      client_all_files = None;
      client_rating = 0;
      client_brand = Brand_unknown;
      client_brand_mod = Brand_mod_unknown;
      client_osinfo = None;
      client_checked = false;
      client_connected = false;
      client_total_downloaded = Int64.zero;
      client_total_uploaded = Int64.zero;
      client_session_downloaded = Int64.zero;
      client_session_uploaded = Int64.zero;
      client_banned = false;
      client_rank = 0;
      client_connect_time = 0;
      client_requests_received = 0;
      client_requests_sent = 0;
      client_slot = SlotNotAsked;
      client_debug = Intset.mem s.DonkeySources.source_num !debug_clients;
      client_pending_messages = [];
      client_emule_proto = emule_proto ();
      client_comp = None;
      client_connection_time = 0;
      client_req_challenge = Int64.zero;
      client_sent_challenge = Int64.zero;
      client_public_key = None;
      client_sui_verified = None;
      client_last_file_req_md4 = None;
      client_osinfo_sent = false;
      } and    client_impl = {
      dummy_client_impl with
      impl_client_val = c;
      impl_client_ops = client_ops;
      impl_client_upload = None;
    }
  in
  Heap.set_tag c tag_client;
  CommonClient.new_client_with_num client_impl s.DonkeySources.source_num;
  H.add clients_by_kind c;
  clients_root := c :: !clients_root;
  check_client_country_code c;
  c

exception ClientFound of client
let find_client_by_key key =
  try
    H.iter (fun c ->
      if
     (match c.client_kind with
      | Indirect_address (server_ip, server_port, id, port, real_ip) ->
          Indirect_address (server_ip, server_port, id, 0, Ip.null)
      | _ -> c.client_kind) =
     (match key with
      | Indirect_address (server_ip, server_port, id, port, real_ip) ->
          Indirect_address (server_ip, server_port, id, 0, Ip.null) 
      | _ -> key) then
        raise (ClientFound c)
    ) clients_by_kind;
    raise Not_found
  with ClientFound c -> c

let new_client key cc =
  try
    let c = find_client_by_key key in
(* An indirect client without real_ip might have been created earlier.
   If that client connected us later we have its real ip *)
    (match key with
    | Indirect_address (_,_,_,_,ip_real) ->
        let old_ip =
          match c.client_kind with
          | Indirect_address (_,_,_,_,old_ip) ->
              if old_ip = Ip.null then None else Some old_ip
          | _ -> None
        in
        if ip_real <> Ip.null then c.client_kind <- key;
        (match old_ip with
        | Some old_ip ->
            if old_ip <> ip_real then check_client_country_code c
        | None -> if ip_real <> Ip.null then check_client_country_code c)
    | _ -> ());
    c
  with _ ->
      create_client key cc

let create_client = ()

let client_type c =
  client_type (as_client c)

let set_client_type c t=
  set_client_type (as_client c) t

let friend_add c =
  friend_add (as_client c)

let string_of_server s =
  Printf.sprintf "%s:%d" (Ip.to_string s.server_ip) s.server_port

let set_client_name c name md4 =
  if name <> c.client_name || c.client_md4 <> md4 then begin
      c.client_name <- name;
      c.client_md4 <- md4;
    end

let find_client_by_name name =
  try
    H.iter (fun c ->
        if c.client_name = name then raise (ClientFound c)
    ) clients_by_kind;
    raise Not_found
  with ClientFound c -> c

let local_mem_stats level buf =
  let client_counter = ref 0 in
  let unconnected_unknown_clients = ref 0 in
  let uninteresting_clients = ref 0 in
  let aliased_clients = ref 0 in
  let myconnected_clients = ref 0 in
  let closed_connections = ref 0 in
  let unlocated_client = ref 0 in
  let bad_numbered_clients = ref 0 in
  let disconnected_alias = ref 0 in
  let dead_clients = ref 0 in
  let buffers = ref 0 in
  let waiting_msgs = ref 0 in
  let connected_clients_by_num = Hashtbl.create 100 in
  H.iter (fun c ->
(*
      if c.client_num <> num then begin
          incr bad_numbered_clients;
          try
            let cc = Hashtbl.find clients_by_num c.client_num in
            if cc.client_sock = None then incr disconnected_alias;
          with _ -> incr dead_clients;
end;
*)
      let num = client_num c in
      incr client_counter;
      match c.client_source.DonkeySources.source_sock with
        NoConnection -> begin
            match c.client_kind with
              Indirect_address _ -> incr unconnected_unknown_clients
            | _ -> ()
          end
      | ConnectionWaiting _  -> ()
      | Connection sock ->
          let buf_len, nmsgs = TcpBufferedSocket.buf_size sock in
          (try
              Hashtbl.find connected_clients_by_num num
            with _ ->
                incr myconnected_clients;
                waiting_msgs := !waiting_msgs + nmsgs;
                buffers := !buffers + buf_len;
(*
                Printf.bprintf buf "%d: %6d/%6d\n" num
                  buf_len nmsgs *)
          );
          if TcpBufferedSocket.closed sock then
            incr closed_connections;
  ) clients_by_kind;

  let bad_clients_in_files = ref 0 in
  Hashtbl.iter (fun _ file ->
      DonkeySources.iter_all_sources (fun s ->
          match s.DonkeySources.source_sock with
            NoConnection -> begin
                match s.DonkeySources.source_uid with
                  Indirect_address _ -> incr bad_clients_in_files
                | _ -> ()
              end
          | _ -> ()
      ) file.file_sources;
  ) files_by_md4;

  Printf.bprintf buf "Clients: %d\n" !client_counter;
  Printf.bprintf buf "   Bad Clients: %d/%d\n" !unconnected_unknown_clients
    !bad_clients_in_files;
  Printf.bprintf buf "   Read Buffers: %d\n" !buffers;
  Printf.bprintf buf "   Write Messages: %d\n" !waiting_msgs;
  Printf.bprintf buf "   Uninteresting clients: %d\n" !uninteresting_clients;
  Printf.bprintf buf "   Connected clients: %d\n" !myconnected_clients;
  Printf.bprintf buf "   Aliased clients: %d\n" !aliased_clients;
  Printf.bprintf buf "   Closed clients: %d\n" !closed_connections;
  Printf.bprintf buf "   Unlocated clients: %d\n" !unlocated_client;
  Printf.bprintf buf "   Bad numbered clients: %d\n" !bad_numbered_clients;
  Printf.bprintf buf "   Dead clients: %d\n" !dead_clients;
  Printf.bprintf buf "   Disconnected aliases: %d\n" !disconnected_alias;

  Printf.bprintf buf "Number of old files: %d\n" (List.length !!old_files);
  Printf.bprintf buf "Current files: %d\n" (List.length !current_files);

  let counter = ref 0 in
  UdpClientWHashtbl.iter (fun _ -> incr counter) udp_clients;
  Printf.bprintf buf "  udp_clients: %d\n" !counter;

  Printf.bprintf buf "  client_to_client_tags: %d\n" (List.length !client_to_client_tags);
  Printf.bprintf buf "  client_to_server_tags: %d\n" (List.length !client_to_server_tags);
  Printf.bprintf buf "  overnet_connectreply_tags: %d\n" (List.length !overnet_connectreply_tags);
  Printf.bprintf buf "  overnet_connect_tags: %d\n" (List.length !overnet_connect_tags);
  Printf.bprintf buf "  clients_root: %d\n" (List.length !clients_root);
  Printf.bprintf buf "  servers_list: %d\n" (List.length !servers_list);
  Printf.bprintf buf "  xs_servers_list: %d\n" (List.length !xs_servers_list);
  Printf.bprintf buf "  connected_server_list: %d\n" (List.length !connected_server_list);
  Printf.bprintf buf "  connecting_server_list: %d\n" (List.length !connecting_server_list);
  Printf.bprintf buf "  udp_servers_list: %d\n" (List.length !udp_servers_list);
  Printf.bprintf buf "  interesting_clients: %d\n" (List.length !interesting_clients);
  Printf.bprintf buf "  shared_files: %d\n" (List.length !shared_files);
  Printf.bprintf buf "  servers_by_key: %d\n" (Hashtbl.length servers_by_key);
  Printf.bprintf buf "  banned_ips: %d\n" (Hashtbl.length banned_ips);
  Printf.bprintf buf "  old_requests: %d\n" (Hashtbl.length old_requests);
  Printf.bprintf buf "  connected_clients: %d\n" (Hashtbl.length connected_clients);
  Printf.bprintf buf "  files_by_md4: %d\n" (Hashtbl.length files_by_md4);
  Printf.bprintf buf "  shared_files_info: %d\n" (Hashtbl.length shared_files_info);
  Printf.bprintf buf "  file_groups: %d\n" (Hashtbl.length file_groups);
  Printf.bprintf buf "  udp_servers_replies: %d\n" (Hashtbl.length udp_servers_replies);
  Printf.bprintf buf "  join_queue_by_md4: %d\n" (Hashtbl.length join_queue_by_md4);
  Printf.bprintf buf "  join_queue_by_id: %d\n" (Hashtbl.length join_queue_by_id);

(*  let list = H.to_list clients_by_kind in *)
  if level > 0 then begin
    H.iter (fun c ->
      Printf.bprintf buf "[%d ok: %s rating: %d]\n"
        (client_num c)
        (string_of_date (c.client_source.DonkeySources.source_age))
(* TODO: add connection state *)
        c.client_rating;
    ) clients_by_kind;
  end;
  ()

let remove_client c =
  client_remove (as_client c);
(*  hashtbl_remove clients_by_kind c.client_kind c; *)
(*  hashtbl_remove clients_by_name c.client_name c *)
  ()


let friend_remove c =
  friend_remove  (as_client c)


(* Parts stolen from update_master_servers. Maybe someone competent
   enough reduces the redundant code produced here. *)
let last_connected_master () =
  let server_list = connected_servers () in
  let masters = ref [] in
  List.iter (
    fun s ->
      if s.server_master then
        match s.server_sock with
          | Connection _ ->
              masters := s :: !masters
          | _ -> s.server_master <- false
  ) server_list;
  match !masters with
  | s :: _ -> s
  | [] -> raise Not_found

let last_connected_server () =
  match !servers_list with
  | s :: _ -> s
  | [] ->
      servers_list :=
      Hashtbl.fold (fun key s l ->
          s :: l
      ) servers_by_key [];
      match !servers_list with
        [] -> raise Not_found
      | s :: _ -> s

let string_of_file_state s =
  match  s with
  | FileDownloading -> "File Downloading"
  | FilePaused -> "File Paused"
  | FileDownloaded -> "File Downloaded"
  | FileShared     -> "File Shared"
  | FileCancelled -> "File Cancelled"
  | FileNew -> "File New"
  | FileAborted s -> Printf.sprintf "Aborted: %s" s
  | FileQueued -> "File Queued"

let left_bytes = "MLDK"

let overnet_server_ip = ref Ip.null
let overnet_server_port = ref 0

let overnet_port_info = ref 0
let kademlia_port_info = ref 0


(*************************************************************

Define a function to be called when the "mem_stats" command
  is used to display information on structure footprint.

**************************************************************)

let _ =
  Heap.add_memstat "DonkeyGlobals" local_mem_stats

(*************************************************************

   Save the state of the client positive queries for files
 if a JoinQueue message was sent. Use this information if
 an AvailableSlot message is received while not JoinQueue
 message was sent (client_asked_for_slot false).

**************************************************************)

let client_id c =
  match c.client_kind with
    Direct_address (ip, port) -> (ip, port, zero)
  | Indirect_address (server_ip, server_port, id, port, real_ip) ->
     (server_ip, server_port, id)
  | Invalid_address _ -> (Ip.null, 0, zero)

let save_join_queue c =
  if c.client_file_queue <> [] then
    let files = List.map (fun (file, chunks, _) ->
          file, Bitv.copy chunks
      ) c.client_file_queue in
    begin
      if c.client_debug then
        lprintf_nl "Saving %d files associated with %s"
        (List.length files) (Md4.to_string c.client_md4);
      Hashtbl.add join_queue_by_md4 c.client_md4 (files, last_time ());
      try
        let id = client_id c in
        Hashtbl.add join_queue_by_id id (files, last_time ());
      with _ -> ()
    end

let clean_join_queue_tables () =
  let current_time = last_time () in

  let list = Hashtbl2.to_list2 join_queue_by_md4 in
  Hashtbl.clear join_queue_by_md4;
  List.iter (fun (key, ((v,time) as e)) ->
      if time + Date.half_hour_in_secs > current_time then
        Hashtbl.add join_queue_by_md4 key e
  ) list;

  let list = Hashtbl2.to_list2 join_queue_by_id in
  Hashtbl.clear join_queue_by_id;
  List.iter (fun (key, ((v,time) as e)) ->
      if time + Date.half_hour_in_secs > current_time then
        Hashtbl.add join_queue_by_id key e
  ) list

let client_public_key = ref ""
let key_check_started = ref false

let _ =
  option_hook client_private_key (fun _ ->
    if Autoconf.donkey_sui_works () then
      begin
        if not (try String.sub !!client_private_key 0 4 = "MIIB" with e -> false) then
          if !key_check_started then
            begin
              let s1 =
                Printf.sprintf "can not create valid client_private_key, bad value found: %s"
                  !!client_private_key in
              let s2 = "CryptoPP code seems not to work properly, do not use insane CFLAGS, exiting..." in
              Printf.eprintf "%s[EDK] %s\n%!" (log_time ()) s1;
              Printf.eprintf "%s[EDK] %s\n%!" (log_time ()) s2;
              lprintf_nl "%s" s1;
              lprintf_nl "%s" s2;
              exit 70
            end
          else
            begin
              key_check_started := true;
              lprintf_nl "found bad client_private_key: %s, creating new key..." !!client_private_key;
              client_private_key =:= (DonkeySui.SUI.create_key ());
            end
        else
          begin
            client_public_key := DonkeySui.SUI.load_key (!!client_private_key);
          end
      end
  )

let full_client_identifier c =
    Printf.sprintf "%s (%s%s) '%s'"
      (match c.client_kind with
          Indirect_address (server_ip, server_port, id, port, real_ip) ->
            Printf.sprintf "%s:%d%s[lowID %Ld, server:%s:%d]"
              (Ip.to_string real_ip) port
              (match c.client_country_code with | None -> "" | Some cc -> Printf.sprintf "(%d)" cc)
              id (Ip.to_string server_ip) server_port
        | Direct_address (ip,port) ->
            Printf.sprintf "%s:%d%s" (Ip.to_string ip) port
              (match c.client_country_code with | None -> "" | Some cc -> Printf.sprintf "(%d)" cc)
        | Invalid_address _ -> " invalid IP")
      (GuiTypes.client_software_short (brand_to_string_short c.client_brand) c.client_osinfo)
      (if c.client_emule_proto.emule_release = "" then "" else " " ^ c.client_emule_proto.emule_release)
      (String.escaped c.client_name)
