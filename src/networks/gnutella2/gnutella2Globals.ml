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
open CommonHosts
  
open MultinetTypes
open MultinetFunctions
open MultinetComplexOptions
  
open Gnutella2Types
open Gnutella2Options

                  
let extension_list = [
    "mp3" ; "avi" ; "jpg" ; "jpeg" ; "txt" ; "mov" ; "mpg" 
]

let new_shared_files = ref false
  
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

  
  
let network = new_network "Gnutella2"  
     [ 
    NetworkHasSupernodes; 
    NetworkHasSearch;
    NetworkHasUpload;
    NetworkHasMultinet;
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
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

  
let (network_file_ops: file_network) = new_file_network "Gnutella2"
let (file_ops: file network_file) = new_network_file network_file_ops

  
let current_files = ref ([] : Gnutella2Types.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
let (searches_by_uid : (Md4.t, local_search) Hashtbl.t) = Hashtbl.create 11

  (*
let redirector_connected = ref false
(* let redirectors_ips = ref ( [] : Ip.t list) *)
let redirectors_to_try = ref ( [] : string list)
  *)

let files_by_uid = Hashtbl.create 13
(* let files_by_key = Hashtbl.create 13 *)

let (users_by_uid ) = Hashtbl.create 127
let (clients_by_uid ) = Hashtbl.create 127
(* let results_by_key = Hashtbl.create 127 *)
let results_by_uid = Hashtbl.create 127
  
(***************************************************************


             HOST SCHEDULER


****************************************************************)
  
  
let ready _ = false
  
(* From the main workflow, hosts are moved to these workflows when they
are ready to be connected. They will only be connected when connections
will be available. We separate g1/g2, and g0 (unknown kind). *)
let (g2_ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* peers are only tested when no ultrapeers are available... *)
let (g2_peers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that we should try to contact by UDP *)
let (g2_waiting_udp_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that have replied to our UDP requests *)
let (g2_active_udp_queue : host Queue.t) = Queues.fifo ()

let g2_nservers = ref 0

let g2_connected_servers = ref ([] : server list)
  
module H = CommonHosts.Make(struct
      include Gnutella2Types
      type ip = Ip.t
      
      let requests = 
        [ 
          Tcp_Connect, 
          (600, (fun kind ->
                [ match kind with
                  | true -> g2_ultrapeers_waiting_queue
                  | (_) -> g2_peers_waiting_queue
                ]
            ));
          Udp_Connect,
          (600, (fun kind ->
                   [g2_waiting_udp_queue]
            ))]
        
      let default_requests kind = [Tcp_Connect,0; Udp_Connect,0]
    end)

let new_server ip port =
  let h = H.new_host ip port (true) in
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

      (*
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
        *)

let add_source r s index =
  let key = (s, index) in
  if not (List.mem key r.result_sources) then begin
      r.result_sources <- key :: r.result_sources
    end

let new_result file_name file_size tags uids =
  let uids = expand_uids uids in
  let result = ref None in
  List.iter (fun uid ->
      match uid with
        Sha1 (_, sha1) ->
          let r = 
            try
              Hashtbl.find results_by_uid sha1
            with _ -> 
                let rec result = {
                    result_result = result_impl;
                    result_name = file_name;
                    result_size = file_size;
                    result_sources = [];
                    result_tags = tags;
                    result_uids = [];
                  } and
                  result_impl = {
                    dummy_result_impl with
                    impl_result_val = result;
                    impl_result_ops = result_ops;
                  } in
                new_result result_impl;
                Hashtbl.add results_by_uid sha1 result;
                result
          in
          r.result_uids <- expand_uids (uids @ r.result_uids);
          result := Some r;
      | _ -> ()) uids;
  match !result with None -> raise Not_found
  | Some r -> r

let megabyte = Int64.of_int (1024 * 1024)

let set_tiger_tiger_hashes file chunk_size array =
  file.file_tiger <- Some (chunk_size, array);
  
  let file_shared = file.file_shared in
  let rec verifier b =
    match file.file_tiger with 
      None ->  ()
    | Some (block_size, array) ->
        Unix32.flush_fd (file_fd file_shared);
        let num, begin_pos, end_pos = Int64Swarmer.block_block b in
        
        if block_size = megabyte then begin
            
            lprintf "Md4 to compute: %d %Ld-%Ld\n" num begin_pos end_pos;
            let tiger = TigerTree.digest_subfile (file_fd file_shared) 
              begin_pos (end_pos -- begin_pos) in
            let result = tiger = array.(num) in
            lprintf "Tiger computed: %s against %s = %s\n"
              (TigerTree.to_string tiger) 
            (TigerTree.to_string array.(num))
            (if result then "VERIFIED" else "CORRUPTED");
            if result then 
              Int64Swarmer.loaded_block b
            else 
              Int64Swarmer.reload_block b;
            
            if result then begin
                let bitmap = Int64Swarmer.verified_bitmap file.file_partition in
                try ignore (String.index bitmap '3')
                with _ -> 
(* Add this file to the shared list *)
                    new_shared_files := true;
                    file_must_update file_shared;
              end;
          end else  
        if block_size < megabyte then begin
(* We can be more accurate *)
            let nsubblocks = Int64.to_int (megabyte // block_size) in
            let block_result = ref true in
            for i = 0 to nsubblocks - 1 do
              let begin_pos = begin_pos ++ block_size ** i in
              let end_pos = begin_pos ++ block_size in
              lprintf "Md4 to compute: %d %Ld-%Ld\n" num begin_pos end_pos;
              let tiger = TigerTree.digest_subfile (file_fd file_shared) 
                begin_pos block_size in
              let num = num * nsubblocks + i in
              let result = tiger = array.(num) in
              lprintf "Tiger computed: %s against %s = %s\n"
                (TigerTree.to_string tiger) 
              (TigerTree.to_string array.(num))
              (if result then "VERIFIED" else "CORRUPTED");
              block_result := !block_result && result;
              (if result then
                  Int64Swarmer.loaded_ranges
                else
                  Int64Swarmer.reload_ranges) 
              b begin_pos end_pos
            done;
            let result = !block_result in
            if result then 
              Int64Swarmer.loaded_block b
            else 
              Int64Swarmer.reload_block b;
            
            if result && first_verified_block file.file_partition then begin
(* Add this file to the shared list *)
                lprintf "gnutella2: can share partial file\n";
                new_shared_files := true;
                file_must_update file_shared;
              end;
          
          end else begin
(* If we succeed, we can validate more blocks, otherwise, we must invalidate
all the completely downloaded blocks *)
            let num = Int64.to_int (begin_pos // block_size) in
            let begin_pos = block_size ** num in
            let end_pos = begin_pos ++ block_size in
            lprintf "Md4 to compute: %d %Ld-%Ld\n" num begin_pos end_pos;
            let tiger = TigerTree.digest_subfile (file_fd file_shared) 
              begin_pos block_size in
            let result = tiger = array.(num) in
            lprintf "Tiger computed: %s against %s = %s\n"
              (TigerTree.to_string tiger) 
            (TigerTree.to_string array.(num))
            (if result then "VERIFIED" else "CORRUPTED");
            
            if result then begin
                let bitmap = Int64Swarmer.verified_bitmap file.file_partition in
                try
                  ignore (String.index bitmap '3')
                with _ -> 
(* Add this file to the shared list *)
                    lprintf "gnutella2: can share partial file\n";
                    new_shared_files := true;
                    file_must_update file_shared;
              end;
            
            (if result then 
              Int64Swarmer.loaded_blocks
            else 
                Int64Swarmer.reload_blocks) file.file_partition
              begin_pos end_pos
            
          end
          
          
  in
  
  Int64Swarmer.set_verifier file.file_partition verifier;
    ()
  
let new_file file_shared = 
  let partition = fixed_partition file_shared.file_swarmer network.network_num megabyte in
  let keywords = get_name_keywords (file_best_name file_shared) in
  let words = String2.unsplit keywords ' ' in
  let rec file = {
      file_shared = file_shared;
      file_clients = [];
      file_partition = partition;
      file_searches = [search];
      file_tiger = None;
    } and search = {
      search_search = FileWordSearch (file, words);
      search_hosts = Intset.empty;
      search_uid = Md4.random ();
    } 
  in
  
  Hashtbl.add searches_by_uid search.search_uid search;
  lprintf "SET SIZE : %Ld\n" (file_size file_shared);
  current_files := file :: !current_files;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
  file

exception FileFound of file
  
let new_file file_shared =
  let file_name = file_best_name file_shared in
  let file_size = file_size file_shared in
  let file_uids = file_shared.file_uids in
  let file = ref None in
  let good_hash = ref false in
  List.iter (fun uid ->
      match uid with
        Sha1 _ | Ed2k _ | TigerTree _ | Bitprint _ ->
          good_hash := true;
          (try file := Some (Hashtbl.find files_by_uid uid) with _ -> ())
      | _ -> ()) file_uids;
  match !file with
    None -> 
      if !good_hash then
        let file = new_file file_shared in
        List.iter (fun uid ->
            match uid with
              Sha1 _ | Ed2k _ | TigerTree _ | Bitprint _ ->
                Hashtbl.add files_by_uid uid file
            | _ -> ()) file_uids;
        file
      else raise Not_found
  | Some file -> file
  
let _ =
  register_network network_file_ops;
  network_file_ops.op_download_start <-   (fun file ->
      try ignore (new_file file) with _ -> ())

      
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
      let chunks = [ Int64.zero, file_size file.file_shared ] in
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
      file_add_source file.file_shared (as_client c.client_client)
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
  
let file_state file =  file_state file.file_shared
  
let file_num file =  file_num  file.file_shared

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
  let file_shared = file.file_shared in
  let uids = file_shared.file_uids in
    List.iter (fun uid ->
      Hashtbl.remove files_by_uid uid
  ) uids;
  current_files := List2.removeq file !current_files  

let udp_sock = ref (None : UdpSocket.t option)

let client_ip sock =
  CommonOptions.client_ip
    (match sock with 
    | Connection sock | CompressedConnection (_,_,_,sock) -> Some sock
    | _ -> None)

let disconnect_from_server s r =
  match s.server_sock with
  | Connection sock | CompressedConnection (_,_,_,sock) ->
      let h = s.server_host in
      (match server_state s with 
          Connected _ ->
            let connection_time = Int32.to_int (
                Int32.sub (int32_time ()) s.server_connected) in
            lprintf "DISCONNECT FROM SERVER %s:%d after %d seconds\n" 
              (Ip.to_string h.host_addr) h.host_port
              connection_time
            ; 
            lprintf "Reason: %s\n"
                (
                match r with
                  Closed_for_timeout -> "timeout"
                | Closed_for_lifetime -> "lifetime"
                | Closed_by_peer -> "closed by peer"
                | Closed_for_error error -> error
                | Closed_by_user -> "by me"
                | Closed_for_overflow -> "overflow"
                | Closed_connect_failed -> "connect failed"
                | Closed_for_exception e -> Printexc2.to_string e
              )
        | _ -> ()
      );
      (try close sock r with _ -> ());
      s.server_sock <- NoConnection;
      set_server_state s (NotConnected (r, -1));
      s.server_need_qrt <- true;
      decr g2_nservers;
      if List.memq s !g2_connected_servers then
        g2_connected_servers := List2.removeq s !g2_connected_servers
  | _ -> ()

    
    
let clean_file s =
  String2.replace_char s '\r' '\n';
  String2.replace_char s ' ' '\n'

let client_name () =
  let name = !!client_name in
  let len = String.length name in
  if len < 32 then name else  String.sub name 0 32
    