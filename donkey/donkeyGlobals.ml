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

open Md4
open CommonResult
open CommonFile
open CommonServer
open CommonComplexOptions
open CommonClient
open Options
open CommonTypes
open DonkeyTypes
open BasicSocket
open CommonOptions
open DonkeyOptions
open CommonOptions
open CommonGlobals

  
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
  
let network = CommonNetwork.new_network "Donkey"
    network_options_prefix commit_in_subdir
  
let (shared_ops : file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network

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

  (*
let client_schedule_size = 60 * 24
let client_schedule = Array.create client_schedule_size ([] : client list)
let client_schedule_minute = ref 0
let client_schedule_remaining_seconds = ref 60
    *)

let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

  
let (pre_shared_ops : file_to_share CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network
    
let (shared_ops : file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network
  
  
let tag_client = 200
let tag_server = 201
let tag_file   = 202
    
(* HOOKS *)

let client_must_update c =
  client_must_update (as_client c.client_client)

let server_must_update s =
  server_must_update (as_server s.server_server)

    
let file_priority file = file.file_file.impl_file_priority
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file.file_file.impl_file_downloaded
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let file_best_name file = file_best_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)

(* let say_hook = ref (fun (c:client) (s:string) -> ())

  
(* These 3 hooks are used to handle connections with a server. They are mainly
  used in DonkeyFiles *)
let server_is_connected_hook = ref (fun
      (s: server) 
      (sock: server_sock) -> ())
let received_from_server_hook = ref (fun 
      (s: server) 
      (sock: server_sock) 
      (t: DonkeyProtoServer.t) -> ())
let server_is_disconnected_hook = ref (fun 
      (s: server) -> ())

(* hook called when something changed on a file. Currently, it is only called
  when a file is added or removed. *)
let file_change_hook = ref (fun (file: file) -> ())
  *)

  (* CONSTANTS *)
let page_size = Int64.of_int 4096    


(* GLOBAL STATE *)
  
open DonkeyMftp
  
let client_tags = ref ([] : tag list)
let client_port = ref 0
let overnet_client_tags = ref ([] : tag list)
let overnet_client_port = ref 0

(* overnet_md4 should be different from client_md4 for protocol safety reasons *)
let overnet_md4 = Md4.random()
(*let overnet_md4 = Md4.of_string "FBB5EA4C0A82FB995911223344556677";*)
    
module H = Weak2.Make(struct
      type t = client
      let hash c = Hashtbl.hash c.client_kind
      
      let equal x y = x.client_kind = y.client_kind
    end)

  
let clients_by_kind = H.create 127
(* let clients_by_name = Hashtbl.create 127 *)

let nservers = ref 0
let servers_by_key = Hashtbl.create 127
let servers_list = ref ([] : server list)
  
(* let remaining_time_for_clients = ref (60 * 15) *)
let location_counter = ref 0
let download_credit = ref 0 

let current_files = ref ([] : file list)
  
let sleeping = ref false
  
let xs_last_search = ref (-1)
let xs_servers_list = ref ([] : server list)
  
let has_upload = ref 0
let upload_credit = ref 0
let zone_size = Int64.of_int (180 * 1024) 
let block_size = Int64.of_int 9728000
  
let queue_timeout = ref (60. *. 10.) (* 10 minutes *)
    
let nclients = ref 0
  
let connected_server_list = ref ([]  : server list)

  
let (banned_ips : (Ip.t, int) Hashtbl.t) = Hashtbl.create 113
let (old_requests : (int * int, request_record) Hashtbl.t) = 
  Hashtbl.create 13013

  
let (pending_slots_map : client Intmap.t ref) = ref Intmap.empty
let (pending_slots_fifo : int Fifo.t)  = Fifo.create ()

  
let max_file_groups = 1000
let (file_groups_fifo : Md4.t Fifo.t) = Fifo.create ()

    
let (connected_clients : (Md4.t, client) Hashtbl.t) = Hashtbl.create 13

  
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

let add_connected_server c =
  connected_server_list := c :: !connected_server_list
  
let remove_connected_server c =
  connected_server_list := List2.removeq c !connected_server_list

let connected_servers () = !connected_server_list
  
let udp_sock = ref (None: UdpSocket.t option)
        
let get_udp_sock () =
  match !udp_sock with
    None -> failwith "No UDP socket"
  | Some sock -> sock

  
let listen_sock = ref (None : TcpServerSocket.t option)
  
let reversed_sock = ref (None : TcpServerSocket.t option)


  
let udp_servers_list = ref ([] : server list)
let interesting_clients = ref ([] : client list)
  
  
(* 'NEW' FUNCTIONS *)  
let files_by_md4 = Hashtbl.create 127

let find_file md4 = Hashtbl.find files_by_md4 md4

    
let servers_ini_changed = ref true
let upload_clients = (Fifo.create () : client Fifo.t)

let shared_files_info = (Hashtbl.create 127 : (string, shared_file_info) Hashtbl.t)
let new_shared = ref ([] : file list)
let shared_files = ref ([] : file_to_share list)

  
(* compute the name used to save the file *)
  
let update_best_name file =
  
  let md4_name = Md4.to_string file.file_md4 in
  
  if file_best_name file = md4_name then
    try
(*      Printf.printf "BEST NAME IS MD4 !!!"; print_newline (); *)
      let rec good_name file list =
        match list with
          [] -> raise Not_found;
        | t :: q -> if t <> md4_name then
              (String2.replace t '/' "::") else good_name file q in
      
      set_file_best_name (as_file file.file_file) 
      (good_name file file.file_filenames);
     
(*      Printf.printf "BEST NAME now IS %s" (file_best_name file); *)
    with Not_found -> ()
  
let new_file file_state file_name md4 file_size writable =
  try
    find_file md4 
  with _ ->
      let file_size =
        if file_size = Int64.zero then
          try
            Unix32.getsize64 file_name
          with _ ->
              failwith "Zero length file ?"
        else file_size
      in
      let nchunks = Int64.to_int (Int64.div 
          (Int64.sub file_size Int64.one) block_size) + 1 in
      let file_exists = Sys.file_exists file_name in
      let md4s = if file_size <= block_size then
            [md4] 
          else [] in
      let rec file = {
          file_file = file_impl;
          file_shared = None;
          file_exists = file_exists;
          file_md4 = md4;
          file_nchunks = nchunks;
          file_chunks = [||];
          file_chunks_order = [||];
          file_chunks_age = [||];
(*          file_all_chunks = String.make nchunks '0'; *)
          file_absent_chunks =   [Int64.zero, file_size];
          file_filenames = [Filename.basename file_name];
          file_nsources = 0;
          file_md4s = md4s;
          file_available_chunks = Array.create nchunks 0;
          file_format = Unknown_format;
          file_locations = Intmap.empty;
          file_mtime = 0.0;
          file_initialized = false;
        }
      and file_impl = {
          dummy_file_impl with
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_size = file_size;
          impl_file_fd = Unix32.create file_name (if writable then
              [Unix.O_RDWR; Unix.O_CREAT] else [Unix.O_RDONLY]) 0o666;
          impl_file_best_name = Filename.basename file_name;
          impl_file_last_seen = last_time () - 100 * 24 * 3600;
        }
      in
      update_best_name file;
      file_add file_impl file_state;
      Heap.set_tag file tag_file;
      Hashtbl.add files_by_md4 md4 file;
      file

let change_hardname file file_name =
  let fd = file.file_file.impl_file_fd in
  Unix32.set_filename fd file_name
          
let add_client_chunks file client_chunks =
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
  
let is_black_address ip port =
  !!black_list && (
(* Printf.printf "is black ="; *)
    not (Ip.reachable ip) || (Ip.matches ip !!server_black_list) ||
    (List.mem port !!port_black_list) )
      
let new_server ip port score = 
  let key = (ip, port) in
  try
    Hashtbl.find servers_by_key key
  with _ ->
      let rec s = { 
          server_server = server_impl;
          server_next_udp = last_time ();
          server_ip = ip;     
          server_cid = client_ip None;
          server_port = port; 
          server_sock = None; 
          server_nqueries = 0;
          server_search_queries = Fifo.create ();
          server_users_queries = Fifo.create ();
          server_connection_control = new_connection_control ();
          server_score = score;
          server_tags = [];
          server_nfiles = 0;
          server_nusers = 0;
          server_name = "";
          server_description = "";
          server_users = [];
          server_master = false;
          server_mldonkey = false;
          server_last_message = 0;
          server_queries_credit = 0;
          server_waiting_queries = [];
          server_id_requests = Fifo.create ();
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
      server_must_update s;
      s      

let find_server ip port =
  let key = (ip, port) in
  Hashtbl.find servers_by_key key  

let remove_server ip port =
  let key = (ip, port) in
  let s = Hashtbl.find servers_by_key key in
  try
    Hashtbl.remove servers_by_key key;
    servers_list := List2.removeq s !servers_list ;
    (match s.server_sock with
        None -> ()
      | Some sock -> shutdown (TcpBufferedSocket.sock sock) "remove server");
    server_remove (as_server s.server_server)
  with _ -> ()

let dummy_client = 
  let rec c = {
      client_client = client_impl;
      client_upload = None;
      client_kind = Indirect_location ("", Md4.null);   
      client_source = None;
      client_sock = None;
      client_md4 = Md4.null;
      client_last_filereqs = 0;
      client_chunks = [||];
      client_block = None;
      client_zones = [];
      client_connection_control =  new_connection_control_recent_ok ( ());
      client_file_queue = [];
      client_tags = [];
      client_name = "";
      client_all_files = None;
      client_next_view_files = last_time () - 1;
      client_all_chunks = "";
      client_rating = 0;
      client_brand = Brand_unknown;
      client_checked = false;
      client_chat_port = 0 ; (** A VOIR : où trouver le 
            port de chat du client ? *)
      client_connected = false;
      client_power = 0;
      client_downloaded = Int64.zero;
      client_uploaded = Int64.zero;
      client_on_list = false;
      client_banned = false;
      client_has_a_slot = false;
      client_overnet = false;
      client_score = 0;
      client_files = [];
      client_next_queue = 0;
      client_rank = 0;
      client_connect_time = 0;
      client_requests_sent = 0;
      client_requests_received = 0;
    } and
    client_impl = {
      dummy_client_impl with            
      impl_client_val = c;
      impl_client_ops = client_ops;
    }
  in
  c  

let create_client key num =  
  let rec c = {
      client_client = client_impl;
      client_upload = None;
      client_kind = key;   
      client_source = None;
      client_sock = None;
      client_md4 = Md4.null;
      client_last_filereqs = 0;
      client_chunks = [||];
      client_block = None;
      client_zones = [];
      client_connection_control =  new_connection_control_recent_ok ( ());
      client_file_queue = [];
      client_tags = [];
      client_name = "";
      client_all_files = None;
      client_next_view_files = last_time () - 1;
      client_all_chunks = "";
      client_rating = 0;
      client_brand = Brand_unknown;
      client_checked = false;
      client_chat_port = 0 ; (** A VOIR : où trouver le 
            port de chat du client ? *)
      client_connected = false;
      client_power = !!upload_power;
      client_downloaded = Int64.zero;
      client_uploaded = Int64.zero;
      client_on_list = false;            
      client_banned = false;
      client_has_a_slot = false;
      client_overnet = false;
      client_score = 0;
      client_files = [];
      client_next_queue = 0;
      client_rank = 0;
      client_connect_time = 0;
      client_requests_received = 0;
      client_requests_sent = 0;
      } and
    client_impl = {
      dummy_client_impl with            
      impl_client_val = c;
      impl_client_ops = client_ops;
    }
  in
  Heap.set_tag c tag_client;
  CommonClient.new_client_with_num client_impl num;
  H.add clients_by_kind c;
  c

let new_client key =
  try
    H.find clients_by_kind { dummy_client with client_kind = key }
  with _ ->
      create_client key (book_client_num ())

let new_client_with_num key num =
  try
    H.find clients_by_kind { dummy_client with client_kind = key }
  with _ ->
      create_client key num
      
let find_client_by_key key =  
  H.find clients_by_kind { dummy_client with client_kind = key }
  
let client_type c =
  client_type (as_client c.client_client)

let friend_add c =
  friend_add (as_client c.client_client)
      
let set_client_name c name md4 =
  if name <> c.client_name || c.client_md4 <> md4 then begin
(*      hashtbl_remove clients_by_name c.client_name c;   *)

      c.client_name <- name;
      c.client_md4 <- md4;
      
      (*
      if not (Hashtbl.mem clients_by_name c.client_name) then
        Hashtbl.add clients_by_name c.client_name c;
*)
      
      try      
        let kind = Indirect_location (name, md4) in
        let cc = H.find clients_by_kind { dummy_client with client_kind = kind } in
        if cc != c && client_type cc = FriendClient then
          friend_add c
      with _ -> ()
    end
    
exception ClientFound of client
let find_client_by_name name =
  try
    H.iter (fun c ->
        if c.client_name = name then raise (ClientFound c)
    ) clients_by_kind;
    raise Not_found
  with ClientFound c -> c

let udp_servers_replies = (Hashtbl.create 127 : (Md4.t, server) Hashtbl.t)
  
let _ =
  option_hook max_hard_upload_rate (fun _ -> 
      TcpBufferedSocket.change_rate upload_control 
        (!!max_hard_upload_rate * 1024));  
  option_hook max_hard_download_rate (fun _ ->
      let rate = !!max_hard_download_rate in
      TcpBufferedSocket.change_rate download_control 
        (rate * 1024))  
    
let file_groups = (Hashtbl.create 1023 : (Md4.t, file_group) Hashtbl.t)
  
  
module UdpClientWHashtbl = Weak2.Make(struct
      type t = udp_client
      let hash c = Hashtbl.hash (c.udp_client_ip, c.udp_client_port)
      
      let equal x y = x.udp_client_port = y.udp_client_port
        && x.udp_client_ip = y.udp_client_ip
    end)

let udp_clients = UdpClientWHashtbl.create 1023

let local_mem_stats buf = 
  Gc.compact ();
  let client_counter = ref 0 in
  let unconnected_unknown_clients = ref 0 in
  let uninteresting_clients = ref 0 in
  let aliased_clients = ref 0 in
  let connected_clients = ref 0 in
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
      let num = client_num (as_client c.client_client) in
      incr client_counter;
      match c.client_sock with
        None -> begin
            match c.client_kind with
              Indirect_location _ -> incr unconnected_unknown_clients
            | _ -> ()
          end
      | Some sock ->
          let buf_len, nmsgs = TcpBufferedSocket.buf_size sock in
          (try
              Hashtbl.find connected_clients_by_num num
            with _ -> 
                incr connected_clients;
                waiting_msgs := !waiting_msgs + nmsgs;
                buffers := !buffers + buf_len;
(*
                Printf.bprintf buf "%d: %6d/%6d\n" num
                  buf_len nmsgs *)
          );
          if BasicSocket.closed (TcpBufferedSocket.sock sock) then
            incr closed_connections;
  ) clients_by_kind;
  
  let bad_clients_in_files = ref 0 in
  Hashtbl.iter (fun _ file ->
      Intmap.iter (fun _ c ->
          match c.client_sock with
            None -> begin
                match c.client_kind with
                  Indirect_location _ -> incr bad_clients_in_files
                | _ -> ()
              end
          | Some sock -> ()              
      ) file.file_locations;
  ) files_by_md4;
  
  Printf.bprintf buf "Clients: %d\n" !client_counter;
  Printf.bprintf buf "   Bad Clients: %d/%d\n" !unconnected_unknown_clients
    !bad_clients_in_files;
  Printf.bprintf buf "   Read Buffers: %d\n" !buffers;
  Printf.bprintf buf "   Write Messages: %d\n" !waiting_msgs;
  Printf.bprintf buf "   Uninteresting clients: %d\n" !uninteresting_clients;
  Printf.bprintf buf "   Connected clients: %d\n" !connected_clients;
  Printf.bprintf buf "   Aliased clients: %d\n" !aliased_clients;
  Printf.bprintf buf "   Closed clients: %d\n" !closed_connections;
  Printf.bprintf buf "   Unlocated clients: %d\n" !unlocated_client;
  Printf.bprintf buf "   Bad numbered clients: %d\n" !bad_numbered_clients;
  Printf.bprintf buf "   Dead clients: %d\n" !dead_clients;
  Printf.bprintf buf "   Disconnected aliases: %d\n" !disconnected_alias;
  ()
  
let client_num c = client_num (as_client c.client_client)  
let file_num c = file_num (as_file c.file_file)  
let server_num c = server_num (as_server c.server_server)  
  
let remove_client c =
  client_remove (as_client c.client_client);
(*  hashtbl_remove clients_by_kind c.client_kind c; *)
(*  hashtbl_remove clients_by_name c.client_name c *)
  ()

  
  (*
let check_useful_client c = 
  let useful = c.client_is_friend in
  if not useful then begin
      List.iter (fun file ->
          file.file_sources <- Intmap.remove (client_num c) file.file_sources;
          file.file_nlocations <- file.file_nlocations - 1;
      ) c.client_source_for;
      c.client_source_for <- [];
      remove_client c
    end
      *)

let friend_remove c = 
  friend_remove  (as_client c.client_client)

  (*
let remove_file_clients file =
  let locs = file.file_sources in
  file.file_sources <- Intmap.empty;
  file.file_nlocations <- 0;
  Intmap.iter (fun _ c ->
      if List.memq file c.client_source_for then 
        c.client_source_for <- List2.removeq file c.client_source_for;
      check_useful_client c
  ) locs  
    *)

let last_search = ref (Intmap.empty : int Intmap.t)
  
  (*
let remove_source file c =
  if List.memq file c.client_source_for then begin  
      file.file_sources <- Intmap.remove (client_num c) file.file_sources;
      file.file_nlocations <- file.file_nlocations - 1;
(*      Printf.printf "removed source %d" file.file_nlocations; print_newline (); *)
      c.client_source_for <- List2.removeq file c.client_source_for;
      check_useful_client c
    end
  
let new_source file c =
  if not (List.memq file c.client_source_for) then begin
(*      Printf.printf "New source (on %d)" file.file_nlocations;
      print_newline (); *)
      if file.file_nlocations >= !!max_sources_per_file then begin
(* find the oldest location, and remove it *)
(*          Printf.printf "Remove old loc"; print_newline (); *)
          let oldest_time = ref (last_time ()) in
          let oldest_client = ref None in
          let locs = file.file_sources in
          Intmap.iter (fun num c ->
              match c.client_sock with 
                Some _ -> ()
              | None ->
                  let last_conn = connection_last_conn 
                      c.client_connection_control in
                  if last_conn < !oldest_time then begin
                      oldest_time := last_conn;
                      oldest_client := Some c;
                    end
          ) locs;
          match !oldest_client with
(* We couldn't choose which client should be removed since
no sources have been connected yet. *)
            None -> 
              (* Printf.printf "couldnot remove"; print_newline (); *) ()
          | Some c ->
(*              Printf.printf "loc removed"; print_newline (); *)
              remove_source file c
        end;
      file.file_nlocations <- file.file_nlocations + 1;
      file.file_sources <- Intmap.add (client_num c) c file.file_sources;
      file_new_source (as_file file.file_file) (as_client c.client_client);
      c.client_source_for <- file :: c.client_source_for;
(*      Printf.printf "New source added %d" file.file_nlocations; *)
    end    
      *)
  
(* indexation *)
let comments = (Hashtbl.create 127 : (Md4.t,string) Hashtbl.t)

let comment_filename = Filename.concat file_basedir "comments.met"
  
let (results_by_md4 : (Md4.t, result) Hashtbl.t) = Hashtbl.create 1023
  
let history_file = Filename.concat file_basedir "history.met"
let history_file_oc = ref (None : out_channel option)

  
  
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

          
let all_servers () =
  Hashtbl.fold (fun key s l ->
      s :: l
  ) servers_by_key []


let _ =
  Heap.register_dumper "DonkeyGlobals" (fun _ ->
(* current_files *)
      Printf.printf "Current files: %d" (List.length !current_files);
      print_newline ();
(* clients_by_name *)
(*      let list = Hashtbl2.to_list clients_by_name in
      Printf.printf "Clients_by_name: %d" (List.length list);
      print_newline ();
      List.iter (fun c ->
          Printf.printf "[%d %s]" (client_num c)
          (if Hashtbl.mem clients_by_kind c.client_kind then "K" else " ") ;
     ) list;
      print_newline ();
*)
      
(* clients_by_kind *)
      let list = H.to_list clients_by_kind in
      Printf.printf "Clients_by_kind: %d" (List.length list);
      print_newline ();
      List.iter (fun c ->
          Printf.printf "[%d ok: %s tried: %s next: %s state: %d rating: %d]" (client_num c)
          (string_of_date (c.client_connection_control.control_last_ok))
          (string_of_date (c.client_connection_control.control_last_try))
          (string_of_date (connection_next_try c.client_connection_control))
          c.client_connection_control.control_state
          c.client_rating
          ;
          print_newline ();
     ) list;
      print_newline ();
  )
  
  
  
let (store: CommonTypes.result_info Store.t) = 
  Store.create (Filename.concat file_basedir "store")
  
module Document = struct
    type t = Store.index
      
    let num t = Store.index t
    let filtered t = Store.get_attrib store t
    let filter t bool = Store.set_attrib store t bool
  end

let doc_value doc = Store.get store doc
  
module DocIndexer = Indexer2.FullMake(Document)

open Document
  
let index = DocIndexer.create ()

let check_result r tags =
  if r.result_names = [] || r.result_size = Int64.zero then begin
      if !verbose then begin
          Printf.printf "BAD RESULT:";
          print_newline ();
          List.iter (fun tag ->
              Printf.printf "[%s] = [%s]" tag.tag_name
                (match tag.tag_value with
                  String s -> s
                | Uint64 i | Fint64 i -> Int64.to_string i
                | Addr _ -> "addr");
              print_newline ();
          ) tags;
        end;
      false
    end
  else true
    

let result_of_file md4 tags =
  
  let rec r = { 
      result_num = 0;
      result_network = network.network_num;
      result_md4 = md4;
      result_names = [];
      result_size = Int64.zero;
      result_tags = [];
      result_type = "";
      result_format = "";
      result_comment = "";
      result_done = false;
    } in
  List.iter (fun tag ->
      match tag with
        { tag_name = "filename"; tag_value = String s } ->
          r.result_names <- s :: r.result_names
      | { tag_name = "size"; tag_value = Uint64 v } ->
          r.result_size <- v;
      | { tag_name = "format"; tag_value = String s } ->
          r.result_tags <- tag :: r.result_tags;
          r.result_format <- s
      | { tag_name = "type"; tag_value = String s } ->
          r.result_tags <- tag :: r.result_tags;
          r.result_type <- s
      | _ ->
          r.result_tags <- tag :: r.result_tags
  ) tags;
  if check_result r tags then Some r else None
    
let _ =
  add_memstat "DonkeyGlobals" local_mem_stats

let string_of_file_state s =
  match  s with
  |  FileDownloading -> "FileDownloading"
  | FilePaused -> "FilePaused"
  | FileDownloaded -> "FileDownloaded"
  | FileShared     -> "FileShared"
  | FileCancelled -> "FileCancelled"
  | FileNew -> "FileNew"
