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

open CommonResult
open CommonFile
open CommonServer
open CommonComplexOptions
open CommonClient
open Options
open CommonTypes
open DonkeyTypes
open Unix
open DonkeyProtoCom
open BasicSocket
open CommonOptions
open DonkeyOptions
open CommonOptions
open CommonGlobals

let tag_client = 200
let tag_server = 201
let tag_file   = 202
    
(* HOOKS *)

let client_must_update c =
  client_must_update (as_client c.client_client)

let server_must_update s =
  server_must_update (as_server s.server_server)
  
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
let page_size = Int32.of_int 4096    


(* GLOBAL STATE *)
  
open DonkeyMftp
  
let client_tags = ref ([] : tag list)
let client_port = ref 0

let clients_by_kind = Hashtbl.create 127
let clients_by_name = Hashtbl.create 127

let local_searches = ref ([] : local_search list)
let nservers = ref 0
let servers_by_key = Hashtbl.create 127
let servers_list = ref []
let clients_list = ref []
let clients_list_len = ref 0
let remaining_time_for_clients = ref (60 * 15)
let location_counter = ref 0
let download_credit = ref 0 

let current_files = ref []
  
let sleeping = ref false
  
let last_xs = ref (-1)
  
let has_upload = ref 0
let upload_credit = ref 0
let zone_size = Int32.of_int (180 * 1024) 
let block_size = Int32.of_int 9728000
  
let queue_timeout = ref (60. *. 10.) (* 10 minutes *)
    
let nclients = ref 0
  
let connected_server_list = ref ([]  : server list)

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
  
  
let listen_sock = ref (None : TcpServerSocket.t option)
  
let reversed_sock = ref (None : TcpServerSocket.t option)


  
let udp_servers_list = ref []
let interesting_clients = ref []
  
  
(* 'NEW' FUNCTIONS *)  
let files_by_md4 = Hashtbl.create 127

let find_file md4 = Hashtbl.find files_by_md4 md4

        
let set_client_state c s =
  let cc = as_client c.client_client in
  let os = client_state cc in
  if os <> s then begin
      if os = Connected_busy then decr nclients;
      if s = Connected_busy then incr nclients;  
      set_client_state cc s;
    end
    
let set_server_state s state =
  let ss = as_server s.server_server in
  if server_state ss <> state then begin
      set_server_state ss state;
    end
    
let servers_ini_changed = ref true
let upload_clients = Fifo.create ()

let shared_files_info = Hashtbl.create 127
let new_shared = ref []
let shared_files = ref []
  
let new_file file_state file_name md4 file_size writable =
  try
    find_file md4 
  with _ ->
      let file_size =
        if file_size = Int32.zero then
          try
            Unix32.getsize32 file_name
          with _ ->
              failwith "Zero length file ?"
        else file_size
      in
      let nchunks = Int32.to_int (Int32.div file_size block_size) + 1 in
      let file_exists = Sys.file_exists file_name in
      let md4s = if file_size <= block_size then
            [md4] 
          else [] in
      let rec file = {
          file_file = file_impl;
          file_shared = None;
          file_exists = file_exists;
          file_md4 = md4;

          file_hardname = file_name;
          file_nchunks = nchunks;
          file_chunks = [||];
          file_chunks_age = [||];
          file_all_chunks = String.make nchunks '0';
          file_absent_chunks =   [Int32.zero, file_size];
          file_filenames = [];
          file_sources = Intmap.empty;
          file_nlocations = 0;
          file_md4s = md4s;
          file_available_chunks = Array.create nchunks 0;
          file_changed = FileInfoChange;
          file_format = Unknown_format;
          file_new_locations = true;
        }
      and file_impl = {
          dummy_file_impl with
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_size = file_size;
          impl_file_fd = Unix32.create file_name (if writable then
              [O_RDWR; O_CREAT] else [O_RDONLY]) 0o666;
        }
      in
      file_add file_impl file_state;
      Heap.set_tag file tag_file;
      Hashtbl.add files_by_md4 md4 file;
      file

let change_hardname file file_name =
  file.file_hardname <- file_name;
  let fd = file.file_file.impl_file_fd in
  file.file_file.impl_file_fd <- Unix32.create file_name [O_RDWR] 0o666;
  Unix32.close fd
  
let info_change_file file =
  file.file_changed <- FileInfoChange
          
let avail_change_file file = 
  match file.file_changed with
    FileInfoChange -> ()
  | _ -> file.file_changed <- FileAvailabilityChange
          
let add_client_chunks file client_chunks =
  for i = 0 to file.file_nchunks - 1 do
    if client_chunks.(i) then 
      let new_n = file.file_available_chunks.(i) + 1 in
      file.file_available_chunks.(i) <- new_n;
      if new_n = 1 then avail_change_file file
  done
      
let remove_client_chunks file client_chunks = 
  for i = 0 to file.file_nchunks - 1 do
    if client_chunks.(i) then
      let new_n = file.file_available_chunks.(i) - 1 in
      file.file_available_chunks.(i) <- new_n;
      if new_n = 0 then avail_change_file file;
      client_chunks.(i) <- false
  done

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
          server_connection_control = new_connection_control (last_time ());
          server_score = score;
          server_tags = [];
          server_nfiles = 0;
          server_nusers = 0;
          server_name = "";
          server_description = "";
          server_users = [];
          server_master = false;
          server_mldonkey = false;
          server_last_message = 0.0;
          server_queries_credit = 0;
          server_waiting_queries = [];
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
  

let new_client key =
  let c = 
  try
    Hashtbl.find clients_by_kind key
  with _ ->
      let rec c = {
          client_client = client_impl;
          client_upload = None;
          client_kind = key;   
          client_sock = None;
          client_md4 = Md4.null;
          client_chunks = [||];
          client_block = None;
          client_zones = [];
          client_connection_control =  new_connection_control (last_time ());
          client_file_queue = [];
          client_source_for = [];
          client_tags = [];
          client_name = "";
          client_all_files = None;
          client_next_view_files = last_time () -. 1.;
          client_all_chunks = "";
          client_rating = Int32.zero;
          client_is_mldonkey = 0;
          client_checked = false;
          client_chat_port = 0 ; (** A VOIR : où trouver le 
					 port de chat du client ? *)
        } and
          client_impl = {
            dummy_client_impl with            
            impl_client_val = c;
            impl_client_ops = client_ops;
        }
      in
      Heap.set_tag c tag_client;
      CommonClient.new_client client_impl;
        Hashtbl.add clients_by_kind key c;
      c
  in
  c
  
let client_type c =
  client_type (as_client c.client_client)

let friend_add c =
  friend_add (as_client c.client_client)
      
let set_client_name c name md4 =
  if name <> c.client_name || c.client_md4 <> md4 then begin
      hashtbl_remove clients_by_name c.client_name c;  

      c.client_name <- name;
      c.client_md4 <- md4;
      
      if not (Hashtbl.mem clients_by_name c.client_name) then
        Hashtbl.add clients_by_name c.client_name c;
      
      try      
        let kind = Indirect_location (name, md4) in
        let cc = Hashtbl.find clients_by_kind kind in
        if cc != c && client_type cc = FriendClient then
          friend_add c
      with _ -> ()
    end
    
let find_client_by_name name =
  Hashtbl.find clients_by_name name
    

let first_name file =
  match file.file_filenames with
    [] -> Filename.basename file.file_hardname
  | name :: _ -> name

let udp_servers_replies = Hashtbl.create 127
  
let _ =
  option_hook max_hard_upload_rate (fun _ -> 
      TcpBufferedSocket.change_rate upload_control 
        (!!max_hard_upload_rate * 1024));  
  option_hook max_hard_download_rate (fun _ ->
      let rate = !!max_hard_download_rate in
      TcpBufferedSocket.change_rate download_control 
        (rate * 1024))  
    
let file_groups = Hashtbl.create 1023
let udp_clients = Hashtbl.create 1023

let mem_stats buf = 
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
  Hashtbl.iter (fun _ c ->
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
            if c.client_source_for = [] then incr uninteresting_clients
            else begin
                List.iter (fun file ->
                    if not (Intmap.mem num file.file_sources) then
                      begin
                        incr unlocated_client;
                      end
                ) c.client_source_for;
              end;
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
                Printf.bprintf buf "%d: %6d/%6d\n" num
                  buf_len nmsgs
          );
          if BasicSocket.closed (TcpBufferedSocket.sock sock) then
            incr closed_connections;
  ) clients_by_kind;
  Printf.bprintf buf "Clients: %d\n" !client_counter;
  Printf.bprintf buf "   Bad Clients: %d\n" !unconnected_unknown_clients;
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
  
  let direct_locs = ref 0 in
  let indirect_locs = ref 0 in
  let aliased_locations = ref 0 in
  let loc_unaware = ref 0 in
  let unregistered_locs = ref 0 in
  Hashtbl.iter (fun md4 file ->
      Printf.bprintf buf "FILE %s\n" file.file_hardname;
      let len = Intmap.length file.file_sources in
      Printf.bprintf buf "  locs %d\n" len;
      direct_locs := !direct_locs + len;
      
      (*
      Intmap.iter (fun _ c ->
          try
            let cc = Hashtbl.find clients_by_num num in
            if not (List.memq file c.client_files) then 
              incr loc_unaware;
            if cc != c then incr aliased_locations;
          with _ -> incr unregistered_locs;
) file.file_indirect_locations;
  
      Intmap.iter (fun _ c ->
          try
            let cc = Hashtbl.find clients_by_num c.client_num in
            if not (List.memq file c.client_files) then 
              incr loc_unaware;
            if cc != c then incr aliased_locations;
          with _ -> incr unregistered_locs;
      ) file.file_known_locations;
      *)
  ) files_by_md4;
  Printf.bprintf buf "Direct locs: %d\n" !direct_locs;
  Printf.bprintf buf "Indirect locs: %d\n" !indirect_locs;
  Printf.bprintf buf "Aliased locs: %d\n" !aliased_locations;
  Printf.bprintf buf "Unregitered locs: %d\n" !unregistered_locs;
  Printf.bprintf buf "Unaware locs: %d\n" !loc_unaware;
  ()
  
let client_num c = client_num (as_client c.client_client)  
let file_num c = file_num (as_file c.file_file)  
let server_num c = server_num (as_server c.server_server)  
  
let remove_client c =
  client_remove (as_client c.client_client);
  hashtbl_remove clients_by_kind c.client_kind c;
  hashtbl_remove clients_by_name c.client_name c

let check_useful_client c = 
  let useful =
    client_type c <> NormalClient ||
    c.client_sock <> None ||
    match c.client_kind with
      Known_location _ -> c.client_source_for != [] 
    | Indirect_location _ -> 
        c.client_source_for != [] || c.client_upload != None
  in
  if not useful then begin
      List.iter (fun file ->
          file.file_sources <- Intmap.remove (client_num c) file.file_sources;
          file.file_nlocations <- file.file_nlocations - 1;
      ) c.client_source_for;
      c.client_source_for <- [];
      remove_client c
    end
    
let friend_remove c = 
  friend_remove  (as_client c.client_client)
    
let remove_file_clients file =
  let locs = file.file_sources in
  file.file_sources <- Intmap.empty;
  file.file_nlocations <- 0;
  Intmap.iter (fun _ c ->
      if List.memq file c.client_source_for then 
        c.client_source_for <- List2.removeq file c.client_source_for;
      check_useful_client c
  ) locs  
  
let last_search = ref Intmap.empty
      
let remove_source file c =
  if List.memq file c.client_source_for then begin  
      file.file_sources <- Intmap.remove (client_num c) file.file_sources;
      file.file_nlocations <- file.file_nlocations - 1;
      c.client_source_for <- List2.removeq file c.client_source_for;
      check_useful_client c
    end
  
let new_source file c =
  if not (List.memq file c.client_source_for) then begin
      if file.file_nlocations >= !!max_sources_per_file then begin
(* find the oldest location, and remove it *)
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
            None -> assert false
          | Some c ->  remove_source file c
        end;
      file.file_nlocations <- file.file_nlocations + 1;
      file.file_sources <- Intmap.add (client_num c) c file.file_sources;
      file_new_source (as_file file.file_file) (as_client c.client_client);
      c.client_source_for <- file :: c.client_source_for
    end    
   
let file_state file =
  CommonFile.file_state (as_file file.file_file)
   
let file_must_update file =
  file_must_update (as_file file.file_file)
    
let client_state client =
  CommonClient.client_state (as_client client.client_client)
    
let client_new_file client r =
  client_new_file (as_client client.client_client) ""
  (as_result r.result_result)
    
let server_state server =
  CommonServer.server_state (as_server server.server_server)
  
(* indexation *)
let comments = Hashtbl.create 127

let comment_filename = Filename.concat file_basedir "comments.met"

let name_bit = 1
(* "size" *)
(* "bitrate" *)
let artist_bit = 2 (* tag "Artiste" *)
let title_bit = 4  (* tag "Title" *)
let album_bit = 8 (* tag "Album" *)
let media_bit = 16 (* "type" *)
let format_bit = 32 (* "format" *)
  
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
  
let (results_by_md4 : (Md4.t, result) Hashtbl.t) = Hashtbl.create 1023
  
let history_file = Filename.concat file_basedir "history.met"
let history_file_oc = ref None

  
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file.file_file.impl_file_downloaded
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
  
  
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
