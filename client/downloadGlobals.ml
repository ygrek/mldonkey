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
open Options
open DownloadTypes
open Unix
open Mftp_comm
open BasicSocket
open Gui_types
open DownloadOptions

let tag_client = 200
let tag_server = 201
let tag_file   = 202
  
  
let one_day = 3600. *. 60.
let half_day = one_day /. 2.
  
let printf_char c =
  if !!verbose then 
    (print_char c; Pervasives.flush Pervasives.stdout)
    
let printf_string c =
  if !!verbose then 
    (print_string c; Pervasives.flush Pervasives.stdout)
    
(* HOOKS *)
  
let new_server_hook = ref (fun s -> ())      
let server_change_hook = ref (fun (s: server) -> 
      s.server_changed <- NoServerChange)

let client_change_hook = ref (fun (c: client) -> 
      c.client_changed <- NoClientChange)

let say_hook = ref (fun (c:client option) (s:string) -> ())

  
(* These 3 hooks are used to handle connections with a server. They are mainly
  used in DownloadFiles *)
let server_is_connected_hook = ref (fun
      (s: server) 
      (sock: server_sock) -> ())
let received_from_server_hook = ref (fun 
      (s: server) 
      (sock: server_sock) 
      (t: Mftp_server.t) -> ())
let server_is_disconnected_hook = ref (fun 
      (s: server) -> ())
  
let friend_change_hook = ref (fun (friend: client) -> ())
  
(* hook called when something changed on a file. Currently, it is only called
  when a file is added or removed. *)
let file_change_hook = ref (fun (file: file) -> ())
  
  (* CONSTANTS *)
let page_size = Int32.of_int 4096    


(* GLOBAL STATE *)

  
let new_connection_control last_conn = {
    control_next_try = last_time () -. 1.;
    control_last_conn = last_conn;
    control_next_delay = !!min_retry_delay;
  }
  
let connection_ok cc = 
  cc.control_next_delay <- !!min_retry_delay;
  cc.control_last_conn <- last_time ();
  cc.control_next_try <- last_time () +. !!min_retry_delay
  
let connection_try cc =
  cc.control_next_try <- last_time () +. cc.control_next_delay

let connection_failed cc =
  cc.control_next_delay <- minf (cc.control_next_delay *. 2.) half_day

let connection_can_try cc =
  cc.control_next_try < last_time ()
  
let connection_must_try cc =
  cc.control_next_try <- last_time () -. 1.

let connection_set_last_conn cc lc =
  cc.control_last_conn <- lc

let connection_last_conn cc =
  cc.control_last_conn

let connection_delay cc =
  cc.control_next_try <- maxf cc.control_next_try
    (last_time () +. !!min_retry_delay)
  
open Mftp
  
let client_tags = ref ([] : tag list)
let client_port = ref 0

let clients_by_kind = Hashtbl.create 127

let clients_by_num = Hashtbl.create 127
let searches = ref ([] : search list)
let client_counter = ref 0        
let file_counter = ref 0  
let search_counter = ref 0    
let guis = ref ([] : gui_record list)  
let nservers = ref 0
let servers_by_key = Hashtbl2.create 127
let servers_by_num = Hashtbl.create 127
let server_counter = ref 0
let servers_list = ref []
let clients_list = ref []
let clients_list_len = ref 0
let remaining_time_for_clients = ref (60 * 15)
let location_counter = ref 0
let upload_counter = ref 0
let download_credit = ref 0 
let nshared_files = ref 0

let sleeping = ref false
let indirect_clients_by_md4 = Hashtbl.create 127
  
let last_xs = ref (-1)
  
let has_upload = ref 0
let upload_credit = ref 0
let zone_size = Int32.of_int (180 * 1024) 
let block_size = Int32.of_int 9728000
  
let queue_timeout = ref (60. *. 10.) (* 10 minutes *)
    
let nclients = ref 0
let (files_by_anon_client: (Ip.t * int * Ip.t,file * bool ref) Hashtbl.t) = Hashtbl.create 127
  
let connected_server_list = ref ([]  : server list)
    

let user_socks = ref ([] : TcpBufferedSocket.t list)

let gui_server_sock = ref (None : TcpServerSocket.t option)
let udp_sock = ref (None: UdpSocket.t option)
let udp_servers_list = ref []
let interesting_clients = ref []
let dialog_history = ref []
  
  
(* 'NEW' FUNCTIONS *)  
let files_by_md4 = Hashtbl.create 127

let find_file md4 = Hashtbl.find files_by_md4 md4

        
let set_client_state c s =
  if c.client_state <> s then begin
      if c.client_state = Connected_busy then decr nclients;
      if s = Connected_busy then incr nclients;  
      c.client_state <- s;
      c.client_changed <- ClientStateChange;
      !client_change_hook c
    end
    
let set_server_state s state =
  if s.server_state <> state then begin
      s.server_state <- state;
      s.server_changed <- ServerStateChange;
      !server_change_hook s
    end
    
let servers_ini_changed = ref true
let upload_clients = Fifo.create ()

let shared_files_info = Hashtbl.create 127
let new_shared = ref []
let shared_files = ref []
let download_counter = ref 0
  
let new_file file_name md4 file_size writable =
  try
    find_file md4 
  with _ ->
      let file_size =
        if file_size = Int32.zero then
          try
            Printf.printf "CHECK FILE SIZE"; print_newline ();
            Unix32.getsize32 file_name
          with _ ->
              failwith "Zero length file ?"
        else file_size
      in
      incr file_counter;
      let nchunks = Int32.to_int (Int32.div file_size block_size) + 1 in
      let file_exists = Sys.file_exists file_name in
      let md4s = if file_size <= block_size then
            [md4] 
          else [] in
      let rec file = {
          file_shared = false;
          file_exists = file_exists;
          file_md4 = md4;
          file_age = last_time ();
          file_hardname = file_name;
          file_size = file_size;
          file_nchunks = nchunks;
          file_chunks = [||];
          file_chunks_age = [||];
          file_fd = Unix32.create file_name (if writable then
              [O_RDWR; O_CREAT] else [O_RDONLY]) 0o666;
          file_all_chunks = String.make nchunks '0';
          file_absent_chunks =   [Int32.zero, file_size];
          file_filenames = [];
          file_known_locations = Intmap.empty;
          file_nlocations = 0;
          file_indirect_locations = Intmap.empty;
          file_md4s = md4s;
          file_downloaded = Int32.zero;
          file_num = !file_counter;
          file_state = FileDownloading;
          file_available_chunks = Array.create nchunks 0;
          file_changed = FileInfoChange;
          file_last_time = last_time ();
          file_last_downloaded = Int32.zero;
          file_last_rate = 0.0;
          file_format = Unknown_format;
          file_upload_blocks = 0;
          file_upload_requests = 0;
          file_new_locations = true;
        }
      in
      Heap.set_tag file tag_file;
      Hashtbl.add files_by_md4 md4 file;
      file

let change_hardname file file_name =
  file.file_hardname <- file_name;
  let fd = file.file_fd in
  file.file_fd <- Unix32.create file_name [O_RDWR; O_CREAT] 0o666;
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
    Hashtbl2.find servers_by_key key
  with _ ->
      incr server_counter;
      let s = { 
          server_next_udp = last_time ();
          server_ip = ip;     
          server_cid = !!client_ip;
          server_port = port; 
          server_sock = None; 
          server_num = !server_counter; 
          server_nqueries = 0;
          server_search_queries = Fifo.create ();
          server_users_queries = Fifo.create ();
          server_connection_control = new_connection_control (last_time ());
          server_score = score;
          server_tags = [];
          server_nfiles = 0;
          server_nusers = 0;
          server_state = NotConnected;
          server_changed = ServerInfoChange;
          server_name = "";
          server_description = "";
          server_users = [];
          server_master = false;
        }
      in
      Heap.set_tag s tag_server;
      Hashtbl2.add servers_by_key key s;
      Hashtbl.add servers_by_num !server_counter s;
      !server_change_hook s;
      s      

let find_server ip port =
  let key = (ip, port) in
  Hashtbl2.find servers_by_key key  

let remove_server ip port =
  let key = (ip, port) in
  let s = Hashtbl2.find servers_by_key key in
  try
    Hashtbl2.remove servers_by_key key;
    Hashtbl.remove servers_by_num s.server_num;
    servers_list := List2.removeq s !servers_list ;
    (match s.server_sock with
        None -> ()
      | Some sock -> shutdown (TcpBufferedSocket.sock sock) "remove server");
    set_server_state s  Removed;
  with _ -> ()
  
let indirect_friends = Hashtbl.create 127
      
let new_client key =
  incr client_counter;
  let c =  
    match key with 
      Indirect_location ->
        let c = {
            client_upload = None;
            client_kind = key;   
            client_sock = None;
            client_md4 = Md4.null;
            client_chunks = [||];
            client_queries = Fifo.create ();
            client_block = None;
            client_zones = [];
            client_connection_control =  new_connection_control (last_time ());
            client_state = NotConnected;
            client_num = !client_counter;
            client_file_queue = [];
            client_files = [];
            client_is_friend = NotAFriend;
            client_tags = [];
            client_name = "";
            client_all_files = None;
            client_next_view_files = last_time () -. 1.;
            client_all_chunks = "";
            client_changed = ClientInfoChange;
            client_rating = Int32.zero;
            client_is_mldonkey = 0;
            client_alias = None;
            client_aliases = [];
            client_checked = false;
          } in
        c
    | _ ->
        try
          Hashtbl.find clients_by_kind key
        with _ ->
            let l = {
                client_upload = None;
                client_kind = key;   
                client_sock = None;
                client_md4 = Md4.null;
                client_chunks = [||];
                client_queries = Fifo.create ();
                client_block = None;
                client_zones = [];
                client_connection_control =  new_connection_control (last_time ());
                client_state = NotConnected;
                client_num = !client_counter;
                client_file_queue = [];
                client_files = [];
                client_is_friend = NotAFriend;
                client_tags = [];
                client_name = "";
                client_all_files = None;
                client_next_view_files = last_time () -. 1.;
                client_all_chunks = "";
                client_changed = ClientInfoChange;
                client_rating = Int32.zero;
                client_is_mldonkey = 0;
                client_alias = None;
                client_aliases = [];
                client_checked = false;
              } in
            
            Hashtbl.add clients_by_kind key l;
            l
          
  in
  Hashtbl.add clients_by_num c.client_num c;
  Heap.set_tag c tag_client;
  c
  
let remove_client c =
  Hashtbl.remove clients_by_num c.client_num
  
let find_client num =
  Hashtbl.find clients_by_num num      
      
let first_name file =
  match file.file_filenames with
    [] -> Filename.basename file.file_hardname
  | name :: _ -> name

let udp_servers_replies = Hashtbl.create 127
      
let exit_handlers = ref []
let do_at_exit f =
  exit_handlers := f :: !exit_handlers
      
let exit_properly _ =
(*  Printf.printf "exit_properly handlers"; print_newline (); *)
  List.iter (fun f -> try 
(*        Printf.printf "exit_properly handler ..."; print_newline (); *)
        f () ;
(*        Printf.printf "exit_properly done"; print_newline (); *)
      with e -> 
          Printf.printf "exit_properly (exception %s)"
            (Printexc.to_string e); print_newline ();
  ) !exit_handlers;
(*  Printf.printf "exit_properly DONE"; print_newline (); *)
  Pervasives.exit 0
  
let can_open_connection () =
  nb_sockets () < !!max_opened_connections 
  
let upload_control = TcpBufferedSocket.create_write_bandwidth_controler 
    (!!max_hard_upload_rate * 1024)
  
let download_control = TcpBufferedSocket.create_read_bandwidth_controler 
    (!!max_hard_download_rate * 1024)
  
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
  Hashtbl.iter (fun num c ->
      if c.client_num <> num then begin
          incr bad_numbered_clients;
          try
            let cc = Hashtbl.find clients_by_num c.client_num in
            if cc.client_sock = None then incr disconnected_alias;
          with _ -> incr dead_clients;
        end;
      incr client_counter;
      begin
        match c.client_alias with
          None -> ()
        | Some _ -> incr aliased_clients;
      end;
      match c.client_sock with
        None -> begin
            if c.client_files = [] then incr uninteresting_clients
            else begin
                List.iter (fun file ->
                    if not (Intmap.mem c.client_num file.file_known_locations) &&
                      not (Intmap.mem c.client_num file.file_indirect_locations) then
                      begin
                        incr unlocated_client;
                      end
                ) c.client_files;
              end;
            match c.client_kind with
              Indirect_location -> incr unconnected_unknown_clients
            | _ -> ()
          end
      | Some sock ->
          let buf_len, nmsgs = TcpBufferedSocket.buf_size sock in
          (try
              Hashtbl.find connected_clients_by_num c.client_num
            with _ -> 
                incr connected_clients;
                waiting_msgs := !waiting_msgs + nmsgs;
                buffers := !buffers + buf_len;
                Hashtbl.add connected_clients_by_num c.client_num ();
                Printf.bprintf buf "%d: %6d/%6d\n" c.client_num
                  buf_len nmsgs
          );
          if BasicSocket.closed (TcpBufferedSocket.sock sock) then
            incr closed_connections;
  ) clients_by_num;
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
      let len = Intmap.length file.file_known_locations in
      Printf.bprintf buf "  known locs %d\n" len;
      direct_locs := !direct_locs + len;
      
      let len = Intmap.length file.file_indirect_locations in
      Printf.bprintf buf "  indirect locs %d\n" len;
      indirect_locs := !indirect_locs + len;
      Intmap.iter (fun _ c ->
          try
            let cc = Hashtbl.find clients_by_num c.client_num in
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
      
  ) files_by_md4;
  Printf.bprintf buf "Direct locs: %d\n" !direct_locs;
  Printf.bprintf buf "Indirect locs: %d\n" !indirect_locs;
  Printf.bprintf buf "Aliased locs: %d\n" !aliased_locations;
  Printf.bprintf buf "Unregitered locs: %d\n" !unregistered_locs;
  Printf.bprintf buf "Unaware locs: %d\n" !loc_unaware;
  ()

let remove_useless_client c =
  match c.client_sock with
    Some _ -> ()
  | None ->
      if c.client_files = [] then
        remove_client c
  
let remove_file_clients file =
  let known_locs = file.file_known_locations in
  let other_locs = file.file_indirect_locations in
  file.file_indirect_locations <- Intmap.empty;
  file.file_known_locations <- Intmap.empty;
  file.file_nlocations <- 0;
  Intmap.iter (fun _ c ->
      if not (List.memq file c.client_files) then 
        (Printf.printf "direct location wasn't known by client"; print_newline ())
      else
        c.client_files <- List2.removeq file c.client_files;
      remove_useless_client c
  ) known_locs;
    Intmap.iter (fun _ c ->
      if not (List.memq file c.client_files) then 
        (Printf.printf "location wasn't known by client"; print_newline ())
      else
        c.client_files <- List2.removeq file c.client_files;
      remove_useless_client c
  ) other_locs
  

let last_search = ref Intmap.empty
  
(* when purging location, what to do with the client. 
Maybe it is still useful ? *)
  
let new_known_location file c =
  if file.file_nlocations = !!max_sources_per_file then begin
(* find the oldest location, and remove it *)
      let oldest_time = ref (last_time ()) in
      let oldest_num = ref (-1) in
      let locs = file.file_known_locations in
      Intmap.iter (fun num c ->
          match c.client_sock with 
            Some _ -> ()
          | None ->
              let last_conn = connection_last_conn 
                c.client_connection_control in
              if last_conn < !oldest_time then begin
                  oldest_time := last_conn;
                  oldest_num := num;
                end
      ) locs;
      file.file_known_locations <- Intmap.remove !oldest_num locs;
      ()
    end else
    file.file_nlocations <- file.file_nlocations + 1;
  file.file_known_locations <- Intmap.add c.client_num c 
    file.file_known_locations
