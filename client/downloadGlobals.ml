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
open DownloadTypes
open Unix
open Mftp_comm
open BasicSocket
open Gui_types
  
(* HOOKS *)
  
let new_server_hook = ref (fun s -> ())      
let server_change_hook = ref (fun (s: server) -> 
      ())
let client_change_hook = ref (fun (c: client) -> 
      c.client_changed <- NoChange)

let say_hook = ref (fun (c:client option) (s:string) -> ())

  
(* These 3 hooks are used to handle connections with a server. They are mainly
  used in DownloadFiles *)
let server_is_connected_hook = ref (fun
      (s: server) 
      (sock: TcpClientSocket.t) -> ())
let received_from_server_hook = ref (fun 
      (s: server) 
      (sock: TcpClientSocket.t) 
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

let min_retry_delay = ref 3600.
  
let new_connection_control last_conn = {
    control_next_try = last_time () -. 1.;
    control_last_conn = last_conn;
    control_next_delay = !min_retry_delay;
  }
  
let connection_ok cc = 
  cc.control_next_delay <- !min_retry_delay;
  cc.control_last_conn <- last_time ();
  cc.control_next_try <- last_time () +. !min_retry_delay
  
let connection_try cc =
  cc.control_next_try <- last_time () +. cc.control_next_delay

let half_day = 12. *. 3600.
  
let connection_failed cc =
  cc.control_next_delay <- min (cc.control_next_delay *. 2.) half_day

let connection_can_try cc =
  cc.control_next_try < last_time ()
  
let connection_must_try cc =
  cc.control_next_try <- last_time () -. 1.

let connection_set_last_conn cc lc =
  cc.control_last_conn <- lc

let connection_last_conn cc =
  cc.control_last_conn
  
  
  
open Mftp

let _ = Random.self_init ()
  
let client_ip = ref (Ip.of_ints (0,0,0,0))
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
let location_counter = ref 0
let upload_counter = ref 0
let download_credit = ref 0 
let nshared_files = ref 0

let sleeping = ref false
let indirect_clients_by_md4 = Hashtbl.create 127
  
let last_xs = ref (-1)

let max_allowed_connected_servers = 5
  
let has_upload = ref 0
let upload_credit = ref 0
let zone_size = Int32.of_int (180 * 1024) 
let block_size = Int32.of_int 9728000
  
let queue_timeout = ref (60. *. 10.) (* 10 minutes *)
      
let nclients = ref 0
let (files_by_anon_client: (Ip.t * int * Ip.t,file * bool ref) Hashtbl.t) = Hashtbl.create 127
  
let connected_server_list = ref ([]  : server list)
    
let dbserver_sock = ref (None : TcpClientSocket.t option)

let user_socks = ref ([] : TcpClientSocket.t list)

let gui_server_sock = ref (None : TcpServerSocket.t option)
let udp_sock = ref (None: UdpSocket.t option)
let udp_servers_list = ref []
let interesting_clients = ref []
let dialog_history = ref []
  
  
(* 'NEW' FUNCTIONS *)  
let files_by_md4 = Hashtbl.create 127

let find_file md4 = Hashtbl.find files_by_md4 md4

let upload_clients = Fifo.create ()

let shared_files_info = Hashtbl.create 127
let new_shared = ref []
let shared_files = ref []
let download_counter = ref 0
  
let new_file file_name md4 file_size =
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
      let file = {
          file_shared = false;
          file_exists = file_exists;
          file_md4 = md4;
          file_name = file_name;
          file_size = file_size;
          file_nchunks = nchunks;
          file_chunks = [||];
          file_fd = None;
          file_all_chunks = String.make nchunks '0';
          file_absent_chunks =   [Int32.zero, file_size];
          file_filenames = [];
          file_known_locations = [];
          file_indirect_locations = [];
          file_md4s = md4s;
          file_downloaded = Int32.zero;
          file_num = !file_counter;
          file_state = FileDownloading;
          file_available_chunks = Array.create nchunks 0;
          file_changed = BigChange;
          file_last_time = last_time ();
          file_last_downloaded = Int32.zero;
          file_last_rate = 0.0;
          file_format = Unknown_format;
          file_upload_blocks = 0;
          file_upload_requests = 0;
        }
      in
      Hashtbl.add files_by_md4 md4 file;
      file

      
let big_change_file file =
  file.file_changed <- BigChange
          
let small_change_file file = 
  match file.file_changed with
    BigChange -> ()
  | _ -> file.file_changed <- SmallChange
          
let add_client_chunks file client_chunks =
  for i = 0 to file.file_nchunks - 1 do
    if client_chunks.(i) then 
      let new_n = file.file_available_chunks.(i) + 1 in
      file.file_available_chunks.(i) <- new_n;
      if new_n = 1 then small_change_file file
  done
      
let remove_client_chunks file client_chunks = 
  for i = 0 to file.file_nchunks - 1 do
    if client_chunks.(i) then
      let new_n = file.file_available_chunks.(i) - 1 in
      file.file_available_chunks.(i) <- new_n;
      if new_n = 0 then small_change_file file;
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
          server_cid = !client_ip;
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
          server_changed = BigChange;
          server_name = "";
          server_description = "";
          server_users = [];
          server_master = false;
        }
      in
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
      | Some sock -> shutdown (TcpClientSocket.sock sock) "remove server");
    s.server_state <- Removed;
    !server_change_hook s
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
            client_files = [];
            client_is_friend = NotAFriend;
            client_tags = [];
            client_name = "";
            client_all_files = None;
            client_next_view_files = last_time () -. 1.;
            client_all_chunks = "";
            client_changed = SmallChange;
            client_rating = Int32.zero;
            client_is_mldonkey = 0;
            client_alias = None;
          } in
        c
    | _ ->
        try
          Hashtbl.find clients_by_kind key
        with _ ->
            incr client_counter;
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
                client_files = [];
                client_is_friend = NotAFriend;
                client_tags = [];
                client_name = "";
                client_all_files = None;
                client_next_view_files = last_time () -. 1.;
                client_all_chunks = "";
                client_changed = SmallChange;
                client_rating = Int32.zero;
                client_is_mldonkey = 0;
                client_alias = None;
              } in
            Hashtbl.add clients_by_kind key l;
            l
          
  in
  Hashtbl.add clients_by_num !client_counter c;
  c
  
let remove_client c =
  Hashtbl.remove clients_by_num c.client_num
  
let find_client num =
  Hashtbl.find clients_by_num num
  
let file_fd file =
  match file.file_fd with
  | Some fd -> fd
  | None ->
      let file_name = file.file_name in
      let fd = Unix.openfile file_name [O_RDWR; O_CREAT] 0o666 in
      file.file_fd <- Some fd;
      fd
      
      
let first_name file =
  match file.file_filenames with
    [] -> Filename.basename file.file_name
  | name :: _ -> name

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
