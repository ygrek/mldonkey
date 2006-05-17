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

open CommonInteractive
open Int64ops
open Queues
open Printf2
open Md4
open BasicSocket
open Options
open TcpBufferedSocket

open CommonHosts
open CommonOptions
open CommonClient
open CommonUser
open CommonTypes
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonFile
open CommonGlobals
open CommonDownloads  
open CommonNetwork
open CommonSwarming
  
open GnutellaTypes
open GnutellaOptions
open GnutellaNetwork

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[Gnutella] "
    (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[Gnutella] "
    (log_time ()); lprintf

let should_update_shared_files = ref false

  
  
let network = new_network "GNUT" "Gnutella"  
      [ 
    NetworkHasSupernodes; 
    NetworkHasSearch;
    NetworkHasUpload;
    NetworkHasMultinet;
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
let file_state file =  file_state (as_file file)
let file_num file =  file_num (as_file file)
let file_must_update file =  file_must_update (as_file file)
let client_must_update client = client_must_update (as_client client)

let current_files = ref ([] : GnutellaTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)
  
(*let hosts_by_key = Hashtbl.create 103 *)

let (searches_by_uid : (Md4.t, local_search) Hashtbl.t) = Hashtbl.create 11

  (*
let redirector_connected = ref false
(* let redirectors_ips = ref ( [] : Ip.t list) *)
let redirectors_to_try = ref ( [] : string list)
  *)
  

(*let (shareds_by_uid : (uid_type, shared) Hashtbl.t) = Hashtbl.create 13 *)
let files_by_uid = Hashtbl.create 13
(* let files_by_key = Hashtbl.create 13 *)

let (users_by_uid ) = Hashtbl.create 127
let (clients_by_uid ) = Hashtbl.create 127
(* We don't want to support this feature anymore as it is too old. 

  let results_by_key = Hashtbl.create 127 *)
  
  
(* TODO RESULT *)
let (results_by_uid : (uid_type, result) Hashtbl.t) = Hashtbl.create 127 

let max_upload_buffer_len = 102400
let upload_buffer = String.create max_upload_buffer_len
  
(***************************************************************


             HOST SCHEDULER


****************************************************************)
  
let ready _ = false
  
(* From the main workflow, hosts are moved to these workflows when they
are ready to be connected. They will only be connected when connections
will be available. We separate g1/g2, and g0 (unknown kind). *)
let (ultrapeers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* peers are only tested when no ultrapeers are available... *)
let (peers_waiting_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that we should try to contact by UDP *)
let (waiting_udp_queue : host Queue.t) = Queues.workflow ready
  
(* These are the peers that have replied to our UDP requests *)
let (active_udp_queue : host Queue.t) = Queues.fifo ()

let nservers = ref 0

let connected_servers = ref ([] : server list)
  
  
module H = CommonHosts.Make(struct
      include GnutellaTypes
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

let find_server ip port =
  try 
    let h = Hashtbl.find H.hosts_by_key (ip,port) in
    h.host_server
  with _ -> None

let new_server ip port =
  let h = H.new_host ip port Ultrapeer in
  match h.host_server with
    Some s -> s
  | None ->
      let rec s = {
          server_server = server_impl;
          server_ciphers = None;
          server_host = h;
          server_sock = NoConnection;
          server_agent = "<unknown>";
          server_description = "";
          server_nfiles = Int64.zero;
          server_nkb = 0;
          server_nusers = Int64.zero;
          server_maxnusers = 0L;
          server_need_qrt = true;
          server_ping_last = Md4.random ();
          server_last_lni = 0;
          server_nfiles_last = Int64.zero;
          server_nkb_last = 0;
          server_vendor = "";
          
          server_connected = zero;
          server_query_key = NoUdpSupport;
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
      h.host_on_remove <- (fun _ -> server_remove (as_server server_impl));
      s
      
let extract_uids arg = Uid.expand [Uid.of_string arg]
  
let result_sources = Hashtbl.create 1000
  
let add_source r (s : user) (index : file_uri) =
  let ss = 
    try
      Hashtbl.find result_sources r.stored_result_num
    with _ ->
        let ss = ref [] in
        Hashtbl.add result_sources r.stored_result_num ss;
        ss
  in
  let key = (s, index) in
  if not (List.mem_assq key !ss) then begin
      ss := (key, last_time ()) :: !ss
    end

let new_result file_name file_size (tags : CommonTypes.tag list) (uids : Uid.t list) sources =
  match uids with
    [] -> (*
        lprintf "New result by key\n"; 
        let key = (file_name, file_size) in
        try
          Hashtbl.find results_by_key key
        with _ ->
            let r = { dummy_result with
                result_names = [file_name];
                result_size = file_size;
                result_tags = tags;
(* TODO: result_netfid, result_network *)
                result_uids = uids;
              }
            in
            let r = update_result_num r in
            Hashtbl.add results_by_key key r;
    r) *)
      failwith "Result without UID dropped"
  | uid :: other_uids ->
      if !verbose then
        lprintf "New result by UID\n"; 
      let rs = 
        try
          let r = Hashtbl.find results_by_uid (Uid.to_uid uid) in
          increment_avail r
        with _ -> 

            let tags = update_or_create_avail tags in 

            let r = { dummy_result with
                result_names = [file_name];
                result_size = file_size;
                result_tags = tags;
                result_uids = uids;
                result_source_network = network.network_num;
                }
            in
            let rs = update_result_num r in
            Hashtbl.add results_by_uid (Uid.to_uid uid) rs;
            rs
      in
(*      let r = IndexedResults.get_result rs in
      let rec iter_uid uid =
        if not (List.mem uid r.result_uids) then begin
            r.result_uids <- uid :: r.result_uids;
            (try
                let rrs = Hashtbl.find results_by_uid uid in
                if rs != rrs then 
                  let result_uids = rr.result_uids in
                  rr.result_uids <- [];
                  List.iter (fun uid -> 
                      Hashtbl.remove results_by_uid uid) result_uids;
                  List.iter (fun uid -> iter_uid uid) result_uids;
                  List.iter (fun ( (s: user) , (index: file_uri) ) ->
                      add_source r s index
                  ) sources;
              with _ -> ());
            
            Hashtbl.add results_by_uid uid r;
          end
      in
List.iter iter_uid other_uids;
  *)
      rs

let megabyte = Int64.of_int (1024 * 1024)
let megabytes10 = Int64.of_int (10 * 1024 * 1024)
      
let new_file file_temporary file_name file_size file_uids = 
  let file_temp = Filename.concat !!temp_directory file_temporary in
  let t = Unix32.create_rw file_temp in
  let rec file = {
      file_file = file_impl;
      file_temp = file_temporary;
      file_name = file_name;
      file_clients = [];
      file_uids = file_uids;
      file_swarmer = None;
      file_searches = [];
      file_filenames = [file_name, GuiTypes.noips()];
      file_clients_queue = Queues.workflow (fun _ -> false);
      file_nconnected_clients = 0;      
      file_ttr = None;
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = Some t;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();          
      impl_file_best_name = file_name;
    } 
  in
  if !verbose then
    lprintf_nl () "SET SIZE : %Ld\n" file_size;
  let kernel = CommonSwarming.create_swarmer file_temp file_size 
      (Int64.of_int (256 * 1024))  in
  let swarmer = CommonSwarming.create kernel (as_file file) megabyte in
  CommonSwarming.set_verifier swarmer ForceVerification;
  
(* TODO: we could generalize this approach to any UID that is computed
  on the complete file (md5, sha1,...) *)
  if file_size < !!sha1_verification_threshold then 
    List.iter (fun uid ->
        match Uid.to_uid uid with
          (Sha1 _) as uid ->
            CommonSwarming.set_verifier swarmer (Verification [| uid |])
        | _ ->()) file_uids;
  file.file_swarmer <- Some swarmer;
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
  file

exception FileFound of file
  
let new_file file_id file_name file_size file_uids =
(*  if file_uids = [] then 
    try Hashtbl.find files_by_key (file_name, file_size) with
      _ -> 
        let file = new_file file_id file_name file_size in
        Hashtbl.add files_by_key (file_name, file_size) file;
        file
  else *)
  try
    List.iter (fun uid ->
        try  raise (FileFound (Hashtbl.find files_by_uid uid))
        with Not_found -> ()
    ) file_uids;
    let file = new_file file_id file_name file_size file_uids in
    List.iter (fun uid -> 
        if !verbose then
          lprintf "Adding file %s\n" (Uid.to_string uid);
        Hashtbl.add files_by_uid uid file) file_uids;
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
            | Indirect_location (_, uid, _, _) -> uid);
          user_kind = kind;
(*          user_files = []; *)
          user_speed = 0;
          user_vendor = "";
(*          user_gnutella2 = false; *)
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
    
let add_download file c index =
(*  let r = new_result file.file_name (file_size file) in *)
(*  add_source r c.client_user index; *)
  if !verbose then
    lprintf "Adding file to client\n";
  if not (List.memq c file.file_clients) then begin
      let chunks = [ Int64.zero, file_size file ] in
(*      let up = CommonSwarming.register_uploader file.file_swarmer 
          (CommonSwarming.AvailableRanges chunks) in *)
      c.client_downloads <- c.client_downloads @ [{
          download_file = file;
          download_uri = index;
          download_chunks = chunks;
          download_ranges = [];
          download_block = None;
          download_uploader = None;
          download_head_requested = false;
          download_ttr_requested = false;
        }];
      file.file_clients <- c :: file.file_clients;
      file_add_source (as_file file) (as_client c)
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

let client_type c = client_type (as_client c)

let set_client_state client state =
  CommonClient.set_client_state (as_client client) state
  
let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client) 
  
  
let remove_file file = 
(*  if file.file_uids = [] then
    Hashtbl.remove files_by_key (file.file_name, file.file_file.impl_file_size)
  else *)
  List.iter (fun uid ->
        if !verbose then
          lprintf "******REMOVE %s\n" (Uid.to_string uid);
        Hashtbl.remove files_by_uid uid
    ) file.file_uids;
  current_files := List2.removeq file !current_files  

let udp_sock = ref (None : UdpSocket.t option)

let client_ip sock =
  CommonOptions.client_ip
  (match sock with Connection sock -> Some sock | _ -> None)

let disconnect_from_server s r =
  if !verbose then
    lprintf_nl () "disconnect_from_server %s" (string_of_reason r);
  match s.server_sock with
  | Connection sock ->
      let h = s.server_host in
      (match server_state s with 
          Connected _ ->
            let connection_time = Int64.to_int (
                Int64.sub (int64_time ()) s.server_connected) in
            if !verbose then
              lprintf_nl () "disconnect_from_connected_server %s:%d after %d seconds (%s)\n"
                (Ip.string_of_addr h.host_addr) h.host_port
                connection_time (string_of_reason r)
            ;
        | _ -> ()
      );
      (try close sock r with _ -> ());
      s.server_sock <- NoConnection;
      set_server_state s (NotConnected (r, -1));
      s.server_need_qrt <- true;
      decr nservers;
      if List.memq s !connected_servers then
        connected_servers := List2.removeq s !connected_servers
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
  if len < 32 then name else  String.sub name 0 32

(*************************************************************

Define a function to be called when the "mem_stats" command
  is used to display information on structure footprint.

**************************************************************)

let _ =
(*  let network_info = CommonNetwork.network_info network in *)
  let name = network.network_name ^ "Globals" in
  Heap.add_memstat name (fun level buf ->
        Printf.bprintf buf "Number of old files: %d\n" (List.length !!old_files
  )
);

