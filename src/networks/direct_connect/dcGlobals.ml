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
open Printf2

open CommonFile
open CommonOptions
open CommonResult
open BasicSocket
open CommonGlobals
open CommonTypes
open CommonClient
open Options
open CommonUser
open CommonServer
open CommonInteractive
open CommonNetwork

open DcTypes
open DcOptions
  
let log_prefix = "[dcGlo]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

exception Found_client of DcTypes.client
exception Found_server of DcTypes.server
exception Found_file of DcTypes.file
exception Found_user of DcTypes.user  
exception Wrong_file_size of Int64.t * Int64.t
exception BreakIter
  
let network = new_network "DC" "Direct Connect"  
    [ 
    NetworkHasServers; 
    (*NetworkHasRooms;*)
    NetworkHasChat;
    NetworkHasSearch;
    NetworkHasUpload;
    ]

let connection_manager = network.network_connection_manager
  
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
  
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
  
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

let (shared_ops : CommonUploads.shared_file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network

let once_create_filelist = ref false    (* filelist creation one time on start     *)
let once_connect_to_servers = ref false (* autoconnection done to servers on start *)
  
let file_disk_name file = file_disk_name (as_file file.file_file)
let dc_tcp_listen_sock = ref (None : TcpServerSocket.t option)
let dc_udp_sock = ref (None : UdpSocket.t option)
(*let nservers = ref 0  *)                        (* connected servers count *)
let connected_servers = ref ([]: server list) (* list of connected servers (servers that have sent $Hello to us)*)
let servers_by_ip : (string , server) Hashtbl.t = Hashtbl.create 10 
let users_by_name = Hashtbl.create 113
let current_files = ref ([]: file list) 
let dc_files_by_unchecked_hash : (string , file) Hashtbl.t = Hashtbl.create 47
let dc_files_by_key = Hashtbl.create 47
let dc_tiger_computing = ref (None : dc_shared_file option)
let dc_get_nchunks size = Int64.to_int (size // CommonUploads.tiger_block_size) + 1
let dc_shared_files_by_fullname : (string , dc_shared_file) Hashtbl.t = Hashtbl.create 30
let dc_shared_files_by_codedname : (string , dc_shared_file) Hashtbl.t = Hashtbl.create 30
let dc_shared_files_by_hash : (string , dc_shared_file) Hashtbl.t = Hashtbl.create 30
let dc_result_info : (int , dc_result) Hashtbl.t = Hashtbl.create 30
let dc_results_by_file : ((string * string * int64), CommonTypes.result) Hashtbl.t = Hashtbl.create 30
let dc_files_to_hash = ref ([] : dc_shared_file list)
let dc_last_manual_search = ref 0.
let dc_last_autosearch = ref (None : CommonTypes.search option)
let dc_last_autosearch_time = ref 0.
let dc_total_uploaded = ref Int64.zero
(*let current_open_slots = ref 0*)
(*let clients_by_name = Hashtbl.create 113*)
let clients_list = ref ([] : client list)
let dc_hublist = ref ([] : dc_hub list) (* list for DC servers *)
let temp_nick = ref "unknown"
let temp_nick_num = ref 0 (* this is used to name temporary users always with different name *)
let used_slots = ref 0
let dc_download_preread = ref 128 (* max (int64_kbyte-1) atm. Used to make simple check of file corruption *)
let dc_config_files_loaded = ref false
let char5 = char_of_int 5   (* /005 *)
let char13 = char_of_int 13 (* \r *)
let char32 = char_of_int 32 (* "space" *)
let char38 = char_of_int 38 (* & *) 
let char39 = char_of_int 39 (* ' *)
let char42 = char_of_int 42 (* * *)
let char43 = char_of_int 43 (* + *)
let char58 = char_of_int 58 (* : *)
let char60 = char_of_int 60 (* < *)
let char62 = char_of_int 62 (* > *)
let char92 = char_of_int 92 (* slash \ *)
let char129 = char_of_int 129 (* extended ASCII *)
let char154 = char_of_int 154 (* extended ASCII *)
let char160 = char_of_int 160 (* extended ASCII *)
let char165 = char_of_int 165 (* extended ASCII *)
let empty_string = ""
let int64_kbyte = Int64.of_int 1024
let int64_mbyte = Int64.mul int64_kbyte int64_kbyte
let int64_gbyte = Int64.mul int64_mbyte int64_kbyte
let int64_64kbytes = Int64.mul int64_kbyte (Int64.of_int 64) 
let random_port = (Random.int 60000) + 1025
let mylist_ext = ".DcLst"
let bz2_ext = ".bz2"
let xml_ext = ".xml" 
let mylistxmlbz2_ext = xml_ext ^ bz2_ext
let mylist = "MyList" ^ mylist_ext
let mylistxmlbz2 = "files" ^  mylistxmlbz2_ext 
let directconnect_directory = "direct_connect"
let filelist_directory = Filename.concat directconnect_directory "filelists"
let last_share_size = ref 0

(* These are the $Supports commands that MLDonkey understands at the moment *) 
let mldonkey_dc_hub_supports = {
  nogetinfo = true;    (* Hub doesn't need to receive a $GetINFO from a client to send out $MyINFO *)
  nohello = true;      (* Client doesn't need either $Hello or $NickList to be sent *)
  userip2 = false;     (* Support for v2 of the $UserIP command *)
  usercommand = false; (* Support for $UserCommand, which is a standard way of adding hub-specific shortcuts to the client *)
  tthsearch = true;
  opplus = false;
  feed = false;
  mcto = false;
  hubtopic = true;
}
    
let mldonkey_dc_client_supports = {
  bzlist = false;        (* Support for a bzip2 compressed file list *)
  minislots = true;      (* Support for the concept of a "mini-slot" *) (* off not supported *)
  getzblock = false;     (* Instead of $Get and $Send, use $GetZBlock *)
  xmlbzlist = true;      (* Support for UTF-8 XML file lists, includes also support for $UGetBlock *)
  adcget = true;         (* Support for $ADCGET, a file retrieval command backported from the ADC draft *)
  tthl = false;          (* Support for the "tthl" namespace for $ADCGET *)
  tthf = true;           (* Support for the retrieving a file by its TTH through $ADCGET *)
  zlig = false;          (* Support for compressing the stream of data sent by $ADCGET with the ZLib library *)
  clientid = false;      (* Support for the $ClientID command *)
  chunk = false;         (* Extension by Valknut that allows retrieval of sections of a file through a modified $Get syntax *)
  gettestzblock = false; (* Support for compressed transfers with commands $GetTestZBlock and $Sending *)
  getcid = false; 
}
(* DC++ 0674 Supports to hubs:  UserCommand NoGetINFO NoHello UserIP2 TTHSearch GetZBlock *)
(* DC++ 0674 Supports to clients: MiniSlots XmlBZList ADCGet TTHL TTHF GetZBlock ZLIG *)
(* Verlihub supports: OpPlus NoGetINFO NoHello UserIP2 *)

let init_myinfo = {
  dest = empty_string;
  nick = empty_string;
  description = empty_string;
  client_brand = empty_string;
  version = empty_string;
  mode = 'P';
  hubs = (0 , 0 , 0);
  slots = 0;
  conn_speed = empty_string;
  open_upload_slot = 0;
  flag = 1;
  sharesize = Int64.zero;
  email = empty_string;
  bwlimit = 0;
}

(*             FUNCTIONS          *)
let set_server_state s state =
  set_server_state (as_server s.server_server) state
(*let set_room_state s state =
  set_room_state (as_room s.server_room) state *)
let server_num s = server_num (as_server s.server_server)
let file_num s = file_num (as_file s.file_file)
let server_state s = server_state (as_server s.server_server)
let file_state s = file_state (as_file s.file_file)
let server_must_update s = server_must_update (as_server s.server_server)
let file_must_update s = file_must_update (as_file s.file_file)
  
let dc_new_shared_dir dirname = {
    shared_dirname = dirname;
    shared_files = [];
    shared_dirs = [];
  }

let dc_shared_tree = dc_new_shared_dir ""

(* Copy from CommonUploads... *)
let rec dc_add_shared_file node dcsh dir_list =
  match dir_list with
    [] -> assert false
  | [filename] ->
      node.shared_files <- dcsh :: node.shared_files
  | dirname :: dir_tail ->
      let node =
        try
          List.assoc dirname node.shared_dirs
        with _ ->
            let new_node = dc_new_shared_dir dirname in
            node.shared_dirs <- (dirname, new_node) :: node.shared_dirs;
            new_node
      in
      dc_add_shared_file node dcsh dir_tail

let open_slots () = !!dc_open_slots
let current_slots () = open_slots () - !used_slots

let dc_remove_uploader () =
  if !used_slots < 1 then begin
    used_slots := 0;
    if !verbose_upload then lprintf_nl "Slot internal counting error: already 0"
  end else begin
    decr used_slots;
    if !verbose_upload then lprintf_nl "Decreased used slots to (%d)" !used_slots
  end

let dc_insert_uploader () =
  if !used_slots >= open_slots () then begin
    used_slots := open_slots ();
    if !verbose_upload then lprintf_nl "Slot internal counting error: already at maximum"
  end else begin 
    incr used_slots;
    if !verbose_upload then lprintf_nl "Increased used slots to (%d)" !used_slots
  end 

let dc_can_upload () = 
  if !used_slots >= open_slots () then false else true

let counts_as_minislot size = size < int64_64kbytes

let is_even_to_hundreds x = (x > 0) && ((x mod 100) = 0)

let is_even_to_tenths x = (x > 0) && ((x mod 10) = 0)

let is_even_to_twos x = (x > 0) && ((x mod 2) = 0)

let is_valid_tiger_hash hash =
  if String.length hash = 39 then begin
    if (String.contains hash char32) || (String.contains hash char92) ||
       (String.contains hash '/') then false 
    else true
  end else false  

let find_sockets_client sock =
  (try
    List.iter (fun c -> 
      (match c.client_sock with
      | Connection csock ->  if csock == sock then raise (Found_client c)
      | _ -> () )
    ) !clients_list;
    None
  with
  | Found_client c -> Some c
  | _ -> None )

(* set our nick for hubs from .ini or global *)
let local_login () =
  if !!login = "" then !!CommonOptions.global_login else !!login

(* Shorten string to some maximum length *)
let shorten_string s length =
  if length < String.length s then
    try
      let n = Charset.utf8_nth s length in
      String.sub s 0 n
    with
      _ -> s (* relies on bounds checking! FIXME? *)
  else s 

(* Replace one string to another string from string *)
let dc_replace_str_to_str s find_str to_str =
  if find_str = to_str then failwith "dc_replace_str_to_str find_str = to_str";
  let flen = String.length find_str in
  let str = ref "" in
  let rest = ref "" in
  let index = ref 0 in
  let rec replace s =
    let ok =
      (try
        index := String2.search_from s 0 find_str
      with
      | Not_found -> index := -1 );
      if (!index = -1) then begin
        str := !str ^ s; true
      end else begin
        str := !str ^ String2.before s !index ^ to_str;
        rest := String2.after s (!index+flen);
        false
      end
    in
    if not ok then replace !rest
    else !str
  in
  replace s

(* Strip all unnecessary characters from string (CHECK not perfect) *)
let clean_string str =
  (* DC++ static const char* badChars = "$|.[]()-_+"; *)
  let s = ref "" in
  let batch = ref "" in
  let last_was_space = ref false in
  let add_to_s () = 
    if (String.length !batch) > 2 then begin
      s := !s ^ !batch;
      true
    end else false
  in
  String.iter (fun c -> 
    (match c with (* TODO 1..9 *)
    | c when ((c >= 'a') && (c <= 'z')) ||
             ((c >= 'A') && (c <= 'Z')) ||
             ((c >= char129) && (c <= char154)) ||
             ((c >= char160) && (c <= char165)) ||
             ((c >= '0') && (c <= '9')) ->
                last_was_space := false;
                batch := !batch ^ String2.of_char c
    | ' ' | '.' | '-' | '_' -> 
        if !last_was_space then () 
        else begin 
          if add_to_s () then s := !s ^ String2.of_char char32;
          batch := "";  
          last_was_space := true;
        end
    | _ -> () )
  ) str;
  ignore (add_to_s ());
  !s

(* Create temporary nickname for client connection *)
let create_temp_nick () =
  let s = "Unknown" ^ (string_of_int !temp_nick_num) in
  if !temp_nick_num == max_int then temp_nick_num := 0
  else temp_nick_num := succ !temp_nick_num;
  s

(* Add user to server and vice versa *)
let add_user_to_server u s =
  if not (List.memq s u.user_servers) then u.user_servers <- s :: u.user_servers; (* add server to users list *)
  if not (List.memq u s.server_users) then begin
    s.server_users <- u :: s.server_users;                                        (* add user to servers list *)
    server_new_user (as_server s.server_server) (as_user u.user_user);
  end

(* Add new user to hubs userlist *)
(* PROBLEM ?  There can possibly be users from different servers with same names,      *)
(*            and atm. this is not checked in any way. So if on different servers      *)
(*            has users with same name, they are hereafter treated as one              *)
let new_user server name =
  let u =
    (try
      Hashtbl.find users_by_name name
    with _ ->
        let rec user = {
            user_nick = name;
        user_ip = Ip.addr_of_ip Ip.null;
            user_servers = [];
        user_clients = [];
            user_user = user_impl;
        user_uploaded = Int64.zero;
        user_downloaded = Int64.zero;
        user_link = empty_string;
        user_myinfo = init_myinfo;
            user_data = 0.0;
        user_type = Normal;
        user_state = UserIdle;
        user_messages = [];
        user_read_messages = 0; 
          } and user_impl = {
            dummy_user_impl with
            impl_user_ops = user_ops;
            impl_user_val = user;
      } in
        Hashtbl.add users_by_name name user;
        user_add user_impl;
      (*lprintf_nl "New user: %s" user.user_nick;*)
      user )      
  in
  ignore (match server with
         | Some s -> add_user_to_server u s 
         | None -> () );
      u
        
(* Check if user has some of my nicks = is me *)
let has_my_nick u =
  let rec iter slist =
    (match slist with
    | first :: tail ->
        if first.server_last_nick = u.user_nick then true
        else iter tail
    | [] -> false )
  in iter !connected_servers
      
(* Find user by name *)
let search_user_by_name nick =
  try
      Hashtbl.find users_by_name nick
    with _ -> raise Not_found
    
(* Remove server from users serverlist *)
let remove_server_from_user s u =
  if (List.memq s u.user_servers) then begin
    u.user_servers <- List2.removeq s u.user_servers (* remove server from user *)
  end
      
(* Remove user from servers userlist *)
let remove_user_from_server u s =
  if (List.memq u s.server_users) then begin
    s.server_users <- List2.removeq u s.server_users (* remove user frim server *)
  end
      
(* Remove user from servers userlist and if not any pending downloads, from Hashtbl userlist also *)
let remove_user s u =
  remove_user_from_server u s;
  remove_server_from_user s u;
  if (List.length u.user_servers < 1) then begin
    if u.user_clients = [] then begin (* if user has no clients *)
      Hashtbl.remove users_by_name u.user_nick;
      u.user_messages <- [];
      u.user_read_messages <- 0;
    end(* else begin
      lprintf_nl "User (%s) has clients, not removed" u.user_nick
    end*)
  end

(* Is user active ? *)
let user_active u =
  u.user_myinfo.mode = 'A'
 
(* Check is filelist downloading from this user already on queue or loaded *) 
let filelist_already_downloading u =
  (try
    List.iter (fun cl ->
      (match cl.client_state with
      | DcDownloadListWaiting | DcDownloadListConnecting _ | DcDownloadList _ -> raise BreakIter
      | _ -> () )
    ) u.user_clients;
    false 
  with _ -> true )

(* true if user has new messages *)
let user_has_new_messages user = (List.length user.user_messages) > user.user_read_messages

(* file impl for uploading clients *)
let new_upfile dcsh fd user =
  let filename,directory =
    (match dcsh with 
    | Some dcsh -> 
        Filename.basename dcsh.dc_shared_fullname, Filename.dirname dcsh.dc_shared_fullname
    | None ->
        let filename = Unix32.filename fd in
        Filename.basename filename, Filename.dirname filename )
  in
  let rec file = {
    file_file = impl;
    file_unchecked_tiger_root = "";
    file_directory = directory;
    file_name = filename;
    file_clients = [];
    file_search = None;
    (*file_tiger_array = [||];*)
    file_autosearch_count = 0;
  } and impl = {
    (dummy_file_impl ()) with
    impl_file_fd = Some fd;
    impl_file_size = Unix32.getsize64 fd;
    impl_file_downloaded = Int64.zero;
    impl_file_received = Int64.zero;
    impl_file_val = file;
    impl_file_ops = file_ops;
    impl_file_age = last_time ();
    impl_file_best_name = filename;
    impl_file_owner = user;
    impl_file_group = user.user_default_group;
  } in
  file

(* Return existing file or create new one *)
let new_file tiger_root (directory:string) (filename:string) (file_size:int64) user group =
  (try
    let f = Hashtbl.find dc_files_by_unchecked_hash tiger_root in
    if !verbose_download then 
      lprintf_nl "File with hash exists: (%s) (%s) (%s)" tiger_root f.file_directory f.file_name;
    f
  with _ -> 
    let key = (directory, filename, file_size) in
    (try
      let f = Hashtbl.find dc_files_by_key key in (* Then try to find by key (dir,name,size) *)
      if !verbose_download then lprintf_nl "File exists: (%s) (%s)" f.file_directory f.file_name;
      f 
  with _ ->
      let temp_filename =
        (match tiger_root with
        | "" -> Printf.sprintf "DC_%s_%s" directory filename
        | _ -> Printf.sprintf "DC_%s" tiger_root)
      in 
      let fullname = CommonFile.concat_file !!temp_directory temp_filename in
      let temp_file = Unix32.create_rw fullname in
      let current_size = 
        (try
          Unix32.getsize fullname
        with e ->
            if !verbose_unexpected_messages then
              lprintf_nl "Exception (%s) in current_size of (%s)" (Printexc2.to_string e) fullname; 
            Int64.zero )
      in
      let rec file = {
        file_file = impl;
        file_unchecked_tiger_root = tiger_root;
        file_directory = directory;
        file_name = filename;
        file_clients = [];
        file_search = None;
        (*file_tiger_array = [||];*)
        file_autosearch_count = 0;
        } and impl = {
          (dummy_file_impl ()) with
          impl_file_fd = Some temp_file;
          impl_file_size = file_size;
          impl_file_downloaded = current_size;
          impl_file_received = current_size;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();
          impl_file_best_name = filename;
          impl_file_owner = user;
          impl_file_group = group;
        } in
      file_add impl FileNew;  (* CommonInteractive.file_add *)
      current_files := file :: !current_files;
      if tiger_root <> empty_string then Hashtbl.add dc_files_by_unchecked_hash tiger_root file;
      Hashtbl.add dc_files_by_key key file;
      if !verbose_download then 
        lprintf_nl "New File:(%s) (%s) (%s) (%Ld)" tiger_root file.file_directory file.file_name file_size;
      file
    )
  )

(* Some shortcuts to CommonFile... *)
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file_fd (as_file file.file_file)

(* Add new client, return client*)
let new_client () = 
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
          client_name = None;
          client_addr = None;
          client_supports = None;
          client_lock = "";
          client_file = None; (* (file, filename) *)
          client_state = DcIdle;
          client_error = NoError;
          client_error_count = 0;
          client_preread_bytes_left = 0;
          client_pos = Int64.zero;
          client_endpos = Int64.zero; (* atm. upload end position *)
          client_receiving = Int64.zero;
          client_user = None;
          client_connect_time = last_time ();
          client_connection_control = new_connection_control ();
          client_downloaded = Int64.zero;
          client_uploaded = Int64.zero;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_upload = None;
        } in
      (*lprintf_nl "New client";     *)
      CommonClient.new_client impl; 
      clients_list := c :: !clients_list;
      c

(* add client to file & vice versa *)
let add_client_to_file client file = (* TODO  we never empty files clients list so implement some kind of size control *) 
  if not (List.memq client file.file_clients) then begin       (* if client is not on file's contact list... *)
    file.file_clients <- client :: file.file_clients;          (* then add this new client to file contact list *)
    client.client_file <- Some file;
    (*file_add_source (as_file file.file_file) (as_client client.client_client)*) (* CommonFile.file_add_source *)
  end

(* add client to user & vice versa *)
let add_client_to_user client user =
  if not (List.memq client user.user_clients) then begin
    user.user_clients <- user.user_clients @ [ client ];                  (* add client to userlist *)
    client.client_user <- Some user
  end

(* New client to user with file *)
let new_client_to_user_with_file u f =
  let c = new_client () in
  c.client_name <- Some u.user_nick;
  add_client_to_user c u;
  add_client_to_file c f;
  c

let client_type c =
  client_type (as_client c.client_client)

(* Find clients by name, return list of all matching clients *)
(*let find_clients_by_name name =
  let result = ref [] in
    List.iter (fun c ->
      (match c.client_user with
      | Some u -> if u.user_nick = name then result := c :: !result; ()
      | _ -> () )
    ) !clients_list;
  !result *)

(* Print client state to string *)
let client_state_to_string c =
  let get_direction dir =
    (match dir with 
    | Upload i -> Printf.sprintf "Upload %d" i
    | Download i -> Printf.sprintf "Download %d" i )
  in
  (match c.client_state with
  | DcIdle -> "DcIdle"
  | DcDownloadWaiting _ -> "DcDownloadWaiting"
  | DcDownloadConnecting _ -> "DcDownloadConnecting"
  | DcDownloadListWaiting -> "DcDownloadListWaiting"
  | DcDownloadListConnecting _ -> "DcDownloadListConnecting"
  | DcConnectionStyle style ->
      (match style with 
      | ClientActive dir -> Printf.sprintf "DcConnectionStyle ClientActive %s" (get_direction dir)
      | MeActive dir -> Printf.sprintf "DcConnectionStyle MeActive %s" (get_direction dir) )
  | DcDownload _ -> "DcDownload"
  | DcDownloadList _ -> "DcDownloadList"
  | DcUpload _ -> "DcUpload"
  | DcUploadStarting _ -> "DcUploadStarting"
  | DcUploadList _ -> "DcUploadList"
  | DcUploadListStarting _ -> "DcUploadListStarting"
  | DcUploadDoneWaitingForMore -> "DcUploadDoneWaitingForMore" )

(* Copy client data to another *) 
let new_copy_client c =
  {
    (new_client ()) with
    (*client_sock = c.client_sock;*)
    client_name = c.client_name;
    client_addr = c.client_addr;
    client_supports = c.client_supports;
    client_lock = c.client_lock;
    client_file = c.client_file;
    (*client_state = c.client_state;*)
    client_pos = c.client_pos; 
    client_receiving = c.client_receiving;
    client_user = c.client_user;
    client_error = c.client_error;
    client_error_count = c.client_error_count;
    client_endpos = c.client_endpos;
    client_connect_time = c.client_connect_time;
    client_downloaded = c.client_downloaded;
    client_uploaded = c.client_uploaded;
  }

(* Get clients username *)
let clients_username client =
  let name =
    (match client.client_user with
    | Some user -> user.user_nick
    | _ -> "NO USER" )
  in name

(* Remove clients files and all references from files to this client *)
let remove_client_from_clients_file c = 
  (match c.client_file with 
  | Some f ->
      f.file_clients <- List2.removeq c f.file_clients; 
      c.client_file <- None
  | None -> () ) 

(* Remove clients references from users *) 
let remove_client c =
  (match c.client_user with
  | Some u ->
      u.user_clients <- List2.removeq c u.user_clients
      (*lprintf_nl "Removed one client from user %s clientlist" u.user_nick;*)
  | None -> () );
  c.client_user <- None;
  remove_client_from_clients_file c;
  clients_list := List2.removeq c !clients_list;
  client_remove (as_client c.client_client);
  ()

(* Remove file from current filelist *)
let remove_file_from_filelist file =
  (try
    current_files := List2.removeq file !current_files;
    with _ ->
     if !verbose_unexpected_messages then
       lprintf_nl "Could not remove file from !current_files - %s" file.file_name )

(* Remove file from hashtbl dc_files_by_unchecked_hash *)
let remove_file_from_hashes file =
    if file.file_unchecked_tiger_root <> empty_string then begin
      (try
        Hashtbl.remove dc_files_by_unchecked_hash file.file_unchecked_tiger_root;
      with _ -> 
          if !verbose_unexpected_messages then
            lprintf_nl "Could not remove file from hashtable dc_files_unchecked_hash - %s" file.file_name )
    end
  
(* Remove file from hashtab dc_files_by_key *)
let remove_file_from_files file =
  (try
    Hashtbl.remove dc_files_by_key (file.file_directory, file.file_name, file.file_file.impl_file_size);
  with _ -> 
      if !verbose_unexpected_messages then
        lprintf_nl "Could not remove file from hashtable dc_files_by_key - %s" file.file_name )

(* remove all clients of file *)
let remove_files_clients file =
  List.iter (fun c ->
    remove_client c;
  ) file.file_clients;
  file.file_clients <- []

(* remove file from file list *)
let remove_file_with_clients file =
  remove_files_clients file;
  remove_file_from_hashes file;
  remove_file_from_files file;
  remove_file_from_filelist file

(* remove file from file list *)
let remove_file_not_clients file =
  List.iter (fun c ->
    c.client_file <- None;
  ) file.file_clients;
  file.file_clients <- [];
  remove_file_from_hashes file;
  remove_file_from_files file;
  remove_file_from_filelist file
      
let set_client_state c state =
  set_client_state (as_client c.client_client) state

let dc_set_client_disconnected c =
  set_client_disconnected (as_client c.client_client)
              
let set_clients_upload c sh =
  set_client_upload (as_client c.client_client) sh; (*(as_file c.client_file);*)
  set_client_has_a_slot (as_client c.client_client) NormalSlot;
  client_enter_upload_queue (as_client c.client_client)

(* Print closing reason to string *)
let closing_reason_to_text reason =
  (match reason with
  | Closed_for_error text -> Printf.sprintf "Error: Reason (%s)" text
  | Closed_for_timeout -> "Timeout"
  | Closed_for_lifetime -> "Lifetime"
  | Closed_by_peer -> "By peer"
  | Closed_by_user -> "By user (us - operation complete)"
  | Closed_for_overflow -> "Overflow"
  | Closed_connect_failed -> "Connect failed" 
  | Closed_for_exception _ -> "Exception"  )

(* Can client start downloading *)
let is_client_waiting c =
  (match c.client_state with                              (* check user clients states *)
  | DcIdle | DcDownloadWaiting _ | DcDownloadListWaiting -> true
  | DcUpload _ | DcUploadStarting _ | DcUploadListStarting _ | DcUploadList _
  | DcDownloadListConnecting _ | DcDownloadConnecting _ | DcDownload _
  | DcDownloadList _ | DcConnectionStyle _ | DcUploadDoneWaitingForMore -> false )

(* Can client start downloading clients file *)
let is_client_blocking_downloading c =
  (match c.client_state with                           
  | DcIdle | DcUpload _ | DcUploadStarting _ | DcUploadListStarting _
  | DcUploadList _ | DcDownloadWaiting _ | DcDownloadListWaiting
  | DcUploadDoneWaitingForMore -> false
  | DcDownloadListConnecting _ | DcDownloadConnecting _ | DcDownload _
  | DcDownloadList _ | DcConnectionStyle _ -> true )

(* Check user, that has sent RevConnectToMe and we have sent ConnectToMe and we are waiting *)
let check_passive_user u = 
  (match u.user_state with
  | UserPassiveUserInitiating time ->
      if (current_time () -. time) > float_of_int !!client_timeout then begin
        if !verbose_msg_clients then
          lprintf_nl "Resetted passive user (%s) waiting state " u.user_nick;
        u.user_state <- UserIdle;
      end
  | _ -> () )

(* Check all clients, that have sent RevConnectToMe and we have sent ConnectToMe and we are waiting *) 
let check_all_passive_users () =
  Hashtbl.iter (fun _ u ->
     check_passive_user u
  ) users_by_name 

(* Check that user has no downloads and is not in conversation state *)
let can_user_start_downloading u =
  (try
    List.iter (fun c ->                                       
      if (is_client_blocking_downloading c) then raise BreakIter
    ) u.user_clients;
    (match u.user_state with            (* check user state/timeouts *)
    | UserIdle -> ()
    | UserPassiveUserInitiating time -> (* passive users wait check *)
        if (current_time () -. time) > float_of_int !!client_timeout then begin
          if !verbose_msg_clients then lprintf_nl "Resetted RevConnect Passive user waiting (%s)" u.user_nick; 
          u.user_state <- UserIdle;
        end else raise BreakIter
    | _ -> raise BreakIter );
    true
  with _ -> false )

(* Find a connected client by ip *)
(*let find_connected_client_by_ip ip port =
  (try 
    List.iter (fun c ->
          (match c.client_sock with
          | Connection sock ->
              (match c.client_addr with
              | None -> failwith "Client connected but no ip address"
              | Some (cip , cport) -> begin
                  lprintf_nl "Client match found: checking real ips";
                  let rip = Ip.to_string (TcpBufferedSocket.peer_ip sock) in
                  let rport = TcpBufferedSocket.peer_port sock in
                  lprintf_nl "  From socket:        rip= %s  rport= %d" rip rport;
                  lprintf_nl "  From c.client_addr: cip= %s  cport= %d" (Ip.to_string cip) cport;
                  lprintf_nl "  From ConnectToMe  : ip = %s  port = %d" (Ip.to_string ip) port;
                  if (Ip.equal cip ip) then raise (Found_client c)
                end ) 
          | _ -> () )
    ) !clients_list;
    raise Not_found 
  with 
  | Found_client c -> Some c
  | Failure e -> lprintf_nl "In ( find_connected_client_by_ip): %s" e; None
  | Not_found -> None ) *)

(* Find any client with known ip *)
(*let find_client_by_ip ip = 
  (try
    List.iter (fun c ->
      (match c.client_addr with
      | None -> failwith "No ip on client!"
      | Some (cip , _ ) ->
          if (Ip.equal cip ip) then begin 
            (match c.client_user with
            | Some u -> lprintf_nl "  Found matching client from user %s with ip: %s" u.user_nick
               (Ip.to_string cip); ()
            | _ -> failwith "find_client_by_ip: No user in client !" );
            raise (Found_client c)
          end )
    ) !clients_list;
    raise Not_found
  with 
  | Found_client c -> Some c
  | Failure e -> lprintf_nl "In (find_client_by_ip): %s" e; None
  | Not_found -> None ) *)

(* Add needed dc-info fields to result by number *)  
let add_info_to_result r user tiger_root directory =
    let result_info = {
      user = user;
      tth = tiger_root;
      directory = directory;
    } in
    try
      Hashtbl.find dc_result_info r.stored_result_num (* if result number exists in hashtable result_sources *)
                                                      (* return existing result's info (user & directory)  *)
    with _ ->
        Hashtbl.add dc_result_info r.stored_result_num result_info;  (* ...add the new result's info to hashtable *)
        result_info                                           
    
(* add new server/hub by address and port if not exist - return server/hub*)    
let new_server addr ip port =
  let ips = Ip.to_string ip in
  try
    Hashtbl.find servers_by_ip ips 
  with _ ->
      let rec h = { 
          server_server = server_impl;
          server_name = "<unknown>";
          server_addr = addr;
          server_ip = ip;
          server_info = "";
          server_supports = None;
          server_connection_time = nan; (* Stands for ``not a number' *)
          server_hub_state = Waiting;
          server_connection_control = new_connection_control ();
          server_sock = NoConnection;
          server_autoconnect = false;
          server_port = port;
          server_last_nick = "";
          server_search = None;
          server_search_timeout = 0;
          server_users = [];
          server_topic = "";
          server_messages = [];
          server_read_messages = 0;
        } and 
        server_impl = {
          dummy_server_impl with
          impl_server_val = h;
          impl_server_ops = server_ops;
        }         
      in
      server_add server_impl;
      Hashtbl.add servers_by_ip ips h;
      h
  
(* Add server to connected servers *)
let add_connected_server s =
  (*incr nservers;*)
  if not (List.memq s !connected_servers) then
    connected_servers := s :: !connected_servers

(* Remove servers contacts to users and from connected servers *)
let remove_connected_server s =
  (*decr nservers;*)
  connected_servers := List2.removeq s !connected_servers;
  List.iter (fun u ->
    remove_user s u
  ) s.server_users;
  s.server_hub_state <- Waiting;
  s.server_search <- None;
  s.server_users <- []

(* Remove server from known servers list *)
let server_remove s =
  server_remove (as_server s.server_server);
  Hashtbl.remove servers_by_ip (Ip.to_string s.server_ip)
  (*decr nknown_servers;*)
  (*servers_list := List2.removeq s !servers_list*)

(* Iter all servers in connected list *)
let dc_with_connected_servers f =
  List.iter (fun s ->
    f s
  ) !connected_servers

(* Return hub state text *)
let dc_hubstate_to_text s =
  (match s.server_hub_state with
  | Waiting -> "Not connected"
  | User -> "User"
  | Vipped -> "Vip"
  | Opped -> "Op" )

(* Search server by ip and port *)
(*let search_server_by_addr addr port =
  try
    Hashtbl.find servers_by_addr (addr, port)
  with _ ->
    raise Not_found *)

(* add new result to results-hashtable - return the found or new result *)  
let new_result user tiger_root (directory:string) (filename:string) (filesize:int64) =
  let basename = Filename2.basename filename in
  let key = (directory, basename, filesize) in
  (*let r_username = "......" ^ user.user_nick in*)
  (try
    Hashtbl.find dc_results_by_file key (* if result with dir&name&size exists, return the found result *)
  with _ ->                             (* otherwise... *)
      let result_names =
        if tiger_root <> "" then begin
          let found = 
            (try
              ignore (Hashtbl.find dc_shared_files_by_hash tiger_root);
              true
            with _ -> false )
          in
          if found then [filename;"ALREADY DOWNLOADED"]
          else begin
            let found =
              (try
                ignore (Hashtbl.find dc_files_by_unchecked_hash tiger_root);
                true
              with _ -> false )
            in
            if found then [filename;"FILE DOWNLOADING..."]
            else [filename]
          end
        end else [filename]
      in
      let r = {                  (* add new result *)
          dummy_result with
        result_names = result_names;
        result_tags = [ {tag_name = Field_UNKNOWN user.user_nick; tag_value = String ""} ];
          result_size = filesize;
        result_source_network = network.network_num;
        } in
      let rs = update_result_num r in  (* CommonResult.update_result_num, returns Commontypes.result *)
      Hashtbl.add dc_results_by_file key rs;
      rs )  
      
(*let hash_file () =
  let dcsh = List.hd !dc_shared_files in
  let info = CommonUploads.IndexedSharedFiles.get_result dcsh.dc_shared_shared.shared_info in
  if dcsh.dc_shared_chunk <> dc_get_nchunks info.shared_size then compute_tigertree_chunk dcsh*)


(*  Hashtbl.iter (fun n sh ->
    lprintf_nl "(%s)" sh.shared_codedname;
    let info = CommonUploads.IndexedSharedFiles.get_result sh.shared_info in
    lprintf_nl "(%s)" info.shared_fullname

  ) CommonUploads.shared_files*)
  
(*
/** We don't keep leaves for blocks smaller than this... */
        static const int64_t MIN_BLOCK_SIZE = 64*1024;
*)
(*CommonHasher.compute_tiger :
  string -> int64 -> int64 -> (Md4.TigerTree.t job -> unit) -> unit 
*)
  
(* DC++ blocksize
                int64_t bl = 1024;
                while(bl * (int64_t)d->getTigerTree().getLeaves().size() < d->getTigerTree().getFileSize())
                        bl *= 2;
                d->getTigerTree().setBlockSize(bl);
                d->getTigerTree().calcRoot();
*)
(*
AdcCommand Download::getCommand(bool zlib, bool tthf) {
        AdcCommand cmd(AdcCommand::CMD_GET);
        if(isSet(FLAG_TREE_DOWNLOAD)) {
                cmd.addParam("tthl");
        } else if(isSet(FLAG_PARTIAL_LIST)) {
                cmd.addParam("list");
        } else {
                cmd.addParam("file");
        }
        if(tthf && getTTH() != NULL) {
                cmd.addParam("TTH/" + getTTH()->toBase32());
        } else {
                cmd.addParam(Util::toAdcFile(getSource()));
        }
        cmd.addParam(Util::toString(getPos()));
        cmd.addParam(Util::toString(getSize() - getPos()));
  
        if(zlib && getSize() != -1 && BOOLSETTING(COMPRESS_TRANSFERS)) {
                cmd.addParam("ZL1");
        }
*)
