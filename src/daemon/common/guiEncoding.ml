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

open Printf2
open CommonFile
open Md4
open CommonGlobals
open CommonTypes
open GuiTypes
open GuiProto  
open AnyEndian
open LittleEndian
open TcpBufferedSocket

let max_last_seen = 100 * 24 * 3600

let compute_last_seen last_seen =
  let last_seen = BasicSocket.last_time () - last_seen in
  if last_seen > max_last_seen || last_seen < -1 then
    max_last_seen
  else last_seen
  
let buf = Buffer.create 1000

      
let gui_send writer sock t = 
  try
    Buffer.clear buf;
    buf_int buf 0;
    writer buf t;
    let s = Buffer.contents buf in
    let len = String.length s - 4 in
    str_int s 0 len;
    write_string sock s;
  with UnsupportedGuiMessage -> ()
      
(***************

       Encoding of messages basic data types

****************)

let buf = () (* lots of buf variables here. Be sure not to use a previously
  defined one *)

let buf_list buf f list =
  buf_int16 buf (List.length list);
  List.iter (fun x -> f buf x) list

let buf_array buf f list =
  buf_int16 buf (Array.length list);
  Array.iter (fun x -> f buf x) list

let buf_string buf s =
  buf_int16 buf (String.length s);  
  Buffer.add_string buf s

let buf_float buf f =
  let i = int_of_float f in
  buf_string buf (Printf.sprintf "%d.%d" i (int_of_float ((f -. float_of_int i) *. 100.)))

let buf_int_float buf f =
  buf_string buf (Printf.sprintf "%d" (int_of_float ((BasicSocket.date_of_int f))))
  
let rec buf_query buf q =
  match q with
    Q_AND list ->
      buf_int8 buf 0;
      buf_list buf buf_query  list
  | Q_OR list ->
      buf_int8 buf 1;
      buf_list buf buf_query  list
  | Q_ANDNOT (q1,q2) ->
      buf_int8 buf 2;
      buf_query buf q1;
      buf_query buf q2
  | Q_MODULE (s, q) ->
      buf_int8 buf 3;
      buf_string buf s;
      buf_query buf q
  | Q_KEYWORDS (s1,s2) ->
      buf_int8 buf 4;
      buf_string buf s1;
      buf_string buf s2
  | Q_MINSIZE (s1,s2) ->
      buf_int8 buf 5;
      buf_string buf s1;
      buf_string buf s2
  | Q_MAXSIZE (s1,s2) ->
      buf_int8 buf 6;
      buf_string buf s1;
      buf_string buf s2
  | Q_FORMAT (s1,s2) ->
      buf_int8 buf 7;
      buf_string buf s1;
      buf_string buf s2
  | Q_MEDIA (s1,s2) ->
      buf_int8 buf 8;
      buf_string buf s1;
      buf_string buf s2
  | Q_MP3_ARTIST (s1,s2) ->
      buf_int8 buf 9;
      buf_string buf s1;
      buf_string buf s2
  | Q_MP3_TITLE (s1,s2) ->
      buf_int8 buf 10;
      buf_string buf s1;
      buf_string buf s2
  | Q_MP3_ALBUM (s1,s2) ->
      buf_int8 buf 11;
      buf_string buf s1;
      buf_string buf s2
  | Q_MP3_BITRATE (s1,s2) ->
      buf_int8 buf 12;
      buf_string buf s1;
      buf_string buf s2
  | Q_HIDDEN list ->
      buf_int8 buf 13;
      buf_list buf buf_query  list
  | Q_COMBO _ -> assert false
      
let buf_tag buf t =
  buf_string buf t.tag_name;
  match t.tag_value with
  | Uint64 s -> buf_int8 buf 0; buf_int64_32 buf s
  | Fint64 s -> buf_int8 buf 1; buf_int64_32 buf s
  | String s -> buf_int8 buf 2; buf_string buf s
  | Addr ip -> buf_int8 buf 3; buf_ip buf ip
    
let buf_host_state proto buf t =
  if proto < 12 then
    buf_int8 buf (    
      match t with
      | NotConnected _ -> 0
      | Connecting -> 1
      | Connected_initiating -> 2
      | Connected_downloading -> 3
      | Connected (-1) -> 4
      | Connected _ -> 5
      | NewHost -> 6
      | RemovedHost -> 7
      | BlackListedHost -> if proto < 10 then 0 else 8)
  else    
  match t with
  | NotConnected (_, -1) -> buf_int8 buf 0
  | Connecting -> buf_int8 buf  1
  | Connected_initiating -> buf_int8 buf 2
  | Connected_downloading -> buf_int8 buf 3
  | Connected (-1) -> buf_int8 buf 4
  | Connected n -> buf_int8 buf 5; buf_int buf n
  | NewHost -> buf_int8 buf 6
  | RemovedHost -> buf_int8 buf 7
  | BlackListedHost -> buf_int8 buf (if proto < 10 then 0 else 8)
  | NotConnected (_,n) -> buf_int8 buf 9; buf_int buf n
      
    
let buf_client_type buf t =
  buf_int8 buf (match t with
      NormalClient -> 0
    | FriendClient -> 1
    | ContactClient -> 2)

let buf_bool buf b =
  buf_int8 buf (if b then 1 else 0)
      
let buf_result buf r =
  buf_int buf r.result_num;
  buf_int buf r.result_network;
  buf_list buf buf_string r.result_names;
  buf_md4 buf r.result_md4;
  buf_int64_32 buf r.result_size;
  buf_string buf r.result_format;
  buf_string buf r.result_type;
  buf_list buf buf_tag r.result_tags;
  buf_string buf r.result_comment;
  buf_bool buf r.result_done

let buf_user buf u =
  buf_int buf u.user_num;
  buf_md4 buf u.user_md4;
  buf_string buf u.user_name;
  buf_ip buf u.user_ip;
  buf_int16 buf u.user_port;
  buf_list buf buf_tag u.user_tags;
  buf_int buf u.user_server

let buf_room_state buf s =
  buf_int8 buf (match s with RoomOpened -> 0 | 
      RoomClosed -> 1 | RoomPaused -> 2 )

let buf_file_state proto buf s =
  match s with
  | FileDownloading ->   buf_int8 buf 0
  | FilePaused ->   buf_int8 buf 1
  | FileDownloaded ->   buf_int8 buf 2
  | FileShared ->  buf_int8 buf 3    
  | FileCancelled ->  buf_int8 buf 4
  | FileNew ->  buf_int8 buf 5
      
  | FileAborted s -> 
      if proto < 12 then buf_int8 buf 1 (* File Paused *)
      else
        (buf_int8 buf 6; buf_string buf s)
  | FileQueued -> buf_int8 buf (if proto < 14 then 0 else 7)

let buf_room proto buf r =
  buf_int buf r.room_num;
  buf_int buf r.room_network;
  buf_string buf r.room_name;
  buf_room_state buf r.room_state;
  if proto >= 3 then
    buf_int buf r.room_nusers

let buf_message buf m =
  match m with
    ServerMessage s ->      buf_int8 buf 0; buf_string buf s 
  | PublicMessage (n, s) -> buf_int8 buf 1; buf_int buf n; buf_string buf s
  | PrivateMessage (n,s) -> buf_int8 buf 2; buf_int buf n; buf_string buf s

let buf_mp3 buf t =
  let module M = Mp3tag.Id3v1 in
  buf_string buf t.M.title;
  buf_string buf t.M.artist;
  buf_string buf t.M.album;
  buf_string buf t.M.year;
  buf_string buf t.M.comment;
  buf_int buf t.M.tracknum;
  buf_int buf t.M.genre
  
let buf_format buf f =
  match f with
  | FormatUnknown | FormatNotComputed _ -> buf_int8 buf 0
  | FormatType (s1, s2) -> buf_int8 buf 1; 
      buf_string buf s1; buf_string buf s2
  | AVI avi -> buf_int8 buf 2;
      buf_string buf avi.avi_codec;
      buf_int buf avi.avi_width;
      buf_int buf avi.avi_height;
      buf_int buf avi.avi_fps;
      buf_int buf avi.avi_rate;
  | MP3 (t, _) -> 
      buf_int8 buf 3;
      buf_mp3 buf t
      
let buf_kind buf k =
  match k with
    Known_location (ip, port) -> 
      buf_int8 buf 0; buf_ip buf ip; buf_int16 buf port
  | Indirect_location (name, md4) ->
      buf_int8 buf 1; buf_string buf name; buf_md4 buf md4
      
let buf_file proto buf f =
  buf_int buf f.file_num;
  buf_int buf f.file_network;  
  buf_list buf buf_string f.file_names;  
  buf_md4 buf f.file_md4;  
  buf_int64_32 buf f.file_size;  
  buf_int64_32 buf f.file_downloaded;  
  buf_int buf f.file_nlocations;  
  buf_int buf f.file_nclients;  
  buf_file_state proto buf f.file_state;  
  buf_string buf f.file_chunks;  
  buf_string buf f.file_availability;  
  buf_float buf f.file_download_rate;  
  buf_array buf buf_int_float f.file_chunks_age;  
  buf_int_float buf f.file_age;
(* last, so that it can be safely discarded in partial implementations: *)
  buf_format buf f.file_format;
  if proto >= 8 then begin
      buf_string buf f.file_name;
      if proto >= 9 then begin
          let ls = compute_last_seen f.file_last_seen in
          buf_int buf ls;
          if proto >= 12 then begin
              buf_int buf f.file_priority
              
            end
        end
    end
  
let buf_addr buf addr =
  match addr with
    Ip.AddrIp ip ->
      buf_int8 buf 0;
      buf_ip buf ip
  | Ip.AddrName s ->
      buf_int8 buf 1;
      buf_string buf s
  
let buf_server proto buf s =
  buf_int buf s.server_num;
  buf_int buf s.server_network;
  if proto < 2 then 
    buf_ip buf (Ip.ip_of_addr s.server_addr)
  else
    buf_addr buf s.server_addr;    
  buf_int16 buf s.server_port;
  buf_int buf s.server_score;
  buf_list buf buf_tag s.server_tags;
  buf_int buf s.server_nusers;
  buf_int buf s.server_nfiles;
  buf_host_state proto buf s.server_state;
  buf_string buf s.server_name;
  buf_string buf s.server_description
  
let buf_client proto buf c =
  buf_int buf c.client_num;
  buf_int buf c.client_network;
  buf_kind buf c.client_kind;
  buf_host_state proto buf c.client_state;
  buf_client_type buf c.client_type;
  buf_list buf buf_tag c.client_tags;
  buf_string buf c.client_name;
  buf_int buf c.client_rating;
  buf_int buf c.client_chat_port
  
let buf_network buf n =
  buf_int buf n.network_netnum;
  buf_string buf n.network_netname;
  buf_bool buf n.network_enabled;
  buf_string buf n.network_config_filename;
  buf_int64 buf n.network_uploaded;
  buf_int64 buf n.network_downloaded

let buf_search_type buf t =
  buf_int8 buf (
    match t with
      LocalSearch -> 0
    | RemoteSearch -> 1
    | SubscribeSearch -> 2)
    
let buf_search proto buf s = 
  buf_int buf s.search_num;
  buf_query buf s.search_query;
  buf_int buf s.search_max_hits;
  if proto >= 2 then
    buf_search_type buf s.search_type;
  if proto >= 16 then
    buf_int buf s.search_network

let buf_shared_info proto buf s =
  buf_int buf s.shared_num;
  buf_int buf s.shared_network;
  buf_string buf s.shared_filename;
  buf_int64_32 buf s.shared_size;
  buf_int64 buf s.shared_uploaded;
  buf_int buf s.shared_requests;
  if proto >= 10 then
    buf_md4 buf s.shared_id
    
  
(***************

       Encoding of messages from the Core to the GUI 

****************)
  
let rec to_gui proto buf t =
  match t with
  
  | CoreProtocol version -> 
      buf_int16 buf 0; 
      buf_int buf version
  
  | Options_info list -> 
      
      buf_int16 buf 1; 
      buf_list buf (fun buf (name, value) ->
          buf_string buf name; buf_string buf value
      ) list
  
  | DefineSearches list -> 
      buf_int16 buf 3;
      buf_list buf (fun buf (s, query) ->
          buf_string buf s; buf_query buf query) list
  
  | Result_info r -> buf_int16 buf 4;
      buf_result buf r
  
  | Search_result (n1,n2) -> buf_int16 buf 5;
      buf_int buf n1; buf_int buf n2
  
  | Search_waiting (n1,n2) -> buf_int16 buf 6;
      buf_int buf n1; buf_int buf n2
  
  | File_info file_info -> 
      
      buf_int16 buf (if proto < 8 then 7 else 
        if proto < 9 then 40 else 
        if proto < 14 then 43 else 52);
      buf_file proto buf file_info
  
  | File_downloaded (n, size, rate, last_seen) -> 
      buf_int16 buf (if proto < 9 then 8 else 46);
      buf_int buf n; 
      buf_int64_32 buf size; 
      buf_float buf rate; 
      if proto > 8 then
        buf_int buf (compute_last_seen last_seen)
  
  | File_add_source (n1,n2) -> buf_int16 buf 10;
      buf_int buf n1; buf_int buf n2
  
  | Server_busy (n1,n2,n3) -> buf_int16 buf 11;
      buf_int buf n1; buf_int buf n2; buf_int buf n3
  
  | Server_user  (n1,n2) -> buf_int16 buf 12;
      buf_int buf n1; buf_int buf n2
  
  | Server_state (int,host_state) -> buf_int16 buf 13;
      buf_int buf int; buf_host_state proto buf host_state
  
  | Server_info s -> 
      
      buf_int16 buf (if proto < 2 then 14 else 26);
      buf_server proto buf s
  
  | Client_info client_info -> buf_int16 buf 15;
      buf_client proto buf client_info
  
  | Client_state (int, host_state) -> buf_int16 buf 16;
      buf_int buf int; buf_host_state proto buf host_state
  
  | Client_friend (int, client_type) -> buf_int16 buf 17;
      buf_int buf int; buf_client_type buf client_type
  
  | Client_file (n1, s, n2) -> buf_int16 buf 18;
      buf_int buf n1; buf_string buf s; buf_int buf n2
  
  | Console string -> buf_int16 buf 19;
      buf_string buf string
  
  | Network_info network_info -> buf_int16 buf 20;
      buf_network buf network_info
  
  | User_info user_info -> buf_int16 buf 21;
      buf_user buf user_info
  
  | Room_info room_info -> 
      buf_int16 buf (if proto < 3 then 22 else 31);
      buf_room proto buf room_info
  
  | Room_message (int, room_message) -> buf_int16 buf 23;
      buf_int buf int; buf_message buf room_message
  
  | Room_add_user (n1,n2) -> buf_int16 buf 24;
      buf_int buf n1; buf_int buf n2
  
  | MessageFromClient (num, msg) ->
      if proto < 3 then
(* This message was previously send like that ... *)
        
        to_gui proto buf (Room_message (0, PrivateMessage(num, msg)))
      else begin
          buf_int16 buf 27;
          buf_int buf num;
          buf_string buf msg
        end
  
  | BadPassword -> buf_int16 buf 47
  
  | DownloadFiles list ->      
      buf_int16 buf (if proto < 8 then 29 else 
        if proto < 9 then 41 else
        if proto < 14 then  44 else 53);
      buf_list buf (buf_file proto) list
  
  | DownloadedFiles list ->      
      buf_int16 buf (if proto < 8 then 30 else 
        if proto < 9 then 42 else 
        if proto < 14 then  45 else 54);
      buf_list buf (buf_file proto) list
  
  | ConnectedServers list ->      buf_int16 buf 28;
      buf_list buf (buf_server proto) list
  
  | Client_stats s -> 
      buf_int16 buf (if proto < 5 then 25 else
        if proto < 6 then 37 else
        if proto < 10 then 39 else 49);          
      buf_int64 buf s.upload_counter;
      buf_int64 buf s.download_counter;      
      buf_int64 buf s.shared_counter;
      buf_int buf s.nshared_files;
      
      if proto > 4 then
        if proto < 6 then begin
            buf_int buf (s.tcp_upload_rate + s.udp_upload_rate);
            buf_int buf (s.tcp_download_rate + s.udp_download_rate);
          end else begin
            buf_int buf s.tcp_upload_rate;
            buf_int buf s.tcp_download_rate;
            buf_int buf s.udp_upload_rate;
            buf_int buf s.udp_download_rate;
            
            if proto > 9 then begin
                buf_int buf s.ndownloading_files;
                buf_int buf s.ndownloaded_files;
                buf_list buf buf_int s.connected_networks;
              end
          end                
          
  | Room_remove_user (room, user) ->       buf_int16 buf 32;
      buf_int buf room;
      buf_int buf user

  | Shared_file_info  shared_info ->       
      buf_int16 buf (if proto < 10 then 33 else 48);
      buf_shared_info proto buf shared_info
      
  | Shared_file_upload (num, upload,requests) ->    buf_int16 buf 34;
      buf_int buf num;
      buf_int64 buf upload;
      buf_int buf requests
  | Shared_file_unshared num -> buf_int16 buf 35;
      buf_int buf num
 
  | Add_section_option (section, message, option, optype) -> buf_int16 buf 36;
      buf_string buf section;
      buf_string buf message;
      buf_string buf option;
      buf_int8 buf (match optype with
          StringEntry -> 0
        | BoolEntry -> 1
        | FileEntry -> 2)
 
  | Add_plugin_option (section, message, option, optype) -> buf_int16 buf 38;
      buf_string buf section;
      buf_string buf message;
      buf_string buf option;
      buf_int8 buf (match optype with
          StringEntry -> 0
        | BoolEntry -> 1
        | FileEntry -> 2)

  | File_remove_source (n1,n2) -> buf_int16 buf 50;
      buf_int buf n1; buf_int buf n2

  | File_update_availability (file_num, client_num, avail) -> 
      if proto < 11 then raise UnsupportedGuiMessage;
      buf_int16 buf 9;
      buf_int buf file_num; buf_int buf client_num; buf_string buf avail

  | CleanTables (clients, servers) ->      
      if proto < 11 then raise UnsupportedGuiMessage;
      buf_int16 buf 51;
      buf_list buf (fun buf i -> buf_int buf i) clients;
      buf_list buf (fun buf i -> buf_int buf i) servers
        
(***************

     Encoding of messages from the GUI to the Core 

****************)

let rec from_gui proto buf t =
  match t with
  | GuiProtocol int -> buf_int16 buf 0;
      buf_int buf int
  
  | ConnectMore_query -> buf_int16 buf 1
  | CleanOldServers -> buf_int16 buf 2
  | KillServer -> buf_int16 buf 3
  | ExtendedSearch _ -> buf_int16 buf 4
  | Password (login, pass) -> 
      buf_int16 buf (if proto < 14 then 5 else 52);
      buf_string buf pass;
      if proto > 13 then 
        buf_string buf login
        
  | Search_query search -> 
      
      if proto < 2 then begin
          buf_int16 buf 6;
          buf_bool buf (search.search_type = LocalSearch); 
        end else begin
          buf_int16 buf 42;          
        end;
      buf_search proto buf search
  | Download_query (list, int, force) -> 
      buf_int16 buf (if proto < 14 then 7 else 50);
      buf_list buf buf_string list; 
      buf_int buf int;
      if proto > 13 then
        buf_bool buf force
        
  | Url string -> buf_int16 buf 8;
      buf_string buf string
  | RemoveServer_query int -> buf_int16 buf 9;
      buf_int buf int
  | SaveOptions_query list -> buf_int16 buf 10;
      buf_list buf (fun buf (s1,s2) ->
          buf_string buf s1; buf_string buf s2) list
  
  | RemoveDownload_query  int -> buf_int16 buf 11;
      buf_int buf int

      
  | ServerUsers_query  int -> buf_int16 buf 12;
      buf_int buf int
  | SaveFile (int, string) -> buf_int16 buf 13;
      buf_int buf int; buf_string buf string
  | AddClientFriend  int -> buf_int16 buf 14;
      buf_int buf int
  | AddUserFriend  int -> buf_int16 buf 15;
      buf_int buf int
  | RemoveFriend  int -> buf_int16 buf 16;
      buf_int buf int
  | RemoveAllFriends -> buf_int16 buf 17;
  | FindFriend string -> buf_int16 buf 18;
      buf_string buf string
  | ViewUsers  int -> buf_int16 buf 19;
      buf_int buf int
  | ConnectAll  int -> buf_int16 buf 20;
      buf_int buf int
  | ConnectServer  int -> buf_int16 buf 21;
      buf_int buf int
  | DisconnectServer  int -> buf_int16 buf 22;
      buf_int buf int
  | SwitchDownload  (int, bool) -> buf_int16 buf 23;
      buf_int buf int; buf_bool buf bool
  | VerifyAllChunks  int -> buf_int16 buf 24;
      buf_int buf int
  | QueryFormat  int -> buf_int16 buf 25;
      buf_int buf int
  | ModifyMp3Tags (int, tag) -> buf_int16 buf 26;
      buf_int buf int; buf_mp3 buf tag
  | CloseSearch  (int,bool) -> 
      if proto < 15 then begin
          buf_int16 buf 27;
          buf_int buf int
        end else begin
          buf_int16 buf 53;
          buf_int buf int;         
          buf_bool buf bool;         
        end
  | SetOption (s1, s2) -> buf_int16 buf 28;
      buf_string buf s1; buf_string buf s2
  | Command string -> buf_int16 buf 29;
      buf_string buf string
  | Preview  int -> buf_int16 buf 30;
      buf_int buf int
  | ConnectFriend  int -> buf_int16 buf 31;
      buf_int buf int  
  | GetServer_users  int -> buf_int16 buf 32;
      buf_int buf int
  | GetClient_files  int -> buf_int16 buf 33;
      buf_int buf int
  | GetFile_locations  int -> buf_int16 buf 34;
      buf_int buf int
  | GetServer_info  int -> buf_int16 buf 35;
      buf_int buf int
  | GetClient_info  int -> buf_int16 buf 36;
      buf_int buf int
  | GetFile_info  int -> buf_int16 buf 37;
      buf_int buf int
  | GetUser_info  int -> buf_int16 buf 38;
      buf_int buf int
  | SendMessage (int, room_message) -> buf_int16 buf 39;
      buf_int buf int;
      buf_message buf room_message
  | EnableNetwork (int, bool) -> buf_int16 buf 40;
      buf_int buf int; buf_bool buf bool
  
  | BrowseUser  user ->       buf_int16 buf 41;
      buf_int buf user
  
  | MessageToClient (c,m) ->
      if proto < 3 then
(* On previous GUIs, this was done like that ! *)
        from_gui proto buf (SendMessage (-1, PrivateMessage(c,m)))
      else begin
          buf_int16 buf 43 ;
          buf_int buf c;
          buf_string buf m
        end
        
(* These messages are not supported by the core with the provided 
protocol version. Do not send them ? *)
        
(* Introduced with proto 3 *)
  | GetConnectedServers -> buf_int16 buf 44
  | GetDownloadFiles -> buf_int16 buf 45
  | GetDownloadedFiles -> buf_int16 buf 46
      
  | GuiExtensions list -> 
      buf_int16 buf 47;
      buf_list buf (fun buf (ext, bool) ->
          buf_int buf ext;
          buf_int8 buf (if bool then 1 else 0);
      ) list
      
  | SetRoomState (num, state) ->
      buf_int16 buf 48;
      buf_int buf num;
      buf_room_state buf state
      
(* Introduced with proto 4 *)
  | RefreshUploadStats ->      buf_int16 buf 49

(* Introduced with proto 7 *)
        
  | SetFilePriority (num, prio) ->
      if proto >= 12 then
        buf_int16 buf 51; buf_int buf num; buf_int buf prio

  | AddServer_query (net, ip, port) ->
      buf_int16 buf 54;
      buf_int buf net;
      buf_ip buf ip;
      buf_int16 buf port


let best_gui_version = 16
  
(********** Some assertions *********)
  
(*
Writting without bugs is impossible, so just put assertions here
that you want to be checked at startup. For example, check that the 
(decoder o encoder)(msg) = msg ! In particular, it avoids using several
times the same opcode for different messages :)
*)
      
let _ = 
  
  let check encoder decoder msg =
    try
      let buf = Buffer.create 1000 in
      encoder buf msg;
      let s = Buffer.contents buf in
      let opcode = get_int16 s 0 in
      let v = decoder opcode s in
      if (decoder opcode s <> msg) then begin
          AnyEndian.dump s;
          let buf = Buffer.create 1000 in
          encoder buf msg;
          let s2 = Buffer.contents buf in
          AnyEndian.dump s2;
          if s = s2 then
            (lprintf "s = s2"; lprint_newline (); false)
          else
          let len = String.length s in
          let len2 = String.length s2 in
          if len <> len2 then
            (lprintf "different lengths"; lprint_newline (); false)
          else
            (for i = 0 to len-1 do
                if s.[i] <> s2.[i] then
                  lprintf "diff at pos %d(%d)" i
                    (int_of_char s.[i]); lprint_newline ();
              done;
              false)
        end else
      true
        
    with e ->
        lprintf "Exception %s in check" (Printexc2.to_string e);
        lprint_newline ();
        false
  in
(* and    server_info = {
      server_num = 1;
} *)
  let proto = best_gui_version in
  let to_gui = to_gui proto in
  let check_to_gui = 
    check to_gui (GuiDecoding.to_gui proto) in
  assert (check_to_gui (MessageFromClient (32, "Hello")));
  assert (check_to_gui (File_info file_info_test)); 
  assert (check_to_gui (DownloadFiles [file_info_test]));
  assert (check_to_gui (DownloadedFiles [file_info_test]));  
  assert (check_to_gui (ConnectedServers []));    
  assert (check_to_gui (Room_remove_user (5,6)));
  assert (check_to_gui (Shared_file_upload (1, Int64.zero, 32)));
  assert (check_to_gui (Shared_file_unshared 2));
  (* Shared_file_info ??? *)
  assert (check_to_gui (Add_section_option ("section", "message", "option", StringEntry )));
  assert (check_to_gui (Add_plugin_option ("section", "message", "option", StringEntry )));
  
  let check_from_gui = 
    check (from_gui proto)  (GuiDecoding.from_gui proto) in
  assert (check_from_gui (MessageToClient (33, "Bye")));
  assert (check_from_gui (GuiExtensions [1, true; 2, false]));
  assert (check_from_gui GetConnectedServers);
  assert (check_from_gui GetDownloadFiles);
  assert (check_from_gui GetDownloadedFiles);
  assert (check_from_gui (SetRoomState (5, RoomPaused)));
  assert (check_from_gui RefreshUploadStats) ; 
  assert (check_from_gui (SetFilePriority (5,6)));
  assert (check_from_gui (Password ("mldonkey", "toto")));
  