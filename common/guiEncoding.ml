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
open CommonGlobals
open CommonTypes
open GuiTypes
open GuiProto  
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

let buf_tag buf t =
  buf_string buf t.tag_name;
  match t.tag_value with
  | Uint64 s -> buf_int8 buf 0; buf_int64_32 buf s
  | Fint64 s -> buf_int8 buf 1; buf_int64_32 buf s
  | String s -> buf_int8 buf 2; buf_string buf s
  | Addr ip -> buf_int8 buf 3; buf_ip buf ip
    
let buf_host_state proto buf t =
  buf_int8 buf (    
    match t with
      NotConnected -> 0
    | Connecting -> 1
    | Connected_initiating -> 2
    | Connected_busy -> 3
    | Connected_idle -> 4
    | Connected_queued -> 5
    | NewHost -> 6
    | RemovedHost -> 7
    | BlackListedHost -> if proto < 10 then 0 else 8)
     
  
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

let buf_file_state buf s =
  buf_int8 buf (match s with
      FileDownloading -> 0
    | FilePaused -> 1
    | FileDownloaded -> 2
    | FileShared -> 3    
    | FileCancelled -> 4
    | FileNew -> 5
  )
  
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
  | Unknown_format -> buf_int8 buf 0
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
  buf_file_state buf f.file_state;  
  buf_string buf f.file_chunks;  
  buf_string buf f.file_availability;  
  buf_float buf f.file_download_rate;  
  buf_array buf buf_int_float f.file_chunks_age;  
  buf_int_float buf f.file_age;
(* last, so that it can be safely discarded in partial implementations: *)
  buf_format buf f.file_format;
  if proto >= 8 then begin
      buf_string buf f.file_name;
      if proto >= 9 then 
        buf_int buf (compute_last_seen f.file_last_seen)
    end
  
let buf_addr buf addr =
  if addr.addr_name = "" then begin
      buf_int8 buf 0;
      buf_ip buf addr.addr_ip;
    end else begin
      buf_int8 buf 1;
      buf_string buf addr.addr_name
    end
  
let buf_server proto buf s =
  buf_int buf s.server_num;
  buf_int buf s.server_network;
  if proto < 2 then 
    buf_ip buf s.server_addr.addr_ip
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
    buf_search_type buf s.search_type

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
  
let rec to_gui_version_0 proto buf t =
  match t with
    
  | CoreProtocol version -> 
      buf_int16 buf 0; 
      buf_int buf version
      
  | Options_info list -> buf_int16 buf 1; 
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
  
  | File_info file_info -> buf_int16 buf 7;
      buf_file proto buf file_info
      
  | File_downloaded (n, size, rate, last_seen) -> buf_int16 buf 8;
      buf_int buf n; buf_int64_32 buf size; buf_float buf rate
            
  | File_add_source (n1,n2) -> buf_int16 buf 10;
      buf_int buf n1; buf_int buf n2
  
  | Server_busy (n1,n2,n3) -> buf_int16 buf 11;
      buf_int buf n1; buf_int buf n2; buf_int buf n3
      
  | Server_user  (n1,n2) -> buf_int16 buf 12;
      buf_int buf n1; buf_int buf n2
      
  | Server_state (int,host_state) -> buf_int16 buf 13;
      buf_int buf int; buf_host_state proto buf host_state
      
  | Server_info server_info -> buf_int16 buf 14;
      buf_server proto buf server_info
  
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
  
  | Room_info room_info -> buf_int16 buf 22;
      buf_room proto buf room_info
      
  | Room_message (int, room_message) -> buf_int16 buf 23;
      buf_int buf int; buf_message buf room_message
      
  | Room_add_user (n1,n2) -> buf_int16 buf 24;
      buf_int buf n1; buf_int buf n2
      
  | MessageFromClient (num, msg) ->
(* This message was previously send like that ... *)
      to_gui_version_0 proto buf (Room_message (0, PrivateMessage(num, msg)))

      
  | BadPassword -> buf_int16 buf 47

  | DownloadedFiles list ->      
      buf_int16 buf 30;
      buf_list buf (buf_file proto) list

  | DownloadFiles list ->      buf_int16 buf 29;
      buf_list buf (buf_file proto) list

  | ConnectedServers list ->      buf_int16 buf 28;
      buf_list buf (buf_server proto) list
      
  | Client_stats s -> buf_int16 buf 25;
      buf_int64 buf s.upload_counter;
      buf_int64 buf s.download_counter;      
      buf_int64 buf s.shared_counter;
      buf_int buf s.nshared_files

  | Room_remove_user (room, user) ->       buf_int16 buf 32;
      buf_int buf room;
      buf_int buf user

  | Shared_file_info  shared_info ->       buf_int16 buf 33;
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

  | File_update_availability _ -> raise UnsupportedGuiMessage
      
let to_gui_version_2 proto buf t =
  match t with
    Server_info s ->
      buf_int16 buf 26;
      buf_server proto buf s
  | _ -> to_gui_version_0 proto buf t
      
let to_gui_version_3 proto buf t =
  match t with
          
  | MessageFromClient (int, s) ->       buf_int16 buf 27;
      buf_int buf int;
      buf_string buf s
            
  | Room_info info ->
      buf_int16 buf 31;
      buf_room proto buf info
      
  | _ -> to_gui_version_2 proto buf t
      
let to_gui_version_5 proto buf t =
  match t with
         
  | Client_stats s -> buf_int16 buf 37;
      buf_int64 buf s.upload_counter;
      buf_int64 buf s.download_counter;      
      buf_int64 buf s.shared_counter;
      buf_int buf s.nshared_files;
      buf_int buf (s.tcp_upload_rate + s.udp_upload_rate);
      buf_int buf (s.tcp_download_rate + s.udp_download_rate);
      
  | _ -> to_gui_version_3 proto buf t
      
let to_gui_version_6 proto buf t =
  match t with
         
  | Client_stats s -> buf_int16 buf 39;
      buf_int64 buf s.upload_counter;
      buf_int64 buf s.download_counter;      
      buf_int64 buf s.shared_counter;
      buf_int buf s.nshared_files;
      buf_int buf s.tcp_upload_rate;
      buf_int buf s.tcp_download_rate;
      buf_int buf s.udp_upload_rate;
      buf_int buf s.udp_download_rate;
      
  | _ -> to_gui_version_5 proto buf t

let to_gui_version_8 proto buf t =
  match t with
    File_info file -> buf_int16 buf 40;
      buf_file proto buf file      
      
  | DownloadFiles files -> buf_int16 buf 41;
      buf_list buf (buf_file proto) files
      
  | DownloadedFiles files -> buf_int16 buf 42;
      buf_list buf (buf_file proto) files
      
  | _ -> to_gui_version_6 proto buf t

let to_gui_version_9 proto buf t =
  match t with
    File_info file -> buf_int16 buf 43;
      buf_file proto buf file      
      
  | DownloadFiles files -> buf_int16 buf 44;
      buf_list buf (buf_file proto) files
      
  | DownloadedFiles files -> buf_int16 buf 45;
      buf_list buf (buf_file proto) files
      
  | File_downloaded (n, size, rate, last_seen) -> buf_int16 buf 46;
      buf_int buf n; buf_int64_32 buf size; buf_float buf rate; 
      buf_int buf (compute_last_seen last_seen)
      
  | _ -> to_gui_version_8 proto buf t

let to_gui_version_10 proto buf t =
  match t with

  | Shared_file_info  shared_info ->       buf_int16 buf 48;
      buf_shared_info proto buf shared_info

  | Client_stats s ->       buf_int16 buf 49;
      buf_int64 buf s.upload_counter;
      buf_int64 buf s.download_counter;      
      buf_int64 buf s.shared_counter;
      buf_int buf s.nshared_files;
      buf_int buf s.tcp_upload_rate;
      buf_int buf s.tcp_download_rate;
      buf_int buf s.udp_upload_rate;
      buf_int buf s.udp_download_rate;
      buf_int buf s.ndownloading_files;
      buf_int buf s.ndownloaded_files;
      buf_list buf buf_int s.connected_networks;
      
  | _ -> to_gui_version_9 proto buf t

      
let to_gui_version_11 proto buf t =
  match t with

  | File_update_availability (file_num, client_num, avail) -> buf_int16 buf 9;
      buf_int buf file_num; buf_int buf client_num; buf_string buf avail

  | _ -> to_gui_version_10 proto buf t
(* next message must be 49 *) 
      
let to_gui_funs = [| 
    to_gui_version_0; 
    to_gui_version_0;
    to_gui_version_2;
    to_gui_version_3;
    to_gui_version_3;
    to_gui_version_5;
    to_gui_version_6;
    to_gui_version_6; 
    to_gui_version_8; 
    to_gui_version_9; 
    to_gui_version_10; 
    to_gui_version_11; 
  |]

let to_gui proto = to_gui_funs.(proto) proto
  
(***************

     Encoding of messages from the GUI to the Core 

****************)

let rec from_gui_version_0 proto buf t =
  match t with
  | GuiProtocol int -> buf_int16 buf 0;
      buf_int buf int
  
  | ConnectMore_query -> buf_int16 buf 1
  | CleanOldServers -> buf_int16 buf 2
  | KillServer -> buf_int16 buf 3
  | ExtendedSearch _ -> buf_int16 buf 4
  | Password string -> buf_int16 buf 5;
      buf_string buf string
  | Search_query search -> buf_int16 buf 6;
      buf_bool buf (search.search_type = LocalSearch); 
      buf_search proto buf search
  | Download_query (list, int, _) -> buf_int16 buf 7;
      buf_list buf buf_string list; buf_int buf int
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
  | ForgetSearch  int -> buf_int16 buf 27;
      buf_int buf int
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
(* On previous GUIs, this was done like that ! *)
      from_gui_version_0 proto buf (SendMessage (-1, PrivateMessage(c,m)))
      
(* These messages are not supported by the core with the provided 
protocol version. Do not send them! *)
  | GuiExtensions _
  | GetDownloadedFiles
  | GetDownloadFiles
  | GetConnectedServers
  | SetRoomState _ 
  | RefreshUploadStats
    -> raise UnsupportedGuiMessage
    
let from_gui_version_2 proto buf t = 
  match t with
    Search_query s ->
      buf_int16 buf 42;
      buf_search proto buf s
  | _ -> from_gui_version_0 proto buf t
      
let from_gui_version_3 proto buf t = 
  match t with

  | MessageToClient (int, message) ->
      buf_int16 buf 43 ;
      buf_int buf int;
      buf_string buf message
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
  | _ -> from_gui_version_2 proto buf t

let from_gui_version_4 proto buf t  = 
  match t with
  | RefreshUploadStats ->      buf_int16 buf 49
  | _ -> from_gui_version_3 proto buf t


let from_gui_version_7 proto buf t  = 
  match t with
  | Download_query (list, int, force) -> buf_int16 buf 7;
      buf_list buf buf_string list; buf_int buf int; buf_bool buf force
  | _ -> from_gui_version_4 proto buf t
        
let from_gui_funs = [| 
    from_gui_version_0; 
    from_gui_version_0;
    from_gui_version_2; 
    from_gui_version_3; 
    from_gui_version_4; 
    from_gui_version_4; 
    from_gui_version_4; 
    from_gui_version_7;  
    from_gui_version_7;  
    from_gui_version_7;  
    from_gui_version_7;  
    from_gui_version_7;  
    |]

  

let from_gui proto = from_gui_funs.(proto) proto
  
let _ =
  assert (Array.length to_gui_funs = Array.length from_gui_funs)

let best_gui_version = Array.length from_gui_funs - 1
  
  
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
      decoder opcode s = msg
    with e ->
        Printf.printf "Exception %s in check" (Printexc2.to_string e);
        print_newline ();
        false
  in
  let module P = GuiTypes in
  let file_info = {
      P.file_name = "tratra";
      P.file_num = 356;
      P.file_network = 873;
      P.file_names = ["toto"; "tutu"];
      P.file_md4 = Md4.random ();
      P.file_size = Int64.of_string "68758765";
      P.file_downloaded = Int64.of_string "68758764";
      P.file_nlocations = 12;
      P.file_nclients = 18;
      P.file_state = FileDownloading;
      P.file_sources = None;
      P.file_download_rate = 2.2;
      P.file_chunks = "1010100";
      P.file_availability = "012012210";
      P.file_format = Unknown_format;
      P.file_chunks_age = [| 2 |];
      P.file_age = 3;
      P.file_last_seen = BasicSocket.last_time ();
    } 
(* and    server_info = {
      server_num = 1;
} *)
  in
  
  let to_gui = to_gui 11 in
  let check_to_gui = 
    check to_gui GuiDecoding.to_gui in
  assert (check_to_gui (MessageFromClient (32, "Hello")));
  assert (check_to_gui (File_info file_info));
  assert (check_to_gui (DownloadFiles [file_info]));
  assert (check_to_gui (DownloadedFiles [file_info]));
  assert (check_to_gui (ConnectedServers []));    
  assert (check_to_gui (Room_remove_user (5,6)));
  assert (check_to_gui (Shared_file_upload (1, Int64.zero, 32)));
  assert (check_to_gui (Shared_file_unshared 2));
  (* Shared_file_info ??? *)
  assert (check_to_gui (Add_section_option ("section", "message", "option", StringEntry )));
  assert (check_to_gui (Add_plugin_option ("section", "message", "option", StringEntry )));
  
  let check_from_gui = 
    check (from_gui 11)  GuiDecoding.from_gui in
  assert (check_from_gui (MessageToClient (33, "Bye")));
  assert (check_from_gui (GuiExtensions [1, true; 2, false]));
  assert (check_from_gui GetConnectedServers);
  assert (check_from_gui GetDownloadFiles);
  assert (check_from_gui GetDownloadedFiles);
  assert (check_from_gui (SetRoomState (5, RoomPaused)));
  assert (check_from_gui RefreshUploadStats) ; 
