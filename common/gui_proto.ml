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

open CommonGlobals
open CommonTypes


type options = {
    mutable connection_port : int;
    mutable control_port : int;
    mutable gui_port : int;
    
    mutable save_options_delay : float;
    mutable check_client_connections_delay : float;
    mutable check_server_connections_delay : float;
    mutable small_retry_delay : float;
    mutable medium_retry_delay : float;
    mutable long_retry_delay : float;
    
    mutable name : string;
    mutable max_connected_servers : int;
    mutable upload_limit : int;
    mutable features : string;
    
    mutable server_timeout: float;
    mutable client_timeout: float;
    mutable max_server_age : int;
    mutable password : string;
  }
  
type 'a search_request = {
    mutable search_num : int;
    mutable search_type : search_type;
    mutable search_query : 'a; (* query_entry for the GUI *)
    mutable search_max_hits : int;
  }

  
type file_info = {
    file_num : int;    
    file_network : int;
    
    mutable file_names : string list;
    mutable file_md4 : Md4.t;        
    mutable file_size : int32;
    mutable file_downloaded : int32; (* LOT OF CHANGES *)
    mutable file_nlocations : int; (* MANY CHANGES *)
    mutable file_nclients: int;

    mutable file_state : file_state;
    mutable file_chunks : string;
    mutable file_availability : string; (* MANY CHANGES *)
    mutable file_sources : int list option;
    mutable file_download_rate : float; (* LOT OF CHANGES *)
    mutable file_format : format;
    mutable file_chunks_age : float array;
    mutable file_age : float;
  }
  
type user_info = {
    user_num : int;
    user_md4 : Md4.t;
    user_name : string;
    user_ip : Ip.t;
    user_port : int;
    mutable user_tags : tag list;
    user_server : int;
  }

type server_info = {
    server_num : int;
    server_network : int;
    
    mutable server_addr : addr;
    mutable server_port : int;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_state : host_state;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users : int list option;
  } 

type room_info = {
    room_num : int;
    room_network : int;
    room_name : string;
    mutable room_state : room_state;
    mutable room_users : int list;
  }
  
type client_info = {
    client_num : int;
    client_network : int;
    
    mutable client_kind : location_kind;
    mutable client_state : host_state;
    mutable client_type : client_type;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
(* Currently, this is a list, but clearly, in the future, it has to become
  a tree. Zoggy, could you implement that ? *)
    mutable client_files:  result_info list option;
    mutable client_rating : int32;
    mutable client_chat_port : int;
  }

type client_stats = {
    mutable upload_counter : int64;
    mutable download_counter : int64;
    mutable nshared_files : int;
    mutable shared_counter : int64;
  }
  
exception UnsupportedGuiMessage
  
type from_gui =
| GuiProtocol of int
  
| ConnectMore_query
| CleanOldServers
| KillServer
| ExtendedSearch
| Password of string
| Search_query of query_entry search_request
| Download_query of string list * int 
| Url of string
| RemoveServer_query of int
| SaveOptions_query of (string * string) list (* options *)
| RemoveDownload_query of int
| ServerUsers_query of int
| SaveFile of int * string
| AddClientFriend of int
| AddUserFriend of int
| RemoveFriend of int
| RemoveAllFriends
| FindFriend of string
| ViewUsers of int
| ConnectAll of int
| ConnectServer of int
| DisconnectServer of int
| SwitchDownload of int * bool
| VerifyAllChunks of int
| QueryFormat of int
| ModifyMp3Tags of int * Mp3tag.tag
| ForgetSearch of int
| SetOption of string * string
| Command of string
| Preview of int
| ConnectFriend of int  
| GetServer_users of int
| GetClient_files of int
| GetFile_locations of int
| GetServer_info of int
| GetClient_info of int
| GetFile_info of int
| GetUser_info of int
| SendMessage of int * room_message
| EnableNetwork of int * bool
| BrowseUser of int
  
type to_gui =
| CoreProtocol of int
  
| Options_info of (string * string) list (*  options *)
| DefineSearches of (string * CommonTypes.query_entry) list

| Result_info of result_info
  
| Search_result of int * int
| Search_waiting of int * int
  
| File_info of file_info
| File_downloaded of int * int32 * float
| File_availability of int * string * string
| File_source of int * int
  
| Server_busy of int * int * int
| Server_user of int * int
| Server_state of int * host_state
| Server_info of server_info
  
| Client_info of client_info
| Client_state of int * host_state
| Client_friend of int * client_type
| Client_file of int * string * int

| Console of string

| Network_info of network_info
| User_info of user_info

| Room_info of room_info
| Room_message of int * room_message
| Room_user of int * int

| Client_stats of client_stats
  
type arg_handler =  connection_options -> string
type arg_kind = 
  Arg_none of arg_handler
| Arg_multiple of (string list -> arg_handler)
| Arg_one of (string -> arg_handler)
| Arg_two of (string -> string -> arg_handler)
| Arg_three of (string -> string -> string -> arg_handler)

let from_gui_to_string t = 
  match t with
  | GuiProtocol _ -> "GuiProtocol"
  | ConnectMore_query -> "ConnectMore_query"
  | CleanOldServers -> "CleanOldServers"
  | KillServer -> "KillServer"
  | ExtendedSearch -> "ExtendedSearch"
  | Password _ -> "Password"
  | Search_query _ -> "Search_query"
  | Download_query _ -> "Download_query"
  | Url _ -> "Url"
  | RemoveServer_query _ -> "RemoveServer_query"
  | SaveOptions_query _ -> "SaveOptions_query"
  | RemoveDownload_query _ -> "RemoveDownload_query"
  | ServerUsers_query _ -> "ServerUsers_query"
  | SaveFile _ -> "SaveFile"
  | AddClientFriend _ -> "AddClientFriend"
  | AddUserFriend _ -> "AddUserFriend"
  | RemoveFriend _ -> "RemoveFriend"
  | RemoveAllFriends -> "RemoveAllFriends"
  | FindFriend _ -> "FindFriend"
  | ViewUsers _ -> "ViewUsers"
  | ConnectAll _ -> "ConnectAll"
  | ConnectServer _ -> "ConnectServer"
  | DisconnectServer _ -> "DisconnectServer"
  | SwitchDownload _ -> "SwitchDownload"
  | VerifyAllChunks _ -> "VerifyAllChunks"
  | QueryFormat _ -> "QueryFormat"
  | ModifyMp3Tags _ -> "ModifyMp"
  | ForgetSearch _ -> "ForgetSearch"
  | SetOption _ -> "SetOption"
  | Command _ -> "Command"
  | Preview _ -> "Preview"
  | ConnectFriend _ -> "ConnectFriend"
  | GetServer_users _ -> "GetServer_users"
  | GetClient_files _ -> "GetClient_files"
  | GetFile_locations _ -> "GetFile_locations"
  | GetServer_info _ -> "GetServer_info"
  | GetClient_info _ -> "GetClient_info"
  | GetFile_info _ -> "GetFile_info"
  | GetUser_info n -> Printf.sprintf "GetUser_info %d" n
  | SendMessage _ -> "SendMessage"
  | EnableNetwork _ -> "EnableNetwork"
  | BrowseUser _ -> "BrowseUser"      


open LittleEndian
open TcpBufferedSocket
      
let gui_cut_messages f sock nread =
  let b = buf sock in
  try
    while b.len >= 4 do
      let msg_len = get_int b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let s = String.sub b.buf (b.pos+4) msg_len in
          buf_used sock (msg_len + 4);
          let opcode = get_int16 s 0 in
          (f opcode s : unit)
        end
      else raise Not_found
    done
  with Not_found -> ()


      
let buf = Buffer.create 1000

      
let gui_send writer sock t = 
  Buffer.clear buf;
  buf_int buf 0;
  writer buf t;
  let s = Buffer.contents buf in
  let len = String.length s - 4 in
  str_int s 0 len;
  write_string sock s

let buf = ()
  
module Encoding = struct

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
  buf_string buf (Printf.sprintf "%2.2f" f)
  
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
  | Uint32 s -> buf_int8 buf 0; buf_int32 buf s
  | Fint32 s -> buf_int8 buf 1; buf_int32 buf s
  | String s -> buf_int8 buf 2; buf_string buf s
  | Addr ip -> buf_int8 buf 3; buf_ip buf ip
    
let buf_host_state buf t =
  buf_int8 buf (    
    match t with
      NotConnected -> 0
    | Connecting -> 1
    | Connected_initiating -> 2
    | Connected_busy -> 3
    | Connected_idle -> 4
    | Connected_queued -> 5
    | NewHost -> 6
    | RemovedHost -> 7)

  
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
  buf_int32 buf r.result_size;
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
  
let buf_room buf r =
  buf_int buf r.room_num;
  buf_int buf r.room_network;
  buf_string buf r.room_name;
  buf_room_state buf r.room_state

let buf_message buf m =
  match m with
    ServerMessage s ->      buf_int8 buf 0; buf_string buf s 
  | PublicMessage (n, s) -> buf_int8 buf 1; buf_int buf n; buf_string buf s
  | PrivateMessage (n,s) -> buf_int8 buf 2; buf_int buf n; buf_string buf s

let buf_mp3 buf t =
  let module M = Mp3tag in
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
  | Mp3 t -> 
      buf_int8 buf 3;
      buf_mp3 buf t
      
let buf_kind buf k =
  match k with
    Known_location (ip, port) -> 
      buf_int8 buf 0; buf_ip buf ip; buf_int16 buf port
  | Indirect_location (name, md4) ->
      buf_int8 buf 1; buf_string buf name; buf_md4 buf md4
      
let buf_file buf f =
  buf_int buf f.file_num;
  buf_int buf f.file_network;  
  buf_list buf buf_string f.file_names;  
  buf_md4 buf f.file_md4;  
  buf_int32 buf f.file_size;  
  buf_int32 buf f.file_downloaded;  
  buf_int buf f.file_nlocations;  
  buf_int buf f.file_nclients;  
  buf_file_state buf f.file_state;  
  buf_string buf f.file_chunks;  
  buf_string buf f.file_availability;  
  buf_float buf f.file_download_rate;  
  buf_array buf buf_float f.file_chunks_age;  
  buf_float buf f.file_age;
(* last, so that it can be safely discarded in partial implementations: *)
  buf_format buf f.file_format 
  
let buf_server_version_0 buf s =
  buf_int buf s.server_num;
  buf_int buf s.server_network;
  buf_ip buf s.server_addr.addr_ip;
  buf_int16 buf s.server_port;
  buf_int buf s.server_score;
  buf_list buf buf_tag s.server_tags;
  buf_int buf s.server_nusers;
  buf_int buf s.server_nfiles;
  buf_host_state buf s.server_state;
  buf_string buf s.server_name;
  buf_string buf s.server_description

let buf_addr buf addr =
  if addr.addr_name = "" then begin
      buf_int8 buf 0;
      buf_ip buf addr.addr_ip;
    end else begin
      buf_int8 buf 1;
      buf_string buf addr.addr_name
    end
    
  
let buf_server_version_2 buf s =
  buf_int buf s.server_num;
  buf_int buf s.server_network;
  buf_addr buf s.server_addr;
  buf_int16 buf s.server_port;
  buf_int buf s.server_score;
  buf_list buf buf_tag s.server_tags;
  buf_int buf s.server_nusers;
  buf_int buf s.server_nfiles;
  buf_host_state buf s.server_state;
  buf_string buf s.server_name;
  buf_string buf s.server_description
  
let buf_client buf c =
  buf_int buf c.client_num;
  buf_int buf c.client_network;
  buf_kind buf c.client_kind;
  buf_host_state buf c.client_state;
  buf_client_type buf c.client_type;
  buf_list buf buf_tag c.client_tags;
  buf_string buf c.client_name;
  buf_int32 buf c.client_rating;
  buf_int buf c.client_chat_port
  
let buf_network buf n =
  buf_int buf n.network_netnum;
  buf_string buf n.network_netname;
  buf_bool buf n.network_enabled;
  buf_string buf n.network_config_filename;
  buf_int64 buf n.network_uploaded;
  buf_int64 buf n.network_downloaded
  
let buf_search_version_0 buf s = 
  buf_int buf s.search_num;
  buf_query buf s.search_query;
  buf_int buf s.search_max_hits

let buf_search_type buf t =
  buf_int8 buf (
    match t with
      LocalSearch -> 0
    | RemoteSearch -> 1
    | SubscribeSearch -> 2)
  
  
let buf_search_version_2 buf s = 
  buf_int buf s.search_num;
  buf_query buf s.search_query;
  buf_int buf s.search_max_hits;
  buf_search_type buf s.search_type

  
let to_gui_version_0 buf t =
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
      buf_file buf file_info
      
  | File_downloaded (n, size, rate) -> buf_int16 buf 8;
      buf_int buf n; buf_int32 buf size; buf_float buf rate
      
  | File_availability (n, s1, s2) -> buf_int16 buf 9;
      buf_int buf n; buf_string buf s1; buf_string buf s2
      
  | File_source (n1,n2) -> buf_int16 buf 10;
      buf_int buf n1; buf_int buf n2
  
  | Server_busy (n1,n2,n3) -> buf_int16 buf 11;
      buf_int buf n1; buf_int buf n2; buf_int buf n3
      
  | Server_user  (n1,n2) -> buf_int16 buf 12;
      buf_int buf n1; buf_int buf n2
      
  | Server_state (int,host_state) -> buf_int16 buf 13;
      buf_int buf int; buf_host_state buf host_state
      
  | Server_info server_info -> buf_int16 buf 14;
      buf_server_version_0 buf server_info
  
  | Client_info client_info -> buf_int16 buf 15;
      buf_client buf client_info
      
  | Client_state (int, host_state) -> buf_int16 buf 16;
      buf_int buf int; buf_host_state buf host_state
      
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
      buf_room buf room_info
      
  | Room_message (int, room_message) -> buf_int16 buf 23;
      buf_int buf int; buf_message buf room_message
      
  | Room_user (n1,n2) -> buf_int16 buf 24;
      buf_int buf n1; buf_int buf n2

  | _ -> raise UnsupportedGuiMessage
      
let from_gui_version_0 buf t =
  match t with
  | GuiProtocol int -> buf_int16 buf 0;
      buf_int buf int
  
  | ConnectMore_query -> buf_int16 buf 1
  | CleanOldServers -> buf_int16 buf 2
  | KillServer -> buf_int16 buf 3
  | ExtendedSearch -> buf_int16 buf 4
  | Password string -> buf_int16 buf 5;
      buf_string buf string
  | Search_query search -> buf_int16 buf 6;
      buf_bool buf (search.search_type = LocalSearch); 
      buf_search_version_0 buf search
  | Download_query (list, int) -> buf_int16 buf 7;
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
      
  | BrowseUser  int -> buf_int16 buf 41;
      buf_int buf int

(*  | _ -> raise UnsupportedGuiMessage *)
      
let to_gui_version_1 buf t =
  match t with
    Client_stats s ->
      buf_int16 buf 25;
      buf_int64 buf s.upload_counter;
      buf_int64 buf s.download_counter;      
      buf_int64 buf s.shared_counter;
      buf_int buf s.nshared_files
  | _ -> to_gui_version_0 buf t
      
let from_gui_version_1 = from_gui_version_0
      
let to_gui_version_2 buf t =
  match t with
    Server_info s ->
      buf_int16 buf 26;
      buf_server_version_2 buf s
  | _ -> to_gui_version_1 buf t
      
let from_gui_version_2 buf t = 
  match t with
    Search_query s ->
      buf_int16 buf 42;
      buf_search_version_2 buf s
  | _ -> from_gui_version_1 buf t
      
let to_gui = [| 
    to_gui_version_0; 
    to_gui_version_1;
    to_gui_version_2;
    |]
let from_gui = [| 
    from_gui_version_0; 
    from_gui_version_1;
    from_gui_version_2; 
    |]
  
end

module Decoding = struct
    
    
    let get_string s pos = 
      let len = get_int16 s pos in
      String.sub s (pos+2) len, pos+2+len
    
    let get_list f s pos =
      let len = get_int16 s pos in
      let rec iter n pos =
        if n = 0 then [],pos else
        let head, pos = f s pos in
        let tail, pos = iter (n-1) pos in
        head :: tail, pos
      in
      iter len (pos+2)
    
    let get_array f s pos = 
      let list, pos = get_list f s pos in
      Array.of_list list, pos
    
    let get_bool s pos = (get_int8 s pos) = 1
    
    
    let rec get_query s pos =
      let op = get_int8 s pos in
      match op with
      | 0 ->
          let list, pos = get_list get_query s (pos+1) in
          Q_AND list, pos
      | 1 ->
          let list, pos = get_list get_query s (pos+1) in
          Q_OR list, pos
      | 2 ->
          let q1, pos = get_query s (pos+1) in
          let q2, pos = get_query s pos in
          Q_ANDNOT (q1,q2), pos
      | 3 ->
          let s1, pos = get_string s (pos+1) in
          let q, pos = get_query s pos in
          Q_MODULE (s1, q), pos
      
      | 4 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_KEYWORDS (s1,s2), pos
      
      | 5 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MINSIZE (s1,s2), pos
      | 6 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MAXSIZE (s1,s2), pos
      | 7 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_FORMAT (s1,s2), pos
      | 8 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MEDIA (s1,s2), pos
      | 9 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MP3_ARTIST (s1,s2), pos
      | 10 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MP3_TITLE (s1,s2), pos
      | 11 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MP3_ALBUM (s1,s2), pos
      | 12 -> 
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          Q_MP3_BITRATE (s1,s2), pos
      | 13 -> 
          let list, pos = get_list get_query s (pos+1) in
          Q_HIDDEN list, pos
      | _ -> assert false
    
    let get_search_version_0 s pos =
      let num = get_int s pos in
      let q, pos = get_query s (pos+4) in
      let max = get_int s pos in
      { 
        search_num = num; 
        search_query = q;
        search_max_hits = max;
        search_type = RemoteSearch;
      }, (pos+4)

    let get_search_type s pos =
      match get_int8 s pos with
        0 -> LocalSearch
      | 1 -> RemoteSearch
      | 2 -> SubscribeSearch
      | _ -> assert false
      
    let get_search_version_2 s pos =
      let num = get_int s pos in
      let q, pos = get_query s (pos+4) in
      let max = get_int s pos in
      let stype = get_search_type s (pos+4) in
      { 
        search_num = num; 
        search_query = q;
        search_max_hits = max;
        search_type = stype;
      }, (pos+4)
    
    
    let get_mp3 s pos =
      let module M = Mp3tag in
      let title, pos = get_string s pos in
      let artist, pos = get_string s pos in
      let album, pos = get_string s pos in
      let year, pos = get_string s pos in
      let comment, pos = get_string s pos in
      let tracknum = get_int s pos in
      let genre = get_int s (pos+4) in
      {
        M.title = title;
        M.artist = artist;
        M.album = album;
        M.year = year;
        M.comment = comment;
        M.tracknum = tracknum;
        M.genre = genre;
      }, pos + 8
    
    
    let get_format s pos =
      match get_int8 s pos with
      | 0 -> Unknown_format, pos+1
      | 1 ->
          let s1, pos = get_string s (pos+1) in
          let s2, pos = get_string s pos in
          FormatType (s1, s2), pos
      | 2 ->
          let codec, pos = get_string s (pos+1) in
          let width = get_int s pos in
          let height = get_int s (pos+4) in
          let fps = get_int s (pos+8) in
          let rate = get_int s (pos+12) in
          AVI { 
            avi_codec = codec;
            avi_width = width;
            avi_height = height;
            avi_fps = fps;
            avi_rate = rate;
          }, pos+16
      
      | 3 ->
          let t,pos = get_mp3 s (pos+1) in 
          Mp3 t, pos
      | _ -> assert false    
    
    let get_tag s pos =
      let name, pos = get_string s pos in
      let value, pos =
        match get_int8 s pos with
          0 -> 
            Uint32 (get_int32 s (pos+1)), pos+5
        | 1 -> 
            Fint32 (get_int32 s (pos+1)), pos+5
        | 2 -> let s, pos = get_string s (pos+1) in
            String s, pos
        | 3 -> Addr (get_ip s (pos+1)), pos+5
        | _ -> assert false
      in
      { tag_name = name; tag_value = value }, pos
    
    let get_result s pos =
      let num  = get_int s pos in
      let net = get_int s (pos+4) in
      let names, pos = get_list get_string s (pos+8) in
      let md4 = get_md4 s pos in
      let size = get_int32 s (pos+16) in
      let format, pos = get_string s (pos+20) in
      let t, pos = get_string s pos in
      let tags, pos = get_list get_tag s pos in
      let comment, pos = get_string s pos in
      let already_done = get_bool  s pos in
      { 
        result_num = num;
        result_network = net;
        result_names = names;
        result_md4 = md4;
        result_size = size;
        result_format = format;
        result_type = t;
        result_tags = tags;
        result_comment = comment;
        result_done = already_done
      }, pos+1
    
    let get_message s pos =
      match get_int8 s pos with
        0 ->
          let s, pos = get_string s (pos+1) in
          ServerMessage s, pos
      | 1 ->
          let n = get_int s (pos+1) in
          let s, pos = get_string s (pos+5) in
          PublicMessage (n, s), pos
      | 2 ->
          let n = get_int s (pos+1) in
          let s, pos = get_string s (pos+5) in
          PrivateMessage (n,s), pos
      | _ -> assert false
    
    let get_file_state s pos =
      match get_int8 s pos with
      | 0 -> FileDownloading
      | 1 -> FilePaused
      | 2 -> FileDownloaded
      | 3 -> FileShared    
      | 4 -> FileCancelled
      | 5 -> FileNew
      | _ -> assert false
    
    let get_float s pos = 
      let s, pos = get_string s pos in
      float_of_string s, pos
    
    let get_file s pos = 
      let num = get_int s pos in
      let net = get_int s (pos+4) in
      let names, pos = get_list get_string s (pos+8) in
      let md4 = get_md4 s pos in
      let size = get_int32 s (pos+16) in
      let downloaded = get_int32 s (pos+20) in
      let nlocations = get_int s (pos+24) in
      let nclients = get_int s (pos+28) in
      let state = get_file_state s (pos+32) in
      let chunks, pos = get_string s (pos+33) in
      let availability, pos = get_string s pos in
      let rate, pos = get_float s pos in
      let chunks_age, pos = get_array get_float s pos in
      let age, pos = get_float s pos in
      let format, pos = get_format s pos in
      {
        file_num = num;
        file_network = net;
        file_names = names;
        file_md4 = md4;
        file_size = size;
        file_downloaded = downloaded;
        file_nlocations = nlocations;
        file_nclients = nclients;
        file_state = state;
        file_chunks = chunks;
        file_availability = availability;
        file_download_rate = rate;
        file_chunks_age = chunks_age;
        file_age = age;
        file_format = format;
        file_sources = None;
      }, pos
    
    let get_host_state s pos = 
      match get_int8 s pos with
        0 -> NotConnected
      | 1 -> Connecting
      | 2 -> Connected_initiating
      | 3 -> Connected_busy
      | 4 -> Connected_idle
      | 5 -> Connected_queued
      | 6 -> NewHost
      | 7 -> RemovedHost
      | _ -> assert false
    
    
    let get_server_version_0 s pos =
      let num = get_int s pos in
      let net = get_int s (pos+4) in
      let ip = get_ip s (pos+8) in
      let port = get_int16 s (pos+12) in
      let score = get_int s (pos+14) in
      let tags, pos = get_list get_tag s (pos+18) in
      let nusers = get_int s pos in
      let nfiles = get_int s (pos+4) in
      let state = get_host_state s (pos+8) in
      let name, pos = get_string s (pos+9) in
      let description, pos = get_string s pos in
      {
        server_num = num;
        server_network = net;
        server_addr = new_addr_ip ip;
        server_port = port;
        server_score = score;
        server_tags = tags;
        server_nusers = nusers;
        server_nfiles = nfiles;
        server_state = state;
        server_name = name;
        server_description = description;
        server_users = None;
      }, pos

    let get_addr s pos =
      match get_int8 s pos with
        0 ->
          let ip = get_ip s (pos+1) in
          new_addr_ip ip, pos+5
      | 1 ->
          let name,pos = get_string s (pos+1) in
          new_addr_name name, pos
      | _ -> assert false
          
    let get_server_version_2 s pos =
      let num = get_int s pos in
      let net = get_int s (pos+4) in
      let addr,pos = get_addr s (pos+8) in
      let port = get_int16 s pos in
      let score = get_int s (pos+2) in
      let tags, pos = get_list get_tag s (pos+6) in
      let nusers = get_int s pos in
      let nfiles = get_int s (pos+4) in
      let state = get_host_state s (pos+8) in
      let name, pos = get_string s (pos+9) in
      let description, pos = get_string s pos in
      {
        server_num = num;
        server_network = net;
        server_addr = addr;
        server_port = port;
        server_score = score;
        server_tags = tags;
        server_nusers = nusers;
        server_nfiles = nfiles;
        server_state = state;
        server_name = name;
        server_description = description;
        server_users = None;
      }, pos
    
    
    let get_client_type s pos = 
      match get_int8 s pos with
        0 -> NormalClient
      | 1 -> FriendClient
      | 2 -> ContactClient
      | _ -> assert false

    let get_kind s pos =
      match get_int8 s pos with
        0 ->
          let ip = get_ip s (pos+1) in
          let port = get_int16 s (pos+5) in
          Known_location (ip, port), pos+7
      | 1 ->
          let name, pos = get_string s (pos+1) in
          let md4 = get_md4 s pos in
          Indirect_location (name, md4), pos+16
      | _ -> assert false
          
    let get_client s pos =
      let num = get_int s pos in
      let net = get_int s (pos+4) in
      let kind, pos = get_kind s (pos+8) in
      let state = get_host_state s pos in
      let t = get_client_type s (pos+1) in
      let tags, pos = get_list get_tag s (pos+2) in
      let name, pos = get_string s pos in
      let rating = get_int32 s pos in
      let chat_port = get_int s (pos+4) in
      {
        client_num = num;
        client_network = net;
        client_kind = kind;
        client_state = state;
        client_type = t;
        client_tags = tags;
        client_name = name;
        client_rating = rating;
        client_chat_port = chat_port;
        client_files = None;
      }, pos+8

    let get_network s pos =
      let num = get_int s pos in
      let name, pos = get_string s (pos+4) in
      let enabled = get_bool s pos in
      let config_file, pos = get_string s (pos+1) in
      let uploaded = get_int64 s pos in
      let downloaded = get_int64 s (pos+8) in
      { network_netnum = num;
        network_netname = name;
        network_enabled = enabled;
        network_config_filename = config_file;
        network_uploaded = uploaded;
        network_downloaded = downloaded;
      }, pos+16

      
    let get_user s pos = 
      let num = get_int s pos in
      let md4 = get_md4 s (pos+4) in
      let name, pos = get_string s (pos+20) in
      let ip = get_ip s pos in
      let port = get_int16 s (pos+4) in
      let tags, pos = get_list get_tag s (pos+6) in
      let server = get_int s pos in
      {
        user_num = num;
        user_md4 = md4;
        user_name = name;
        user_ip = ip;
        user_port = port;
        user_tags = tags;
        user_server = server;
      }, pos + 4
      
      
    let get_room_state s pos =
      match get_int8 s pos with
        0 -> RoomOpened
      | 1 -> RoomClosed
      | 2 -> RoomPaused
      | _ -> assert false
          
    let get_room s pos =
      let num = get_int s pos in
      let net = get_int s (pos+4) in
      let name, pos = get_string s (pos+8) in
      let state = get_room_state s pos in
      {
        room_num = num;
        room_network = net;
        room_name = name;
        room_state = state;
        room_users = [];
      }, pos + 1 
      
    let from_gui_version_0 opcode s =
      match opcode with
        0 -> GuiProtocol (get_int s 2)
      
      | 1 -> ConnectMore_query
      | 2 -> CleanOldServers
      | 3 -> KillServer
      | 4 -> ExtendedSearch
      | 5 -> let pass,_ = get_string s 2 in Password pass
      | 6 -> 
          let local = get_bool s 2 in
          let search, pos = get_search_version_0 s 3 in
          search.search_type <- if local then LocalSearch else RemoteSearch;
          Search_query search
      | 7 -> 
          let list, pos = get_list get_string s 2 in
          let result_num = get_int s pos in
          Download_query (list, result_num)

      | 8 -> let string, pos = get_string s 2 in
          Url string
      | 9 -> let int = get_int s 2 in RemoveServer_query int
      | 10 ->
          let list, pos = get_list (fun s pos ->
                let s1, pos = get_string s pos in
                let s2, pos = get_string s pos in
                (s1,s2), pos) s 2 in
          SaveOptions_query list
          
      | 11 ->
          let int = get_int s 2 in 
          RemoveDownload_query  int

      | 12 -> 
          let int = get_int s 2 in 
          ServerUsers_query  int
          
      | 13 ->
          let int = get_int s 2 in 
          let s, pos = get_string s 6 in
          SaveFile (int, s)
          
      | 14 ->
          let int = get_int s 2 in 
          AddClientFriend  int
          
      | 15 ->          
          let int = get_int s 2 in 
          AddUserFriend  int
          
      | 16 ->
          let int = get_int s 2 in 
          RemoveFriend  int
          
      | 17 -> RemoveAllFriends
          
      | 18 -> 
          let string, pos = get_string s 2 in
          FindFriend string
          
      | 19 -> 
          let int = get_int s 2 in 
          ViewUsers  int
          
      | 20 -> 
          let int = get_int s 2 in 
          ConnectAll  int
          
      | 21 ->
          let int = get_int s 2 in 
          ConnectServer  int

      | 22 -> 
          let int = get_int s 2 in 
          DisconnectServer  int
          
      | 23 ->
          let int = get_int s 2 in 
          let bool = get_bool s 6 in
          SwitchDownload  (int, bool) 
          
      | 24 ->
          let int = get_int s 2 in 
          VerifyAllChunks  int
          
      | 25 ->
          let int = get_int s 2 in 
          QueryFormat  int
          
      | 26 ->
          let int = get_int s 2 in
          let tag, pos = get_mp3 s 6 in
          ModifyMp3Tags (int, tag)

      | 27 ->
          let int = get_int s 2 in 
          ForgetSearch  int
          
      | 28 ->
          let s1, pos = get_string s 2 in
          let s2, pos = get_string s pos in
          SetOption (s1, s2)
          
      | 29 ->
          let s1, pos = get_string s 2 in
          Command s1
          
      | 30 ->
          let int = get_int s 2 in 
          Preview  int
          
      | 31 ->
          let int = get_int s 2 in 
          ConnectFriend  int
          
      | 32 ->
          let int = get_int s 2 in 
          GetServer_users  int
          
      | 33 ->
          let int = get_int s 2 in 
          GetClient_files  int
          
      | 34 ->
          let int = get_int s 2 in 
          GetFile_locations  int
          
      | 35 ->
          let int = get_int s 2 in 
          GetServer_info  int
          
      | 36 ->
          let int = get_int s 2 in 
          GetClient_info  int
          
      | 37 ->
          let int = get_int s 2 in 
          GetFile_info  int
          
      | 38 ->
          let int = get_int s 2 in 
          GetUser_info  int
          
      | 39 ->
          let int = get_int s 2 in 
          let room_message, pos = get_message s 6 in
          SendMessage (int, room_message)

      | 40 ->
          let int = get_int s 2 in 
          let bool = get_bool s 6 in 
          EnableNetwork (int, bool) 
      
      | 41 ->
          let int = get_int s 2 in 
          BrowseUser  int

      | _ -> 
          Printf.printf "FROM GUI:Unknown message %d" opcode; print_newline ();
          assert false

    let to_gui_version_0 opcode s =
      match opcode with
      | 0 -> CoreProtocol (get_int s 2)
  
      | 1 ->
          let list, pos = get_list (fun s pos ->
                let name, pos = get_string s pos in
                let value, pos = get_string s pos in
                (name, value), pos
            ) s 2 in
          Options_info list
          
      | 3 ->
          let list, pos = get_list (fun s pos ->
                let name, pos = get_string s pos in
                let q, pos = get_query s pos in
                (name, q), pos) s 2 in
          DefineSearches list
          
      | 4 -> 
          let r, pos = get_result s 2 in
          Result_info r

      | 5 ->
          let n1 = get_int s 2 in
          let n2 = get_int s 6 in
          Search_result (n1,n2)
          
      | 6 -> 
          let n1 = get_int s 2 in
          let n2 = get_int s 6 in
          Search_waiting (n1,n2)
  
      | 7 -> 
          let file_info, pos = get_file s 2 in
          File_info file_info
          
      | 8 ->
          let n = get_int s 2 in
          let size = get_int32 s 6 in
          let rate, pos = get_float s 10 in
          File_downloaded (n, size, rate)
          
          
      | 9 ->
          let n = get_int s 2 in
          let s1, pos = get_string s 6 in
          let s2, pos = get_string s pos in
          File_availability (n, s1, s2)
          
      | 10 -> 
          let n1 = get_int s 2 in
          let n2 = get_int s 6 in
          File_source (n1,n2)
  
      | 11 ->
          let n1 = get_int s 2 in
          let n2 = get_int s 6 in
          let n3 = get_int s 10 in
          Server_busy (n1,n2,n3)
          
      | 12 -> 
          let n1 = get_int s 2 in
          let n2 = get_int s 6 in
          Server_user  (n1,n2)
      
      | 13 -> 
          let int = get_int s 2 in
          let host_state = get_host_state s 6 in
          Server_state (int,host_state)
      
      | 14 ->
          let server_info, pos = get_server_version_0 s 2 in
          Server_info server_info
          
      | 15 -> 
          let client_info, pos = get_client s 2 in
          Client_info client_info
      
      | 16 -> 
          let int = get_int s 2 in
          let host_state = get_host_state s 6 in
          Client_state (int, host_state)
      
      | 17 ->
          let int = get_int s 2 in
          let client_type = get_client_type s 6 in
          Client_friend (int, client_type)
      
      | 18 ->
          let n1 = get_int s 2 in
          let s1, pos = get_string s 6 in
          let n2 = get_int s pos in          
          Client_file (n1, s1, n2)
      
      | 19 -> 
          let string, pos = get_string s 2 in
          Console string
  
      | 20 -> 
          let network_info, pos = get_network s 2 in
          Network_info network_info
      
      | 21 ->
          let user_info, pos = get_user s 2 in
          User_info user_info
  
      | 22 ->
          let room_info, pos = get_room s 2 in
          Room_info room_info
      
      | 23 ->
          let int = get_int s 2 in
          let room_message, pos = get_message s 6 in
          Room_message (int, room_message)
      
      | 24 ->
          let n1 = get_int s 2 in
          let n2 = get_int s 6 in          
          Room_user (n1,n2)

      | _ -> 
          Printf.printf "TO GUI:Unknown message %d" opcode; print_newline ();
          assert false

    let from_gui_version_1 = from_gui_version_0
    let to_gui_version_1 opcode s = 
      match opcode with
        25 ->
          let upload = get_int64 s 2 in
          let download = get_int64 s 10 in
          let shared = get_int64 s 18 in
          let nshared = get_int s 26 in
          Client_stats {
            upload_counter = upload;
            download_counter = download;
            shared_counter = shared;
            nshared_files = nshared;
          }
      | _ -> to_gui_version_0 opcode s
      
    let from_gui_version_2 opcode s =
      match opcode with
        42 -> let s, pos = get_search_version_2 s 2 in Search_query s
      | _ -> from_gui_version_1 opcode s
      
    let to_gui_version_2 opcode s = 
      match opcode with
        26 -> let s, pos = get_server_version_2 s 2 in Server_info s
      | _ -> to_gui_version_1 opcode s
          
    let to_gui = [| 
        to_gui_version_0; 
        to_gui_version_1; 
        to_gui_version_2; 
      |]
      
    let from_gui = [| 
        from_gui_version_0; 
        from_gui_version_1; 
        from_gui_version_2; 
        |]
  
  end

let _ =
  assert (Array.length Encoding.to_gui = Array.length Encoding.from_gui);
  assert (Array.length Encoding.to_gui = Array.length Decoding.to_gui);
  assert (Array.length Encoding.from_gui = Array.length Decoding.from_gui)

let best_gui_version = Array.length Encoding.from_gui - 1
  
(*
    
    let gui_handler gui f =
      cut_messages (fun s ->
          let opcode = get_int16 s 0 in
          f gui (from_gui.(gui.gui_version) opcode s)
      )

    
let gui_send gui t =
  TcpBufferedSocket.value_send gui.gui_sock (t : to_gui)

*)
  