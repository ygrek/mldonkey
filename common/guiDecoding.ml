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
open GuiTypes
open GuiProto
open LittleEndian
open TcpBufferedSocket

(*
gui_cut_messages is a reader for TcpBufferedSocket.t that will cut the stream
  in GUI messages, and call f on each message.
*)
  
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


(***************

      Decoding of basic data types

****************)

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

let get_room_version_0 s pos =
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
    room_messages = [];
    room_nusers = 0;
  }, pos + 1 

let get_room_version_3 s pos =
  let num = get_int s pos in
  let net = get_int s (pos+4) in
  let name, pos = get_string s (pos+8) in
  let state = get_room_state s pos in
  let nusers = get_int s (pos+1) in
  {
    room_num = num;
    room_network = net;
    room_name = name;
    room_state = state;
    room_users = [];
    room_messages = [];
    room_nusers = nusers;
  }, pos + 5 

let get_shared_info s pos =
  let num = get_int s pos in
  let network = get_int s (pos+4) in
  let name, pos = get_string s (pos+8) in
  let size = get_int32 s pos in
  let uploaded = get_int64 s (pos+4) in
  let requests = get_int s (pos+12) in
  {
    shared_num = num;
    shared_network = network;
    shared_filename = name;
    shared_size = size;
    shared_uploaded = uploaded;
    shared_requests = requests;
  }
  
(***************

     Decoding of messages from the GUI to the Core 

****************)

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
      
      let msg = SendMessage (int, room_message) in
  
      begin
        match msg with
(* Change private message from former GUIs to MessageToClient ! *)
          SendMessage ((-1 | 0), PrivateMessage(num,s)) ->
            MessageToClient (num,s)
        | _ -> msg
      end
      
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

      let from_gui_version_1 = from_gui_version_0

let from_gui_version_2 opcode s =
  match opcode with
    42 -> let s, pos = get_search_version_2 s 2 in Search_query s
  | _ -> from_gui_version_1 opcode s

let from_gui_version_3 opcode s =
  match opcode with
    43 -> 
      let int = get_int s 2 in 
      let message, pos = get_string s 6 in
      MessageToClient (int, message)
  | 44 -> GetConnectedServers
  | 45 -> GetDownloadFiles
  | 46 -> GetDownloadedFiles
  | 47 -> 
      let list, pos = get_list (fun s pos -> 
            (get_int s pos, 1 = get_int8 s (pos+4)), pos+5) s 2 in
      GuiExtensions list
  | 48 ->
      SetRoomState (get_int s 2, get_room_state s 6)
  | _ -> from_gui_version_2 opcode s

let from_gui_version_4 opcode s = 
  match opcode with
  | 49 -> RefreshUploadStats
  | _ ->  from_gui_version_3 opcode s
      
let from_gui_version_5 opcode s = 
  match opcode with
  | _ ->  from_gui_version_4 opcode s
      
let from_gui_version_6 opcode s = 
  match opcode with
  | _ ->  from_gui_version_5 opcode s
      
let from_gui = [| 
    from_gui_version_0; 
    from_gui_version_1; 
    from_gui_version_2; 
    from_gui_version_3; 
    from_gui_version_4; 
    from_gui_version_5; 
    from_gui_version_6; 
  |]
      
(***************

     Decoding of messages from the Core to the GUI 

****************)

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
      let room_info, pos = get_room_version_0 s 2 in
      Room_info room_info
  
  | 23 ->
      let int = get_int s 2 in
      let room_message, pos = get_message s 6 in
      Room_message (int, room_message) 
      
  | 24 ->
      let n1 = get_int s 2 in
      let n2 = get_int s 6 in          
      Room_add_user (n1,n2)
  
  | _ -> 
      Printf.printf "TO GUI:Unknown message %d" opcode; print_newline ();
      assert false

let to_gui_version_1 opcode s = 
  match opcode with
  | 25 ->
      let upload = get_int64 s 2 in
      let download = get_int64 s 10 in
      let shared = get_int64 s 18 in
      let nshared = get_int s 26 in
      Client_stats {
        upload_counter = upload;
        download_counter = download;
        shared_counter = shared;
        nshared_files = nshared;
        tcp_upload_rate = 0;
        tcp_download_rate = 0;
        udp_upload_rate = 0;
        udp_download_rate = 0;
      }
  | _ -> to_gui_version_0 opcode s

let to_gui_version_2 opcode s = 
  match opcode with
    26 -> let s, pos = get_server_version_2 s 2 in Server_info s
  | _ -> to_gui_version_1 opcode s

let to_gui_version_3 opcode s = 
  match opcode with
    27 -> 
      let int = get_int s 2 in 
      let message, pos = get_string s 6 in
      MessageFromClient (int, message)
      
  | 28 -> 
      let list, pos = get_list get_server_version_2 s 2 in
      ConnectedServers list
      
  | 29 ->
      let list, pos = get_list get_file s 2 in
      DownloadFiles list
      
  | 30 ->
      let list, pos = get_list get_file s 2 in
      DownloadedFiles list
  
  | 31 ->
      let room_info, pos = get_room_version_3 s 2 in
      Room_info room_info

  | _ -> to_gui_version_2 opcode s

let to_gui_version_4 opcode s = 
  match opcode with
  | 32 -> 
      let room = get_int s 2 in 
      let user = get_int s 6 in
      Room_remove_user (room, user)
  | 33 ->
      let s = get_shared_info s 2 in
      Shared_file_info s
  | 34 ->
      let num = get_int s 2 in
      let upload = get_int64 s 6 in
      let requests = get_int s 14 in
      Shared_file_upload (num, upload, requests)
      
  | 35 ->
      let num = get_int s 2 in
      Shared_file_unshared num
      
  | _ -> to_gui_version_3 opcode s

let to_gui_version_5 opcode s = 
  match opcode with
  | 36 -> 
      let section, pos = get_string s 2 in
      let message, pos = get_string s pos in 
      let option, pos = get_string s pos in
      let optype = 
        match get_int8 s pos with
          0 -> StringEntry 
        | 1 -> BoolEntry 
        | 2 -> FileEntry
        | _ -> assert false in
      Add_section_option (section, message, option, optype)
  
  | 37 ->
      let upload = get_int64 s 2 in
      let download = get_int64 s 10 in
      let shared = get_int64 s 18 in
      let nshared = get_int s 26 in
      let tcp_upload_rate = get_int s 30 in
      let tcp_download_rate = get_int s 34 in
      
      Client_stats {
        upload_counter = upload;
        download_counter = download;
        shared_counter = shared;
        nshared_files = nshared;
        udp_upload_rate = 0;
        udp_download_rate = 0;
        tcp_upload_rate = tcp_upload_rate;
        tcp_download_rate = tcp_download_rate;
      }
      
  | _ -> to_gui_version_4 opcode s

let to_gui_version_6 opcode s = 
  match opcode with
  | 38 -> 
      let section, pos = get_string s 2 in
      let message, pos = get_string s pos in 
      let option, pos = get_string s pos in
      let optype = 
        match get_int8 s pos with
          0 -> StringEntry 
        | 1 -> BoolEntry 
        | 2 -> FileEntry
        | _ -> assert false in
      Add_plugin_option (section, message, option, optype)
  
  | 39 ->
      let upload = get_int64 s 2 in
      let download = get_int64 s 10 in
      let shared = get_int64 s 18 in
      let nshared = get_int s 26 in
      let tcp_upload_rate = get_int s 30 in
      let tcp_download_rate = get_int s 34 in
      let udp_upload_rate = get_int s 38 in
      let udp_download_rate = get_int s 42 in
      
      Client_stats {
        upload_counter = upload;
        download_counter = download;
        shared_counter = shared;
        nshared_files = nshared;
        udp_upload_rate = udp_upload_rate;
        udp_download_rate = udp_download_rate;
        tcp_upload_rate = tcp_upload_rate;
        tcp_download_rate = tcp_download_rate;
      }
        
  | _ -> to_gui_version_5 opcode s
      
let to_gui = [| 
    to_gui_version_0; 
    to_gui_version_1; 
    to_gui_version_2; 
    to_gui_version_3;
    to_gui_version_4;
    to_gui_version_5;
    to_gui_version_6;
  |]

let _ =  assert (Array.length from_gui = Array.length to_gui);
  