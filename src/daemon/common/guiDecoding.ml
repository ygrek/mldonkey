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

(* TODO INTERESTED *)

open BasicSocket
open Int64ops
open Printf2
  
open CommonGlobals
open CommonTypes
open GuiTypes
open GuiProto
open AnyEndian
open LittleEndian
open TcpBufferedSocket

exception FromGuiMessageNotImplemented

(*
gui_cut_messages is a reader for TcpBufferedSocket.t that will cut the stream
  in GUI messages, and call f on each message.
*)
  
let gui_cut_messages f sock nread =
  let b = buf sock in
  try
    while b.len >= 4 do
      let msg_len = get_int_bytes b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let s = Bytes.sub b.buf (b.pos+4) msg_len in
          buf_used b (msg_len + 4);
          let opcode = get_int16_bytes s 0 in
          (f opcode s : unit)
        end
      else raise Not_found
    done
  with Not_found -> ()


(***************

      Decoding of basic data types

****************)
let get_int64_28 proto s pos =
  if proto > 27 then
    get_int64 s pos, pos+8
  else
    Int64.of_int (get_int s pos), pos+4
      
let get_string s pos = 
  let len = get_int16 s pos in
  if len land 0xffff = 0xffff then
    let len = get_int s (pos+2) in
    (String.sub s (pos+6) len), pos+6+len
  else
    (String.sub s (pos+2) len), pos+2+len

let get_string_bin s pos =
  get_string s pos
    
let get_ip2 proto s pos =
  let ip,pos = get_ip s pos,(pos+4) in 
  if proto > 37 then
    let _ = get_uint8 s pos in 
    ip,(pos+1)
  else
   ip,pos

let get_hostname proto s pos =
  let hn,pos = get_string s pos in
  if proto > 37 then
    let _ = get_uint8 s pos in
    hn,(pos+1)
  else
    hn,pos
    
let get_list f s pos =
  let len = get_int16 s pos in
  let rec iter n pos =
    if n = 0 then [],pos else
    let head, pos = f s pos in
    let tail, pos = iter (n-1) pos in
    head :: tail, pos
  in
  iter len (pos+2)

let get_list2 proto f s pos =
  let len = get_int16 s pos in
  let rec iter n pos =
    if n = 0 then [],pos else
    let head, pos = f proto s pos in
    let tail, pos = iter (n-1) pos in
    head :: tail, pos
  in
  iter len (pos+2)

let get_array f s pos = 
  let list, pos = get_list f s pos in
  Array.of_list list, pos

let get_bool s pos = (get_uint8 s pos) = 1
let get_bool_option s pos = 
  let i = get_uint8 s pos in
  if i = 2 then None else Some (i = 1)

let get_float s pos = 
  let s, pos = get_string s pos in
  float_of_string s, pos

let get_uid s pos =
  let uid, pos = get_string s pos in
  Uid.of_string uid, pos

let rec get_query s pos =
  let op = get_uint8 s pos in
  match op with
  | 0 ->
      let list, pos = get_list get_query s (pos+1) in
      Q_AND list, pos
  | 1 ->
      let list, pos = get_list get_query s (pos+1) in
      Q_OR list, pos
  | 2 ->
      let q1, pos = get_query s (pos+1) in
      let q2, pos = get_query  s pos in
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

let get_search_type s pos =
  match get_uint8 s pos with
    0 -> LocalSearch
  | 1 -> RemoteSearch
  | 2 -> SubscribeSearch
  | _ -> assert false

let get_search get_query proto s pos =
  let num = get_int s pos in
  let q, pos = get_query s (pos+4) in
  let max = get_int s pos in
  let stype, pos = 
    if proto >= 2 then
      get_search_type s (pos+4), pos+5
    else
      RemoteSearch, pos+4
  in
  let net, pos =
    if proto >= 16 then
      get_int s pos, pos+4
    else 0, pos
  in
  { 
    search_num = num; 
    search_query = q;
    search_max_hits = max;
    search_type = stype;
    search_network = net;
  }, pos

let get_mp3 s pos =
  let module M = Mp3tag.Id3v1 in
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

let get_ogg_stream_type s pos =
  match get_uint8 s pos with
      0 -> OGG_VIDEO_STREAM, pos+1
    | 1 -> OGG_AUDIO_STREAM, pos+1
    | 2 -> OGG_TEXT_STREAM, pos+1
    | 3 -> OGG_INDEX_STREAM, pos+1
    | 4 -> OGG_VORBIS_STREAM, pos+1
    | 5 -> OGG_THEORA_STREAM, pos+1
    | _ -> assert false

let get_vorbis_bitrate s pos =
  match get_uint8 s pos with
      0 ->
        let r, pos = get_float s (pos+1) in
        Maximum_br r, pos
    | 1 ->
        let r, pos = get_float s (pos+1) in
        Nominal_br r, pos
    | 2 ->
        let r, pos = get_float s (pos+1) in
        Minimum_br r, pos
    | _ -> assert false

let get_theora_cs s pos =
  match get_uint8 s pos with
      0 -> CSUndefined
    | 1 -> CSRec470M
    | 2 -> CSRec470BG
    | _ -> assert false

let get_ogg_stream_tag s pos =
  match get_uint8 s pos with
      0 ->
        let codec, pos = get_string s (pos+1) in
        Ogg_codec codec, pos
    | 1 ->
        let n = get_int s (pos+1) in
        Ogg_bits_per_samples n, (pos+5)
    | 2 ->
        let n = get_int s (pos+1) in
        Ogg_duration n, (pos+5)
    | 3 ->
        Ogg_has_subtitle, (pos+1)
    | 4 ->
        Ogg_has_index, (pos+1)
    | 5 ->
        let n = get_int s (pos+1) in
        Ogg_audio_channels n, (pos+5)
    | 6 ->
        let r, pos = get_float s (pos+1) in
        Ogg_audio_sample_rate r, pos
    | 7 ->
        let n = get_int s (pos+1) in
        Ogg_audio_blockalign n, (pos+5)
    | 8 ->
        let r, pos = get_float s (pos+1) in
        Ogg_audio_avgbytespersec r, pos
    | 9 ->
        let r, pos = get_float s (pos+1) in
        Ogg_vorbis_version r, pos
    | 10 ->
        let r, pos = get_float s (pos+1) in
        Ogg_vorbis_sample_rate r, pos
    | 11 ->
        let l, pos = get_list get_vorbis_bitrate s (pos+1) in
        Ogg_vorbis_bitrates l, pos
    | 12 ->
        let n = get_int s (pos+1) in
        Ogg_vorbis_blocksize_0 n, (pos+5)
    | 13 ->
        let n = get_int s (pos+1) in
        Ogg_vorbis_blocksize_1 n, (pos+5)
    | 14 ->
        let r, pos = get_float s (pos+1) in
        Ogg_video_width r, pos
    | 15 ->
        let r, pos = get_float s (pos+1) in
        Ogg_video_height r, pos
    | 16 ->
        let r, pos = get_float s (pos+1) in
        Ogg_video_sample_rate r, pos
    | 17 ->
        let r, pos = get_float s (pos+1) in
        Ogg_aspect_ratio r, pos
    | 18 ->
        let cs = get_theora_cs s (pos+1) in
        Ogg_theora_cs cs, (pos+2)
    | 19 ->
        let n = get_int s (pos+1) in
        Ogg_theora_quality n, (pos+5)
    | 20 ->
        let n = get_int s (pos+1) in
        Ogg_theora_avgbytespersec n, (pos+5)
    | _ -> assert false

let get_ogg_info s pos =
  let sno = get_int s pos in
  let stype, pos = get_ogg_stream_type s (pos+4) in
  let stags, pos = get_list get_ogg_stream_tag s pos in
  {
   stream_no   = sno;
   stream_type = stype;
   stream_tags = stags;
  }, pos

let dummy_info =
  let module M = Mp3tag in {
    M.duration = 0;
    M.samplerate = 0;
    M.mode = M.Stereo;
    M.bitrate = 0;
    M.encoding = M.CBR;
    M.filesize = 0;
  }

let get_format s pos =
  match get_uint8 s pos with
  | 0 -> FormatUnknown, pos+1
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
      MP3 (t, dummy_info), pos
  | 4 ->
      let ogg_infos, pos = get_list get_ogg_info s (pos+1) in
      (OGG ogg_infos), pos
  | _ -> assert false    

let get_uint64_2 proto s pos = 
  let i, pos =
    if proto > 24 then
      get_int64 s pos, pos + 8
    else
      get_uint64_32 s pos, pos + 4
  in i, pos

let get_tag_name s pos =
  let name, pos = get_string s pos in
  let tag_name = field_of_string name in
  tag_name, pos
  
let get_tag proto s pos =
  let tag_name, pos = get_tag_name s pos in
  let value, pos =
    match get_uint8 s pos with
      0 -> 
        Uint64 (get_uint64_32 s (pos+1)), pos+5
    | 1 -> 
        Fint64 (get_uint64_32 s (pos+1)), pos+5
    | 2 -> let s, pos = get_string s (pos+1) in
        String s, pos
    | 3 -> let ip, pos = get_ip2 proto s (pos+1) in
        Addr ip, pos
    | 4 -> Uint16 (get_int16 s (pos+1)), pos+3
    | 5 -> Uint8 (get_uint8 s (pos+1)), pos+2
    | 6 -> Pair (get_uint64_32 s (pos+1), get_uint64_32 s (pos+5)), pos+9
    | _ -> assert false
  in
  { tag_name = tag_name; tag_value = value }, pos

let get_result proto s pos =
  let num  = get_int s pos in
(*  let net = get_int s (pos+4) in *)
  let names, pos = get_list get_string s (pos+8) in
  let uids, pos = 
    if proto < 27 then
      [], pos+ 16
    else
      get_list get_uid s pos
  in
  let size, pos = get_uint64_2 proto s pos in
  let format, pos = get_string s pos in
  let t, pos = get_string s pos in
  let tags, pos = get_list2 proto get_tag s pos in
  let comment, pos = get_string s pos in
  let already_done = get_bool  s pos in
  let time, pos = 
    if proto < 27 then
      0, pos+1
    else
    let date = get_int s pos in
    last_time () - date, pos + 4
  in    
  { 
    result_num = num;
    result_names = names;
    result_uids = uids;
    result_size = size;
    result_format = format;
    result_type = t;
    result_tags = tags;
    result_comment = comment;
    result_done = already_done;
    result_modified = false;
    result_force = false;
    result_time = time;
    result_source_network = 0;
  }, pos+1

let get_message s pos =
  match get_uint8 s pos with
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
  match get_uint8 s pos with
  | 0 -> FileDownloading, pos+1
  | 1 -> FilePaused, pos+1
  | 2 -> FileDownloaded, pos+1
  | 3 -> FileShared, pos+1
  | 4 -> FileCancelled, pos+1
  | 5 -> FileNew, pos+1
  | 6 -> let s, pos = get_string s (pos+1) in FileAborted s, pos
  | 7 -> FileQueued, pos+1
  | _ -> assert false

let get_float_date proto s pos = 
  let date, pos =
    if proto > 23 then
      let date = get_int s pos in
      last_time () - date, pos + 4
    else
    let s, pos = get_string s pos in
    BasicSocket.normalize_time (int_of_float (float_of_string s)), pos
  in
(*  lprintf "get_float_date(%d): %d\n" proto date; *)
  date, pos
  
let get_int_date proto s pos = 
  if proto > 23 then
    let date = get_int s pos in
    last_time () - date, pos + 4
  else
  get_int s pos, pos + 4


let get_int_pos s pos =
  get_int s pos, pos + 4
  
let get_sub_files proto s pos =
  get_list (fun s pos ->
    let name, pos = get_string s pos in
    let size, pos = get_int64 s pos, pos+8 in
    let magic, pos = 
      if proto > 40 then
        get_string s pos
      else
        "", pos
    in
    (name, size, Some magic), pos
  ) s pos
  
let get_file_comments proto s pos = 
  get_list (fun s pos ->
     let ip, pos = get_ip2 proto s pos in
     let name, pos = get_string s pos in
     let rating, pos = get_uint8 s pos, pos+1 in
     let comment, pos = get_string s pos in
     (ip, name, rating, comment), pos
  ) s pos

  
let get_file proto s pos = 
  let num = get_int s pos in
  let net = get_int s (pos+4) in
  let names, pos = 
    get_list get_string s (pos+8) in
  let md4 = get_md4 s pos in
  let size,pos = get_uint64_2 proto s (pos+16) in
  let downloaded, pos = get_uint64_2 proto s pos in
  let file_all_sources = get_int s pos in
  let file_active_sources = get_int s (pos+4) in
  let state, pos = get_file_state s (pos+8) in
  let chunks, pos = get_string s pos in
  let availability, pos = 
    if proto > 17 then
      get_list (fun s pos ->
          let net = get_int s pos in
          let avail, pos = get_string_bin s (pos+4) in
          (net, avail), pos
      ) s pos
    else
    let avail, pos = get_string_bin s pos in
    [net, avail], pos in
  let rate, pos = get_float s pos in
  let chunks_age, pos = get_array (get_float_date proto) s pos in
  let age, pos = get_float_date proto s pos in
  let format, pos = get_format s pos in
  let name, pos = if proto >= 8 then
      get_string s pos else List.hd names, pos in
  let last_seen, pos = if proto >= 9 then 
      get_int s pos, pos+4 else BasicSocket.last_time (), pos in
  let priority, pos = if proto >= 12 then
      get_int s pos, pos+4 else 0, pos in
  let comment, pos = if proto > 21 then
      get_string s pos
    else "", pos in
  let last_seen = BasicSocket.last_time () - last_seen in
  let uids, pos = 
    if proto < 31 then
      [], pos
    else
      get_list get_uid s pos
  in
  let sub_files, pos =
    if proto > 35 then
      get_sub_files proto s pos
    else [], pos
  in
  let magic, pos = 
    if proto > 40 then
      let ms, pos = get_string s pos
      in Some ms, pos 
    else
      Some "", pos
  in
  let comments, pos = 
    if proto > 40 then
      get_file_comments proto s pos
    else [], pos
  in
  let user, pos = 
    if proto > 40 
      then get_string s pos
      else "", pos
  in
  let group, pos = 
    if proto > 40 
      then get_string s pos
      else "", pos
  in
  (*
  assert (num = file_info_test.file_num);
  assert (net = file_info_test.file_network);
  assert (names = file_info_test.file_names);
  assert (md4 = file_info_test.file_md4);
  assert (size = file_info_test.file_size);
  assert (downloaded = file_info_test.file_downloaded);
  assert (nlocations = file_info_test.file_nlocations);
  assert (nclients = file_info_test.file_nclients);
  assert (state = file_info_test.file_state);
  assert (chunks = file_info_test.file_chunks);
  assert (availability = file_info_test.file_availability);
  assert (rate = file_info_test.file_download_rate);
  assert (chunks_age = file_info_test.file_chunks_age);
  assert (age = file_info_test.file_age);
  assert (last_seen = file_info_test.file_last_seen);
  assert (name = file_info_test.file_name);
  assert (priority = file_info_test.file_priority);
*)
  
  {
    file_fields = Fields_file_info.all;
    
    file_comment = comment;
    file_num = num;
    file_network = net;
    file_names = names;
    file_md4 = md4;
    file_size = size;
    file_downloaded = downloaded;
    file_all_sources = file_all_sources;
    file_active_sources = file_active_sources;
    file_state = state;
    file_chunks = 
      if chunks <> "" then 
        Some (VerificationBitmap.of_string chunks)
      else None;
    file_chunk_size = None;
    file_availability = availability;
    file_download_rate = rate;
    file_chunks_age = chunks_age;
    file_age = age;
    file_format = format;
    file_sources = None;
    file_name = name;
    file_last_seen = last_seen;
    file_priority = priority;
    file_uids = uids;
    file_sub_files = sub_files;
    file_magic = magic;
    file_comments = comments;
    file_user = user;
    file_group = group;
    file_release = false;
  }, pos

let get_host_state proto s pos =
  if proto < 12 then
  (match get_uint8 s pos with
  | 0 -> NotConnected (BasicSocket.Closed_by_user, -1)
  | 1 -> Connecting
  | 2 -> Connected_initiating
  | 3 -> Connected_downloading (-1)
  | 4 -> Connected (-1)
  | 5 -> Connected 0
  | 6 -> NewHost
  | 7 -> RemovedHost
  | 8 -> BlackListedHost
  | 9 -> NotConnected (BasicSocket.Closed_by_user,0)
  | 10 -> Connected (-2) 
  | _ -> assert false), pos+1
  else
  match get_uint8 s pos with
  | 0 -> NotConnected (BasicSocket.Closed_by_user,-1), pos+1
  | 1 -> Connecting, pos+1
  | 2 -> Connected_initiating, pos+1
  | 3 -> 
      if proto < 21 
      then Connected_downloading (-1), pos+1
      else  Connected_downloading(get_int s (pos+1)), pos+5
  | 4 -> Connected (-1), pos+1
  | 5 -> Connected (get_int s (pos+1)), pos+5
  | 6 -> NewHost, pos+1
  | 7 -> RemovedHost, pos+1
  | 8 -> BlackListedHost, pos+1
  | 9 -> NotConnected (BasicSocket.Closed_by_user,
     get_int s (pos+1)), pos+5
  | 10 -> Connected (-2), pos+1
  | _ -> assert false


let get_addr proto s pos =
  let addr, pos =
  match get_uint8 s pos with
    0 ->
      let ip, pos = get_ip2 proto s (pos+1) in
      Ip.addr_of_ip ip, pos
  | 1 ->
      let name,pos = get_hostname proto s (pos+1) in
      Ip.addr_of_string name, pos
  | _ -> assert false
  in
  let blocked, pos = 
    if proto > 33 then get_bool s pos, pos+1 
    else false, pos 
  in
    addr, pos

let get_server proto s pos =
  let num = get_int s pos in
  let net = get_int s (pos+4) in
  let addr, pos = if proto < 2 then
      Ip.addr_of_ip (get_ip s (pos+8)), pos+12
    else 
      get_addr proto s (pos+8)      
  in
  let port = get_int16 s pos in
  let score = get_int s (pos+2) in
  let tags, pos = get_list2 proto get_tag s (pos+6) in
  let nusers, pos = get_int64_28 proto s pos in
  let nfiles, pos = get_int64_28 proto s pos in
  let state, pos = get_host_state proto s pos in
  let name, pos = get_string s pos in
  let description, pos = get_string s pos in
  let preferred = if proto > 28 then get_bool s pos else false in
  let pos=pos+1 in
  let ve,ma,lo,so,ha,pi,pos = 
    if proto > 39 then
      let ve,pos = get_string s pos in
      let ma,pos = get_int64 s pos, (pos+8) in
      let lo,pos = get_int64 s pos, (pos+8) in
      let so,pos = get_int64 s pos, (pos+8) in
      let ha,pos = get_int64 s pos, (pos+8) in
      let pi,pos = get_int s pos, (pos+4) in
      ve,ma,lo,so,ha,pi,pos
    else
      "",0L,0L,0L,0L,0,pos
  in
  {
    server_num = num;
    server_network = net;
    server_addr = addr;
    server_port = port;
    server_realport = 0;
    server_country_code = None;
    server_score = score;
    server_tags = tags;
    server_nusers = nusers;
    server_nfiles = nfiles;
    server_state = state;
    server_name = name;
    server_description = description;
    server_banner = "";
    server_users = None;
    server_preferred = preferred;
    server_master = false;
    server_version = ve; 
    server_max_users = ma;
    server_lowid_users = lo;
    server_soft_limit = so;
    server_hard_limit = ha;
    server_ping = pi;
    server_published_files = 0;
    server_features = None;
  }, pos

let get_client_type s pos = 
  match get_uint8 s pos with
    0 -> 0
  | 1 -> client_friend_tag
  | 2 -> client_contact_tag
  | _ -> assert false

let get_kind proto s pos =
  match get_uint8 s pos with
    0 ->
      let ip,pos = get_ip2 proto s (pos+1) in
      let port = get_int16 s pos in
      Known_location (ip, port), pos+2
  | 1 ->
      let name, pos = get_hostname proto s (pos+1) in
      let md4 = get_md4 s pos in
      let ip, port, pos = 
        if proto > 38 then begin
          let myip , pos = get_ip2 proto s (pos+16) in
          let myport = get_int16 s pos in
          myip, myport, (pos+2)
        end else
          Ip.null, 0, (pos+16)
      in  
      Indirect_location (name, md4, ip, port), pos
  | _ -> assert false

let get_client proto s pos =
  if proto <= 18 then
    let num = get_int s pos in
    let net = get_int s (pos+4) in
    let kind, pos = get_kind proto s (pos+8) in
    let state,pos = get_host_state proto s pos in
    let t = get_client_type s pos in
    let tags, pos = get_list2 proto get_tag s (pos+1) in
    let name, pos = get_string s pos in
    let rating = get_int s pos in
    let chat_port = get_int s (pos+4) in
    {
      client_num = num;
      client_network = net;
      client_kind = kind;
      client_state = state;
      client_type = t;
      client_tags = tags;
      client_name = name;
      client_country_code = None;
      client_rating = rating;
      client_chat_port = chat_port;
      client_connect_time = 0;
      client_software = "";
      client_os = None;
      client_release = "";
      client_emulemod = "";
      client_total_downloaded = zero;
      client_total_uploaded = zero;
      client_session_downloaded = zero;
      client_session_uploaded = zero;
      client_upload = None;
      client_sui_verified = None;
      client_file_queue = [];
    }, pos+8
  else
  let num = get_int s pos in
  let net = get_int s (pos+4) in
  let kind, pos = get_kind proto s (pos+8) in
  let state,pos = get_host_state proto s pos in
  let t = get_client_type s pos in
  let tags, pos = get_list2 proto get_tag s (pos+1) in
  let name, pos = get_string s pos in
  let rating = get_int s pos in
  let software, pos = get_string s (pos+4) in
  let downloaded = get_int64 s pos in
  let uploaded = get_int64 s (pos+8) in
(*  let sock_addr, pos = get_string s (pos+16) in *)
  let pos = pos + 16 in
  let upload, pos = match get_string s pos with
      "", pos -> None, pos
    | s, pos -> Some s, pos
  in
  let connect_time, pos =
    if proto >= 20 then
      get_int_date proto s pos
    else 0, pos
  in
  let emulemod, pos =
    if proto >= 21 then
      get_string s pos
      else "", pos
   in
   let release, pos =
     if proto >= 33 then
       get_string s pos
       else "", pos
   in 
   let verified =
     if proto >= 35 then
       get_bool_option s pos
       else None
   in  {
    client_num = num;
    client_network = net;
    client_kind = kind;
    client_state = state;
    client_type = t;
    client_tags = tags;
    client_name = name;
    client_country_code = None;
    client_rating = rating;
    client_chat_port = 0;
    client_connect_time = connect_time;
    client_software = software;
    client_os = None;
    client_release = release;
    client_emulemod = emulemod;
    client_total_downloaded = downloaded;
    client_total_uploaded = uploaded;
    client_session_downloaded = 0L;
    client_session_uploaded = 0L;
    client_upload = upload;
    client_sui_verified = verified;
    client_file_queue = [];
  }, pos

let default_flags = [
    NetworkHasServers ;
    NetworkHasRooms;
    NetworkHasMultinet;
    NetworkHasSearch;
    NetworkHasChat;
    NetworkHasUpload
  ]

let get_network proto s pos =
  let num = get_int s pos in
  let name, pos = get_string s (pos+4) in
  let enabled = get_bool s pos in
  let config_file, pos = get_string s (pos+1) in
  let uploaded = get_int64 s pos in
  let downloaded = get_int64 s (pos+8) in
  let connected, flags, pos = 
    if proto > 17 then 
      let connected = get_int s (pos+16) in
      let flags, pos = 
        get_list (fun s pos ->
            (match get_int16 s pos with
                0 -> NetworkHasServers
              | 1 -> NetworkHasRooms
              | 2 -> NetworkHasMultinet
              | 3 -> VirtualNetwork
              | 4 -> NetworkHasSearch
              | 5 -> NetworkHasChat
              | 6 -> NetworkHasSupernodes
              | 7 -> NetworkHasUpload
              | _ -> UnknownNetworkFlag
            ), pos+2
        ) s (pos+20)  
      in
      connected, flags, pos
    else 0, default_flags, pos+16 in
  { network_netnum = num;
    network_netname = name;
    network_netflags = flags;
    network_enabled = enabled;
    network_config_filename = config_file;
    network_uploaded = uploaded;
    network_downloaded = downloaded;
    network_connected_servers = connected;
  }, pos


let get_user proto s pos =
  let num = get_int s pos in
  let md4 = get_md4 s (pos+4) in
  let name, pos = get_string s (pos+20) in
  let ip, pos = get_ip2 proto s pos in
  let port = get_int16 s pos in
  let tags, pos = get_list2 proto get_tag s (pos+2) in
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
  match get_uint8 s pos with
    0 -> RoomOpened
  | 1 -> RoomClosed
  | 2 -> RoomPaused
  | _ -> assert false

let get_room proto s pos =
  let num = get_int s pos in
  let net = get_int s (pos+4) in
  let name, pos = get_string s (pos+8) in
  let state = get_room_state s pos in
  let nusers,pos = if proto >= 3 then get_int s (pos+1), pos+5 else 0, pos+1 in
  {
    room_num = num;
    room_network = net;
    room_name = name;
    room_state = state;
    room_users = [];
    room_messages = [];
    room_nusers = nusers;
  }, pos + 1 

let get_shared_info proto s pos =
  let num = get_int s pos in
  let network = get_int s (pos+4) in
  let name, pos = get_string s (pos+8) in
  let size,pos = get_uint64_2 proto s pos in
  let uploaded = get_int64 s pos in
  let requests = get_int s (pos+8) in
  {
    shared_num = num;
    shared_network = network;
    shared_filename = name;
    shared_size = size;
    shared_uploaded = uploaded;
    shared_requests = requests;
    shared_uids = [];
    shared_sub_files = [];
    shared_magic = Some "";
  }

let get_shared_info_version_10 proto s pos =
  let num = get_int s pos in
  let network = get_int s (pos+4) in
  let name, pos = get_string s (pos+8) in
  let size,pos = get_uint64_2 proto s pos in
  let uploaded = get_int64 s pos in
  let requests = get_int s (pos+8) in
  let uids, pos = 
    if proto < 31 then
      [], pos+12
    else
      get_list get_uid s (pos+12)
  in
  let sub_files, pos =
    if proto > 36 then
      get_sub_files proto s pos
    else [], pos
  in
  let magic, pos = 
    if proto > 40 then
      let ms, pos = get_string s (pos) in
      Some ms, pos
    else
      Some "", pos
  in
  {
    shared_num = num;
    shared_network = network;
    shared_filename = name;
    shared_size = size;
    shared_uploaded = uploaded;
    shared_requests = requests;
    shared_uids = uids;
    shared_sub_files = sub_files;
    shared_magic = magic;
  }


(***************

     Decoding of messages from the GUI to the Core 

****************)

let from_gui (proto : int array) opcode s =
  try

    let proto = if opcode > from_gui_last_opcode then 0 else proto.(opcode) in
    if !verbose_gui_decoding then
      lprintf_nl "[gDe] FROM_GUI: Opcode %d %s" opcode (String.escaped s);
    match opcode with
      0 -> GuiProtocol (get_int s 2)

    | 1 -> ConnectMore_query

    | 2 -> CleanOldServers

    | 3 -> KillServer

    | 4 -> ExtendedSearch (-1, ExtendSearchRemotely)

    | 5
    | 52 ->
        if proto < 14 then
           let pass = fst (get_string s 2) in Password ((CommonUserDb.admin_user ()).CommonTypes.user_name, pass)
        else
        let pass,pos = get_string s 2 in
        let login,pos = get_string s pos in
        Password (login, pass)

    | 6 ->
        let local = get_bool s 2 in
        let search, pos = get_search get_query  proto s 3 in
        search.search_type <- if local then LocalSearch else RemoteSearch;
        Search_query search

    | 7 ->
        let list, pos = get_list get_string s 2 in
        let result_num = get_int s pos in
        Download_query (list, result_num, false)

    | 8 -> let string, pos = get_string s 2 in
        lprintf_nl "[gDe] Received string: [%s]" (String.escaped string);
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
        let tag, pos = get_mp3  s 6 in
        ModifyMp3Tags (int, tag)

    | 27 ->
        let int = get_int s 2 in 
        CloseSearch  (int, true)

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
        let room_message, pos = get_message  s 6 in        
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

    | 42 -> let s, pos = get_search get_query  proto s 2 in Search_query s

    | 43 -> 
        let int = get_int s 2 in 
        let message, pos = get_string s 6 in
        MessageToClient (int, message)

    | 44 -> GetConnectedServers

    | 45 -> GetDownloadFiles

    | 46 -> GetDownloadedFiles

    | 47 ->
        let list, pos = get_list (fun s pos -> 
              (get_int s pos, 1 = get_uint8 s (pos+4)), pos+5) s 2 in
        GuiExtensions list

    | 48 ->
        SetRoomState (get_int s 2, get_room_state s 6)

    | 49 -> RefreshUploadStats

    | 50 ->
        let list, pos = get_list get_string s 2 in
        let result_num = get_int s pos in
        let force = get_bool s (pos+4) in
        Download_query (list, result_num, force)

    | 51 ->
        SetFilePriority(get_int s 2, get_int s 6)      

    (* 52 -> see 5 *)

    | 53 ->
        let int = get_int s 2 in 
        let bool = get_bool s 6 in 
        CloseSearch  (int, bool)

    | 54 ->
        let net = get_int s 2 in
        let ip = get_ip s 6 in
        let port = get_int16 s 10 in
        AddServer_query (net, ip, port) 

    | 55 ->
        let list, pos = get_list  (fun s pos ->
              let opcode = get_int16 s pos in
              let from_guip = get_bool s (pos+2) in
              let proto = get_int s (pos+3) in
              (opcode, from_guip, proto), pos+7
          )  s 2 in
        MessageVersions list

    | 56 ->
        let num = get_int s 2 in
        let new_name, pos = get_string s 6 in
        RenameFile(num, new_name)

    | 57 ->
        GetUploaders

    | 58 ->
        GetPending

    | 59 ->
        GetSearches

    | 60 ->
        GetSearch (get_int s 2)

    | 61 ->
        ConnectClient (get_int s 2)

    | 62 ->
        DisconnectClient (get_int s 2)

    | 63 ->
        let n = get_int s 2 in
        let s, pos = get_string s 6 in
        NetworkMessage (n, s)

    | 64 ->
        InterestedInSources (get_bool s 2)

    | 65 -> 
        GetVersion

    (* introduced with protocol 32 *)
    | 66 ->
         let num = get_int s 2 in
         let name, pos = get_string s 6 in
         ServerRename (num, name)

    | 67 ->
         let num = get_int s 2 in
         let b = get_bool s 6 in
         ServerSetPreferred (num, b)

    | 68 ->
         let num = get_int s 2 in
         GetStats num

    | _ -> 
        lprintf_nl "FROM GUI:Unknown message %d" opcode; 
        raise FromGuiMessageNotImplemented

  with e ->
      lprintf_nl "Decoding gui proto[%d]: exception %s, opcode %d" proto.(opcode)
        (Printexc2.to_string e) opcode;
      dump s;
      raise e

(***************

     Decoding of messages from the Core to the GUI 

****************)

let dummy_option = 
  let module M = Options in
  {
    M.option_name = "";
    M.option_shortname = "";
    M.option_desc = "";
    M.option_default = "";
    M.option_value = "";
    M.option_help = "";
    M.option_type = "";
    M.option_advanced = false;
    M.option_restart = false;
    M.option_public = false;
    M.option_internal = false;
  }

let to_gui (proto : int array)  opcode s =
  try
    let proto = if opcode > to_gui_last_opcode then 0 else proto.(opcode) in
    
    if !verbose_gui_decoding then
      lprintf_nl "[gDe] TO GUI: Opcode %d" opcode;
    match opcode with
    | 0 ->
        let version = get_int s 2 in
        let (max_to_gui, max_from_gui) =
          if String.length s > 10 then
            get_int s 6, get_int s 10
          else
            57, 62
        in
        CoreProtocol (version, max_to_gui, max_from_gui)
    
    | 1 ->
        let list, pos = get_list (fun s pos ->
              let module M = Options in
              let name, pos = get_string s pos in
              let value, pos = get_string s pos in
              { dummy_option with
                M.option_name = name;
                M.option_value = value;
              }, pos
          ) s 2 in
        Options_info list
    
    | 3 ->
        let list, pos = get_list (fun s pos ->
              let name, pos = get_string s pos in
              let q, pos = get_query  s pos in
              (name, q), pos) s 2 in
        DefineSearches list
    
    | 4 -> 
        let r, pos = get_result proto  s 2 in
        Result_info r
    
    | 5 ->
        let n1 = get_int s 2 in
        let n2 = get_int s 6 in
        Search_result (n1,n2, None)
    
    | 6 -> 
        let n1 = get_int s 2 in
        let n2 = get_int s 6 in
        Search_waiting (n1,n2)
    
    | 7 -> 
        let file_info, pos = get_file proto  s 2 in
        File_info file_info
    
    | 8 ->
        let n = get_int s 2 in
        let size,pos = get_uint64_2 proto s 6 in
        let rate, pos = get_float  s pos in
        File_downloaded (n, size, rate, BasicSocket.last_time ())
    
    | 9 ->
        let file_num = get_int s 2 in
        let client_num = get_int s 6 in
        let avail,_ = get_string_bin s 10 in
        File_update_availability (file_num, client_num, avail)
    
    | 10 -> 
        let n1 = get_int s 2 in
        let n2 = get_int s 6 in
        File_add_source (n1,n2)
    
    | 11 ->
        let n1 = get_int s 2 in
        let n2, pos = get_int64_28 proto s 6 in
        let n3, pos = get_int64_28 proto s pos in
        Server_busy (n1,n2,n3)
    
    | 12 -> 
        let n1 = get_int s 2 in
        let n2 = get_int s 6 in
        Server_user  (n1,n2)
    
    | 13 -> 
        let int = get_int s 2 in
        let host_state, pos = get_host_state proto s 6 in
        Server_state (int,host_state)
    
    | 14 ->
        let server_info, pos = get_server proto  s 2 in
        Server_info server_info
    
    | 15 -> 
        let client_info, pos = get_client proto  s 2 in
        Client_info client_info
    
    | 16 -> 
        let int = get_int s 2 in
        let host_state, pos = get_host_state proto s 6 in
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
        let network_info, pos = get_network proto  s 2 in
        Network_info network_info
    
    | 21 ->
        let user_info, pos = get_user proto s 2 in
        User_info user_info
    
    | 22 ->
        let room_info, pos = get_room proto  s 2 in
        Room_info room_info
    
    | 23 ->
        let int = get_int s 2 in
        let room_message, pos = get_message  s 6 in
        Room_message (int, room_message) 
    
    | 24 ->
        let n1 = get_int s 2 in
        let n2 = get_int s 6 in          
        Room_add_user (n1,n2)
    
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
          connected_networks = [];
          ndownloading_files = 0;
          ndownloaded_files = 0;
        }
    
    | 26 -> let s, pos = get_server proto  s 2 in Server_info s
    | 27 -> 
        let int = get_int s 2 in 
        let message, pos = get_string s 6 in
        MessageFromClient (int, message)
    
    | 28 -> 
        let list, pos = get_list (get_server proto ) s 2 in
        ConnectedServers list
    
    | 29 ->
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadFiles list
    
    | 30 ->
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadedFiles list
    
    | 31 ->
        let room_info, pos = get_room proto  s 2 in
        Room_info room_info
    
    | 32 -> 
        let room = get_int s 2 in 
        let user = get_int s 6 in
        Room_remove_user (room, user)
    | 33 ->
        let s = get_shared_info proto  s 2 in
        Shared_file_info s
    
    | 34 ->
        let num = get_int s 2 in
        let upload = get_int64 s 6 in
        let requests = get_int s 14 in
        Shared_file_upload (num, upload, requests)
    
    | 35 ->
        let num = get_int s 2 in
        Shared_file_unshared num
    
    | 36 -> 
        let section, pos = get_string s 2 in
        let message, pos = get_string s pos in 
        let option, pos = get_string s pos in
        let module M = Options in
        let o = if proto > 16 then 
            let optype, pos = get_string s pos in
            let help, pos = get_string s pos in
            let value, pos = get_string s pos in
            let default, pos = get_string s pos in
            let advanced = get_bool s pos in
            {
              M.option_desc = message;
              M.option_name = option;
              M.option_shortname = option;
              M.option_type = optype;
              M.option_help = help;
              M.option_value = value;
              M.option_default = default;
              M.option_advanced = advanced;
              M.option_restart = false;
              M.option_public = false;
              M.option_internal = false;
            }
          else
          let optype = 
            match get_uint8 s pos with
              0 -> "String" 
            | 1 -> "Bool" 
            | 2 -> "Filename"
            | _ -> assert false in
          {
            dummy_option with
            M.option_desc = message;
            M.option_name = option;
            M.option_shortname = option;
            
            M.option_type = optype;
          } in
        Add_section_option (section, o)
        
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
          connected_networks = [];
          ndownloading_files = 0;
          ndownloaded_files = 0;
        
        }
    
    | 38 -> 
        let section, pos = get_string s 2 in
        let message, pos = get_string s pos in 
        let option, pos = get_string s pos in
        let module M = Options in
        let o = if proto > 16 then 
            let optype, pos = get_string s pos in
            let help, pos = get_string s pos in
            let value, pos = get_string s pos in
            let default, pos = get_string s pos in
            let advanced = get_bool s pos in
            {
              M.option_desc = message;
              M.option_name = option;
              M.option_shortname = option;
              M.option_type = optype;
              M.option_help = help;
              M.option_value = value;
              M.option_default = default;
              M.option_advanced = advanced;
              M.option_restart = false;
              M.option_public = false;
              M.option_internal = false;
            }
          else
          let optype = 
            match get_uint8 s pos with
              0 -> "String" 
            | 1 -> "Bool" 
            | 2 -> "Filename"
            | _ -> assert false in
          {
            dummy_option with
            M.option_desc = message;
            M.option_name = option;
            M.option_type = optype;
          } in
        Add_plugin_option (section, o)
    
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
          connected_networks = [];
          ndownloading_files = 0;
          ndownloaded_files = 0;
        }
    
    |  40 ->
        let file, pos = get_file proto  s 2 in
        File_info file
    
    | 41 ->
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadFiles list
    
    | 42 ->
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadedFiles list
    
    | 43 ->
        let file, pos = get_file proto  s 2 in
        File_info file
    
    | 44 ->
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadFiles list
    
    | 45 ->
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadedFiles list
    
    | 46 ->
        let n = get_int s 2 in
        let size,pos = get_uint64_2 proto s 6 in
        let rate, pos = get_float  s pos in
        let last_seen = get_int s pos in
        File_downloaded (n, size, rate, 
          BasicSocket.last_time () - last_seen)
    
    | 47 -> BadPassword
    
    | 48 ->
        let s = get_shared_info_version_10 proto  s 2 in
        Shared_file_info s      
    
    | 49 ->
        let upload = get_int64 s 2 in
        let download = get_int64 s 10 in
        let shared = get_int64 s 18 in
        let nshared = get_int s 26 in
        let tcp_upload_rate = get_int s 30 in
        let tcp_download_rate = get_int s 34 in
        let udp_upload_rate = get_int s 38 in
        let udp_download_rate = get_int s 42 in
        let ndownloading_files = get_int s 46 in
        let ndownloaded_files = get_int s 50 in
        let connected_networks, pos = get_list
            (fun s pos -> 
              if proto > 17 then 
                (get_int s pos, get_int s (pos+4)), pos+8
              else
                ( (get_int s pos,0),  pos+4 )
          )  s 54 in
        
        Client_stats {
          upload_counter = upload;
          download_counter = download;
          shared_counter = shared;
          nshared_files = nshared;
          udp_upload_rate = udp_upload_rate;
          udp_download_rate = udp_download_rate;
          tcp_upload_rate = tcp_upload_rate;
          tcp_download_rate = tcp_download_rate;
          connected_networks = connected_networks;
          ndownloading_files = ndownloading_files;
          ndownloaded_files = ndownloaded_files;
        }
    
    | 50 -> 
        let n1 = get_int s 2 in
        let n2 = get_int s 6 in
        File_remove_source (n1,n2)
    
    | 51 ->
        let clients,pos = get_list (fun s pos ->
              get_int s pos, pos+4) s 2 in
        let servers,pos = get_list (fun s pos ->
              get_int s pos, pos+4) s pos in
        CleanTables (clients, servers)

    | 52 -> 
        let file, pos = get_file proto  s 2 in
        File_info file
    | 53 -> 
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadFiles list
    | 54 -> 
        let list, pos = get_list (get_file proto ) s 2 in
        DownloadedFiles list

    | 55 ->
        let list, pos = get_list get_int_pos s 2 in
        Uploaders list
    | 56 ->
        let list, pos = get_list get_int_pos s 2 in
        Pending list
        
    | 57 ->
        let s, pos = get_search get_string proto s 2 in
        Search s
        
    | 58 ->
        let s, pos = get_string s 2 in 
        Version s
        
    | _ -> 
        lprintf_nl "TO GUI:Unknown message %d" opcode; 
        raise Not_found


  with e ->
      lprintf_nl "Decoding gui proto[%d]: exception %s, opcode %d" proto.(opcode)
        (Printexc2.to_string e) opcode;
      
      dump s;
      raise e
