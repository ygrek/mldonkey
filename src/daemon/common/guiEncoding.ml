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

open BasicSocket
open Printf2
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
    Buffer.reset buf;
    buf_int buf 0;
    writer buf t;
    let s = Buffer.to_bytes buf in
    let len = Bytes.length s - 4 in
    str_int s 0 len;
    write_all_bytes sock s;
  with UnsupportedGuiMessage -> ()
      
(***************

       Encoding of messages basic data types

****************)

let buf = () (* lots of buf variables here. Be sure not to use a previously
  defined one *)
    
let buf_list buf f list =
  buf_int16 buf (List.length list);
  List.iter (fun x -> f buf x) list
  
let buf_list2 proto buf f list =
  buf_int16 buf (List.length list);
  List.iter (fun x -> f proto buf x) list
  
let buf_array buf f list =
  buf_int16 buf (Array.length list);
  Array.iter (fun x -> f buf x) list

(* this else block already destroys backwards compatibility *)
(* if len <= 0xfffff, how would the gui know what to do when len == 0xffff? *)

let buf_string buf s =
  (* charset conversion could generate longer strings *)
  let s = Charset.Locale.to_utf8 s in
  let len = String.length s in
  if len < 0xffff then begin
      buf_int16 buf len;
      Buffer.add_string buf s
    end else  begin
      buf_int16 buf 0xffff;
      buf_int buf len;
      Buffer.add_string buf s
    end 

let buf_string_bin buf s =
  buf_string buf s 
    
let buf_hostname proto buf s =
  buf_string buf s;
  if proto > 37 then
    let cc = 0 in (* TODO: figure out country# of this hostname *)
    buf_int8 buf cc

let buf_ip2 proto buf ip cc =
  buf_ip buf ip;
  if proto > 37 then begin
    let cc =
      match cc with
      | None -> Geoip.get_country_code ip
      | Some cc -> cc
    in
    buf_int8 buf cc
  end
    
let buf_uid buf uid =
  buf_string buf (Uid.to_string uid)

let buf_float buf f =
  let i = int_of_float f in
  buf_string buf (Printf.sprintf "%d.%d" i (int_of_float ((f -. float_of_int i) *. 100.)))

let buf_float_date proto buf date =
(*  lprintf "buf_float_date(%d): %d\n" proto date; *)
  if proto > 23 then
    buf_int buf (last_time () - date)
  else
    buf_string buf (Printf.sprintf "%.0f" (BasicSocket.date_of_int date))

let buf_int_date proto buf date =
  if proto > 23 then
    buf_int buf (last_time () - date)
  else
    buf_int buf date

let buf_int64_2 proto buf i =
  if proto > 24 then
    buf_int64 buf i
  else
    buf_int64_32 buf i
  
let buf_int64_28 proto buf i =
  if proto > 27 then
    buf_int64 buf i
  else
    buf_int64_32 buf i
  
let rec buf_query buf q =
  match q with
    Q_AND list ->
      buf_int8 buf 0;
      buf_list buf buf_query list
  | Q_OR list ->
      buf_int8 buf 1;
      buf_list buf buf_query list
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
      buf_list buf buf_query list
  | Q_COMBO _ -> assert false
      
let buf_tag proto buf t =
  buf_string buf (string_of_field t.tag_name);
  match t.tag_value with
  | Uint64 s -> buf_int8 buf 0; buf_int64_32 buf s
  | Fint64 s -> buf_int8 buf 1; buf_int64_32 buf s
  | String s -> buf_int8 buf 2; buf_string buf s
  | Addr ip -> buf_int8 buf 3; buf_ip2 proto buf ip None
  | Uint16 n -> buf_int8 buf 4; buf_int16 buf n
  | Uint8 n -> buf_int8 buf 5; buf_int8 buf n
  | Pair (n1,n2) -> buf_int8 buf 6; buf_int64_32 buf n1; buf_int64_32 buf n2
      
let buf_host_state proto buf t =
  if proto < 12 then
    buf_int8 buf (    
      match t with
      | ServerFull
      | NotConnected _ -> 0
      | Connecting -> 1
      | Connected_initiating -> 2
      | Connected_downloading _ -> 3
      | Connected (-1) -> 4
      | Connected i -> if i < 0 then 4 else 5
      | NewHost -> 6
      | RemovedHost -> 7
      | BlackListedHost -> if proto < 10 then 0 else 8)
  else    
  match t with
  | ServerFull
  | NotConnected (_, -1) -> buf_int8 buf 0
  | Connecting -> buf_int8 buf  1
  | Connected_initiating -> buf_int8 buf 2
  | Connected_downloading n -> 
      if proto < 21 
      then buf_int8 buf 3
      else (buf_int8 buf 3; buf_int buf n)
  | Connected (-1) -> buf_int8 buf 4
  | Connected (-2) -> buf_int8 buf (if proto >= 22 then 10 else 4)
  | Connected n -> buf_int8 buf 5; buf_int buf n
  | NewHost -> buf_int8 buf 6
  | RemovedHost -> buf_int8 buf 7
  | BlackListedHost -> buf_int8 buf (if proto < 10 then 0 else 8)
  | NotConnected (_,n) -> buf_int8 buf 9; buf_int buf n
      
    
let buf_client_type buf t =
  buf_int8 buf (
    if t land client_friend_tag <> 0 then 1 else
    if t land client_contact_tag <> 0 then 2 else
      0)
  
let buf_bool buf b =
  buf_int8 buf (if b then 1 else 0)
      
let buf_bool_option buf b =
  buf_int8 buf
  (match b with
  | None -> 2
  | Some b -> if b then 1 else 0)
      
let buf_result proto buf r =
  buf_int buf r.result_num;
  buf_int buf r.result_source_network;
  buf_list buf buf_string r.result_names;
  if proto < 27 then
    buf_md4 buf Md4.null
  else
    buf_list buf buf_uid r.result_uids;
  buf_int64_2 proto buf r.result_size;
  buf_string buf r.result_format;
  buf_string buf r.result_type;
  buf_list2 proto buf buf_tag r.result_tags;
  buf_string buf r.result_comment;
  buf_bool buf r.result_done;
  if proto > 26 then
    let date = r.result_time in
    buf_int buf (last_time () - date)

let buf_user proto buf u =
  buf_int buf u.user_num;
  buf_md4 buf u.user_md4;
  buf_string buf u.user_name;
  buf_ip2 proto buf u.user_ip None;
  buf_int16 buf u.user_port;
  buf_list2 proto buf buf_tag u.user_tags;
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

let buf_ogg_stream_type buf st =
  match st with
  OGG_VIDEO_STREAM  -> buf_int8 buf 0
| OGG_AUDIO_STREAM  -> buf_int8 buf 1
| OGG_INDEX_STREAM  -> buf_int8 buf 2
| OGG_TEXT_STREAM   -> buf_int8 buf 3
| OGG_VORBIS_STREAM -> buf_int8 buf 4
| OGG_THEORA_STREAM -> buf_int8 buf 5

let buf_vorbis_bitrate buf br =
  match br with
  Maximum_br r -> buf_int8 buf 0; buf_float buf r
| Nominal_br r -> buf_int8 buf 1; buf_float buf r
| Minimum_br r -> buf_int8 buf 2; buf_float buf r

let buf_theora_cs buf cs =
  match cs with
  CSUndefined -> buf_int8 buf 0
| CSRec470M   -> buf_int8 buf 1
| CSRec470BG  -> buf_int8 buf 2

let buf_ogg_stream_tag buf tag =
  match tag with
  Ogg_codec s                 -> buf_int8 buf 0; buf_string buf s
| Ogg_bits_per_samples n      -> buf_int8 buf 1; buf_int buf n
| Ogg_duration n              -> buf_int8 buf 2; buf_int buf n
| Ogg_has_subtitle            -> buf_int8 buf 3
| Ogg_has_index               -> buf_int8 buf 4
| Ogg_audio_channels n        -> buf_int8 buf 5; buf_int buf n
| Ogg_audio_sample_rate r     -> buf_int8 buf 6; buf_float buf r
| Ogg_audio_blockalign n      -> buf_int8 buf 7; buf_int buf n
| Ogg_audio_avgbytespersec r  -> buf_int8 buf 8; buf_float buf r
| Ogg_vorbis_version r        -> buf_int8 buf 9; buf_float buf r
| Ogg_vorbis_sample_rate r    -> buf_int8 buf 10; buf_float buf r
| Ogg_vorbis_bitrates l       -> buf_int8 buf 11; buf_list buf buf_vorbis_bitrate l
| Ogg_vorbis_blocksize_0 n    -> buf_int8 buf 12; buf_int buf n
| Ogg_vorbis_blocksize_1 n    -> buf_int8 buf 13; buf_int buf n
| Ogg_video_width r           -> buf_int8 buf 14; buf_float buf r
| Ogg_video_height r          -> buf_int8 buf 15; buf_float buf r
| Ogg_video_sample_rate r     -> buf_int8 buf 16; buf_float buf r
| Ogg_aspect_ratio r          -> buf_int8 buf 17; buf_float buf r
| Ogg_theora_cs cs            -> buf_int8 buf 18; buf_theora_cs buf cs
| Ogg_theora_quality n        -> buf_int8 buf 19; buf_int buf n
| Ogg_theora_avgbytespersec n -> buf_int8 buf 20; buf_int buf n

let buf_ogg buf stream =
  buf_int buf stream.stream_no;
  buf_ogg_stream_type buf stream.stream_type;
  buf_list buf buf_ogg_stream_tag stream.stream_tags

let buf_format proto buf f =
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
  | OGG ogg_infos ->
      if proto > 31
        then begin
          buf_int8 buf 4;
          buf_list buf buf_ogg ogg_infos;
        end else buf_int8 buf 0

let buf_kind proto buf k cc =
  match k with
    Known_location (ip, port) -> 
      buf_int8 buf 0; buf_ip2 proto buf ip cc; buf_int16 buf port
  | Indirect_location (name, md4, ip, port) ->
      buf_int8 buf 1; buf_hostname proto buf name; buf_md4 buf md4;
        if proto > 38 then begin buf_ip2 proto buf ip cc; buf_int16 buf port end
      
let buf_partial_file proto buf f =
  buf_int buf f.file_num;
  if f.file_fields.Fields_file_info.file_network then begin
      buf_int8 buf 1;
      buf_int buf f.file_network;  
    end;
  if f.file_fields.Fields_file_info.file_names then begin
      buf_int8 buf 2;
      buf_list buf buf_string f.file_names;
    end;      
  if f.file_fields.Fields_file_info.file_md4 then begin
      buf_int8 buf 3;
      buf_md4 buf f.file_md4;  
    end;
  if f.file_fields.Fields_file_info.file_size then begin
      buf_int8 buf 4;
      buf_int64_2 proto buf f.file_size;  
    end;
  if f.file_fields.Fields_file_info.file_downloaded then begin
      buf_int8 buf 5;
      buf_int64_2 proto buf f.file_downloaded;  
    end;
  if f.file_fields.Fields_file_info.file_nlocations then begin
      buf_int8 buf 6;
      buf_int buf f.file_all_sources;  
    end;
  if f.file_fields.Fields_file_info.file_nclients then begin
      buf_int8 buf 7;
      buf_int buf f.file_active_sources;  
    end;
  if f.file_fields.Fields_file_info.file_state then begin
      buf_int8 buf 8;
      buf_file_state proto buf f.file_state;  
    end;
  if f.file_fields.Fields_file_info.file_chunks then begin
      buf_int8 buf 9;
    buf_string buf (match f.file_chunks with
    | None -> ""
    | Some chunks -> VerificationBitmap.to_string chunks);  
    end;
  if f.file_fields.Fields_file_info.file_availability then begin
      buf_int8 buf 10;
      buf_list buf (fun buf (network, avail) ->
          buf_int buf network;
          buf_string_bin buf avail
      ) f.file_availability
    end;
  if f.file_fields.Fields_file_info.file_download_rate then begin
      buf_int8 buf 11;
      buf_float buf f.file_download_rate;  
    end;
  if f.file_fields.Fields_file_info.file_chunks_age then begin
      buf_int8 buf 12;
      buf_array buf (buf_float_date proto) f.file_chunks_age;  
    end;
  if f.file_fields.Fields_file_info.file_age then begin
      buf_int8 buf 13;
      buf_float_date proto buf f.file_age;
    end;
  if f.file_fields.Fields_file_info.file_format then begin
      buf_int8 buf 14;
      buf_format proto buf f.file_format;
    end;
  if f.file_fields.Fields_file_info.file_name then begin
      buf_int8 buf 15;
      buf_string buf f.file_name;
    end;
  if f.file_fields.Fields_file_info.file_last_seen then begin
      buf_int8 buf 16;
      buf_int buf (compute_last_seen f.file_last_seen);
    end;
  if f.file_fields.Fields_file_info.file_priority then begin
      buf_int8 buf 17;
      buf_int buf f.file_priority;
    end;
  if f.file_fields.Fields_file_info.file_comment then begin
      buf_int8 buf 18;
      buf_string buf f.file_comment
    end          
      
let buf_file_field proto buf field =
  match field with
  | Fields_file_info.File_network x ->
      buf_int8 buf 1;
      buf_int buf x
  | Fields_file_info.File_names x ->
      buf_int8 buf 2;
      buf_list buf buf_string x;  
  | Fields_file_info.File_md4 x ->
      buf_int8 buf 3;
      buf_md4 buf x
  | Fields_file_info.File_size x ->
      buf_int8 buf 4;
      buf_int64_2 proto buf x
  | Fields_file_info.File_downloaded x ->
      buf_int8 buf 5;
      buf_int64_2 proto buf x
  | Fields_file_info.File_nlocations x ->
      buf_int8 buf 6;
      buf_int buf x
  | Fields_file_info.File_nclients x ->
      buf_int8 buf 7;
      buf_int buf x
  | Fields_file_info.File_state x ->
      buf_int8 buf 8;
      buf_file_state proto buf x
  | Fields_file_info.File_chunks x ->
      buf_int8 buf 9;
      buf_string buf x
  | Fields_file_info.File_availability x ->
      buf_int8 buf 10;
      buf_list buf (fun buf (network, avail) ->
          buf_int buf network;
          buf_string_bin buf avail
      ) x
  | Fields_file_info.File_download_rate x ->
      buf_int8 buf 11;
      buf_float buf x
  | Fields_file_info.File_chunks_age x ->
      buf_int8 buf 12;
      buf_array buf (buf_float_date proto) x
  | Fields_file_info.File_age x ->
      buf_int8 buf 13;
      buf_float_date proto buf x
  | Fields_file_info.File_format x ->
      buf_int8 buf 14;
      buf_format proto buf x
  | Fields_file_info.File_name x ->
      buf_int8 buf 15;
      buf_string buf x
  | Fields_file_info.File_last_seen x ->
      buf_int8 buf 16;
      buf_int buf (compute_last_seen x);
  | Fields_file_info.File_priority x ->
      buf_int8 buf 17;
      buf_int buf x
  | Fields_file_info.File_comment x ->
      buf_int8 buf 18;
      buf_string buf x
    
let magic_string m =
  match m with
     Some s -> s
   | None -> ""
    
let buf_sub_files proto buf l =
  buf_list buf (fun buf (name, size, magic) ->
    buf_string buf name;
    buf_int64 buf size;
    if proto > 40 then
      buf_string buf (magic_string magic)
  ) l

let buf_file_comments proto buf l =
  buf_list buf (fun buf (ip, name, rating, comment) ->
    buf_ip2 proto buf ip None;
    buf_string buf name;
    buf_int8 buf rating;
    buf_string buf comment;
  ) l
    
let buf_file proto buf f =
  buf_int buf f.file_num;
  buf_int buf f.file_network;  
  buf_list buf buf_string f.file_names;
  buf_md4 buf f.file_md4;  
  buf_int64_2 proto buf f.file_size;  
  buf_int64_2 proto buf f.file_downloaded;  
  buf_int buf f.file_all_sources;  
  buf_int buf f.file_active_sources;  
  buf_file_state proto buf f.file_state;  
  buf_string buf (match f.file_chunks with
  | None -> ""
  | Some chunks -> VerificationBitmap.to_string chunks);  
  if proto > 17 then
    buf_list buf (fun buf (network, avail) ->
        buf_int buf network;
        buf_string_bin buf avail
    ) f.file_availability
  else
    buf_string_bin buf (match f.file_availability with
        [] -> ""
      | (_, av) :: _ -> av);
  buf_float buf f.file_download_rate;  
  buf_array buf (buf_float_date proto) f.file_chunks_age;  
  buf_float_date proto buf f.file_age;
(* last, so that it can be safely discarded in partial implementations: *)
  buf_format proto buf f.file_format;
  if proto >= 8 then 
    buf_string buf f.file_name;
  if proto >= 9 then 
    let ls = compute_last_seen f.file_last_seen in
    buf_int buf ls;
    if proto >= 12 then
      buf_int buf f.file_priority;
    if proto > 21 then
      buf_string buf f.file_comment;
    if proto > 30 then
      buf_list buf buf_uid f.file_uids;
    if proto > 35 then
      buf_sub_files proto buf f.file_sub_files;
    if proto > 40 then begin
      buf_string buf (magic_string f.file_magic);
      buf_file_comments proto buf f.file_comments;
      buf_string buf f.file_user;
      buf_string buf f.file_group;
    end
      
let buf_addr proto buf addr cc =
  (match addr with
    Ip.AddrIp ip ->
      buf_int8 buf 0;
      buf_ip2 proto buf ip cc
  | Ip.AddrName s ->
      buf_int8 buf 1;
      buf_hostname proto buf s
  );
  if proto > 33 then begin
    let is_blocked =
      try !Ip.banned (Ip.ip_of_addr addr, cc) <> None
      with _ -> false
    in
    buf_bool buf is_blocked 
  end
  
let buf_server proto buf s =
  buf_int buf s.server_num;
  buf_int buf s.server_network;
  if proto < 2 then 
    buf_ip buf (Ip.ip_of_addr s.server_addr)
  else
    buf_addr proto buf s.server_addr s.server_country_code;
  buf_int16 buf s.server_port;
  buf_int buf s.server_score;
  buf_list2 proto buf buf_tag s.server_tags;
  buf_int64_28 proto buf s.server_nusers;
  buf_int64_28 proto buf s.server_nfiles;
  buf_host_state proto buf s.server_state;
  buf_string buf s.server_name;
  buf_string buf s.server_description;
  if proto > 28 then 
    buf_bool buf s.server_preferred;
  if proto > 39 then begin
    buf_string buf s.server_version;
    buf_int64 buf s.server_max_users;
    buf_int64 buf s.server_lowid_users;
    buf_int64 buf s.server_soft_limit;
    buf_int64 buf s.server_hard_limit;
    buf_int buf s.server_ping;
  end

let buf_client proto buf c =
  buf_int buf c.client_num;
  buf_int buf c.client_network;
  buf_kind proto buf c.client_kind c.client_country_code;
  buf_host_state proto buf c.client_state;
  buf_client_type buf c.client_type;
  buf_list2 proto buf buf_tag c.client_tags;
  buf_string buf c.client_name;
  buf_int buf c.client_rating;
  if proto <= 18 then begin
      buf_int buf c.client_chat_port
    end else 
    begin
      buf_string buf (client_software_short c.client_software c.client_os);
      buf_int64 buf c.client_session_downloaded;
      buf_int64 buf c.client_session_uploaded;
      (match c.client_upload with
          Some s -> buf_string buf s
        | None -> buf_string buf "");
      if proto >= 20 then begin
          buf_int_date proto buf c.client_connect_time
      end;
      if proto >= 21 then begin
        buf_string buf c.client_emulemod
      end;
      if proto >= 33 then begin
        buf_string buf c.client_release
      end;
      if proto >= 35 then begin
        buf_bool_option buf c.client_sui_verified
      end;
    end
    
let buf_network proto buf n =
  buf_int buf n.network_netnum;
  buf_string buf n.network_netname;
  buf_bool buf n.network_enabled;
  buf_string buf n.network_config_filename;
  buf_int64 buf n.network_uploaded;
  buf_int64 buf n.network_downloaded;
  if proto > 17 then begin
      buf_int buf n.network_connected_servers;
      buf_list buf (fun buf e ->
          buf_int16 buf (match e with
              NetworkHasServers -> 0
            | NetworkHasRooms -> 1
            | NetworkHasMultinet -> 2
            | VirtualNetwork -> 3
            | NetworkHasSearch -> 4
            | NetworkHasChat -> 5
            | NetworkHasSupernodes -> 6
            | NetworkHasUpload -> 7
            | NetworkHasStats -> -1
            | UnknownNetworkFlag -> -1
          )
      ) n.network_netflags;
    end

let buf_search_type buf t =
  buf_int8 buf (
    match t with
      LocalSearch -> 0
    | RemoteSearch -> 1
    | SubscribeSearch -> 2)
    
let buf_search buf_query proto buf s = 
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
  buf_int64_2 proto buf s.shared_size;
  buf_int64 buf s.shared_uploaded;
  buf_int buf s.shared_requests;
  if proto >= 10 then
    if proto < 31 then
      (* only return the ed2k hash *)
      let md4 = ref Md4.null in
      List.iter
        (fun s_uid ->
          match Uid.to_uid s_uid with
            | Ed2k hash -> md4 := hash
            | _ -> ()
        ) s.shared_uids;
      buf_md4 buf !md4
    else
      buf_list buf buf_uid s.shared_uids;
  if proto > 36 then
    buf_sub_files proto buf s.shared_sub_files;
  if proto > 40 then
    buf_string buf (magic_string s.shared_magic)

let buf_stat_info proto buf n =
  buf_string buf n.string_long;
  buf_string buf n.string_short;
  buf_int buf n.seen;
  buf_int buf n.banned;
  buf_int buf n.filerequest;
  buf_int64 buf n.download;
  buf_int64 buf n.upload

let buf_stat_info_list proto buf (s,i,l) =
  buf_string buf s;
  buf_int buf i;
  buf_list2 proto buf buf_stat_info l

(***************

       Encoding of messages from the Core to the GUI 

****************)
  
let rec to_gui (proto : int array) buf t =
  try
    let buf_opcode buf opcode =
       if !verbose_gui_decoding then
         lprintf_nl "[gEn] TO_GUI: %d %s" opcode (GuiProto.string_of_to_gui t);
       buf_int16 buf opcode
    in
    match t with
    
    | CoreProtocol (version, max_to_gui, max_from_gui) -> 
        buf_opcode buf 0; 
        buf_int buf version;
        buf_int buf max_to_gui;
        buf_int buf max_from_gui
    
    | Options_info list -> 
        
        buf_opcode buf 1; 
        buf_list buf (fun buf o ->
            let module M = Options in
            buf_string buf o.M.option_name; buf_string buf o.M.option_value
        ) list
    
    | DefineSearches list -> 
        buf_opcode buf 3;
        buf_list buf (fun buf (s, query) ->
            buf_string buf s; buf_query buf query) list
    
    | Result_info r -> buf_opcode buf 4;
        let proto = proto.(4) in
        buf_result proto buf r
    
    | Search_result (n1,n2, _) -> buf_opcode buf 5;
        buf_int buf n1; buf_int buf n2
    
    | Search_waiting (n1,n2) -> buf_opcode buf 6;
        buf_int buf n1; buf_int buf n2
    
    | File_info file_info -> 
        let proto = proto.(52) in
        buf_opcode buf (if proto < 8 then 7 else 
          if proto < 9 then 40 else 
          if proto < 14 then 43 else 52);
        buf_file proto buf file_info
    
    | File_downloaded (n, size, rate, last_seen) -> 
        let proto = proto.(46) in
        buf_opcode buf (if proto < 9 then 8 else 46);
        buf_int buf n; 
        buf_int64_2 proto buf size; 
        buf_float buf rate; 
        if proto > 8 then
          buf_int buf (compute_last_seen last_seen)
    
    | File_add_source (n1,n2) -> buf_opcode buf 10;
        buf_int buf n1; buf_int buf n2
    
    | Server_busy (n1,n2,n3) -> buf_int16 buf 11;
        buf_int buf n1; 
        buf_int64_28 proto.(11) buf n2; 
        buf_int64_28 proto.(11) buf n3
    
    | Server_user  (n1,n2) -> buf_opcode buf 12;
        buf_int buf n1; buf_int buf n2
    
    | Server_state (int,host_state) -> buf_opcode buf 13;
        let proto = proto.(13) in
        buf_int buf int; buf_host_state proto buf host_state
    
    | Server_info s -> 
        let proto = proto.(26) in
        buf_opcode buf (if proto < 2 then 14 else 26);
        buf_server proto buf s
    
    | Client_info client_info -> buf_opcode buf 15;
        let proto = proto.(15) in
        buf_client proto buf client_info
    
    | Client_state (int, host_state) -> buf_opcode buf 16;
        let proto = proto.(16) in
        buf_int buf int; buf_host_state proto buf host_state
    
    | Client_friend (int, client_type) -> buf_opcode buf 17;
        buf_int buf int; buf_client_type buf client_type
    
    | Client_file (n1, s, n2) -> buf_opcode buf 18;
        buf_int buf n1; buf_string buf s; buf_int buf n2
    
    | Console string -> buf_opcode buf 19;
        buf_string buf string
    
    | Network_info network_info -> buf_opcode buf 20;
        let proto = proto.(20) in
        buf_network proto buf network_info
    
    | User_info user_info -> buf_opcode buf 21;
        let proto = proto.(21) in
        buf_user proto buf user_info
    
    | Room_info room_info -> 
        let proto = proto.(31) in
        
        buf_opcode buf (if proto < 3 then 22 else 31);
        buf_room proto buf room_info
    
    | Room_message (int, room_message) -> buf_opcode buf 23;
        buf_int buf int; buf_message buf room_message
    
    | Room_add_user (n1,n2) -> buf_opcode buf 24;
        buf_int buf n1; buf_int buf n2
    
    | MessageFromClient (num, msg) ->
        let protocol = proto.(27) in
        if protocol < 3 then
(* This message was previously send like that ... *)
          
          to_gui proto buf (Room_message (0, PrivateMessage(num, msg)))
        else begin
            buf_opcode buf 27;
            buf_int buf num;
            buf_string buf msg
          end
    
    | BadPassword -> buf_opcode buf 47
    
    | DownloadFiles list ->      
        let proto = proto.(53) in
        buf_opcode buf (if proto < 8 then 29 else 
          if proto < 9 then 41 else
          if proto < 14 then  44 else 53);
        buf_list buf (buf_file proto) list
    
    | DownloadedFiles list ->      
        let proto = proto.(54) in
        buf_opcode buf (if proto < 8 then 30 else 
          if proto < 9 then 42 else 
          if proto < 14 then  45 else 54);
        buf_list buf (buf_file proto) list
    
    | ConnectedServers list ->
        let proto = proto.(28) in
        buf_opcode buf 28;
        buf_list buf (buf_server proto) list
    
    | Client_stats s -> 
        let proto = proto.(49) in
        buf_opcode buf (if proto < 5 then 25 else
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
                  buf_list buf (fun buf (n,ns) ->
                      buf_int buf n;
                      if proto > 17 then 
                        buf_int buf ns
                  ) s.connected_networks;
                end
            end                
    
    | Room_remove_user (room, user) ->
        buf_opcode buf 32;
        buf_int buf room;
        buf_int buf user
    
    | Shared_file_info  shared_info ->       
        let proto = proto.(48) in
        buf_opcode buf (if proto < 10 then 33 else 48);
        buf_shared_info proto buf shared_info
    
    | Shared_file_upload (num, upload,requests) ->
        buf_opcode buf 34;
        buf_int buf num;
        buf_int64 buf upload;
        buf_int buf requests
    | Shared_file_unshared num -> buf_opcode buf 35;
        buf_int buf num
    
    | Add_section_option (section, o) ->
        let proto = proto.(36) in
        let module M = Options in
        buf_opcode buf 36;
        buf_string buf section;
        let desc = if o.M.option_desc = "" 
          then o.M.option_name else o.M.option_desc in
        buf_string buf desc;
        buf_string buf o.M.option_name;
        if proto > 16 then begin
            buf_string buf o.M.option_type;
            buf_string buf o.M.option_help;
            buf_string buf o.M.option_value;
            buf_string buf o.M.option_default;
            buf_bool buf o.M.option_advanced;
          end
        else
          buf_int8 buf (match o.M.option_type with
              "Bool" -> 1
            | "Filename" -> 2
            | _ -> 0);
    
    | Add_plugin_option (section, o) ->
        let proto = proto.(38) in
        let module M = Options in
        buf_opcode buf 38;
        buf_string buf section;
        let desc = if o.M.option_desc = "" 
          then o.M.option_name else o.M.option_desc in
        buf_string buf desc;
        buf_string buf o.M.option_name;
        if proto > 16 then begin
            buf_string buf o.M.option_type;
            buf_string buf o.M.option_help;
            buf_string buf o.M.option_value;
            buf_string buf o.M.option_default;
            buf_bool buf o.M.option_advanced;
          end        
        else
          buf_int8 buf (match o.M.option_type with
              "Bool" -> 1
            | "Filename" -> 2
            | _ -> 0);
    
    | File_remove_source (n1,n2) ->
        buf_opcode buf 50;
        buf_int buf n1; buf_int buf n2
    
    | File_update_availability (file_num, client_num, avail) -> 
        let proto = proto.(9) in
        if proto < 11 then raise UnsupportedGuiMessage;
        buf_opcode buf 9;
        buf_int buf file_num; buf_int buf client_num; buf_string_bin buf avail
    
    | CleanTables (clients, servers) ->      
        let proto = proto.(11) in
        if proto < 11 then raise UnsupportedGuiMessage;
        buf_opcode buf 51;
        buf_list buf (fun buf i -> buf_int buf i) clients;
        buf_list buf (fun buf i -> buf_int buf i) servers
    
    | Uploaders list -> buf_opcode buf 55; buf_list buf buf_int list
    
    | Pending list -> buf_opcode buf 56; buf_list buf buf_int list
    
    | Search s ->
        let proto = proto.(57) in
        buf_opcode buf 57; buf_search buf_string proto buf s
    
    | Version s ->
(*        let proto = proto.(58) in *)
        buf_opcode buf 58; buf_string buf s
    
    | GiftServerAttach _ -> assert false
    | GiftServerStats _ -> assert false

    | Stats (num, l) ->
        let proto = proto.(59) in
        buf_opcode buf 59;
        buf_int buf num;
        buf_list2 proto buf buf_stat_info_list l

  with e ->
      lprintf "GuiEncoding.to_gui: Exception %s\n"
        (Printexc2.to_string e)
      
(***************

     Encoding of messages from the GUI to the Core 

****************)

let rec from_gui (proto : int array) buf t =
  try
    let buf_opcode buf opcode =
       if !verbose_gui_decoding then
         lprintf_nl "[gEn] FROM_GUI: %d %s" opcode (GuiProto.string_of_from_gui t);
       buf_int16 buf opcode
    in
    match t with
    | GuiProtocol int -> buf_opcode buf 0;
        buf_int buf int
    
    | ConnectMore_query -> buf_opcode buf 1
    | CleanOldServers -> buf_opcode buf 2
    | KillServer -> buf_opcode buf 3
    | ExtendedSearch _ -> buf_opcode buf 4
    | Password (login, pass) ->
        let proto = proto.(52) in
        buf_opcode buf (if proto < 14 then 5 else 52);
        buf_string buf pass;
        if proto > 13 then 
          buf_string buf login
    
    | Search_query search -> 
        let proto = proto.(42) in
        if proto < 2 then begin
            buf_opcode buf 6;
            buf_bool buf (search.search_type = LocalSearch); 
          end else begin
            buf_opcode buf 42;          
          end;
        buf_search buf_query proto buf search

    | Download_query (list, int, force) -> 
        let proto = proto.(50) in
        buf_opcode buf (if proto < 14 then 7 else 50);
        buf_list buf buf_string list; 
        buf_int buf int;
        if proto > 13 then
          buf_bool buf force
    
    | Url string -> buf_opcode buf 8;
        buf_string buf string
    | RemoveServer_query int -> buf_opcode buf 9;
        buf_int buf int
    | SaveOptions_query list -> buf_opcode buf 10;
        buf_list buf (fun buf (s1,s2) ->
            buf_string buf s1; buf_string buf s2) list
    
    | RemoveDownload_query  int -> buf_opcode buf 11;
        buf_int buf int
    
    
    | ServerUsers_query  int -> buf_opcode buf 12;
        buf_int buf int
    | SaveFile (int, string) -> buf_opcode buf 13;
        buf_int buf int; buf_string buf string
    | AddClientFriend  int -> buf_opcode buf 14;
        buf_int buf int
    | AddUserFriend  int -> buf_opcode buf 15;
        buf_int buf int
    | RemoveFriend  int -> buf_opcode buf 16;
        buf_int buf int
    | RemoveAllFriends -> buf_opcode buf 17;
    | FindFriend string -> buf_opcode buf 18;
        buf_string buf string
    | ViewUsers  int -> buf_opcode buf 19;
        buf_int buf int
    | ConnectAll  int -> buf_opcode buf 20;
        buf_int buf int
    | ConnectServer  int -> buf_opcode buf 21;
        buf_int buf int
    | DisconnectServer  int -> buf_opcode buf 22;
        buf_int buf int
    | SwitchDownload  (int, bool) -> buf_opcode buf 23;
        buf_int buf int; buf_bool buf bool
    | VerifyAllChunks  int -> buf_opcode buf 24;
        buf_int buf int
    | QueryFormat  int -> buf_opcode buf 25;
        buf_int buf int
    | ModifyMp3Tags (int, tag) -> buf_opcode buf 26;
        buf_int buf int; buf_mp3 buf tag
    | CloseSearch  (int,bool) -> 
        let proto = proto.(53) in
        if proto < 15 then begin
            buf_opcode buf 27;
            buf_int buf int
          end else begin
            buf_opcode buf 53;
            buf_int buf int;         
            buf_bool buf bool;         
          end
    | SetOption (s1, s2) -> buf_opcode buf 28;
        buf_string buf s1; buf_string buf s2
    | Command string -> buf_opcode buf 29;
        buf_string buf string
    | Preview  int -> buf_opcode buf 30;
        buf_int buf int
    | ConnectFriend  int -> buf_opcode buf 31;
        buf_int buf int  
    | GetServer_users  int -> buf_opcode buf 32;
        buf_int buf int
    | GetClient_files  int -> buf_opcode buf 33;
        buf_int buf int
    | GetFile_locations  int -> buf_opcode buf 34;
        buf_int buf int
    | GetServer_info  int -> buf_opcode buf 35;
        buf_int buf int
    | GetClient_info  int -> buf_opcode buf 36;
        buf_int buf int
    | GetFile_info  int -> buf_opcode buf 37;
        buf_int buf int
    | GetUser_info  int -> buf_opcode buf 38;
        buf_int buf int
    | SendMessage (int, room_message) -> buf_opcode buf 39;
        buf_int buf int;
        buf_message buf room_message
    | EnableNetwork (int, bool) -> buf_opcode buf 40;
        buf_int buf int; buf_bool buf bool
    
    | BrowseUser  user ->       buf_opcode buf 41;
        buf_int buf user
    
    | MessageToClient (c,m) ->
        let protocol = proto.(43) in
        if protocol < 3 then
(* On previous GUIs, this was done like that ! *)
          from_gui proto buf (SendMessage (-1, PrivateMessage(c,m)))
        else begin
            buf_opcode buf 43 ;
            buf_int buf c;
            buf_string buf m
          end

(* These messages are not supported by the core with the provided 
protocol version. Do not send them ? *)

(* Introduced with proto 3 *)
    | GetConnectedServers -> buf_opcode buf 44
    | GetDownloadFiles -> buf_opcode buf 45
    | GetDownloadedFiles -> buf_opcode buf 46
    
    | GuiExtensions list -> 
        buf_opcode buf 47;
        buf_list buf (fun buf (ext, bool) ->
            buf_int buf ext;
            buf_int8 buf (if bool then 1 else 0);
        ) list
    
    | SetRoomState (num, state) ->
        buf_opcode buf 48;
        buf_int buf num;
        buf_room_state buf state

(* Introduced with proto 4 *)
    | RefreshUploadStats ->      buf_opcode buf 49

(* Introduced with proto 7 *)
    
    | SetFilePriority (num, prio) ->
        let proto = proto.(51) in
        if proto >= 12 then
          buf_opcode buf 51; buf_int buf num; buf_int buf prio
    
    | AddServer_query (net, ip, port) ->
        buf_opcode buf 54;
        buf_int buf net;
        buf_ip buf ip;
        buf_int16 buf port
    
    | MessageVersions list ->
        buf_opcode buf 55;
        buf_list buf (fun buf (opcode, from_guip, proto) ->
            buf_int16 buf opcode;
            buf_bool buf from_guip;
            buf_int buf proto
        ) list
    
    | RenameFile (num, new_name) ->
        buf_opcode buf 56; buf_int buf num; buf_string buf new_name

    | GetUploaders -> buf_opcode buf 57 
    | GetPending -> buf_opcode buf 58
    | GetSearches -> buf_opcode buf 59
    | GetSearch search_id -> buf_opcode buf 60; buf_int buf search_id
    | ConnectClient c -> buf_opcode buf 61; buf_int buf c
    | DisconnectClient c -> buf_opcode buf 62; buf_int buf c
    | NetworkMessage (n, s) -> buf_opcode buf 63; buf_int buf n; buf_string buf s
    | GiftAttach _ -> assert false
    | GiftStats -> assert false
        
    | InterestedInSources interested ->
        buf_int16 buf 64; buf_bool buf interested
    | GetVersion -> buf_opcode buf 65; 

    (* introduced with protocol 32 *)
    | ServerRename (num, s) ->
        buf_opcode buf 66;
        buf_int buf num;
        buf_string buf s
    | ServerSetPreferred (num, preferred) ->
        buf_opcode buf 67;
        buf_int buf num;
        buf_bool buf preferred
    | GetStats n ->
        buf_opcode buf 68;
        buf_int buf n

  with e ->
      lprintf "GuiEncoding.from_gui: Exception %s\n"
        (Printexc2.to_string e)
        
        
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
      if (v <> msg) then begin
          AnyEndian.dump s;
          let buf = Buffer.create 1000 in
          encoder buf msg;
          let s2 = Buffer.contents buf in
          AnyEndian.dump s2;
          if s = s2 then
            (lprintf "s = s2\n"; false)
          else
          let len = String.length s in
          let len2 = String.length s2 in
          if len <> len2 then
            (lprintf "different lengths\n"; false)
          else
            (for i = 0 to len-1 do
                if s.[i] <> s2.[i] then
                  lprintf "diff at pos %d(%d)\n" i
                    (int_of_char s.[i]);
              done;
              false)
        end else
        true
    
    with e ->
        lprintf "Exception %s in check\n" (Printexc2.to_string e);
        
        false
  in
(* and    server_info = {
      server_num = 1;
} *)
  
  for best_gui_version = best_gui_version downto 24 do
(*     lprintf "best_gui_version: %d\n" best_gui_version; *)
    let proto = Array.make (to_gui_last_opcode+1) best_gui_version in
    let to_gui = to_gui proto in
    let check_to_gui = 
      check to_gui (GuiDecoding.to_gui proto) in
    assert (check_to_gui (MessageFromClient (32, "Hello")));
(*  assert (check_to_gui (File_info file_info_test));  *)
    assert (check_to_gui (DownloadFiles [file_info_test]));
    assert (check_to_gui (DownloadedFiles [file_info_test]));  
    assert (check_to_gui (ConnectedServers []));    
    assert (check_to_gui (Room_remove_user (5,6)));
    assert (check_to_gui (Shared_file_upload (1, Int64.zero, 32)));
    assert (check_to_gui (Shared_file_unshared 2));
(* Shared_file_info ??? *)
(*
  assert (check_to_gui (Add_section_option ("section", "message", "option", StringEntry )));
  assert (check_to_gui (Add_plugin_option ("section", "message", "option", StringEntry )));
*)  
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
  done
  
