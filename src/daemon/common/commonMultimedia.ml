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
open CommonTypes

let input_int8 ic =
  int_of_char (input_char ic)
  
let input_int16 ic =
  let i0 = input_int8  ic in
  let i1 = input_int8 ic in
  i0 lor (i1 lsl 8)

let input_int32 ic =
  let i0 = input_int16 ic in
  let i1 = input_int16 ic in
  let i0 = Int64.of_int i0 in
  let i1 = Int64.of_int i1 in
  or64 i0 (left64 i1 16)
  
let input_int ic =
  let i0 = input_int16 ic in
  let i1 = input_int16 ic in
  i0 lor (i1 lsl 16)

let input_string4 ic = really_input_string ic 4

let print_string4 v s =
  lprintf "%s :" v;
  for i = 0 to 3 do
    let c = s.[i] in
    let int = int_of_char c in
    if int > 31 && int < 127 then
      lprint_char c
    else lprintf "[%d]" int
  done;
  lprint_newline ()

let print_int32 s i=
  lprintf_nl "%s: %Ld" s i

let print_int16 s i=
  lprintf_nl "%s: %d" s i

(**********************************************************************************)
(*                                                                                *)
(*                  Variables                                                     *)
(*                                                                                *)
(**********************************************************************************)

let sizeof_old_ogm_packet = 48

(**********************************************************************************)
(*                                                                                *)
(*                  read16                                                        *)
(*                                                                                *)
(**********************************************************************************)

let read16 s =
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let s' = Printf.sprintf "0x%02X%02X" b a in
  int_of_string s'

(**********************************************************************************)
(*                                                                                *)
(*                  read24                                                        *)
(*                                                                                *)
(**********************************************************************************)

let read24 s = 
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let c = int_of_char s.[2] in
  let s' = Printf.sprintf "0x%02X%02X%02X" c b a in
  int_of_string s'

(**********************************************************************************)
(*                                                                                *)
(*                  read32                                                        *)
(*                                                                                *)
(**********************************************************************************)

let read32 s = 
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let c = int_of_char s.[2] in
  let d = int_of_char s.[3] in
  let s' = Printf.sprintf "0x%02X%02X%02X%02X" d c b a in
  Int64.to_float (Int64.of_string s')

(**********************************************************************************)
(*                                                                                *)
(*                  read64                                                        *)
(*                                                                                *)
(**********************************************************************************)

let read64 s =
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let c = int_of_char s.[2] in
  let d = int_of_char s.[3] in
  let e = int_of_char s.[4] in
  let f = int_of_char s.[5] in
  let g = int_of_char s.[6] in
  let h = int_of_char s.[7] in
  let s' = Printf.sprintf "0x%02X%02X%02X%02X%02X%02X%02X%02X" h g f e d c b a in
  Int64.to_float (Int64.of_string s')

(**********************************************************************************)
(*                                                                                *)
(*                  read16B                                                       *)
(*                                                                                *)
(**********************************************************************************)

let read16B s =
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let s' = Printf.sprintf "0x%02X%02X" a b in
  int_of_string s'

(**********************************************************************************)
(*                                                                                *)
(*                  read24B                                                       *)
(*                                                                                *)
(**********************************************************************************)

let read24B s = 
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let c = int_of_char s.[2] in
  let s' = Printf.sprintf "0x%02X%02X%02X" a b c in
  int_of_string s'

(**********************************************************************************)
(*                                                                                *)
(*                  read32B                                                       *)
(*                                                                                *)
(**********************************************************************************)

let read32B s = 
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let c = int_of_char s.[2] in
  let d = int_of_char s.[3] in
  let s' = Printf.sprintf "0x%02X%02X%02X%02X" a b c d in
  Int64.to_float (Int64.of_string s')

(**********************************************************************************)
(*                                                                                *)
(*                  read64B                                                       *)
(*                                                                                *)
(**********************************************************************************)

let read64B s =
  let a = int_of_char s.[0] in
  let b = int_of_char s.[1] in
  let c = int_of_char s.[2] in
  let d = int_of_char s.[3] in
  let e = int_of_char s.[4] in
  let f = int_of_char s.[5] in
  let g = int_of_char s.[6] in
  let h = int_of_char s.[7] in
  let s' = Printf.sprintf "0x%02X%02X%02X%02X%02X%02X%02X%02X" a b c d e f g h in
  Int64.to_float (Int64.of_string s')

(**********************************************************************************)
(*                                                                                *)
(*                  get_audio_codec                                               *)
(*                                                                                *)
(**********************************************************************************)

let get_audio_codec s =
  let str = ref "" in
  String.iter (fun c ->
    if c <> '\000'
      then str := Printf.sprintf "%s%c" !str c
  ) s;
  try
    let n = int_of_string !str in
    match n with
      1 -> "pcm"
    | 55 -> "mp3"
    | 2000 -> "ac3"
    | _ -> Printf.sprintf "[%s]" s
  with _ -> Printf.sprintf "[%s]" s

(**********************************************************************************)
(*                                                                                *)
(*                  get_theora_cs                                                 *)
(*                                                                                *)
(**********************************************************************************)

let get_theora_cs n =
  match n with
      1 -> CSRec470M
    | 2 -> CSRec470BG
    | _ -> CSUndefined

(**********************************************************************************)
(*                                                                                *)
(*                  page_seek                                                     *)
(*                                                                                *)
(**********************************************************************************)

let rec page_seek ic s pos =
  if (pos_in ic - pos) > 255
    then failwith "No more OGG Stream Header"
    else begin
      really_input ic s 0 4;
      seek_in ic (pos_in ic - 3);
      if Misc.bytes_equal_string s "OggS"
        then seek_in ic (pos_in ic + 3)
        else page_seek ic s pos
  end

(**********************************************************************************)
(*                                                                                *)
(*                  normalize_stream_type                                         *)
(*                                                                                *)
(**********************************************************************************)

let normalize_stream_type s ct =
  let s = String.sub s 0 6 in
  if s = "vorbis" && ct = 0x1
    then OGG_VORBIS_STREAM
    else if s = "theora" && ct = 0x80
      then OGG_THEORA_STREAM
      else begin
        let s = (String.sub s 0 5) in
        if s = "video" && ct = 0x1
          then OGG_VIDEO_STREAM
          else if s = "audio" && ct = 0x1
            then OGG_AUDIO_STREAM
            else if s = "index" && ct = 0x1
              then OGG_INDEX_STREAM
              else if (String.sub s 0 4) = "text" && ct = 0x1
                then OGG_TEXT_STREAM
                else failwith "No more OGG Stream Header"
      end

(**********************************************************************************)
(*                                                                                *)
(*                  next_ogg_stream                                               *)
(*                                                                                *)
(**********************************************************************************)

let rec next_ogg_stream ic ogg_infos str stream_number =
  let pos = pos_in ic in
  page_seek ic str pos;
  let pos = pos_in ic in
(*
  let serial_number = String.create 4 in
  seek_in ic (pos+10);
  really_input ic serial_number 0 4;
  let stream_number = read32 serial_number in
  lprintf "Stream Serial Number: %0.f\n" stream_number;
*)
  seek_in ic (pos+24);
  let content_type = really_input_string ic 1 in
  let content_type = int_of_char content_type.[0] in
  seek_in ic (pos+25);
  let stream_type = really_input_string ic 8 in
  let stream_type = normalize_stream_type stream_type content_type in
  incr stream_number;
  let pos = pos_in ic in
  page_seek ic str pos;
  let sizeof_packet = pos_in ic - pos - 4 in (* remove 4 bytes for 'OggS' *)
  seek_in ic pos;
  match stream_type with
      OGG_VIDEO_STREAM -> get_ogg_video_info ic ogg_infos str sizeof_packet stream_number
    | OGG_AUDIO_STREAM -> get_ogg_audio_info ic ogg_infos str sizeof_packet stream_number
    | OGG_INDEX_STREAM -> get_ogg_index_info ic ogg_infos str stream_number
    | OGG_TEXT_STREAM -> get_ogg_text_info ic ogg_infos str stream_number
    | OGG_VORBIS_STREAM -> get_ogg_vorbis_info ic ogg_infos str stream_number
    | OGG_THEORA_STREAM -> get_ogg_theora_info ic ogg_infos str stream_number

and get_ogg_video_info  ic ogg_infos str sizeof_packet stream_number =
  let s = really_input_string ic sizeof_packet in
  let codec = String.lowercase (String.sub s 0 4) in
  let time_unit = read64 (String.sub s 8 8) in
  let video_width =
    if sizeof_packet >= sizeof_old_ogm_packet
      then read32 (String.sub s 36 4)
      else read32 (String.sub s 34 4)
  in
  let video_height =
    if sizeof_packet >= sizeof_old_ogm_packet
      then read32 (String.sub s 40 4)
      else read32 (String.sub s 38 4)
  in
  let sample_rate = 10000000. /. time_unit in
  ogg_infos := {
    stream_no   = !stream_number;
    stream_type = OGG_VIDEO_STREAM;
    stream_tags = [
      Ogg_codec codec;
      Ogg_video_sample_rate sample_rate;
      Ogg_video_width video_width;
      Ogg_video_height video_height;];
  } :: !ogg_infos;
  next_ogg_stream ic ogg_infos str stream_number

and get_ogg_audio_info  ic ogg_infos str sizeof_packet stream_number =
  let s = really_input_string ic sizeof_packet in
  let codec = get_audio_codec (String.sub s 0 4) in
  let sample_per_unit = read64 (String.sub s 16 8) in
  let channels =
    if sizeof_packet >= sizeof_old_ogm_packet
      then read16 (String.sub s 36 2)
      else read16 (String.sub s 34 2)
  in
  let blockalign =
    if sizeof_packet >= sizeof_old_ogm_packet
      then read16 (String.sub s 38 2)
      else read16 (String.sub s 36 2)
  in
  let avgbytespersec =
    if sizeof_packet >= sizeof_old_ogm_packet
      then read32 (String.sub s 40 4)
      else read32 (String.sub s 38 4)
  in
  ogg_infos := {
    stream_no   = !stream_number;
    stream_type = OGG_AUDIO_STREAM;
    stream_tags = [
      Ogg_codec codec;
      Ogg_audio_channels channels;
      Ogg_audio_sample_rate sample_per_unit;
      Ogg_audio_blockalign blockalign;
      Ogg_audio_avgbytespersec avgbytespersec;];
  } :: !ogg_infos;
  next_ogg_stream ic ogg_infos str stream_number

and get_ogg_vorbis_info  ic ogg_infos str stream_number =
  seek_in ic (pos_in ic - 2); (* ogm sets 8 octets in the common header as vorbis uses 6 octects for 'vorbis' *)
  let s = really_input_string ic 22 in
  let version = read32 (String.sub s 0 4) in
  let audio_channels = int_of_char s.[4] in
  let sample_rate = read32 (String.sub s 5 4) in
  let br_max = read32 (String.sub s 9 4) in
  let br_nom = read32 (String.sub s 13 4) in
  let br_min = read32 (String.sub s 17 4) in
  let blocksize_1 = ((int_of_char s.[21]) asr 4) land 15 in
  let blocksize_0 = (int_of_char s.[21]) land 15 in
  let l = ref [] in
  (if br_max > 0. then l := (Maximum_br br_max) :: !l);
  (if br_nom > 0. then l := (Nominal_br br_nom) :: !l);
  (if br_min > 0. then l := (Minimum_br br_min) :: !l);
  ogg_infos := {
    stream_no   = !stream_number;
    stream_type = OGG_VORBIS_STREAM;
    stream_tags = [
      Ogg_codec "vorbis";
      Ogg_vorbis_version version;
      Ogg_audio_channels audio_channels;
      Ogg_vorbis_sample_rate sample_rate;
      Ogg_vorbis_bitrates !l;
      Ogg_vorbis_blocksize_0 blocksize_0;
      Ogg_vorbis_blocksize_1 blocksize_1;];
  } :: !ogg_infos;
  next_ogg_stream ic ogg_infos str stream_number

and get_ogg_theora_info  ic ogg_infos str stream_number =
  seek_in ic (pos_in ic - 2); (* ogm sets 8 octets in the common header as theora uses 6 octects for 'theora' *)
  let s = really_input_string ic 34 in
  let vmaj = int_of_char s.[0] in
  let vmin = int_of_char s.[1] in
  let vrev = int_of_char s.[2] in
  let codec = Printf.sprintf "theora-%d.%d.%d" vmaj vmin vrev in
  (* multiply by 16 to get the actual frame width in pixels *)
  (* multiply by 16 to get the actual frame height in pixels *)
  let picw = read24B (String.sub s 7 3)  in
  let pich = read24B (String.sub s 10 3) in
  let frn = read32B (String.sub s 15 4) in
  let frd = read32B (String.sub s 19 4) in
  let sample_rate = frn /. frd in
  let parn = read24B (String.sub s 23 3) in
  let pard = read24B (String.sub s 26 3) in
  let parn, pard =
    if parn = 0
      then (1, 1)
      else (parn, pard)
  in
  let cs = int_of_char s.[29] in
  let nombr = read24B (String.sub s 30 3) in
  let qual = (int_of_char s.[33] asr 2) land 63 in
  ogg_infos := {
    stream_no   = !stream_number;
    stream_type = OGG_THEORA_STREAM;
    stream_tags = [
      Ogg_codec codec;
      Ogg_video_sample_rate sample_rate;
      Ogg_video_width (float_of_int picw);
      Ogg_video_height (float_of_int pich);
      Ogg_aspect_ratio (float_of_int pard /. float_of_int parn);
      Ogg_theora_cs (get_theora_cs cs);
      Ogg_theora_quality qual] @
      (if nombr > 0 then [Ogg_theora_avgbytespersec nombr] else []);
  } :: !ogg_infos;
  next_ogg_stream ic ogg_infos str stream_number

and get_ogg_text_info ic ogg_infos str stream_number =
  ogg_infos := {
    stream_no   = !stream_number;
    stream_type = OGG_TEXT_STREAM;
    stream_tags = [Ogg_has_subtitle];
  } :: !ogg_infos;
  next_ogg_stream ic ogg_infos str stream_number

and get_ogg_index_info ic ogg_infos str stream_number =
  ogg_infos := {
    stream_no   = !stream_number;
    stream_type = OGG_INDEX_STREAM;
    stream_tags = [Ogg_has_index];
  } :: !ogg_infos;
  next_ogg_stream ic ogg_infos str stream_number

(**********************************************************************************)
(*                                                                                *)
(*                  search_info_ogg                                               *)
(*                                                                                *)
(**********************************************************************************)

let search_info_ogg ic =
    let stream_number = ref 0 in
    let str = Bytes.create 4 in
    let ogg_infos = ref [] in
    (* make sure the current reading position is at the file beginning *)
    seek_in ic 0;
    try
       next_ogg_stream ic ogg_infos str stream_number;
    with _ ->
      begin
        match !ogg_infos with
            [] -> ()
          | _ -> raise (FormatFound (OGG (List.rev !ogg_infos)))
      end

(**********************************************************************************)
(*                                                                                *)
(*                  search_info_avi                                               *)
(*                                                                                *)
(**********************************************************************************)

let search_info_avi ic =
  try
(* pos: 0 *)
    let s = input_string4 ic in
    if s <> "RIFF" then failwith "Not an AVI file (RIFF absent)";

(* pos: 4 *)
    let size = input_int32 ic in
(*
  lprintf "SIZE %s\n" (Int64.to_string size);
*)

(* pos: 8 *)
    let s = input_string4 ic in
    if s <> "AVI " then failwith  "Not an AVI file (AVI absent)";

(* pos: 12 *)
    let s = input_string4 ic in
    if s <> "LIST" then failwith  "Not an AVI file (LIST absent)";

(* position 16 *)
    let rec iter_list pos end_pos =
(*
  lprintf "POS %s/%s\n" (Int64.to_string pos) (Int64.to_string end_pos);
  *)
      if pos < end_pos then begin
(* on peut s'arreter quand size = 0 *)
          seek_in ic (Int64.to_int pos);
          let size2 = input_int32 ic in
(*
        lprintf "SIZE2 %s\n" (Int64.to_string size2);
*)
          
          if size2 = Int64.zero then raise Not_found;
          let header_name = input_string4 ic in
(*        lprint_string4 "header\n" header_name;  *)
(* pos: pos + 8 *)       
          begin
            match header_name with
              "hdrl" ->
(*              lprintf "HEADER\n";  *)
                
                let s = input_string4 ic in
                if s <> "avih" then failwith "Bad AVI file (avih absent)";

(* pos: pos + 12 *)
                let main_header_len = 52 in             
                
                ignore (input_string4 ic);
                
                seek_in ic ((Int64.to_int pos) + main_header_len + 20);
                let pos_in = 
                  pos ++ Int64.of_int (main_header_len + 24) in
                let last_pos = pos_in ++ size2 in
                iter_list pos_in last_pos
            
            
            | "movi" ->
(*              lprintf "CHUNKS\n";  *)
                ()
            
            | "strl" ->
(*              lprintf "STREAM DESCRIPTION\n";  *)
                
                let offset = 4L  in
                let pos0 = pos ++ offset in
                let end_pos0 = pos ++ size2 in
                iter_list pos0 end_pos0
            
            | "strh" ->
(*              lprintf "STREAM HEADER\n";  *)
                
                ignore (input_string4 ic);
                
                let fccType = input_string4 ic in
                let fccHandler = input_string4 ic in
                let _dwFlags = input_int32 ic in (* Contains AVITF_* flags *)
                let _wPriority = input_int16 ic in
                let _wLanguage = input_int16 ic in
                let _dwInitialFrames = input_int32 ic in
                let dwScale = input_int32 ic in
                let dwRate = input_int32 ic in (* dwRate / dwScale == samples/second *)
                let _dwStart = input_int32 ic in
                let dwLength = input_int32 ic in
                let _dwSuggestedBufferSize = input_int32 ic in
                let _dwQuality = input_int32 ic in
                let _dwSampleSize = input_int32 ic in
                let _rcFrame_x = input_int16 ic in
                let _rcFrame_y = input_int16 ic in
                let rcFrame_dx = input_int16 ic in
                let rcFrame_dy = input_int16 ic in
                
                if fccType = "vids" then                
                  raise (FormatFound (AVI {
                        avi_codec = fccHandler;
                        avi_width = rcFrame_dx;
                        avi_height = rcFrame_dy;
                        avi_fps = int_of_float(1000.0 *. Int64.to_float(dwRate) /. Int64.to_float(dwScale));
                        avi_rate = Int64.to_int dwLength;
                      }));
                
                
                
(*
                print_string4 "fccType" fccType;
                print_string4 "fccHandler" fccHandler;
                print_int32 "dwFlags " dwFlags; (* Contains AVITF_* flags *)
                print_int16 "wPriority" wPriority;
                print_int16 "wLanguage" wLanguage;
                print_int32 "dwInitialFrames " dwInitialFrames;
                print_int32 "dwScale " dwScale;
                print_int32 "dwRate " dwRate; (* dwRate / dwScale == samples/second *)
                print_int32 "dwStart " dwStart;
                print_int32 "dwLength " dwLength;
                print_int32 "dwSuggestedBufferSize " dwSuggestedBufferSize;
                print_int32 "dwQuality " dwQuality;
                print_int32 "dwSampleSize " dwSampleSize;
                print_int16 "rcFrame_x" rcFrame_x;
                print_int16 "rcFrame_y" rcFrame_y;
                print_int16 "rcFrame_dx" rcFrame_dx;
                print_int16 "rcFrame_dy" rcFrame_dy;
*)
                ()
            | _ -> ()
          end;
          
          iter_list (pos ++ size2 ++ 8L) end_pos
        end 
    
    in
    let pos0 = 16L in
    iter_list pos0 (pos0 ++ size);
(*  lprintf "DONE\n";  *)
    ()
  with
  | FormatFound f as e -> raise e
  | _ -> ()

let search_info_mp3 filename =
  try
    let tag = Mp3tag.Id3v1.read filename in
    let info = Mp3tag.info filename in
    (* lprintf "MP3 INFO FOUND\n"; *)
    raise (FormatFound (MP3 (tag, info)))
  with
  | FormatFound _ as e -> raise e
  | Not_found -> () (* The file couldn't be found *)
  | x -> ()
      (*
      lprintf "error while looking for mp3file %s: %s\n" filename
        (Printexc2.to_string x); 
      *)

let get_info file =
  let file =
    if Autoconf.windows then
      Charset.Locale.to_locale (file)
    else
      file
  in
  try
    Unix2.tryopen_read_bin file (fun ic ->
      search_info_mp3 file;
      search_info_avi ic ;
      search_info_ogg ic);
    let es = 
      try 
        List.map String.lowercase (Filename2.extensions file) 
      with _ -> []
    in
    match List.rev es with
    | "exe" :: _ -> FormatType ("exe", "Pro")
    | "ace" :: _ -> FormatType ("ace", "Pro")
    | "rar" :: _ -> FormatType ("rar", "Pro")
    | "gz"  :: _ -> FormatType ("gzip", "Pro")
    | "z"   :: _ -> FormatType ("compress", "Pro")
    | "tar" :: _ -> FormatType ("tar", "Pro")
    | "bz2" :: _ -> FormatType ("bzip2", "Pro")
    | "gif" :: _ -> FormatType ("gif", "Image")
    | "jpg" :: _ -> FormatType ("jpeg", "Image")
    | "jpeg" :: _ -> FormatType ("jpeg", "Image")
    | "txt" :: _ -> FormatType ("txt", "Doc")
    | "doc" :: _ -> FormatType ("doc", "Doc")
    | "wav" :: _ -> FormatType ("wav", "Audio")
    | "mpg" :: _ -> FormatType ("mpg", "Video")
    | "mpeg" :: _ -> FormatType ("mpg", "Video")
    | "mov" :: _ -> FormatType ("mov", "Video")
    | "ogm" :: _ -> FormatType ("ogm", "Video")
    | "asf" :: _ -> FormatType ("asf", "Video")
    | _ -> FormatUnknown
  with 
    | FormatFound f -> f
    | e -> 
        lprintf_nl "get_info: Exception in %s" (Printexc2.to_string e);
        FormatUnknown
          
          
module Bchunk =
  struct
    
(*

 /*
  *	binchunker for Unix
  *	Copyright (C) 1998,2001  Heikki Hannikainen <hessu@hes.iki.fi>
  *
  *  This program is free software; you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation; either version 2 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program; if not, write to the Free Software
  *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#define VERSION "1.1.1"
#define USAGE "Usage: bchunk [-v] [-p (PSX)] [-w (wav)] [-s (swabaudio)]\n" \
        "         <image.bin> <image.cue> <basename>\n" \
        "Example: bchunk foo.bin foo.cue foo\n" \
        "  -v  Verbose mode\n" \
        "  -p  PSX mode: truncate MODE2/2352 to 2336 bytes instead of normal 2048\n" \
        "  -w  Output audio files in WAV format\n" \
        "  -s  swabaudio: swap byte order in audio tracks\n"
        
#define VERSTR	"binchunker for Unix, version " VERSION " by Heikki Hannikainen <hessu@hes.iki.fi>\n" \
                "\tCreated with the kind help of Bob Marietta <marietrg@SLU.EDU>,\n" \
                "\tpartly based on his Pascal (Delphi) implementation.\n" \
                "\tSupport for MODE2/2352 ISO tracks thanks to input from\n" \
                "\tGodmar Back <gback@cs.utah.edu> and Colas Nahaboo <Colas@Nahaboo.com>\n" \
                "\tReleased under the GNU GPL, version 2 or later (at your option).\n\n"

#define CUELLEN 1024
#define SECTLEN 2352

#define WAV_RIFF_HLEN 12
#define WAV_FORMAT_HLEN 24
#define WAV_DATA_HLEN 8
#define WAV_HEADER_LEN WAV_RIFF_HLEN + WAV_FORMAT_HLEN + WAV_DATA_HLEN

/*
 *	Ugly way to convert integers to little-endian format.
 *	First let netinet's hton() functions decide if swapping should
 *	be done, then convert back.
 */

#include <inttypes.h>
#include <netinet/in.h>

#define bswap_16(x) \
     ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8))
#define bswap_32(x) \
     ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |  \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))

#define htoles(x) bswap_16(htons(x))
#define htolel(x) bswap_32(htonl(x))



struct track_t {
        int num;
        int mode;
        int audio;
        char *modes;
        char *extension;
        int bstart;
        int bsize;
        long startsect;
        long stopsect;
        long start;
        long stop;
        struct track_t *next;
};

char *basefile = NULL;
char *binfile = NULL;
char *cuefile = NULL;
int verbose = 0;
int psxtruncate = 0;
int swabaudio = 0;
int towav = 0;

/*
 *	Parse arguments
 */

void parse_args(int argc, char *argv[])
{
        int s;
        
        while ((s = getopt(argc, argv, "swvp?h")) != -1) {
                switch (s) {
                        case 'v':
                                verbose = 1;
                                break;
                        case 'w':
                                towav = 1;
                                break;
                        case 'p':
                                psxtruncate = 1;
                                break;
                        case 's':
                                swabaudio = 1;
                                break;
                        case '?':
                        case 'h':
                                fprintf(stderr, "%s", USAGE);
                                exit(0);
                }
        }

        if (argc - optind != 3) {
                fprintf(stderr, "%s", USAGE);
                exit(1);
        }
        
        while (optind < argc) {
                switch (argc - optind) {
                        case 3:
                                binfile = strdup(argv[optind]);
                                break;
                        case 2:
                                cuefile = strdup(argv[optind]);
                                break;
                        case 1:
                                basefile = strdup(argv[optind]);
                                break;
                        default:
                                fprintf(stderr, "%s", USAGE);
                                exit(1);
                }
                optind++;
        }
}

/*
 *	Convert a mins:secs:frames format to plain frames
 */

long time2frames(char *s)
{
        int mins = 0, secs = 0, frames = 0;
        char *p, *t;
        
        if (!(p = strchr(s, ':')))
                return -1;
        *p = '\0';
        mins = atoi(s);
        
        p++;
        if (!(t = strchr(p, ':')))
                return -1;
        *t = '\0';
        secs = atoi(p);
        
        t++;
        frames = atoi(t);
        
        return 75 * (mins * 60 + secs) + frames;
}

/*
 *	Parse the mode string
 */

void gettrackmode(struct track_t *track, char *modes)
{
        static char ext_iso[] = "iso";
        static char ext_cdr[] = "cdr";
        static char ext_wav[] = "wav";
        static char ext_ugh[] = "ugh";
        
        track->audio = 0;
        
        if (!strcasecmp(modes, "MODE1/2352")) {
                track->bstart = 16;
                track->bsize = 2048;
                track->extension = ext_iso;
                
        } else if (!strcasecmp(modes, "MODE2/2352")) {
                track->extension = ext_iso;
                if (psxtruncate) {
                        /* PSX: truncate from 2352 to 2336 byte tracks */
                        track->bstart = 0;
                        track->bsize = 2336;
                } else {
                        /* Normal MODE2/2352 */
                        track->bstart = 24;
                        track->bsize = 2048;
                }
                
        } else if (!strcasecmp(modes, "MODE2/2336")) {
                /* WAS 2352 in V1.361B still work?
                 * what if MODE2/2336 single track bin, still 2352 sectors?
                 */
                track->bstart = 16;
                track->bsize = 2336;
                track->extension = ext_iso;
                
        } else if (!strcasecmp(modes, "AUDIO")) {
                track->bstart = 0;
                track->bsize = 2352;
                track->audio = 1;
                if (towav)
                        track->extension = ext_wav;
                else
                        track->extension = ext_cdr;
        } else {
                printf("(?) ");
                track->bstart = 0;
                track->bsize = 2352;
                track->extension = ext_ugh;
        }
}

/*
 *	return a progress bar
 */

char *progressbar(float f, int l)
{
        static char s[80];
        int i, n;
        
        n = l * f;
        for (i = 0; i < n; i++) {
                s[i] = '*';
        }
        for (; i < l; i++) {
                s[i] = ' ';
        }
        s[i] = '\0';
        
        return s;
}

/*
 *	Write a track
 */

int writetrack(FILE *bf, struct track_t *track, char *bname)
{
        char *fname;
        FILE *f;
        char buf[SECTLEN+10];
        long sz, sect, realsz, reallen;
        char c, *p, *p2, *ep;
        int32_t l;
        int16_t i;
        float fl;
        
        if (!(fname = malloc(strlen(bname) + 8))) {
                fprintf(stderr, "main(): malloc() failed, out of memory\n");
                exit(4);
        }
        sprintf(fname, "%s%2.2d.%s", bname, track->num, track->extension);
        
        printf("%2d: %s ", track->num, fname);
        
        if (!(f = fopen(fname, "w"))) {
                fprintf(stderr, " Could not fopen track file: %s\n", strerror(errno));
                exit(4);
        }
        
        if (fseek(bf, track->start, SEEK_SET)) {
                fprintf(stderr, " Could not fseek to track location: %s\n", strerror(errno));
                exit(4);
        }
        
        reallen = (track->stopsect - track->startsect + 1) * track->bsize;
        if (verbose) {
                printf("\n mmc sectors %ld->%ld (%ld)", track->startsect, track->stopsect, track->stopsect - track->startsect + 1);
                printf("\n mmc bytes %ld->%ld (%ld)", track->start, track->stop, track->stop - track->start + 1);
                printf("\n sector data at %d, %d bytes per sector", track->bstart, track->bsize);
                printf("\n real data %ld bytes", (track->stopsect - track->startsect + 1) * track->bsize);
                printf("\n");
        }

        printf("                                          ");
        
        if ((track->audio) && (towav)) {
                // RIFF header
                fputs("RIFF", f);
                l = htolel(reallen + WAV_DATA_HLEN + WAV_FORMAT_HLEN + 4);
                fwrite(&l, 4, 1, f);  // length of file, starting from WAVE
                fputs("WAVE", f);
                // FORMAT header
                fputs("fmt ", f);
                l = htolel(0x10);     // length of FORMAT header
                fwrite(&l, 4, 1, f);
                i = htoles(0x01);     // constant
                fwrite(&i, 2, 1, f);
                i = htoles(0x02);	// channels
                fwrite(&i, 2, 1, f);
                l = htolel(44100);	// sample rate
                fwrite(&l, 4, 1, f);
                l = htolel(44100 * 4);	// bytes per second
                fwrite(&l, 4, 1, f);
                i = htoles(4);		// bytes per sample
                fwrite(&i, 2, 1, f);
                i = htoles(2*8);	// bits per channel
                fwrite(&i, 2, 1, f);
                // DATA header
                fputs("data", f);
                l = htolel(reallen);
                fwrite(&l, 4, 1, f);
        }
        
        realsz = 0;
        sz = track->start;
        sect = track->startsect;
        fl = 0;
        while ((sect <= track->stopsect) && (fread(buf, SECTLEN, 1, bf) > 0)) {
                if (track->audio) {
                        if (swabaudio) {
                                /* swap low and high bytes */
                                p = &buf[track->bstart];
                                ep = p + track->bsize;
                                while (p < ep) {
                                        p2 = p + 1;
                                        c = *p;
                                        *p = *p2;
                                        *p2 = c;
                                        p += 2;
                                }
                        }
                }
                if (fwrite(&buf[track->bstart], track->bsize, 1, f) < 1) {
                        fprintf(stderr, " Could not write to track: %s\n", strerror(errno));
                        exit(4);
                }
                sect++;
                sz += SECTLEN;
                realsz += track->bsize;
                if (((sz / SECTLEN) % 500) == 0) {
                        fl = (float)realsz / (float)reallen;
                        printf("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b%4ld/%-4ld MB  [%s] %3.0f %%", realsz/1024/1024, reallen/1024/1024, progressbar(fl, 20), fl * 100);
                        fflush(stdout);
                }
        }
        
        fl = (float)realsz / (float)reallen;
        printf("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b%4ld/%-4ld MB  [%s] %3.0f %%", realsz/1024/1024, reallen/1024/1024, progressbar(1, 20), fl * 100);
        fflush(stdout);
        
        if (ferror(bf)) {
                fprintf(stderr, " Could not read from %s: %s\n", binfile, strerror(errno));
                exit(4);
        }
        
        if (fclose(f)) {
                fprintf(stderr, " Could not fclose track file: %s\n", strerror(errno));
                exit(4);
        }
        
        printf("\n");
        return 0;
}

/*
 *	Main
 */

int main(int argc, char **argv)
{
        char s[CUELLEN+1];
        char *p, *t;
        int i, idx;
        struct track_t *tracks = NULL;
        struct track_t *track = NULL;
        struct track_t *prevtrack = NULL;
        struct track_t **prevp = &tracks;
        
        FILE *binf, *cuef;
        
        printf("%s", VERSTR);
        
        parse_args(argc, argv);
        
        if (!((binf = fopen(binfile, "r")))) {
                fprintf(stderr, "Could not open BIN %s: %s\n", binfile, strerror(errno));
                return 2;
        }
        
        if (!((cuef = fopen(cuefile, "r")))) {
                fprintf(stderr, "Could not open CUE %s: %s\n", cuefile, strerror(errno));
                return 2;
        }
        
        printf("Reading the CUE file:\n");
        
        /* We don't really care about the first line. */
        if (!fgets(s, CUELLEN, cuef)) {
                fprintf(stderr, "Could not read first line from %s: %s\n", cuefile, strerror(errno));
                return 3;
        }
        
        i = 0;
        while (fgets(s, CUELLEN, cuef)) {
                while ((p = strchr(s, '\r')) || (p = strchr(s, '\n')))
                        *p = '\0';
                        
                if ((p = strstr(s, "TRACK"))) {
                        printf("\nTrack ");
                        if (!(p = strchr(p, ' '))) {
                                fprintf(stderr, "... ouch, no space after TRACK.\n");
                                continue;
                        }
                        p++;
                        if (!(t = strchr(p, ' '))) {
                                fprintf(stderr, "... ouch, no space after track number.\n");
                                continue;
                        }
                        *t = '\0';
                        
                        prevtrack = track;
                        if (!(track = malloc(sizeof(struct track_t)))) {
                                fprintf(stderr, "main(): malloc() failed, out of memory\n");
                                exit(4);
                        }
                        *prevp = track;
                        prevp = &track->next;
                        track->next = NULL;
                        track->num = atoi(p);
                        
                        p = t + 1;
                        printf("%2d: %-12.12s ", track->num, p);
                        track->modes = strdup(p);
                        track->extension = NULL;
                        track->mode = 0;
                        track->audio = 0;
                        track->bsize = track->bstart = -1;
                        track->bsize = -1;
                        track->startsect = track->stopsect = -1;
                        
                        gettrackmode(track, p);
                        
                } else if ((p = strstr(s, "INDEX"))) {
                        if (!(p = strchr(p, ' '))) {
                                printf("... ouch, no space after INDEX.\n");
                                continue;
                        }
                        p++;
                        if (!(t = strchr(p, ' '))) {
                                printf("... ouch, no space after index number.\n");
                                continue;
                        }
                        *t = '\0';
                        t++;
                        idx = atoi(p);
                        printf(" %s %s", p, t);
                        track->startsect = time2frames(t);
                        track->start = track->startsect * SECTLEN;
                        if (verbose)
                                printf(" (startsect %ld ofs %ld)", track->startsect, track->start);
                        if ((prevtrack) && (prevtrack->stopsect < 0)) {
                                prevtrack->stopsect = track->startsect;
                                prevtrack->stop = track->start - 1;
                        }
                }
        }
        
        if (track) {
                fseek(binf, 0, SEEK_END);
                track->stop = ftell(binf);
                track->stopsect = track->stop / SECTLEN;
        }
        
        printf("\n\n");
        
        
        printf("Writing tracks:\n\n");
        for (track = tracks; (track); track = track->next)
                writetrack(binf, track, basefile);
                
        fclose(binf);
        fclose(cuef);
        
        return 0;
}


  *)
    
  end
