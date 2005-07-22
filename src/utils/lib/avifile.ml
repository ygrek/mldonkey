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

(*
    RIFF('AVI' 
 *            LIST('hdrl'
 *                  avih(<MainAVIHeader>)
 *                  LIST ('strl'
 *                      strh(<Stream header>)
 *                      strf(<Stream format>)
 *                      ... additional header data
 *            LIST('movi'        
 *                { LIST('rec' 
 *                            SubChunk...
 *                         )
 *                    | SubChunk } ....     
 *            )
 *            [ <AVIIndex> ]
 *      )
 /* form types, list types, and chunk types */
#define formtypeAVI             mmioFOURCC('A', 'V', 'I', ' ')
#define listtypeAVIHEADER       mmioFOURCC('h', 'd', 'r', 'l')
#define ckidAVIMAINHDR          mmioFOURCC('a', 'v', 'i', 'h')
#define listtypeSTREAMHEADER    mmioFOURCC('s', 't', 'r', 'l')
#define ckidSTREAMHEADER        mmioFOURCC('s', 't', 'r', 'h')
#define ckidSTREAMFORMAT        mmioFOURCC('s', 't', 'r', 'f')
#define ckidSTREAMHANDLERDATA   mmioFOURCC('s', 't', 'r', 'd')
#define ckidSTREAMNAME          mmioFOURCC('s', 't', 'r', 'n')

#define listtypeAVIMOVIE        mmioFOURCC('m', 'o', 'v', 'i')
#define listtypeAVIRECORD       mmioFOURCC('r', 'e', 'c', ' ')

#define ckidAVINEWINDEX         mmioFOURCC('i', 'd', 'x', '1')

/*
** Stream types for the <fccType> field of the stream header.
*/
#define streamtypeVIDEO         mmioFOURCC('v', 'i', 'd', 's')
#define streamtypeAUDIO         mmioFOURCC('a', 'u', 'd', 's')
#define streamtypeMIDI          mmioFOURCC('m', 'i', 'd', 's')
#define streamtypeTEXT          mmioFOURCC('t', 'x', 't', 's')

/* Basic chunk types */
#define cktypeDIBbits           aviTWOCC('d', 'b')
#define cktypeDIBcompressed     aviTWOCC('d', 'c')
#define cktypePALchange         aviTWOCC('p', 'c')
#define cktypeWAVEbytes         aviTWOCC('w', 'b')

/* Chunk id to use for extra chunks for padding. */
#define ckidAVIPADDING          mmioFOURCC('J', 'U', 'N', 'K')


*)
let input_int8 ic =
  int_of_char (input_char ic)
  
let input_int16 ic =
  let i0 = input_int8  ic in
  let i1 = input_int8 ic in
  i0 + 256 * i1

let int32_65536 = Int64.of_int 65536
  
let input_int32 ic =
  let i0 = input_int16 ic in
  let i1 = input_int16 ic in
  let i0 = Int64.of_int i0 in
  let i1 = Int64.of_int i1 in
  Int64.add i0 (Int64.mul i1 int32_65536)
  
let input_int ic =
  let i0 = input_int16 ic in
  let i1 = input_int16 ic in
  i0 + i1 * 65536

let input_string4 ic =
  let s = String.create 4 in
  really_input ic s 0 4;
  s

let print_string4 v s =
  lprintf "%s :" v;
  for i = 0 to 3 do
    let c = s.[i] in
    let int = int_of_char c in
    if int > 31 && int <127 then
      lprint_char c
    else lprintf "[%d]" int
  done

let print_int32 s i=
  lprintf_nl "%s: %s" s (Int64.to_string i)

let print_int16 s i=
  lprintf "%s: %d" s i

let load file =
  let ic = open_in file in
(* pos: 0 *)
  let s = input_string4 ic in
  if s <> "RIFF" then failwith "Not an AVI file (RIFF absent)";

(* pos: 4 *)
  let size = input_int32 ic in
  lprintf_nl "SIZE %s" (Int64.to_string size);

(* pos: 8 *)
  let s = input_string4 ic in
  if s <> "AVI " then failwith  "Not an AVI file (AVI absent)";

(* pos: 12 *)
  let s = input_string4 ic in
  if s <> "LIST" then failwith  "Not an AVI file (LIST absent)";

(* position 16 *)
  let rec iter_list pos end_pos =
    lprintf_nl "POS %s/%s" (Int64.to_string pos) (Int64.to_string end_pos);
    if pos < end_pos then begin
(* on peut s'arreter quand size = 0 *)
        seek_in ic (Int64.to_int pos);
        let size2 = input_int32 ic in
        lprintf_nl "SIZE2 %s" (Int64.to_string size2);
        
        let header_name = input_string4 ic in
        print_string4 "header" header_name; lprint_newline ();
(* pos: pos + 8 *)       
        begin
          match header_name with
            "hdrl" ->
              lprintf_nl "HEADER";
              
              let s = input_string4 ic in
              if s <> "avih" then failwith "Bad AVI file (avih absent)";

(* pos: pos + 12 *)
              let main_header_len = 52 in             
              
              ignore (input_string4 ic);

              
              let dwMicroSecPerFrame = input_int32 ic in
              let dwMaxBytesPerSec = input_int32 ic in
              let dwPaddingGranularity = input_int32 ic in
              let dwFlags = input_int32 ic in
              let dwTotalFrames = input_int32 ic in
              let dwInitialFrames = input_int32 ic in
              let dwStreams = input_int32 ic in
              let dwSuggestedBufferSize = input_int32 ic in              
              let dwWidth = input_int32 ic in
              let dwHeight = input_int32 ic in
              
              (*
              print_int32 "dwMicroSecPerFrame" dwMicroSecPerFrame ;
              print_int32 "dwMaxBytesPerSec" dwMaxBytesPerSec ;
              print_int32 "dwPaddingGranularity" dwPaddingGranularity ;
              print_int32 "dwFlags" dwFlags ;
              print_int32 "dwTotalFrames" dwTotalFrames ;
              print_int32 "dwInitialFrames" dwInitialFrames ;
              print_int32 "dwStreams" dwStreams ;
              print_int32 "dwSuggestedBufferSize" dwSuggestedBufferSize ;
              print_int32 "dwWidth" dwWidth;
              print_int32 "dwHeight" dwHeight;
*)
              seek_in ic ((Int64.to_int pos) + main_header_len +20);
              let s = input_string4 ic in
(*              print_string4 "LIST:" s; *)

              let pos_in = Int64.add pos (Int64.of_int (
                    main_header_len +24)) in
              let last_pos = Int64.add pos_in size2 in
                  iter_list pos_in last_pos
              
          | "movi" ->
              lprintf_nl "CHUNKS"
              
          | "strl" ->
              lprintf_nl "STREAM DESCRIPTION";
              
              let offset = Int64.of_int 4  in
              let pos0 = Int64.add pos offset in
              let end_pos0 = Int64.add pos size2 in
              iter_list pos0 end_pos0

          | "strh" ->
              lprintf_nl "STREAM HEADER";
              
              ignore (input_string4 ic);
              
              let fccType = input_string4 ic in
              let fccHandler = input_string4 ic in
              let dwFlags = input_int32 ic in (* Contains AVITF_* flags *)
              let wPriority = input_int16 ic in
              let wLanguage = input_int16 ic in
              let dwInitialFrames = input_int32 ic in
              let dwScale = input_int32 ic in
              let dwRate = input_int32 ic in (* dwRate / dwScale == samples/second *)
              let dwStart = input_int32 ic in
              let dwLength = input_int32 ic in
              let dwSuggestedBufferSize = input_int32 ic in
              let dwQuality = input_int32 ic in
              let dwSampleSize = input_int32 ic in
              let rcFrame_x = input_int16 ic in
              let rcFrame_y = input_int16 ic in
              let rcFrame_dx = input_int16 ic in
              let rcFrame_dy = input_int16 ic in

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
              ()
              
          | _ -> ()
        end;
        
        iter_list (Int64.add pos (Int64.add size2 (Int64.of_int 8))) end_pos
    end 
    
  in
  let pos0 = Int64.of_int 16 in
  iter_list pos0 (Int64.add pos0 size);
  close_in ic;

