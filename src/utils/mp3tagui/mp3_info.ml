(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

type channel_mode = 
    Stereo
  | Joint_stereo
  | Dual_channel_stereo
  | Mono

type mp3_encoding = CBR | VBR

type t =
  { duration: int;                      (** in seconds *)
    samplerate: int;                    (** in kilobits per second *)
    mode: channel_mode;                 (** stereo, mono, etc *)
    bitrate: int;                       (** in kilobits per second *)
    encoding: mp3_encoding;             (** variable or constant bit rate *)
    filesize: int                       (** in bytes *)
  }

let check_head h =
  let h0 = Char.code (Bytes.get h 0)
  and h1 = Char.code (Bytes.get h 1)
  and h2 = Char.code (Bytes.get h 2) in
  h0 = 0xFF &&
  h1 land 0xE0 = 0xE0 &&
  h1 land 0x18 <> 0x08 &&
  h1 land 0x06 <> 0x00 &&
  h2 land 0xF0 <> 0xF0 &&
  h2 land 0x0C <> 0x0C
  
let tabsel_123 =
[| [| [|0; 32; 64; 96; 128; 160; 192; 224; 256; 288; 320; 352; 384; 416; 448|];
      [|0; 32; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320; 384|];
      [|0; 32; 40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320|]
   |];
   [| [|0; 32; 48; 56; 64; 80; 96; 112; 128; 144; 160; 176; 192; 224; 256|];
      [|0; 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160|];
      [|0; 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160|]
   |]
|]

let mpg123_freqs =
  [|44100; 48000; 32000; 22050; 24000; 16000; 11025; 12000; 8000|]

let mpg123_bs =
  [|0; 384; 1152; 1152|]

let read_i4 ic =
  let b1 = input_byte ic in let b2 = input_byte ic in
  let b3 = input_byte ic in let b4 = input_byte ic in
  (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

let get_xing_header ic header =
  let id = (header lsr 19) land 1
  and mode = (header lsr 6) land 3 in
  let offset =
    if id > 0
    then if mode <> 3 then 32 else 17
    else if mode <> 3 then 17 else 9 in
  seek_in ic (pos_in ic + offset);
  let buf = String.create 4 in
  really_input ic buf 0 4;
  if not (Misc.bytes_equal_string buf "Xing") then raise Not_found;
  let flags = read_i4 ic in
  (* 3 = FRAMES_FLAG | BYTES_FLAG *)
  if flags land 3 <> 3 then raise Not_found;
  let frames = read_i4 ic in
  let bytes = read_i4 ic in
  (frames, bytes)

let for_channel ic =
  seek_in ic 0;
  let buf = String.create 4 in
  really_input ic buf 0 4;
  while not (check_head buf) do
    Bytes.blit buf 1 buf 0 3;
    buf.[3] <- input_char ic
  done;
  let header = 
    (Char.code (Bytes.get buf 1) lsl 16) lor
    (Char.code (Bytes.get buf 2) lsl 8) lor
    (Char.code (Bytes.get buf 3)) in
  let (lsf, mpeg25) =
    if header land 0x100000 <> 0
    then ((if header land 0x80000 = 0 then 1 else 0), false)
    else (1, true) in
  let lay = 4 - ((header lsr 17) land 3) in
  let sampling_frequency =
    if mpeg25
    then 6 + ((header lsr 10) land 3)
    else ((header lsr 10) land 3) + 3 * lsf in
  let sample_rate =
    mpg123_freqs.(sampling_frequency) in
  let bitrate_index = (header lsr 12) land 0xF in
(*  let padding = (header lsr 9) land 1 in *)
  let mode =
    match (header lsr 6) land 3 with
      0 -> Stereo | 1 -> Joint_stereo | 2 -> Dual_channel_stereo | _ -> Mono in
  let tpf =
    float mpg123_bs.(lay) /. float (max 1 (sample_rate lsl lsf)) in
  let bpf =
    float tabsel_123.(lsf).(lay - 1).(bitrate_index) *.
    (if lay = 1 then 12000.0 *. 4.0 else 144000.0) /.
    float (max 1 (sample_rate lsl lsf)) in
  let filesize = in_channel_length ic in
  let (enc, duration, bitrate) =
    try
      let (frames, bytes) = get_xing_header ic header in
      (VBR,
       tpf *. float frames,
        truncate (float bytes *. 8.0 /. (
            max 0.00001 (tpf *. float frames *. 1000.0))))
    with Not_found ->
      let len = filesize - pos_in ic in
      (CBR,
       float len /. (max 0.00001 bpf) *. tpf,
       tabsel_123.(lsf).(lay - 1).(bitrate_index)) in
  { duration = truncate duration;
    samplerate = sample_rate / 1000;
    mode = mode;
    bitrate = bitrate;
    encoding = enc;
    filesize = filesize }

(*
  let framesize =
    if bitrate_index = 0 then 0 else
    match lay with
      1 ->
        let a = tabsel_123.(lsf).(0).(bitrate_index) * 12000 in
        let b = a / mpg123_freqs.(sampling_frequency) in
        ((b + padding) lsl 2) - 4
    | 2 ->
        let a = tabsel_123.(lsf).(1).(bitrate_index) * 144000 in
        let b = a / mpg123_freqs.(sampling_frequency) in
        b + padding - 4
    | 3 ->
        let a = tabsel_123.(lsf).(1).(bitrate_index) * 144000 in
        let b = a / (mpg123_freqs.(sampling_frequency) lsl lsf) in
        b + padding - 4
    | _ ->
        assert false in
*)

let info filename =
  let ic = open_in_bin filename in
  try
    let res = for_channel ic in close_in ic; res
  with x ->
    close_in ic; raise x
