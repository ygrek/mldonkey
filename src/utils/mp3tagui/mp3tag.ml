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

module Id3v1 = struct 
  type tag = Mp3_tag.Id3v1.tag =  { 
      mutable title: string; 
      mutable artist: string; 
      mutable album: string;
      mutable year:string; 
      mutable comment: string; 
      mutable tracknum: int; 
      mutable genre: int 
    }
        
  let has_tag = Mp3_tag.Id3v1.has_tag
  let read = Mp3_tag.Id3v1.read_tag
  let write t f = Mp3_tag.Id3v1.write_tag f t
  let merge = Mp3_tag.Id3v1.merge
  let no_tag = Mp3_tag.Id3v1.no_tag

end

module Id3v2 = struct 
  type tag = (string * string) list

  let read = Mp3_tag.Id3v2.read_tag
  let write t ?src f = Mp3_tag.Id3v2.write_tag ?src f t
  let merge = Mp3_tag.Id3v2.merge
  let no_tag = Mp3_tag.Id3v2.no_tag

end

let read_both_as_v1 = Mp3_tag.read_file_both_v1
let read_both_as_v2 = Mp3_tag.read_file_both_v2
let write_both_v1 t ?src f = Mp3_tag.write_file_both_v1 ?src f t
let write_both_v2 t ?src f= Mp3_tag.write_file_both_v2 ?src f t


let v2_of_v1 = Mp3_tag.v1_to_v2
let v1_of_v2 = Mp3_tag.v2_to_v1


let string_of_genre = Mp3_misc.string_of_genre
let genre_of_string = Mp3_misc.genre_of_string
let genres = Mp3_genres.genres_names

type channel_mode = Mp3_info.channel_mode =
    Stereo
  | Joint_stereo
  | Dual_channel_stereo
  | Mono

type mp3_encoding = Mp3_info.mp3_encoding =
    CBR (** Constant Bit Rate *)
  | VBR (** Variable Bit Rate *)

type info = Mp3_info.t =
  { duration: int;                      (** in seconds *)
    samplerate: int;                    (** in kilobits per second *)
    mode: channel_mode;                 (** stereo, mono, etc *)
    bitrate: int;                       (** in kilobits per second *)
    encoding: mp3_encoding;             (** variable or constant bit rate *)
    filesize: int                       (** in bytes *)
  }

let info = Mp3_info.info

