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

(* Getting information on an MP3 file. *)

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

(** Return information on the given MP3 file.
   @raise Sys_error if an error occurs with the file. *)
val info : string -> t
