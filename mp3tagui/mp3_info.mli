(***********************************************************************)
(*                               Mp3tag                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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
