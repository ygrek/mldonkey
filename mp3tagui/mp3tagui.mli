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

(** Graphical user interface functions. *)

(** A type to indicate which tag we want to read and write.*)
type id3 = 
    V1 (** Id3 version 1 *)
  | V2 (** Id3 version 2 *)
  | Both (** Both id3 versions 1 and 2 *)


(** Make the user edit the tag of the given file. *)
val edit_file : id3 -> string -> unit

(** Make the user edit the given v1 tag in a window with the given title. *)
val edit_tag_v1 : string -> Mp3tag.Id3v1.tag -> unit

(** Make the user edit the given v2 tag in a window with the given title. *)
val edit_tag_v2 : string -> Mp3tag.Id3v2.tag -> Mp3tag.Id3v2.tag
