(* Copyright 2001, Maxence Guesdon, INRIA Rocquencourt, FRANCE *)
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
