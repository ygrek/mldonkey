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

(** Interface for editing mp3 tags and getting information. *)

(** Reading and writing id3 v1.1 tags. *)
module Id3v1 : sig

  (** An id3 v1.1 tag. *)
  type tag = { 
      mutable title: string; 
      mutable artist: string; 
      mutable album: string;
      mutable year:string; 
      mutable comment: string; 
      mutable tracknum: int; 
      mutable genre: int 
    }

  (** Check if the given file has a id3 v1.1 tag.
     @raise Sys_error if an error occurs while opening the file. 
  *)
  val has_tag : string -> bool

  (** Read the tags in a mp3 file.
     @raise Not_found if the file doesn't contain tags.
     @raise Sys_error if an error occurs while opening the file. 
  *)
  val read : string -> tag

  (** Write the given tag structure into the given file. 
     @raise Sys_error if an error occurs with the file.
  *)
  val write : tag -> string -> unit

  (** Merge two tags. 
     [merge t1 t2] return a new tag where field values in [t1]
     have been replaced by the ones in [t2] when they are defined
     in [t2].*)
  val merge : tag -> tag -> tag

  (** The empty tag. *)
  val no_tag : tag
end

(** Reading and writing id3 v2.3 tags. *)
module Id3v2 : sig
    type tag = (string * string) list

    (** Read the tags in a mp3 file.
       @return the list of information or [[]] if no information was found.
       @raise Sys_error if an error occurs while opening the file. 
    *)
    val read : string -> tag

    (**  Write the given tag structure into the given file. 
       @raise Sys_error if an error occurs with the file.
       @param src the optional source filename, if it is
       different from the filename.
    *)
    val write : tag -> ?src:string -> string -> unit

    (** Merge two tags. 
       [merge t1 t2] return a new tag where values in [t1]
       have been replaced by the ones in [t2] when they are defined
       in [t2].*)
    val merge : tag -> tag -> tag

    (** The empty tag. *)
    val no_tag : tag
  end

(** {2 Reading and writing both versions of tags} *)

(** Get a v1 tag from the v1 and v2 tags of the given file.
   The returned tag is the result of [merge v2 v1].*)
val read_both_as_v1 : string -> Id3v1.tag

(** Get a v2 tag from the v1 and v2 tags of the given file.
   The returned tag is the result of [merge v2 v1].*)
val read_both_as_v2 : string -> Id3v2.tag

(** Write the given v1 tag to the v1 and v2 tags of the given file.
   @param src the optional source filename, if it is
   different from the filename.*)
val write_both_v1 : Id3v1.tag -> ?src: string -> string -> unit

(** Write the given v2 tag to the v1 and v2 tags of the given file.
   @param src the optional source filename, if it is
   different from the filename.*)
val write_both_v2 : Id3v2.tag -> ?src: string -> string -> unit


(** {2 Tag conversions} *)

val v2_of_v1 : Id3v1.tag -> Id3v2.tag
val v1_of_v2 : Id3v2.tag -> Id3v1.tag

(** {2 Genres} *)

(** Get the genre string from the given genre id. *)
val string_of_genre : int -> string

(** Get the genre id from the given genre string. *)
val genre_of_string : string -> int

(** The list of genre names. *)
val genres : string list



(** {2 Getting information on a MP3 file} *)

type channel_mode = 
    Stereo
  | Joint_stereo
  | Dual_channel_stereo
  | Mono

type mp3_encoding = 
    CBR (** Constant Bit Rate *)
  | VBR (** Variable Bit Rate *)

type info =
  { duration: int;                      (** in seconds *)
    samplerate: int;                    (** in kilobits per second *)
    mode: channel_mode;                 (** stereo, mono, etc *)
    bitrate: int;                       (** in kilobits per second *)
    encoding: mp3_encoding;             (** variable or constant bit rate *)
    filesize: int                       (** in bytes *)
  }

(** Return information on the given MP3 file. *)
val info : string -> info

