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

open Mp3tag
open Mp3tag.Id3v1
open Configwin

type id3 = 
    V1
  | V2
  | Both

let params_from_tag tag =
  let title = { 
    string_label = Mp3_messages.title ;
    string_value = tag.title;
    string_editable = true ;
    string_f_apply = (fun s -> tag.title <- s) ;
  } in
  let artist = { 
    string_label = Mp3_messages.artist ;
    string_value = tag.artist;
    string_editable = true ;
    string_f_apply = (fun s -> tag.artist <- s) ;
  } in
  let album = { 
    string_label = Mp3_messages.album ;
    string_value = tag.album;
    string_editable = true ;
    string_f_apply = (fun s -> tag.album <- s) ;
  } in
  let year = { 
    string_label = Mp3_messages.year ;
    string_value = tag.year;
    string_editable = true ;
    string_f_apply = (fun s -> tag.year <- s) ;
  } in  
  let tracknum = { 
    string_label = Mp3_messages.tracknum ;
    string_value = string_of_int tag.tracknum;
    string_editable = true ;
    string_f_apply = 
    (fun s -> tag.tracknum <- (try int_of_string s with _ -> 0)) ;
  } in  
  let comment = { 
    string_label = Mp3_messages.comment ;
    string_value = tag.comment;
    string_editable = true ;
    string_f_apply = (fun s -> tag.comment <- s) ;
  } in
  let genre = { 
    combo_label = Mp3_messages.genre ;
    combo_value = Mp3_misc.string_of_genre tag.genre;
    combo_choices = Mp3_genres.genres_names;
    combo_editable = true ;
    combo_blank_allowed = true ;
    combo_new_allowed = false ;
    combo_f_apply = (fun s -> tag.genre <- Mp3_misc.genre_of_string s) ;
  } in
  let params = [
    String_param artist ;
    String_param album ;
    String_param title ;
    String_param comment ;
    String_param tracknum ;
    String_param year ;
    Combo_param genre
  ] 
  in
  params

let edit_file id3 filename =
  let tag = 
    try 
      let read =
	match id3 with
	  V1 -> Mp3tag.Id3v1.read 
	| V2 -> fun f -> Mp3tag.v1_of_v2 (Mp3tag.Id3v2.read f)
	| Both -> Mp3tag.read_both_as_v1
      in
      prerr_endline "reade";
      read filename
    with 
      Not_found ->
	{ title = "" ;
	  artist = "" ; 
	  album = "" ;
	  year = "" ;
	  comment = "" ; 
	  tracknum = 0; 
	  genre = 0 ;
	} 
    | x -> raise x
  in
  let params = params_from_tag tag in
  match Configwin.simple_get filename params with
    Configwin.Return_ok -> 
      (
       match id3 with
	 V1 -> Mp3tag.Id3v1.write tag filename
       | V2 -> Mp3tag.Id3v2.write (Mp3tag.v2_of_v1 tag) filename
       | Both -> Mp3tag.write_both_v1 tag filename
      )
  | _ -> () 

let edit_tag_v1 title tag =
  let params = params_from_tag tag in
  ignore (Configwin.simple_get title params)

let edit_tag_v2 title tag = 
  let tag1 = Mp3tag.v1_of_v2 tag in
  edit_tag_v1 title tag1 ;
  Mp3tag.v2_of_v1 tag1
