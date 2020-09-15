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

open Mp3tag
open Mp3tag.Id3v1
module C = Configwin


type id3 = 
    V1
  | V2
  | Both

let params_from_tag tag = 
  let title = C.string
      ~f: (fun s -> tag.title <- s)
      Mp3_messages.title tag.title
  in
  let artist = C.string
      ~f: (fun s -> tag.artist <- s) 
      Mp3_messages.artist tag.artist
  in
  let album = C.string
      ~f: (fun s -> tag.album <- s)
      Mp3_messages.album tag.album
  in
  let year = C.string
      ~f:(fun s -> tag.year <- s) 
      Mp3_messages.year tag.year
  in
  let tracknum = C.string
      ~f: (fun s -> tag.tracknum <- (try int_of_string s with _ -> 0))
      Mp3_messages.tracknum (string_of_int tag.tracknum)
  in
  let comment = C.string
      ~f:(fun s -> tag.comment <- s)
      Mp3_messages.comment tag.comment
  in
  let genre = C.combo
      ~f: (fun s -> tag.genre <- Mp3_misc.genre_of_string s)
      ~blank_allowed: true
      Mp3_messages.genre
      (Mp3_genres.genres_names)
      (Mp3_misc.string_of_genre tag.genre)
  in
  let params = [
    artist ;
    album ;
    title ;
    comment ;
    tracknum ;
    year ;
    genre
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