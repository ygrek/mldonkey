(*********************************************************************************)
(*                OCaml-RSS                                                      *)
(*                                                                               *)
(*    Copyright (C) 2003 Institut National de Recherche en Informatique et       *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as published by       *)
(*    the Free Software Foundation; either version 2 of the License, or          *)
(*    any later version.                                                         *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*********************************************************************************)

type date = Rss_date.t = {
  year : int;		(** complete year *)
  month : int;		(** 1..12 *)
  day : int;		(** 1..31 *)
  hour : int;
  minute : int;
  second : int;
  zone : int;		(** in minutes; 60 = UTC+0100 *)
  week_day : int	(** 0 = sunday; -1 if not given *)
}

let since_epoch = Rss_date.since_epoch
let float_to_date t = Rss_date.create t

type email = string (** can be, for example: foo@bar.com (Mr Foo Bar) *)
type url = string
type category = Rss_types.category =
    {
      cat_name : string ;
      cat_domain : url option ;
    } 
      
type image = Rss_types.image =
    {
      image_url : url ;
      image_title : string ;
      image_link : url ;
      image_height : int option ;
      image_width : int option ;
      image_desc : string option ;
    } 

type text_input = Rss_types.text_input =
    {
      ti_title : string ; (** The label of the Submit button in the text input area. *)
      ti_desc : string ; (** Explains the text input area. *)
      ti_name : string ; (** The name of the text object in the text input area. *)
      ti_link : string ; (** The URL of the CGI script that processes text input requests. *)
    } 

type enclosure = Rss_types.enclosure =
    {
      encl_url : url ; (** URL of the enclosure *)
      encl_length : int ; (** size in bytes *)
      encl_type : string ; (** MIME type *)
    } 

type guid = Rss_types.guid =
    {
      guid_name : string ; (** can be a permanent url, if permalink is true *)
      guid_permalink : bool ; (** default is true when no value was specified *)
    } 

type source = Rss_types.source = 
    {
      src_name : string ;
      src_url : url ;
    } 

type item = Rss_types.item =
    {
      item_title : string option;
      item_link : url option;
      item_desc : string option;
      item_pubdate : date option ;
      item_author : email option ;
      item_categories : category list ;
      item_comments : url option ; 
      item_enclosure : enclosure option ;
      item_guid : guid option ; 
      item_source : source option ;
    } 

type channel = Rss_types.channel =
    {
      ch_title : string ;
      ch_link : url ;
      ch_desc : string ;
      ch_language : string option ;
      ch_copyright : string option ;
      ch_managing_editor : email option ;
      ch_webmaster : email option ;
      ch_pubdate : date option ;
      ch_last_build_date : date option ;
      ch_categories : category list ;
      ch_generator : string option ;
      ch_docs : url option ;
      ch_ttl : int option ;
      ch_image : image option ;
      ch_text_input : text_input option ;
      ch_items : item list ;
    } 

let item ?title
    ?link
    ?desc
    ?pubdate
    ?author
    ?(cats=[])
    ?comments
    ?encl
    ?guid
    ?source
    () =
  {
    item_title = title ;
    item_link = link ;
    item_desc = desc;
    item_pubdate = pubdate ;
    item_author = author ;
    item_categories = cats ;
    item_comments = comments ; 
    item_enclosure = encl ;
    item_guid = guid ; 
    item_source = source ;
  } 

let channel ~title ~link ~desc
    ?language
    ?copyright
    ?managing_editor
    ?webmaster
    ?pubdate
    ?last_build_date
    ?(cats=[])
    ?generator
    ?docs
    ?ttl
    ?image
    ?text_input
    items
    =
  {
    ch_title = title ;
    ch_link = link ;
    ch_desc = desc ;
    ch_language = language ;
    ch_copyright = copyright ;
    ch_managing_editor = managing_editor ;
    ch_webmaster = webmaster ;
    ch_pubdate = pubdate ;
    ch_last_build_date = last_build_date ;
    ch_categories = cats ;
    ch_generator = generator ;
    ch_docs = docs ;
    ch_ttl = ttl ;
    ch_image = image ;
    ch_text_input = text_input ;
    ch_items = items ;
  } 
  

let channel_of_file = Rss_io.channel_of_file
let channel_of_string = Rss_io.channel_of_string

let print_channel = Rss_io.print_channel

let print_file file ch =
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  print_channel fmt ch;
  Format.pp_print_flush fmt ();
  close_out oc
