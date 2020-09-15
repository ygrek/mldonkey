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

(** The OCaml-RSS library. *)

(** {2 Types} *)

type date = {
  year : int;		(** complete year *)
  month : int;		(** 1..12 *)
  day : int;		(** 1..31 *)
  hour : int;
  minute : int;
  second : int;
  zone : int;		(** in minutes; 60 = UTC+0100 *)
  week_day : int	(** 0 = sunday; -1 if not given *)
}

val since_epoch : date -> float
val float_to_date : float -> date
val string_of_date : ?fmt: string -> date -> string

type email = string (** can be, for example: foo\@bar.com (Mr Foo Bar) *)

type url = string
type category = 
    {
      mutable cat_name : string ;
      mutable cat_domain : url option ;
    } 
      
type image = 
    {
      mutable image_url : url ;
      mutable image_title : string ;
      mutable image_link : url ;
      mutable image_height : int option ;
      mutable image_width : int option ;
      mutable image_desc : string option ;
    } 

type text_input = 
    {
      mutable ti_title : string ; (** The label of the Submit button in the text input area. *)
      mutable ti_desc : string ; (** Explains the text input area. *)
      mutable ti_name : string ; (** The name of the text object in the text input area. *)
      mutable ti_link : string ; (** The URL of the CGI script that processes text input requests. *)
    } 

type enclosure = 
    {
      mutable encl_url : url ; (** URL of the enclosure *)
      mutable encl_length : int ; (** size in bytes *)
      mutable encl_type : string ; (** MIME type *)
    } 

type guid = 
    {
      mutable guid_name : string ; (** can be a permanent url, if permalink is true *)
      mutable guid_permalink : bool ; (** default is true when no value was specified *)
    } 

type source = 
    {
      mutable src_name : string ;
      mutable src_url : url ;
    } 

type item = 
    {
      mutable item_title : string option; (** Optional title *)
      mutable item_link : url option; (** Optional link *)
      mutable item_desc : string option; (** Optional description *)
      mutable item_pubdate : date option ; (** Date of publication *)
      mutable item_author : email option ; (** Author of the item *)
      mutable item_categories : category list ;
      mutable item_comments : url option ; (** Url of comments about this item *)
      mutable item_enclosure : enclosure option ;
      mutable item_guid : guid option ;
      mutable item_source : source option ;
    } 

type channel = 
    {
      mutable ch_title : string ; (** Mandatory title *)
      mutable ch_link : url ; (** Mandatory link of the site *)
      mutable ch_desc : string ; (** Mandatory description *)
      mutable ch_language : string option ; (** Language of the news *)
      mutable ch_copyright : string option ; (** Copyright note *)
      mutable ch_managing_editor : email option ; (** Managing editor of the news *)
      mutable ch_webmaster : email option ; (** The webmasterof the site *)
      mutable ch_pubdate : date option ; (** Publication date of the channel *)
      mutable ch_last_build_date : date option ; (** When the channel was last built *)
      mutable ch_categories : category list ;
      mutable ch_generator : string option ; (** The tool used to generate this channel *)
      mutable ch_docs : url option ; (** An url to a RSS reference *)
      mutable ch_ttl : int option ; (** Time to live, in minutes *)
      mutable ch_image : image option ;
      mutable ch_text_input : text_input option ;
      mutable ch_items : item list ;
    } 

(** {2 Building items and channels} *)

val item : 
    ?title: string ->
      ?link: url ->
        ?desc: string ->
          ?pubdate: date ->
            ?author: email ->
              ?cats: category list ->
                ?comments: url ->
                  ?encl: enclosure ->
                    ?guid: guid ->
                      ?source: source ->
                        unit ->
                          item

val channel :
    title: string ->
      link: url ->
        desc: string ->
          ?language: string ->
            ?copyright: string ->
              ?managing_editor: email ->
                ?webmaster: email ->
                  ?pubdate: date ->
                    ?last_build_date: date ->
                      ?cats: category list ->
                        ?generator: string ->
                          ?docs: url ->
                            ?ttl: int ->
                              ?image: image ->
                                ?text_input: text_input ->
                                  item list ->
                                    channel

(** {2 Reading channels} *)

val channel_of_file : string -> channel
val channel_of_string : string -> channel

(** {2 Writing channels} *)

val print_channel : ?date_fmt: string -> Format.formatter -> channel -> unit
val print_file : ?date_fmt: string -> string -> channel -> unit
