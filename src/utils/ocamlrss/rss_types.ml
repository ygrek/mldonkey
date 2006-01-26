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

type email = string (** can be, for example: foo@bar.com (Mr Foo Bar) *)

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

type cloud = int (* A VOIR *)
(* <cloud domain="radio.xmlstoragesystem.com" 
   port="80" 
   path="/RPC2" 
   registerProcedure="xmlStorageSystem.rssPleaseNotify" 
   protocol="xml-rpc" />
 *)

type pics_rating = int (* A VOIR *)
type skip_hours = int (* A VOIR *)
type skip_days = int (* A VOIR *)

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
      mutable item_title : string option ;
      mutable item_link : url option ;
      mutable item_desc : string option ;
      mutable item_pubdate : Rss_date.t option ;
      mutable item_author : email option ;
      mutable item_categories : category list ;
      mutable item_comments : url option ;
      mutable item_enclosure : enclosure option ;
      mutable item_guid : guid option ;
      mutable item_source : source option ;
    } 

type channel =
    {
      mutable ch_title : string ;
      mutable ch_link : url ;
      mutable ch_desc : string ;
      mutable ch_language : string option ;
      mutable ch_copyright : string option ;
      mutable ch_managing_editor : email option ;
      mutable ch_webmaster : email option ;
      mutable ch_pubdate : Rss_date.t option ;
      mutable ch_last_build_date : Rss_date.t option ;
      mutable ch_categories : category list ;
      mutable ch_generator : string option ;
(*      mutable ch_cloud : cloud option ; *)
      mutable ch_docs : url option ;
      mutable ch_ttl : int option ;
      mutable ch_image : image option ;
(*      mutable ch_rating : pics_rating option ; *)
      mutable ch_text_input : text_input option ;
(*
      mutable ch_skip_hours : skip_hours option ;
      mutable ch_skip_days : skip_days option ;
*)
      mutable ch_items : item list ;
    } 
