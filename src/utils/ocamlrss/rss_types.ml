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
      cat_name : string ;
      cat_domain : url option ;
    } 

type image = 
    {
      image_url : url ;
      image_title : string ;
      image_link : url ;
      image_height : int option ;
      image_width : int option ;
      image_desc : string option ;
    } 

type text_input = 
    {
      ti_title : string ; (** The label of the Submit button in the text input area. *)
      ti_desc : string ; (** Explains the text input area. *)
      ti_name : string ; (** The name of the text object in the text input area. *)
      ti_link : string ; (** The URL of the CGI script that processes text input requests. *)
    } 

type enclosure = 
    {
      encl_url : url ; (** URL of the enclosure *)
      encl_length : int ; (** size in bytes *)
      encl_type : string ; (** MIME type *)
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
      guid_name : string ; (** can be a permanent url, if permalink is true *)
      guid_permalink : bool ; (** default is true when no value was specified *)
    } 

type source =
    {
      src_name : string ;
      src_url : url ;
    } 

type item =
    {
      item_title : string option ;
      item_link : url option ;
      item_desc : string option ;
      item_pubdate : Rss_date.t option ;
      item_author : email option ;
      item_categories : category list ;
      item_comments : url option ; 
      item_enclosure : enclosure option ;
      item_guid : guid option ; 
      item_source : source option ;
    } 

type channel =
    {
      ch_title : string ;
      ch_link : url ;
      ch_desc : string ;
      ch_language : string option ;
      ch_copyright : string option ;
      ch_managing_editor : email option ;
      ch_webmaster : email option ;
      ch_pubdate : Rss_date.t option ;
      ch_last_build_date : Rss_date.t option ;
      ch_categories : category list ;
      ch_generator : string option ;
(*      ch_cloud : cloud option ; *)
      ch_docs : url option ;
      ch_ttl : int option ;
      ch_image : image option ;
(*      ch_rating : pics_rating option ; *)
      ch_text_input : text_input option ;
(*
      ch_skip_hours : skip_hours option ;
      ch_skip_days : skip_days option ;
*)
      ch_items : item list ;
    } 

