(* Copyright 2004 b8_bavard, b8_fee_carabine, INRIA *)
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

(**********************************************************************************)
(*                                                                                *)
(*                          Icons ressources of mlchat                            *)
(*                                                                                *)
(**********************************************************************************)

let table = [
  "icon_settings", (Menu_settings_svg.t, "");
  "icon_help", (Menu_help_svg.t, "");
  "icon_mlchat", (Menu_mlchat_svg.t, "");

  "icon_directory", (Stock_directory_svg.t, "");
  "icon_color", (Stock_color_svg.t, "");
  "icon_font", (Stock_font_svg.t, "");
  "icon_password", (Stock_password_svg.t, "");
  "icon_close", (Stock_close_svg.t, "");
  "icon_ok", (Stock_ok_svg.t, "");

  "icon_unknown", (Mime_unknown_svg.t, "");

]

(**********************************************************************************)
(*                                                                                *)
(*                            Loading functions                                   *)
(*                                                                                *)
(**********************************************************************************)

open Zlib

(* Convenient function to load an icon set to customize the GUI *)
(* TODO :
     - Allow to load an icon set from a given directory
     - Allow all formats possible : png, gif, jpeg, ico, etc...
     - Maybe use an index.theme file like gnome to indicate the
       directories available (16x16, 32x32, 48x48, etc...) to 
       allow different sizes to be displayed in a pretty manner.
*)

(* Return a pixbuf for a given svg data *)
let pixb icon_name pixel_size =
  let svg = uncompress_string icon_name in
  let z = float_of_int pixel_size /. 48. in
  let size_cb = (Rsvg.at_zoom z z) in
  let pb = Rsvg.render_from_string ~size_cb svg in
  GdkPixbuf.saturate_and_pixelate ~saturation:1. ~dest:pb ~pixelate:false pb;
  pb

(* function to desaturate icons *)
let saturate pb desat =
  if desat then
    GdkPixbuf.saturate_and_pixelate 
        ~dest:pb 
        ~saturation:0. 
        ~pixelate:true 
        pb;
  pb



let pix_buf icon_name pixel_size ?(desat=false) () =
  try
    let (default, o) = List.assoc icon_name table in
    match o with
      "" -> 
	let pb = pixb default pixel_size in
        saturate pb desat
    | f ->
	try
	  let pb = GdkPixbuf.from_file f in
          saturate pb desat
	with
	  _ ->
	    let pb = pixb default pixel_size in
            saturate pb desat
  with
    Not_found ->
      let pb = pixb Mime_unknown_svg.t pixel_size in
      saturate pb desat

let get_icon ~(icon : string) ?(desat=false) () =
  pix_buf icon 16 ~desat ()
