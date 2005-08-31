(* Copyright 2005 beedauchon *)
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

open Options

open CommonGlobals
open CommonOptions
open Printf2

let disable_gfx_support () =
  if !!html_mods_vd_gfx then
    html_mods_vd_gfx =:= false;
    html_mods_vd_gfx_tag =:= false

let show_nogd_debug_msg () =
  lprintf "Warning: Gd support was not compiled (install libgd-dev to enable it)\n"

let remove_files () =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let really_remove_files () = ()

(* main *)

let do_draw_pic ttl vl hl gdown gup =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let do_draw_down_pic ttl top_title vl hl gdown =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let do_draw_up_pic ttl top_title vl hl gup =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let do_draw_h_pic ttl vl hl gdown gup =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let do_draw_down_h_pic ttl top_title vl hl gdown =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let do_draw_up_h_pic ttl top_title vl hl gup =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let do_draw_tag title gdown gup =
(
  disable_gfx_support ();
  show_nogd_debug_msg ()
)

let png_version_num () = ""
