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

(** Gui icons. *)

let connected = [|
"14 14 9 1";
"       c None";
".      c #C0C0C0";
"+      c #808000";
"@      c #808080";
"#      c #FFFF00";
"$      c #FFFFFF";
"%      c #000080";
"&      c #800000";
"*      c #FF0000";
"    ......    ";
"   ..++@@..   ";
"  .+@####@@.  ";
" .@##$$$$#$@. ";
"..+#$$$####@..";
".@#$$%$#%$##@.";
".+#$.@..@.##@.";
".+##########@.";
".+##&####*##@.";
"..+##&**&##@..";
" .+########@. ";
"  .++#$$#@@.  ";
"   ..+++@..   ";
"    ......    "|]


let not_connected = [|
"12 12 4 1";
"       c None";
".      c #808080";
"+      c #C0C0C0";
"@      c #FFFFFF";
"    ....    ";
"  ..+@+@..  ";
" .@+@+@+@+. ";
" .+@+@+@+@. ";
".+@+@+@+@+@.";
".@...@@...@.";
".+@+@+@+@+@.";
".@+@+@+@+@+.";
" .@+@..+@+. ";
" .+@+@+@+@. ";
"  ..@+@+..  ";
"    ....    "|]

let create_gdk_pixmap i =
  let gdk_pix = GDraw.pixmap_from_xpm_d ~data: i 
      ~colormap: (Gdk.Color.get_system_colormap ())
      () 
  in
  gdk_pix

let create_pixmap i =
  let gdk_pix = create_gdk_pixmap i in
  let pix = GMisc.pixmap gdk_pix () in
  pix
