(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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
