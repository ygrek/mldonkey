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

(** Main module of the standalone tool. *)

ignore (GMain.Main.init ());;

let pred (i1,h1,p1) (i2,h2,p2) =
  i1 = i2 && h1 = h2 && p1 = p2

let main () =
  let _ = GMain.Main.init () in
  let config = Chat_args.parse () in
  let com = new Mlchat.tcp config in
  let app = new Mlchat.app ~pred: pred config com in
  let window = GWindow.window 
      ~allow_shrink: true
      ~allow_grow: true
      ~icon:(Chat_art.get_icon ~icon:"icon_mlchat" ())
      ~width:(Gdk.Screen.height () * 2 / 5)
      ~height:(Gdk.Screen.width () * 1 / 3)
      ~title:Chat_messages.software () 
  in
  window#add app#coerce ;
  ignore (app#box#connect#destroy window#destroy);
  ignore (window#connect#destroy GMain.Main.quit);
  app#init_window window ;
  window#show () ;
  GMain.Main.main ()

let _ = main ()
