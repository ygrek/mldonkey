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
      ~width: 330 ~height: 240
      ~title: Chat_messages.software () 
  in
  window#add app#coerce ;
  ignore (app#box#connect#destroy window#destroy);
  ignore (window#connect#destroy GMain.Main.quit);
  app#init_window window ;
  window#show () ;
  GMain.Main.main ()

let _ = main ()
