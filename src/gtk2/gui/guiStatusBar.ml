(* Copyright 2004 b8_bavard, INRIA *)
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


open GuiTypes2
open CommonTypes
open GuiTypes

module M = GuiMessages
module O = GuiOptions
module G = GuiGlobal
module Mi = GuiMisc
module A = GuiArt
module U = GuiUtf8

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)


(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let menubar = GMenu.menu_bar ()
let menuitem = GMenu.menu_item ~packing:menubar#add ()

let label_sharedfiles = GMisc.label ~xalign:0. ~yalign:0. ()
let label_filesdownloaded = GMisc.label ~xalign:0. ~yalign:0. ()
let label_servers = GMisc.label ~xalign:0. ~yalign:0. ()
let label_updown = GMisc.label ~xalign:1. ~yalign:0. ()
let label_corestatus = GMisc.label ~xalign:0. ~yalign:0.5 ~use_underline:true ()

let image_corestatus = GMisc.image ~xalign:0. ~yalign:0.5 ()
let image_updown = GMisc.image ~xalign:1. ~yalign:0. ()

(*************************************************************************)
(*                                                                       *)
(*                        update_sharedfiles                             *)
(*                                                                       *)
(*************************************************************************)

let update_sharedfiles nfiles shared =
  let size = GuiMisc.size_of_int64 shared in
  let t = Printf.sprintf "%s %5d/%-12s" !M.mW_lb_shared nfiles size in
  label_sharedfiles#set_text t

(*************************************************************************)
(*                                                                       *)
(*                        update_downloadedfiles                         *)
(*                                                                       *)
(*************************************************************************)

let update_downloadedfiles () =
  let t = Printf.sprintf "%s %5d/%-5d"
    !M.mW_lb_filesdownloaded !G.ndownloaded !G.ndownloads
  in
  label_filesdownloaded#set_text t

(*************************************************************************)
(*                                                                       *)
(*                        update_servers                                 *)
(*                                                                       *)
(*************************************************************************)

let update_servers () =
  let t = Printf.sprintf "%s %5d/%-5d"
    !M.mW_lb_serversconnected !G.nconnected_servers !G.nservers
  in
  label_servers#set_text t

(*************************************************************************)
(*                                                                       *)
(*                        update_updown                                  *)
(*                                                                       *)
(*************************************************************************)

let update_updown uprate downrate =
  let up = float_of_int uprate /. 1024. in
  let down = float_of_int downrate /. 1024. in
  let t = Printf.sprintf "%s %5.1f ko/s | %s %5.1f ko/s"
    !M.mW_lb_updload up !M.mW_lb_download down
  in
  let icon =
    match (up, down) with
        (u, d) when u >= 0.05 && d >= 0.05 -> M.icon_state_updown
      | (u, _) when u >= 0.05 -> M.icon_state_up
      | (_, d) when d >= 0.05 -> M.icon_state_down
      |  _ -> M.icon_state_notupdown
  in
  let pixb = A.get_icon ~icon ~size:A.SMALL () in
  image_updown#set_pixbuf pixb;
  label_updown#set_text t

(*************************************************************************)
(*                                                                       *)
(*                        update_corestatus                              *)
(*                                                                       *)
(*************************************************************************)

let update_corestatus status =
  G.core_status := status;
  if not !CommonGlobals.core_included
    then begin
      let (icon, t) =
        match status with
            Core_connecting -> (M.icon_menu_core_disconnect, !M.mW_lb_connecting)
          | Core_notconnected -> (M.icon_menu_core_kill, !M.mW_lb_not_connected)
          | Core_Connected -> (M.icon_menu_core_connectto, !M.mW_lb_connected)
      in
      let pixb = A.get_icon ~icon ~size:A.SMALL () in
      image_corestatus#set_pixbuf pixb;
      label_corestatus#set_label t
    end else begin
      let (icon, t) = (M.icon_type_source_normal, !M.mW_me_main_menu) in
      let pixb = A.get_icon ~icon ~size:A.SMALL () in
      image_corestatus#set_pixbuf pixb;
      label_corestatus#set_label t
    end

(*************************************************************************)
(*                                                                       *)
(*                        update_labels                                  *)
(*                                                                       *)
(*************************************************************************)

let update_labels () =
  update_downloadedfiles ();
  update_servers ();
  let s = label_sharedfiles#text in
  let (_, s') = String2.cut_at s ':' in
  label_sharedfiles#set_text (Printf.sprintf "%s%s" !M.mW_lb_shared s');
  let s = label_updown#text in
  let (_, s) = String2.cut_at s ':' in
  let (down, s) = String2.cut_at s '|' in
  let (_, up) = String2.cut_at s ':' in
  label_updown#set_text (Printf.sprintf "%s%s|%s%s" !M.mW_lb_updload up !M.mW_lb_download down);
  update_corestatus !G.core_status

(*************************************************************************)
(*                                                                       *)
(*                        clear                                          *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  update_sharedfiles 0 (Int64.of_int 0);
  update_downloadedfiles ();
  update_servers ();
  update_updown 0 0

(*************************************************************************)
(*                                                                       *)
(*                        attach                                         *)
(*                                                                       *)
(*************************************************************************)

let attach (table : GPack.table) left w =
  table#attach ~left ~top:1
    ~right:(left + 1) ~bottom:2
    ~xpadding:0 ~ypadding:0 
    ~expand:`X ~fill:`X
    w#coerce

(*************************************************************************)
(*                                                                       *)
(*                        status_box                                     *)
(*                                                                       *)
(*************************************************************************)

let status_box () =
  let table =
    GPack.table ~columns:5 ~homogeneous:false ~border_width:3
       ~col_spacings:6 ()
  in
  let hbox_corestatus = GPack.hbox ~homogeneous:false ~spacing:6 () in
  hbox_corestatus#pack ~expand:true ~fill:false image_corestatus#coerce;
  hbox_corestatus#pack ~expand:true ~fill:true label_corestatus#coerce;
  let hbox_transfers = GPack.hbox ~homogeneous:false ~spacing:6 () in
  hbox_transfers#pack ~expand:true ~fill:true image_updown#coerce;
  hbox_transfers#pack ~expand:true ~fill:false label_updown#coerce;
  menuitem#add hbox_corestatus#coerce;
  let left = ref 0 in
  List.iter (fun w ->
    attach table !left w;
    incr left
  ) [
     menubar#coerce;
     label_sharedfiles#coerce;
     label_filesdownloaded#coerce;
     label_servers#coerce;
     hbox_transfers#coerce;
    ];

  table#coerce


let _ =
  update_sharedfiles 0 (Int64.of_int 0);
  update_downloadedfiles ();
  update_servers ();
  update_updown 0 0;
  update_corestatus Core_notconnected
