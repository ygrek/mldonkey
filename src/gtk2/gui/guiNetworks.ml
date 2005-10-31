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

(* The Networks window of MLgui *)

open GuiTypes2
open GuiTypes
open CommonTypes

open GuiTools
open GuiGlobal
open GuiProto

module M = GuiMessages
module Mi = GuiMisc
module O = GuiOptions
module G = GuiGlobal
module A = GuiArt
module U = GuiUtf8

let verbose = O.gtk_verbose_networks

let lprintf' fmt =
  Printf2.lprintf ("GuiNetworks: " ^^ fmt)


(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let enable_network (num, b) =
  (if !!verbose then lprintf' "Enable network %d %b\n" num b);
  GuiCom.send (EnableNetwork (num, b))

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (boxes : (int, net_box) Hashtbl.t) = Hashtbl.create 13

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let networks_timerID = ref (GMain.Timeout.add ~ms:2000 ~callback:(fun _ -> true))

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  Hashtbl.clear boxes

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let update_net_pix num b =
  try
    let box = Hashtbl.find boxes num in
    box.box_button#child#destroy ();
    let desat = not b in
    let pixb = Mi.network_pixb num ~size:A.LARGE ~desat () in
    let text = Mi.network_name num in
    match pixb with
        Some pixbuf -> ignore (GMisc.image ~pixbuf ~packing:box.box_button#add ())
      | _ -> ignore (GMisc.label ~text ~packing:box.box_button#add ())

  with _ -> ()

let update_label_uled num uploaded =
  try
    let box = Hashtbl.find boxes num in
    box.box_uploaded#set_label (Mi.size_of_int64 uploaded)
  with _ -> ()

let update_label_dled num downloaded =
  try
    let box = Hashtbl.find boxes num in
    box.box_downloaded#set_label (Mi.size_of_int64 downloaded)
  with _ -> ()

let update_label_serv_connected num nconnected =
  try
    let box = Hashtbl.find boxes num in
    box.box_connected_servers#set_label (Printf.sprintf "%d" nconnected)
  with _ -> ()

let update_network n =
  (if !!verbose then lprintf' "Receiveing data for network %s\n" n.network_netname);
  try
    let nn = Hashtbl.find G.networks n.network_netnum in
    if nn.net_enabled <> n.network_enabled
      then begin
        nn.net_enabled <- n.network_enabled;
        update_net_pix n.network_netnum n.network_enabled;
        GuiServers.clean_servers n.network_netnum n.network_enabled
      end;
    if nn.net_uploaded <> n.network_uploaded
      then begin
        nn.net_uploaded <- n.network_uploaded;
        update_label_uled n.network_netnum n.network_uploaded
      end;
    if nn.net_downloaded <> n.network_downloaded
      then begin
        nn.net_downloaded <- n.network_downloaded;
        update_label_dled n.network_netnum n.network_downloaded
      end;
    if nn.net_connected <> n.network_connected
      then begin
        nn.net_connected <- n.network_connected;
        update_label_serv_connected n.network_netnum n.network_connected
      end
  with _ ->
    if n.network_netname <> "Global Shares"
      then begin
        let nn =
          {
           net_num = n.network_netnum;
           net_name = n.network_netname;
           net_flags = n.network_netflags;
           net_enabled = n.network_enabled;
           net_displayed = true;
           net_uploaded = n.network_uploaded;
           net_downloaded = n.network_downloaded;
           net_connected = n.network_connected;
          }
        in
        Hashtbl.add G.networks n.network_netnum nn
      end

(*************************************************************************)
(*                                                                       *)
(*                         networks window                               *)
(*                                                                       *)
(*************************************************************************)

open GMain

let flags =
[
  NetworkHasServers, M.icon_state_server_init;
  NetworkHasRooms, M.icon_menu_rooms;
  NetworkHasMultinet, M.icon_net_globalshare;
  NetworkHasSearch, M.icon_menu_searches;
  NetworkHasChat, M.icon_menu_mlchat;
  NetworkHasSupernodes, M.icon_net_supernode;
  NetworkHasUpload, M.icon_menu_uploads;
]

let display_network (table : GPack.table) n left top =
  let top = ref top in
  let hbox =
    GPack.hbox ~homogeneous:false
      ~spacing:6 ~border_width:6 ()
  in
  let button =
    GButton.button
      ~packing:(hbox#pack ~expand:false ~fill:false) ()
  in
  let desat = not n.net_enabled in
  let pixb = Mi.network_pixb n.net_num ~size:A.LARGE ~desat () in
  let _ =
    match pixb with
        Some pixbuf ->
          ignore (GMisc.image ~pixbuf ~packing:button#add ())
      | _ -> ()
  in
  let vbox =
    GPack.vbox ~homogeneous:false ~spacing:3 ~border_width:6 
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let name = Mi.network_full_name n.net_num in
  let markup = create_bold_markup name in
  let label =
    GMisc.label ~markup ~xalign:0.
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_flags =
    GPack.hbox ~homogeneous:false ~spacing:3
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  List.iter (fun (flag, icon) ->
    let desat = not (List.mem flag n.net_flags) in
    let pixbuf = A.get_icon ~icon ~size:A.SMALL ~desat () in
    ignore (GMisc.image ~pixbuf ~packing:(hbox_flags#pack ~expand:false ~fill:true) ())
  ) flags;
  let label = !M.nT_lb_display ^ " " ^ name in
  let check =
    GButton.check_button
      ~active:n.net_displayed ~label ()
  in
  table#attach ~left ~top:!top
    ~xpadding:0 ~ypadding:0
    ~right:(left + 2) ~bottom:(!top + 1)
    ~expand:`X ~fill:`X
    hbox#coerce;
  incr top;
  table#attach ~left ~top:!top
    ~xpadding:0 ~ypadding:0
    ~right:(left + 2) ~bottom:(!top + 1)
    ~expand:`X ~fill:`X
    check#coerce;
  incr top;
  let label_dled = GMisc.label ~xalign:0. () in
  let label_uled = GMisc.label ~xalign:0. () in
  let label_serv_connected = GMisc.label ~xalign:0. () in
  List.iter (fun (s1, s2, label) ->
    let markup = create_markup s1 in
    let l = GMisc.label ~markup ~xalign:0. () in
    table#attach ~left ~top:!top
      ~xpadding:18 ~ypadding:0
      ~right:(left + 1) ~bottom:(!top + 1)
      ~expand:`X ~fill:`X
      l#coerce;
    let markup = create_markup s2 in
    label#set_use_markup true;
    label#set_label markup;
    table#attach ~left:(left + 1) ~top:!top
      ~xpadding:0 ~ypadding:0
      ~right:(left + 2) ~bottom:(!top + 1)
      ~expand:`X ~fill:`X
      label#coerce;
    incr top
  ) [!M.nT_lb_net_dled,           Mi.size_of_int64 n.net_downloaded,   label_dled;
     !M.nT_lb_net_uled,           Mi.size_of_int64 n.net_uploaded,     label_uled;
     !M.nT_lb_net_serv_connected, Printf.sprintf "%d" n.net_connected, label_serv_connected;
    ];
  ignore (button#connect#clicked ~callback:
    (fun _ ->
       enable_network (n.net_num, not n.net_enabled)
  ));
  ignore (check#connect#toggled ~callback:
    (fun _ ->
       n.net_displayed <- check#active;
       networks_filtered :=
         begin
           if n.net_displayed
             then List2.removeq n.net_num !G.networks_filtered
             else n.net_num :: !G.networks_filtered
         end;
       GuiServers.reset_servers_filter ();
       GuiDownloads.reset_downloads_filter ();
       GuiFriends.reset_friends_filter ();
       GuiRooms.reset_rooms_filter ();
       GuiQueries.reset_results_filter ();
       GuiUploads.reset_uploads_filter ();
       GuiUploads.reset_uploaders_filter ()
  ));
  let box = 
    {
     box_button = button;
     box_uploaded = label_uled;
     box_downloaded = label_dled;
     box_connected_servers = label_serv_connected;
    }
  in
  Hashtbl.add boxes n.net_num box;
  (!top + 1)



let networks_box gui =
  let vbox = GPack.vbox ~homogeneous:false ~border_width:6 () in
  ignore (vbox#connect#destroy ~callback:
    (fun _ ->
       Hashtbl.clear boxes;
       Timeout.remove (!networks_timerID)
  ));
  let scroll_table_box =
    GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let table =
    GPack.table ~columns:4 ~homogeneous:false
       ~col_spacings:6 ~border_width:6
       ~packing:scroll_table_box#add_with_viewport ()
  in
  let net_list =
    List.sort (fun (num1, _) (num2, _) ->
      compare (Mi.network_full_name num1) (Mi.network_full_name num2)
    ) (Hashtbl2.to_list2 G.networks)
  in
  let top = ref 0 in
  let left = ref 0 in
  let len = (List.length net_list) / 2 in
  let n = ref 0 in
  List.iter (fun (num, net) ->
    top := display_network table net !left !top;
    incr n;
    if !n > len
       then begin
         left := 2;
         top := 0;
         n := 0;
       end
  ) net_list;
  vbox#coerce
