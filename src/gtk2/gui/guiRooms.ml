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

(* The rooms window of MLgui *)

open GuiTypes2
open GuiTypes
open CommonTypes

open GuiTools
open GuiGlobal
open GuiColumns
open Md4
open GuiProto

module M = GuiMessages
module Mi = GuiMisc
module O = GuiOptions
module G = GuiGlobal
module A = GuiArt
module U = GuiUtf8

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)
let (<:>) = GuiTools.(<:>)

let verbose = O.gtk_verbose_rooms

let lprintf' fmt =
  Printf2.lprintf ("GuiRooms: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (dialogs : (int * GuiTemplates.chat_buffer) list ref) = ref []

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let (current_room : int option ref) = ref None
let (chat_box : GuiTemplates.chat_view option ref) = ref None

let (room_label : GMisc.label option ref) = ref None
let (user_label : GMisc.label option ref) = ref None

let nrooms_opened = ref 0
let (view_context : GPango.context option ref) = ref None

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

module RoomUsers = GuiUsers.UserList (struct

  let columns = O.rooms_users_columns
  let view_context = view_context
  let module_name = "RoomUsers"

end)

let userstore = new RoomUsers.g_user ()

module Rooms = GuiTemplates.Gview(struct

  module Column = GuiColumns.Room

  type item = room_info
  type key = int

  let columns = O.rooms_columns
  let get_key = (fun r -> r.room_num)
  let module_name = "Rooms"

end)

class g_room () =
  let room_cols         = new GTree.column_list in
  let room_network_str  = room_cols#add Gobject.Data.string in
  let room_network_pixb = room_cols#add Gobject.Data.gobject_option in
  let room_name         = room_cols#add Gobject.Data.string in
  let room_name_pixb    = room_cols#add Gobject.Data.gobject_option in
  let room_state_str    = room_cols#add Gobject.Data.string in
  let room_nusers       = room_cols#add Gobject.Data.int in
  object (self)
    inherit Rooms.g_list room_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

    method from_item row ro =
      store#set ~row ~column:room_network_str (Mi.network_name ro.room_network);
      store#set ~row ~column:room_network_pixb (Mi.network_pixb ro.room_network ~size:A.SMALL ());
      store#set ~row ~column:room_name (U.utf8_of ro.room_name);
      store#set ~row ~column:room_name_pixb (Mi.room_state_to_icon ro.room_state ~size:A.SMALL);
      store#set ~row ~column:room_state_str (Mi.room_state_to_string ro.room_state);
      store#set ~row ~column:room_nusers ro.room_nusers

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

    method from_new_item row ro ro_new =
      if ro.room_state <> ro_new.room_state
        then begin
          store#set ~row ~column:room_name_pixb (Mi.room_state_to_icon ro_new.room_state ~size:A.SMALL);
          store#set ~row ~column:room_state_str (Mi.room_state_to_string ro_new.room_state);
        end;
      if ro.room_nusers <> ro_new.room_nusers
        then begin
          store#set ~row ~column:room_nusers ro_new.room_nusers
        end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

    method content col c =
      match c with
          Col_room_name ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
                  col#pack ~expand:false renderer;
                  col#add_attribute renderer "pixbuf" room_name_pixb
                end;
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack ~expand:false renderer;
              col#set_cell_data_func renderer
                (fun model row ->
                   match !view_context with
                       Some context when col#width > 0 ->
                         begin
                           let width =
                             if !!O.gtk_look_use_icons
                               then (col#width - 4 - !!O.gtk_look_lists_icon_size) - 4 * !G.char_width
                               else col#width - 4 * !G.char_width
                           in
                           let name = model#get ~row ~column:room_name in
                           let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                           renderer#set_properties [ `TEXT s ]
                         end
                  | _ -> renderer#set_properties [ `TEXT "" ]
                )
            end

        | Col_room_nusers ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" room_nusers
            end

        | Col_room_state ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" room_state_str
            end

        | Col_room_network ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "pixbuf" room_network_pixb
                end else begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" room_network_str
                end
            end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

    method sort_items c ro1 ro2 =
      match c with
          Col_room_name -> compare (String.lowercase ro1.room_name) (String.lowercase ro2.room_name)
        | Col_room_nusers -> compare ro1.room_nusers ro2.room_nusers
        | Col_room_state -> compare ro1.room_state ro2.room_state
        | Col_room_network -> compare ro1.room_network ro2.room_network

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      List.iter (fun ro ->
        try
          let (row, _) = self#find_item ro.room_num in 
          store#set ~row ~column:room_network_pixb (Mi.network_pixb ro.room_network ~size:A.SMALL ());
          store#set ~row ~column:room_name_pixb (Mi.room_state_to_icon ro.room_state ~size:A.SMALL);
        with _ -> ()
      ) (self#all_items ())

  end

let roomstore = new g_room ()


(*************************************************************************)
(*                                                                       *)
(*                         update_rooms_label                            *)
(*                                                                       *)
(*************************************************************************)

let update_rooms_label () =
  match !room_label with
      Some label ->
        begin
          let markup =
            create_default_bold_markup
             (Printf.sprintf "%s (%d / %d)" !M.rT_lb_rooms !nrooms_opened roomstore#nitems)
          in
          label#set_label markup
        end
      | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         update_users_label                            *)
(*                                                                       *)
(*************************************************************************)

let update_users_label () =
  match !user_label with
      Some label ->
        begin
          let markup =
            create_default_bold_markup
             (Printf.sprintf "%s (%d)" !M.rT_lb_users userstore#nitems)
          in
          label#set_label markup
        end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let close_open_room sel () =
  List.iter (fun ro ->
    match ro.room_state with
        RoomOpened -> GuiCom.send (SetRoomState (ro.room_num, RoomClosed))
      | _ -> GuiCom.send (SetRoomState (ro.room_num, RoomOpened))
  ) sel

let on_entry_return num s =
  GuiCom.send (SendMessage (num, PublicMessage (0, s)))

let get_user_info user_num =
  GuiCom.send (GetUser_info user_num)

(*************************************************************************)
(*                                                                       *)
(*                         room_menu                                     *)
(*                                                                       *)
(*************************************************************************)

let room_menu (sel : room_info list) =
  match sel with
      [] -> []
    | _ ->
          [
           `I (!M.rT_me_close_open_room, close_open_room sel) ;
          ]

(*************************************************************************)
(*                                                                       *)
(*                         on_select_room                                *)
(*                                                                       *)
(*************************************************************************)

let on_select_room (sel : room_info list) =
  try
    userstore#clear ();
    update_users_label ();
    (if !!verbose then lprintf' "Searching box for chats\n");
    let box =
      match !chat_box with
          Some w -> (w#clear (); w)
        | _ -> raise Exit
    in
    match sel with
        [] -> (if !!verbose then lprintf' "No room selected\n")
      | ro :: tail ->
          begin
            current_room := Some ro.room_num;
            match ro.room_state with
                RoomOpened  ->
                  begin
                    List.iter (fun user_num ->
                      try
                        let u = Hashtbl.find G.users user_num in
                        ignore (userstore#add_item u);
                      with _ -> get_user_info user_num
                    ) ro.room_users;
                    update_users_label ();
                    try
                      let chat_buf = List.assoc ro.room_num !dialogs in
                      box#set_buffer chat_buf
                    with _ -> (if !!verbose then lprintf' "No chat dialog availabale\n")
                  end
              | _  -> (if !!verbose then lprintf' "room_users empty\n")

          end

  with _ -> (if !!verbose then lprintf' "No chat_box found\n")

(*************************************************************************)
(*                                                                       *)
(*                         on_double_click_room                          *)
(*                                                                       *)
(*************************************************************************)

let on_double_click_room (ro : room_info) =
  close_open_room [ro] ()

(*************************************************************************)
(*                                                                       *)
(*                         filter_room                                   *)
(*                                                                       *)
(*************************************************************************)

let filter_room (ro : room_info) =
  not (List.memq ro.room_network !G.networks_filtered)

(*************************************************************************)
(*                                                                       *)
(*                         Templates initialization                      *)
(*                                                                       *)
(*************************************************************************)

let _ =
  roomstore#set_filter filter_room

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  List.iter (fun (_, chat_buf) ->
    chat_buf#clear ()
  ) !dialogs;
  let _ =
    match !chat_box with
        None -> ()
      | Some box -> box#clear ()
  in
  dialogs := [];
  current_room := None;
  userstore#clear ();
  roomstore#clear ();
  nrooms_opened := 0;
  update_rooms_label ();
  update_users_label ()

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let add_chat_to_room ro =
  match ro.room_state with
      RoomOpened ->
        begin
          let chat_buf = GuiTemplates.chat_buffer ~on_entry:(on_entry_return ro.room_num) () in
          dialogs := (ro.room_num, chat_buf) :: !dialogs;
          incr nrooms_opened;
          update_rooms_label ()
        end
    | _ ->
        begin
          try
            let chat_buf = List.assoc ro.room_num !dialogs in
            chat_buf#clear ();
            dialogs := List.remove_assoc ro.room_num !dialogs;
            List.iter (fun user_num ->
              Hashtbl.remove G.users user_num
            ) ro.room_users;
            let num = match !current_room with Some n -> n | _ -> assert false in
            let box = match !chat_box with Some w -> w | _ -> assert false in
            if num = ro.room_num
              then begin
                box#clear ();
                userstore#clear ();
                decr nrooms_opened;
                update_rooms_label ();
                update_users_label ()
              end
          with _ -> ()
        end

let add_room ro =
  if ro.room_num <> 0
    then begin
      (if !!verbose then lprintf' "Adding room %s num: %d\n" ro.room_name ro.room_num);
      add_chat_to_room ro;
      ignore (roomstore#add_item ro);
      update_rooms_label ()
    end

let room_info r =
  try
    (if !!verbose then lprintf' "Room_info of %s\n" r.room_name);
    let (row, ro) = roomstore#find_item r.room_num in
    (* no need to keep ro.room_messages, it is stored in dialogs *)
    let ro_new = {r with room_users = ro.room_users} in
    (if ro_new.room_state <> ro.room_state
      then add_chat_to_room ro_new);
    roomstore#update_item row ro ro_new
  with _ -> add_room r

let find_user_name user_num =
  try
    let u = Hashtbl.find G.users user_num in
    u.user_name
  with _ -> raise Not_found

let message_from_server s =
  let len = String.length s in
  if len > 0
    then begin
      match s.[0] with
          '<' ->
            begin
              try 
                let pos = String.index s '>' in
                let u = String.sub s 1 (pos - 1) in
                let mes = String.sub s (pos + 1) (len - pos - 1) in
                (mes, u, false)
              with _ -> (s, "From server", false)
            end
        | _ -> (s, "From server", false)

    end else ("", "From server", false)

let add_room_message room_num msg =
  (if !!verbose then lprintf' "Adding message to room %d\n" room_num);
  try
    let chat_buf = List.assoc room_num !dialogs in
    let (mes, name, priv) =
      match msg with
          ServerMessage s -> message_from_server s
        | PublicMessage (n, s) -> (s, (find_user_name n), false)
        | PrivateMessage (n, s) -> (s, (find_user_name n), true)
    in
    chat_buf#insert_text mes name ~priv ();
    (* no need to store [msg] in the record field room_messages, it is stored in dialogs *)
  with _ -> ()

let remove_room_user room_num user_num =
  (if !!verbose then lprintf' "Removing user to room %d\n" room_num);
  try
    let (row, ro) = roomstore#find_item room_num in
    (if List.mem user_num ro.room_users
      then ro.room_users <- List.filter (fun n -> n <> user_num) ro.room_users);
    let _ =
      match (!current_room, ro.room_state) with
          (Some n, RoomOpened) when n = room_num ->
             begin
               try
                 let u = Hashtbl.find G.users user_num in
                 userstore#remove_item u;
                 update_users_label ();
               with _ -> ()
             end
        | _ -> ()
    in
    Hashtbl.remove G.users user_num         (* remove the user from G.users if it exists *)
  with _ -> 
    begin
      (if !!verbose then lprintf' "room not found in remove_user ... removing user %d\n" user_num);
      Hashtbl.remove G.users user_num       (* Anyway remove the user if the room does'nt exist *)
    end

let add_room_user room_num user_num =
  (if !!verbose then lprintf' "Adding user to room %d\n" room_num);
  try
    let (row, ro) = roomstore#find_item room_num in
    (if not (List.mem user_num ro.room_users)
      then ro.room_users <- user_num :: ro.room_users);
    match (!current_room, ro.room_state) with
        (Some n, RoomOpened) when n = room_num ->
           begin
             try
               let u = Hashtbl.find G.users user_num in
               ignore (userstore#add_item u);
               update_users_label ()
             with _ -> ()
           end
      | _ -> ()

  with _ ->
    begin
      (if !!verbose then lprintf' "room not found in add_user ... removing user %d\n" user_num);
      Hashtbl.remove G.users user_num       (* remove the user if the room does'nt exist *)
    end

let update_user_info u_new =
  match !current_room with
      Some room_num ->
         begin
           try
             let (_, ro) = roomstore#find_item room_num in
             if List.mem u_new.user_num ro.room_users
               then begin
                 try
                   let (row, u) = userstore#find_item u_new.user_num in
                   userstore#update_item row u u_new
                 with _ -> ()
               end
           with _ -> ()
         end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let reset_rooms_filter () =
  roomstore#refresh_filter ()

(*************************************************************************)
(*                                                                       *)
(*                         rooms window                                  *)
(*                                                                       *)
(*************************************************************************)

open GMain

let rooms_box gui =
  let hpaned_rooms =
    GPack.paned `HORIZONTAL ()
  in
  let hpaned_users =
    GPack.paned `HORIZONTAL
      ~packing:hpaned_rooms#add2 ()
  in
  ignore (hpaned_rooms#connect#destroy ~callback:
    (fun _ ->
       view_context := None;
       chat_box := None;
       current_room := None;
       userstore#clear ();
       room_label := None;
       user_label := None;
  ));
  let vbox_rooms =
    GPack.vbox ~homogeneous:false  ~border_width:6  ~spacing:6
      ~packing:hpaned_rooms#add1 ()
  in
  let vbox_users =
    GPack.vbox ~homogeneous:false ~border_width:6  ~spacing:6
      ~packing:hpaned_users#add1 ()
  in
  let vbox_chat =
    GPack.vbox ~border_width:6 ~spacing:6
      ~packing:hpaned_users#add2 ()
  in

  let rooms_evbox =
    GBin.event_box ~packing:(vbox_rooms#pack ~expand:false ~fill:true) ()
  in
  rooms_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let rooms_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:rooms_evbox#add ()
  in
  let users_evbox =
    GBin.event_box ~packing:(vbox_users#pack ~expand:false ~fill:true) ()
  in
  users_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let users_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:users_evbox#add ()
  in
  let chat_evbox =
    GBin.event_box ~packing:(vbox_chat#pack ~expand:false ~fill:true) ()
  in
  chat_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let markup = GuiTools.create_default_bold_markup !M.rT_lb_chat in
  let chat_label =
    GMisc.label ~xalign:0. ~yalign:0. ~markup
      ~xpad:3 ~ypad:3 ~packing:chat_evbox#add ()
  in

  let roomview =
    Rooms.treeview ~mode:`MULTIPLE
      ~packing:(vbox_rooms#pack ~expand:true ~fill:true) ()
  in
  view_context := Some roomview#view#misc#pango_context;
  roomview#set_model roomstore#gmodel;
  roomview#set_menu room_menu;
  roomview#set_on_select on_select_room;
  roomview#set_on_double_click on_double_click_room;
  let userview =
    RoomUsers.treeview ~mode:`MULTIPLE
      ~packing:(vbox_users#pack ~expand:true ~fill:true) ()
  in
  userview#set_model userstore#gmodel;
  userview#set_menu GuiUsers.user_menu;
  let chat_view =
    GuiTemplates.chat_view ~extended:true ~my_name:!G. client_name
      ~packing:(vbox_chat#pack ~expand:true ~fill:true) ()
  in

  GuiTools.set_hpaned hpaned_rooms O.rooms_hpane_left;
  GuiTools.get_hpaned hpaned_rooms O.rooms_hpane_left;
  GuiTools.set_hpaned hpaned_users O.rooms_hpane2_left;
  GuiTools.get_hpaned hpaned_users O.rooms_hpane2_left;

  chat_box := Some chat_view;

  rooms_label#set_use_markup true;
  room_label := Some rooms_label;
  update_rooms_label ();

  users_label#set_use_markup true;
  user_label := Some users_label;
  update_users_label ();

  hpaned_rooms#coerce
