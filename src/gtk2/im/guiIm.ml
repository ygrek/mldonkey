(* Copyright 2004 b8_bavard *)
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
open ImOptions
open ImAccount
open ImProtocol
open ImEvent
open ImTypes
open ImIdentity
open ImChat
open ImRoom  

module O = GuiOptions
module A = GuiArt
module U = GuiUtf8
module M = GuiMessages

let verbose = !!O.gtk_verbose_im

let lprintf' fmt =
  Printf2.lprintf ("GuiIm: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let quit_on_close = ref false

(*************************************************************************)
(*                                                                       *)
(*                         main_window                                   *)
(*                                                                       *)
(*************************************************************************)

let main_window () =
  let width = (Gdk.Screen.width ()) * 1 / 2 in
  let height = (Gdk.Screen.height ()) * 1 / 2 in
  let window =
    GWindow.window ~width ~height
      ~title:(!M.iM_wt_software)
      ~icon:(A.get_icon ~icon:M.icon_menu_im ~size:A.SMALL ())
      ~modal:false ()
  in
  ignore (window#event#connect#delete ~callback:
    (fun _ ->
       if !quit_on_close
         then CommonGlobals.exit_properly 0
         else window#coerce#misc#hide ();
         true
  ));
  let box =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let menu =
    GMenu.menu_item ~label:!M.iM_me_menu ~use_mnemonic:true
      ~packing:menubar#add ()
  in
  let main_menu = GMenu.menu ~packing:menu#set_submenu () in
  let itemAccounts =
    GMenu.image_menu_item ~label:!M.iM_me_new_accounts ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_mlchat ~size:A.SMALL ()) ())
      ~packing:main_menu#add ()
  in
  let itemQuit =
    GMenu.image_menu_item ~label:!M.iM_me_quit ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_quit ~size:A.SMALL ()) ())
      ~packing:main_menu#add ()
  in
  let new_accounts = GMenu.menu ~packing:itemAccounts#set_submenu () in
  ignore (itemQuit#connect#activate 
    (fun _ -> 
       if !quit_on_close
         then CommonGlobals.exit_properly 0
         else window#coerce#misc#hide ()
  ));
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_menubar in
  let _ = main_menu#set_accel_group accel_menubar in
  itemAccounts#add_accelerator ~group:accel_menubar ~modi:[`CONTROL]
      ~flags:[`VISIBLE] GdkKeysyms._n;
  itemQuit#add_accelerator ~group:accel_menubar ~modi:[`CONTROL]
      ~flags:[`VISIBLE] GdkKeysyms._w;
  let main_notebook =
    GPack.notebook ~tab_pos:`TOP ~show_tabs:true ~homogeneous_tabs:true
    ~show_border:true ~scrollable:true ~enable_popup:true
    ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  main_notebook#append_page
    ~tab_label:((GMisc.label ~text:!M.iM_lb_accounts ())#coerce)
   (GuiImAccounts.accounts_window ());
  ImProtocol.iter (fun p ->
    let menu_item =
      GMenu.menu_item ~label:(Printf.sprintf !M.iM_me_x_account (protocol_name p))
        ~packing:new_accounts#add ()
    in
    ignore (menu_item#connect#activate ~callback:
      (fun _ ->
         let account = protocol_new_account p in
         GuiImAccounts.input_account account;
         ImEvent.add_event (Account_event account);
         if verbose then lprintf' "NEW ACCOUNT\n"
    ))
  );
  ImEvent.set_event_handler (fun event ->
      match event with
      | Account_event account ->
          GuiImAccounts.h_update_account account

      | Account_friend_event id -> ()
(*
          (if verbose then lprintf' "Account_friend_event\n");
          let account = identity_account id in
          begin  try
              let w = find_account_window account in
              (if verbose then lprintf' "Window available\n");
              w#update_contact id
            with _ -> ()  end
*)
      | Chat_open_event chat ->
          GuiImChat.h_open_chat chat

      | Chat_close_event chat ->
          GuiImChat.h_close_chat chat

      | Chat_my_message (chat, msg) -> ()

      | Chat_message_event (chat, id, msg) ->
          GuiImChat.h_chat_message (chat, id, msg)

      | Room_join room ->
          GuiImRooms.h_join_room room main_notebook

      | Room_leave room ->
          GuiImRooms.h_leave_room room

      | Room_public_message (room, _)
      | Room_message (room, _, _)
      | Room_user_join (room, _) 
      | Room_user_leave (room, _) ->
          GuiImRooms.h_room_event room event
  );
  window#show ()




