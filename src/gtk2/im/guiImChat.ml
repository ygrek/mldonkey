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

module U = GuiUtf8
module O = GuiOptions
module A = GuiArt
module M = GuiMessages

let verbose = !!O.gtk_verbose_im

let lprintf' fmt =
  Printf2.lprintf ("GuiImChat: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Types                                         *)
(*                                                                       *)
(*************************************************************************)

type im_chat =
  {
    buffer : GuiTemplates.chat_buffer;
    view   : GuiTemplates.chat_view;
  }

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let chats = Hashtbl.create 13

(*************************************************************************)
(*                                                                       *)
(*                         messages to the core                          *)
(*                                                                       *)
(*************************************************************************)

let on_entry_return chat s =
  (if verbose then lprintf' "SEND MESSAGE %s\n" s);
  let len = String.length s in
  let s = 
    if len <= 0 then ""
      else
        match s.[0] with
            '\n' -> String.sub s 1 (len - 1)
          | _ -> s
  in
  chat_send chat s

(*************************************************************************)
(*                                                                       *)
(*                         dialog                                        *)
(*                                                                       *)
(*************************************************************************)

let dialog chat =
  let chat_buf = GuiTemplates.chat_buffer ~on_entry:(on_entry_return chat) () in
  let chatview =
    GuiTemplates.chat_view ~extended:true ~buffer:chat_buf
      ~my_name:(account_name (chat_account chat)) ()
  in
  {buffer = chat_buf; view = chatview}

(*************************************************************************)
(*                                                                       *)
(*                         new_dialog                                    *)
(*                                                                       *)
(*************************************************************************)

let new_dialog chat =
  let width = (Gdk.Screen.width ()) * 3 / 10 in
  let height = (Gdk.Screen.height ()) * 3 / 10 in
  let window =
    GWindow.window ~width ~height
      ~icon:(A.get_icon ~icon:M.icon_menu_mlchat ~size:A.SMALL ())
      ~allow_grow:false ~allow_shrink:false
      ~resizable:false ~modal:false ~border_width:6
      ~title:(U.utf8_of (chat_name chat)) ()
  in
  let dialog = dialog chat in
  ignore (window#connect#destroy ~callback:
    (fun _ ->
       chat_close chat
  ));
  window#add dialog.view#coerce;
  window, dialog

(*************************************************************************)
(*                                                                       *)
(*                         find_chat_window                              *)
(*                                                                       *)
(*************************************************************************)

let find_chat_window chat =
  try
    let (w, d) = Hashtbl.find chats (chat_num chat) in
    w#present ();
    (w, d)
  with _ ->
    begin
      let w = new_dialog chat in
      Hashtbl.add chats (chat_num chat) w;
      w
    end

(*************************************************************************)
(*                                                                       *)
(*                         messages from the core                        *)
(*                                                                       *)
(*************************************************************************)

let h_open_chat chat =
  let (w, _) = find_chat_window chat in
  w#show ()

let h_close_chat chat =
  try
    let num = chat_num chat in
    let (w, d) = Hashtbl.find chats num in
    Hashtbl.remove chats num;
    d.view#clear ();
    d.view#destroy ();
    w#destroy ();
    d.buffer#clear ()
  with _ -> ()

let h_chat_message (chat, id, msg) =
  let (_, d) = find_chat_window chat in
  d.buffer#insert_text msg (identity_name id) ~priv:true ()
