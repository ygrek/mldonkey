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

(* The users window of MLgui *)

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

(*************************************************************************)
(*                                                                       *)
(*                         user_num                                      *)
(*                                                                       *)
(*************************************************************************)

let user_num key =
  try int_of_string key with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         user_of_key                                   *)
(*                                                                       *)
(*************************************************************************)

let user_of_key key =
  try
    let num = user_num key in
    Hashtbl.find G.users num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_users                                 *)
(*                                                                       *)
(*************************************************************************)

let keys_to_users keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = user_of_key k in
      l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         user_key                                      *)
(*                                                                       *)
(*************************************************************************)

let user_key user_num =
  Printf.sprintf "%d" user_num

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let add_to_friends sel ()  =
  let l = keys_to_users sel in
  List.iter (fun u ->
    GuiCom.send (AddUserFriend u.user_num)
  ) l

let browse_files sel () =
  let l = keys_to_users sel in
  List.iter (fun u ->
    GuiCom.send (BrowseUser u.user_num)
  ) l


(*************************************************************************)
(*                                                                       *)
(*                         user_menu                                     *)
(*                                                                       *)
(*************************************************************************)

let user_menu sel =
  match sel with
      [] -> []
    | _ ->
          [
           `I (!M.rT_me_add_to_friends, add_to_friends sel) ;
           `I (!M.rT_me_browse_files, browse_files sel) ;
          ]

(*************************************************************************)
(*                                                                       *)
(*                         hashtbl_users_update                          *)
(*                                                                       *)
(*************************************************************************)

let hashtbl_users_update u u_new =
  u.user_tags <- u_new.user_tags


module UserList(R:

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Argument                              *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

    sig

      val columns      : (user_column * float) list Options.option_record
      val view_context : GPango.context option ref
      val module_name  : string

    end) = 
  (struct

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

    module Users = GuiTemplates.Gview(struct

      module Column = GuiColumns.User

      type item = user_info

      let columns = R.columns
      let get_key = (fun u -> user_key u.user_num)
      let module_name = R.module_name
    end)

    class g_user () =
      let user_cols     = new GTree.column_list in
      let user_name     = user_cols#add Gobject.Data.string in
      let user_md4      = user_cols#add Gobject.Data.string in
      let user_ip_port  = user_cols#add Gobject.Data.string in
      let user_tags     = user_cols#add Gobject.Data.string in
      object

        inherit Users.g_list user_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

        method from_item row (u : user_info) =
          store#set ~row ~column:user_name (U.utf8_of u.user_name);
          store#set ~row ~column:user_ip_port (Mi.ip_to_string u.user_ip u.user_port);
          store#set ~row ~column:user_md4 (Md4.to_string u.user_md4);
          store#set ~row ~column:user_tags (Mi.tags_to_string u.user_tags)

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

        method from_new_item (row : Gtk.tree_iter) (u : user_info) (u_new : user_info) =
          if u.user_tags <> u_new.user_tags
            then begin
              store#set ~row ~column:user_tags (Mi.tags_to_string u_new.user_tags);
            end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

        method content (col : GTree.view_column) c =
          let autosize = match col#sizing with `AUTOSIZE -> true | _ -> false in
          match c with
              Col_user_name ->
                begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  if autosize
                    then col#add_attribute renderer "text" user_name
                    else col#set_cell_data_func renderer
                      (fun model row ->
                         match !R.view_context with
                           Some context when col#width > 0 ->
                             begin
                               let width = col#width - 4 * !G.char_width in
                               let name = model#get ~row ~column:user_name in
                               let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                               renderer#set_properties [ `TEXT s ]
                             end
                         | _ -> renderer#set_properties [ `TEXT "" ]
                      )
                end

            | Col_user_addr ->
                begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" user_ip_port
                end

            | Col_user_tags ->
                begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" user_tags
                end

            | Col_user_md4 ->
                begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" user_md4
                end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

        method sort_items c k1 k2 =
          try
            let u1 = user_of_key k1 in
            let u2 = user_of_key k2 in
            match c with
              Col_user_name -> compare (String.lowercase u1.user_name) (String.lowercase u2.user_name)
            | Col_user_addr -> compare u1.user_ip u2.user_ip
            | Col_user_tags -> compare u1.user_tags u2.user_tags
            | Col_user_md4 -> compare u1.user_md4 u2.user_md4
          with _ -> 0

      end


(*************************************************************************)
(*                                                                       *)
(*                         shortcut                                      *)
(*                                                                       *)
(*************************************************************************)

    let treeview = Users.treeview

  end)
