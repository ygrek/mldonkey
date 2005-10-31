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
open GuiColumns

module U = GuiUtf8
module O = GuiOptions
module A = GuiArt
module M = GuiMessages

let verbose = !!O.gtk_verbose_im

let lprintf' fmt =
  Printf2.lprintf ("GuiImAccounts: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (act_by_num : (int, account) Hashtbl.t) = Hashtbl.create 13

(*************************************************************************)
(*                                                                       *)
(*                         account_pixb                                  *)
(*                                                                       *)
(*************************************************************************)

let account_pixb ac =
  if !!O.gtk_look_use_icons
    then begin
      let size = A.SMALL in
      let pixb =
        match (account_status ac) with
            Status_offline -> A.get_icon ~icon:M.icon_menu_mlchat ~size ~desat:true ()
          | _ -> A.get_icon ~icon:M.icon_menu_mlchat ~size ()
      in
      Some pixb
    end else None

(*************************************************************************)
(*                                                                       *)
(*                         string_of_status                              *)
(*                                                                       *)
(*************************************************************************)

let string_of_status status =
  match status with
  | Status_online Online_available -> !M.iM_tx_online
  | Status_online Online_away -> !M.iM_tx_online_away
  | Status_connecting -> !M.iM_tx_connecting
  | Status_offline -> !M.iM_tx_offline

(*************************************************************************)
(*                                                                       *)
(*                         act_num                                       *)
(*                                                                       *)
(*************************************************************************)

let act_num key =
  try int_of_string key with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         act_of_key                                    *)
(*                                                                       *)
(*************************************************************************)

let act_of_key key =
  try
    let num = act_num key in
    Hashtbl.find act_by_num num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_acts                                  *)
(*                                                                       *)
(*************************************************************************)

let keys_to_acts keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = act_of_key k in
      l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         act_key                                       *)
(*                                                                       *)
(*************************************************************************)

let act_key act_num =
  Printf.sprintf "%d" act_num

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

module Accounts = GuiTemplates.Gview(struct

  module Column = GuiColumns.IMAccount

  type item = account

  let columns = O.account_columns
  let get_key = (fun ac -> act_key (account_num ac))
  let module_name = "IM Accounts"

end)


class g_account () =
  let ac_cols      = new GTree.column_list in
  let ac_name      = ac_cols#add Gobject.Data.string in
  let ac_status    = ac_cols#add Gobject.Data.string in
  let ac_protocol  = ac_cols#add Gobject.Data.string in
  let ac_name_pixb = ac_cols#add Gobject.Data.gobject_option in
  object
  inherit Accounts.g_list ac_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

  method from_item (row : Gtk.tree_iter) (ac : account) =
    store#set ~row ~column:ac_name      (U.utf8_of (account_name ac));
    store#set ~row ~column:ac_status    (U.simple_utf8_of (string_of_status (account_status ac)));
    store#set ~row ~column:ac_protocol  (U.simple_utf8_of (protocol_name (account_protocol ac)));
    store#set ~row ~column:ac_name_pixb (account_pixb ac)

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

  method from_new_item (row : Gtk.tree_iter) (ac : account) (ac_new : account) =
    Printf2.lprintf "    ** Account name: %s | %s\n    ** Account status %s | %s\n    ** Account protocol %s | %s\n"
      (account_name ac) (account_name ac_new)
      (string_of_status (account_status ac)) (string_of_status (account_status ac_new))
      (protocol_name (account_protocol ac)) (protocol_name (account_protocol ac_new));
    store#set ~row ~column:ac_name (U.utf8_of (account_name ac_new));
    store#set ~row ~column:ac_status (U.simple_utf8_of (string_of_status (account_status ac_new)));
    store#set ~row ~column:ac_name_pixb (account_pixb ac_new);
    store#set ~row ~column:ac_protocol (U.simple_utf8_of (protocol_name (account_protocol ac_new)))

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

  method content col c =
    match c with
      Col_account_name ->
         begin
           if !!O.gtk_look_use_icons
             then begin
               let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
               col#pack ~expand:false renderer;
               col#add_attribute renderer "pixbuf" ac_name_pixb
             end;
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack ~expand:false renderer;
           col#add_attribute renderer "text" ac_name
         end

    | Col_account_status ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack renderer;
           col#add_attribute renderer "text" ac_status
         end

    | Col_account_protocol ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack renderer;
           col#add_attribute renderer "text" ac_protocol
         end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

  method sort_items c k1 k2 =
    try
      let ac1 = act_of_key k1 in
      let ac2 = act_of_key k2 in
      match c with
        Col_account_name -> compare (account_name ac1) (account_name ac2)
      | Col_account_status -> compare (account_status ac1) (account_status ac2)
      | Col_account_protocol -> compare (account_protocol ac1) (account_protocol ac2)
    with _ -> 0

  end

let accountstore = new g_account ()


(*************************************************************************)
(*                                                                       *)
(*                         input_record                                  *)
(*                                                                       *)
(*************************************************************************)

module C = ConfigWindow

let preference ?(help="") label v box_type () =
  {
    C.pref_section = None;
    C.opt_section = "";
    C.pref_subsection = None;
    C.pref_help = help;
    C.pref_advanced = false;
    C.pref_default = v;
    C.pref_name = label;
    C.pref_label = label;
    C.pref_group = None;
    C.pref_option_list = [];
    C.pref_value = v;
    C.pref_new_value = v;
    C.pref_type = box_type;
    C.pref_apply = (fun () -> ());
    C.pref_apply_default = (fun () -> ());
  }

let input_record record account =
  let rec iter list params refs =
    match list with 
      (_, name, _, from_record, to_record) :: tail ->
        let param, f = 
          match from_record, to_record with
            FromString fs, ToString ts ->
              let pref = preference name (ts ()) C.BString () in
              pref, (fun _ -> fs pref.C.pref_new_value)

          | FromBool fb, ToBool tb ->
              let pref = preference name (string_of_bool (tb ())) C.BBool () in
              pref, (fun _ -> fb (C.safe_bool pref.C.pref_new_value))

          | FromInt fi, ToInt ti ->
              let pref = preference name (string_of_int (ti ())) C.BInt () in
              pref, (fun _ -> fi (int_of_float (C.safe_int pref.C.pref_new_value)))

          | _ -> assert false
        in
        iter tail (param :: params) (f :: refs)
    | [] ->
        begin
          let prefs = (List.rev params) in
          let on_ok () =
            List.iter (fun f -> f ()) refs;
            add_event (Account_event account);
            Options.save_with_help accounts_ini
          in
          C.simple_panel ~prefs 
              ~title:!M.iM_lb_new_account
              ~icon:(A.get_icon ~icon:M.icon_menu_mlchat ~size:A.SMALL ())
              ~on_ok ()
        end
  in 
  iter record [] []


(*************************************************************************)
(*                                                                       *)
(*                         input_account                                 *)
(*                                                                       *)
(*************************************************************************)

let input_account account =
  try
    input_record (account_config_record account) account
  with e ->
      if verbose then lprintf' "Execption %s in input_account\n"
        (Printexc2.to_string e)

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let settings sel () =
  let l = keys_to_acts sel in
  List.iter (fun account ->
     input_account account
  ) l

let connect sel () =
  let l = keys_to_acts sel in
  List.iter (fun account ->
    match account_status account with
        Status_offline -> account_login account
      | _ -> account_logout account
  ) l

let remove sel () = ()

let ask_for_room account =
  let pref = preference !M.im_lb_room_name "" C.BString () in
  let on_ok () =
    account_join_room account pref.C.pref_new_value
  in
  C.input_window ~pref
      ~title:!M.iM_lb_join_room
      ~icon:(A.get_icon ~icon:M.icon_menu_mlchat ~size:A.SMALL ())
      ~on_ok ()

(*************************************************************************)
(*                                                                       *)
(*                         account_menu                                  *)
(*                                                                       *)
(*************************************************************************)

let account_menu sel =
  match sel with
      [] -> []
    | k :: tail ->
        begin
          let basic_menu = 
            [
              `I (!M.iM_me_connect_disconnect, connect sel) ;
              `I (!M.iM_me_settings, settings sel) ;
              `I (!M.iM_me_remove, remove sel) ;
            ]
          in
          try
            let account = act_of_key k in
            if tail = [] && account_has_rooms account then
              (`I (!M.iM_me_join_room, (fun _ -> ask_for_room account)))::
              (let prefered_rooms = account_prefered_rooms account in
               if prefered_rooms = []
                 then []
                 else
                   [ `M (!M.iM_me_prefered_rooms,
                        List.map (fun name ->
                            `I ((U.utf8_of name), (fun _ -> 
                                  account_join_room account name))
                        ) prefered_rooms)]
                ) @basic_menu
              else basic_menu
            with _ -> basic_menu
          end

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let h_update_account account =
  lprintf' "h_update_account %d\n" (account_num account);
  try
    let ac = act_of_key (act_key (account_num account)) in
    let row = accountstore#find_row (act_key (account_num account)) in
    lprintf' "Updating Account %d\n" (account_num account);
    accountstore#update_item row ac account;
    lprintf' "Updated Account %d\n" (account_num account)
  with _ ->
    lprintf' "Adding Account %d\n" (account_num account);
    ignore (accountstore#add_item account);
    lprintf' "Added Account %d\n" (account_num account)

(*************************************************************************)
(*                                                                       *)
(*                         accounts_window                               *)
(*                                                                       *)
(*************************************************************************)

let accounts_window () =
  let accountview = Accounts.treeview ~mode:`MULTIPLE () in
  accountview#set_model accountstore#gmodel;
  accountview#set_menu account_menu;
  accountview#coerce
