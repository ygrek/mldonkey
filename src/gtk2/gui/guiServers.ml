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

(* The servers window of MLgui *)

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

let verbose = O.gtk_verbose_servers

let lprintf' fmt =
  Printf2.lprintf ("GuiServers: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let current_net = ref 0

let (user_label : GMisc.label option ref) = ref None
let (server_label : GMisc.label option ref) = ref None

let net_cols = new GTree.column_list
let net_num = net_cols#add Gobject.Data.int
let net_text = net_cols#add Gobject.Data.string
let net_icon = net_cols#add Gobject.Data.gobject_option
let net_store = GTree.list_store net_cols
let net_model = GTree.model_sort net_store

let _ =
  net_model#set_default_sort_func
    (fun model iter_a iter_b ->
       let a = model#get ~row:iter_a ~column:net_text in
       let b = model#get ~row:iter_b ~column:net_text in
       compare a b
  )

let (view_context : GPango.context option ref) = ref None

(*************************************************************************)
(*                                                                       *)
(*                         server_num                                    *)
(*                                                                       *)
(*************************************************************************)

let server_num key =
  try int_of_string key with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         server_of_key                                 *)
(*                                                                       *)
(*************************************************************************)

let server_of_key key =
  try
    let num = server_num key in
    Hashtbl.find G.servers num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_servers                               *)
(*                                                                       *)
(*************************************************************************)

let keys_to_servers keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = server_of_key k in
      l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         server_key                                    *)
(*                                                                       *)
(*************************************************************************)

let server_key server_num =
  Printf.sprintf "%d" server_num

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

module ServerUsers = GuiUsers.UserList (struct

  let columns = O.servers_users_columns
  let view_context = view_context
  let module_name = "ServerUsers"

end)


let userstore = new ServerUsers.g_user ()

module Servers = GuiTemplates.Gview(struct

  module Column = GuiColumns.Server

  type item = server_info

  let columns = O.servers_columns
  let get_key = (fun s -> server_key s.server_num)
  let module_name = "Servers"

end)

class g_server () =
(* first we create a GTree.column_list *)
  let server_cols         = new GTree.column_list in
(*
 * we fill the newly created GTree.column_list to
 * define the interface between the GTree.model and
 * the  GTree.view.
 *)
  let server_network_str  = server_cols#add Gobject.Data.string in
  let server_name         = server_cols#add Gobject.Data.string in
  let server_ip_port      = server_cols#add Gobject.Data.string in
  let server_state_str    = server_cols#add Gobject.Data.string in
  let server_nusers       = server_cols#add Gobject.Data.int64 in
  let server_nfiles       = server_cols#add Gobject.Data.int64 in
  let server_desc         = server_cols#add Gobject.Data.string in
  let server_tags_str     = server_cols#add Gobject.Data.string in
  let server_score        = server_cols#add Gobject.Data.int in
  let server_network_pixb = server_cols#add Gobject.Data.gobject_option in
  let server_state_pixb   = server_cols#add Gobject.Data.gobject_option in
  let server_preferred    = server_cols#add Gobject.Data.boolean in
  object (self)
(* from this point you cannot change server_cols ! *)
  inherit Servers.g_list server_cols

(*
 * to create an instance from the virtual class g_list of GuiTemplates
 * we have to define its virtual methods.
 *)

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

  method from_item (row : Gtk.tree_iter) (s : server_info) =
    store#set ~row ~column:server_nusers        s.server_nusers;
    store#set ~row ~column:server_nfiles        s.server_nfiles;
    store#set ~row ~column:server_score         s.server_score;
    store#set ~row ~column:server_network_str  (Mi.network_name s.server_network);
    store#set ~row ~column:server_name         (U.utf8_of s.server_name);
    store#set ~row ~column:server_ip_port      (Mi.address_to_string s.server_addr s.server_port);
    store#set ~row ~column:server_state_str    (Mi.string_of_state s.server_state 0);
    store#set ~row ~column:server_tags_str     (Mi.tags_to_string s.server_tags);
    store#set ~row ~column:server_desc         (U.utf8_of s.server_description);
    store#set ~row ~column:server_network_pixb (Mi.network_pixb s.server_network ~size:A.SMALL ());
    store#set ~row ~column:server_state_pixb   (Mi.server_state_of_server s.server_network s.server_state ~size:A.SMALL);
    store#set ~row ~column:server_preferred     s.server_preferred

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

  method from_new_item (row : Gtk.tree_iter) (s : server_info) (s_new : server_info) =
    if s.server_name <> s_new.server_name
      then begin
        store#set ~row ~column:server_name (U.utf8_of s_new.server_name);
      end;
    if s.server_state <> s_new.server_state
      then begin
        store#set ~row ~column:server_state_str (Mi.string_of_state s_new.server_state 0);
        store#set ~row ~column:server_state_pixb (Mi.server_state_of_server s_new.server_network s_new.server_state ~size:A.SMALL)
      end;
    if (s.server_addr, s.server_port) <> (s_new.server_addr, s_new.server_port)
      then begin
        store#set ~row ~column:server_ip_port (Mi.address_to_string s_new.server_addr s_new.server_port);
      end;
    if s.server_nusers <> s_new.server_nusers
      then begin
        store#set ~row ~column:server_nusers s_new.server_nusers;
      end;
    if s.server_nfiles <> s_new.server_nfiles
      then begin
        store#set ~row ~column:server_nfiles s_new.server_nfiles;
      end;
    if s.server_tags <> s_new.server_tags
      then begin
        store#set ~row ~column:server_tags_str (Mi.tags_to_string s_new.server_tags);
      end;
    if s.server_description <> s_new.server_description
      then begin
        store#set ~row ~column:server_desc (U.utf8_of s_new.server_description)
      end;
    if s.server_score <> s_new.server_score
      then begin
        store#set ~row ~column:server_score s_new.server_score
      end;
    if s.server_preferred <> s_new.server_preferred
      then begin
        store#set ~row ~column:server_preferred s_new.server_preferred
      end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

  method content col c =
    let autosize = match col#sizing with `AUTOSIZE -> true | _ -> false in
    match c with
      Col_server_name ->
         begin
           if !!O.gtk_look_use_icons
             then begin
               let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
               col#pack ~expand:false renderer;
               col#add_attribute renderer "pixbuf" server_state_pixb
             end;
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack ~expand:false renderer;
           if autosize
             then col#add_attribute renderer "text" server_name
             else col#set_cell_data_func renderer
               (fun model row ->
                  match !view_context with
                    Some context when col#width > 0 ->
                      begin
                        let width =
                          if !!O.gtk_look_use_icons
                            then (col#width - 4 - !!O.gtk_look_lists_icon_size) - 4 * !G.char_width
                            else col#width - 4 * !G.char_width
                        in
                        let name = model#get ~row ~column:server_name in
                        let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                        renderer#set_properties [ `TEXT s ; `EDITABLE true ];
                        ignore (renderer#connect#edited ~callback:
                          (fun path name ->
                             try
                               let iter = self#get_iter path in
                               let k = self#find_model_key iter in
                               let s = server_of_key k in
                               GuiCom.send (ServerRename (server_num k, name));
                               let row = self#convert_iter_to_child_iter iter in
                               store#set ~row ~column:server_name s.server_name
                             with _ -> ()
                        ));
                      end
                  | _ -> renderer#set_properties [ `TEXT "" ]
             )
         end

    | Col_server_address ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack renderer;
           col#add_attribute renderer "text" server_ip_port
         end

    | Col_server_state ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack renderer;
           col#add_attribute renderer "text" server_state_str
         end

    | Col_server_users ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
           col#pack renderer;
           col#add_attribute renderer "text" server_nusers
         end

    | Col_server_files ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
           col#pack renderer;
           col#add_attribute renderer "text" server_nfiles
         end

    | Col_server_desc ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack renderer;
           col#add_attribute renderer "text" server_desc
         end

    | Col_server_tags ->
         begin
           let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
           col#pack renderer;
           col#add_attribute renderer "text" server_tags_str
         end

    | Col_server_network ->
         begin
           if !!O.gtk_look_use_icons
             then begin
               let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
               col#pack renderer;
               col#add_attribute renderer "pixbuf" server_network_pixb
             end else begin
               let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
               col#pack renderer;
               col#add_attribute renderer "text" server_network_str
             end
         end

    | Col_server_preferred ->
         begin
           let renderer = GTree.cell_renderer_toggle [`XALIGN 0.5] in
           col#pack renderer;
           col#add_attribute renderer "active" server_preferred;
           ignore (renderer#connect#toggled ~callback:
             (fun path ->
                try
                  let iter = self#get_iter path in
                  let k = self#find_model_key iter in
                  let s = server_of_key k in
                  GuiCom.send (ServerSetPreferred (server_num k, not s.server_preferred));
                  let row = self#convert_iter_to_child_iter iter in
                  store#set ~row ~column:server_preferred s.server_preferred
                with _ -> ()
           ));
         end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

  method sort_items c k1 k2 =
    try
      let s1 = server_of_key k1 in
      let s2 = server_of_key k2 in
      match c with
        Col_server_address ->
          begin
            let i = compare s1.server_addr s2.server_addr in
            if i = 0
              then compare s1.server_port s2.server_port
              else i
          end

      | Col_server_state -> compare (Mi.string_of_state s1.server_state 0)
                                    (Mi.string_of_state s2.server_state 0)
      | Col_server_users -> compare s1.server_nusers s2.server_nusers
      | Col_server_files -> compare s1.server_nfiles s2.server_nfiles
      | Col_server_desc -> compare (String.lowercase s1.server_description)
                                   (String.lowercase s2.server_description)
      | Col_server_network -> compare s1.server_network s2.server_network
      | Col_server_name -> compare (String.lowercase s1.server_name)
                                   (String.lowercase s2.server_name)
      | Col_server_tags -> compare s1.server_tags s2.server_tags
      | Col_server_preferred -> compare s1.server_preferred s2.server_preferred
  with _ -> 0

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      let f k row =
        let s = server_of_key k in
        store#set ~row ~column:server_network_pixb (Mi.network_pixb s.server_network ~size:A.SMALL ());
        store#set ~row ~column:server_state_pixb   (Mi.server_state_of_server s.server_network s.server_state ~size:A.SMALL)
      in
      List.iter (fun k ->
        try
          let row = self#find_row k in
          Gaux.may ~f:(f k ) row
        with _ -> ()
      ) (self#all_items ())

  end

let serverstore = new g_server ()

(*************************************************************************)
(*                                                                       *)
(*                         update_servers_label                          *)
(*                                                                       *)
(*************************************************************************)

let update_servers_labels () =
  let _ =
    match !server_label with
        Some label ->
          begin
            let markup =
              create_default_bold_markup
               (Printf.sprintf "%s (%d)" !M.sT_lb_servers !G.nservers)
            in
            label#set_label markup
          end
      | _ -> ()
  in
  GuiStatusBar.update_servers ()

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
             (Printf.sprintf "%s (%d)" !M.sT_lb_users userstore#nitems)
          in
          label#set_label markup
        end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let remove sel () =
  let l = keys_to_servers sel in
  List.iter (fun s ->
    GuiCom.send (RemoveServer_query s.server_num)
  ) l

let connect_to sel () =
  let l = keys_to_servers sel in
  List.iter (fun s ->
    GuiCom.send (ConnectServer s.server_num)
  ) l

let disconnect sel () =
  let l = keys_to_servers sel in
  List.iter (fun s ->
    GuiCom.send (DisconnectServer s.server_num)
  ) l

let view_users sel () =
  let l = keys_to_servers sel in
  List.iter (fun s ->
    GuiCom.send (ViewUsers s.server_num)
  ) l

let add_new_server entry_addr entry_port () =
  try
    let addr = entry_addr#text in
    let port = entry_port#value_as_int in
    let ip = Ip.of_string addr in
    GuiCom.send (AddServer_query (!current_net, ip, port))
  with _ -> ()

let connect_more_servers () =
  GuiCom.send ConnectMore_query

let remove_old_servers () =
  GuiCom.send CleanOldServers

let clear_users sel () =
  let l = keys_to_servers sel in
  List.iter (fun s ->
    match s.server_users with
        None -> ()
      | Some l ->
          begin
            List.iter (fun user_num ->
              Hashtbl.remove G.users user_num
            ) l;
            s.server_users <- None
          end
  ) l;
  userstore#clear (); (* if we call clear_users there is a good chance that some users are currently displaied *)
  update_users_label ()

let get_user_info user_num =
  GuiCom.send (GetUser_info user_num)

let server_set_preferred sel b () =
  let l = keys_to_servers sel in
  List.iter (fun s ->
    GuiCom.send (ServerSetPreferred (s.server_num, b))
  ) l

let server_rename s name () =
  GuiCom.send (ServerRename (s.server_num, name))

(*************************************************************************)
(*                                                                       *)
(*                         server_menu                                   *)
(*                                                                       *)
(*************************************************************************)

let server_menu sel =
  let l =
    match sel with
        [] -> []
      | _ ->
            [
             `I ((!M.sT_me_connect), connect_to sel) ;
             `I ((!M.sT_me_disconnect), disconnect sel) ;
             `I ((!M.sT_me_view_users), view_users sel) ;
             `I ((!M.sT_me_remove), remove sel) ;
             `I ((!M.sT_me_clear_users), clear_users sel);
             `S
            ]
  in
  l @
            [
             `I ((!M.sT_me_connect_more_servers), connect_more_servers) ;
             `I ((!M.sT_me_remove_old_servers), remove_old_servers)
            ]

(*************************************************************************)
(*                                                                       *)
(*                         on_select_server                              *)
(*                                                                       *)
(*************************************************************************)

let on_select_server sel =
  userstore#clear ();
  update_users_label ();
  match sel with
      [] -> ()
    | k :: tail ->
        begin
          try
            let s = server_of_key k in
            match s.server_users with
                None -> (if !!verbose then lprintf' "No user for server %s\n" s.server_name)
              | Some l ->
                  begin
                    List.iter (fun user_num ->
                      try
                        (if !!verbose then lprintf' "Add user %d to list of server %s\n" user_num s.server_name);
                        let u = Hashtbl.find G.users user_num in
                        userstore#add_item u ~f:update_users_label ()
                      with _ -> get_user_info user_num
                    ) l
                  end
          with _ -> ()
        end

(*************************************************************************)
(*                                                                       *)
(*                         filter_server                                 *)
(*                                                                       *)
(*************************************************************************)

let filter_disconnected_servers = ref false

let filter_server k =
  try
    let s = server_of_key k in
    not ((!filter_disconnected_servers &&
           (match s.server_state with
                NotConnected _ 
              | NewHost -> true | _ -> false)) ||
         List.memq s.server_network !G.networks_filtered)
  with _ -> true

(*************************************************************************)
(*                                                                       *)
(*                         Templates initialization                      *)
(*                                                                       *)
(*************************************************************************)

let _ =
  serverstore#set_filter filter_server

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  serverstore#clear ();
  userstore#clear ();
  net_store#clear ();
  current_net := 0;
  update_servers_labels ();
  update_users_label ()

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let hashtbl_server_update s s_new =
  s.server_addr <- s_new.server_addr;
  s.server_port <- s_new.server_port;
  s.server_realport <- s_new.server_realport;
  s.server_score <- s_new.server_score;
  s.server_tags <- s_new.server_tags;
  s.server_nusers <- s_new.server_nusers;
  s.server_nfiles <- s_new.server_nfiles;
  s.server_state <- s_new.server_state;
  s.server_name <- s_new.server_name;
  s.server_description <- s_new.server_description;
  s.server_users <- s_new.server_users;
  s.server_banner <- s_new.server_banner;
  s.server_preferred <- s_new.server_preferred

let remove_server server_num =
  try
    let s = Hashtbl.find G.servers server_num in
    let _ =
      match s.server_users with
          None -> ()
        | Some l ->
            List.iter (fun user_num ->
              Hashtbl.remove G.users user_num
            ) l
    in
    Hashtbl.remove G.servers server_num;
    serverstore#remove_item (server_key server_num);
    decr G.nservers;
    update_servers_labels ()
  with _ -> ()

let update_server serv =
  try
    let s = Hashtbl.find G.servers serv.server_num in
    let row = serverstore#find_row (server_key serv.server_num) in
    let s_new = {serv with server_users = s.server_users} in
    let _ =
      match Mi.is_connected s_new.server_state, Mi.is_connected s.server_state with
          true, false -> incr G.nconnected_servers
        | false, true -> decr G.nconnected_servers
        | _ -> ()
    in
    Gaux.may ~f:(fun r -> serverstore#update_item r s s_new) row;
    hashtbl_server_update s s_new;
    update_servers_labels ()
  with Not_found ->
    begin
      serverstore#add_item serv ~f:update_servers_labels ();
      if Mi.is_connected serv.server_state
        then begin
          incr G.nconnected_servers
        end;
      incr G.nservers;
      Hashtbl.add G.servers serv.server_num serv
    end


let server_info s =
  match s.server_state with
      RemovedHost -> 
        remove_server s.server_num
    | _  ->
        update_server s

let h_server_update_state server_num state =
  try
    let s = Hashtbl.find G.servers server_num in
    let row = serverstore#find_row (server_key server_num) in
    if state = RemovedHost
      then remove_server server_num
      else begin
        let s_new = {s with server_state = state} in
        let _ =
          match Mi.is_connected s_new.server_state, Mi.is_connected s.server_state with
              true, false -> incr G.nconnected_servers
            | false, true -> decr G.nconnected_servers
            | _ -> ()
        in
        Gaux.may ~f:(fun r -> serverstore#update_item r s s_new) row;
        s.server_state <- s_new.server_state
      end

  with Not_found -> GuiCom.send (GetServer_info server_num)

let h_server_busy server_num nusers nfiles =
  try
    let s = Hashtbl.find G.servers server_num in
    let row = serverstore#find_row (server_key server_num) in
    let s_new = {s with server_nusers = nusers;
                        server_nfiles = nfiles}
    in
    Gaux.may ~f:(fun r -> serverstore#update_item r s s_new) row;
    s.server_nusers <- s_new.server_nusers;
    s.server_nfiles <- s_new.server_nfiles
  with Not_found -> GuiCom.send (GetServer_info server_num)

let h_server_update_users server_num user =
  try
    let s = Hashtbl.find G.servers server_num in
    let _row = serverstore#find_row (server_key server_num) in
    match s.server_users with
        None ->
          begin
            s.server_users <- Some [user]
          end
      | Some list ->
          if not (List.mem user list)
            then begin
              s.server_users <- Some (user :: list)
            end

  with Not_found ->
    if server_num <> 0
      then begin
        Hashtbl.remove G.users user;       (* Anyway remove the user. Will be sent back by the core *)
        GuiCom.send (GetServer_info server_num);
        GuiCom.send (GetServer_users server_num)
      end

let clean_servers_table servers =
  let l = serverstore#all_items () in
  (if !!verbose then lprintf' "Cleaning servers\n   servers table : %d\n   new servers   : %d\n"
     (List.length l) (List.length servers));
  List.iter (fun k ->                                              (* the core sends more servers than what the GUI displays.
                                                                    * better to do it this way.
                                                                    *)
    try
      let s = server_of_key k in
      if not (List.mem s.server_num servers)
        then remove_server s.server_num
    with _ -> ()
  ) l;
  if !!verbose
    then begin
      let l = serverstore#all_items () in
      lprintf' "   ----------------------------\n   servers table : %d\n" (List.length l)
    end

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let clean_servers net_num net_enabled =
  if not net_enabled
    then begin
      let l = serverstore#all_items () in
      List.iter (fun k ->
        try
          let s = server_of_key k in
          if s.server_network = net_num
            then remove_server s.server_num
        with _ -> ()
      ) l
    end

let reset_servers_filter () =
  serverstore#refresh_filter ()

(*************************************************************************)
(*                                                                       *)
(*                         on_net_select                                 *)
(*                                                                       *)
(*************************************************************************)

let on_net_select net_num =
  current_net := net_num

(*************************************************************************)
(*                                                                       *)
(*                         renderer_pack_combobox                        *)
(*                                                                       *)
(*************************************************************************)

let renderer_pack_combobox (combobox : GEdit.combo_box)
     ((col_pixb : GdkPixbuf.pixbuf option GTree.column),
      (col_text : string GTree.column),
      (col_num  : int GTree.column)) (f : int -> unit) =
  if !!O.gtk_look_use_icons
    then begin
      let pixb_renderer = GTree.cell_renderer_pixbuf [] in
      combobox#pack pixb_renderer ;
      combobox#add_attribute pixb_renderer "pixbuf" col_pixb
    end;
  let str_renderer = GTree.cell_renderer_text [ `XPAD 6 ] in
  combobox#pack str_renderer;
  combobox#add_attribute str_renderer "text" col_text;
  ignore (combobox#connect#changed ~callback:
    (fun _ ->
      match combobox#active_iter with
          Some row -> 
            begin
              let num = combobox#model#get ~row ~column:col_num in
              f num
            end
        | _ -> ()
  ))

(*************************************************************************)
(*                                                                       *)
(*                         build_net_menu                                *)
(*                                                                       *)
(*************************************************************************)

let build_net_menu () =
  Hashtbl.iter (fun num net ->
    if net.net_enabled && (Mi.net_has_server net)
      then begin
        let row = net_store#append () in
        net_store#set ~row ~column:net_num net.net_num;
        net_store#set ~row ~column:net_text (U.simple_utf8_of net.net_name);
        net_store#set ~row ~column:net_icon (Mi.network_pixb net.net_num ~size:A.SMALL ())
      end
  ) G.networks

(*************************************************************************)
(*                                                                       *)
(*                         servers window                                *)
(*                                                                       *)
(*************************************************************************)

open GMain

let add_server_box (table : GPack.table) =
  let hbox_add_s =
    GPack.hbox ~homogeneous:false
      ~spacing:6 ~border_width:6 ()
  in
  let pixbuf = A.get_icon ~icon:M.icon_stock_add_server ~size:A.SMALL () in
  let _img =
    GMisc.image ~pixbuf ~xalign:0.
      ~packing:(hbox_add_s#pack ~expand:false ~fill:true) ()
  in
  let markup = create_bold_markup !M.sT_lb_add_server in
  let _label =
    GMisc.label ~markup ~xalign:0.
      ~packing:(hbox_add_s#pack ~expand:false ~fill:true) ()
  in
  let markup = create_markup !M.sT_lb_server_ip in
  let label_ip_addr = GMisc.label ~markup ~xalign:0. () in
  let markup = create_markup !M.sT_lb_server_port in
  let label_port = GMisc.label ~markup ~xalign:0. () in
  let net_combo = GEdit.combo_box ~model:net_model () in
  let entry_ip_addr = GEdit.entry ~width:100 () in
  let range = GData.adjustment ~lower:1. ~upper:65535. ~step_incr:1. () in
  let entry_port =
    GEdit.spin_button ~adjustment:range ~rate:1. ~digits:0
      ~numeric:true ~snap_to_ticks:true ~update_policy:`IF_VALID
      ~width:60 ~wrap:true ()
  in
  let wtool = tool_bar `HORIZONTAL ~layout:`END () in
  let markup = create_markup !M.sT_lb_server_add in
  let bAdd_server = wtool#add_button
      ~style:`TEXT
      ~markup
      ~f:(add_new_server entry_ip_addr entry_port) ()
  in
  bAdd_server#misc#set_sensitive false;
  ignore (entry_ip_addr#connect#changed ~callback:
    (fun _ ->
       try
         let b = (Ip.valid (Ip.of_string entry_ip_addr#text)) in
         bAdd_server#misc#set_sensitive b
       with _ -> bAdd_server#misc#set_sensitive false
  ));
  let top = ref 0 in
  List.iter (fun data ->
    List.iter (fun (w, (left, right, xpadding)) ->
      table#attach ~left ~top:!top
        ~right ~bottom:(!top + 1)
        ~xpadding ~ypadding:0 
        ~shrink:`X ~fill:`X
        w
    ) data;
    incr top
  ) [
     [(hbox_add_s#coerce,    (0, 2, 0 ))];
     [(net_combo#coerce,     (0, 2, 18))];
     [(label_ip_addr#coerce, (0, 1, 18)); (label_port#coerce, (1, 2, 0))];
     [(entry_ip_addr#coerce, (0, 1, 18)); (entry_port#coerce, (1, 2, 0))];
     [(wtool#coerce,         (0, 2, 0 ))];
    ];

  renderer_pack_combobox net_combo (net_icon, net_text, net_num) on_net_select;
  net_combo#set_active 0


let servers_box gui =
  let vpaned_servers =
    GPack.paned `VERTICAL ~border_width:6 ()
  in
  build_net_menu ();
  ignore (vpaned_servers#connect#destroy ~callback:
    (fun _ ->
      view_context := None;
      userstore#clear ();
      net_store#clear ();
      current_net := 0;
      user_label := None;
      server_label := None;
  ));
  let vbox_servers =
    GPack.vbox ~homogeneous:false
      ~packing:vpaned_servers#add1 ()
  in
  let hbox = 
    GPack.hbox ~homogeneous:false ~spacing:6
      ~packing:vpaned_servers#add2 ()
  in
  let vbox_users =
    GPack.vbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in

  let servers_evbox =
    GBin.event_box ~packing:(vbox_servers#pack ~expand:false ~fill:true) ()
  in
  servers_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let servers_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:servers_evbox#add ()
  in
  let users_evbox =
    GBin.event_box ~packing:(vbox_users#pack ~expand:false ~fill:true) ()
  in
  users_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let users_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:users_evbox#add ()
  in

  let serverview = 
    Servers.treeview ~mode:`MULTIPLE
      ~packing:(vbox_servers#pack ~expand:true ~fill:true) ()
  in
  view_context := Some serverview#view#misc#pango_context;
  serverview#set_model serverstore#gmodel;
  serverview#set_menu server_menu;
  serverview#set_on_select on_select_server;
  let userview = 
    ServerUsers.treeview ~mode:`MULTIPLE 
      ~packing:(vbox_users#pack ~expand:true ~fill:true) ()
  in
  userview#set_model userstore#gmodel;
  userview#set_menu GuiUsers.user_menu;

  let vbox_cmd =
    GPack.vbox ~homogeneous:false ~spacing:12
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_tb =
    GPack.hbox ~homogeneous:false ~spacing:6 ~border_width:6
      ~packing:(vbox_cmd#pack ~expand:false ~fill:true) ()
  in
  let t_button =
    GButton.toggle_button
      ~packing:(hbox_tb#pack ~expand:false ~fill:false) ()
  in
  t_button#set_active (not !filter_disconnected_servers);
  let box =
    GPack.hbox ~homogeneous:false ~spacing:6
      ~packing:t_button#add ()
  in
  let pixbuf =
    A.get_icon ~size:A.MEDIUM
      ~icon:(M.icon_stock_all_servers) ()
  in
  let _img =
    GMisc.image ~pixbuf ~xalign:0.
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let markup = create_markup !M.sT_lb_display_all_servers in
  let _label =
    GMisc.label ~markup ~xalign:0. ~use_underline:true
      ~mnemonic_widget:t_button#coerce
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let frame =
    GBin.frame ~border_width:6 ~shadow_type:`ETCHED_IN
    ~packing:(vbox_cmd#pack ~expand:false ~fill:true) ()
  in
  let table_server =
    GPack.table ~columns:2 ~homogeneous:false
       ~row_spacings:6 ~col_spacings:2
       ~border_width:6 ~packing:frame#add ()
  in
  add_server_box table_server;

  ignore (t_button#connect#toggled ~callback:
    (fun _ ->
       filter_disconnected_servers := not t_button#active;
       serverstore#refresh_filter ()
  ));

  GuiTools.set_vpaned vpaned_servers O.servers_vpane_up;
  GuiTools.get_vpaned vpaned_servers O.servers_vpane_up;

  servers_label#set_use_markup true;
  users_label#set_use_markup true;
  server_label := Some servers_label;
  user_label := Some users_label;
  update_servers_labels ();
  update_users_label ();

  vpaned_servers#coerce
