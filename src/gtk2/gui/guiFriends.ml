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

(* The friends window of MLgui *)

open GuiTypes2
open GuiTypes
open CommonTypes

open GuiTools
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

let verbose = O.gtk_verbose_friends

let lprintf' fmt =
  Printf2.lprintf ("GuiFriends: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (dialogs : (int * GuiTemplates.chat_buffer) list ref) = ref []
let source_has_file = ref []

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let (view_context : GPango.context option ref) = ref None
let (wnote_chat : GPack.notebook option ref) = ref None
let (friend_label : GMisc.label option ref) = ref None
let (result_label : GMisc.label option ref) = ref None

let folder_opened () =
  if !!O.gtk_look_use_icons
    then Some (A.get_icon ~icon:M.icon_stock_directory_open ~size:A.SMALL ())
    else None

let folder_closed () =
  if !!O.gtk_look_use_icons
    then Some (A.get_icon ~icon:M.icon_stock_directory ~size:A.SMALL ())
    else None

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

module Friends = GuiTemplates.Gview(struct

  module Column = GuiColumns.Friend

  type item = source_info
  type key = int

  let columns = O.friends_columns
  let get_key = (fun s -> s.source_num)
  let module_name = "Friends"

end)

class g_friend () =
  let friend_cols         = new GTree.column_list in
  let friend_network      = friend_cols#add Gobject.Data.string in
  let friend_network_pixb = friend_cols#add Gobject.Data.gobject_option in
  let friend_name         = friend_cols#add Gobject.Data.string in
  let friend_type_pixb    = friend_cols#add Gobject.Data.gobject_option in
  let friend_state_pixb   = friend_cols#add Gobject.Data.gobject_option in
  let friend_color        = friend_cols#add Gobject.Data.string in
  object (self)
    inherit Friends.g_list friend_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

    method from_item row (s : source_info) =
      store#set ~row ~column:friend_network (Mi.network_name s.source_network);
      store#set ~row ~column:friend_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
      store#set ~row ~column:friend_name s.source_name;
      store#set ~row ~column:friend_type_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
      store#set ~row ~column:friend_state_pixb (Mi.client_state_to_icon false ~size:A.SMALL);
      store#set ~row ~column:friend_color !!O.gtk_color_default

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

    method from_new_item (row : Gtk.tree_iter) (s : source_info) (s_new : source_info) =
      let update_has_file =
        try
          not (List.assoc s_new.source_num !source_has_file)
        with _ -> false
      in
      if s.source_name <> s_new.source_name
        then begin
          store#set ~row ~column:friend_name s_new.source_name;
        end;
      if s.source_type <> s_new.source_type
        then begin
          store#set ~row ~column:friend_type_pixb (Mi.source_type_to_icon s_new.source_type ~size:A.SMALL)
        end;
      if update_has_file
        then begin
          store#set ~row ~column:friend_state_pixb
            (Mi.client_state_to_icon update_has_file ~size:A.SMALL);
          store#set ~row ~column:friend_color !!O.gtk_color_state_files_listed
        end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

    method content (col : GTree.view_column) c =
      match c with
          Col_friend_name ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer_type = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
                  col#pack ~expand:false renderer_type;
                  col#add_attribute renderer_type "pixbuf" friend_type_pixb;
                  let renderer_state = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
                  col#pack ~expand:false renderer_state;
                  col#add_attribute renderer_state "pixbuf" friend_state_pixb
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
                              then (col#width - 2 * 4 - 2 * !!O.gtk_look_lists_icon_size) - 4 * !G.char_width
                              else col#width - 2 * 4 * !G.char_width
                          in
                          let name = model#get ~row ~column:friend_name in
                          let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                          renderer#set_properties [ `TEXT s ]
                        end
                      | _ -> renderer#set_properties [ `TEXT "" ]
              );
              col#add_attribute renderer "foreground" friend_color;
            end

        | Col_friend_network ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "pixbuf" friend_network_pixb
                end else begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" friend_network;
                  col#add_attribute renderer "foreground" friend_color;
                end
            end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

    method sort_items c s1 s2 =
      match c with
          Col_friend_network -> compare s1.source_network s2.source_network
        | Col_friend_name -> compare s1.source_name s2.source_name

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      List.iter (fun s ->
        try
          let (row, _) = self#find_item s.source_num in 
          store#set ~row ~column:friend_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
          store#set ~row ~column:friend_type_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
          store#set ~row ~column:friend_state_pixb (Mi.client_state_to_icon false ~size:A.SMALL);
          let rec iter items =
            match items with
                [] -> ()
              | (GTreeDirectory tree) :: tail ->
                   begin
                     tree.g_file_tree_pixb <- folder_closed ();
                     iter tail
                   end
              | (GTreeFile r) :: tail ->
                  begin
                    r.res_network_pixb <- Mi.network_pixb r.res_num ~size:A.SMALL ();
                    r.res_name_pixb <- Mi.file_type_of_name r.res_name ~size:A.SMALL;
                    iter tail
                  end
          in
          match s.source_files with
              None -> ()
            | Some ft ->
                begin
                  ft.g_file_tree_pixb <- folder_closed ();
                  iter ft.g_file_tree_list
                end
        with _ -> ()
      ) (self#all_items ())

  end

module FriendResults = GuiResults.ResultList(struct

  let columns = O.friends_results_columns
  let view_context = view_context
  let module_name = "FriendResults"

end)

module FriendFolders = GuiTemplates.Gview(struct

  module Column = GuiColumns.Directory

  type item = g_file_tree
  type key = int

  let columns = O.friends_dirs_columns
  let get_key = (fun ft -> ft.g_file_tree_num)
  let module_name = "FriendFolders"

end)

class g_folder () =
  let folder_cols = new GTree.column_list in
  let folder_name = folder_cols#add Gobject.Data.string in
  let folder_pixb = folder_cols#add Gobject.Data.gobject_option in
  object
    inherit FriendFolders.g_tree folder_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

    method from_item row (ft : g_file_tree) =
      store#set ~row ~column:folder_name ft.g_file_tree_name;
      store#set ~row ~column:folder_pixb ft.g_file_tree_pixb

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

    method from_new_item row (ft : g_file_tree) (ft_new : g_file_tree) =
      if ft.g_file_tree_pixb != ft_new.g_file_tree_pixb
        then begin
          store#set ~row ~column:folder_pixb ft_new.g_file_tree_pixb
        end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

    method content (col : GTree.view_column) c =
      begin
        if !!O.gtk_look_use_icons
          then begin
            let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.; `XPAD 4] in
            col#pack ~expand:false renderer;
            col#add_attribute renderer "pixbuf" folder_pixb
          end;
        let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
        col#pack ~expand:false renderer;
        col#add_attribute renderer "text" folder_name
      end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

    method sort_items _ ft1 ft2 =
      compare ft1.g_file_tree_name ft2.g_file_tree_name

  end

let friendstore = new g_friend ()
let folderstore = new g_folder ()
let resultstore = new FriendResults.g_result ()

(*************************************************************************)
(*                                                                       *)
(*                         update_friends_label                          *)
(*                                                                       *)
(*************************************************************************)

let update_friends_label () =
  match !friend_label with
      Some label ->
        begin
          let markup =
            create_default_bold_markup
             (Printf.sprintf "%s (%d)" !M.fT_lb_friends friendstore#nitems)
          in
          label#set_label markup
        end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         update_results_label                          *)
(*                                                                       *)
(*************************************************************************)

let update_results_label () =
  match !result_label with
      Some label ->
        begin
          let markup =
            create_default_bold_markup
             (Printf.sprintf "%s (%d)" !M.qT_lb_results resultstore#nitems)
          in
          label#set_label markup
        end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         add_chat                                      *)
(*                                                                       *)
(*************************************************************************)

let add_chat (notebook : GPack.notebook) (s : source_info) (chat : GuiTemplates.chat_view) =
  let main_evbox = GBin.event_box () in
  main_evbox#add chat#coerce;
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let wl =
    GMisc.label ~text:(U.utf8_of s.source_name)
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let evbox = GBin.event_box ~packing:(hbox#pack ~expand:false ~fill:false) () in
  let pixbuf = A.get_icon ~icon:M.icon_stock_close_overlay ~size:A.SMALL () in
  ignore (GMisc.image ~pixbuf ~xalign:1.0 ~yalign:0. ~packing:evbox#add ());
  evbox#misc#set_state `ACTIVE;
  let on_closure () =
    chat#clear ();
    main_evbox#destroy ();
    dialogs := List.remove_assoc s.source_num !dialogs
  in
  GuiTools.add_key ~key:GdkKeysyms._w ~target:main_evbox ~f:on_closure ~mods:[`CONTROL] ();
  ignore (evbox#event#connect#button_press ~callback:
    (fun ev ->
      GdkEvent.get_type ev = `BUTTON_PRESS &&
      GdkEvent.Button.button ev = 1 &&
      (
       on_closure ();
       true
      )
  ));
  notebook#append_page ~tab_label:hbox#coerce main_evbox#coerce

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let remove sel () = 
  List.iter (fun s ->
    GuiCom.send (RemoveFriend s.source_num);
    (* there is a #bug here or is it volontary ? The core doesn't send back the updated client_type !!! *)
    GuiCom.send (GuiProto.GetClient_info s.source_num)
  ) sel

let remove_all_friends () = 
  GuiCom.send RemoveAllFriends;
  (* there is a #bug here or is it volontary ? The core doesn't send back the updated client_type !!! *)
  List.iter (fun s ->
    GuiCom.send (GuiProto.GetClient_info s.source_num)
  ) (friendstore#all_items ())


let find_friend () =
  match GToolbox.input_string (!M.fT_wt_find_friend) (!M.fT_lb_name) with
      None -> ()
    | Some s -> GuiCom.send (FindFriend s)

let on_entry_return num s =
  GuiCom.send (MessageToClient (num, s))

let download_selected_dir (sel : g_file_tree list) () =
  match sel with
      [] -> ()
    | ft :: tail ->
        begin
          let files = Mi.list_files ft in
          let len = List.length files in
          match (GToolbox.question_box
              (!M.qT_wt_download_selected_dir)
              [ !M.pW_lb_ok ; !M.pW_lb_cancel]
              (U.utf8_of (Printf.sprintf !M.qT_lb_confirm_download_dir 
                 len ft.g_file_tree_name))) 
          with
            1 ->
              List.iter (fun r -> 
                GuiCom.send (Download_query ([r.res_name], r.res_num, false))
              ) files
          | _ -> ()
        end

let show_details s () =
  let item = Source (s, 0) in
  GuiInfoWindow.window item ()

(*************************************************************************)
(*                                                                       *)
(*                         friend_menu                                   *)
(*                                                                       *)
(*************************************************************************)

let friend_menu (sel : source_info list) =
  [
   `I (!M.fT_me_find_friend, find_friend) ;
   `I (!M.fT_me_remove_all_friends, remove_all_friends)
  ] @
  (match sel with
      [] -> []
    | s :: tail ->
               [
                `S;
                `I ((!M.dT_me_show_source_details), show_details s) ;
                `I (!M.fT_me_remove, remove sel) ;
               ])

(*************************************************************************)
(*                                                                       *)
(*                         on_select_friend                              *)
(*                                                                       *)
(*************************************************************************)

let rec insert_dir ft id ?parent () =
  ft.g_file_tree_num <- !id;
  let parent = folderstore#add_item ft ?parent () in
  incr id;
  match ft.g_file_tree_list with
      [] -> ()
    | _ ->
        List.iter (fun tree_item ->
          match tree_item with
              GTreeFile _ -> ()
            | GTreeDirectory d -> insert_dir d id ~parent ()
        ) ft.g_file_tree_list

let on_select_friend (sel : source_info list) =
  resultstore#clear ();
  folderstore#clear ();
  update_results_label ();
  match sel with
      [] -> ()
    | s :: tail ->
        match s.source_files with
            None -> (GuiCom.send (GetClient_files s.source_num))
          | Some ft ->
            begin
              let id = ref 1 in
              insert_dir ft id ()
            end

(*************************************************************************)
(*                                                                       *)
(*                         on_double_click_friend                        *)
(*                                                                       *)
(*************************************************************************)

let on_double_click_friend (s : source_info) =
  match !wnote_chat with
      Some w when not (List.mem_assoc s.source_num !dialogs) ->
        begin
          let buffer = GuiTemplates.chat_buffer ~on_entry:(on_entry_return s.source_num) () in
          let chat = GuiTemplates.chat_view ~buffer  ~my_name:!G.client_name () in
          dialogs := (s.source_num, buffer) :: !dialogs;
          add_chat w s chat
        end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         filter_friend                                 *)
(*                                                                       *)
(*************************************************************************)

let filter_friend s = not (
  List.memq s.source_network !G.networks_filtered
  )

(*************************************************************************)
(*                                                                       *)
(*                         folder_menu                                   *)
(*                                                                       *)
(*************************************************************************)

let folder_menu (sel : g_file_tree list) =
  match sel with
      [] -> []
    | _ ->
          [
           `I (!M.qT_me_download_directory, download_selected_dir sel);
          ]

(*************************************************************************)
(*                                                                       *)
(*                         on_select_folder                              *)
(*                                                                       *)
(*************************************************************************)

let on_select_folder (sel : g_file_tree list) =
  resultstore#clear ();
  update_results_label ();
  match sel with
      [] -> ()
    | ft :: tail ->
        begin
          List.iter (fun tree_item ->
            match tree_item with
                GTreeFile r -> ignore (resultstore#add_item r)
              | _ -> ()
          ) ft.g_file_tree_list;
          update_results_label ()
        end

(*************************************************************************)
(*                                                                       *)
(*                         on_expanded_folder                            *)
(*                                                                       *)
(*************************************************************************)

let on_expanded_folder path (ft : g_file_tree) =
  let (row, _) = folderstore#find_item ft.g_file_tree_num in
  let ft_new = {ft with g_file_tree_pixb = folder_opened ()} in
  folderstore#update_item row ft ft_new

(*************************************************************************)
(*                                                                       *)
(*                         on_collapsed_folder                           *)
(*                                                                       *)
(*************************************************************************)

let on_collapsed_folder path (ft : g_file_tree) =
  let (row, _) = folderstore#find_item ft.g_file_tree_num in
  let ft_new = {ft with g_file_tree_pixb = folder_closed ()} in
  folderstore#update_item row ft ft_new

(*************************************************************************)
(*                                                                       *)
(*                         Templates initialization                      *)
(*                                                                       *)
(*************************************************************************)

let _ =
  friendstore#set_filter filter_friend;
  resultstore#set_filter GuiResults.filter_result

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  List.iter (fun (_, chat_buf) -> chat_buf#clear ()) !dialogs;
  let _ =
    match !wnote_chat with
        None -> ()
      | Some w ->
          begin
            let n = ref 0 in
            List.iter (fun _ ->
              w#remove_page !n;
              incr n
            ) !dialogs
          end
  in
  dialogs := [];
  source_has_file := [];
  resultstore#clear ();
  folderstore#clear ();
  friendstore#clear ();
  update_friends_label ();
  update_results_label ()

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let remove_friend s =
  friendstore#remove_item s;
  update_friends_label ();
  if List.mem_assoc s.source_num !source_has_file
    then source_has_file := List.remove_assoc s.source_num !source_has_file

let h_update_friend s_new =
  try
    let (row, s) = friendstore#find_item s_new.source_num in
    if client_browsed_tag land s_new.source_type = 0
      then remove_friend s
      else friendstore#update_item row s s_new
 with _ ->
    if client_browsed_tag land s_new.source_type <> 0
      then begin
        ignore (friendstore#add_item s_new);
        update_friends_label ()
      end

let add_friend_files (source_num , dirname, result_num) =
  try
    let r = Hashtbl.find G.results result_num in
    try
      let (_, s_new) = friendstore#find_item source_num in
      let tree =
        match s_new.source_files with
            None -> {g_file_tree_num = 0; g_file_tree_list = []; g_file_tree_name = ""; g_file_tree_pixb = folder_closed ()}
          | Some tree -> { tree with g_file_tree_list = tree.g_file_tree_list }
      in
      Mi.add_file tree dirname r;
      s_new.source_files <- Some tree;
      Hashtbl.remove G.results result_num;
      (if not (List.mem_assoc source_num !source_has_file)
         then source_has_file := (source_num, false) :: !source_has_file);
      h_update_friend s_new;
      if not (List.assoc source_num !source_has_file)
        then begin
          source_has_file := List.remove_assoc source_num !source_has_file;
          source_has_file := (source_num, true) :: !source_has_file;
        end
    with _ ->
      Hashtbl.remove G.results result_num (* remove the result if friend does'nt exist *)
(*      GuiCom.send (GetClient_info source_num);
      GuiCom.send (GetClient_files source_num); *)
  with _ -> ()

let add_message_from_client source_num mes =
  try
    let s = Hashtbl.find G.sources source_num in
    try
      let chat_buf = List.assoc s.source_num !dialogs in
      chat_buf#insert_text mes s.source_name ()
    with _ ->
      begin
        let buffer = GuiTemplates.chat_buffer ~on_entry:(on_entry_return source_num) () in
        buffer#insert_text mes s.source_name ();
        dialogs := (s.source_num, buffer) :: !dialogs;
        match !wnote_chat with
            None -> ()
          | Some w ->
              begin
                let chat = GuiTemplates.chat_view ~buffer ~my_name:!G.client_name () in
                add_chat w s chat
              end
      end
  with _ -> raise Not_found


let clean_friends_table friend =
  remove_friend friend

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let reset_friends_filter () =
  friendstore#refresh_filter ()

(*************************************************************************)
(*                                                                       *)
(*                         friends window                                *)
(*                                                                       *)
(*************************************************************************)

open GMain

let friends_box gui =
  let hpaned_friends =
    GPack.paned `HORIZONTAL ()
  in
  ignore (hpaned_friends#connect#destroy ~callback:
    (fun _ ->
       friend_label := None;
       result_label := None;
       view_context := None;
       wnote_chat := None;
       folderstore#clear ();
       resultstore#clear ();
  ));
  let vbox_friends =
    GPack.vbox ~homogeneous:false  ~border_width:6
      ~packing:hpaned_friends#add1 ()
  in
  let vpaned_friends =
    GPack.paned `VERTICAL
      ~packing:hpaned_friends#add2 ()
  in
  let hpaned_dirs =
    GPack.paned `HORIZONTAL
      ~packing:vpaned_friends#add1 ()
  in
  let vbox_messages =
    GPack.vbox ~border_width:6
    ~packing:vpaned_friends#add2 ()
  in
  let vbox_folders =
    GPack.vbox ~homogeneous:false ~border_width:6 
      ~packing:hpaned_dirs#add1 ()
  in
  let vbox_results =
    GPack.vbox ~homogeneous:false ~border_width:6 
      ~packing:hpaned_dirs#add2 ()
  in

  let friends_evbox =
    GBin.event_box ~packing:(vbox_friends#pack ~expand:false ~fill:true) ()
  in
  friends_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let friends_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:friends_evbox#add ()
  in
  let folders_evbox =
    GBin.event_box ~packing:(vbox_folders#pack ~expand:false ~fill:true) ()
  in
  folders_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let markup = GuiTools.create_default_bold_markup !M.qT_lb_directories in
  let folders_label =
    GMisc.label ~xalign:0. ~yalign:0. ~markup
      ~xpad:3 ~ypad:3 ~packing:folders_evbox#add ()
  in
  let results_evbox =
    GBin.event_box ~packing:(vbox_results#pack ~expand:false ~fill:true) ()
  in
  results_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let results_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:results_evbox#add ()
  in
  let messages_evbox =
    GBin.event_box ~packing:(vbox_messages#pack ~expand:false ~fill:true) ()
  in
  messages_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let markup = GuiTools.create_default_bold_markup !M.fT_lb_messages in
  let messages_label =
    GMisc.label ~xalign:0. ~yalign:0. ~markup
      ~xpad:3 ~ypad:3 ~packing:messages_evbox#add ()
  in

  let friendview =
    Friends.treeview ~mode:`MULTIPLE
      ~packing:(vbox_friends#pack ~expand:true ~fill:true) ()
  in
  view_context := Some friendview#view#misc#pango_context;
  friendview#set_model friendstore#gmodel;
  friendview#set_menu friend_menu;
  friendview#set_on_select on_select_friend;
  friendview#set_on_double_click on_double_click_friend;
  let folderview =
    FriendFolders.treeview ~mode:`MULTIPLE
      ~packing:(vbox_folders#pack ~expand:true ~fill:true) ()
  in
  folderview#set_model folderstore#gmodel;
  folderview#set_menu folder_menu;
  folderview#set_on_select on_select_folder;
  folderview#set_on_collapsed on_collapsed_folder;
  folderview#set_on_expanded on_expanded_folder;
  let resultview =
    FriendResults.treeview ~mode:`MULTIPLE
      ~packing:(vbox_results#pack ~expand:true ~fill:true) ()
  in
  resultview#set_model resultstore#gmodel;
  resultview#set_menu GuiResults.result_menu;

  let note =
    GPack.notebook ~homogeneous_tabs:false ~show_border:true
      ~scrollable:true ~enable_popup:false
      ~packing:(vbox_messages#pack ~expand:true ~fill:true) ()
  in
  List.iter (fun (source_num, buffer) ->
    try
      let s = Hashtbl.find G.sources source_num in
      let chat = GuiTemplates.chat_view ~buffer ~my_name:!G. client_name () in
      add_chat note s chat
    with _ -> ()
  ) !dialogs;

  GuiTools.set_hpaned hpaned_friends O.friends_hpane_left;
  GuiTools.get_hpaned hpaned_friends O.friends_hpane_left;
  GuiTools.set_vpaned vpaned_friends O.friends_vpane_up;
  GuiTools.get_vpaned vpaned_friends O.friends_vpane_up;
  GuiTools.set_hpaned hpaned_dirs O.friends_hpane_dirs;
  GuiTools.get_hpaned hpaned_dirs O.friends_hpane_dirs;

  wnote_chat := Some note;

  friends_label#set_use_markup true;
  friend_label := Some friends_label;
  update_friends_label ();
  results_label#set_use_markup true;
  result_label := Some results_label;
  update_results_label ();

  hpaned_friends#coerce
