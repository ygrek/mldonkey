(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

class gui () =
  let box = GPack.vbox ~homogeneous:false () in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let itemfile = GMenu.menu_item ~label:"File" ~packing:(menubar#add) () in
  let menuFile = GMenu.menu ~packing:(itemfile#set_submenu) () in
  let itemOptions =
    GMenu.menu_item ~label:(Chat_messages.m_options) ~packing:(menuFile#add)
      ()
  in
  let itemAddPeople =
    GMenu.menu_item ~label:(Chat_messages.m_add_people)
      ~packing:(menuFile#add) ()
  in
  let itemToggleTemp =
    GMenu.menu_item ~label:(Chat_messages.m_toggle_temp)
      ~packing:(menuFile#add) ()
  in
  let itemOpenDialog =
    GMenu.menu_item ~label:(Chat_messages.m_open_dialog_for_selected_people)
      ~packing:(menuFile#add) ()
  in
  let _ = GMenu.menu_item ~packing:(menuFile#add) () in
  let itemRemovePeople =
    GMenu.menu_item ~label:(Chat_messages.m_remove_people)
      ~packing:(menuFile#add) ()
  in
  let _ = GMenu.menu_item ~packing:(menuFile#add) () in
  let itemQuit =
    GMenu.menu_item ~label:(Chat_messages.m_quit) ~packing:(menuFile#add) ()
  in
  let itemHelp = GMenu.menu_item ~label:"?" ~packing:(menubar#add) () in
  let menuHelp = GMenu.menu ~packing:(itemHelp#set_submenu) () in
  let itemAbout =
    GMenu.menu_item ~label:(Chat_messages.m_about) ~packing:(menuHelp#add) ()
  in
  let accelgroup = GtkData.AccelGroup.create () in
  let _ = menuFile#set_accel_group accelgroup in
  let _ =
    itemOptions#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._O
  in
  let _ =
    itemAddPeople#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._A
  in
  let _ =
    itemToggleTemp#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._T
  in
  let _ =
    itemOpenDialog#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._D
  in
  let _ =
    itemRemovePeople#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._K
  in
  let _ =
    itemQuit#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._Q
  in
  let _ = menuHelp#set_accel_group accelgroup in
  let _anonymous_container_1 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let wlist =
    GList.clist
      ~titles:(
        [""; Chat_messages.id; Chat_messages.host; Chat_messages.port;
         Chat_messages.temporary])
      ~shadow_type:`NONE ~selection_mode:`EXTENDED ~titles_show:true
      ~packing:(_anonymous_container_1#add) ()
  in
  object
    val box = box
    val menubar = menubar
    val accelgroup = accelgroup
    val itemfile = itemfile
    val menuFile = menuFile
    val itemOptions = itemOptions
    val itemAddPeople = itemAddPeople
    val itemToggleTemp = itemToggleTemp
    val itemOpenDialog = itemOpenDialog
    val itemRemovePeople = itemRemovePeople
    val itemQuit = itemQuit
    val itemHelp = itemHelp
    val menuHelp = menuHelp
    val itemAbout = itemAbout
    val wlist = wlist
    method box = box
    method menubar = menubar
    method accelgroup = accelgroup
    method itemfile = itemfile
    method menuFile = menuFile
    method itemOptions = itemOptions
    method itemAddPeople = itemAddPeople
    method itemToggleTemp = itemToggleTemp
    method itemOpenDialog = itemOpenDialog
    method itemRemovePeople = itemRemovePeople
    method itemQuit = itemQuit
    method itemHelp = itemHelp
    method menuHelp = menuHelp
    method itemAbout = itemAbout
    method wlist = wlist
    method coerce = box#coerce
  end
class dialog () =
  let box = GPack.vbox ~homogeneous:false () in
  let wscroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let wt_dialog =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:(wscroll#add) ()
  in
  let wtool =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`ICONS ~space_style:`EMPTY
      ~tooltips:true ~button_relief:`NORMAL
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let wt_input =
    GEdit.text ~height:50 ~editable:true ~word_wrap:true ~line_wrap:true
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let wb_show_hide =
    GButton.button ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let _65 =
    GMisc.label ~text:(Chat_messages.show_hide_people) ~justify:`LEFT
      ~line_wrap:true ~packing:(wb_show_hide#add) ()
  in
  let wscroll_people =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let wlist_people =
    GList.clist
      ~titles:([Chat_messages.id; Chat_messages.host; Chat_messages.port])
      ~shadow_type:`NONE ~selection_mode:`SINGLE ~titles_show:true
      ~packing:(wscroll_people#add) ()
  in
  object
    val box = box
    val wscroll = wscroll
    val wt_dialog = wt_dialog
    val wtool = wtool
    val wt_input = wt_input
    val wb_show_hide = wb_show_hide
    val wscroll_people = wscroll_people
    val wlist_people = wlist_people
    method box = box
    method wscroll = wscroll
    method wt_dialog = wt_dialog
    method wtool = wtool
    method wt_input = wt_input
    method wb_show_hide = wb_show_hide
    method wscroll_people = wscroll_people
    method wlist_people = wlist_people
    method coerce = box#coerce
  end
