

class window () =
  let window =
    GWindow.window ~width:400 ~height:200 ~title:(Gui_messages.iM_wt_software)
    ~allow_shrink:true ~allow_grow:true 
(* ~auto_shrink:true *)
    ~modal:false ()
  in
  let box =
    GPack.vbox ~width:600 ~height:440 ~homogeneous:false ~packing:(window#add)
    ()
  in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let _Menu =
    GMenu.menu_item ~label:(Gui_messages.iM_me_file)
    ~packing:(menubar#add) ()
  in
  let _FileMenu = GMenu.menu ~packing:(_Menu#set_submenu) () in
  let itemOptions =
    GMenu.menu_item ~label:(Gui_messages.iM_me_settings)
    ~packing:(_FileMenu#add) ()
  in
  let _ = GMenu.menu_item ~packing:(_FileMenu#add) () in
  let itemSetStatus =
    GMenu.menu_item ~label:"Change Status" ~packing:(_FileMenu#add) ()
  in
  let menuSetStatus = GMenu.menu ~packing:(itemSetStatus#set_submenu) () in
  let itemSetStatusOffline =
    GMenu.menu_item ~label:"Offline" ~packing:(menuSetStatus#add) ()
  in
  let itemAddFriend =
    GMenu.menu_item ~label:"Add Friend" ~packing:(_FileMenu#add) ()
  in
  let _ = GMenu.menu_item ~packing:(_FileMenu#add) () in
  let itemQuit =
    GMenu.menu_item ~label:(Gui_messages.iM_me_quit)
    ~packing:(_FileMenu#add) ()
  in
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_menubar in
  let _ = _FileMenu#set_accel_group accel_menubar in
  let _ = menuSetStatus#set_accel_group accel_menubar in
  let friends =
    GPack.hbox ~homogeneous:false ~packing:(box#pack ~expand:true ~fill:true)
    ()
  in
  let hbox_status =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let label_connect_status =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0)
    ~yalign:(-1.0) ~packing:(hbox_status#pack ~expand:true ~fill:true) ()
  in
  object
    val window = window
    val box = box
    val menubar = menubar
    val accel_menubar = accel_menubar
    val itemOptions = itemOptions
    val itemSetStatus = itemSetStatus
    val menuSetStatus = menuSetStatus
    val itemSetStatusOffline = itemSetStatusOffline
    val itemAddFriend = itemAddFriend
    val itemQuit = itemQuit
    val friends = friends
    val hbox_status = hbox_status
    val label_connect_status = label_connect_status
    method window = window
    method box = box
    method menubar = menubar
    method accel_menubar = accel_menubar
    method itemOptions = itemOptions
    method itemSetStatus = itemSetStatus
    method menuSetStatus = menuSetStatus
    method itemSetStatusOffline = itemSetStatusOffline
    method itemAddFriend = itemAddFriend
    method itemQuit = itemQuit
    method friends = friends
    method hbox_status = hbox_status
    method label_connect_status = label_connect_status
end
class accounts () =
  let window =
    GWindow.window ~width:400 ~height:200 ~title:(Gui_messages.iM_wt_software)
    ~allow_shrink:true ~allow_grow:true 
(* ~auto_shrink:true *)
    ~modal:false ()
  in
  let box =
    GPack.vbox ~width:600 ~height:440 ~homogeneous:false ~packing:(window#add)
    ()
  in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let _Menu =
    GMenu.menu_item ~label:(Gui_messages.iM_me_file)
    ~packing:(menubar#add) ()
  in
  let _FileMenu = GMenu.menu ~packing:(_Menu#set_submenu) () in
  let itemOptions =
    GMenu.menu_item ~label:(Gui_messages.iM_me_settings)
    ~packing:(_FileMenu#add) ()
  in
  let _ = GMenu.menu_item ~packing:(_FileMenu#add) () in
  let itemQuit =
    GMenu.menu_item ~label:(Gui_messages.iM_me_quit)
    ~packing:(_FileMenu#add) ()
  in
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_menubar in
  let _ = _FileMenu#set_accel_group accel_menubar in
  let friends =
    GPack.hbox ~homogeneous:false ~packing:(box#pack ~expand:true ~fill:true)
    ()
  in
  let hbox_status =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let label_connect_status =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0)
    ~yalign:(-1.0) ~packing:(hbox_status#pack ~expand:true ~fill:true) ()
  in
  object
    val window = window
    val box = box
    val menubar = menubar
    val accel_menubar = accel_menubar
    val itemOptions = itemOptions
    val itemQuit = itemQuit
    val friends = friends
    val hbox_status = hbox_status
    val label_connect_status = label_connect_status
    method window = window
    method box = box
    method menubar = menubar
    method accel_menubar = accel_menubar
    method itemOptions = itemOptions
    method itemQuit = itemQuit
    method friends = friends
    method hbox_status = hbox_status
    method label_connect_status = label_connect_status
end
class window2 () =
  let window =
    GWindow.window ~width:640 ~height:400 ~title:(Gui_messages.iM_wt_software)
    ~allow_shrink:true ~allow_grow:true 
    (* ~auto_shrink:true *) ~modal:false ()
  in
  let box =
    GPack.vbox ~width:600 ~height:440 ~homogeneous:false ~packing:(window#add)
    ()
  in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let _Menu =
    GMenu.menu_item ~label:(Gui_messages.iM_me_file)
    ~packing:(menubar#add) ()
  in
  let _FileMenu = GMenu.menu ~packing:(_Menu#set_submenu) () in
  let itemOptions =
    GMenu.menu_item ~label:(Gui_messages.iM_me_settings)
    ~packing:(_FileMenu#add) ()
  in
  let _ = GMenu.menu_item ~packing:(_FileMenu#add) () in
  let itemQuit =
    GMenu.menu_item ~label:(Gui_messages.iM_me_quit)
    ~packing:(_FileMenu#add) ()
  in
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_menubar in
  let _ = _FileMenu#set_accel_group accel_menubar in
  let main_notebook =
    GPack.notebook ~tab_pos:`LEFT ~show_tabs:true ~homogeneous_tabs:true
    ~show_border:true ~scrollable:true
    (* ~popup:true *)
    ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let accounts_hbox =
    GPack.hbox ~homogeneous:false
      ~packing:(
      fun w ->
        main_notebook#append_page
          ~tab_label:((GMisc.label ~text:"Accounts" ())#coerce) w)
    ()
  in
  let contacts_hbox =
    GPack.hbox ~homogeneous:false
      ~packing:(
      fun w ->
        main_notebook#append_page
          ~tab_label:((GMisc.label ~text:"Friends" ())#coerce) w)
    ()
  in
  let hbox_status =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let label_connect_status =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0)
    ~yalign:(-1.0) ~packing:(hbox_status#pack ~expand:true ~fill:true) ()
  in
  object
    val window = window
    val box = box
    val menubar = menubar
    val accel_menubar = accel_menubar
    val itemOptions = itemOptions
    val itemQuit = itemQuit
    val main_notebook = main_notebook
    val accounts_hbox = accounts_hbox
    val contacts_hbox = contacts_hbox
    val hbox_status = hbox_status
    val label_connect_status = label_connect_status
    method window = window
    method box = box
    method menubar = menubar
    method accel_menubar = accel_menubar
    method itemOptions = itemOptions
    method itemQuit = itemQuit
    method main_notebook = main_notebook
    method accounts_hbox = accounts_hbox
    method contacts_hbox = contacts_hbox
    method hbox_status = hbox_status
    method label_connect_status = label_connect_status
end
class room_tab () =
  let room_vbox = GPack.vbox ~homogeneous:false () in
  let wpane =
    GPack.paned `HORIZONTAL ~width:600
      ~packing:(room_vbox#pack ~expand:true ~fill:true) ()
  in
  let wscroll =
    GBin.scrolled_window ~width:450 ~height:0 ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC ~placement:`TOP_LEFT ~packing:(wpane#add1) ()
  in
  let text =
    GEdit.text ~editable:false (* ~word_wrap:true ~line_wrap:true *)
    ~packing:(wscroll#add) ()
  in
  let hbox_91 =
    GPack.hbox ~homogeneous:false
      ~packing:(room_vbox#pack ~expand:false ~fill:true) ()
  in
  let nick_label =
    GMisc.label ~text:"My nick:" ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0)
    ~yalign:(-1.0) ~packing:(hbox_91#pack ~expand:false ~fill:true) ()
  in
  let entry =
    GEdit.entry ~visibility:true ~editable:true
    ~packing:(hbox_91#pack ~expand:true ~fill:true) ()
  in
  object
    val room_vbox = room_vbox
    val wpane = wpane
    val wscroll = wscroll
    val text = text
    val hbox_91 = hbox_91
    val nick_label = nick_label
    val entry = entry
    method room_vbox = room_vbox
    method wpane = wpane
    method wscroll = wscroll
    method text = text
    method hbox_91 = hbox_91
    method nick_label = nick_label
    method entry = entry
    method coerce = room_vbox#coerce
end
