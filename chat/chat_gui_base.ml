class gui () =
  let box = GPack.vbox ~homogeneous:false () in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let itemfile = GMenu.menu_item ~label:"File" ~packing:menubar#add () in
  let menuFile = GMenu.menu ~packing:itemfile#set_submenu () in
  let itemOptions =
    GMenu.menu_item ~label:(Chat_messages.m_options) ~packing:menuFile#add ()
  in
  let itemSetNotTemp =
    GMenu.menu_item ~label:(Chat_messages.m_set_not_temp)
      ~packing:menuFile#add ()
  in
  let itemOpenDialog =
    GMenu.menu_item ~label:(Chat_messages.m_open_dialog_for_selected_people)
      ~packing:menuFile#add ()
  in
  let itemQuit =
    GMenu.menu_item ~label:(Chat_messages.m_quit) ~packing:menuFile#add ()
  in
  let itemHelp = GMenu.menu_item ~label:"?" ~packing:menubar#add () in
  let menuHelp = GMenu.menu ~packing:itemHelp#set_submenu () in
  let itemAbout =
    GMenu.menu_item ~label:(Chat_messages.m_about) ~packing:menuHelp#add ()
  in
  let accelgroup = GtkData.AccelGroup.create () in
  let _ = menuFile#set_accel_group accelgroup in
  let _ =
    itemOptions#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._O
  in
  let _ =
    itemSetNotTemp#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._A
  in
  let _ =
    itemOpenDialog#add_accelerator ~group:accelgroup ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._D
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
      ~shadow_type:`NONE ~selection_mode:`SINGLE ~titles_show:true
      ~packing:_anonymous_container_1#add ()
  in
  object
    val box = box
    val menubar = menubar
    val accelgroup = accelgroup
    val itemfile = itemfile
    val menuFile = menuFile
    val itemOptions = itemOptions
    val itemSetNotTemp = itemSetNotTemp
    val itemOpenDialog = itemOpenDialog
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
    method itemSetNotTemp = itemSetNotTemp
    method itemOpenDialog = itemOpenDialog
    method itemQuit = itemQuit
    method itemHelp = itemHelp
    method menuHelp = menuHelp
    method itemAbout = itemAbout
    method wlist = wlist
    method coerce = box#coerce
  end;;
class dialog () =
  let box = GPack.vbox ~homogeneous:false () in
  let wscroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let wt_dialog =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:wscroll#add ()
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
  object
    val box = box
    val wscroll = wscroll
    val wt_dialog = wt_dialog
    val wtool = wtool
    val wt_input = wt_input
    method box = box
    method wscroll = wscroll
    method wt_dialog = wt_dialog
    method wtool = wtool
    method wt_input = wt_input
    method coerce = box#coerce
  end;;
