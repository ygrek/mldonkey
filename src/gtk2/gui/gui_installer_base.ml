class window () =
  let window =
    GWindow.window ~width:(Options.( !! ) Gui_options.gui_width)
      ~height:(Options.( !! ) Gui_options.gui_height)
      ~title:(Gui_messages.software) ~allow_shrink:true ~allow_grow:true
      ~auto_shrink:true ~modal:false ()
  in
  let box =
    GPack.vbox ~width:600 ~height:440 ~homogeneous:false ~packing:(window#add)
      ()
  in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let _Menu =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mFile)
      ~packing:(menubar#add) ()
  in
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_menubar in
  let hbox_77 =
    GPack.hbox ~homogeneous:false ~packing:(box#pack ~expand:false ~fill:true)
      ()
  in
  let wl_78 =
    GMisc.label ~text:"Welcome to MLdonkey installer" ~justify:`CENTER
      ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0)
      ~packing:(hbox_77#pack ~expand:false ~fill:true) ()
  in
  let notebook =
    GPack.notebook ~tab_pos:`LEFT ~show_tabs:true ~homogeneous_tabs:true
      ~show_border:true ~scrollable:true ~popup:true
      ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let hbox_status =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  object
    val window = window
    val box = box
    val menubar = menubar
    val accel_menubar = accel_menubar
    val hbox_77 = hbox_77
    val wl_78 = wl_78
    val notebook = notebook
    val hbox_status = hbox_status
    method window = window
    method box = box
    method menubar = menubar
    method accel_menubar = accel_menubar
    method hbox_77 = hbox_77
    method wl_78 = wl_78
    method notebook = notebook
    method hbox_status = hbox_status
  end
class tab () =
  let _anonymous_container_1 = GPack.vbox ~homogeneous:false () in
  let main_hbox =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_1#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_2 =
    GPack.hbox ~homogeneous:false
      ~packing:(
        _anonymous_container_1#pack ~expand:false ~fill:true ~padding:20)
      ()
  in
  let _ =
    GPack.vbox ~homogeneous:false
      ~packing:(_anonymous_container_2#pack ~expand:true ~fill:true) ()
  in
  let button_next =
    GButton.button
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~height:50 ~text:"Next" ~justify:`CENTER
      ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0) ~xpad:3 ~ypad:3
      ~packing:(button_next#add) ()
  in
  object
    val main_hbox = main_hbox
    val button_next = button_next
    method main_hbox = main_hbox
    method button_next = button_next
    method coerce = _anonymous_container_1#coerce
  end
