



class box () =
  let _anonymous_container_1 = GPack.vbox ~homogeneous:false () in
  let _anonymous_container_2 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_1#pack ~expand:true ~fill:true) ()
  in
  let text =
    GText.view ~editable:false (* GTK2 ~word_wrap:true ~line_wrap:true *)
      ~packing:(_anonymous_container_2#add) ()
  in
  let _anonymous_container_3 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:180 ~text:(Gui_messages.cT_lb_command)
      ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0)
      ~packing:(_anonymous_container_3#pack ~expand:false ~fill:true) ()
  in
  let we_command =
    GEdit.combo ~popdown_strings:[] (* GTK2 ~use_arrows:`DEFAULT *) ~case_sensitive:true
      ~value_in_list:false (* GTK2 ~ok_if_empty:true *)
      ~packing:(_anonymous_container_3#pack ~expand:true ~fill:true) ()
  in
  let wb_clear_console =
    GButton.button
      ~packing:(_anonymous_container_3#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:180 ~text:(Gui_messages.cT_lb_clear_console)
      ~justify:`RIGHT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0)
      ~packing:(wb_clear_console#add) ()
  in
  object
    val text = text
    val we_command = we_command
    val wb_clear_console = wb_clear_console
    method text = text
    method we_command = we_command
    method wb_clear_console = wb_clear_console
    method coerce = _anonymous_container_1#coerce
  end
