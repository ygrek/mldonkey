class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let evbox = GBin.event_box ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let _anonymous_container_2 =
    GPack.hbox ~border_width:2 ~homogeneous:false ~packing:(evbox#add) ()
  in
  let label =
    GMisc.label ~text:(Gui_messages.uT_lb_uploaders)
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true ~padding:3) ()
  in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true ~padding:2) ()
  in
  object
    val vbox = vbox
    val evbox = evbox
    val label = label
    val wtool1 = wtool1
    val wtool2 = wtool2
    method vbox = vbox
    method evbox = evbox
    method label = label
    method wtool1 = wtool1
    method wtool2 = wtool2
    method coerce = vbox#coerce
  end
class paned () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let wpane =
    GPack.paned `HORIZONTAL ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let wpane2 =
    GPack.paned `VERTICAL ~packing:(wpane#add2) ()
  in
  let vbox2 = GPack.vbox ~homogeneous:false ~packing:(wpane2#add2) () in
  let evbox1 = GBin.event_box ~packing:(vbox2#pack ~expand:false ~fill:false) () in
  let hbox =
    GPack.hbox ~border_width:2 ~homogeneous:false ~spacing:5 ~packing:(evbox1#add) ()
  in
  let label =
    GMisc.label
      ~justify:`LEFT ~line_wrap:true ~text:(Gui_messages.fT_lb_messages)
      ~packing:(hbox#pack ~expand:false ~fill:true ~padding:3) ()
  in
  object
    val vbox = vbox
    val wpane = wpane
    val wpane2 = wpane2
    val vbox2 = vbox2
    val evbox1 = evbox1
    val label = label
    method vbox = vbox
    method wpane = wpane
    method wpane2 = wpane2
    method vbox2 = vbox2
    method evbox1 = evbox1
    method label = label
    method coerce = vbox#coerce
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
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`ICONS ~space_style:`EMPTY
      ~tooltips:true ~button_relief:`NORMAL
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let wtool2 =
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
    val wtool1 = wtool1
    val wtool2 = wtool2
    val wt_input = wt_input
    method box = box
    method wscroll = wscroll
    method wt_dialog = wt_dialog
    method wtool1 = wtool1
    method wtool2 = wtool2
    method wt_input = wt_input
    method coerce = box#coerce
  end
