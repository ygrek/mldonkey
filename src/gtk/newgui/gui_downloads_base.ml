class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
      ~packing:(vbox#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
      ~packing:(vbox#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let ask_clients = GButton.button ~packing:vbox#add ()
  in
  object
    val vbox = vbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    val ask_clients = ask_clients
    method vbox = vbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method ask_clients = ask_clients
    method coerce = vbox#coerce
  end
class paned () =
  let box = GPack.vbox ~homogeneous:false () in
  let evbox1 = GBin.event_box ~packing:(box#pack ~expand:false ~fill:false) () in
  let _anonymous_container_1 =
    GPack.vbox ~homogeneous:false ~packing:(evbox1#add) ()
  in
  let _anonymous_container_2 =
    GPack.hbox ~homogeneous:false ~spacing:5 ~border_width:5
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:false) ()
  in
  let label_entry_ed2k_url =
    GMisc.label ~text:(Gettext.gettext Gui_messages.dT_lb_ed2k) ~justify:`LEFT
      ~line_wrap:true ~packing:(_anonymous_container_2#pack ~expand:false ~fill:false ~padding:5 ) ()
  in
  let entry_ed2k_url =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_2#pack ~expand:true ~fill:true ~padding:5 ) ()
  in
  
  object
    val box = box

    val evbox1 = evbox1
    val label_entry_ed2k_url = label_entry_ed2k_url
    val entry_ed2k_url = entry_ed2k_url
    method box = box

    method evbox1 = evbox1
    method label_entry_ed2k_url = label_entry_ed2k_url
    method entry_ed2k_url = entry_ed2k_url
    method coerce = box#coerce
  end
