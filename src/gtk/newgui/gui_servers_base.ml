class box () =
  let vbox1 = GPack.vbox ~spacing:3 ~homogeneous:false () in
  let evbox1 = GBin.event_box ~packing:(vbox1#pack ~expand:false ~fill:false) () in
  let _anonymous_container_1 =
    GPack.hbox ~border_width:2 ~homogeneous:false ~spacing:5 ~packing:(evbox1#add) ()
  in
  let label_servers =
    GMisc.label ~text:(Gettext.gettext Gui_messages.servers)
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding:3) ()
  in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:false ~padding:2) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:false ~padding:2) ()
  in
  object
    val vbox1 = vbox1
    val evbox1 = evbox1
    val label_servers = label_servers
    val wtool1 = wtool1
    val wtool2 = wtool2
    method vbox1 = vbox1
    method evbox1 = evbox1
    method label_servers = label_servers
    method wtool1 = wtool1
    method wtool2 = wtool2
    method coerce = vbox1#coerce
  end

class pane () =
  let vpaned_servers =
    GPack.paned `VERTICAL ~width:0 ~height:0 ()
  in
  object
    val vpaned_servers = vpaned_servers
    method vpaned_servers = vpaned_servers
    method coerce = vpaned_servers#coerce
  end

class box_old () =
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
  let hbox_add =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Gettext.gettext Gui_messages.ip ^ ": ") ~justify:`LEFT
      ~line_wrap:true ~xpad:2
      ~packing:(hbox_add#pack ~expand:false ~fill:true) ()
  in
  let we_ip =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(hbox_add#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let _ =
    GMisc.label ~text:(Gettext.gettext Gui_messages.port ^ ": ")
      ~justify:`LEFT ~line_wrap:true ~xpad:2
      ~packing:(hbox_add#pack ~expand:false ~fill:true) ()
  in
  let we_port =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(hbox_add#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let wb_add =
    GButton.button ~packing:(hbox_add#pack ~expand:false ~fill:true) ()
  in
  let _31 =
    GMisc.label ~text:(Gettext.gettext Gui_messages.add_server) ~justify:`LEFT
      ~line_wrap:true ~xpad:2 ~packing:(wb_add#add) ()
  in
  object
    val vbox = vbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    val hbox_add = hbox_add
    val we_ip = we_ip
    val we_port = we_port
    val wb_add = wb_add
    method vbox = vbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method hbox_add = hbox_add
    method we_ip = we_ip
    method we_port = we_port
    method wb_add = wb_add
    method coerce = vbox#coerce
  end
