class upstats_box () =
  let vbox = GPack.vbox ~spacing:3 ~homogeneous:false () in
  let evbox1 = GBin.event_box ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let _anonymous_container_1 =
    GPack.hbox ~border_width:2 ~spacing:5 ~homogeneous:false ~packing:(evbox1#add) ()
  in
  let label_shared_files =
    GMisc.label ~text:(Gui_messages.uT_lb_shared_files)
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
  let vpaned =
    GPack.paned `VERTICAL ~width:0 ~height:0
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let vbox_uploaders = GPack.vbox ~spacing:3 ~homogeneous:false ~packing:(vpaned#add2) () in
  let hbox_uploaders = GPack.hbox ~homogeneous:false ~packing:(vbox_uploaders#pack ~expand:true ~fill:true) ()
  in
  object
    val vbox = vbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    val evbox1 = evbox1
    val label_shared_files = label_shared_files
    val vpaned = vpaned
    val vbox_uploaders = vbox_uploaders
    val hbox_uploaders = hbox_uploaders
    method vbox = vbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method evbox1 = evbox1
    method label_shared_files = label_shared_files
    method vpaned = vpaned
    method vbox_uploaders = vbox_uploaders
    method hbox_uploaders = hbox_uploaders
    method coerce = vbox#coerce
  end
