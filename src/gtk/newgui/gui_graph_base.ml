class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let scroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let vbox_1 =
    GPack.vbox ~homogeneous:false
      ~packing:(scroll#add_with_viewport) ()
  in
  let downloads_frame =
    GBin.frame ~border_width:5 ~label:(Gettext.gettext Gui_messages.downloads) ~label_xalign:(-1.0)
    ~label_yalign:(-1.0) ~shadow_type:`ETCHED_OUT
    ~packing:(vbox_1#pack ~expand:true ~fill:true ~padding:3) ()
  in
  let uploads_frame =
    GBin.frame ~border_width:5 ~label:(Gettext.gettext Gui_messages.uploads) ~label_xalign:(-1.0)
    ~label_yalign:(-1.0) ~shadow_type:`ETCHED_OUT
    ~packing:(vbox_1#pack ~expand:true ~fill:true ~padding:3) ()
  in
  object
    val vbox = vbox
    val downloads_frame = downloads_frame
    val uploads_frame = uploads_frame
    method vbox = vbox
    method downloads_frame = downloads_frame
    method uploads_frame = uploads_frame
    method coerce = vbox#coerce
  end
