class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let evbox1 = GBin.event_box ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let _anonymous_container_1 =
    GPack.hbox ~border_width:2 ~homogeneous:false ~spacing:5 ~packing:(evbox1#add) ()
  in
  let label_users =
    GMisc.label 
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding:3) ()
  in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH 
(* GTK2 ~space_size:2 *)
(* GTK2 ~space_style:`LINE *) 
    ~tooltips:true 
(* GTK2 ~button_relief:`NONE *)
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH 
(* GTK2 ~space_size:2 *)
(* GTK2 ~space_style:`LINE *)
      ~tooltips:true
(* GTK2 ~button_relief:`NONE *)
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding:2) ()
  in
  object
    val vbox = vbox
    val evbox1 = evbox1
    val label_users = label_users
    val wtool1 = wtool1
    val wtool2 = wtool2
    method vbox = vbox
    method evbox1 = evbox1
    method label_users = label_users
    method wtool1 = wtool1
    method wtool2 = wtool2
    method coerce = vbox#coerce
  end
