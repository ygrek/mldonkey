class box tb_style () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let evbox = GBin.event_box ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let hbox = GPack.hbox ~homogeneous:false
               ~packing:(evbox#add) ()
  in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:tb_style 
(* GTK2 ~space_size:2 *)
(* GTK2 ~space_style:`LINE *) 
      ~tooltips:true
    (* GTK2 ~button_relief:`NONE *)
      ~packing:(hbox#pack ~expand:false ~fill:true ~padding:2) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:tb_style 
(* GTK2 ~space_size:2   ~space_style:`LINE *)
      ~tooltips:true
(* GTK2 ~button_relief:`NONE *)
      ~packing:(hbox#pack ~expand:false ~fill:true ~padding:2) ()
  in
  object
    val vbox = vbox
    val hbox = hbox
    val evbox = evbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    method vbox = vbox
    method hbox = hbox
    method evbox = evbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method coerce = vbox#coerce
  end

class files () =
  let wpane = GPack.paned `HORIZONTAL () in
  let vbox_dir = GPack.vbox ~homogeneous:false ~packing:(wpane#add1) () in
  let evbox = GBin.event_box ~packing:(vbox_dir#pack ~expand:false ~fill:false) () in
  let hbox = GPack.hbox ~homogeneous:false
               ~packing:(evbox#add) () in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`ICONS 
    (* GTK2 ~space_style:`LINE *)
      ~tooltips:true
    (* GTK2 ~button_relief:`NONE *)
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`ICONS 
(* GTK2 ~space_style:`LINE *)
      ~tooltips:true 
(* GTK2 ~button_relief:`NONE *)
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let wscroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(vbox_dir#pack ~expand:true ~fill:true) ()
  in
  let wtree =
    GBroken.tree
    (* GTK2  ~selection_mode:`SINGLE ~view_mode:`LINE  ~view_lines:true *)
      ~packing:(wscroll#add_with_viewport) ()
  in
  object
    val wpane = wpane
    val vbox_dir = vbox_dir
    val hbox = hbox
    val evbox = evbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    val wscroll = wscroll
    val wtree = wtree
    method wpane = wpane
    method vbox_dir = vbox_dir
    method hbox = hbox
    method evbox = evbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method wscroll = wscroll
    method wtree = wtree
    method coerce = wpane#coerce
  end
