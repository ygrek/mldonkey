class box tb_style () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let evbox = GBin.event_box ~packing:(vbox#pack ~expand:false ~fill:false) () in
  let _anonymous_container_1 =
    GPack.hbox ~border_width:2 ~homogeneous:false ~spacing:5 ~packing:(evbox#add) ()
  in
  let label_rooms_messages =
    GMisc.label
      ~justify:`LEFT ~line_wrap:true ~text:(Gettext.gettext Gui_messages.rooms_messages)
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding:3) ()
  in
  let wscroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let wt_10 =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:(wscroll#add) ()
  in
  let we_11 =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(vbox#pack ~expand:false ~fill:false) ()
  in
  object
    val vbox = vbox
    val evbox = evbox
    val label_rooms_messages = label_rooms_messages
    val wscroll = wscroll
    val wt_10 = wt_10
    val we_11 = we_11
    method vbox = vbox
    method evbox = evbox
    method label_rooms_messages = label_rooms_messages
    method wscroll = wscroll
    method wt_10 = wt_10
    method we_11 = we_11
    method coerce = vbox#coerce
  end
class box2 () =
  let vbox =
    GPack.vbox ~width:0 ~height:0 ~border_width:0 ~homogeneous:false
      ~spacing:0 ()
  in
  let hpaned =
    GPack.paned `HORIZONTAL ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let rooms_pane = GPack.paned `VERTICAL ~packing:(hpaned#add2) () in
  let room_pane =
    GPack.paned `HORIZONTAL
      ~packing:(rooms_pane#add1) ()
  in
  object
    val vbox = vbox
    val hpaned = hpaned
    val rooms_pane = rooms_pane
    val room_pane = room_pane
    method vbox = vbox
    method hpaned = hpaned
    method rooms_pane = rooms_pane
    method room_pane = room_pane
    method coerce = vbox#coerce
  end
