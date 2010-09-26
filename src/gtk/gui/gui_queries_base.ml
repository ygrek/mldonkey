class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_style:`EMPTY
      ~tooltips:true ~button_relief:`NORMAL
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_style:`EMPTY
      ~tooltips:true ~button_relief:`NORMAL
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let box_fields =
    GPack.vbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_1 =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:"Network : "
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true) ()
  in
  let (nets, nets_wcombo) = Gui_global.networks_combo true in
  let _ =
    _anonymous_container_1#pack nets_wcombo#coerce ~padding: 1 
  in
  let _anonymous_container_2 =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Gettext.gettext Gui_messages.max_hits ^ " : ")
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true) ()
  in
  let we_max_hits =
    GEdit.entry ~text:"200" ~visibility:true ~editable:true
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true) ()
  in
  let hbox_show =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let wchk_show =
    GButton.check_button ~active:false ~draw_indicator:true
      ~packing:(hbox_show#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Gettext.gettext Gui_messages.show_hidden_fields)
      ~justify:`LEFT ~line_wrap:true ~xpad:2
      ~packing:(hbox_show#pack ~expand:false ~fill:true) ()
  in
  object
    val vbox = vbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    val box_fields = box_fields
    val we_max_hits = we_max_hits
    val hbox_show = hbox_show
    val wchk_show = wchk_show
    val nets = nets
    val nets_wcombo = nets_wcombo
    method vbox = vbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method box_fields = box_fields
    method we_max_hits = we_max_hits
    method hbox_show = hbox_show
    method wchk_show = wchk_show
    method coerce = vbox#coerce
  end
class paned () =
  let wnote_queries =
    GPack.notebook ~tab_pos:`LEFT ~show_tabs:true ~homogeneous_tabs:true
      ~show_border:true ~scrollable:true ~popup:true ()
  in
  object
    val wnote_queries = wnote_queries
    method wnote_queries = wnote_queries
    method coerce = wnote_queries#coerce
  end

