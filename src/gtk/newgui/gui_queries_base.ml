class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let hbox =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) () in
  let wt_frame =
    GBin.frame ~border_width:1 ~shadow_type:`ETCHED_OUT
      ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let hbox_wt =
    GPack.hbox ~homogeneous:false
      ~packing:(wt_frame#add) () in
  let wtool1 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_style:`LINE
      ~tooltips:true ~button_relief:`NONE
      ~packing:(hbox_wt#pack ~expand:false ~fill:true) ()
  in
  let wtool2 =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_style:`LINE
      ~tooltips:true ~button_relief:`NONE
      ~packing:(hbox_wt#pack ~expand:false ~fill:true) ()
  in
  let common_frame =
    GBin.frame ~border_width:1 ~shadow_type:`ETCHED_OUT
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_1 =
    GPack.hbox ~homogeneous:false
      ~packing:(common_frame#add) ()
  in
  let _ =
    GMisc.label ~text:(Gettext.gettext Gui_messages.network2)
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding: 2) ()
  in
  let (nets, nets_wcombo) = Gui_global.networks_combo true in
  let _ =
    _anonymous_container_1#pack nets_wcombo#coerce ~padding: 5
  in
  let _ =
    GMisc.label ~text:(Gettext.gettext Gui_messages.max_hits ^ " : ")
      ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true) ()
  in
  let we_max_hits =
    GEdit.entry ~text:"200" ~visibility:true ~editable:true
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding: 5) ()
  in
  let hbox_show =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_1#pack ~expand:false ~fill:true ~padding: 5) ()
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
  let bf_frame =
    GBin.frame ~border_width:2 ~shadow_type:`ETCHED_OUT
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let box_fields =
    GPack.vbox ~homogeneous:false
      ~packing:(bf_frame#add) ()
  in
  object
    val vbox = vbox
    val hbox = hbox
    val wtool1 = wtool1
    val wtool2 = wtool2
    val box_fields = box_fields
    val we_max_hits = we_max_hits
    val hbox_show = hbox_show
    val wchk_show = wchk_show
    (* nets becomes mutable to be able to update the combo box in the search tab *)
    val mutable nets = nets
    val nets_wcombo = nets_wcombo
    method vbox = vbox
    method hbox = hbox
    method wtool1 = wtool1
    method wtool2 = wtool2
    method box_fields = box_fields
    method we_max_hits = we_max_hits
    method hbox_show = hbox_show
    method wchk_show = wchk_show
    method nets_wcombo = nets_wcombo
    method coerce = vbox#coerce
  end

class paned () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let wnote_queries =
    GPack.notebook ~tab_pos:`TOP ~show_tabs:true ~homogeneous_tabs:false
      ~show_border:true ~scrollable:true ~popup:true
      ~packing:(vbox#pack ~padding:2 ~expand:false ~fill:false) ()
  in
  let wnote_results =
    GPack.notebook ~tab_pos:`TOP ~show_tabs:true ~homogeneous_tabs:true
      ~show_border:true ~scrollable:true ~popup:true
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  object
    val vbox = vbox
    val wnote_queries = wnote_queries
    val wnote_results = wnote_results
    method vbox = vbox
    method wnote_queries = wnote_queries
    method wnote_results = wnote_results
    method coerce = vbox#coerce
  end

