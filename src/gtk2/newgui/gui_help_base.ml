class box () =
  let wscroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ()
  in
  let wtext =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:(wscroll#add) ()
  in
  object
    val wscroll = wscroll
    val wtext = wtext
    method wscroll = wscroll
    method wtext = wtext
    method coerce = wscroll#coerce
  end
