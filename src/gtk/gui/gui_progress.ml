class progress () =
  let win =
    GWindow.window ~width:400 ~height:20 ~allow_shrink:true ~allow_grow:true
      ~auto_shrink:true ~modal:false ()
  in
  let hbox = GPack.hbox ~homogeneous:false ~packing:(win#add) () in
  let wpb =
    GRange.progress_bar ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let wb_cancel =
    GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _65 =
    GMisc.label ~text:(Chat_messages.cancel) ~justify:`LEFT ~line_wrap:true
      ~packing:(wb_cancel#add) ()
  in
  object
    val win = win
    val hbox = hbox
    val wpb = wpb
    val wb_cancel = wb_cancel
    method win = win
    method hbox = hbox
    method wpb = wpb
    method wb_cancel = wb_cancel
  end
~
~
~
~
~

class file_progress title file size f_stop =
  let s_file =
    let len = String.length file in
    if len > filename_max_length then
      "..."^
      (String.sub file (len - filename_max_length) filename_max_length)
    else
      file
  in
  let format_string = s_file^" : %p\\% of "^(size_of_int size) in
  object(self)
    inherit Chat_gui_base.progress ()

    method progress done_size =
      wpb#set_value (float_of_int done_size);
      while Glib.Main.pending () do
        ignore (Glib.Main.iteration false)
      done

    method message s =
      wpb#set_format_string (s_file^" : "^s)

    initializer
      ignore (wb_cancel#connect#clicked f_stop);
      win#set_title title;
      wpb#set_show_text true;
      wpb#set_format_string format_string;
      wpb#configure ~current: 0.0 ~min: 0.0 ~max: (float_of_int size);
      win#show ()
  end



mate dans cameleon/cam_modules_view.ml
[b8]Zoggy says:
il faut faire :
[b8]Zoggy says:
  let tooltips = GData.tooltips () in
tooltips#set_tip item#coerce ~text: (Odoc_info.string_of_info i)
[b8]Zoggy says:
item#coerce est ici pour un widget tree mai tu peux faire un truc similaire pour une row de glist il me semble (je l'avais fait mais je l'ai vire)
[b8]Zoggy says:
tu peux faier un seul let tooltips = GData.tooltips () in pour tout tes tooltips (enfin un par glist)
