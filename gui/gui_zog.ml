class box_servers () =
  let box = GPack.hbox ~homogeneous:false () in
  let _anonymous_container_1 =
    GPack.vbox ~homogeneous:false ~packing:(box#pack ~expand:true ~fill:true)
      ()
  in
  let _anonymous_container_2 =
    GPack.hbox ~homogeneous:false
      ~packing:(
        _anonymous_container_1#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.ip) ~justify:`LEFT ~line_wrap:true
      ~packing:(
        _anonymous_container_2#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let entry_servers_new_ip =
    GEdit.entry ~width:160 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.port) ~justify:`LEFT ~line_wrap:true
      ~packing:(
        _anonymous_container_2#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let entry_servers_new_port =
    GEdit.entry ~width:80 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_2#pack ~expand:false ~fill:true) ()
  in
  let button_servers_add =
    GButton.button
      ~packing:(
        _anonymous_container_2#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.add_server) ~justify:`LEFT ~line_wrap:true
      ~packing:button_servers_add#add ()
  in
  let _ =
    GMisc.separator `VERTICAL
      ~packing:(
        _anonymous_container_2#pack ~expand:false ~fill:true ~padding:5)
      ()
  in
  let button_servers_connect_more =
    GButton.button
      ~packing:(
        _anonymous_container_2#pack ~expand:true ~fill:true ~padding:3)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.connect_more_servers) ~justify:`LEFT
      ~line_wrap:true ~packing:button_servers_connect_more#add ()
  in
  let button_remove_old_servers =
    GButton.button
      ~packing:(_anonymous_container_2#pack ~expand:true ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.remove_old_servers) ~justify:`LEFT ~line_wrap:true
      ~packing:button_remove_old_servers#add ()
  in
  let _anonymous_container_3 =
    GPack.paned `HORIZONTAL
      ~packing:(_anonymous_container_1#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_4 =
    GPack.vbox ~width:600 ~homogeneous:false
      ~packing:_anonymous_container_3#add1 ()
  in
  let _anonymous_container_5 =
    GBin.scrolled_window ~hpolicy:`ALWAYS ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_4#pack ~expand:true ~fill:true) ()
  in
  let clist_servers =
    GList.clist ~columns:5
      ~titles:(
        [Mes.address; Mes.status; Mes.server_nusers; Mes.server_nfiles;
         Mes.server_desc])
      ~shadow_type:`NONE ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_5#add ()
  in
  let _anonymous_container_6 =
    GPack.hbox ~homogeneous:true
      ~packing:(_anonymous_container_4#pack ~expand:false ~fill:true) ()
  in
  let button_servers_remove =
    GButton.button
      ~packing:(_anonymous_container_6#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.remove) ~justify:`LEFT ~line_wrap:true
      ~packing:button_servers_remove#add ()
  in
  let button_servers_connect =
    GButton.button
      ~packing:(_anonymous_container_6#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.connect) ~justify:`LEFT ~line_wrap:true
      ~packing:button_servers_connect#add ()
  in
  let button_servers_disconnect =
    GButton.button
      ~packing:(_anonymous_container_6#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.disconnect) ~justify:`LEFT ~line_wrap:true
      ~packing:button_servers_disconnect#add ()
  in
  let button_servers_view_users =
    GButton.button
      ~packing:(_anonymous_container_6#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.view_users) ~justify:`LEFT ~line_wrap:true
      ~packing:button_servers_view_users#add ()
  in
  let _anonymous_container_7 =
    GPack.vbox ~width:200 ~homogeneous:false
      ~packing:_anonymous_container_3#add2 ()
  in
  let _anonymous_container_8 =
    GBin.scrolled_window ~width:200 ~hpolicy:`AUTOMATIC ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_7#pack ~expand:true ~fill:true) ()
  in
  let clist_users =
    GList.clist ~width:180 ~columns:2 ~titles:(["Kind"; Mes.users])
      ~shadow_type:`NONE ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_8#add ()
  in
  let _anonymous_container_9 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_7#pack ~expand:false ~fill:true) ()
  in
  let button_add_to_friends =
    GButton.button
      ~packing:(_anonymous_container_9#pack ~expand:true ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.add_to_friends) ~justify:`LEFT ~line_wrap:true
      ~packing:button_add_to_friends#add ()
  in
  object
    val box = box
    val entry_servers_new_ip = entry_servers_new_ip
    val entry_servers_new_port = entry_servers_new_port
    val button_servers_add = button_servers_add
    val button_servers_connect_more = button_servers_connect_more
    val button_remove_old_servers = button_remove_old_servers
    val clist_servers = clist_servers
    val button_servers_remove = button_servers_remove
    val button_servers_connect = button_servers_connect
    val button_servers_disconnect = button_servers_disconnect
    val button_servers_view_users = button_servers_view_users
    val clist_users = clist_users
    val button_add_to_friends = button_add_to_friends
    method box = box
    method entry_servers_new_ip = entry_servers_new_ip
    method entry_servers_new_port = entry_servers_new_port
    method button_servers_add = button_servers_add
    method button_servers_connect_more = button_servers_connect_more
    method button_remove_old_servers = button_remove_old_servers
    method clist_servers = clist_servers
    method button_servers_remove = button_servers_remove
    method button_servers_connect = button_servers_connect
    method button_servers_disconnect = button_servers_disconnect
    method button_servers_view_users = button_servers_view_users
    method clist_users = clist_users
    method button_add_to_friends = button_add_to_friends
    method coerce = box#coerce
  end;;
class box_search () =
  let box = GPack.vbox ~homogeneous:false () in
  let hbox_501 =
    GPack.hbox ~homogeneous:false ~packing:(box#pack ~expand:false ~fill:true)
      ()
  in
  let label_query =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true
      ~packing:(hbox_501#pack ~expand:true ~fill:true) ()
  in
  let label_waiting =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true
      ~packing:(hbox_501#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_10 =
    GBin.scrolled_window ~hpolicy:`ALWAYS ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let clist_search_results =
    GList.clist ~columns:5
      ~titles:([Mes.size; Mes.filename; "Format"; Mes.properties; Mes.md4])
      ~shadow_type:`ETCHED_OUT ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_10#add ()
  in
  let _anonymous_container_11 =
    GPack.hbox ~homogeneous:true ~packing:(box#pack ~expand:false ~fill:true)
      ()
  in
  let button_search_download =
    GButton.button
      ~packing:(_anonymous_container_11#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.download_selected_files) ~justify:`LEFT
      ~line_wrap:true ~packing:button_search_download#add ()
  in
  let button_stop =
    GButton.button
      ~packing:(_anonymous_container_11#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.stop_search) ~justify:`LEFT ~line_wrap:true
      ~packing:button_stop#add ()
  in
  let button_close =
    GButton.button
      ~packing:(_anonymous_container_11#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.close_search) ~justify:`LEFT ~line_wrap:true
      ~packing:button_close#add ()
  in
  object
    val box = box
    val hbox_501 = hbox_501
    val label_query = label_query
    val label_waiting = label_waiting
    val clist_search_results = clist_search_results
    val button_search_download = button_search_download
    val button_stop = button_stop
    val button_close = button_close
    method box = box
    method hbox_501 = hbox_501
    method label_query = label_query
    method label_waiting = label_waiting
    method clist_search_results = clist_search_results
    method button_search_download = button_search_download
    method button_stop = button_stop
    method button_close = button_close
    method coerce = box#coerce
  end;;
class box_downloads () =
  let box = GPack.hbox ~homogeneous:false () in
  let _anonymous_container_12 =
    GPack.vbox ~homogeneous:false ~packing:(box#pack ~expand:true ~fill:true)
      ()
  in
  let _anonymous_container_13 =
    GPack.hbox ~homogeneous:false
      ~packing:(
        _anonymous_container_12#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.ed2k) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_13#pack ~expand:false ~fill:true) ()
  in
  let entry_ed2k_url =
    GEdit.entry ~width:500 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_13#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.recover_md4) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_13#pack ~expand:false ~fill:true) ()
  in
  let entry_md4 =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_13#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_14 =
    GPack.paned `HORIZONTAL
      ~packing:(_anonymous_container_12#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_15 =
    GPack.vbox ~width:600 ~homogeneous:false
      ~packing:_anonymous_container_14#add1 ()
  in
  let _anonymous_container_16 =
    GPack.paned `VERTICAL
      ~packing:(_anonymous_container_15#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_17 =
    GPack.vbox ~height:200 ~homogeneous:false
      ~packing:_anonymous_container_16#add1 ()
  in
  let label_downloaded =
    GMisc.label ~text:(Mes.files_downloaded 0) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_17#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_18 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_17#pack ~expand:true ~fill:true) ()
  in
  let clist_downloaded =
    GList.clist ~columns:5
      ~titles:(
        [Mes.filename; Mes.size; Mes.downloaded; Mes.availability; Mes.md4])
      ~shadow_type:`NONE ~selection_mode:`SINGLE ~titles_show:true
      ~packing:_anonymous_container_18#add ()
  in
  let _anonymous_container_19 =
    GPack.hbox ~homogeneous:true
      ~packing:(_anonymous_container_17#pack ~expand:false ~fill:true) ()
  in
  let button_downloaded_save =
    GButton.button
      ~packing:(_anonymous_container_19#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.downloaded_save) ~justify:`LEFT ~line_wrap:true
      ~packing:button_downloaded_save#add ()
  in
  let _anonymous_container_20 =
    GPack.vbox ~height:200 ~homogeneous:false
      ~packing:_anonymous_container_16#add2 ()
  in
  let label_downloading =
    GMisc.label ~text:(Mes.downloading_files 0) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_20#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_21 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_20#pack ~expand:true ~fill:true) ()
  in
  let clist_downloads =
    GList.clist ~columns:7
      ~titles:(
        [Mes.filename; Mes.size; Mes.downloaded; Mes.percent; Mes.rate;
         Mes.state; Mes.availability; Mes.md4])
      ~shadow_type:`NONE ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_21#add ()
  in
  let label_file_info =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_20#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_22 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_20#pack ~expand:false ~fill:true) ()
  in
  let draw_availability =
    GMisc.drawing_area ~height:20
      ~packing:(_anonymous_container_22#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_23 =
    GPack.hbox ~homogeneous:true
      ~packing:(_anonymous_container_20#pack ~expand:false ~fill:true) ()
  in
  let button_download_cancel =
    GButton.button
      ~packing:(_anonymous_container_23#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.cancel) ~justify:`LEFT ~line_wrap:true
      ~packing:button_download_cancel#add ()
  in
  let button_download_retry_connect =
    GButton.button
      ~packing:(_anonymous_container_23#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.retry_connect) ~justify:`LEFT ~line_wrap:true
      ~packing:button_download_retry_connect#add ()
  in
  let _anonymous_container_24 =
    GPack.vbox ~width:200 ~homogeneous:false
      ~packing:_anonymous_container_14#add2 ()
  in
  let _anonymous_container_25 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_24#pack ~expand:true ~fill:true) ()
  in
  let clist_locations =
    GList.clist ~columns:3 ~titles:(["Kind"; "Name"; "Status"])
      ~shadow_type:`NONE ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_25#add ()
  in
  let label_locations_status =
    GMisc.label ~text:(Mes.connected_to_locations 0 0) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_24#pack ~expand:false ~fill:true) ()
  in
  let button_download_add_friend =
    GButton.button
      ~packing:(_anonymous_container_24#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.add_to_friends) ~justify:`LEFT ~line_wrap:true
      ~packing:button_download_add_friend#add ()
  in
  object
    val box = box
    val entry_ed2k_url = entry_ed2k_url
    val entry_md4 = entry_md4
    val label_downloaded = label_downloaded
    val clist_downloaded = clist_downloaded
    val button_downloaded_save = button_downloaded_save
    val label_downloading = label_downloading
    val clist_downloads = clist_downloads
    val label_file_info = label_file_info
    val draw_availability = draw_availability
    val button_download_cancel = button_download_cancel
    val button_download_retry_connect = button_download_retry_connect
    val clist_locations = clist_locations
    val label_locations_status = label_locations_status
    val button_download_add_friend = button_download_add_friend
    method box = box
    method entry_ed2k_url = entry_ed2k_url
    method entry_md4 = entry_md4
    method label_downloaded = label_downloaded
    method clist_downloaded = clist_downloaded
    method button_downloaded_save = button_downloaded_save
    method label_downloading = label_downloading
    method clist_downloads = clist_downloads
    method label_file_info = label_file_info
    method draw_availability = draw_availability
    method button_download_cancel = button_download_cancel
    method button_download_retry_connect = button_download_retry_connect
    method clist_locations = clist_locations
    method label_locations_status = label_locations_status
    method button_download_add_friend = button_download_add_friend
    method coerce = box#coerce
  end;;
class box_friends () =
  let box = GPack.hbox ~homogeneous:false () in
  let _anonymous_container_26 =
    GPack.paned `HORIZONTAL ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_27 =
    GBin.frame ~width:180 ~height:200 ~border_width:1 ~label:(Mes.friends)
      ~shadow_type:`ETCHED_OUT ~packing:_anonymous_container_26#add1 ()
  in
  let _anonymous_container_28 =
    GPack.vbox ~width:200 ~height:100 ~homogeneous:false ~spacing:10
      ~packing:_anonymous_container_27#add ()
  in
  let _anonymous_container_29 =
    GBin.scrolled_window ~hpolicy:`ALWAYS ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_28#pack ~expand:true ~fill:true) ()
  in
  let clist_friends =
    GList.clist ~columns:3
      ~titles:([Mes.friend_status; Mes.friend_name; Mes.friend_kind])
      ~shadow_type:`NONE ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_29#add ()
  in
  let _anonymous_container_30 =
    GPack.hbox ~homogeneous:true
      ~packing:(_anonymous_container_28#pack ~expand:false ~fill:true) ()
  in
  let button_friends_remove =
    GButton.button
      ~packing:(_anonymous_container_30#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.remove) ~justify:`LEFT ~line_wrap:true
      ~packing:button_friends_remove#add ()
  in
  let _anonymous_container_31 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_28#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.find_friend) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_31#pack ~expand:false ~fill:true) ()
  in
  let entry_find_friend =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_31#pack ~expand:true ~fill:true) ()
  in
  let wpane_522 =
    GPack.paned `VERTICAL ~packing:_anonymous_container_26#add2 ()
  in
  let _anonymous_container_32 =
    GBin.frame ~width:600 ~height:200 ~border_width:1 ~label:(Mes.files)
      ~shadow_type:`ETCHED_OUT ~packing:wpane_522#add1 ()
  in
  let _anonymous_container_33 =
    GPack.vbox ~homogeneous:false ~packing:_anonymous_container_32#add ()
  in
  let _anonymous_container_34 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_33#pack ~expand:true ~fill:true) ()
  in
  let clist_friends_files =
    GList.clist ~columns:4
      ~titles:([Mes.filename; Mes.size; Mes.properties; Mes.md4])
      ~shadow_type:`ETCHED_OUT ~selection_mode:`MULTIPLE ~titles_show:true
      ~packing:_anonymous_container_34#add ()
  in
  let _anonymous_container_35 =
    GPack.hbox ~border_width:5 ~homogeneous:false
      ~packing:(_anonymous_container_33#pack ~expand:false ~fill:true) ()
  in
  let button_friends_download =
    GButton.button
      ~packing:(_anonymous_container_35#pack ~expand:true ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.download_selected_files) ~justify:`LEFT
      ~line_wrap:true ~packing:button_friends_download#add ()
  in
  let _anonymous_container_36 =
    GPack.hbox ~homogeneous:false
      ~packing:(
        _anonymous_container_35#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.ip) ~justify:`LEFT ~line_wrap:true
      ~packing:(
        _anonymous_container_36#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let entry_friends_new_ip =
    GEdit.entry ~width:110 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_36#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.port) ~justify:`LEFT ~line_wrap:true
      ~packing:(
        _anonymous_container_36#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let entry_friends_new_port =
    GEdit.entry ~width:60 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_36#pack ~expand:false ~fill:true) ()
  in
  let button_friends_add =
    GButton.button
      ~packing:(
        _anonymous_container_36#pack ~expand:false ~fill:true ~padding:3)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.add_friend) ~justify:`LEFT ~line_wrap:true
      ~packing:button_friends_add#add ()
  in
  let _anonymous_container_37 =
    GBin.frame ~label:(Mes.dialog) ~shadow_type:`NONE ~packing:wpane_522#add2
      ()
  in
  let _anonymous_container_38 =
    GPack.vbox ~homogeneous:false ~packing:_anonymous_container_37#add ()
  in
  let entry_dialog =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_38#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_39 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_38#pack ~expand:true ~fill:true) ()
  in
  let text_dialog =
    GEdit.text ~editable:true ~word_wrap:true ~line_wrap:true
      ~packing:_anonymous_container_39#add ()
  in
  object
    val box = box
    val clist_friends = clist_friends
    val button_friends_remove = button_friends_remove
    val entry_find_friend = entry_find_friend
    val wpane_522 = wpane_522
    val clist_friends_files = clist_friends_files
    val button_friends_download = button_friends_download
    val entry_friends_new_ip = entry_friends_new_ip
    val entry_friends_new_port = entry_friends_new_port
    val button_friends_add = button_friends_add
    val entry_dialog = entry_dialog
    val text_dialog = text_dialog
    method box = box
    method clist_friends = clist_friends
    method button_friends_remove = button_friends_remove
    method entry_find_friend = entry_find_friend
    method wpane_522 = wpane_522
    method clist_friends_files = clist_friends_files
    method button_friends_download = button_friends_download
    method entry_friends_new_ip = entry_friends_new_ip
    method entry_friends_new_port = entry_friends_new_port
    method button_friends_add = button_friends_add
    method entry_dialog = entry_dialog
    method text_dialog = text_dialog
    method coerce = box#coerce
  end;;
class box_searches () =
  let _anonymous_container_40 = GPack.paned `HORIZONTAL () in
  let _anonymous_container_41 =
    GBin.frame ~width:310 ~border_width:1 ~label:(Mes.query)
      ~shadow_type:`ETCHED_OUT ~packing:_anonymous_container_40#add1 ()
  in
  let _anonymous_container_42 =
    GPack.vbox ~homogeneous:false ~spacing:10
      ~packing:_anonymous_container_41#add ()
  in
  let _anonymous_container_43 =
    GPack.vbox ~homogeneous:false
      ~packing:(_anonymous_container_42#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_44 =
    GPack.hbox ~homogeneous:false ~spacing:3
      ~packing:(_anonymous_container_43#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.search) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_44#pack ~expand:false ~fill:true) ()
  in
  let entry_search_words =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_44#pack ~expand:false ~fill:true) ()
  in
  let hbox_473 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_43#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.max_hits) ~justify:`LEFT ~line_wrap:true
      ~packing:(hbox_473#pack ~expand:false ~fill:true) ()
  in
  let combo_max_hits =
    GEdit.combo ~popdown_strings:(["250"; "500"; "1000"; "2000"; "4000"])
      ~use_arrows:`DEFAULT ~case_sensitive:true ~value_in_list:false
      ~ok_if_empty:true ~packing:(hbox_473#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_45 =
    GBin.frame ~label:(Mes.options) ~shadow_type:`NONE
      ~packing:(_anonymous_container_42#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_46 =
    GPack.vbox ~homogeneous:false ~packing:_anonymous_container_45#add ()
  in
  let _anonymous_container_47 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_46#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.min_size) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_47#pack ~expand:false ~fill:true) ()
  in
  let entry_search_minsize =
    GEdit.entry ~width:80 ~text:"" ~visibility:true ~editable:true
      ~packing:(_anonymous_container_47#pack ~expand:false ~fill:false) ()
  in
  let combo_search_minsize_unit =
    GEdit.combo ~width:50 ~popdown_strings:(["Mo"; "ko"; ""])
      ~use_arrows:`DEFAULT ~case_sensitive:true ~value_in_list:true
      ~ok_if_empty:true
      ~packing:(_anonymous_container_47#pack ~expand:false ~fill:false) ()
  in
  let _anonymous_container_48 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_46#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.max_size) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_48#pack ~expand:false ~fill:true) ()
  in
  let entry_search_maxsize =
    GEdit.entry ~width:80 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_48#pack ~expand:false ~fill:true) ()
  in
  let combo_search_maxsize_unit =
    GEdit.combo ~width:50 ~popdown_strings:(["Mo"; "ko"; ""])
      ~use_arrows:`DEFAULT ~case_sensitive:true ~value_in_list:true
      ~ok_if_empty:true
      ~packing:(_anonymous_container_48#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_49 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_46#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.media) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_49#pack ~expand:false ~fill:true) ()
  in
  let combo_search_media =
    GEdit.combo
      ~popdown_strings:(
        [""; "Audio"; "Video"; "Program"; "Image"; "Documentation";
         "Collection"])
      ~use_arrows:`DEFAULT ~case_sensitive:true ~value_in_list:true
      ~ok_if_empty:true
      ~packing:(_anonymous_container_49#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_50 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_46#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.format) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_50#pack ~expand:false ~fill:true) ()
  in
  let combo_format =
    GEdit.combo ~popdown_strings:([""; "avi"; "mp3"]) ~use_arrows:`DEFAULT
      ~case_sensitive:true ~value_in_list:false ~ok_if_empty:true
      ~packing:(_anonymous_container_50#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_51 =
    GBin.frame ~label:(Mes.mp3_options) ~shadow_type:`NONE
      ~packing:(_anonymous_container_42#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_52 =
    GPack.vbox ~homogeneous:false ~packing:_anonymous_container_51#add ()
  in
  let _anonymous_container_53 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_52#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.album) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_53#pack ~expand:false ~fill:true) ()
  in
  let entry_album =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_53#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_54 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_52#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.artist) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_54#pack ~expand:false ~fill:true) ()
  in
  let entry_artist =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_54#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_55 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_52#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.title) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_55#pack ~expand:false ~fill:true) ()
  in
  let entry_title =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_55#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_56 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_52#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.min_bitrate) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_56#pack ~expand:false ~fill:true) ()
  in
  let combo_min_bitrate =
    GEdit.combo ~popdown_strings:([""; "64"; "96"; "128"; "160"; "192"])
      ~use_arrows:`DEFAULT ~case_sensitive:true ~value_in_list:true
      ~ok_if_empty:true
      ~packing:(_anonymous_container_56#pack ~expand:false ~fill:true) ()
  in
  let button_search_submit =
    GButton.button
      ~packing:(
        _anonymous_container_42#pack ~expand:false ~fill:true ~padding:5)
      ()
  in
  let _ =
    GMisc.label ~text:(Mes.submit) ~justify:`LEFT ~line_wrap:true
      ~packing:button_search_submit#add ()
  in
  let _anonymous_container_57 =
    GPack.hbox ~homogeneous:true
      ~packing:(_anonymous_container_42#pack ~expand:false ~fill:true) ()
  in
  let button_extended_search =
    GButton.button
      ~packing:(_anonymous_container_57#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.extended_search) ~justify:`LEFT ~line_wrap:true
      ~packing:button_extended_search#add ()
  in
  let button_local_search =
    GButton.button
      ~packing:(_anonymous_container_57#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.local_search) ~justify:`LEFT ~line_wrap:true
      ~packing:button_local_search#add ()
  in
  let _anonymous_container_58 =
    GBin.frame ~width:600 ~border_width:1 ~label:(Mes.results)
      ~shadow_type:`ETCHED_OUT ~packing:_anonymous_container_40#add2 ()
  in
  let notebook_results =
    GPack.notebook ~tab_pos:`TOP ~show_tabs:true ~homogeneous_tabs:true
      ~show_border:true ~scrollable:true ~popup:true
      ~packing:_anonymous_container_58#add ()
  in
  object
    val entry_search_words = entry_search_words
    val hbox_473 = hbox_473
    val combo_max_hits = combo_max_hits
    val entry_search_minsize = entry_search_minsize
    val combo_search_minsize_unit = combo_search_minsize_unit
    val entry_search_maxsize = entry_search_maxsize
    val combo_search_maxsize_unit = combo_search_maxsize_unit
    val combo_search_media = combo_search_media
    val combo_format = combo_format
    val entry_album = entry_album
    val entry_artist = entry_artist
    val entry_title = entry_title
    val combo_min_bitrate = combo_min_bitrate
    val button_search_submit = button_search_submit
    val button_extended_search = button_extended_search
    val button_local_search = button_local_search
    val notebook_results = notebook_results
    method entry_search_words = entry_search_words
    method hbox_473 = hbox_473
    method combo_max_hits = combo_max_hits
    method entry_search_minsize = entry_search_minsize
    method combo_search_minsize_unit = combo_search_minsize_unit
    method entry_search_maxsize = entry_search_maxsize
    method combo_search_maxsize_unit = combo_search_maxsize_unit
    method combo_search_media = combo_search_media
    method combo_format = combo_format
    method entry_album = entry_album
    method entry_artist = entry_artist
    method entry_title = entry_title
    method combo_min_bitrate = combo_min_bitrate
    method button_search_submit = button_search_submit
    method button_extended_search = button_extended_search
    method button_local_search = button_local_search
    method notebook_results = notebook_results
    method coerce = _anonymous_container_40#coerce
  end;;
class box_options () =
  let box = GPack.hbox ~homogeneous:false () in
  let _anonymous_container_59 =
    GPack.vbox ~homogeneous:false ~packing:(box#pack ~expand:true ~fill:true)
      ()
  in
  let _anonymous_container_60 =
    GPack.hbox ~width:0 ~homogeneous:false ~spacing:3
      ~packing:(_anonymous_container_59#pack ~expand:true ~fill:true) ()
  in
  let _anonymous_container_61 =
    GPack.vbox ~width:400 ~homogeneous:false
      ~packing:(_anonymous_container_60#pack ~expand:true ~fill:true) ()
  in
  let _frame_options_ports =
    GBin.frame ~label:(Mes.ports) ~shadow_type:`ETCHED_OUT
      ~packing:(_anonymous_container_61#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_62 =
    GPack.vbox ~homogeneous:false ~packing:_frame_options_ports#add ()
  in
  let _anonymous_container_63 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_62#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.connection_port) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_63#pack ~expand:false ~fill:true) ()
  in
  let entry_options_conn_port =
    GEdit.entry ~width:100 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_63#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_64 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_62#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.control_port) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_64#pack ~expand:false ~fill:true) ()
  in
  let entry_options_rmt_port =
    GEdit.entry ~width:100 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_64#pack ~expand:false ~fill:true) ()
  in
  let _frame_options_delays =
    GBin.frame ~label:(Mes.delays) ~shadow_type:`ETCHED_OUT
      ~packing:(_anonymous_container_61#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_65 =
    GPack.vbox ~homogeneous:false ~packing:_frame_options_delays#add ()
  in
  let _anonymous_container_66 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.save_options_delay) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_66#pack ~expand:false ~fill:true) ()
  in
  let entry_options_save_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_66#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_67 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.check_client_connections) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_67#pack ~expand:false ~fill:true) ()
  in
  let entry_options_check_clients_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_67#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_68 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.check_server_connection) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_68#pack ~expand:false ~fill:true) ()
  in
  let entry_options_check_servers_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_68#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_69 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.small_retry_delay) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_69#pack ~expand:false ~fill:true) ()
  in
  let entry_options_small_retry_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_69#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_70 =
    GPack.hbox ~width:200 ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.medium_retry_delay) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_70#pack ~expand:false ~fill:true) ()
  in
  let entry_options_medium_retry_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_70#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_71 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.long_retry_delay) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_71#pack ~expand:false ~fill:true) ()
  in
  let entry_options_long_retry_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_71#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_72 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_65#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.gui_refresh_delay) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_72#pack ~expand:false ~fill:true) ()
  in
  let entry_refresh_delay =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_72#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_73 =
    GPack.vbox ~width:400 ~homogeneous:false
      ~packing:(_anonymous_container_60#pack ~expand:false ~fill:true) ()
  in
  let _frame_options_general =
    GBin.frame ~label:(Mes.general) ~shadow_type:`ETCHED_OUT
      ~packing:(_anonymous_container_73#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_74 =
    GPack.vbox ~homogeneous:false ~packing:_frame_options_general#add ()
  in
  let _anonymous_container_75 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_74#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:100 ~text:(Mes.name) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_75#pack ~expand:false ~fill:true) ()
  in
  let entry_options_name =
    GEdit.entry ~width:260 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_75#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_76 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_74#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.max_connected_servers) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_76#pack ~expand:false ~fill:true) ()
  in
  let entry_options_maxconn_servers =
    GEdit.entry ~width:100 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_76#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_77 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_74#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.upload_limit) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_77#pack ~expand:false ~fill:true) ()
  in
  let entry_options_upload_limit =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_77#pack ~expand:false ~fill:true) ()
  in
  let _frame_options_timeouts =
    GBin.frame ~label:(Mes.timeouts) ~shadow_type:`ETCHED_OUT
      ~packing:(_anonymous_container_73#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_78 =
    GPack.vbox ~homogeneous:false ~packing:_frame_options_timeouts#add ()
  in
  let _anonymous_container_79 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_78#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.server_connection) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_79#pack ~expand:false ~fill:true) ()
  in
  let entry_options_server_timeout =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_79#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_80 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_78#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.client_connection) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_80#pack ~expand:false ~fill:true) ()
  in
  let entry_options_client_timeout =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_80#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_81 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_78#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.max_server_age) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_81#pack ~expand:false ~fill:true) ()
  in
  let entry_max_server_age =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_81#pack ~expand:false ~fill:true) ()
  in
  let _frame_server =
    GBin.frame ~label:"Server" ~shadow_type:`NONE
      ~packing:(_anonymous_container_73#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_82 =
    GPack.vbox ~homogeneous:false ~packing:_frame_server#add ()
  in
  let _anonymous_container_83 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_82#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.hostname) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_83#pack ~expand:false ~fill:true) ()
  in
  let entry_server_hostname =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_83#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_84 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_82#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.password) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_84#pack ~expand:false ~fill:true) ()
  in
  let entry_options_password =
    GEdit.entry ~visibility:true ~editable:true
      ~packing:(_anonymous_container_84#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_85 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_82#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.gui_port) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_85#pack ~expand:false ~fill:true) ()
  in
  let entry_options_gui_port =
    GEdit.entry ~width:100 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_85#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_86 =
    GPack.hbox ~homogeneous:true
      ~packing:(_anonymous_container_59#pack ~expand:false ~fill:true) ()
  in
  let button_options_save =
    GButton.button
      ~packing:(_anonymous_container_86#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.save_and_apply_options) ~justify:`LEFT
      ~line_wrap:true ~packing:button_options_save#add ()
  in
  object
    val box = box
    val entry_options_conn_port = entry_options_conn_port
    val entry_options_rmt_port = entry_options_rmt_port
    val entry_options_save_delay = entry_options_save_delay
    val entry_options_check_clients_delay = entry_options_check_clients_delay
    val entry_options_check_servers_delay = entry_options_check_servers_delay
    val entry_options_small_retry_delay = entry_options_small_retry_delay
    val entry_options_medium_retry_delay = entry_options_medium_retry_delay
    val entry_options_long_retry_delay = entry_options_long_retry_delay
    val entry_refresh_delay = entry_refresh_delay
    val entry_options_name = entry_options_name
    val entry_options_maxconn_servers = entry_options_maxconn_servers
    val entry_options_upload_limit = entry_options_upload_limit
    val entry_options_server_timeout = entry_options_server_timeout
    val entry_options_client_timeout = entry_options_client_timeout
    val entry_max_server_age = entry_max_server_age
    val entry_server_hostname = entry_server_hostname
    val entry_options_password = entry_options_password
    val entry_options_gui_port = entry_options_gui_port
    val button_options_save = button_options_save
    method box = box
    method entry_options_conn_port = entry_options_conn_port
    method entry_options_rmt_port = entry_options_rmt_port
    method entry_options_save_delay = entry_options_save_delay
    method entry_options_check_clients_delay =
      entry_options_check_clients_delay
    method entry_options_check_servers_delay =
      entry_options_check_servers_delay
    method entry_options_small_retry_delay = entry_options_small_retry_delay
    method entry_options_medium_retry_delay = entry_options_medium_retry_delay
    method entry_options_long_retry_delay = entry_options_long_retry_delay
    method entry_refresh_delay = entry_refresh_delay
    method entry_options_name = entry_options_name
    method entry_options_maxconn_servers = entry_options_maxconn_servers
    method entry_options_upload_limit = entry_options_upload_limit
    method entry_options_server_timeout = entry_options_server_timeout
    method entry_options_client_timeout = entry_options_client_timeout
    method entry_max_server_age = entry_max_server_age
    method entry_server_hostname = entry_server_hostname
    method entry_options_password = entry_options_password
    method entry_options_gui_port = entry_options_gui_port
    method button_options_save = button_options_save
    method coerce = box#coerce
  end;;
class box_help () =
  let _anonymous_container_87 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ()
  in
  let text =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:_anonymous_container_87#add ()
  in
  object
    val text = text
    method text = text
    method coerce = _anonymous_container_87#coerce
  end;;
class box_console () =
  let _anonymous_container_88 = GPack.vbox ~homogeneous:false () in
  let _anonymous_container_89 =
    GPack.hbox ~homogeneous:false
      ~packing:(_anonymous_container_88#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~width:200 ~text:(Mes.command) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_89#pack ~expand:false ~fill:true) ()
  in
  let entry_command =
    GEdit.entry ~width:500 ~visibility:true ~editable:true
      ~packing:(_anonymous_container_89#pack ~expand:false ~fill:true) ()
  in
  let _anonymous_container_90 =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(_anonymous_container_88#pack ~expand:true ~fill:true) ()
  in
  let text =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:_anonymous_container_90#add ()
  in
  let button_clear_console =
    GButton.button
      ~packing:(_anonymous_container_88#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(Mes.clear_console) ~justify:`LEFT ~line_wrap:true
      ~packing:button_clear_console#add ()
  in
  object
    val entry_command = entry_command
    val text = text
    val button_clear_console = button_clear_console
    method entry_command = entry_command
    method text = text
    method button_clear_console = button_clear_console
    method coerce = _anonymous_container_88#coerce
  end;;
class gui () =
  let box = GPack.vbox ~width:800 ~height:400 ~homogeneous:false () in
  let menubar =
    GMenu.menu_bar ~width:800 ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let _Menu =
    GMenu.menu_item ~label:(Mes.mConnections) ~packing:menubar#add ()
  in
  let _FileMenu = GMenu.menu ~packing:_Menu#set_submenu () in
  let itemReconnect =
    GMenu.menu_item ~label:(Mes.mReconnect) ~packing:_FileMenu#add ()
  in
  let itemDisconnect =
    GMenu.menu_item ~label:(Mes.mDisconnect) ~packing:_FileMenu#add ()
  in
  let itemKill =
    GMenu.menu_item ~label:(Mes.kill_server) ~packing:_FileMenu#add ()
  in
  let itemQuit = GMenu.menu_item ~label:"Exit" ~packing:_FileMenu#add () in
  let _Menu_notebook =
    GMenu.menu_item ~label:"Pages" ~packing:menubar#add ()
  in
  let menu456 = GMenu.menu ~packing:_Menu_notebook#set_submenu () in
  let itemServers =
    GMenu.menu_item ~label:"Servers" ~packing:menu456#add ()
  in
  let itemDownloads =
    GMenu.menu_item ~label:"Downloads" ~packing:menu456#add ()
  in
  let itemFriends =
    GMenu.menu_item ~label:"Friends" ~packing:menu456#add ()
  in
  let itemSearches =
    GMenu.menu_item ~label:"Queries" ~packing:menu456#add ()
  in
  let itemOptions =
    GMenu.menu_item ~label:"Options" ~packing:menu456#add ()
  in
  let itemHelp = GMenu.menu_item ~label:"Help" ~packing:menu456#add () in
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = _FileMenu#set_accel_group accel_menubar in
  let _ =
    itemReconnect#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._R
  in
  let _ =
    itemDisconnect#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._X
  in
  let _ =
    itemQuit#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._E
  in
  let _ = menu456#set_accel_group accel_menubar in
  let _ =
    itemServers#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._S
  in
  let _ =
    itemDownloads#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._D
  in
  let _ =
    itemFriends#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._F
  in
  let _ =
    itemSearches#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._Q
  in
  let _ =
    itemOptions#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._O
  in
  let _ =
    itemHelp#add_accelerator ~group:accel_menubar ~modi:([`CONTROL])
      ~flags:([`VISIBLE; `LOCKED]) GdkKeysyms._H
  in
  let notebook =
    GPack.notebook ~width:800 ~tab_pos:`TOP ~show_tabs:true
      ~homogeneous_tabs:true ~show_border:true ~scrollable:true ~popup:true
      ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let tab_servers = new box_servers () in
  let _ =
    notebook#append_page
      ~tab_label:(GMisc.label ~text:(Mes.servers) ())#coerce
      tab_servers#coerce
  in
  let tab_downloads = new box_downloads () in
  let _ =
    notebook#append_page
      ~tab_label:(GMisc.label ~text:(Mes.downloads) ())#coerce
      tab_downloads#coerce
  in
  let tab_friends = new box_friends () in
  let _ =
    notebook#append_page
      ~tab_label:(GMisc.label ~text:(Mes.friends) ())#coerce
      tab_friends#coerce
  in
  let tab_searches = new box_searches () in
  let _ =
    notebook#append_page ~tab_label:(GMisc.label ~text:"Queries" ())#coerce
      tab_searches#coerce
  in
  let tab_options = new box_options () in
  let _ =
    notebook#append_page
      ~tab_label:(GMisc.label ~text:(Mes.options) ())#coerce
      tab_options#coerce
  in
  let tab_console = new box_console () in
  let _ =
    notebook#append_page
      ~tab_label:(GMisc.label ~text:(Mes.console) ())#coerce
      tab_console#coerce
  in
  let tab_help = new box_help () in
  let _ =
    notebook#append_page ~tab_label:(GMisc.label ~text:(Mes.help) ())#coerce
      tab_help#coerce
  in
  let _anonymous_container_91 =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let label_connect_status =
    GMisc.label ~text:(Mes.not_connected) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_91#pack ~expand:true ~fill:true) ()
  in
  let label_servers_status =
    GMisc.label ~text:(Mes.connected_to_servers 0 0) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_91#pack ~expand:true ~fill:true) ()
  in
  let label_download_status =
    GMisc.label ~text:(Mes.downloaded_files 0 0) ~justify:`LEFT
      ~line_wrap:true
      ~packing:(_anonymous_container_91#pack ~expand:true ~fill:true) ()
  in
  let label_searches_status =
    GMisc.label ~text:(Mes.no_current_search) ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_91#pack ~expand:true ~fill:true) ()
  in
  let label_upload_status =
    GMisc.label ~text:"" ~justify:`LEFT ~line_wrap:true
      ~packing:(_anonymous_container_91#pack ~expand:true ~fill:true) ()
  in
  object
    val box = box
    val menubar = menubar
    val accel_menubar = accel_menubar
    val itemReconnect = itemReconnect
    val itemDisconnect = itemDisconnect
    val itemKill = itemKill
    val itemQuit = itemQuit
    val menu456 = menu456
    val itemServers = itemServers
    val itemDownloads = itemDownloads
    val itemFriends = itemFriends
    val itemSearches = itemSearches
    val itemOptions = itemOptions
    val itemHelp = itemHelp
    val notebook = notebook
    val tab_servers = tab_servers
    val tab_downloads = tab_downloads
    val tab_friends = tab_friends
    val tab_searches = tab_searches
    val tab_options = tab_options
    val tab_console = tab_console
    val tab_help = tab_help
    val label_connect_status = label_connect_status
    val label_servers_status = label_servers_status
    val label_download_status = label_download_status
    val label_searches_status = label_searches_status
    val label_upload_status = label_upload_status
    method box = box
    method menubar = menubar
    method accel_menubar = accel_menubar
    method itemReconnect = itemReconnect
    method itemDisconnect = itemDisconnect
    method itemKill = itemKill
    method itemQuit = itemQuit
    method menu456 = menu456
    method itemServers = itemServers
    method itemDownloads = itemDownloads
    method itemFriends = itemFriends
    method itemSearches = itemSearches
    method itemOptions = itemOptions
    method itemHelp = itemHelp
    method notebook = notebook
    method tab_servers = tab_servers
    method tab_downloads = tab_downloads
    method tab_friends = tab_friends
    method tab_searches = tab_searches
    method tab_options = tab_options
    method tab_console = tab_console
    method tab_help = tab_help
    method label_connect_status = label_connect_status
    method label_servers_status = label_servers_status
    method label_download_status = label_download_status
    method label_searches_status = label_searches_status
    method label_upload_status = label_upload_status
    method coerce = box#coerce
  end;;
