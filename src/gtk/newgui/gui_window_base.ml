(* icon_on icon_off menu_icon wlabel *)
let main_nbk_data =
[
  (Gui_messages.o_xpm_nbk_networks_on, Gui_messages.o_xpm_nbk_networks_off, Gui_messages.o_xpm_nbk_networks_menu,
    Gettext.gettext Gui_messages.mNetworks);
  (Gui_messages.o_xpm_nbk_servers_on, Gui_messages.o_xpm_nbk_servers_off, Gui_messages.o_xpm_nbk_servers_menu,
    Gettext.gettext Gui_messages.mServers);
  (Gui_messages.o_xpm_nbk_downloads_on, Gui_messages.o_xpm_nbk_downloads_off, Gui_messages.o_xpm_nbk_downloads_menu,
    Gettext.gettext Gui_messages.mDownloads);
  (Gui_messages.o_xpm_nbk_friends_on, Gui_messages.o_xpm_nbk_friends_off, Gui_messages.o_xpm_nbk_friends_menu,
    Gettext.gettext Gui_messages.mFriends);
  (Gui_messages.o_xpm_nbk_search_on, Gui_messages.o_xpm_nbk_search_off, Gui_messages.o_xpm_nbk_search_menu,
    Gettext.gettext Gui_messages.mSearch);
  (Gui_messages.o_xpm_nbk_rooms_on, Gui_messages.o_xpm_nbk_rooms_off, Gui_messages.o_xpm_nbk_rooms_menu,
    Gettext.gettext Gui_messages.mRooms);
  (Gui_messages.o_xpm_nbk_uploads_on, Gui_messages.o_xpm_nbk_uploads_off, Gui_messages.o_xpm_nbk_uploads_menu,
    Gettext.gettext Gui_messages.uploads);
  (Gui_messages.o_xpm_nbk_console_on, Gui_messages.o_xpm_nbk_console_off, Gui_messages.o_xpm_nbk_console_menu,
    Gettext.gettext Gui_messages.mConsole);
  (Gui_messages.o_xpm_nbk_graphs_on, Gui_messages.o_xpm_nbk_graphs_off, Gui_messages.o_xpm_nbk_graphs_menu,
    Gettext.gettext Gui_messages.mGraph);
]

let tab_box n str data =
  let (icon_on, icon_off, _, wlabel) = List.nth data n in
  let wpix = if str then icon_on else icon_off in
  let hbox = GPack.hbox ~homogeneous:false () in
  let main_pix = Gui_options.gdk_pix wpix in
  ignore (GMisc.pixmap main_pix ~packing:hbox#add ());
  let label = GMisc.label ~text:wlabel ~packing:hbox#add () in
  let style = label#misc#style#copy in
  style#set_font (Gdk.Font.load_fontset (Options.(!!) Gui_options.font_main_tab));
  let fg_col = if str then (Options.(!!) Gui_options.color_tab_selected)
                      else (Options.(!!) Gui_options.color_tab_not_selected)
  in
  style#set_fg [(`NORMAL, `NAME fg_col)];
  label#misc#set_style style;
  hbox

let menu_label_box n data =
  let hbox = GPack.hbox ~homogeneous:false ~spacing:2 () in
  let ( _, _, icon, menu_text) = List.nth data n in
  let menu_pix = Gui_options.gdk_pix icon in
  ignore (GMisc.pixmap menu_pix ~packing:hbox#pack ());
  ignore (GMisc.label ~text:menu_text ~justify:`LEFT ~packing:hbox#pack ());
  hbox

class window () =

  let window =
    GWindow.window ~width:(Options.( !! ) Gui_options.gui_width)
      ~height:(Options.( !! ) Gui_options.gui_height)
      ~title:(Gui_messages.software) ~allow_shrink:true ~allow_grow:true
      ~auto_shrink:true ~modal:false ()
  in
  let box =
    GPack.vbox ~homogeneous:false ~packing:(window#add) () in
  let menubar =
    GMenu.menu_bar ~packing:(box#pack ~expand:false ~fill:false) ()
  in
  let g_menu =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mFile)
      ~packing:(menubar#add) ()
  in
  let g_menu = GMenu.menu ~packing:(g_menu#set_submenu) () in
  let _ = GMenu.menu_item ~packing:(g_menu#add) () in
  let itemReconnect =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mReconnect)
      ~packing:(g_menu#add) ()
  in
  let itemDisconnect =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mDisconnect)
      ~packing:(g_menu#add) ()
  in
  let itemScanPorts =
    GMenu.menu_item ~label:"Scan ports" ~packing:(g_menu#add) ()
  in
  let cores =
    GMenu.menu_item ~label:"Reconnect To" ~packing:(g_menu#add) ()
  in
  let cores_menu = GMenu.menu ~packing:(cores#set_submenu) () in
  let _ = GMenu.menu_item ~packing:(g_menu#add) () in
  let _Menu_notebook =
    GMenu.menu_item ~label:"Pages" ~packing:(menubar#add) ()
  in
  let _menu456 = GMenu.menu ~packing:(_Menu_notebook#set_submenu) () in
  let itemServers =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mServers)
      ~packing:(_menu456#add) ()
  in
  let itemDownloads =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mDownloads)
      ~packing:(_menu456#add) ()
  in
  let itemFriends =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mFriends)
      ~packing:(_menu456#add) ()
  in
  let itemResults =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mSearch)
      ~packing:(_menu456#add) ()
  in
  let itemRooms =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mRooms)
      ~packing:(_menu456#add) ()
  in
  let itemUploads =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mUploads)
      ~packing:(_menu456#add) ()
  in
  let itemConsole =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mConsole)
      ~packing:(_menu456#add) ()
  in
  let itemHelp =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.mGraph)
      ~packing:(_menu456#add) ()
  in
  let _Menu_display =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.display)
      ~packing:(menubar#add) ()
  in
  let menu_display = GMenu.menu ~packing:(_Menu_display#set_submenu) () in
  let _Menu_networks =
    GMenu.menu_item ~label:(Gettext.gettext Gui_messages.network)
      ~packing:(menubar#add) ()
  in
  let menu_networks = GMenu.menu ~packing:(_Menu_networks#set_submenu) () in
  let accel_menubar = GtkData.AccelGroup.create () in
  let _ = window#add_accel_group accel_menubar in
  let _ = g_menu#set_accel_group accel_menubar in
  let _ = cores_menu#set_accel_group accel_menubar in
  let _ = _menu456#set_accel_group accel_menubar in
  let _ = menu_display#set_accel_group accel_menubar in
  let _ = menu_networks#set_accel_group accel_menubar in
  let hbox_w = GPack.hbox ~packing:box#add () in
  let notebook =
    GPack.notebook ~tab_pos:(Options.( !! ) Gui_options.notebook_tab)
      ~show_tabs:true ~homogeneous_tabs:false ~show_border:true
      ~scrollable:true ~popup:true ~packing:(hbox_w#pack~expand:true ~fill:true) ()
  in
  let tab_networks = new Gui_networks.networks_box () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 0 false main_nbk_data)#coerce
            ~menu_label:(menu_label_box 0 main_nbk_data)#coerce
            tab_networks#coerce
  in
  let tab_servers = new Gui_servers.pane_servers () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 1 false main_nbk_data)#coerce
            ~menu_label:(menu_label_box 1 main_nbk_data)#coerce
            tab_servers#coerce
  in
  let tab_downloads = new Gui_downloads.pane_downloads () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 2 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 2 main_nbk_data)#coerce
            tab_downloads#coerce
  in
  let tab_friends = new Gui_friends.pane_friends () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 3 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 3 main_nbk_data)#coerce
            tab_friends#coerce
  in
  let tab_queries = new Gui_queries.paned () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 4 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 4 main_nbk_data)#coerce
            tab_queries#coerce
  in
  let tab_rooms = new Gui_rooms.pane_rooms () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 5 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 5 main_nbk_data)#coerce
            tab_rooms#coerce
  in
  let tab_uploads = new Gui_uploads.upstats_box () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 6 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 6 main_nbk_data)#coerce
            tab_uploads#coerce
  in
  let tab_console = new Gui_console.box () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 7 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 7 main_nbk_data)#coerce
            tab_console#coerce
  in
  let tab_graph = new Gui_graph.box () in
  let _ = notebook#append_page
            ~tab_label:(tab_box 8 false main_nbk_data)#coerce
	    ~menu_label:(menu_label_box 8 main_nbk_data)#coerce
            tab_graph#coerce
  in
  let vbox = GPack.vbox ~homogeneous:false ~packing:(hbox_w#pack~expand:false ~fill:false) () in
  let wtool1 =
    GButton.toolbar ~orientation:`VERTICAL ~style:`BOTH
      ~tooltips:true ~button_relief:`NORMAL
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let buttonAbout = wtool1#insert_button
		     ~tooltip:(Gettext.gettext Gui_messages.about)
                     ~icon: (Gui_options.pixmap Gui_messages.o_xpm_about)#coerce
                   ();
  in
  let buttonIm = wtool1#insert_button
                  ~tooltip:(Gettext.gettext Gui_messages.im)
                  ~icon: (Gui_options.pixmap Gui_messages.o_xpm_im)#coerce
                 ();
  in
  let buttonOptions = wtool1#insert_button
                       ~tooltip:(Gettext.gettext Gui_messages.settings)
                       ~icon: (Gui_options.pixmap Gui_messages.o_xpm_settings)#coerce
                       ();
  in
  let buttonQuit = wtool1#insert_button
                    ~tooltip:(Gettext.gettext Gui_messages.exit)
                    ~icon: (Gui_options.pixmap Gui_messages.o_xpm_exit)#coerce
                    ();
  in
  let buttonGui = wtool1#insert_button
                   ~tooltip:(Gettext.gettext Gui_messages.gui)
                   ~icon: (Gui_options.pixmap Gui_messages.o_xpm_gui)#coerce
                    ();
  in
  let buttonKill = wtool1#insert_button
                    ~tooltip:(Gettext.gettext Gui_messages.kill_core)
                    ~icon: (Gui_options.pixmap Gui_messages.o_xpm_kill_core)#coerce
                    ();
  in
  let hbox_status =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let label_connect_status =
    GMisc.label ~text:(Gettext.gettext Gui_messages.not_connected)
      ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0)
      ~packing:(hbox_status#pack ~expand:true ~fill:true) ()
  in
  let _ = menubar#misc#hide () in
  object
    val window = window
    val box = box
    val menubar = menubar
    val accel_menubar = accel_menubar
    val itemReconnect = itemReconnect
    val itemDisconnect = itemDisconnect
    val itemScanPorts = itemScanPorts
    val cores = cores
    val cores_menu = cores_menu
    val itemServers = itemServers
    val itemDownloads = itemDownloads
    val itemFriends = itemFriends
    val itemResults = itemResults
    val itemRooms = itemRooms
    val itemUploads = itemUploads
    val itemConsole = itemConsole
    val itemHelp = itemHelp
    val g_menu = g_menu
    val menu_display = menu_display
    val menu_networks = menu_networks
    val notebook = notebook
    val buttonAbout = buttonAbout
    val buttonIm = buttonIm
    val buttonOptions = buttonOptions
    val buttonQuit = buttonQuit
    val buttonGui = buttonGui
    val buttonKill = buttonKill
    val tab_networks = tab_networks
    val tab_servers = tab_servers
    val tab_downloads = tab_downloads
    val tab_friends = tab_friends
    val tab_queries = tab_queries
    val tab_rooms = tab_rooms
    val tab_uploads = tab_uploads
    val tab_console = tab_console
    val tab_graph = tab_graph
    val hbox_status = hbox_status
    val label_connect_status = label_connect_status
    method window = window
    method box = box
    method menubar = menubar
    method accel_menubar = accel_menubar
    method itemReconnect = itemReconnect
    method itemDisconnect = itemDisconnect
    method itemScanPorts = itemScanPorts
    method cores = cores
    method cores_menu = cores_menu
    method itemServers = itemServers
    method itemDownloads = itemDownloads
    method itemFriends = itemFriends
    method itemResults = itemResults
    method itemRooms = itemRooms
    method itemUploads = itemUploads
    method itemConsole = itemConsole
    method itemHelp = itemHelp
    method g_menu = g_menu
    method menu_display = menu_display
    method menu_networks = menu_networks
    method notebook = notebook
    method buttonAbout = buttonAbout
    method buttonIm = buttonIm
    method buttonOptions = buttonOptions
    method buttonQuit = buttonQuit
    method buttonGui = buttonGui
    method buttonKill = buttonKill
    method tab_networks = tab_networks
    method tab_servers = tab_servers
    method tab_downloads = tab_downloads
    method tab_friends = tab_friends
    method tab_queries = tab_queries
    method tab_rooms = tab_rooms
    method tab_uploads = tab_uploads
    method tab_console = tab_console
    method tab_graph = tab_graph
    method hbox_status = hbox_status
    method label_connect_status = label_connect_status
  end
