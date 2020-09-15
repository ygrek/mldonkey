
open Gettext

module M = Gui_messages

(* icon_on icon_off menu_icon wlabel *)

let main_nbk_data =
[
 (M.o_xpm_nbk_networks_on, M.o_xpm_nbk_networks_off, M.o_xpm_nbk_networks_menu, M.mW_lb_networks);
 (M.o_xpm_nbk_servers_on, M.o_xpm_nbk_servers_off, M.o_xpm_nbk_servers_menu, M.mW_lb_servers);
 (M.o_xpm_nbk_downloads_on, M.o_xpm_nbk_downloads_off, M.o_xpm_nbk_downloads_menu, M.mW_lb_downloads);
 (M.o_xpm_nbk_friends_on, M.o_xpm_nbk_friends_off, M.o_xpm_nbk_friends_menu, M.mW_lb_friends);
 (M.o_xpm_nbk_search_on, M.o_xpm_nbk_search_off, M.o_xpm_nbk_search_menu, M.mW_lb_search);
 (M.o_xpm_nbk_rooms_on, M.o_xpm_nbk_rooms_off, M.o_xpm_nbk_rooms_menu, M.mW_lb_rooms);
 (M.o_xpm_nbk_uploads_on, M.o_xpm_nbk_uploads_off, M.o_xpm_nbk_uploads_menu, M.mW_lb_uploads);
 (M.o_xpm_nbk_console_on, M.o_xpm_nbk_console_off, M.o_xpm_nbk_console_menu, M.mW_lb_console);
 (M.o_xpm_nbk_graphs_on, M.o_xpm_nbk_graphs_off, M.o_xpm_nbk_graphs_menu, M.mW_lb_graph);
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
      ~title:(Gui_messages.mW_wt_software) ~allow_shrink:true ~allow_grow:true
      ~auto_shrink:true ~modal:false ()
  in
  let box =
    GPack.vbox ~homogeneous:false ~packing:(window#add) () in
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
                     ~tooltip:(Gui_messages.mW_ti_about)
                     ~icon: (Gui_options.pixmap Gui_messages.o_xpm_about)#coerce
                   ();
  in
  let buttonOptions = wtool1#insert_button
                       ~tooltip:(Gui_messages.mW_ti_settings)
                       ~icon: (Gui_options.pixmap Gui_messages.o_xpm_settings)#coerce
                       ();
  in
  let buttonQuit = wtool1#insert_button
                    ~tooltip:(Gui_messages.mW_ti_exit)
                    ~icon: (Gui_options.pixmap Gui_messages.o_xpm_exit)#coerce
                    ();
  in
  let buttonGui = wtool1#insert_button
                   ~tooltip:(Gui_messages.mW_ti_gui)
                   ~icon: (Gui_options.pixmap Gui_messages.o_xpm_gui)#coerce
                    ();
  in
  let buttonKill = wtool1#insert_button
                    ~tooltip:(Gui_messages.mW_ti_kill_core)
                    ~icon: (Gui_options.pixmap Gui_messages.o_xpm_kill_core)#coerce
                    ();
  in
  let hbox_status =
    GPack.hbox ~homogeneous:false ~spacing:10
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let label_connect_status =
    GMisc.label ~text:(Gui_messages.mW_lb_not_connected)
      ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0)
      ~packing:(hbox_status#pack ~expand:true ~fill:true) ()
  in
  object
    val window = window
    val box = box
    val notebook = notebook
    val buttonAbout = buttonAbout
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

    method notebook = notebook
    method buttonAbout = buttonAbout
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
