(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** GUI labels. *)

open Printf2
open Options
open Gettext

let message_file_name = try
    Sys.getenv "MLDONKEY_GUI_MESSAGES"
  with _ -> 
      let filename = 
        Filename.concat CommonOptions.home_dir "newgui_messages.ini"
      in
      Unix2.safe_mkdir (Filename.dirname filename);
      filename

let _s x = _s "Gui_messages" x
let _b x = _b "Gui_messages" x  


      (*
let _ =
  lprintf "Using Message File %s" message_file_name; lprint_newline ()
  *)

let message_file = Options.create_options_file message_file_name
let message_section = file_section message_file [] ""
let message name (t :  
    ('a , unit, string) format Options.option_class) x = (* define_option message_section [name] "" t x *)
  (_b x : ('a, unit, string) format)

(******************************************************************************************

For a new message convention :
all message tags are labeled as follows :
<loc>_<type>_<description> where :

<loc> : 2-letter code with first letter lowercase and second letter uppercase
* mW = main Window
* nT = networks Tab
* sT = servers Tab
* dT = downloads Tab
* fT = friends Tab
* qT = queries Tab
* uT = uploads Tab
* rT = rooms Tab
* cT = console Tab
* gT = graph Tab
* iM = Im window
* pW = for a popup window

<type>	: 2-letter code
* lb = label
* me = menu label
* ti = tips
* tx = simple text in a list
* wt = window title

<description> : as per your mood ;-)

At the first glance it does not look cute (especially because the same text is repeated
several times) but it can avoid a mess.

*******************************************************************************************)

(** Networks Tab **)
let nT_lb_display_bt = 
  _s 
  "Display BitTorrent"
let nT_lb_net_bt = 
  _s 
  "BITTORRENT"
let nT_lb_display_dc = 
  _s 
  "Display Direct Connect"
let nT_lb_net_dc =
  _s 
  "DIRECT CONNECT"
let nT_lb_display_ed2k = 
  _s 
  "Display Edonkey / Overnet"
let nT_lb_net_ed2k = 
  _s 
  "EDONKEY / OVERNET"
let nT_lb_display_nap = 
  _s 
  "Display Open Napster"
let nT_lb_net_nap = 
  _s 
  "OPEN NAPSTER"
let nT_lb_display_gnut = 
  _s 
  "Display Gnutella"
let nT_lb_net_gnut = 
  _s 
  "GNUTELLA"
let nT_lb_display_ftt = 
  _s 
  "Display Fasttrack"
let nT_lb_net_ftt = 
  _s 
  "FASTTRACK"
let nT_lb_display_slsk = 
  _s 
  "Display Soulseek"
let nT_lb_net_slsk = 
  _s 
  "SOULSEEK"

(** Servers Tab **)
let sT_wt_add_server = 
  _s 
  "Add server by Address"
let sT_ti_add_server = 
  _s 
  "Add a server"
let sT_lb_add_server = 
  _s 
  "Address ( ip : port ) : "
let sT_lb_network = 
  _s 
  "Network : "
let sT_ti_display_all_servers = 
  _s 
  "View all servers (even not connected)"
let sT_lb_display_all_servers = 
  _s 
  "View all"
let sT_lb_users = 
  _s 
  "Users"
let sT_lb_servers = 
  _s 
  "Servers"
let sT_me_remove = 
  _s 
  "Remove"
let sT_me_disconnect = 
  _s 
  "Disconnect"
let sT_me_view_users = 
  _s 
  "View users"
let sT_me_connect = 
  _s 
  "Connect"
let sT_me_connect_more_servers = 
  _s 
  "More servers"
let sT_me_remove_old_servers = 
  _s 
  "Clear list"

(** Downloads Tab **)
let dT_wt_save_as = 
  _s 
  "Save file as" 
let dT_lb_save_as = 
  _s 
  "Save file as : "
let dT_tx_downloading = 
  _s 
  "Downloading"
let dT_tx_waiting =
  _s 
  " Waiting..."
let dT_tx_cancelled = 
  _s 
  "Cancelled"
let dT_tx_queued = 
  _s 
  "Queued"
let dT_tx_paused = 
  _s 
  "Paused"
let dT_tx_complete = 
  _s 
  "Complete"
let dT_tx_dl_done = 
  _s 
  "Done"
let dT_tx_dl_aborted =
  message "dT_tx_dl_aborted" (T.option (T.string T.format)) 
  "Aborted : %s"
let dT_tx_connected = 
  _s 
  "Connected"
let dT_tx_connecting = 
  _s 
  "Connecting"
let dT_tx_new_host = 
  _s 
  "New Host"
let dT_tx_initiating = 
  _s 
  "Initiating"
let dT_tx_ranked = 
  message "dT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d"
let dT_tx_queued_out = 
  _s 
  "Queued Out"
let dT_tx_ranked_out = 
  message "dT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d out"
let dT_tx_failed = 
  message "dT_tx_failed" (T.option (T.int T.format)) 
  "Failed %d"
let dT_tx_removed = 
  _s 
  "Removed"
let dT_tx_black_listed = 
  _s 
  "Black Listed"
let dT_tx_unknown = 
  _s 
  "Unknown"
let dT_tx_priority_veryhigh = 
  _s 
  "Very High"
let dT_tx_priority_high = 
  _s 
  "High"
let dT_tx_priority_normal = 
  _s 
  "Normal"
let dT_tx_priority_low = 
  _s 
  "Low"
let dT_tx_priority_verylow = 
  _s 
  "Very Low"
let dT_wt_cancel = 
  _s 
  "Cancel File(s)"
let dT_lb_ask_cancel_download_files = 
  _s 
  "Cancel the following file(s) ?\n"
let dT_wt_edit_mp3 = 
  _s 
  "MP3 tags Editor"
let dT_me_view_sources = 
  _s 
  "Show/Hide Sources"
let dT_me_preview = 
  _s 
  "Preview"
let dT_me_pause_resume_dl = 
  _s 
  "Pause/Resume"
let dT_me_retry_connect = 
  _s 
  "Retry connect"
let dT_me_cancel = 
  _s 
  "Cancel"
let dT_me_verify_chunks = 
  _s 
  "Verify chunks"
let dT_me_set_priority = 
  _s 
  "Set priority"
let dT_me_set_priority_veryhigh = 
  _s 
  "Very High"
let dT_me_set_priority_high = 
  _s 
  "High"
let dT_me_set_priority_normal = 
  _s 
  "Normal"
let dT_me_set_priority_low = 
  _s 
  "Low"
let dT_me_set_priority_verylow = 
  _s 
  "Very Low"
let dT_me_get_format = 
  _s 
  "Get format info"
let dT_me_edit_mp3 = 
  _s 
  "Edit mp3 tags"
let dT_me_save_all = 
  _s 
  "Save all"
let dT_me_save_as = 
  _s 
  "Save file as"
let dT_me_save = 
  _s 
  "Save"
let dT_me_add_to_friends = 
  _s 
  "Add to friends"
let dT_lb_ed2k = 
  _s 
  "ed2k link : "

(** Friends Tab **)
let fT_lb_messages = 
  _s 
  "Messages"
let fT_tx_downloading = 
  _s 
  "Downloading"
let fT_tx_queued = 
  _s 
  "Queued"
let fT_tx_connected = 
  _s 
  "Connected"
let fT_tx_connecting = 
  _s 
  "Connecting"
let fT_tx_new_host = 
  _s 
  "New Host"
let fT_tx_initiating = 
  _s 
  "Initiating"
let fT_tx_ranked = 
  message "fT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d"
let fT_tx_queued_out = 
  _s 
  "Queued Out"
let fT_tx_ranked_out = 
  message "fT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d out"
let fT_tx_failed = 
  message "fT_tx_failed" (T.option (T.int T.format)) 
  "Failed %d"
let fT_tx_removed = 
  _s 
  "Removed"
let fT_tx_black_listed = 
  _s 
  "Black Listed"
let fT_tx_files_listed = 
  _s 
  "Files Listed"
let fT_tx_friend = 
  _s 
  "Friend"
let fT_tx_contact = 
  _s 
  "Contact"
let fT_tx_normal = 
  _s 
  "Normal"
let fT_tx_direct = 
  _s 
  "Direct"
let fT_wt_find_friend = 
  _s 
  "Find friend"
let fT_lb_name = 
  _s 
  "Name"
let fT_me_find_friend = 
  _s 
  "Find friend"
let fT_me_remove = 
  _s 
  "Remove friend"
let fT_me_remove_all_friends = 
  _s 
  "Clear list"

(** Query Box **)
let qT_lb_album_searches = 
  _s 
  "Album"
let qT_lb_movie_searches = 
  _s 
  "Movie"
let qT_lb_mp3_searches = 
  _s 
  "MP3"
let qT_lb_complex_searches = 
  _s 
  "Complex"
let qT_lb_sharereactor_searches = 
  _s 
  "ShareReactor"
let qT_lb_jigle_searches = 
  _s 
  "Jigle"
let qT_lb_freedb_searches = 
  _s 
  "FreeDB"
let qT_lb_imdb_searches = 
  _s 
  "IMDB"
let qT_lb_local_search = 
  _s 
  "Local Search"
let qT_lb_submit = 
  _s 
  "Submit"
let qT_lb_subscribe = 
  _s 
  "Subscribe"
let qT_ti_local_search = 
  _s 
  "Local Search"
let qT_ti_submit = 
  _s 
  "Submit"
let qT_ti_subscribe = 
  _s 
  "Subscribe"
let qT_lb_close_search = 
  _s 
  "Close Search"
let qT_ti_close_search = 
  _s 
  "Close Search"
let qT_lb_stop_search = 
  _s 
  "Stop Search"
let qT_ti_stop_search = 
  _s 
  "Stop Search"

let qT_tx_all_networks = 
  _s 
  "All Networks"
let qT_lb_show_hidden_fields = 
  _s 
  "Show hidden fields"
let qT_lb_network = 
  _s 
  "Network :"
let qT_lb_max_hits = 
  _s 
  "Max Hits"

let qT_lb_and_not = 
  _s 
  "And Not"
let qT_tx_audio = 
  _s 
  "Audio"
let qT_tx_video = 
  _s 
  "Video"
let qT_tx_program = 
  _s 
  "Program"
let qT_tx_image = 
  _s 
  "Image"
let qT_tx_documentation = 
  _s 
  "Documentation"
let qT_tx_collection = 
  _s 
  "Collection"
let qT_lb_keywords = 
  _s 
  "Keywords"
let qT_lb_media = 
  _s 
  "Media"
let qT_lb_format = 
  _s 
  "Format"
let qT_lb_min_size = 
  _s 
  "Min size"
let qT_lb_max_size = 
  _s 
  "Max size"
let qT_lb_min_bitrate = 
  _s 
  "Min Bitrate"
let qT_lb_title = 
  _s 
  "Title"
let qT_lb_number_of_results = 
  _s 
  "Number of results"
let qT_lb_sort_by = 
  _s 
  "Sort by"
let qT_lb_album = 
  _s 
  "Album"
let qT_lb_fields = 
  _s 
  "Fields"
let qT_lb_artist = 
  _s 
  "Artist"
let qT_lb_track_title = 
  _s 
  "Track/Title"
let qT_lb_track = 
  _s 
  "Track"
let qT_lb_rest = 
  _s 
  "Rest"
let qT_lb_categories = 
  _s 
  "Categories"
let qT_lb_all = 
  _s 
  "All"
let qT_lb_blues = 
  _s 
  "Blues"
let qT_lb_classical = 
  _s 
  "Classical"
let qT_lb_data = 
  _s 
  "Data"
let qT_lb_folk = 
  _s 
  "Folk"
let qT_lb_rock = 
  _s 
  "Rock"
let qT_lb_soundtrack = 
  _s 
  "Soundtrack"
let qT_lb_availability = 
  _s 
  "Availability"
let qT_lb_completesources = 
  _s 
  "Completesources"
let qT_lb_size = 
  _s 
  "Size"
let qT_lb_dvd_rips = 
  _s 
  "DVD Rips"
let qT_lb_screeners = 
  _s 
  "Screeners"
let qT_lb_pc_games = 
  _s 
  "PC Games"
let qT_lb_software = 
  _s 
  "Software"
let qT_lb_anime = 
  _s 
  "Anime"
let qT_lb_series = 
  _s 
  "Series"
let qT_lb_funstuff =  
  _s 
  "Funstuff"
let qT_lb_adult = 
  _s 
  "Adult"
let qT_lb_consoles = 
  _s 
  "Consoles"
let qT_lb_books = 
  _s 
  "Books"
let qT_lb_xbox = 
  _s 
  "XBOX"
let qT_lb_hentai = 
  _s 
  "Hentai"
let qT_lb_ps2 = 
  _s 
  "PS2"
let qT_lb_gay = 
  _s 
  "Gay"

let qT_tx_unknown = 
  _s 
  "Unknown"
let qT_tx_download = 
  _s 
  "Download"
let qT_tx_force_download = 
  _s 
  "Force Download"
let qT_lb_extended_search = 
  _s 
  "Extended Search"
let qT_ti_extended_search = 
  _s 
  "Extended Search"
let qT_lb_results = 
  message "qT_lb_results" (T.option (T.int T.format)) 
  "%d Results"
let qT_lb_waiting_for_replies = 
  message "qT_lb_waiting_for_replies" (T.option (T.int T.format)) 
  "Waiting for %d replies"
let qT_wt_download_selected_dir = 
  _s 
  "Download selected directory"
let qT_lb_download_selected_dir = 
  _s "Download"
let qT_ti_download_selected_dir = 
  _s 
  "Download the selected directory"
let qT_lb_confirm_download_dir = 
  message "qT_lb_confirm_download_dir" (T.option (T.int (T.string T.format)))
  "Do you want to download the %d files in the directory %s ?"

(** Rooms Tab **)
let rT_lb_close_room = 
  _s 
  "Close room"
let rT_ti_close_room = 
  _s 
  "Close the current room"
let rT_lb_available_rooms = 
  _s 
  "Available Rooms"
let rT_lb_opened_rooms = 
  _s 
  "Opened Rooms"
let rT_lb_rooms_messages = 
  _s 
  "Messages"
let rT_lb_users = 
  _s 
  "Users"
let rT_me_browse_files = 
  _s 
  "Browse files"
let rT_me_add_to_friends = 
  _s 
  "Add to friends"
let rT_tx_direct = 
  _s 
  "Direct"

(** Uploads **)
let uT_lb_shared_files  = 
  _s 
  "Shared files"
let uT_lb_add_shared_directory = 
  _s 
  "Add Shared Directory"
let uT_lb_priority = 
  _s 
  "Priority :"
let uT_ti_add_shared_directory = 
  _s 
  "Add a new shared directory"
let uT_wt_add_new_directory = 
  _s 
  "Add a Shared Directory"
let uT_lb_uploaders  = 
  _s 
  "Uploaders"
let uT_lb_directory = 
  _s 
  "Directory : "
let uT_lb_add_new_directory = 
  _s 
  "Add New Directory"
let uT_me_add_to_friends = 
  _s 
  "Add to friends"
let uT_me_copy_ed2k = 
  _s 
  "Copy ed2k link to console/clipboard"
let uT_lb_show_pending_slots = 
  _s 
  "Hide/Show pending slots"
let uT_ti_show_pending_slots = 
  _s 
  "Hide/Show pending slots"

(** Console **)
let cT_lb_clear_console = 
  _s 
  "Clear Console"
let cT_lb_command = 
  _s 
  "Command"

(** Graph **)
let gT_lb_downloads = 
  _s 
  "Downloads"
let gT_lb_uploads = 
  _s 
  "Uploads"

(** Im **)
let iM_wt_software = "MLDonkey"
let iM_me_file = 
  _s 
  "File"
let iM_me_settings = 
  _s 
  "Settings"
let iM_me_quit = 
  _s 
  "Quit"

(** Popup Windows **)
let pW_lb_servers_remove_icons = 
  _s 
  "Please wait ... Removing Servers icons"
let pW_lb_servers_add_icons = 
  _s 
  "Please wait ... Generating Servers icons"
let pW_lb_downloads_remove_icons = 
  _s 
  "Please wait ... Removing Downloads icons"
let pW_lb_downloads_add_icons = 
  _s 
  "Please wait ... Generating Downloads icons"
let pW_lb_friends_remove_icons = 
  _s 
  "Please wait ... Removing Friends icons"
let pW_lb_friends_add_icons = 
  _s 
  "Please wait ... Generating Friends icons"
let pW_lb_results_remove_icons = 
  _s 
  "Please wait ... Removing Searches icons"
let pW_lb_results_add_icons = 
  _s 
  "Please wait ... Generating Searches icons"
let pW_lb_rooms_remove_icons = 
  _s 
  "Please wait ... Removing Rooms icons"
let pW_lb_rooms_add_icons = 
  _s 
  "Please wait ... Generating Rooms icons"
let pW_lb_uploads_remove_icons = 
  _s 
  "Please wait ... Removing Sources icons"
let pW_lb_uploads_add_icons = 
  _s 
  "Please wait ... Generating Sources icons"
let pW_lb_ok = 
  _s 
  "Ok"
let pW_lb_cancel = 
  _s 
  "Cancel"
let pW_wt_bad_password = 
  _s 
  "Bad Password"
let pW_lb_bad_password = 
  _s 
  "Authorization Failed\nPlease, click the Settings button -> GUI -> GUI server and enter a valid password" 

(** Main Window **)
let mW_tx_action_unknown = 
  _s 
  "Unknown action: "
let mW_sb_connected_to_servers = 
  message "mW_sb_connected_to_servers" (T.option (T.int (T.int T.format))) 
  "Connected to %d/%d server(s)"
let mW_sb_downloaded_files = 
  message "mW_sb_downloaded_files" (T.option (T.int (T.int T.format))) 
  "Downloaded Files: %d/%d"
let mW_sb_files_shared = 
  message "mW_sb_files_shared" (T.option (T.int (T.string (T.int (T.int (T.int (T.int T.format))))))) 
  "Shared: %5d/%-12s   U/D bytes/s: %7d[%5d]/%-7d[%5d]"
let mW_lb_connecting = 
  _s 
  "Connecting"
let mW_lb_connected = 
  _s 
  "Connected"
let mW_lb_not_connected = 
  _s 
  "Not Connected"
let mW_ti_about = 
  _s 
  "About"
let mW_ti_settings = 
  _s 
  "Settings"
let mW_ti_exit = 
  _s 
  "Exit"
let mW_ti_gui = 
  _s 
  "Gui"
let mW_ti_kill_core = 
  _s 
  "Kill core"
let mW_me_reconnect = 
  _s 
  "Reconnect"
let mW_me_disconnect = 
  _s 
  "Disconnect"
let mW_me_scan_ports = 
  _s 
  "Scan Ports"
let mW_me_reconnect_to = 
  _s 
  "Reconnect To"
let mW_me_Quit = 
  _s 
  "Quit"
let mW_lb_networks = 
  _s 
  "Networks"
let mW_lb_servers = 
  _s 
  "Servers"
let mW_lb_downloads = 
  _s 
  "Downloads"
let mW_lb_friends = 
  _s 
  "Friends"
let mW_lb_search = 
  _s 
  "Search"
let mW_lb_rooms = 
  _s 
  "Rooms"
let mW_lb_uploads = 
  _s 
  "Uploads"
let mW_lb_console = 
  _s 
  "Console"
let mW_lb_graph = 
  _s 
  "Graph"
let mW_wt_software = 
  "MLDonkey"

(** Common Menus labels **)
let mAutosize = 
  _s 
  "Autosize"
let mSort = 
  _s 
  "Sort"
let mRemove_column = 
  _s 
  "Remove Column"
let mAdd_column_after = 
  _s 
  "Add Column After"
let mAdd_column_before = 
  _s 
  "Add Column Before"

(** Messages and string constants. *)

let chat_config_file = 
  Filename.concat CommonOptions.home_dir "chat.ini"

(** {2 Command line messages} *)

let usage = Sys.argv.(0)^" [options] <files>\n"
let options_are = "Options are :"

(** {2 Config options labels and messages} *)

(* options labels *)
let o_gui_port = 
  _s 
  "GUI port"
let o_hostname = 
  _s 
  "Hostname"
let o_password = 
  _s 
  "Password"
let o_gui_server = 
  _s 
  "GUI server"
let o_lang = 
  _s 
  "Language"
let o_login = 
  _s 
  "Login"

let o_gui = 
  _s 
  "GUI"
let o_options = 
  _s 
  "Options"
let o_plugins = 
  _s 
  "Plugins"

let o_colors = 
  _s 
  "Colors"
let o_fonts = 
  _s 
  "Fonts"
let o_layout = 
  _s 
  "Layout"
let o_columns = 
  _s 
  "Columns titles"
let o_client = 
  _s 
  "Client"
let o_misc = 
  _s 
  "Misc"
let o_graph = 
  _s 
  "Graph"

let o_col_default = 
  _s 
  "Default"
let o_col_downloaded = 
  _s 
  "Downloaded"
let o_col_downloading = 
  _s 
  "Downloading"
let o_col_avail = 
  _s 
  "Available"
let o_col_not_avail = 
  _s 
  "Not available"
let o_col_connected = 
  _s 
  "Connected"
let o_col_not_connected = 
  _s 
  "Not connected"
let o_col_connecting = 
  _s 
  "Connecting"
let o_col_files_listed = 
  _s 
  "Files listed"
let o_col_files_result = 
  _s 
  "Files result"
let o_col_tab_selected = 
  _s 
  "Tab selected"
let o_col_tab_not_selected = 
  _s 
  "Tab not selected"
let o_col_list_bg = 
  _s 
  "Lists background"
let o_col_network_enabled = 
  _s 
  "Network enabled"
let o_col_network_disabled = 
  _s 
  "Network diabled"

let o_font_list = 
  _s 
  "Lists & Trees"
let o_font_main_tab = 
  _s 
  "Main tabs labels"
let o_font_networks = 
  _s 
  "Networks labels"
let o_font_graphic = 
  _s 
  "Graphic texts"

let o_auto_resize = 
  _s 
  "Auto-resize"
let o_files_auto_expand_depth = 
  _s 
  "Files auto-expand depth"
let o_use_size_suffixes = 
  _s 
  "Use size suffixes (G, M, k)"
let o_use_availability_height = 
  _s 
  "Use height encoded availability"
let o_use_relative_availability = 
  _s 
  "Use relative % availability"
let o_toolbars_style = 
  _s 
  "Style of toolbars"
let o_tab_position = 
  _s 
  "Tab position"
let o_mini_toolbars = 
  _s 
  "Mini icons in toolbars"
let o_use_icons = 
  _s 
  "Use icons in the lists"
let o_use_graphical_availability = 
  _s 
  "Use graphical represention for availability"
let o_h_use_reliable_sources = 
  _s 
  "Display only reliable sources"
let o_max_file_name_len = 
  _s 
  "Maximum length of a file name"
let o_max_result_name_len = 
  _s 
  "Maximum length of a result name"
let o_max_client_name_len = 
  _s 
  "Maximum length of a client name"

let o_servers_columns = 
  _s 
  "Servers"
let o_downloads_columns = 
  _s 
  "Downloads"
let o_downloaded_columns = 
  _s 
  "Downloaded"
let o_friends_columns = 
  _s 
  "Friends"
let o_file_locations_columns = 
  _s 
  "File locations"
let o_results_columns = 
  _s 
  "Results"
let o_shared_files_up_colums = 
  _s 
  "Shared files upload info"

let o_max_download_rate = 
  _s 
  "Max download rate (ko/s)"
let o_max_upload_rate = 
  _s 
  "Max upload rate (ko/s)"
let o_download_time_range = 
  _s 
  "Downloads time range +(s, mn, h, d, w)"
let o_upload_time_range = 
  _s 
  "Uploads time range +(s, mn, h, d, w)"
let o_col_bg_download = 
  _s 
  "Downloads background"
let o_col_bg_upload = 
  _s 
  "Uploads background"
let o_col_grid_download = 
  _s 
  "Downloads grid"
let o_col_grid_upload = 
  _s 
  "Uploads grid"
let o_col_fg_download = 
  _s
  "Download rate"
let o_col_fg_upload = 
  _s 
  "Uploads rate"
let o_col_fg_download_av = 
  _s 
  "Average download rate"
let o_col_fg_upload_av = 
  _s 
  "Average uploads rate"

(* help  messages *)

let h_gui_port = 
  _s 
  "The server port to connect to"
let h_hostname = 
  _s 
  "The server hostname to connect to"
let h_gui_password = 
  _s 
  "The password to use when connecting to the server"
let h_login = 
  _s 
  "Your login name (default is admin)"
let h_history =
  _s 
  "History of connected cores"
let h_lang = 
  _s 
  "What is the language of your system"

let h_col_default = 
  _s 
  "Default color in lists"
let h_col_downloaded = 
  _s 
  "Color for downloaded files"
let h_col_downloading = 
  _s 
  "Color for files being downloaded"
let h_col_avail = 
  _s 
  "Color for available files, not downloading"
let h_col_not_avail = 
  _s 
  "Color for unavailable files"
let h_col_connected = 
  _s 
  "Color for connected servers or users"
let h_col_not_connected = 
  _s 
  "Color for not connected servers or users"
let h_col_connecting = 
  _s 
  "Color for servers or users with which a connection is being established"
let h_col_files_listed = 
  _s 
  "Color for users whose list of files has been retrieved"
let h_col_files_result = 
  _s 
  "Color for the result list in the queries tab"
let h_col_tab_selected = 
  _s 
  "Color for tab selected"
let h_col_tab_not_selected = 
  _s 
  "Color for tab not selected"
let h_col_network_enabled = 
  _s 
  "Color for the Network Name when enabled"
let h_col_network_disabled = 
  _s 
  "Color for the Network Name when disabled"
let h_col_list_bg = 
  _s 
  "Unified color background for lists"

let h_auto_resize = 
  _s 
  "Auto-resize lists columns"
let h_files_auto_expand_depth = 
  _s 
  "The depth to which the directories of a friend are automatically expanded"
let h_use_size_suffixes = 
  _s 
  "Whether sizes are printed using G(iga), M(ega) and k(ilo) suffixes."
let h_availability_max =  
  _s 
  "If use_availability_height is true, which availability corresponds to a full bar ?"
let h_use_availability_height = 
  _s 
  "Display the availability of a chunk as height or color coded bar"
let h_use_relative_availability =
   _s 
  "Calculate % availability ignoring already present chunks"
let h_toolbars_style = 
  _s 
  "What is displayed in toolbar buttons : text, icon or both"
let h_mini_toolbars = 
  _s 
  "What is displayed in toolbar buttons : small or big icons"

let h_compaction_overhead =
  _s 
  "The percentage of free memory before a compaction is triggered"
let h_interface_buffer =
  _s 
  "The size of the buffer to the core"
let h_copy_messages = 
  _s 
  "For bundle binaries, should we directly pass structures 
   between the core and the GUI (faster), or copy them (fewer bugs)" 

let h_servers_vpane_up = 
  _s 
  "Size in % of upper part of the servers hpane"
let h_friends_hpane_left = 
  _s 
  "Size in % of left part of the friends hpane"
let h_friends_vpane_up =  
  _s 
  "Size in % of up part of the friends vpane"
let h_friends_hpane_dirs = 
  _s 
  "Size in % of the directories part of the files box"
let h_rooms_hpane_left = 
  _s 
  "Size in % of left part of the rooms hpane"
let h_rooms_hpane2_left = 
  _s 
  "Size in % of left part of the second rooms hpane"
let h_rooms_vpane_up = 
  _s 
  "Size in % of upper part of the rooms vpane"
let h_uploads_vpane_up = 
  _s 
  "Size in % of up part of the uploads vpane"
let h_gui_width = 
  _s 
  "Width of GUI window"
let h_gui_height = 
  _s
  "Height of GUI window" 
let h_last_tab =
  _s
  "The last tab opened before closing the GUI"

let h_tab_position = 
  _s 
  "How are displayed the Tabs : left, right, top, bottom"
let h_use_icons = 
  _s 
  "Display various icons in the lists"
let h_use_graphical_availability = 
  _s 
  "What is displayed in availability column : graphical or text"
let h_use_reliable_sources = 
  _s 
  "Filter the sources that have only a file availability"
let h_max_file_name_len = 
  _s 
  "What is the maximum length of a file name displayed in the downloads list"
let h_max_result_name_len = 
  _s 
  "What is the maximum length of a file name displayed in the results list"
let h_max_client_name_len = 
  _s 
  "What is the maximum length of a client name displayed in the friends and uploads lists"

let h_servers_columns = 
  _s 
  "Columns for the servers"
let h_downloads_columns = 
  _s 
  "Columns for the files being downloaded"
let h_downloaded_columns = 
  _s 
  "Columns for the downloaded files"
let h_friends_columns = 
  _s 
  "Columns for the friends"
let h_file_locations_columns = 
  _s 
  "Columns for the locations of a file"
let h_users_columns =
  _s 
  "Columns of the users lists"
let h_rooms_columns =
  _s 
  "Columns of the room lists"
let h_results_columns = 
  _s 
  "Columns for the results of searches and files of a friends"
let h_shared_files_up_columns = 
  _s 
  "Columns for the list of shared files upload information"

let h_max_download_rate = 
  _s 
  "Max download bandwith capacity (ko/s)"
let h_max_upload_rate = 
  _s 
  "Max upload bandwith capacity (ko/s)"
let h_download_time_range = 
  _s 
  "Time range for the downloads graph"
let h_upload_time_range = 
  _s 
  "Time range for the uploads graph"
let h_col_bg_download = 
  _s 
  "Color for the downloads graph background"
let h_col_bg_upload = 
  _s 
  "Color for the uploads graph background"
let h_col_grid_download = 
  _s
  "Color for the downloads graph grid"
let h_col_grid_upload = 
  _s 
  "Color for the uploads graph grid"
let h_col_fg_download = 
  _s 
  "Color for the download rate"
let h_col_fg_upload = 
  _s 
  "Color for the upload rate"
let h_col_fg_download_av = 
  _s 
  "Color for the average download rate"
let h_col_fg_upload_av = 
  _s 
  "Color for the average upload rate"

let h_font_list = 
  _s 
  "Font for the list & trees texts"
let h_font_main_tab = 
  _s 
  "Font for the main notebook tabs labels"
let h_font_networks = 
  _s 
  "Font for the networks labels in the Networks Tab"
let h_font_graphic = 
  _s 
  "Font to display texts in the Graph tab"

(** Columns **)
let c_name = 
  _s 
  "Name"
let c_md4 = 
  _s 
  "MD4"
let c_size = 
  _s 
  "Size"
let c_downloaded = 
  _s 
  "Downloaded"
let c_percent = 
  _s 
  "%"
let c_state = 
  _s 
  "State"
let c_avail = 
  _s 
  "Availability"
let c_csour = 
  _s 
  "Completesources"
let c_rate = 
  _s 
  "Rate"
let c_format = 
  _s 
  "Format"
let c_network = 
  _s 
  "Net"
let c_status = 
  _s 
  "Status"
let c_eta = 
  _s 
  "ETA"
let c_priority = 
  _s 
  "Priority"
let c_kind = 
  _s 
  "Kind"
let c_client_type = 
  _s 
  "Type"
let c_client_rating = 
  _s 
  "Rating"
let c_client_software = 
  _s 
  "Brand"
let c_client_release = 
  _s 
  "Rel"
let c_client_emulemod = 
  _s 
  "Mod"
let c_client_downloaded = 
  _s 
  "Downloaded"
let c_client_uploaded = 
  _s 
  "Uploaded"
let c_client_upload = 
  _s 
  "File uploaded"
let c_client_sock_addr = 
  _s 
  "IP Address"
let c_client_connect_time = 
  _s 
  "Connected Time"
let c_address = 
  _s 
  "Address"
let c_server_nusers = 
  _s 
  "Users"
let c_server_nfiles = 
  _s 
  "Files"
let c_server_desc = 
  _s 
  "Description"
let c_duration = 
  _s 
  "Duration"
let c_codec = 
  _s 
  "Codec"
let c_bitrate  = 
  _s 
  "Bitrate"
let c_comment = 
  _s 
  "Comment"
let c_nusers = 
  _s 
  "Users"
let c_filename = 
  _s 
  "Filename"
let c_uploaded = 
  _s 
  "Uploaded"
let c_requests = 
  _s 
  "Requests"

(** {2 Actions names for key bindings} *)

let a_page_networks = "page_networks"
let a_page_servers = "page_servers"
let a_page_downloads = "page_downloads"
let a_page_friends = "page_friends"
let a_page_searches = "page_searches"
let a_page_rooms = "page_rooms"
let a_page_uploads = "page_uploads"
let a_page_console = "page_console"
let a_page_graph = "page_graph"
let a_next_page = "next_page"
let a_previous_page = "previous_page"
let a_reconnect = "reconnect"
let a_exit = "exit"

let a_select_all = "select_all"
let a_connect = "connect"
let a_connect_more = "connect_more"

let a_cancel_download = "cancel_download"
let a_save_all_files = "save_all_files"
let a_menu_save_file = "menu_save_file"

let a_download_selection = "download_selection"
let a_remove_friend = "remove_friend"


(** {2 Icons names} *)

let o_xpm_toggle_display_all_servers = "toggle_display_all_servers"
let o_xpm_add_server = "add_server"
let o_xpm_submit_search = "submit_search"
let o_xpm_extend_search = "extend_search"
let o_xpm_local_search = "local_search"
let o_xpm_subscribe_search = "subscribe_search"
let o_xpm_close_search = "close_search"
let o_xpm_stop_search = "stop_search"
let o_xpm_close_room = "close_room"
let o_xpm_add_shared_directory = "add_shared_directory"
let o_xpm_download_directory = "download_directory"
let o_xpm_view_pending_slots = "view_pending_slots"

let o_xpm_mini_toggle_display_all_servers = "toggle_display_all_servers_mini"
let o_xpm_mini_add_server = "add_server_mini"
let o_xpm_mini_submit_search = "submit_search_mini"
let o_xpm_mini_extend_search = "extend_search_mini"
let o_xpm_mini_local_search = "local_search_mini"
let o_xpm_mini_subscribe_search = "subscribe_search_mini"
let o_xpm_mini_close_search = "close_search_mini"
let o_xpm_mini_stop_search = "stop_search_mini"
let o_xpm_mini_close_room = "close_room_mini"
let o_xpm_mini_add_shared_directory = "add_shared_directory_mini"
let o_xpm_mini_download_directory = "download_directory_mini"
let o_xpm_mini_view_pending_slots = "view_pending_slots_mini"

let o_xpm_nbk_networks_on = "nbk_networks_on"
let o_xpm_nbk_networks_off = "nbk_networks_off"
let o_xpm_nbk_networks_menu = "nbk_networks_mini"
let o_xpm_nbk_servers_on = "nbk_servers_on"
let o_xpm_nbk_servers_off = "nbk_servers_off"
let o_xpm_nbk_servers_menu = "nbk_servers_mini"
let o_xpm_nbk_downloads_on = "nbk_downloads_on"
let o_xpm_nbk_downloads_off = "nbk_downloads_off"
let o_xpm_nbk_downloads_menu = "nbk_downloads_mini"
let o_xpm_nbk_friends_on = "nbk_friends_on"
let o_xpm_nbk_friends_off = "nbk_friends_off"
let o_xpm_nbk_friends_menu = "nbk_friends_mini"
let o_xpm_nbk_search_on = "nbk_search_on"
let o_xpm_nbk_search_off = "nbk_search_off"
let o_xpm_nbk_search_menu = "nbk_search_mini"
let o_xpm_nbk_rooms_on = "nbk_rooms_on"
let o_xpm_nbk_rooms_off = "nbk_rooms_off"
let o_xpm_nbk_rooms_menu = "nbk_rooms_mini"
let o_xpm_nbk_uploads_on = "nbk_uploads_on"
let o_xpm_nbk_uploads_off = "nbk_uploads_off"
let o_xpm_nbk_uploads_menu = "nbk_uploads_mini"
let o_xpm_nbk_console_on = "nbk_console_on"
let o_xpm_nbk_console_off = "nbk_console_off"
let o_xpm_nbk_console_menu = "nbk_console_mini"
let o_xpm_nbk_graphs_on = "nbk_graphs_on"
let o_xpm_nbk_graphs_off = "nbk_graphs_off"
let o_xpm_nbk_graphs_menu = "nbk_graphs_mini"

let o_xpm_about = "about"
let o_xpm_settings = "settings"
let o_xpm_exit = "exit"
let o_xpm_gui = "gui"
let o_xpm_kill_core = "kill_core"
let o_xpm_splash_screen = "splash_screen"

let o_xpm_album_search = "album_search"
let o_xpm_movie_search = "movie_search"
let o_xpm_mp3_search = "mp3_search"
let o_xpm_complex_search = "complex_search"
let o_xpm_sharereactor_search = "sharereactor_search"
let o_xpm_jigle_search = "jigle_search"
let o_xpm_freedb_search = "freedb_search"
let o_xpm_imdb_search = "imdb_search"

let o_xpm_bt = "bt"
let o_xpm_dc = "dc"
let o_xpm_ed2k = "ed2k"
let o_xpm_fasttrack = "fasttrack"
let o_xpm_gnutella = "gnutella"
let o_xpm_napster = "napster"
let o_xpm_slsk = "slsk"
let o_xpm_unknown = "unknown"

let o_xpm_downloading = "downloading"
let o_xpm_connect_y = "connect_y"
let o_xpm_connect_m = "connect_m"
let o_xpm_connect_n = "connect_n"
let o_xpm_removedhost = "removedhost"
let o_xpm_blacklistedhost = "blacklistedhost"
let o_xpm_files_listed = "files_listed"
let o_xpm_server_c_low = "server_c_low"
let o_xpm_server_c_high = "server_c_high"
let o_xpm_server_ci = "server_ci"
let o_xpm_server_nc = "server_nc"

let o_xpm_friend_user = "friend_user"
let o_xpm_contact_user = "contact_user"
let o_xpm_normal_user = "normal_user"

let o_xpm_priority_0 = "priority_0"
let o_xpm_priority_1 = "priority_1"
let o_xpm_priority_2 = "priority_2"

let o_xpm_mimetype_binary = "mimetype_binary"
let o_xpm_mimetype_cdimage = "mimetype_cdimage"
let o_xpm_mimetype_debian = "mimetype_debian"
let o_xpm_mimetype_html = "mimetype_html"
let o_xpm_mimetype_images = "mimetype_images"
let o_xpm_mimetype_java = "mimetype_java"
let o_xpm_mimetype_pdf = "mimetype_pdf"
let o_xpm_mimetype_postscript = "mimetype_postscript"
let o_xpm_mimetype_real = "mimetype_real"
let o_xpm_mimetype_recycled = "mimetype_recycled"
let o_xpm_mimetype_rpm = "mimetype_rpm"
let o_xpm_mimetype_shellscript = "mimetype_shellscript"
let o_xpm_mimetype_soffice = "mimetype_soffice"
let o_xpm_mimetype_sound = "mimetype_sound"
let o_xpm_mimetype_source = "mimetype_source"
let o_xpm_mimetype_spreadsheet = "mimetype_spreadsheet"
let o_xpm_mimetype_tex = "mimetype_tex"
let o_xpm_mimetype_text = "mimetype_text"
let o_xpm_mimetype_tgz = "mimetype_tgz"
let o_xpm_mimetype_video = "mimetype_video"
let o_xpm_mimetype_wordprocessing = "mimetype_wordprocessing"
let o_xpm_mimetype_unknown = "mimetype_unknown"

let o_xpm_tree_closed = "tree_closed"
let o_xpm_tree_opened = "tree_opened"

let o_xpm_bt_net_on = "bt_net_on"
let o_xpm_dc_net_on = "dc_net_on"
let o_xpm_ed2k_net_on = "ed2k_net_on"
let o_xpm_ftt_net_on = "ftt_net_on"
let o_xpm_gnut_net_on = "gnut_net_on"
let o_xpm_nap_net_on = "nap_net_on"
let o_xpm_slsk_net_on = "slsk_net_on"
let o_xpm_mld_tux_on = "mld_tux_on"
let o_xpm_bt_net_off = "bt_net_off"
let o_xpm_dc_net_off = "dc_net_off"
let o_xpm_ed2k_net_off = "ed2k_net_off"
let o_xpm_ftt_net_off = "ftt_net_off"
let o_xpm_gnut_net_off = "gnut_net_off"
let o_xpm_nap_net_off = "nap_net_off"
let o_xpm_slsk_net_off = "slsk_net_off"
let o_xpm_mld_tux_off = "mld_tux_off"
  

let _ =
  (
    try
      Options.load message_file
    with
      Sys_error _ ->
        (try Options.save message_file with _ -> ())
    | e ->
        lprintf "Error %s loading message file %s"
          (Printexc2.to_string e) 
        (Options.options_file_name message_file);
        lprint_newline ();
        lprintf "Using default messages."; lprint_newline ();
  )
  
(** {2 Help text, change log} *)
    
let help_string version = Printf.sprintf "

                      MLDonkey
                      ========

Release: %s
Authors: MLDonkey project, http://mldonkey.sf.net

This documentation file is now obsolete. Read the FAQ instead (either
on the project WEB site or in the FAQ.html file).

For help, see:

IRC channel: irc.freenode.net, chat #mldonkey

Web sites:
http://mldonkey.sf.net         Official site

Mailing-lists:
mldonkey-users@nongnu.org
Archives: http://mail.nongnu.org/mailman/listinfo/mldonkey-users

In the console, use '?' for help on commands.
" version


