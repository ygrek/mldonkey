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
      Filename.concat CommonOptions.config_dir "newgui_messages.ini"

      (*
let _ =
  lprintf "Using Message File %s" message_file_name; lprint_newline ()
  *)

let message_file = Options.create_options_file message_file_name
let message_section = file_section message_file [] ""
let message name t x = define_option message_section [name] "" t x

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
  message "nT_lb_display_bt" (T.option T.format) 
  "Display BitTorrent"
let nT_lb_net_bt = 
  message "nT_lb_net_bt" (T.option T.format) 
  "BITTORRENT"
let nT_lb_display_dc = 
  message "nT_lb_display_dc" (T.option T.format) 
  "Display Direct Connect"
let nT_lb_net_dc =
  message "nT_lb_net_dc" (T.option T.format) 
  "DIRECT CONNECT"
let nT_lb_display_ed2k = 
  message "nT_lb_display_ed2k" (T.option T.format) 
  "Display Edonkey / Overnet"
let nT_lb_net_ed2k = 
  message "nT_lb_net_ed2k" (T.option T.format) 
  "EDONKEY / OVERNET"
let nT_lb_display_nap = 
  message "nT_lb_display_nap" (T.option T.format) 
  "Display Open Napster"
let nT_lb_net_nap = 
  message "nT_lb_net_nap" (T.option T.format) 
  "OPEN NAPSTER"
let nT_lb_display_gnut = 
  message "nT_lb_display_gnut" (T.option T.format) 
  "Display Gnutella"
let nT_lb_net_gnut = 
  message "nT_lb_net_gnut" (T.option T.format) 
  "GNUTELLA"
let nT_lb_display_ftt = 
  message "nT_lb_display_ftt" (T.option T.format) 
  "Display Fasttrack"
let nT_lb_net_ftt = 
  message "nT_lb_net_ftt" (T.option T.format) 
  "FASTTRACK"
let nT_lb_display_slsk = 
  message "nT_lb_display_slsk" (T.option T.format) 
  "Display Soulseek"
let nT_lb_net_slsk = 
  message "nT_lb_net_slsk" (T.option T.format) 
  "SOULSEEK"

(** Servers Tab **)
let sT_wt_add_server = 
  message "sT_wt_add_server" (T.option T.format) 
  "Add server by Address"
let sT_ti_add_server = 
  message "sT_ti_add_server" (T.option T.format) 
  "Add a server"
let sT_lb_add_server = 
  message "sT_lb_add_server" (T.option T.format) 
  "Address ( ip : port ) : "
let sT_lb_network = 
  message "sT_lb_network" (T.option T.format) 
  "Network : "
let sT_ti_display_all_servers = 
  message "sT_ti_display_all_servers" (T.option T.format) 
  "View all servers (even not connected)"
let sT_lb_display_all_servers = 
  message "sT_lb_display_all_servers" (T.option T.format) 
  "View all"
let sT_lb_users = 
  message "sT_lb_users" (T.option T.format) 
  "Users"
let sT_lb_servers = 
  message "sT_lb_servers" (T.option T.format) 
  "Servers"
let sT_me_remove = 
  message "sT_me_remove" (T.option T.format) 
  "Remove"
let sT_me_disconnect = 
  message "sT_me_disconnect" (T.option T.format) 
  "Disconnect"
let sT_me_view_users = 
  message "sT_me_view_users" (T.option T.format) 
  "View users"
let sT_me_connect = 
  message "sT_me_connect" (T.option T.format) 
  "Connect"
let sT_me_connect_more_servers = 
  message "sT_me_connect_more_servers" (T.option T.format) 
  "More servers"
let sT_me_remove_old_servers = 
  message "sT_me_remove_old_servers" (T.option T.format) 
  "Clear list"

(** Downloads Tab **)
let dT_wt_save_as = 
  message "dT_wt_save_as" (T.option T.format) 
  "Save file as" 
let dT_lb_save_as = 
  message "dT_lb_save_as" (T.option T.format) 
  "Save file as : "
let dT_tx_downloading = 
  message "dT_tx_downloading" (T.option T.format) 
  "Downloading"
let dT_tx_waiting =
  message "dT_tx_waiting" (T.option T.format) 
  " Waiting..."
let dT_tx_cancelled = 
  message "dT_tx_cancelled" (T.option T.format) 
  "Cancelled"
let dT_tx_queued = 
  message "dT_tx_queued" (T.option T.format) 
  "Queued"
let dT_tx_paused = 
  message "dT_tx_paused" (T.option T.format) 
  "Paused"
let dT_tx_complete = 
  message "dT_tx_complete" (T.option T.format) 
  "Complete"
let dT_tx_dl_done = 
  message "dT_tx_dl_done" (T.option T.format) 
  "Done"
let dT_tx_dl_aborted =
  message "dT_tx_dl_aborted" (T.option (T.string T.format)) 
  "Aborted : %s"
let dT_tx_connected = 
  message "dT_tx_connected" (T.option T.format) 
  "Connected"
let dT_tx_connecting = 
  message "dT_tx_connecting" (T.option T.format) 
  "Connecting"
let dT_tx_new_host = 
  message "dT_tx_new_host" (T.option T.format) 
  "New Host"
let dT_tx_initiating = 
  message "dT_tx_initiating" (T.option T.format) 
  "Initiating"
let dT_tx_ranked = 
  message "dT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d"
let dT_tx_queued_out = 
  message "dT_tx_queued_out" (T.option T.format) 
  "Queued Out"
let dT_tx_ranked_out = 
  message "dT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d out"
let dT_tx_failed = 
  message "dT_tx_failed" (T.option (T.int T.format)) 
  "Failed %d"
let dT_tx_removed = 
  message "dT_tx_removed" (T.option T.format) 
  "Removed"
let dT_tx_black_listed = 
  message "dT_tx_black_listed" (T.option T.format) 
  "Black Listed"
let dT_tx_unknown = 
  message "dT_tx_unknown" (T.option T.format) 
  "Unknown"
let dT_tx_priority_high = 
  message "dT_tx_priority_high" (T.option T.format) 
  "High"
let dT_tx_priority_normal = 
  message "dT_tx_priority_normal" (T.option T.format) 
  "Normal"
let dT_tx_priority_low = 
  message "dT_tx_priority_low" (T.option T.format) 
  "Low"
let dT_wt_cancel = 
  message "dT_wt_cancel" (T.option T.format) 
  "Cancel File(s)"
let dT_lb_ask_cancel_download_files = 
  message "dT_lb_ask_cancel_download_files" (T.option T.format) 
  "Cancel the following file(s) ?\n"
let dT_wt_edit_mp3 = 
  message "dT_wt_edit_mp3" (T.option T.format) 
  "MP3 tags Editor"
let dT_me_view_sources = 
  message "dT_me_view_sources" (T.option T.format) 
  "Show/Hide Sources"
let dT_me_preview = 
  message "dT_me_preview" (T.option T.format) 
  "Preview"
let dT_me_pause_resume_dl = 
  message "dT_me_pause_resume_dl" (T.option T.format) 
  "Pause/Resume"
let dT_me_retry_connect = 
  message "dT_me_retry_connect" (T.option T.format) 
  "Retry connect"
let dT_me_cancel = 
  message "dT_me_cancel" (T.option T.format) 
  "Cancel"
let dT_me_verify_chunks = 
  message "dT_me_verify_chunks" (T.option T.format) 
  "Verify chunks"
let dT_me_set_priority = 
  message "dT_me_set_priority" (T.option T.format) 
  "Set priority"
let dT_me_set_priority_high = 
  message "dT_me_set_priority_high" (T.option T.format) 
  "High"
let dT_me_set_priority_normal = 
  message "dT_me_set_priority_normal" (T.option T.format) 
  "Normal"
let dT_me_set_priority_low = 
  message "dT_me_set_priority_low" (T.option T.format) 
  "Low"
let dT_me_get_format = 
  message "dT_me_get_format" (T.option T.format) 
  "Get format info"
let dT_me_edit_mp3 = 
  message "dT_me_edit_mp3" (T.option T.format) 
  "Edit mp3 tags"
let dT_me_save_all = 
  message "dT_me_save_all" (T.option T.format) 
  "Save all"
let dT_me_save_as = 
  message "dT_me_save_as" (T.option T.format) 
  "Save file as"
let dT_me_save = 
  message "dT_me_save" (T.option T.format) 
  "Save"
let dT_me_add_to_friends = 
  message "dT_me_add_to_friends" (T.option T.format) 
  "Add to friends"
let dT_lb_ed2k = 
  message "dT_lb_ed2k" (T.option T.format) 
  "ed2k link : "

(** Friends Tab **)
let fT_lb_messages = 
  message "fT_lb_messages" (T.option T.format) 
  "Messages"
let fT_tx_downloading = 
  message "fT_tx_downloading" (T.option T.format) 
  "Downloading"
let fT_tx_queued = 
  message "fT_tx_queued" (T.option T.format) 
  "Queued"
let fT_tx_connected = 
  message "fT_tx_connected" (T.option T.format) 
  "Connected"
let fT_tx_connecting = 
  message "fT_tx_connecting" (T.option T.format) 
  "Connecting"
let fT_tx_new_host = 
  message "dT_tx_new_host" (T.option T.format) 
  "New Host"
let fT_tx_initiating = 
  message "fT_tx_initiating" (T.option T.format) 
  "Initiating"
let fT_tx_ranked = 
  message "fT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d"
let fT_tx_queued_out = 
  message "fT_tx_queued_out" (T.option T.format) 
  "Queued Out"
let fT_tx_ranked_out = 
  message "fT_tx_ranked" (T.option (T.int T.format)) 
  "Ranked %d out"
let fT_tx_failed = 
  message "fT_tx_failed" (T.option (T.int T.format)) 
  "Failed %d"
let fT_tx_removed = 
  message "fT_tx_removed" (T.option T.format) 
  "Removed"
let fT_tx_black_listed = 
  message "fT_tx_black_listed" (T.option T.format) 
  "Black Listed"
let fT_tx_files_listed = 
  message "fT_tx_files_listed" (T.option T.format) 
  "Files Listed"
let fT_tx_friend = 
  message "fT_tx_friend" (T.option T.format) 
  "Friend"
let fT_tx_contact = 
  message "fT_tx_contact" (T.option T.format) 
  "Contact"
let fT_tx_normal = 
  message "fT_tx_normal" (T.option T.format) 
  "Normal"
let fT_tx_direct = 
  message "fT_tx_direct" (T.option T.format) 
  "Direct"
let fT_wt_find_friend = 
  message "fT_wt_find_friend" (T.option T.format) 
  "Find friend"
let fT_lb_name = 
  message "fT_lb_name" (T.option T.format) 
  "Name"
let fT_me_find_friend = 
  message "fT_me_find_friend" (T.option T.format) 
  "Find friend"
let fT_me_remove = 
  message "fT_me_remove" (T.option T.format) 
  "Remove friend"
let fT_me_remove_all_friends = 
  message "fT_me_remove_all_friends" (T.option T.format) 
  "Clear list"

(** Query Box **)
let qT_lb_album_searches = 
  message "qT_lb_album_searches" (T.option T.format) 
  "Album"
let qT_lb_movie_searches = 
  message "qT_lb_movie_searches" (T.option T.format) 
  "Movie"
let qT_lb_mp3_searches = 
  message "qT_lb_mp3_searches" (T.option T.format) 
  "MP3"
let qT_lb_complex_searches = 
  message "qT_lb_complex_searches" (T.option T.format) 
  "Complex"
let qT_lb_sharereactor_searches = 
  message "qT_lb_sharereactor_searches" (T.option T.format) 
  "ShareReactor"
let qT_lb_jigle_searches = 
  message "qT_lb_jigle_searches" (T.option T.format) 
  "Jigle"
let qT_lb_freedb_searches = 
  message "qT_lb_freedb_searches" (T.option T.format) 
  "FreeDB"
let qT_lb_imdb_searches = 
  message "qT_lb_imdb_searches" (T.option T.format) 
  "IMDB"
let qT_lb_local_search = 
  message "qT_lb_local_search" (T.option T.format) 
  "Local Search"
let qT_lb_submit = 
  message "qT_lb_submit" (T.option T.format) 
  "Submit"
let qT_lb_subscribe = 
  message "qT_lb_subscribe" (T.option T.format) 
  "Subscribe"
let qT_ti_local_search = 
  message "qT_ti_local_search" (T.option T.format) 
  "Local Search"
let qT_ti_submit = 
  message "qT_ti_submit" (T.option T.format) 
  "Submit"
let qT_ti_subscribe = 
  message "qT_ti_subscribe" (T.option T.format) 
  "Subscribe"
let qT_lb_close_search = 
  message "qT_lb_close_search" (T.option T.format) 
  "Close Search"
let qT_ti_close_search = 
  message "qT_ti_close_search" (T.option T.format) 
  "Close Search"
let qT_lb_stop_search = 
  message "qT_lb_stop_search" (T.option T.format) 
  "Stop Search"
let qT_ti_stop_search = 
  message "qT_ti_stop_search" (T.option T.format) 
  "Stop Search"

let qT_tx_all_networks = 
  message "qT_tx_all_networks" (T.option T.format) 
  "All Networks"
let qT_lb_show_hidden_fields = 
  message "qT_lb_show_hidden_fields" (T.option T.format) 
  "Show hidden fields"
let qT_lb_network = 
  message "qT_lb_network" (T.option T.format) 
  "Network :"
let qT_lb_max_hits = 
  message "max_hits" (T.option T.format) 
  "Max Hits"

let qT_lb_and_not = 
  message "qT_lb_and_not" (T.option T.format) 
  "And Not"
let qT_tx_audio = 
  message "qT_tx_audio" (T.option T.format) 
  "Audio"
let qT_tx_video = 
  message "qT_tx_video" (T.option T.format) 
  "Video"
let qT_tx_program = 
  message "qT_tx_program" (T.option T.format) 
  "Program"
let qT_tx_image = 
  message "qT_tx_image" (T.option T.format) 
  "Image"
let qT_tx_documentation = 
  message "qT_tx_documentation" (T.option T.format) 
  "Documentation"
let qT_tx_collection = 
  message "qT_tx_collection" (T.option T.format) 
  "Collection"
let qT_lb_keywords = 
  message "qT_lb_keywords" (T.option T.format) 
  "Keywords"
let qT_lb_media = 
  message "qT_lb_media" (T.option T.format) 
  "Media"
let qT_lb_format = 
  message "qT_lb_format" (T.option T.format) 
  "Format"
let qT_lb_min_size = 
  message "min_size" (T.option T.format) 
  "Min size"
let qT_lb_max_size = 
  message "max_size" (T.option T.format) 
  "Max size"
let qT_lb_min_bitrate = 
  message "qT_lb_min_bitrate" (T.option T.format) 
  "Min Bitrate"
let qT_lb_title = 
  message "qT_lb_title" (T.option T.format) 
  "Title"
let qT_lb_number_of_results = 
  message "qT_lb_number_of_results" (T.option T.format) 
  "Number of results"
let qT_lb_sort_by = 
  message "qT_lb_sort_by" (T.option T.format) 
  "Sort by"
let qT_lb_album = 
  message "qT_lb_album" (T.option T.format) 
  "Album"
let qT_lb_fields = 
  message "qT_lb_fields" (T.option T.format) 
  "Fields"
let qT_lb_artist = 
  message "qT_lb_artist" (T.option T.format) 
  "Artist"
let qT_lb_track_title = 
  message "qT_lb_track_title" (T.option T.format) 
  "Track/Title"
let qT_lb_track = 
  message "qT_lb_track" (T.option T.format) 
  "Track"
let qT_lb_rest = 
  message "qT_lb_rest" (T.option T.format) 
  "Rest"
let qT_lb_categories = 
  message "qT_lb_categories" (T.option T.format) 
  "Categories"
let qT_lb_all = 
  message "qT_lb_all" (T.option T.format) 
  "All"
let qT_lb_blues = 
  message "qT_lb_blues" (T.option T.format) 
  "Blues"
let qT_lb_classical = 
  message "qT_lb_classical" (T.option T.format) 
  "Classical"
let qT_lb_data = 
  message "qT_lb_data" (T.option T.format) 
  "Data"
let qT_lb_folk = 
  message "qT_lb_folk" (T.option T.format) 
  "Folk"
let qT_lb_rock = 
  message "qT_lb_rock" (T.option T.format) 
  "Rock"
let qT_lb_soundtrack = 
  message "qT_lb_soundtrack" (T.option T.format) 
  "Soundtrack"
let qT_lb_availability = 
  message "qT_lb_availability" (T.option T.format) 
  "Availability"
let qT_lb_size = 
  message "qT_lb_size" (T.option T.format) 
  "Size"
let qT_lb_dvd_rips = 
  message "qT_lb_dvd_rips" (T.option T.format) 
  "DVD Rips"
let qT_lb_screeners = 
  message "qT_lb_screeners" (T.option T.format) 
  "Screeners"
let qT_lb_pc_games = 
  message "qT_lb_pc_games" (T.option T.format) 
  "PC Games"
let qT_lb_software = 
  message "qT_lb_software" (T.option T.format) 
  "Software"
let qT_lb_anime = 
  message "qT_lb_anime" (T.option T.format) 
  "Anime"
let qT_lb_series = 
  message "qT_lb_series" (T.option T.format) 
  "Series"
let qT_lb_funstuff =  
  message "qT_lb_funstuff" (T.option T.format) 
  "Funstuff"
let qT_lb_adult = 
  message "qT_lb_adult" (T.option T.format) 
  "Adult"
let qT_lb_consoles = 
  message "qT_lb_consoles" (T.option T.format) 
  "Consoles"
let qT_lb_books = 
  message "qT_lb_books" (T.option T.format) 
  "Books"
let qT_lb_xbox = 
  message "qT_lb_xbox" (T.option T.format) 
  "XBOX"
let qT_lb_hentai = 
  message "qT_lb_hentai" (T.option T.format) 
  "Hentai"
let qT_lb_ps2 = 
  message "qT_lb_ps2" (T.option T.format) 
  "PS2"
let qT_lb_gay = 
  message "qT_lb_gay" (T.option T.format) 
  "Gay"

let qT_tx_unknown = 
  message "qT_tx_unknown" (T.option T.format) 
  "Unknown"
let qT_tx_download = 
  message "download" (T.option T.format) 
  "Download"
let qT_tx_force_download = 
  message "force_download" (T.option T.format) 
  "Force Download"
let qT_lb_extended_search = 
  message "qT_lb_extended_search" (T.option T.format) 
  "Extended Search"
let qT_ti_extended_search = 
  message "qT_ti_extended_search" (T.option T.format) 
  "Extended Search"
let qT_lb_results = 
  message "qT_lb_results" (T.option (T.int T.format)) 
  "%d Results"
let qT_lb_waiting_for_replies = 
  message "qT_lb_waiting_for_replies" (T.option (T.int T.format)) 
  "Waiting for %d replies"
let qT_wt_download_selected_dir = 
  message "qT_wt_download_selected_dir" (T.option T.format) 
  "Download selected directory"
let qT_lb_download_selected_dir = 
  message "qT_lb_download_selected_dir" 
  (T.option T.format) "Download"
let qT_ti_download_selected_dir = 
  message "qT_ti_download_selected_dir" (T.option T.format) 
  "Download the selected directory"
let qT_lb_confirm_download_dir = 
  message "qT_lb_confirm_download_dir" (T.option (T.int (T.string T.format)))
  "Do you want to download the %d files in the directory %s ?"

(** Rooms Tab **)
let rT_lb_close_room = 
  message "rT_lb_close_room" (T.option T.format) 
  "Close room"
let rT_ti_close_room = 
  message "rT_ti_close_room" (T.option T.format) 
  "Close the current room"
let rT_lb_available_rooms = 
  message "rT_lb_available_rooms" (T.option T.format) 
  "Available Rooms"
let rT_lb_opened_rooms = 
  message "rT_lb_opened_rooms" (T.option T.format) 
  "Opened Rooms"
let rT_lb_rooms_messages = 
  message "rT_lb_rooms_messages" (T.option T.format) 
  "Messages"
let rT_lb_users = 
  message "rT_lb_users" (T.option T.format) 
  "Users"
let rT_me_browse_files = 
  message "rT_me_browse_files" (T.option T.format) 
  "Browse files"
let rT_me_add_to_friends = 
  message "rT_me_add_to_friends" (T.option T.format) 
  "Add to friends"
let rT_tx_direct = 
  message "rT_tx_direct" (T.option T.format) 
  "Direct"

(** Uploads **)
let uT_lb_shared_files  = 
  message "uT_lb_shared_files" (T.option T.format) 
  "Shared files"
let uT_lb_add_shared_directory = 
  message "uT_lb_add_shared_directory" (T.option T.format) 
  "Add Shared Directory"
let uT_lb_priority = 
  message "uT_lb_priority" (T.option T.format) 
  "Priority :"
let uT_ti_add_shared_directory = 
  message "uT_ti_add_shared_directory" (T.option T.format) 
  "Add a new shared directory"
let uT_wt_add_new_directory = 
  message "uT_wt_add_new_directory" (T.option T.format) 
  "Add a Shared Directory"
let uT_lb_uploaders  = 
  message "uT_lb_uploaders" (T.option T.format) 
  "Uploaders"
let uT_lb_directory = 
  message "uT_lb_directory" (T.option T.format) 
  "Directory : "
let uT_lb_add_new_directory = 
  message "uT_lb_add_new_directory" (T.option T.format) 
  "Add New Directory"
let uT_me_add_to_friends = 
  message "dT_me_add_to_friends" (T.option T.format) 
  "Add to friends"
let uT_me_copy_ed2k = 
  message "uT_me_copy_ed2k" (T.option T.format) 
  "Copy ed2k link to console/clipboard"

(** Console **)
let cT_lb_clear_console = 
  message "cT_lb_clear_console" (T.option T.format) 
  "Clear Console"
let cT_lb_command = 
  message "cT_lb_command" (T.option T.format) 
  "Command"

(** Graph **)
let gT_lb_downloads = 
  message "gT_lb_downloads" (T.option T.format) 
  "Downloads"
let gT_lb_uploads = 
  message "gT_lb_uploads" (T.option T.format) 
  "Uploads"

(** Im **)
let iM_wt_software = "MLDonkey"
let iM_me_file = 
  message "iM_me_file" (T.option T.format) 
  "File"
let iM_me_settings = 
  message "iM_me_settings" (T.option T.format) 
  "Settings"
let iM_me_quit = 
  message "iM_me_quit" (T.option T.format) 
  "Quit"

(** Popup Windows **)
let pW_lb_servers_remove_icons = 
  message "pW_lb_servers_remove_icons" (T.option T.format) 
  "Please wait ... Removing Servers icons"
let pW_lb_servers_add_icons = 
  message "pW_lb_servers_add_icons" (T.option T.format) 
  "Please wait ... Generating Servers icons"
let pW_lb_downloads_remove_icons = 
  message "pW_lb_downloads_remove_icons" (T.option T.format) 
  "Please wait ... Removing Downloads icons"
let pW_lb_downloads_add_icons = 
  message "pW_lb_downloads_add_icons" (T.option T.format) 
  "Please wait ... Generating Downloads icons"
let pW_lb_friends_remove_icons = 
  message "pW_lb_friends_remove_icons" (T.option T.format) 
  "Please wait ... Removing Friends icons"
let pW_lb_friends_add_icons = 
  message "pW_lb_friends_add_icons" (T.option T.format) 
  "Please wait ... Generating Friends icons"
let pW_lb_results_remove_icons = 
  message "pW_lb_results_remove_icons" (T.option T.format) 
  "Please wait ... Removing Searches icons"
let pW_lb_results_add_icons = 
  message "pW_lb_results_add_icons" (T.option T.format) 
  "Please wait ... Generating Searches icons"
let pW_lb_rooms_remove_icons = 
  message "pW_lb_rooms_remove_icons" (T.option T.format) 
  "Please wait ... Removing Rooms icons"
let pW_lb_rooms_add_icons = 
  message "pW_lb_rooms_add_icons" (T.option T.format) 
  "Please wait ... Generating Rooms icons"
let pW_lb_uploads_remove_icons = 
  message "pW_lb_uploads_remove_icons" (T.option T.format) 
  "Please wait ... Removing Sources icons"
let pW_lb_uploads_add_icons = 
  message "pW_lb_uploads_add_icons" (T.option T.format) 
  "Please wait ... Generating Sources icons"
let pW_lb_ok = 
  message "pW_lb_ok" (T.option T.format) 
  "Ok"
let pW_lb_cancel = 
  message "pW_lb_cancel" (T.option T.format) 
  "Cancel"
let pW_wt_bad_password = 
  message "pW_wt_bad_password" (T.option T.format) 
  "Bad Password"
let pW_lb_bad_password = 
  message "pW_lb_bad_password" (T.option T.format) 
  "Authorization Failed\nPlease, click the Settings button -> GUI -> GUI server and enter a valid password" 

(** Main Window **)
let mW_tx_action_unknown = 
  message "mW_tx_action_unknown" (T.option T.format) 
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
  message "mW_sb_connecting" (T.option T.format) 
  "Connecting"
let mW_lb_connected = 
  message "mW_sb_connected" (T.option T.format) 
  "Connected"
let mW_lb_not_connected = 
  message "mW_sb_not_connected" (T.option T.format) 
  "Not Connected"
let mW_ti_about = 
  message "mW_ti_about" (T.option T.format) 
  "About"
let mW_ti_im = 
  message "mW_ti_im" (T.option T.format) 
  "Im"
let mW_ti_settings = 
  message "mW_ti_settings" (T.option T.format) 
  "Settings"
let mW_ti_exit = 
  message "mW_ti_exit" (T.option T.format) 
  "Exit"
let mW_ti_gui = 
  message "mW_ti_gui" (T.option T.format) 
  "Gui"
let mW_ti_kill_core = 
  message "mW_ti_kill_core" (T.option T.format) 
  "Kill core"
let mW_me_reconnect = 
  message "mW_me_reconnect" (T.option T.format) 
  "Reconnect"
let mW_me_disconnect = 
  message "mW_me_disconnect" (T.option T.format) 
  "Disconnect"
let mW_me_scan_ports = 
  message "mW_me_scan_ports" (T.option T.format) 
  "Scan Ports"
let mW_me_reconnect_to = 
  message "mW_me_reconnect_to" (T.option T.format) 
  "Reconnect To"
let mW_me_Quit = 
  message "mW_me_Quit" (T.option T.format) 
  "Quit"
let mW_lb_networks = 
  message "mW_lb_networks" (T.option T.format) 
  "Networks"
let mW_lb_servers = 
  message "mW_lb_servers" (T.option T.format) 
  "Servers"
let mW_lb_downloads = 
  message "mW_lb_downloads" (T.option T.format) 
  "Downloads"
let mW_lb_friends = 
  message "mW_lb_friends" (T.option T.format) 
  "Friends"
let mW_lb_search = 
  message "mW_lb_search" (T.option T.format) 
  "Search"
let mW_lb_rooms = 
  message "mW_lb_rooms" (T.option T.format) 
  "Rooms"
let mW_lb_uploads = 
  message "mW_lb_uploads" (T.option T.format) 
  "Uploads"
let mW_lb_console = 
  message "mW_lb_console" (T.option T.format) 
  "Console"
let mW_lb_graph = 
  message "mW_lb_graph" (T.option T.format) 
  "Graph"
let mW_wt_software = 
  "MLDonkey"

(** Common Menus labels **)
let mAutosize = 
  message "mAutosize" (T.option T.format) 
  "Autosize"
let mSort = 
  message "mSort" (T.option T.format) 
  "Sort"
let mRemove_column = 
  message "mRemove_column" (T.option T.format) 
  "Remove Column"
let mAdd_column_after = 
  message "mAdd_column_after" (T.option T.format) 
  "Add Column After"
let mAdd_column_before = 
  message "mAdd_column_before" (T.option T.format) 
  "Add Column Before"

(** Messages and string constants. *)

let chat_config_file = 
  Filename.concat CommonOptions.config_dir "chat.ini"

(** {2 Command line messages} *)

let usage = Sys.argv.(0)^" [options] <files>\n"
let options_are = "Options are :"

(** {2 Config options labels and messages} *)

(* options labels *)
let o_gui_port = 
  message "o_gui_port" (T.option T.format) 
  "GUI port"
let o_hostname = 
  message "o_hostname" (T.option T.format) 
  "Hostname"
let o_password = 
  message "o_password" (T.option T.format) 
  "Password"
let o_gui_server = 
  message "o_gui_server" (T.option T.format) 
  "GUI server"
let o_lang = 
  message "o_lang" (T.option T.format) 
  "Language"
let o_login = 
  message "o_login" (T.option T.format) 
  "Login"

let o_gui = 
  message "o_gui" (T.option T.format) 
  "GUI"
let o_options = 
  message "o_options" (T.option T.format) 
  "Options"
let o_plugins = 
  message "o_plugins" (T.option T.format) 
  "Plugins"

let o_colors = 
  message "o_colors" (T.option T.format) 
  "Colors"
let o_fonts = 
  message "o_fonts" (T.option T.format) 
  "Fonts"
let o_layout = 
  message "o_layout" (T.option T.format) 
  "Layout"
let o_columns = 
  message "o_columns" (T.option T.format) 
  "Columns titles"
let o_client = 
  message "o_client" (T.option T.format) 
  "Client"
let o_misc = 
  message "o_misc" (T.option T.format) 
  "Misc"
let o_graph = 
  message "o_graph" (T.option T.format) 
  "Graph"

let o_col_default = 
  message "o_col_default" (T.option T.format) 
  "Default"
let o_col_downloaded = 
  message "o_col_downloaded" (T.option T.format) 
  "Downloaded"
let o_col_downloading = 
  message "o_col_downloading" (T.option T.format) 
  "Downloading"
let o_col_avail = 
  message "o_col_avail" (T.option T.format) 
  "Available"
let o_col_not_avail = 
  message "o_col_not_avail" (T.option T.format) 
  "Not available"
let o_col_connected = 
  message "o_col_connected" (T.option T.format) 
  "Connected"
let o_col_not_connected = 
  message "o_col_not_connected" (T.option T.format) 
  "Not connected"
let o_col_connecting = 
  message "o_col_connecting" (T.option T.format) 
  "Connecting"
let o_col_files_listed = 
  message "o_col_files_listed" (T.option T.format) 
  "Files listed"
let o_col_files_result = 
  message "o_col_files_result" (T.option T.format) 
  "Files result"
let o_col_tab_selected = 
  message "o_col_tab_selected" (T.option T.format) 
  "Tab selected"
let o_col_tab_not_selected = 
  message "o_col_tab_not_selected" (T.option T.format) 
  "Tab not selected"
let o_col_list_bg = 
  message "o_col_list_bg" (T.option T.format) 
  "Lists background"
let o_col_network_enabled = 
  message "o_col_network_enabled" (T.option T.format) 
  "Network enabled"
let o_col_network_disabled = 
  message "o_col_network_disabled" (T.option T.format) 
  "Network diabled"

let o_font_list = 
  message "o_font_list" (T.option T.format) 
  "Lists & Trees"
let o_font_main_tab = 
  message "o_font_main_tab" (T.option T.format) 
  "Main tabs labels"
let o_font_networks = 
  message "o_font_networks" (T.option T.format) 
  "Networks labels"
let o_font_graphic = 
  message "o_font_graphic" (T.option T.format) 
  "Graphic texts"

let o_auto_resize = 
  message "o_auto_resize" (T.option T.format) 
  "Auto-resize"
let o_files_auto_expand_depth = 
  message "o_files_auto_expand_depth" (T.option T.format) 
  "Files auto-expand depth"
let o_use_size_suffixes = 
  message "o_use_size_suffixes" (T.option T.format) 
  "Use size suffixes (G, M, k)"
let o_use_availability_height = 
  message "o_use_availability_height" (T.option T.format) 
  "Use height encoded availability"
let o_use_relative_availability = 
  message "o_use_relative_availability" (T.option T.format) 
  "Use relative %% availability"
let o_toolbars_style = 
  message "o_toolbars_style" (T.option T.format) 
  "Style of toolbars"
let o_tab_position = 
  message "o_tab_position" (T.option T.format) 
  "Tab position"
let o_mini_toolbars = 
  message "o_mini_toolbars" (T.option T.format) 
  "Mini icons in toolbars"
let o_use_icons = 
  message "o_use_icons" (T.option T.format) 
  "Use icons in the lists"
let o_use_graphical_availability = 
  message "o_use_graphical_availability" (T.option T.format) 
  "Use graphical represention for availability"
let o_h_use_reliable_sources = 
  message "o_h_use_reliable_sources" (T.option T.format) 
  "Display only reliable sources"
let o_max_file_name_len = 
  message "o_max_file_name_len" (T.option T.format) 
  "Maximum length of a file name"
let o_max_result_name_len = 
  message "o_max_result_name_len" (T.option T.format) 
  "Maximum length of a result name"
let o_max_client_name_len = 
  message "o_max_client_name_len" (T.option T.format) 
  "Maximum length of a client name"

let o_servers_columns = 
  message "o_servers_columns" (T.option T.format) 
  "Servers"
let o_downloads_columns = 
  message "o_downloads_columns" (T.option T.format) 
  "Downloads"
let o_downloaded_columns = 
  message "o_downloaded_columns" (T.option T.format) 
  "Downloaded"
let o_friends_columns = 
  message "o_friends_columns" (T.option T.format) 
  "Friends"
let o_file_locations_columns = 
  message "o_file_locations_columns" (T.option T.format) 
  "File locations"
let o_results_columns = 
  message "o_results_columns" (T.option T.format) 
  "Results"
let o_shared_files_up_colums = 
  message "o_shared_files_up_colums" (T.option T.format) 
  "Shared files upload info"

let o_max_download_rate = 
  message "o_max_download_rate" (T.option T.format) 
  "Max download rate (ko/s)"
let o_max_upload_rate = 
  message "o_max_upload_rate" (T.option T.format) 
  "Max upload rate (ko/s)"
let o_download_time_range = 
  message "o_download_time_range" (T.option T.format) 
  "Downloads time range +(s, mn, h, d, w)"
let o_upload_time_range = 
  message "o_upload_time_range" (T.option T.format) 
  "Uploads time range +(s, mn, h, d, w)"
let o_col_bg_download = 
  message "o_col_bg_download" (T.option T.format) 
  "Downloads background"
let o_col_bg_upload = 
  message "o_col_bg_upload" (T.option T.format) 
  "Uploads background"
let o_col_grid_download = 
  message "o_col_grid_download" (T.option T.format) 
  "Downloads grid"
let o_col_grid_upload = 
  message "o_col_grid_upload" (T.option T.format) 
  "Uploads grid"
let o_col_fg_download = 
  message "o_col_fg_download"(T.option T.format) 
  "Download rate"
let o_col_fg_upload = 
  message "o_col_fg_upload" (T.option T.format) 
  "Uploads rate"
let o_col_fg_download_av = 
  message "o_col_fg_download_av" (T.option T.format) 
  "Average download rate"
let o_col_fg_upload_av = 
  message "o_col_fg_upload_av" (T.option T.format) 
  "Average uploads rate"

(* help  messages *)

let h_gui_port = 
  message "h_gui_port" (T.option T.format) 
  "The server port to connect to"
let h_hostname = 
  message "h_hostname" (T.option T.format) 
  "The server hostname to connect to"
let h_gui_password = 
  message "h_gui_password" (T.option T.format) 
  "The password to use when connecting to the server"
let h_login = 
  message "h_login" (T.option T.format) 
  "Your login name (default is admin)"
let h_history =
  message "h_history" (T.option T.format) 
  "History of connected cores"
let h_lang = 
  message "h_lang" (T.option T.format) 
  "What is the language of your system"

let h_col_default = 
  message "h_col_default" (T.option T.format) 
  "Default color in lists"
let h_col_downloaded = 
  message "h_col_downloaded" (T.option T.format) 
  "Color for downloaded files"
let h_col_downloading = 
  message "h_col_downloading" (T.option T.format) 
  "Color for files being downloaded"
let h_col_avail = 
  message "h_col_avail" (T.option T.format) 
  "Color for available files, not downloading"
let h_col_not_avail = 
  message "h_col_not_avail" (T.option T.format) 
  "Color for unavailable files"
let h_col_connected = 
  message "h_col_connected" (T.option T.format) 
  "Color for connected servers or users"
let h_col_not_connected = 
  message "h_col_not_connected" (T.option T.format) 
  "Color for not connected servers or users"
let h_col_connecting = 
  message "h_col_connecting" (T.option T.format) 
  "Color for servers or users with which a connection is being established"
let h_col_files_listed = 
  message "h_col_files_listed" (T.option T.format) 
  "Color for users whose list of files has been retrieved"
let h_col_files_result = 
  message "h_col_files_result" (T.option T.format) 
  "Color for the result list in the queries tab"
let h_col_tab_selected = 
  message "h_col_tab_selected" (T.option T.format) 
  "Color for tab selected"
let h_col_tab_not_selected = 
  message "h_col_tab_not_selected" (T.option T.format) 
  "Color for tab not selected"
let h_col_network_enabled = 
  message "h_col_network_enabled" (T.option T.format) 
  "Color for the Network Name when enabled"
let h_col_network_disabled = 
  message "h_col_network_disabled" (T.option T.format) 
  "Color for the Network Name when disabled"
let h_col_list_bg = 
  message "h_col_list_bg" (T.option T.format) 
  "Unified color background for lists"

let h_auto_resize = 
  message "h_auto_resize" (T.option T.format) 
  "Auto-resize lists columns"
let h_files_auto_expand_depth = 
  message "h_files_auto_expand_depth" (T.option T.format) 
  "The depth to which the directories of a friend are automatically expanded"
let h_use_size_suffixes = 
  message "h_use_size_suffixes" (T.option T.format) 
  "Whether sizes are printed using G(iga), M(ega) and k(ilo) suffixes."
let h_availability_max =  
  message "h_availability_max" (T.option T.format) 
  "If use_availability_height is true, which availability corresponds to a full bar ?"
let h_use_availability_height = 
  message "h_use_availability_height" (T.option T.format) 
  "Display the availability of a chunk as height or color coded bar"
let h_use_relative_availability =
   message "h_use_relative_availability" (T.option T.format) 
  "Calculate %% availability ignoring already present chunks"
let h_toolbars_style = 
  message "h_toolbars_style" (T.option T.format) 
  "What is displayed in toolbar buttons : text, icon or both"
let h_mini_toolbars = 
  message "h_mini_toolbars" (T.option T.format) 
  "What is displayed in toolbar buttons : small or big icons"

let h_compaction_overhead =
  message "h_compaction_overhead" (T.option T.format) 
  "The percentage of free memory before a compaction is triggered"
let h_interface_buffer =
  message "h_interface_buffer" (T.option T.format) 
  "The size of the buffer to the core"
let h_copy_messages = 
  message "h_copy_messages" (T.option T.format) 
  "For bundle binaries, should we directly pass structures 
   between the core and the GUI (faster), or copy them (fewer bugs)" 

let h_servers_vpane_up = 
  message "h_servers_vpane_up" (T.option T.format) 
  "Size in %% of upper part of the servers hpane"
let h_friends_hpane_left = 
  message "h_friends_hpane_left" (T.option T.format) 
  "Size in %% of left part of the friends hpane"
let h_friends_vpane_up =  
  message "h_friends_vpane_up" (T.option T.format) 
  "Size in %% of up part of the friends vpane"
let h_friends_hpane_dirs = 
  message "h_friends_hpane_dirs" (T.option T.format) 
  "Size in %% of the directories part of the files box"
let h_rooms_hpane_left = 
  message "h_rooms_hpane_left" (T.option T.format) 
  "Size in %% of left part of the rooms hpane"
let h_rooms_hpane2_left = 
  message "h_rooms_hpane2_left" (T.option T.format) 
  "Size in %% of left part of the second rooms hpane"
let h_rooms_vpane_up = 
  message "h_rooms_vpane_up" (T.option T.format) 
  "Size in %% of upper part of the rooms vpane"
let h_uploads_vpane_up = 
  message "h_uploads_vpane_up" (T.option T.format) 
  "Size in %% of up part of the uploads vpane"
let h_gui_width = 
  message "h_gui_width" (T.option T.format) 
  "Width of GUI window"
let h_gui_height = 
  message "h_gui_height" (T.option T.format)
  "Height of GUI window" 
let h_last_tab =
  message "h_last_tab" (T.option T.format)
  "The last tab opened before closing the GUI"

let h_tab_position = 
  message "h_tab_position" (T.option T.format) 
  "How are displayed the Tabs : left, right, top, bottom"
let h_use_icons = 
  message "h_use_icons" (T.option T.format) 
  "Display various icons in the lists"
let h_use_graphical_availability = 
  message "h_use_graphical_availability" (T.option T.format) 
  "What is displayed in availability column : graphical or text"
let h_use_reliable_sources = 
  message "h_use_reliable_sources" (T.option T.format) 
  "Filter the sources that have only a file availability"
let h_max_file_name_len = 
  message "h_max_file_name_len" (T.option T.format) 
  "What is the maximum length of a file name displayed in the downloads list"
let h_max_result_name_len = 
  message "h_max_result_name_len" (T.option T.format) 
  "What is the maximum length of a file name displayed in the results list"
let h_max_client_name_len = 
  message "h_max_client_name_len" (T.option T.format) 
  "What is the maximum length of a client name displayed in the friends and uploads lists"

let h_servers_columns = 
  message "h_servers_columns" (T.option T.format) 
  "Columns for the servers"
let h_downloads_columns = 
  message "h_downloads_columns" (T.option T.format) 
  "Columns for the files being downloaded"
let h_downloaded_columns = 
  message "h_downloaded_columns" (T.option T.format) 
  "Columns for the downloaded files"
let h_friends_columns = 
  message "h_friends_columns" (T.option T.format) 
  "Columns for the friends"
let h_file_locations_columns = 
  message "h_file_locations_columns" (T.option T.format) 
  "Columns for the locations of a file"
let h_users_columns =
  message "h_users_columns" (T.option T.format) 
  "Columns of the users lists"
let h_rooms_columns =
  message "h_rooms_columns" (T.option T.format) 
  "Columns of the room lists"
let h_results_columns = 
  message "h_results_columns" (T.option T.format) 
  "Columns for the results of searches and files of a friends"
let h_shared_files_up_columns = 
  message "h_shared_files_up_columns" (T.option T.format) 
  "Columns for the list of shared files upload information"

let h_max_download_rate = 
  message "h_max_download_rate" (T.option T.format) 
  "Max download bandwith capacity (ko/s)"
let h_max_upload_rate = 
  message "h_max_upload_rate" (T.option T.format) 
  "Max upload bandwith capacity (ko/s)"
let h_download_time_range = 
  message "h_download_time_range" (T.option T.format) 
  "Time range for the downloads graph"
let h_upload_time_range = 
  message "h_upload_time_range" (T.option T.format) 
  "Time range for the uploads graph"
let h_col_bg_download = 
  message "h_col_bg_download" (T.option T.format) 
  "Color for the downloads graph background"
let h_col_bg_upload = 
  message "h_col_bg_upload" (T.option T.format) 
  "Color for the uploads graph background"
let h_col_grid_download = 
  message "h_col_grid_download" (T.option T.format)
  "Color for the downloads graph grid"
let h_col_grid_upload = 
  message "h_col_grid_upload" (T.option T.format) 
  "Color for the uploads graph grid"
let h_col_fg_download = 
  message "h_col_fg_download" (T.option T.format) 
  "Color for the download rate"
let h_col_fg_upload = 
  message "h_col_fg_upload" (T.option T.format) 
  "Color for the upload rate"
let h_col_fg_download_av = 
  message "h_col_fg_download_av" (T.option T.format) 
  "Color for the average download rate"
let h_col_fg_upload_av = 
  message "h_col_fg_upload_av" (T.option T.format) 
  "Color for the average upload rate"

let h_font_list = 
  message "h_font_list" (T.option T.format) 
  "Font for the list & trees texts"
let h_font_main_tab = 
  message "h_font_main_tab" (T.option T.format) 
  "Font for the main notebook tabs labels"
let h_font_networks = 
  message "h_font_networks" (T.option T.format) 
  "Font for the networks labels in the Networks Tab"
let h_font_graphic = 
  message "h_font_graphic" (T.option T.format) 
  "Font to display texts in the Graph tab"

(** Columns **)
let c_name = 
  message "c_name" (T.option T.format) 
  "Name"
let c_md4 = 
  message "c_md4" (T.option T.format) 
  "MD4"
let c_size = 
  message "c_size" (T.option T.format) 
  "Size"
let c_downloaded = 
  message "c_downloaded" (T.option T.format) 
  "Downloaded"
let c_percent = 
  message "c_percent" (T.option T.format) 
  "%%"
let c_state = 
  message "c_state" (T.option T.format) 
  "State"
let c_avail = 
  message "c_avail" (T.option T.format) 
  "Availability"
let c_rate = 
  message "c_rate" (T.option T.format) 
  "Rate"
let c_format = 
  message "c_format" (T.option T.format) 
  "Format"
let c_network = 
  message "c_network" (T.option T.format) 
  "Net"
let c_age = 
  message "c_age" (T.option T.format) 
  "Age"
let c_last_seen = 
  message "c_last_seen" (T.option T.format) 
  "Last seen"
let c_eta = 
  message "c_eta" (T.option T.format) 
  "ETA"
let c_priority = 
  message "c_priority" (T.option T.format) 
  "Priority"
let c_kind = 
  message "c_kind" (T.option T.format) 
  "Kind"
let c_client_type = 
  message "c_client_type" (T.option T.format) 
  "Type"
let c_client_rating = 
  message "c_client_rating" (T.option T.format) 
  "Rating"
let c_client_software = 
  message "c_client_software" (T.option T.format) 
  "Brand"
let c_client_emulemod = 
  message "c_client_emulemod" (T.option T.format) 
  "Mod"
let c_client_downloaded = 
  message "c_client_downloaded" (T.option T.format) 
  "Downloaded"
let c_client_uploaded = 
  message "c_client_uploaded" (T.option T.format) 
  "Uploaded"
let c_client_upload = 
  message "c_client_upload" (T.option T.format) 
  "File uploaded"
let c_client_sock_addr = 
  message "c_client_sock_addr" (T.option T.format) 
  "IP Address"
let c_client_connect_time = 
  message "c_client_connect_time" (T.option T.format) 
  "Connected Time"
let c_address = 
  message "c_address" (T.option T.format) 
  "Address"
let c_server_nusers = 
  message "c_server_nusers" (T.option T.format) 
  "Users"
let c_server_nfiles = 
  message "c_server_nfiles" (T.option T.format) 
  "Files"
let c_server_desc = 
  message "c_server_desc" (T.option T.format) 
  "Description"
let c_duration = 
  message "c_duration" (T.option T.format) 
  "Duration"
let c_codec = 
  message "c_codec" (T.option T.format) 
  "Codec"
let c_bitrate  = 
  message "c_bitrate" (T.option T.format) 
  "Bitrate"
let c_comment = 
  message "c_comment" (T.option T.format) 
  "Comment"
let c_nusers = 
  message "c_nusers" (T.option T.format) 
  "Users"
let c_filename = 
  message "c_filename" (T.option T.format) 
  "Filename"
let c_uploaded = 
  message "c_uploaded" (T.option T.format) 
  "Uploaded"
let c_requests = 
  message "c_requests" (T.option T.format) 
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
let o_xpm_im = "im"
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
let o_xpm_server_c = "server_c"
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
Authors: MLDonkey project, http://www.mldonkey.net/

This documentation file is now obsolete. Read the FAQ instead (either
on the project WEB site or in the FAQ.html file).

For help, see:

IRC channel: irc.freenode.net, chat #mldonkey

Web sites:
http://www.mldonkey.net/         Official site, bug reports
http://www.mldonkeyworld.com/    English forum
http://www.mldonkey.org/         German forum

Mailing-lists:
mldonkey-users@nongnu.org
Archives: http://mail.nongnu.org/mailman/listinfo/mldonkey-users

In the console, use '?' for help on commands.
" version


