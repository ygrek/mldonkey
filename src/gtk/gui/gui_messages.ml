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
      Filename.concat CommonOptions.home_dir "gui_messages.ini"

      (*
let _ =
  lprintf "Using Message File %s" message_file_name; lprint_newline ()
  *)

let message_file = Options.create_options_file message_file_name
let message_section = file_section message_file [] ""
let message name t x = define_option message_section [name] "" t x
  
let ok = message "ok" (T.option T.format) "Ok"
let cancel = message "cancel" (T.option T.format) "Cancel"
let password = message "password" (T.option T.format) "Password"
let address = message "address" (T.option T.format) "Address"
let percent = message "percent" (T.option T.format) "%%"
let rate = message "rate" (T.option T.format) "Rate"
let age = message "age" (T.option T.format) "Age"
let last_seen = message "last_seen" (T.option T.format) "Last seen"
let eta = message "eta" (T.option T.format) "ETA"
let priority = message "priority" (T.option T.format) "Priority"
let state = message "state" (T.option T.format) "State"
let servers = message "servers" (T.option T.format) "Servers"
let downloads = message "downloads" (T.option T.format) "Downloads"
let friends = message "friends" (T.option T.format) "Friends"
let searches = message "searches" (T.option T.format) "Searches"
let options = message "options" (T.option T.format) "Options"
let not_connected = message "not_connected" (T.option T.format) "Not connected"
let black_listed = message "black_listed" (T.option T.format) "Black Listed"
let connected_to_servers = 
  message "connected_to_servers" (T.option (T.int (T.int T.format)))
  "Connected to %d/%d server(s)"
let downloading_files = 
  message "downloading_files" (T.option (T.int T.format))
  "Downloading %d file(s)"
let no_current_search = message "no_current_search" (T.option T.format) "No current search"
let connection_port = message "connection_port" (T.option T.format) "Connection port"
let control_port = message "control_port" (T.option T.format) "Control port"
let gui_port = message "gui_port" (T.option T.format) "GUI port"
let ports = message "ports" (T.option T.format) "Ports"
let delays = message "delays" (T.option T.format) "Delays"
let save_options_delay = message "save_options_delay" (T.option T.format) "Save options delay"
let check_client_connections = message "check_client_connections" (T.option T.format) "Check client connections"
let check_server_connection = message "check_server_connection" (T.option T.format) "Check server connection"
let check_serverDB_connection = message "check_serverDB_connection" (T.option T.format) "Check serverDB connection"
let small_retry_delay = message "small_retry_delay" (T.option T.format) "Small retry delay"
let medium_retry_delay = message "medium_retry_delay" (T.option T.format) "Medium retry delay"
let long_retry_delay = message "long_retry_delay" (T.option T.format) "Long retry delay"
let gui_refresh_delay = message "gui_refresh_delay" (T.option T.format) "Gui refresh delay"
let general = message "general" (T.option T.format) "General"
let name = message "name" (T.option T.format) "Name"
let max_connected_servers = message "max_connected_servers" (T.option T.format) "Max connected servers"
let max_connected_clients = message "max_connected_clients" (T.option T.format) "Max connected clients"
let disconnect = message "disconnect" (T.option T.format) "Disconnect"
let download_limit = message "download_limit" (T.option T.format) "Download limit (kB/s)"
let upload_limit = message "upload_limit" (T.option T.format) "Upload limit (kB/s)"
let timeouts = message "timeouts" (T.option T.format) "Timeouts"
let server_connection = message "server_connection" (T.option T.format) "Server connection"
let client_connection = message "client_connection" (T.option T.format) "Client connection"
let save_and_apply_options = message "save_and_apply_options" (T.option T.format) "Apply and save options"
let query = message "query" (T.option T.format) "Query"
let search = message "search" (T.option T.format) "Search"
let min_size = message "min_size" (T.option T.format) "Min size"
let max_size = message "max_size" (T.option T.format) "Max size"
let media = message "media" (T.option T.format) "Media"
let format = message "format" (T.option T.format) "Format"
let network = message "network" (T.option T.format) "Network"
let client_type = message "client_type" (T.option T.format) "Type"
let album = message "album" (T.option T.format) "Album"
let friend = message "friend" (T.option T.format) "Friend"
let contact = message "contact" (T.option T.format) "Browse"
let artist = message "artist" (T.option T.format) "Artist"
let title = message "title" (T.option T.format) "Title"
let min_bitrate = message "min_bitrate" (T.option T.format) "Min bitrate"
let stop_search = message "stop_search" (T.option T.format) "Stop Search"
let close_search = message "close_search" (T.option T.format) "Remove Search"
let mp3_options = message "mp3_options" (T.option T.format) "Mp3 options"
let submit = message "submit" (T.option T.format) "Submit"
let results = message "results" (T.option (T.int T.format)) "%d Results"
let friends = message "friends" (T.option T.format) "Friends"
let ip = message "ip" (T.option T.format) "IP"
let port = message "port" (T.option T.format) "Port"
let remove = message "remove" (T.option T.format) "Remove"
let close = message "close" (T.option T.format) "Close"
let close_room = message "close_room" (T.option T.format) "Close room"
let view_files = message "view_files" (T.option T.format) "View files"
let view_users = message "view_users" (T.option T.format) "View users"
let files = message "files" (T.option T.format) "Files"
let network = message "network" (T.option T.format) "Network"
let display = message "display" (T.option T.format) "Display"
let filename = message "filename" (T.option T.format) "Filename"
let size = message "size" (T.option T.format) "Size"
let properties = message "properties" (T.option T.format) "Properties"
let md4 = message "md4" (T.option T.format) "MD4"
let download_selected_files = message "download_selected_files" (T.option T.format) "Download selected files"
let download_selected_dir = message "download_selected_dir" (T.option T.format) "Download selected directory"
let download = message "download" (T.option T.format) "Download"
let force_download = message "force_download" (T.option T.format) 
  "Force Download"
let subscribe = message "subscribe" (T.option T.format) "Subscribe"
let downloaded = message "downloaded" (T.option T.format) "Downloaded"
let availability = message "availability" (T.option T.format) "Availability"
let cancel = message "cancel" (T.option T.format) "Cancel"
let retry_connect = message "retry_connect" (T.option T.format) "Retry connect"
let disconnect_all = message "disconnect_all" (T.option T.format) "Disconnect all"
let connected_to_locations = message "connected_to_locations"
    (T.option (T.int (T.int T.format)))
  "Connected to %d / %d locations"
let add_to_friends = message "add_to_friends" (T.option T.format) "Add to friends"
let add_server = message "add_server" (T.option T.format) "Add server"
let add_friend = message "add_friend" (T.option T.format) "Add friend"
let connect_more_servers_text = message "connect_more_servers" (T.option T.format) "More servers"
let connect_more_servers_tips = message "connect_more_servers_tips" (T.option T.format) "Connect to more servers"
let users = message "users" (T.option T.format) "Users"
let status = message "status" (T.option T.format) "Status"
let connect = message "connect" (T.option T.format) "Connect"
let disconnect = message "disconnect" (T.option T.format) "Disconnect"
let files_to_download = message "files_to_download" (T.option T.format) "Downloading Files"
let downloaded_files = message "downloaded_files"
    (T.option (T.int (T.int T.format))) 
  "Downloaded Files: %d/%d"
let files_downloaded = message "files_downloaded"
    (T.option (T.int T.format))  "%d Downloaded Files"
let save_all = message "save_all" (T.option T.format) "Save all"
let save = message "save" (T.option T.format) "Save"
let server_name = message "server_name" (T.option T.format) "Name"
let server_desc = message "server_desc" (T.option T.format) "Description"
let server_nusers = message "server_nusers" (T.option T.format) "Users"
let server_nfiles = message "server_nfiles" (T.option T.format) "Files"
let ed2k = message "ed2k" (T.option T.format) "ed2k:"
let recover_md4 = message "recover_md4" (T.option T.format) "Recover MD4:"
let remove_old_servers_text = message "remove_old_servers" (T.option T.format) "Clean"
let remove_old_servers_tips = message "remove_old_servers_tips" (T.option T.format) "Clean the server list"
let max_server_age = message "max_server_age" (T.option T.format) "Max server age (days)"
let max_hits = message "max_hits" (T.option T.format) "Max Hits"
let features = message "features" (T.option T.format) "Features"
let hostname = message "hostname" (T.option T.format) "Client hostname"
let option_name = message "option_name" (T.option T.format) "Option:"
let option_value = message "option_value" (T.option T.format) "Value:"
let set_option = message "set_option" (T.option T.format) "Set Option"
let command = message "command" (T.option T.format) "Command"
let console = message "console" (T.option T.format) "Console"
let clear_console = message "clear_console" (T.option T.format) "Clear Console"
let friend_kind = message "friend_kind" (T.option T.format) "Kind"
let friend_status = message "friend_status" (T.option T.format) "Status"
let friend_name = message "friend_name" (T.option T.format) "Name"
let dialog = message "dialog" (T.option T.format) "Chat"
let yes = message "yes" (T.option T.format) "Yes"
let no = message "no" (T.option T.format) "No"
let connecting = message "connecting" (T.option T.format) "Connecting"
let initiating = message "initiating" (T.option T.format) "Initiating"
let connected = message "connected" (T.option T.format) "Connected"
let removed = message "removed" (T.option T.format) "Removed"
let queued = message "queued" (T.option T.format) "Queued"
let downloading = message "downloading" (T.option T.format) "Downloading"
let cancelled = message "cancelled" (T.option T.format) "Cancelled"
let paused = message "paused" (T.option T.format) "Paused"
let dl_done = message "dl_done" (T.option T.format) "Done"
let unknown = message "unknown" (T.option T.format) "Unknown"
let nusers = message "nusers" (T.option T.format) "Users"
  
let save_as = message "save_as" (T.option T.format) "Save file as" 
let edit_mp3 = message "edit_mp3" (T.option T.format) "Edit mp3 tags" 
let kind = message "kind" (T.option T.format) "Kind"
let direct = message "direct" (T.option T.format) "Direct"
let upload = message "upload" (T.option T.format) "Upload"
let pause_resume_dl = message "pause_resume_dl" (T.option T.format) "Pause/Resume"
let verify_chunks = message "verify_chunks" (T.option T.format) "Verify chunks"
let preview = message "preview" (T.option T.format) "Preview"
let get_format = message "get_format" (T.option T.format) "Get format info"
let find_friend = message "find_friend" (T.option T.format) "Find friend"
let remove_all_friends_text = message "remove_all_friends" (T.option T.format) "Clear"
let remove_all_friends_tips = message "remove_all_friends_tips" (T.option T.format) "Remove all friends"
let toggle_display_all_servers_text = message "toggle_display_all_servers" (T.option T.format) "View All"
let toggle_display_all_servers_tips = message "toggle_display_all_servers_tips" (T.option T.format) "View all servers (even not connected)"
  
let browse_files = message "browse_files" (T.option T.format) "Browse files"
  
let show_hidden_fields = message "show_hidden_fields" (T.option T.format) "Show hidden fields"

let waiting_for_replies n = Printf.sprintf "Waiting for %d replies" n

let uploads = message "uploads" (T.option T.format) "Uploads"
let requests = message "requests" (T.option T.format) "Requests"
let upstats = message "upstats" (T.option T.format) "Uploads stats"
let uploaded = message "uploaded" (T.option T.format) "Uploaded"
let refresh = message "refresh" (T.option T.format) "Refresh"

let confirm_download_dir dir n = 
  Printf.sprintf "Do you want to download the  %d files in the directory %s ?"
    n dir

(** Menus labels. *)

let mFile = message "mFile" (T.option T.format) "File"
let mKill_server = message "mKill_server" (T.option T.format) "Kill core"
let mFile = message "mFile" (T.option T.format) "File"
let mReconnect = message "mReconnect" (T.option T.format) "Reconnect"
let mDisconnect = message "mDisconnect" (T.option T.format) "Disconnect"
let mQuit = message "mQuit" (T.option T.format) "Quit"
let mHelp = message "mHelp" (T.option T.format) "Help"
let mSettings = message "mSettings" (T.option T.format) "Settings"
let mServers = message "mServers" (T.option T.format) "Servers"
let mDownloads = message "mDownloads" (T.option T.format) "Downloads"
let mFriends = message "mFriends" (T.option T.format) "Friends"
let mConsole = message "mConsole" (T.option T.format) "Console"
let mQueries = message "mQueries" (T.option T.format) "Queries"
let mResults = message "mResults" (T.option T.format) "Results"
let mRooms = message "mRooms" (T.option T.format) "Rooms"
let mUploads = message "mUploads" (T.option T.format) "Uploads"

(** Messages and string constants. *)

let software = "MLDonkey"
let software_version = "1.0"

let chat_config_file = Filename.concat CommonOptions.file_basedir "mldonkey_chat.ini"

(** {2 Command line messages} *)

let usage = Sys.argv.(0)^" [options] <files>\n"
let options_are = "Options are :"


(** {2 Messages} *)

let title = message "title" (T.option T.format) "Title"
let artist = message "artist" (T.option T.format) "Artist"
let album = message "album" (T.option T.format) "Album"
let year = message "year" (T.option T.format) "Year"
let tracknum = message "tracknum" (T.option T.format) "Track number"
let comment = message "comment" (T.option T.format) "Comment"
let genre = message "genre" (T.option T.format) "Genre"

let local_search = message "local_search" (T.option T.format) "Local Search"
let extended_search = message "extended_search" (T.option T.format) "Extended Search"

let action_unknown s = "Unknown action: "^s

let ask_cancel_download_files files =
  "Cancel the download of file(s) \n"^
  (String.concat ",\n" files)^
  " ?"

(** {2 Config options labels and messages} *)

let o_gui_port = message "o_gui_port" (T.option T.format) "GUI port"
let h_gui_port = message "h_gui_port" (T.option T.format) "The server port to connect to"
let o_hostname = message "o_hostname" (T.option T.format) "Hostname"
let h_hostname = message "h_hostname" (T.option T.format) "The server hostname to connect to"
let o_password = message "o_password" (T.option T.format) "Password"
let h_gui_password = message "h_gui_password" (T.option T.format) "The password to use when connecting to the server"
let o_gui_server = message "o_gui_server" (T.option T.format) "GUI server"
let o_col_default = message "o_col_default" (T.option T.format) "Default"
let h_col_default = message "h_col_default" (T.option T.format) "Default color in lists"
let o_col_downloaded = message "o_col_downloaded" (T.option T.format) "Downloaded"
let h_col_downloaded = message "h_col_downloaded" (T.option T.format) "Color for downloaded files"
let o_col_downloading = message "o_col_downloading" (T.option T.format) "Downloading"
let h_col_downloading = message "h_col_downloading" (T.option T.format) "Color for files being downloaded"
let o_col_avail = message "o_col_avail" (T.option T.format) "Available"
let h_col_avail = message "h_col_avail" (T.option T.format) "Color for available files, not downloading"
let o_col_not_avail = message "o_col_not_avail" (T.option T.format) "Not available"
let h_col_not_avail = message "h_col_not_avail" (T.option T.format) "Color for unavailable files"
let o_col_connected = message "o_col_connected" (T.option T.format) "Connected"
let h_col_connected = message "h_col_connected" (T.option T.format) "Color for connected servers or users"
let o_col_not_connected = message "o_col_not_connected" (T.option T.format) "Not connected"
let h_col_not_connected = message "h_col_not_connected" (T.option T.format) "Color for not connected servers or users"
let o_col_connecting = message "o_col_connecting" (T.option T.format) "Connecting"
let h_col_connecting = 
  "Color for servers or users with which a connection is being established"
let o_col_files_listed = message "o_col_files_listed" (T.option T.format) "Files listed"
let h_col_files_listed = 
  "Color for users whose list of files has been retrieved"
let o_colors = message "o_colors" (T.option T.format) "Colors"

let o_auto_resize = message "o_auto_resize" (T.option T.format) "Auto-resize"
let h_auto_resize = message "h_auto_resize" (T.option T.format) "Auto-resize lists columns"

let o_files_auto_expand_depth = message "o_files_auto_expand_depth" (T.option T.format) "Files auto-expand depth"
let h_files_auto_expand_depth = 
  "The depth to which the directories of a friend are automatically expanded"

let o_use_size_suffixes = message "o_use_size_suffixes" (T.option T.format) "Use size suffixes (G, M, k)"
let h_use_size_suffixes = 
  "Whether sizes are printed using G(iga), M(ega) and k(ilo) suffixes."

let h_use_availability_height = message "h_use_availability_height" (T.option T.format) "Display the availability of a chunk as height or color coded bar"
let o_use_availability_height = message "o_use_availability_height" (T.option T.format) "Use height encoded availability"

let h_use_relative_availability = message "h_use_relative_availability" (T.option T.format) "Calculate %% availability ignoring already present chunks"
let o_use_relative_availability = message "o_use_relative_availability" (T.option T.format) "Use relative %% availability"

let h_chunk_width = message "h_chunk_width" (T.option T.format) "Width of a chunk in availability bar"
let o_chunk_width = message "o_chunk_width" (T.option T.format) "Chunk width"

let h_toolbars_style = message "h_toolbars_style" (T.option T.format) "What is displayed in toolbar buttons : text, icon or both"
let o_toolbars_style = message "o_toolbars_style" (T.option T.format) "Style of toolbars"

let o_layout = message "o_layout" (T.option T.format) "Layout"

let o_servers_columns = message "o_servers_columns" (T.option T.format) "Servers"
let h_servers_columns = message "h_servers_columns" (T.option T.format) "Columns for the servers"
let o_downloads_columns = message "o_downloads_columns" (T.option T.format) "Downloads"
let h_downloads_columns = message "h_downloads_columns" (T.option T.format) "Columns for the files being downloaded"
let o_downloaded_columns = message "o_downloaded_columns" (T.option T.format) "Downloaded"
let h_downloaded_columns = message "h_downloaded_columns" (T.option T.format) "Columns for the downloaded files"
let o_friends_columns = message "o_friends_columns" (T.option T.format) "Friends"
let h_friends_columns = message "h_friends_columns" (T.option T.format) "Columns for the friends"
let o_file_locations_columns = message "o_file_locations_columns" (T.option T.format) "File locations"
let h_file_locations_columns = message "h_file_locations_columns" (T.option T.format) "Columns for the locations of a file"
let o_results_columns = message "o_results_columns" (T.option T.format) "Results"
let h_results_columns = message "h_results_columns" (T.option T.format) "Columns for the results of searches and files of a friends"

let o_shared_files_up_colums = message "o_shared_files_up_colums" (T.option T.format) "Shared files upload info"
let h_shared_files_up_columns = 
  "Columns for the list of shared files upload information"

let o_columns = message "o_columns" (T.option T.format) "Columns titles"

let o_gui = message "o_gui" (T.option T.format) "GUI"
let o_client = message "o_client" (T.option T.format) "Client"
let o_options = message "o_options" (T.option T.format) "Options"
let o_misc = message "o_misc" (T.option T.format) "Misc"

(** {2 Strings to specify colors} *)



(** {2 Actions names for key bindings} *)

let a_page_servers = "page_servers"
let a_page_downloads = "page_downloads"
let a_page_friends = "page_friends"
let a_page_queries = "page_queries"
let a_page_results = "page_results"
let a_page_rooms = "page_rooms"
let a_page_uploads = "page_uploads"
let a_page_options = "page_options"
let a_page_console = "page_console"
let a_page_help = "page_help"
let a_next_page = "next_page"
let a_previous_page = "previous_page"
let a_reconnect = "reconnect"
let a_exit = "exit"

let a_select_all = "select_all"
let a_connect = "connect"
let a_connect_more = "connect_more"


let a_download_selection = "download_selection"
let a_remove_friend = "remove_friend"

let a_cancel_download = "cancel_download"
let a_save_all_files = "save_all_files"
let a_menu_save_file = "menu_save_file"

(** {2 Dangerous Strings} *)

(* Be careful, these strings may appear in configuration files...
Changing them may make your config unparsable. *)
  
let c_name = "Name"
let c_md4 = "MD4"
let c_size = "Size"
let c_downloaded = "Downloaded"
let c_state = "State"
let c_avail = "Availability"
let c_rate = "Rate"
let c_format = "Format"
let c_network = "Network"
let c_age = "Age"
let c_last_seen = "Last seen"
let c_eta = "ETA"
let c_priority = "Priority"
let c_kind = "Kind"
let c_client_type = "Type"
let c_address = "Address"
let c_server_nusers = "Users"
let c_server_nfiles = "Files"
let c_server_desc = "Description"
let c_properties = "Properties"
let c_comment = "Comment"
let c_nusers = "Users"
let c_filename = "Filename"
let c_uploaded = "Uploaded"
let c_requests = "Requests"
  
let o_xpm_remove = "remove"
let o_xpm_cancel = "cancel"
let o_xpm_connect = "connect"
let o_xpm_disconnect = "disconnect"
let o_xpm_view_users = "view_users"
let o_xpm_connect_more_servers = "connect_more_servers"
let o_xpm_remove_old_servers = "remove_old_servers"
let o_xpm_save = "save"
let o_xpm_save_all = "save_all"
let o_xpm_save_as = "save_as"
let o_xpm_edit_mp3 = "edit_mp3"
let o_xpm_pause_resume = "pause_resume"
let o_xpm_get_format = "get_format"
let o_xpm_preview = "preview"
let o_xpm_verify_chunks = "verify_chunks"
let o_xpm_retry_connect = "retry_connect"
let o_xpm_add_to_friends = "add_to_friends"
let o_xpm_toggle_display_all_servers = "toggle_display_all_servers"
let o_xpm_download = "download"
let o_xpm_submit_search = "submit_search"
let o_xpm_extend_search = "extend_search"
let o_xpm_local_search = "local_search"
let o_xpm_find_friend = "find_friend"
let o_xpm_remove_all_friends = "remove_all_friends"
let o_xpm_close_room = "close_room"
let o_xpm_refresh = "refresh"

let o_xpm_mini_remove = "remove_mini"
let o_xpm_mini_cancel = "cancel_mini"
let o_xpm_mini_connect = "connect_mini"
let o_xpm_mini_disconnect = "disconnect_mini"
let o_xpm_mini_view_users = "view_users_mini"
let o_xpm_mini_connect_more_servers = "connect_more_servers_mini"
let o_xpm_mini_remove_old_servers = "remove_old_servers_mini"
let o_xpm_mini_save = "save_mini"
let o_xpm_mini_save_all = "save_all_mini"
let o_xpm_mini_save_as = "save_as_mini"
let o_xpm_mini_edit_mp3 = "edit_mp3_mini"
let o_xpm_mini_pause_resume = "pause_resume_mini"
let o_xpm_mini_get_format = "get_format_mini"
let o_xpm_mini_preview = "preview_mini"
let o_xpm_mini_verify_chunks = "verify_chunks_mini"
let o_xpm_mini_retry_connect = "retry_connect_mini"
let o_xpm_mini_add_to_friends = "add_to_friends_mini"
let o_xpm_mini_toggle_display_all_servers = "toggle_display_all_servers_mini"
let o_xpm_mini_download = "download_mini"
let o_xpm_mini_submit_search = "submit_search_mini"
let o_xpm_mini_extend_search = "extend_search_mini"
let o_xpm_mini_local_search = "local_search_mini"
let o_xpm_mini_find_friend = "find_friend_mini"
let o_xpm_mini_remove_all_friends = "remove_all_friends_mini"
let o_xpm_mini_close_room = "close_room_mini"
let o_xpm_mini_refresh = "refresh_mini"

  

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

let _s x = _s "Gui_messages" x
let _b x = _b "Gui_messages" x  

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
