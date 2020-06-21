(* Copyright 2004 b8_bavard, INRIA *)
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

(* GUI labels. *)

open Printf2
open Options
open Gettext

let bin_dir = Filename.dirname Sys.argv.(0)

let hidden_dir_prefix =
  if Autoconf.system = "windows" then "" else "."

let config_dir_basename = hidden_dir_prefix ^ "mldonkey"

let home_dir =
  match Autoconf.system with
  | "windows" -> if (Filename.basename bin_dir) = "bin" then Filename.dirname bin_dir else "."
  | _ -> Filename.concat (try Sys.getenv "HOME" with _ -> ".") config_dir_basename

let file_basedir_pre =
  try
    let s = Sys.getenv "MLDONKEY_DIR" in
    if s = "" then home_dir else Filename2.normalize s
  with _ -> home_dir

let file_basedir =
(* Creating dirs does work differently on Windows than Unix.
   Dirs like c:\b are split down by unix2.safe_mkdir to "c".
   This function splits the directory name into the drive name
   and chdir to it before creating directories.
   Non-absolute paths in $MLDONKEY_DIR do not work as well *)
  if Sys.file_exists (Filename.concat (Sys.getcwd ()) "mlgui.ini") then
    "."
  else
    if Autoconf.system = "windows" && file_basedir_pre <> home_dir then
      match String2.split file_basedir_pre ':' with
      | drive :: directory :: _ ->
          Unix.chdir (drive ^ ":\\");
          directory
      | _ -> lprintf "Please provide an absolute path in MLDONKEY_DIR like d:\\mldonkey, exiting...\n"; exit 2
    else file_basedir_pre

let _ =
  if not !CommonGlobals.core_included
    then begin
      (try
         Unix2.safe_mkdir file_basedir
       with e ->
         lprintf_nl "Exception (%s) trying to create dir %s"
           (Printexc2.to_string e) file_basedir;
         exit 2);
      Unix2.can_write_to_directory file_basedir;
      Unix.chdir file_basedir;
      lprintf_nl "Starting MLGui %s ... " Autoconf.current_version;
      lprintf_nl "MLGui is working in %s" (Sys.getcwd ())
    end


let dirname_concat dir =
  match Autoconf.system with
      "windows" -> Filename.concat file_basedir dir
    | _ -> file_basedir

let lang_dir = dirname_concat "lang"
let gui_config_dir = dirname_concat "interface"
let log_dir = dirname_concat "logs"

let _ =
  if not !CommonGlobals.core_included
  then begin
    let filename =
      try
        Sys.getenv "MLGUI_STRINGS"
      with _ ->
        Filename.concat lang_dir "mlgui_strings"
    in
    Unix2.safe_mkdir (Filename.dirname filename);
    Unix2.can_write_to_directory (Filename.dirname filename);
    set_strings_file filename
  end

let _s x = (* lprintf "searching string %s\n" x; *) GuiUtf8.simple_utf8_of (_s "GuiMessages" x)

let _s_ x = (_s x) ^ ":"
let _b x = _b "GuiMessages" x
let message (t : ('a , unit, string) format Options.option_class) x = (_b x : ('a, unit, string) format)

(*

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

*)

(*
 * TODO :
 *   - change utils/lib/gettext.ml4 to allow changing language without
 *     restarting mlnet and mlgui?
 *     Some ideas :
 *       * add a new message between gui/core to change language. Core sends
 *         back new strings (options, help messages, etc...) to gui
 *       * or force the core in english and make translation in gui only?
 *)



(* Networks Tab *)

let nT_lb_display  = ref ""
let nT_lb_net_bt  = ref ""
let nT_lb_net_dc  = ref ""
let nT_lb_net_ed2k  = ref ""
let nT_lb_net_nap  = ref ""
let nT_lb_net_gnut  = ref ""
let nT_lb_net_gnut2  = ref ""
let nT_lb_net_ftt  = ref ""
let nT_lb_net_slsk  = ref ""
let nT_lb_net_filetp  = ref ""
let nT_lb_net_dled  = ref ""
let nT_lb_net_uled  = ref ""
let nT_lb_net_serv_connected  = ref ""

(* Servers Tab *)

let sT_lb_users  = ref ""
let sT_lb_servers  = ref ""
let sT_lb_add_server  = ref ""
let sT_lb_network  = ref ""
let sT_lb_server_ip  = ref ""
let sT_lb_server_port  = ref ""
let sT_lb_server_add  = ref ""
let sT_lb_display_all_servers  = ref ""
let sT_me_remove  = ref ""
let sT_me_disconnect  = ref ""
let sT_me_view_users  = ref ""
let sT_me_connect  = ref ""
let sT_me_connect_more_servers  = ref ""
let sT_me_remove_old_servers  = ref ""
let sT_me_clear_users  = ref ""

(* Downloads Tab *)

let dT_tx_updown  = ref ""
let dT_tx_uploading  = ref ""
let dT_tx_downloading  = ref ""
let dT_tx_waiting  = ref ""
let dT_tx_cancelled  = ref ""
let dT_tx_queued  = ref ""
let dT_tx_paused  = ref ""
let dT_tx_complete  = ref ""
let dT_tx_dl_done  = ref ""
let dT_tx_dl_aborted  = ref (message (T.option (T.string T.format)) "Aborted : %s")
let dT_tx_connected  = ref ""
let dT_tx_connected_high  = ref ""
let dT_tx_connecting  = ref ""
let dT_tx_new_host  = ref ""
let dT_tx_initiating  = ref ""
let dT_tx_ranked  = ref (message (T.option (T.int T.format))  "Ranked %d")
let dT_tx_queued_out  = ref ""
let dT_tx_ranked_out  = ref (message (T.option (T.int T.format))  "Ranked %d out")
let dT_tx_failed  = ref (message (T.option (T.int T.format))  "Failed %d")
let dT_tx_removed  = ref ""
let dT_tx_black_listed  = ref ""
let dT_tx_unknown  = ref ""
let dT_tx_priority_veryhigh  = ref ""
let dT_tx_priority_high  = ref ""
let dT_tx_priority_normal  = ref ""
let dT_tx_priority_low  = ref ""
let dT_tx_priority_verylow  = ref ""
let dT_wt_save_as  = ref ""
let dT_lb_save_as  = ref ""
let dT_wt_cancel  = ref ""
let dT_lb_ask_cancel_download_files  = ref ""
let dT_wt_edit_mp3  = ref ""
let dT_me_preview  = ref ""
let dT_me_pause_resume_dl  = ref ""
let dT_me_retry_connect  = ref ""
let dT_me_cancel  = ref ""
let dT_me_verify_chunks  = ref ""
let dT_me_set_priority  = ref ""
let dT_me_set_priority_veryhigh  = ref ""
let dT_me_set_priority_high  = ref ""
let dT_me_set_priority_normal  = ref ""
let dT_me_set_priority_low  = ref ""
let dT_me_set_priority_verylow  = ref ""
let dT_me_get_format = ref ""
let dT_me_edit_mp3  = ref ""
let dT_me_save_all  = ref ""
let dT_me_save_as  = ref ""
let dT_me_save  = ref ""
let dT_me_browse_files  = ref ""
let dT_me_add_to_friends  = ref ""
let dT_me_razorback2_stats = ref ""
let dT_me_filedonkey_stats = ref ""
let dT_me_isohunt_stats = ref ""
let dT_lb_link  = ref ""
let dT_me_show_file_details  = ref ""
let dT_me_show_source_details  = ref ""
let dT_wt_show_source_details  = ref ""
let dT_lb_stats_get_main_page  = ref ""
let dT_lb_stats_get_image  = ref ""
let dT_lb_stats_rate  = ref ""
let dT_lb_stats_available  = ref ""
let dT_lb_stats_complete  = ref ""

(* Friends Tab *)

let fT_lb_friends  = ref ""
let fT_lb_messages  = ref ""
let fT_wt_find_friend  = ref ""
let fT_lb_name  = ref ""
let fT_me_find_friend  = ref ""
let fT_me_remove  = ref ""
let fT_me_remove_all_friends  = ref ""

(* Query Box *)

let qT_lb_directories  = ref ""
let qT_lb_results  = ref ""
let qT_lb_all_networks  = ref ""
let qT_me_download  = ref ""
let qT_me_force_download  = ref ""
let qT_me_download_directory  = ref ""
let qT_wt_download_selected_dir  = ref ""
let qT_lb_confirm_download_dir  = ref (
    message (T.option (T.int (T.string T.format))) "Do you want to download the %d files in the directory %s ?")
let qT_lb_waiting_for_replies  = ref ""
let qT_lb_max_hits  = ref ""
let qT_lb_extended_fields  = ref ""
let qT_lb_local_search  = ref ""
let qT_lb_submit_search  = ref ""
let qT_lb_subscribe_search  = ref ""
let qT_lb_customed_search_editor  = ref ""
let qT_lb_stop_search  = ref ""
let qT_lb_extend_search  = ref ""
let qT_lb_album_searches  = ref ""
let qT_lb_movie_searches  = ref ""
let qT_lb_mp3_searches  = ref ""
let qT_lb_complex_searches  = ref ""
let qT_lb_freedb_searches  = ref ""
let qT_lb_imdb_searches  = ref ""
let qT_lb_and_not  = ref ""
let qT_lb_or  = ref ""
let qT_tx_audio  = ref ""
let qT_tx_video  = ref ""
let qT_tx_program  = ref ""
let qT_tx_image  = ref ""
let qT_tx_documentation  = ref ""
let qT_tx_collection  = ref ""
let qT_lb_keywords  = ref ""
let qT_lb_media  = ref ""
let qT_lb_format  = ref ""
let qT_lb_min_size  = ref ""
let qT_lb_max_size  = ref ""
let qT_lb_min_bitrate  = ref ""
let qT_lb_title  = ref ""
let qT_lb_number_of_results  = ref ""
let qT_lb_sort_by  = ref ""
let qT_lb_album  = ref ""
let qT_lb_fields  = ref ""
let qT_lb_artist  = ref ""
let qT_lb_track_title  = ref ""
let qT_lb_track  = ref ""
let qT_lb_rest  = ref ""
let qT_lb_categories  = ref ""
let qT_lb_all  = ref ""
let qT_lb_blues  = ref ""
let qT_lb_classical  = ref ""
let qT_lb_data  = ref ""
let qT_lb_folk  = ref ""
let qT_lb_rock  = ref ""
let qT_lb_soundtrack  = ref ""
let qT_lb_availability  = ref ""
let qT_lb_size  = ref ""
let qT_lb_year = ref ""
let qT_lb_genre = ref ""
let qT_lb_comment = ref ""
let qT_lb_advanced_search = ref ""
let qT_lb_network = ref ""
let qT_lb_min_availability = ref ""

(* Rooms Tab *)

let rT_lb_chat  = ref ""
let rT_lb_rooms  = ref ""
let rT_lb_users  = ref ""
let rT_lb_paused_room  = ref ""
let rT_lb_closed_room  = ref ""
let rT_lb_opened_room  = ref ""
let rT_me_close_open_room  = ref ""
let rT_me_browse_files  = ref ""
let rT_me_add_to_friends  = ref ""

(* Uploads *)

let uT_lb_uploads  = ref ""
let uT_lb_uploaders  = ref ""
let uT_me_copy_ed2k  = ref ""
let uT_lb_show_pending  = ref ""
let uT_tx_friend  = ref ""
let uT_tx_contact  = ref ""
let uT_tx_normal  = ref ""

(* Console *)

let cT_lb_clear_console  = ref ""
let cT_lb_command  = ref ""

(* Graph *)

let gT_lb_time_range = ref ""
let gT_lb_quarter = ref ""
let gT_lb_hour = ref ""
let gT_lb_halfday = ref ""
let gT_lb_day = ref ""
let gT_lb_week = ref ""
let gT_lb_month = ref ""
let gT_lb_year = ref ""
let gT_lb_global_downloads = ref ""
let gT_lb_global_uploads = ref ""
let gT_lb_file_down_up = ref ""

(* file Window *)

let fW_wt_show_file_details  = ref ""
let fW_lb_file_info  = ref ""
let fW_lb_transfer_info  = ref ""
let fW_lb_avail_info  = ref ""
let fW_lb_file_names  = ref ""
let fW_lb_file_hash  = ref ""
let fW_lb_file_format = ref ""
let fW_lb_file_size  = ref ""
let fW_lb_file_state  = ref ""
let fW_lb_file_chunks  = ref ""
let fW_lb_file_avail  = ref ""
let fW_lb_file_age  = ref ""
let fW_lb_file_prio  = ref ""
let fW_lb_file_sources  = ref ""
let fW_lb_file_rate  = ref ""
let fW_lb_file_dled  = ref ""
let fW_lb_file_complete_chunks  = ref ""
let fW_lb_last_seen  = ref ""
let fW_lb_file_eta  = ref ""
let fW_lb_rename  = ref ""
let fW_lb_file_names_col  = ref ""
let fW_wt_show_source_details  = ref ""
let fW_lb_source_info  = ref ""
let fW_lb_files_requested  = ref ""
let fW_lb_source_kind  = ref ""
let fW_lb_source_software  = ref ""
let fW_lb_source_chat_port  = ref ""
let fW_lb_source_dlding  = ref ""
let fW_lb_source_connect_time  = ref ""
let fW_lb_source_dled  = ref ""
let fW_lb_source_upled  = ref ""
let fW_lb_source_rating  = ref ""
let fW_lb_source_hash  = ref ""
let fW_lb_source_average_dled  = ref ""
let fW_lb_source_average_upled  = ref ""

(* Popup Windows *)

let pW_lb_ok  = ref ""
let pW_lb_apply  = ref ""
let pW_lb_default  = ref ""
let pW_lb_cancel  = ref ""
let pW_lb_close  = ref ""
let pW_wt_bad_password  = ref ""
let pW_lb_bad_password  = ref ""

(* Main Window *)

let mW_wt_software  = ref ""
let mW_lb_connecting  = ref ""
let mW_lb_connected  = ref ""
let mW_lb_not_connected  = ref ""
let mW_lb_updload  = ref ""
let mW_lb_download  = ref ""
let mW_lb_shared  = ref ""
let mW_lb_filesdownloaded  = ref ""
let mW_lb_serversconnected  = ref ""
let mW_lb_networks  = ref ""
let mW_lb_servers  = ref ""
let mW_lb_downloads  = ref ""
let mW_lb_friends  = ref ""
let mW_lb_search  = ref ""
let mW_lb_rooms  = ref ""
let mW_lb_uploads  = ref ""
let mW_lb_console  = ref ""
let mW_lb_graph  = ref ""
let mW_lb_settings  = ref ""
let mW_lb_initializing  = ref ""
let mW_lb_warning = ref ""
let mW_me_quit  = ref ""
let mW_me_kill_core  = ref ""
let mW_me_reconnect  = ref ""
let mW_me_disconnect  = ref ""
let mW_me_scan_ports  = ref ""
let mW_me_reconnect_to  = ref ""
let mW_me_settings  = ref ""
let mW_me_main_menu  = ref ""
let mW_me_restore = ref ""

(* Settings Window *)

let cW_lb_main  = ref ""
let cW_lb_interfaces  = ref ""
let cW_lb_tools  = ref ""
let cW_lb_other  = ref ""
let cW_lb_bittorrent  = ref ""
let cW_lb_direct_connect  = ref ""
let cW_lb_donkey  = ref ""
let cW_lb_fasttrack  = ref ""
let cW_lb_filetp  = ref ""
let cW_lb_gnutella  = ref ""
let cW_lb_open_napster  = ref ""
let cW_lb_soulseek  = ref ""
let cW_lb_general  = ref ""
let cW_lb_bandwidth  = ref ""
let cW_lb_network_config  = ref ""
let cW_lb_security  = ref ""
let cW_lb_mlgui  = ref ""
let cW_lb_html_mods  = ref ""
let cW_lb_paths  = ref ""
let cW_lb_startup  = ref ""
let cW_lb_download  = ref ""
let cW_lb_mail  = ref ""
let cW_lb_debug  = ref ""
let cW_lb_gnutella1  = ref ""
let cW_lb_gnutella2  = ref ""
let cW_lb_general_  = ref ""
let cW_lb_rates  = ref ""
let cW_lb_connections  = ref ""
let cW_lb_proxy  = ref ""
let cW_lb_user  = ref ""
let cW_lb_gui  = ref ""
let cW_lb_web  = ref ""
let cW_lb_telnet  = ref ""
let cW_lb_gift  = ref ""
let cW_lb_colors  = ref ""
let cW_lb_fonts  = ref ""
let cW_lb_graph  = ref ""
let cW_lb_display_conf  = ref ""
let cW_lb_look  = ref ""
let cW_lb_mail_setup  = ref ""
let cW_lb_client  = ref ""
let cW_lb_server  = ref ""
let cW_lb_connection_param  = ref ""
let cW_lb_uploads  = ref ""
let cW_lb_downloads  = ref ""
let cW_lb_sources  = ref ""
let cW_lb_peers  = ref ""
let cW_lb_servers  = ref ""
let cW_lb_overnet  = ref ""
let cW_lb_tracker  = ref ""
let cW_lb_other_  = ref ""
let cW_lb_browse  = ref ""
let cW_lb_font_sample  = ref ""
let cW_lb_day  = ref ""
let cW_lb_hour  = ref ""
let cW_lb_minute  = ref ""
let cW_lb_second  = ref ""

(* Im Window *)

let iM_me_default  = ref ""
let iM_me_arabic  = ref ""
let iM_me_armenian  = ref ""
let iM_me_baltic  = ref ""
let iM_me_celtic  = ref ""
let iM_me_centraleuropean  = ref ""
let iM_me_chinesesimplified  = ref ""
let iM_me_chinesetraditional  = ref ""
let iM_me_cyrillic  = ref ""
let iM_me_georgian  = ref ""
let iM_me_greek  = ref ""
let iM_me_hebrew  = ref ""
let iM_me_japanese  = ref ""
let iM_me_korean  = ref ""
let iM_me_nordic  = ref ""
let iM_me_romanian  = ref ""
let iM_me_southeuropean  = ref ""
let iM_me_tajik  = ref ""
let iM_me_thai  = ref ""
let iM_me_turkish  = ref ""
let iM_me_unicode  = ref ""
let iM_me_vietnamese  = ref ""
let iM_me_westerneuropean  = ref ""
let iM_me_auto_detect  = ref ""
let iM_me_character_coding  = ref ""

let iM_wt_software = ref ""
let iM_lb_title = ref ""
let iM_me_new_accounts = ref ""
let iM_me_x_account  = ref (message (T.option (T.string T.format))  "New %s account")
let iM_me_menu = ref ""
let iM_tx_online = ref ""
let iM_tx_online_away = ref ""
let iM_tx_connecting = ref ""
let iM_tx_offline = ref ""
let iM_me_connect_disconnect = ref ""
let iM_me_settings = ref ""
let iM_me_quit = ref ""
let iM_me_remove = ref ""
let iM_me_join_room = ref ""
let iM_me_preferred_rooms = ref ""
let iM_lb_new_account = ref ""
let im_lb_room_name = ref ""
let iM_lb_join_room = ref ""
let iM_lb_close = ref ""
let iM_lb_accounts = ref ""

(* Common Menus labels *)

let mAutosize  = ref ""
let mAutosize_all = ref ""
let mSort  = ref ""
let mRemove_column  = ref ""
let mAdd_column_after  = ref ""
let mAdd_column_before  = ref ""

(* Columns names *)
let c_name  = ref ""
let c_md4  = ref ""
let c_uid  = ref ""
let c_size  = ref ""
let c_downloaded  = ref ""
let c_percent  = ref ""
let c_state  = ref ""
let c_avail  = ref ""
let c_rate  = ref ""
let c_format  = ref ""
let c_network  = ref ""
let c_age  = ref ""
let c_last_seen  = ref ""
let c_comment  = ref ""
let c_eta  = ref ""
let c_priority  = ref ""
let c_kind  = ref ""
let c_client_type  = ref ""
let c_client_rating  = ref ""
let c_client_software  = ref ""
let c_client_emulemod  = ref ""
let c_client_downloaded  = ref ""
let c_client_uploaded  = ref ""
let c_client_download_rate  = ref ""
let c_client_upload_rate  = ref ""
let c_client_upload  = ref ""
let c_client_sock_addr  = ref ""
let c_client_connect_time  = ref ""
let c_address  = ref ""
let c_server_nusers  = ref ""
let c_server_nfiles  = ref ""
let c_server_desc  = ref ""
let c_duration  = ref ""
let c_codec  = ref ""
let c_bitrate   = ref ""
let c_comment  = ref ""
let c_nusers  = ref ""
let c_filename  = ref ""
let c_uploaded  = ref ""
let c_requests  = ref ""
let c_tags  = ref ""
let c_type  = ref ""
let c_folders  = ref ""
let c_sources  = ref ""
let c_complete  = ref ""
let c_status = ref ""
let c_protocol = ref ""
let c_preferred = ref ""

let load_messages () =
(* Networks Tab *)

  nT_lb_display  :=  _s "Display";
  nT_lb_net_bt  :=  _s "BITTORRENT";
  nT_lb_net_dc  :=  _s "DIRECT CONNECT";
  nT_lb_net_ed2k  :=  _s "EDONKEY / OVERNET";
  nT_lb_net_nap  :=  _s "OPEN NAPSTER";
  nT_lb_net_gnut  :=  _s "GNUTELLA";
  nT_lb_net_gnut2  :=  _s "GNUTELLA2";
  nT_lb_net_ftt  :=  _s "FASTTRACK";
  nT_lb_net_slsk  :=  _s "SOULSEEK";
  nT_lb_net_filetp  :=  _s "FileTP";
  nT_lb_net_dled  :=  _s_ "Downloaded";
  nT_lb_net_uled  :=  _s_ "Uploaded";
  nT_lb_net_serv_connected  :=  _s_ "Connected servers";

(* Servers Tab *)

  sT_lb_users  :=  _s "Users";
  sT_lb_servers  :=  _s "Servers";
  sT_lb_add_server  :=  _s "Add a server";
  sT_lb_network  :=  _s_ "Network";
  sT_lb_server_ip  :=  _s_ "Ip address";
  sT_lb_server_port  :=  _s_ "Port";
  sT_lb_server_add  :=  _s "_Add server";
  sT_lb_display_all_servers  :=  _s "Vi_ew all servers";
  sT_me_remove  :=  _s "Remove";
  sT_me_disconnect  :=  _s "Disconnect";
  sT_me_view_users  :=  _s "View users";
  sT_me_connect  :=  _s "Connect";
  sT_me_connect_more_servers  :=  _s "More servers";
  sT_me_remove_old_servers  :=  _s "Clear list";
  sT_me_clear_users  :=  _s "Clear users";

(* Downloads Tab *)

  dT_tx_updown  :=  _s "Uploading/Downloading";
  dT_tx_uploading  :=  _s "Uploading";
  dT_tx_downloading  :=  _s "Downloading";
  dT_tx_waiting  :=  _s "Waiting";
  dT_tx_cancelled  :=  _s "Cancelled";
  dT_tx_queued  :=  _s "Queued";
  dT_tx_paused  :=  _s "Paused";
  dT_tx_complete  :=  _s "Complete";
  dT_tx_dl_done  :=  _s "Done";
  dT_tx_dl_aborted  :=  message (T.option (T.string T.format)) "Aborted : %s";
  dT_tx_connected  :=  _s "Connected";
  dT_tx_connected_high  :=  _s "Connected [High ID]";
  dT_tx_connecting  :=  _s "Connecting";
  dT_tx_new_host  :=  _s "New Host";
  dT_tx_initiating  :=  _s "Initiating";
  dT_tx_ranked  :=  message (T.option (T.int T.format))  "Ranked %d";
  dT_tx_queued_out  :=  _s "Queued Out";
  dT_tx_ranked_out  :=  message (T.option (T.int T.format))  "Ranked %d out";
  dT_tx_failed  :=  message (T.option (T.int T.format))  "Failed %d";
  dT_tx_removed  :=  _s "Removed";
  dT_tx_black_listed  :=  _s "Black Listed";
  dT_tx_unknown  :=  _s "Unknown";
  dT_tx_priority_veryhigh  :=  _s "Very high";
  dT_tx_priority_high  :=  _s "High";
  dT_tx_priority_normal  :=  _s "Normal";
  dT_tx_priority_low  :=  _s "Low";
  dT_tx_priority_verylow  :=  _s "Very low";
  dT_wt_save_as  :=  _s "Save file as" ;
  dT_lb_save_as  :=  _s "Save file as : ";
  dT_wt_cancel  :=  _s "Cancel File(s)";
  dT_lb_ask_cancel_download_files  :=  _s "Cancel the following file(s) ?\n";
  dT_wt_edit_mp3  :=  _s "MP3 tags Editor";
  dT_me_preview  :=  _s "Preview";
  dT_me_pause_resume_dl  :=  _s "Pause / Resume";
  dT_me_retry_connect  :=  _s "Retry connect";
  dT_me_cancel  :=  _s "Cancel";
  dT_me_verify_chunks  :=  _s "Verify chunks";
  dT_me_set_priority  :=  _s "Set priority";
  dT_me_set_priority_veryhigh  :=  _s "Very high";
  dT_me_set_priority_high  :=  _s "High";
  dT_me_set_priority_normal  :=  _s "Normal";
  dT_me_set_priority_low  :=  _s "Low";
  dT_me_set_priority_verylow  :=  _s "Very low";
  dT_me_get_format := _s "Get file format";
  dT_me_edit_mp3  :=  _s "Edit mp3 tags";
  dT_me_save_all  :=  _s "Save all";
  dT_me_save_as  :=  _s "Save file as";
  dT_me_save  :=  _s "Save";
  dT_me_browse_files  :=  _s "Browse files";
  dT_me_add_to_friends  :=  _s "Add to friends";
  dT_lb_link  :=  _s "Download link : ";
  dT_me_show_file_details  :=  _s "Show file details";
  dT_me_show_source_details  :=  _s "Show source details";
  dT_wt_show_source_details  :=  _s "Source details";
  dT_me_razorback2_stats := _s "Get/Refresh Razorback Stats";
  dT_me_filedonkey_stats := _s "Get/Refresh FileDonkey Stats";
  dT_me_isohunt_stats := _s "Get/Refresh IsoHunt Stats";
  dT_lb_stats_get_main_page := _s "Getting main page for file";
  dT_lb_stats_get_image := _s "Getting graph image for file";
  dT_lb_stats_rate := _s_ "Rate";
  dT_lb_stats_available := _s_ "Availability";
  dT_lb_stats_complete := _s_ "Complete";

(* Friends Tab *)

  fT_lb_friends  :=  _s "Friends";
  fT_lb_messages  :=  _s "Messages";
  fT_wt_find_friend  :=  _s "Find friend";
  fT_lb_name  :=  _s "Name";
  fT_me_find_friend  :=  _s "Find friend";
  fT_me_remove  :=  _s "Remove friend";
  fT_me_remove_all_friends  :=  _s "Clear list";

(* Query Box *)

  qT_lb_directories  :=  _s "Directories";
  qT_lb_results  :=  _s "Results";
  qT_lb_all_networks  :=  _s "All Networks";
  qT_me_download  :=  _s "Download";
  qT_me_force_download  :=  _s "Force Download";
  qT_me_download_directory  :=  _s "Download the selected directory";
  qT_wt_download_selected_dir  :=  _s "Download selected directory";
  qT_lb_confirm_download_dir  :=  message (T.option (T.int (T.string T.format))) "Do you want to download the %d files in the directory %s ?";
  qT_lb_waiting_for_replies  :=  _s_ "Replies expected";
  qT_lb_max_hits  :=  _s_ "Max hits";
  qT_lb_extended_fields  :=  _s "Show extended fields";
  qT_lb_local_search  :=  _s "_Local search";
  qT_lb_submit_search  :=  _s "Su_bmit search";
  qT_lb_subscribe_search  :=  _s "Subscribe searc_h";
  qT_lb_customed_search_editor  :=  _s "Customed search edit_or";
  qT_lb_stop_search  :=  _s "Sto_p search";
  qT_lb_extend_search  :=  _s "E_xtend search";
  qT_lb_album_searches  :=  _s "Album";
  qT_lb_movie_searches  :=  _s "Movie";
  qT_lb_mp3_searches  :=  _s "MP3";
  qT_lb_complex_searches  :=  _s "Complex";
  qT_lb_freedb_searches  :=  _s "FreeDB";
  qT_lb_imdb_searches  :=  _s "IMDB";
  qT_lb_and_not  :=  _s "And not";
  qT_lb_or  :=  _s "Or";
  qT_tx_audio  :=  _s "Audio";
  qT_tx_video  :=  _s "Video";
  qT_tx_program  :=  _s "Program";
  qT_tx_image  :=  _s "Image";
  qT_tx_documentation  :=  _s "Documentation";
  qT_tx_collection  :=  _s "Collection";

  qT_lb_media  :=  _s "Media";
  qT_lb_format  :=  _s "Format";
  qT_lb_min_size  :=  _s "Min size";
  qT_lb_max_size  :=  _s "Max size";
  qT_lb_min_bitrate  :=  _s "Min Bitrate";
  qT_lb_title  :=  _s "Title";
  qT_lb_number_of_results  :=  _s "Number of results";
  qT_lb_sort_by  :=  _s "Sort by";
  qT_lb_album  :=  _s "Album";
  qT_lb_fields  :=  _s "Fields";
  qT_lb_artist  :=  _s "Artist";
  qT_lb_track_title  :=  _s "Track/Title";
  qT_lb_track  :=  _s "Track";
  qT_lb_rest  :=  _s "Rest";
  qT_lb_categories  :=  _s "Categories";
  qT_lb_all  :=  _s "All";
  qT_lb_blues  :=  _s "Blues";
  qT_lb_classical  :=  _s "Classical";
  qT_lb_data  :=  _s "Data";
  qT_lb_folk  :=  _s "Folk";
  qT_lb_rock  :=  _s "Rock";
  qT_lb_soundtrack  :=  _s "Soundtrack";
  qT_lb_availability  :=  _s "Availability";
  qT_lb_size  :=  _s "Size";
  qT_lb_year := _s "Year";
  qT_lb_genre := _s "Genre";
  qT_lb_comment := _s "Comment";
  qT_lb_advanced_search := _s "Advanced search";
  qT_lb_network := _s_ "Network";
  qT_lb_keywords  := _s_ "Keywords";
  qT_lb_min_availability := _s_ "Min Availability";

(* Rooms Tab *)

  rT_lb_chat  :=  _s "Chat";
  rT_lb_rooms  :=  _s "Rooms";
  rT_lb_users  :=  _s "Users";
  rT_lb_paused_room  :=  _s "Paused";
  rT_lb_closed_room  :=  _s "Closed";
  rT_lb_opened_room  :=  _s "Opened";
  rT_me_close_open_room  :=  _s "Close/Open room";
  rT_me_browse_files  :=  _s "Browse files";
  rT_me_add_to_friends  :=  _s "Add to friends";

(* Uploads *)

  uT_lb_uploads  :=  _s "Uploads";
  uT_lb_uploaders  :=  _s "Uploaders";
  uT_me_copy_ed2k  :=  _s "Copy ed2k link";
  uT_lb_show_pending  :=  _s "Show _pending slots";
  uT_tx_friend  :=  _s "Friend";
  uT_tx_contact  :=  _s "Contact";
  uT_tx_normal  :=  _s "Normal";

(* Console *)

  cT_lb_clear_console  :=  _s "Clear C_onsole" ;
  cT_lb_command  :=  _s_ "Command";

(* Graph *)

  gT_lb_time_range := _s_ "Time range";
  gT_lb_quarter := _s "Quarter";
  gT_lb_hour := _s "Hour";
  gT_lb_halfday := _s "Half day";
  gT_lb_day := _s "Day";
  gT_lb_week := _s "Week";
  gT_lb_month := _s "Month";
  gT_lb_year := _s "Year";
  gT_lb_global_downloads := _s "Global Downloads";
  gT_lb_global_uploads := _s "Global Uploads";
  gT_lb_file_down_up := _s "Files Uploads and Downloads";

(* file Window *)

  fW_wt_show_file_details  :=  _s "File details";
  fW_lb_file_info  :=  _s "File information";
  fW_lb_transfer_info  :=  _s "Transfer information";
  fW_lb_avail_info  :=  _s "Availability information";
  fW_lb_file_names  :=  _s "Alternative file names";
  fW_lb_file_hash  :=  _s_ "Hash";
  fW_lb_file_format := _s_ "Format";
  fW_lb_file_size  :=  _s_ "Size";
  fW_lb_file_state  :=  _s_ "State";
  fW_lb_file_chunks  :=  _s_ "Chunks";
  fW_lb_file_avail  :=  _s_ "Availability";
  fW_lb_file_age  :=  _s_ "Age";
  fW_lb_file_prio  :=  _s_ "Priority";
  fW_lb_file_sources  :=  _s_ "Sources";
  fW_lb_file_rate  :=  _s_ "Average rate";
  fW_lb_file_dled  :=  _s_ "Downloaded";
  fW_lb_file_complete_chunks  :=  _s_ "Completed chunks";
  fW_lb_last_seen  :=  _s_ "Last seen";
  fW_lb_file_eta  :=  _s_ "ETA (inst / average)";
  fW_lb_rename  :=  _s "Rename file";
  fW_lb_file_names_col  :=  _s "File names";
  fW_wt_show_source_details  :=  _s "Client details";
  fW_lb_source_info  :=  _s "Client information";
  fW_lb_files_requested  :=  _s "Files requested";
  fW_lb_source_kind  :=  _s_ "Kind";
  fW_lb_source_software  :=  _s_ "Software";
  fW_lb_source_chat_port  :=  _s_ "Chat port";
  fW_lb_source_dlding  :=  _s_ "Currently downloading";
  fW_lb_source_connect_time  :=  _s_ "Connected time";
  fW_lb_source_dled  :=  _s_ "Downloaded";
  fW_lb_source_upled  :=  _s_ "Uploaded";
  fW_lb_source_rating  :=  _s_ "Rating";
  fW_lb_source_hash  :=  _s_ "Client Hash";
  fW_lb_source_average_dled  :=  _s_ "Average download rate";
  fW_lb_source_average_upled  :=  _s_ "Average upload rate";

(* Popup Windows *)

  pW_lb_ok  :=  _s "_Ok";
  pW_lb_apply  :=  _s "_Apply";
  pW_lb_default  :=  _s "_Default";
  pW_lb_cancel  :=  _s "_Cancel";
  pW_lb_close  :=  _s "_Close";
  pW_wt_bad_password  :=  _s "Bad Password";
  pW_lb_bad_password  :=  _s "Authorization Failed\nPlease, click the Settings button -> GUI -> GUI server and enter a valid password" ;

(* Main Window *)

  mW_wt_software  :=  "MLDonkey";
  mW_lb_connecting  :=  _s "Conn_ecting";
  mW_lb_connected  :=  _s "Conn_ected";
  mW_lb_not_connected  :=  _s "Not Conn_ected";
  mW_lb_updload  :=  _s_ "Up";
  mW_lb_download  :=  _s_ "Down";
  mW_lb_shared  :=  _s_ "Files shared";
  mW_lb_filesdownloaded  :=  _s_ "Files downloaded";
  mW_lb_serversconnected  :=  _s_ "Servers connected";
  mW_lb_networks  :=  _s "_Networks";
  mW_lb_servers  :=  _s "Ser_vers";
  mW_lb_downloads  :=  _s "_Downloads";
  mW_lb_friends  :=  _s "_Friends";
  mW_lb_search  :=  _s "_Search";
  mW_lb_rooms  :=  _s "_Rooms";
  mW_lb_uploads  :=  _s "_Uploads";
  mW_lb_console  :=  _s "_Console";
  mW_lb_graph  :=  _s "_Graph";
  mW_lb_settings  :=  _s "Se_ttings";
  mW_lb_initializing  :=  _s "Initializing";
  mW_lb_warning := _s "Warning!";
  mW_me_quit  :=  _s "_Quit";
  mW_me_kill_core  :=  _s "_Kill core";
  mW_me_reconnect  :=  _s "_Reconnect";
  mW_me_disconnect  :=  _s "_Disconnect";
  mW_me_scan_ports  :=  _s "_Scan Ports";
  mW_me_reconnect_to  :=  _s "Reconnect t_o";
  mW_me_settings  :=  !mW_lb_settings;
  mW_me_main_menu  :=  _s "Main me_nu";
  mW_me_restore := _s "R_estore";

(* Settings Window *)

  cW_lb_main  :=  _s "_Main";
  cW_lb_interfaces  :=  _s "_Interfaces";
  cW_lb_tools  :=  _s "_Tools";
  cW_lb_other  :=  _s "_Others";
  cW_lb_bittorrent  :=  _s "_BitTorrent";
  cW_lb_direct_connect  :=  _s "Di_rect Connect";
  cW_lb_donkey  :=  _s "_Donkey";
  cW_lb_fasttrack  :=  _s "_Fasttrack";
  cW_lb_filetp  :=  _s "FileT_P";
  cW_lb_gnutella  :=  _s "_Gnutella";
  cW_lb_open_napster  :=  _s "Op_en Napster";
  cW_lb_soulseek  :=  _s "_Soulseek";
  cW_lb_general  :=  _s "Gener_al";
  cW_lb_bandwidth  :=  _s "Band_width";
  cW_lb_network_config  :=  _s "_Network config";
  cW_lb_security  :=  _s "Securit_y";
  cW_lb_mlgui  :=  _s "MLg_ui";
  cW_lb_html_mods  :=  _s "_HTML mods";
  cW_lb_paths  :=  _s "P_aths";
  cW_lb_startup  :=  _s "Start_up";
  cW_lb_download  :=  _s "Do_wnload";
  cW_lb_mail  :=  _s "M_ail";
  cW_lb_debug  :=  _s "De_bug";
  cW_lb_gnutella1  :=  _s "Gn_utella";
  cW_lb_gnutella2  :=  _s "Gnu_tella2";
  cW_lb_general_  :=  _s "General";
  cW_lb_rates  :=  _s "Rates";
  cW_lb_connections  :=  _s "Connections";
  cW_lb_proxy  :=  _s "Proxy";
  cW_lb_user  :=  _s "User";
  cW_lb_gui  :=  _s "Gui";
  cW_lb_web  :=  _s "Web";
  cW_lb_telnet  :=  _s "Telnet";
  cW_lb_gift  :=  _s "GiFT";
  cW_lb_colors  :=  _s "Colors";
  cW_lb_fonts  :=  _s "Fonts";
  cW_lb_graph  :=  _s "Graph";
  cW_lb_display_conf  :=  _s "Display setup";
  cW_lb_look  :=  _s "Look and feel";
  cW_lb_mail_setup  :=  _s "Mail setup";
  cW_lb_client  :=  _s "Client";
  cW_lb_server  :=  _s "Server";
  cW_lb_connection_param  :=  _s "Connection parameters";
  cW_lb_uploads  :=  _s "Uploads";
  cW_lb_downloads  :=  _s "Downloads";
  cW_lb_sources  :=  _s "Sources";
  cW_lb_peers  :=  _s "Peers";
  cW_lb_servers  :=  _s "Servers";
  cW_lb_overnet  :=  _s "Overnet : Specific options";
  cW_lb_tracker  :=  _s "Tracker";
  cW_lb_other_  :=  _s "Others";
  cW_lb_browse  :=  _s "Browse...";
  cW_lb_font_sample  :=  _s "Font sample";
  cW_lb_day  :=  _s "d";
  cW_lb_hour  :=  _s "h";
  cW_lb_minute  :=  _s "min";
  cW_lb_second  :=  _s "s";

(* Im Window *)

  iM_me_default  :=  _s "Default";
  iM_me_arabic  :=  _s "Arabic";
  iM_me_armenian  :=  _s "Armenian";
  iM_me_baltic  :=  _s "Baltic";
  iM_me_celtic  :=  _s "Celtic";
  iM_me_centraleuropean  :=  _s "Central European";
  iM_me_chinesesimplified  :=  _s "Chinese simplified";
  iM_me_chinesetraditional  :=  _s "Chineses traditional";
  iM_me_cyrillic  :=  _s "Cyrillic";
  iM_me_georgian  :=  _s "Georgian";
  iM_me_greek  :=  _s "Greek";
  iM_me_hebrew  :=  _s "Hebrew";
  iM_me_japanese  :=  _s "Japanese";
  iM_me_korean  :=  _s "Korean";
  iM_me_nordic  :=  _s "Nordic";
  iM_me_romanian  :=  _s "Romanian";
  iM_me_southeuropean  :=  _s "South European";
  iM_me_tajik  :=  _s "Tajik";
  iM_me_thai  :=  _s "Thai";
  iM_me_turkish  :=  _s "Turkish";
  iM_me_unicode  :=  _s "Universal";
  iM_me_vietnamese  :=  _s "Vietnamese";
  iM_me_westerneuropean  :=  _s "Western European";
  iM_me_auto_detect  :=  _s "Auto detect";
  iM_me_character_coding  :=  _s "Character coding";

  iM_wt_software := "MLdonkey : Instant Messenger";
  iM_lb_title := _s "IM Window";
  iM_me_new_accounts := _s "New Accounts";
  iM_me_x_account  := (message (T.option (T.string T.format))  "New %s account");
  iM_me_menu := _s "_Menu";
  iM_tx_online := _s "Online";
  iM_tx_online_away := _s "Away";
  iM_tx_connecting := _s "Connecting";
  iM_tx_offline := _s "Offline";
  iM_me_connect_disconnect := _s "Connect/Disconnect";
  iM_me_settings := _s "Settings";
  iM_me_quit := _s "Quit";
  iM_me_remove := _s "Remove";
  iM_me_join_room := _s "Join Room";
  iM_me_preferred_rooms := _s "Preferred Rooms";
  iM_lb_new_account := _s "New Account";
  im_lb_room_name := _s "Room Name";
  iM_lb_join_room := _s "Join Room";
  iM_lb_close := _s "_Close";
  iM_lb_accounts := _s "Accounts";

(* Common Menus labels *)

  mAutosize  :=  _s "Autosize";
  mAutosize_all  :=  _s "Autosize All Columns";
  mSort  :=  _s "Sort";
  mRemove_column  :=  _s "Remove Column";
  mAdd_column_after  :=  _s "Add Column After";
  mAdd_column_before  :=  _s "Add Column Before";

(* Columns names *)
  c_name  :=  _s "Name";
  c_md4  :=  _s "MD4";
  c_uid  :=  _s "Uid";
  c_size  :=  _s "Size";
  c_downloaded  :=  _s "Downloaded";
  c_percent  :=  _s "%";
  c_state  :=  _s "State";
  c_avail  :=  _s "Availability";
  c_rate  :=  _s "Rate";
  c_format  :=  _s "Format";
  c_network  :=  _s "Net";
  c_age  :=  _s "Age";
  c_last_seen  :=  _s "Last seen";
  c_comment  :=  _s "Comment";
  c_eta  :=  _s "ETA";
  c_priority  :=  _s "Priority";
  c_kind  :=  _s "Kind";
  c_client_type  :=  _s "Type";
  c_client_rating  :=  _s "Rating";
  c_client_software  :=  _s "Brand";
  c_client_emulemod  :=  _s "Mod";
  c_client_downloaded  :=  _s "Downloaded";
  c_client_uploaded  :=  _s "Uploaded";
  c_client_download_rate  :=  _s "Download rate";
  c_client_upload_rate  :=  _s "Upload rate";
  c_client_upload  :=  _s "File uploaded";
  c_client_sock_addr  :=  _s "IP Address";
  c_client_connect_time  :=  _s "Connected Time";
  c_address  :=  _s "Address";
  c_server_nusers  :=  _s "Users";
  c_server_nfiles  :=  _s "Files";
  c_server_desc  :=  _s "Description";
  c_duration  :=  _s "Duration";
  c_codec  :=  _s "Codec";
  c_bitrate   :=  _s "Bitrate";
  c_comment  :=  _s "Comment";
  c_nusers  :=  _s "Users";
  c_filename  :=  _s "Filename";
  c_uploaded  :=  _s "Uploaded";
  c_requests  :=  _s "Requests";
  c_tags  :=  _s "Tags";
  c_type  :=  _s "Type";
  c_folders  :=  _s "Folder name";
  c_sources  :=  _s "Sources";
  c_complete  :=  _s "Complete";
  c_status := _s "Status";
  c_protocol := _s "Protocol";
  c_preferred := _s "Pref."

(* Messages and string constants. *)

let _ = Unix2.safe_mkdir gui_config_dir

let chat_config_file = Filename.concat gui_config_dir "chat.ini"

(* {2 Command line messages} *)

let usage = Sys.argv.(0)^" [options] <files>\n"
let options_are = "Options are :"

(* {Icons names} *)

let icon_splash_screen = "splash_screen"
let icon_menu_networks = "menu_networks"
let icon_menu_servers = "menu_servers"
let icon_menu_downloads = "menu_downloads"
let icon_menu_friends = "menu_friends"
let icon_menu_searches = "menu_searches"
let icon_menu_rooms = "menu_rooms"
let icon_menu_uploads = "menu_uploads"
let icon_menu_console = "menu_console"
let icon_menu_graph = "menu_graph"
let icon_menu_settings = "menu_settings"
let icon_menu_quit = "menu_quit"
let icon_menu_help = "menu_help"
let icon_menu_core = "menu_core"
let icon_menu_core_reconnect = "menu_core_reconnect"
let icon_menu_core_connectto = "menu_core_connectto"
let icon_menu_core_scanports = "menu_core_scanports"
let icon_menu_core_disconnect = "menu_core_disconnect"
let icon_menu_core_kill = "menu_core_kill"
let icon_menu_search_album = "menu_search_album"
let icon_menu_search_movie = "menu_search_movie"
let icon_menu_search_mp3 = "menu_search_mp3"
let icon_menu_search_complex = "menu_search_complex"
let icon_menu_search_freedb = "menu_search_freedb"
let icon_menu_search_imdb = "menu_search_imdb"
let icon_menu_interfaces = "menu_interfaces"
let icon_menu_tools = "menu_tools"
let icon_menu_others = "menu_others"
let icon_net_bittorrent = "net_bittorrent"
let icon_net_dc = "net_dc"
let icon_net_ed2k = "net_ed2k"
let icon_net_fasttrack = "net_fasttrack"
let icon_net_filetp = "net_filetp"
let icon_net_gnutella1 = "net_gnutella1"
let icon_net_gnutella2 = "net_gnutella2"
let icon_net_napster = "net_napster"
let icon_net_soulseek = "net_soulseek"
let icon_net_globalshare = "net_globalshare"
let icon_net_supernode = "net_supernode"
let icon_net_multinet = "net_multinet"
let icon_stock_shared_directory = "stock_shared_directory"
let icon_stock_directory = "stock_directory"
let icon_stock_directory_open = "stock_directory_open"
let icon_stock_color = "stock_color"
let icon_stock_font = "stock_font"
let icon_stock_info = "stock_info"
let icon_stock_password = "stock_password"
let icon_stock_download_directory = "stock_download_directory"
let icon_stock_pending_slots = "stock_pending_slots"
let icon_stock_close = "stock_close"
let icon_stock_close_overlay = "stock_close_overlay"
let icon_stock_stop = "stock_stop"
let icon_stock_ok = "stock_ok"
let icon_stock_all_servers = "stock_all_servers"
let icon_stock_add_server = "stock_add_server"
let icon_stock_subscribe_search = "stock_subscribe_search"
let icon_stock_submit_search = "stock_submit_search"
let icon_stock_extend_search = "stock_extend_search"
let icon_stock_local_search = "stock_local_search"
let icon_stock_warning = "stock_warning"
let icon_type_source_contact = "type_source_contact"
let icon_type_source_friend = "type_source_friend"
let icon_type_source_normal = "type_source_normal"
let icon_state_server_conh = "state_server_conh"
let icon_state_server_conl = "state_server_conl"
let icon_state_server_init = "state_server_init"
let icon_state_server_notcon = "state_server_notcon"
let icon_state_server_unknown = "state_server_unknown"
let icon_state_source_fileslisted = "state_source_fileslisted"
let icon_state_down = "state_down"
let icon_state_up = "state_up"
let icon_state_updown = "state_updown"
let icon_state_notupdown = "state_notupdown"
let icon_mime_unknown = "mime_unknown"
let icon_mime_images = "mime_images"
let icon_mime_binary = "mime_binary"
let icon_mime_cdimage = "mime_cdimage"
let icon_mime_debian = "mime_debian"
let icon_mime_html = "mime_html"
let icon_mime_java = "mime_java"
let icon_mime_pdf = "mime_pdf"
let icon_mime_postscript = "mime_postscript"
let icon_mime_real = "mime_real"
let icon_mime_recycled = "mime_recycled"
let icon_mime_rpm = "mime_rpm"
let icon_mime_shellscript = "mime_shellscript"
let icon_mime_soffice = "mime_soffice"
let icon_mime_sound = "mime_sound"
let icon_mime_source = "mime_source"
let icon_mime_spreadsheet = "mime_spreadsheet"
let icon_mime_tex = "mime_tex"
let icon_mime_text = "mime_text"
let icon_mime_tgz = "mime_tgz"
let icon_mime_video = "mime_video"
let icon_mime_wordprocessing = "mime_wordprocessing"
let icon_tree_closed = "tree_closed"
let icon_tree_opened = "tree_opened"

let icon_emoticon_airplane = "emoticon_airplane"
let icon_emoticon_angel = "emoticon_angel"
let icon_emoticon_arrogant = "emoticon_arrogant"
let icon_emoticon_asl = "emoticon_asl"
let icon_emoticon_bad = "emoticon_bad"
let icon_emoticon_baringteeth = "emoticon_baringteeth"
let icon_emoticon_bat = "emoticon_bat"
let icon_emoticon_beer = "emoticon_beer"
let icon_emoticon_bowl = "emoticon_bowl"
let icon_emoticon_boy = "emoticon_boy"
let icon_emoticon_cake = "emoticon_cake"
let icon_emoticon_cat = "emoticon_cat"
let icon_emoticon_cigaret = "emoticon_cigaret"
let icon_emoticon_clock = "emoticon_clock"
let icon_emoticon_confused = "emoticon_confused"
let icon_emoticon_cry = "emoticon_cry"
let icon_emoticon_cup = "emoticon_cup"
let icon_emoticon_devil = "emoticon_devil"
let icon_emoticon_dog = "emoticon_dog"
let icon_emoticon_dude_hug = "emoticon_dude_hug"
let icon_emoticon_dunno = "emoticon_dunno"
let icon_emoticon_embarrassed = "emoticon_embarrassed"
let icon_emoticon_envelope = "emoticon_envelope"
let icon_emoticon_eyeroll = "emoticon_eyeroll"
let icon_emoticon_film = "emoticon_film"
let icon_emoticon_girl = "emoticon_girl"
let icon_emoticon_girl_hug = "emoticon_girl_hug"
let icon_emoticon_ip = "emoticon_ip"
let icon_emoticon_kiss = "emoticon_kiss"
let icon_emoticon_lightning = "emoticon_lightning"
let icon_emoticon_love = "emoticon_love"
let icon_emoticon_megasmile = "emoticon_megasmile"
let icon_emoticon_moon = "emoticon_moon"
let icon_emoticon_nerd = "emoticon_nerd"
let icon_emoticon_omg = "emoticon_omg"
let icon_emoticon_party = "emoticon_party"
let icon_emoticon_pizza = "emoticon_piza"
let icon_emoticon_plate = "emoticon_plate"
let icon_emoticon_present = "emoticon_present"
let icon_emoticon_rainbow = "emoticon_rainbow"
let icon_emoticon_sad = "emoticon_sad"
let icon_emoticon_sarcastic = "emoticon_sarcastic"
let icon_emoticon_secret = "emoticon_secret"
let icon_emoticon_shade = "emoticon_shade"
let icon_emoticon_sick = "emoticon_sick"
let icon_emoticon_sleepy = "emoticon_sleepy"
let icon_emoticon_sorry = "emoticon_sorry"
let icon_emoticon_sshh = "emoticon_sshh"
let icon_emoticon_storm = "emoticon_storm"
let icon_emoticon_sun = "emoticon_sun"
let icon_emoticon_teeth = "emoticon_teeth"
let icon_emoticon_thumbs_down = "emoticon_thumbs_down"
let icon_emoticon_thumbs_up = "emoticon_thumbs_up"
let icon_emoticon_tongue = "emoticon_tongue"
let icon_emoticon_ugly = "emoticon_ugly"
let icon_emoticon_ulove = "emoticon_ulove"
let icon_emoticon_wink = "emoticon_wink"


(* {2 Help text, change log} *)
    
let help_string version = Printf.sprintf "

                      MLDonkey
                      ========

Release: %s
Authors: MLDonkey project, http://mldonkey.sf.net/

This documentation file is now obsolete. Read the FAQ instead (either
on the project WEB site or in the FAQ.html file).

For help, see:

IRC channel: irc.freenode.net, chat #mldonkey

Web sites:
http://mldonkey.sf.net          Official site

Mailing-lists:
mldonkey-users@nongnu.org
Archives: http://mail.nongnu.org/mailman/listinfo/mldonkey-users

In the console, use '?' for help on commands.
" version



let _ =
  load_messages ()
