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

(* Icons of the GUI *)


module O = GuiOptions
module M = GuiMessages
module VB = VerificationBitmap

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let verbose = O.gtk_verbose_art

let lprintf' fmt =
  Printf2.lprintf ("GuiArt :" ^^ fmt)

(**********************************************************************************)
(*                                                                                *)
(*                          types                                                 *)
(*                                                                                *)
(**********************************************************************************)

type icon_size =
  SMALL
| MEDIUM
| LARGE

(**********************************************************************************)
(*                                                                                *)
(*                          from_icons_dir                                        *)
(*                                                                                *)
(**********************************************************************************)

(* TODO :
     - Allow to load an icon set from a given directory
     - Allow all formats possible : png, gif, jpeg, ico, etc...
     - Maybe use an index.theme file like gnome to indicate the
       directories available (16x16, 32x32, 48x48, etc...) to 
       allow different sizes to be displayed in a pretty manner.
*)

let from_icons_dir icon_name = ""

(**********************************************************************************)
(*                                                                                *)
(*                          Icons ressources of the GUI                           *)
(*                                                                                *)
(**********************************************************************************)

let table = [
  M.icon_splash_screen, (Splash_screen_svg.t, from_icons_dir M.icon_splash_screen);

  M.icon_menu_networks, (Menu_networks_svg.t, from_icons_dir M.icon_menu_networks);
  M.icon_menu_servers, (Menu_servers_svg.t, from_icons_dir M.icon_menu_servers);
  M.icon_menu_downloads, (Menu_downloads_svg.t, from_icons_dir M.icon_menu_downloads);
  M.icon_menu_friends, (Menu_friends_svg.t, from_icons_dir M.icon_menu_friends);
  M.icon_menu_searches, (Menu_searches_svg.t, from_icons_dir M.icon_menu_searches);
  M.icon_menu_rooms, (Menu_rooms_svg.t, from_icons_dir M.icon_menu_rooms);
  M.icon_menu_uploads, (Menu_uploads_svg.t, from_icons_dir M.icon_menu_uploads);
  M.icon_menu_console, (Menu_console_svg.t, from_icons_dir M.icon_menu_console);
  M.icon_menu_graph, (Menu_graph_svg.t, from_icons_dir M.icon_menu_graph);
  M.icon_menu_settings, (Menu_settings_svg.t, from_icons_dir M.icon_menu_settings);
  M.icon_menu_quit, (Menu_quit_svg.t, from_icons_dir M.icon_menu_quit);
  M.icon_menu_help, (Menu_help_svg.t, from_icons_dir M.icon_menu_help);
  M.icon_menu_core, (Menu_core_svg.t, from_icons_dir M.icon_menu_core);
  M.icon_menu_core_reconnect, (Menu_core_reconnect_svg.t, from_icons_dir M.icon_menu_core_reconnect);
  M.icon_menu_core_connectto, (Menu_core_connectto_svg.t, from_icons_dir M.icon_menu_core_connectto);
  M.icon_menu_core_scanports, (Menu_core_scanports_svg.t, from_icons_dir M.icon_menu_core_scanports);
  M.icon_menu_core_disconnect, (Menu_core_disconnect_svg.t, from_icons_dir M.icon_menu_core_disconnect);
  M.icon_menu_core_kill, (Menu_core_kill_svg.t, from_icons_dir M.icon_menu_core_kill);
  M.icon_menu_search_album, (Menu_search_album_svg.t, from_icons_dir M.icon_menu_search_album);
  M.icon_menu_search_movie, (Menu_search_movie_svg.t, from_icons_dir M.icon_menu_search_movie);
  M.icon_menu_search_mp3, (Menu_search_mp3_svg.t, from_icons_dir M.icon_menu_search_mp3);
  M.icon_menu_search_complex, (Menu_search_complex_svg.t, from_icons_dir M.icon_menu_search_complex);
  M.icon_menu_search_freedb, (Menu_search_freedb_svg.t, from_icons_dir M.icon_menu_search_freedb);
  M.icon_menu_search_imdb, (Menu_search_imdb_svg.t, from_icons_dir M.icon_menu_search_imdb);
  M.icon_menu_interfaces, (Menu_interfaces_svg.t, from_icons_dir M.icon_menu_interfaces);
  M.icon_menu_tools, (Menu_tools_svg.t, from_icons_dir M.icon_menu_tools);
  M.icon_menu_others, (Menu_others_svg.t, from_icons_dir M.icon_menu_others);

  M.icon_net_bittorrent, (Net_bittorrent_svg.t, from_icons_dir M.icon_net_bittorrent);
  M.icon_net_dc, (Net_dc_svg.t, from_icons_dir M.icon_net_dc);
  M.icon_net_ed2k, (Net_ed2k_svg.t, from_icons_dir M.icon_net_ed2k);
  M.icon_net_fasttrack, (Net_fasttrack_svg.t, from_icons_dir M.icon_net_fasttrack);
  M.icon_net_filetp, (Net_filetp_svg.t, from_icons_dir M.icon_net_filetp);
  M.icon_net_gnutella1, (Net_gnutella1_svg.t, from_icons_dir M.icon_net_gnutella1);
  M.icon_net_gnutella2, (Net_gnutella2_svg.t, from_icons_dir M.icon_net_gnutella2);
  M.icon_net_napster, (Net_napster_svg.t, from_icons_dir M.icon_net_napster);
  M.icon_net_soulseek, (Net_soulseek_svg.t, from_icons_dir M.icon_net_soulseek);
  M.icon_net_globalshare, (Net_globalshare_svg.t, from_icons_dir M.icon_net_globalshare);
  M.icon_net_supernode, (Net_supernode_svg.t, from_icons_dir M.icon_net_supernode);
  M.icon_net_multinet, (Net_multinet_svg.t, from_icons_dir M.icon_net_multinet);

  M.icon_stock_shared_directory, (Stock_shared_directory_svg.t, from_icons_dir M.icon_stock_shared_directory);
  M.icon_stock_directory, (Stock_directory_svg.t, from_icons_dir M.icon_stock_directory);
  M.icon_stock_directory_open, (Stock_directory_open_svg.t, from_icons_dir M.icon_stock_directory_open);
  M.icon_stock_color, (Stock_color_svg.t, from_icons_dir M.icon_stock_color);
  M.icon_stock_font, (Stock_font_svg.t, from_icons_dir M.icon_stock_font);
  M.icon_stock_info, (Stock_info_svg.t, from_icons_dir M.icon_stock_info);
  M.icon_stock_password, (Stock_password_svg.t, from_icons_dir M.icon_stock_password);
  M.icon_stock_download_directory, (Stock_download_directory_svg.t, from_icons_dir M.icon_stock_download_directory);
  M.icon_stock_pending_slots, (Stock_pending_slots_svg.t, from_icons_dir M.icon_stock_pending_slots);
  M.icon_stock_close, (Stock_close_svg.t, from_icons_dir M.icon_stock_close);
  M.icon_stock_close_overlay, (Stock_close_overlay_svg.t, from_icons_dir M.icon_stock_close_overlay);
  M.icon_stock_stop, (Stock_stop_svg.t, from_icons_dir M.icon_stock_stop);
  M.icon_stock_ok, (Stock_ok_svg.t, from_icons_dir M.icon_stock_ok);
  M.icon_stock_all_servers, (Stock_all_servers_svg.t, from_icons_dir M.icon_stock_all_servers);
  M.icon_stock_add_server, (Stock_add_server_svg.t, from_icons_dir M.icon_stock_add_server);
  M.icon_stock_subscribe_search, (Stock_subscribe_search_svg.t, from_icons_dir M.icon_stock_subscribe_search);
  M.icon_stock_submit_search, (Stock_submit_search_svg.t, from_icons_dir M.icon_stock_submit_search);
  M.icon_stock_extend_search, (Stock_extend_search_svg.t, from_icons_dir M.icon_stock_extend_search);
  M.icon_stock_local_search, (Stock_local_search_svg.t, from_icons_dir M.icon_stock_local_search);
  M.icon_stock_warning, (Stock_warning_svg.t, from_icons_dir M.icon_stock_warning);

  M.icon_type_source_contact, (Type_source_contact_svg.t, from_icons_dir M.icon_type_source_contact);
  M.icon_type_source_friend, (Type_source_friend_svg.t, from_icons_dir M.icon_type_source_friend);
  M.icon_type_source_normal, (Type_source_normal_svg.t, from_icons_dir M.icon_type_source_normal);

  M.icon_state_server_conh, (State_server_conh_svg.t, from_icons_dir M.icon_state_server_conh);
  M.icon_state_server_conl, (State_server_conl_svg.t, from_icons_dir M.icon_state_server_conl);
  M.icon_state_server_init, (State_server_init_svg.t, from_icons_dir M.icon_state_server_init);
  M.icon_state_server_notcon, (State_server_notcon_svg.t, from_icons_dir M.icon_state_server_notcon);
  M.icon_state_server_unknown, (State_server_unknown_svg.t, from_icons_dir M.icon_state_server_unknown);

  M.icon_state_source_fileslisted, (State_source_fileslisted_svg.t, from_icons_dir M.icon_state_source_fileslisted);

  M.icon_state_up, (State_up_svg.t, from_icons_dir M.icon_state_up);
  M.icon_state_updown, (State_updown_svg.t, from_icons_dir M.icon_state_updown);
  M.icon_state_notupdown, (State_notupdown_svg.t, from_icons_dir M.icon_state_notupdown);
  M.icon_state_down, (State_down_svg.t, from_icons_dir M.icon_state_down);

  M.icon_mime_unknown, (Mime_unknown_svg.t, from_icons_dir M.icon_mime_unknown);
  M.icon_mime_images, (Mime_images_svg.t, from_icons_dir M.icon_mime_images);
  M.icon_mime_binary, (Mime_binary_svg.t, from_icons_dir M.icon_mime_binary);
  M.icon_mime_cdimage, (Mime_cdimage_svg.t, from_icons_dir M.icon_mime_cdimage);
  M.icon_mime_debian, (Mime_debian_svg.t, from_icons_dir M.icon_mime_debian);
  M.icon_mime_html, (Mime_html_svg.t, from_icons_dir M.icon_mime_html);
  M.icon_mime_java, (Mime_java_svg.t, from_icons_dir M.icon_mime_java);
  M.icon_mime_pdf, (Mime_pdf_svg.t, from_icons_dir M.icon_mime_pdf);
  M.icon_mime_postscript, (Mime_postscript_svg.t, from_icons_dir M.icon_mime_postscript);
  M.icon_mime_real, (Mime_real_svg.t, from_icons_dir M.icon_mime_real);
  M.icon_mime_recycled, (Mime_recycled_svg.t, from_icons_dir M.icon_mime_recycled);
  M.icon_mime_rpm, (Mime_rpm_svg.t, from_icons_dir M.icon_mime_rpm);
  M.icon_mime_shellscript, (Mime_shellscript_svg.t, from_icons_dir M.icon_mime_shellscript);
  M.icon_mime_soffice, (Mime_soffice_svg.t, from_icons_dir M.icon_mime_soffice);
  M.icon_mime_sound, (Mime_sound_svg.t, from_icons_dir M.icon_mime_sound);
  M.icon_mime_source, (Mime_source_svg.t, from_icons_dir M.icon_mime_source);
  M.icon_mime_spreadsheet, (Mime_spreadsheet_svg.t, from_icons_dir M.icon_mime_spreadsheet);
  M.icon_mime_tex, (Mime_tex_svg.t, from_icons_dir M.icon_mime_tex);
  M.icon_mime_text, (Mime_text_svg.t, from_icons_dir M.icon_mime_text);
  M.icon_mime_tgz, (Mime_tgz_svg.t, from_icons_dir M.icon_mime_tgz);
  M.icon_mime_video, (Mime_video_svg.t, from_icons_dir M.icon_mime_video);
  M.icon_mime_wordprocessing, (Mime_wordprocessing_svg.t, from_icons_dir M.icon_mime_wordprocessing);

  M.icon_emoticon_airplane, (Emoticon_airplane_svg.t, from_icons_dir M.icon_emoticon_airplane);
  M.icon_emoticon_angel, (Emoticon_angel_svg.t, from_icons_dir M.icon_emoticon_angel);
  M.icon_emoticon_arrogant, (Emoticon_arrogant_svg.t, from_icons_dir M.icon_emoticon_arrogant);
  M.icon_emoticon_asl, (Emoticon_asl_svg.t, from_icons_dir M.icon_emoticon_asl);
  M.icon_emoticon_bad, (Emoticon_bad_svg.t, from_icons_dir M.icon_emoticon_bad);
  M.icon_emoticon_baringteeth, (Emoticon_baringteeth_svg.t, from_icons_dir M.icon_emoticon_baringteeth);
  M.icon_emoticon_bat, (Emoticon_bat_svg.t, from_icons_dir M.icon_emoticon_bat);
  M.icon_emoticon_beer, (Emoticon_beer_svg.t, from_icons_dir M.icon_emoticon_beer);
  M.icon_emoticon_bowl, (Emoticon_bowl_svg.t, from_icons_dir M.icon_emoticon_bowl);
  M.icon_emoticon_boy, (Emoticon_boy_svg.t, from_icons_dir M.icon_emoticon_boy);
  M.icon_emoticon_cake, (Emoticon_cake_svg.t, from_icons_dir M.icon_emoticon_cake);
  M.icon_emoticon_cat, (Emoticon_cat_svg.t, from_icons_dir M.icon_emoticon_cat);
  M.icon_emoticon_cigaret, (Emoticon_cigaret_svg.t, from_icons_dir M.icon_emoticon_cigaret);
  M.icon_emoticon_clock, (Emoticon_clock_svg.t, from_icons_dir M.icon_emoticon_clock);
  M.icon_emoticon_confused, (Emoticon_confused_svg.t, from_icons_dir M.icon_emoticon_confused);
  M.icon_emoticon_cry, (Emoticon_cry_svg.t, from_icons_dir M.icon_emoticon_cry);
  M.icon_emoticon_cup, (Emoticon_cup_svg.t, from_icons_dir M.icon_emoticon_cup);
  M.icon_emoticon_devil, (Emoticon_devil_svg.t, from_icons_dir M.icon_emoticon_devil);
  M.icon_emoticon_dog, (Emoticon_dog_svg.t, from_icons_dir M.icon_emoticon_dog);
  M.icon_emoticon_dude_hug, (Emoticon_dude_hug_svg.t, from_icons_dir M.icon_emoticon_dude_hug);
  M.icon_emoticon_dunno, (Emoticon_dunno_svg.t, from_icons_dir M.icon_emoticon_dunno);
  M.icon_emoticon_embarrassed, (Emoticon_embarrassed_svg.t, from_icons_dir M.icon_emoticon_embarrassed);
  M.icon_emoticon_envelope, (Emoticon_envelope_svg.t, from_icons_dir M.icon_emoticon_envelope);
  M.icon_emoticon_eyeroll, (Emoticon_eyeroll_svg.t, from_icons_dir M.icon_emoticon_eyeroll);
  M.icon_emoticon_film, (Emoticon_film_svg.t, from_icons_dir M.icon_emoticon_film);
  M.icon_emoticon_girl, (Emoticon_girl_svg.t, from_icons_dir M.icon_emoticon_girl);
  M.icon_emoticon_girl_hug, (Emoticon_girl_hug_svg.t, from_icons_dir M.icon_emoticon_girl_hug);
  M.icon_emoticon_ip, (Emoticon_ip_svg.t, from_icons_dir M.icon_emoticon_ip);
  M.icon_emoticon_kiss, (Emoticon_kiss_svg.t, from_icons_dir M.icon_emoticon_kiss);
  M.icon_emoticon_lightning, (Emoticon_lightning_svg.t, from_icons_dir M.icon_emoticon_lightning);
  M.icon_emoticon_love, (Emoticon_love_svg.t, from_icons_dir M.icon_emoticon_love);
  M.icon_emoticon_megasmile, (Emoticon_megasmile_svg.t, from_icons_dir M.icon_emoticon_megasmile);
  M.icon_emoticon_moon, (Emoticon_moon_svg.t, from_icons_dir M.icon_emoticon_moon);
  M.icon_emoticon_nerd, (Emoticon_nerd_svg.t, from_icons_dir M.icon_emoticon_nerd);
  M.icon_emoticon_omg, (Emoticon_omg_svg.t, from_icons_dir M.icon_emoticon_omg);
  M.icon_emoticon_party, (Emoticon_party_svg.t, from_icons_dir M.icon_emoticon_party);
  M.icon_emoticon_pizza, (Emoticon_pizza_svg.t, from_icons_dir M.icon_emoticon_pizza);
  M.icon_emoticon_plate, (Emoticon_plate_svg.t, from_icons_dir M.icon_emoticon_plate);
  M.icon_emoticon_present, (Emoticon_present_svg.t, from_icons_dir M.icon_emoticon_present);
  M.icon_emoticon_rainbow, (Emoticon_rainbow_svg.t, from_icons_dir M.icon_emoticon_rainbow);
  M.icon_emoticon_sad, (Emoticon_sad_svg.t, from_icons_dir M.icon_emoticon_sad);
  M.icon_emoticon_sarcastic, (Emoticon_sarcastic_svg.t, from_icons_dir M.icon_emoticon_sarcastic);
  M.icon_emoticon_secret, (Emoticon_secret_svg.t, from_icons_dir M.icon_emoticon_secret);
  M.icon_emoticon_shade, (Emoticon_shade_svg.t, from_icons_dir M.icon_emoticon_shade);
  M.icon_emoticon_sick, (Emoticon_sick_svg.t, from_icons_dir M.icon_emoticon_sick);
  M.icon_emoticon_sleepy, (Emoticon_sleepy_svg.t, from_icons_dir M.icon_emoticon_sleepy);
  M.icon_emoticon_sshh, (Emoticon_sshh_svg.t, from_icons_dir M.icon_emoticon_sshh);
  M.icon_emoticon_storm, (Emoticon_storm_svg.t, from_icons_dir M.icon_emoticon_storm);
  M.icon_emoticon_sun, (Emoticon_sun_svg.t, from_icons_dir M.icon_emoticon_sun);
  M.icon_emoticon_teeth, (Emoticon_teeth_svg.t, from_icons_dir M.icon_emoticon_teeth);
  M.icon_emoticon_thumbs_down, (Emoticon_thumbs_down_svg.t, from_icons_dir M.icon_emoticon_thumbs_down);
  M.icon_emoticon_thumbs_up, (Emoticon_thumbs_up_svg.t, from_icons_dir M.icon_emoticon_thumbs_up);
  M.icon_emoticon_tongue, (Emoticon_tongue_svg.t, from_icons_dir M.icon_emoticon_tongue);
  M.icon_emoticon_ugly, (Emoticon_ugly_svg.t, from_icons_dir M.icon_emoticon_ugly);
  M.icon_emoticon_ulove, (Emoticon_ulove_svg.t, from_icons_dir M.icon_emoticon_ulove);
  M.icon_emoticon_wink, (Emoticon_wink_svg.t, from_icons_dir M.icon_emoticon_wink);

]

(**********************************************************************************)
(*                                                                                *)
(*                            Loading functions                                   *)
(*                                                                                *)
(**********************************************************************************)

open Zlib

(* Convenient function to load an icon set to customize the GUI *)

(* Return a pixbuf for a given svg data *)
let pixb icon_name pixel_size =
  let svg = uncompress_string icon_name in
  let z = float_of_int pixel_size /. 48. in
  let size_cb = (Rsvg.at_zoom z z) in
  let pb = Rsvg.render_from_string ~size_cb svg in
  GdkPixbuf.saturate_and_pixelate ~dest:pb ~saturation:!!O.gtk_look_icon_saturation ~pixelate:false pb;
  pb

(* function to desaturate icons *)
let saturate pb desat =
  if desat then
    GdkPixbuf.saturate_and_pixelate 
        ~dest:pb 
        ~saturation:0. 
        ~pixelate:true 
        pb;
  pb



let pix_buf icon_name pixel_size ?(desat=false) () =
  try
    let (default, o) = List.assoc icon_name table in
    match o with
      "" -> 
        let pb = pixb default pixel_size in
        saturate pb desat
    | f ->
        try
          let pb = GdkPixbuf.from_file f in
          saturate pb desat
        with
          _ ->
            let pb = pixb default pixel_size in
            saturate pb desat
  with
    Not_found ->
      begin
        (if !!verbose then lprintf' "icon %s not found\n" icon_name);
        let pb = pixb Mime_unknown_svg.t pixel_size in
        saturate pb desat
      end

(**********************************************************************************)
(*                                                                                *)
(*                          get_icon                                              *)
(*                                                                                *)
(**********************************************************************************)


let (small_icons : ((string * int *  bool), GdkPixbuf.pixbuf) Hashtbl.t) = Hashtbl.create 103
let (medium_icons : ((string * int *  bool), GdkPixbuf.pixbuf) Hashtbl.t) = Hashtbl.create 103
let (large_icons : ((string * int *  bool), GdkPixbuf.pixbuf) Hashtbl.t) = Hashtbl.create 23

let find_icon table key =
  try
    let pixb = Hashtbl.find table key in
    pixb
  with _ ->
    begin
      let (icon, s, desat) = key in
      (if !!verbose then lprintf' "icon name: %s size: %d desat:%b doesn't exist in Hashtbl\n" icon s desat);
      let pixb = pix_buf icon s ~desat () in
      Hashtbl.add table key pixb;
      pixb
    end

let get_icon ~(icon : string) ~(size : icon_size) ?(desat=false) () =
  match size with
      SMALL -> 
        begin
          let s = !!O.gtk_look_lists_icon_size in
          let key =  (icon, s, desat) in
          find_icon small_icons key
        end

    | MEDIUM -> 
        begin
          let s = !!O.gtk_look_toolbars_icon_size in
          let key =  (icon, s, desat) in
          find_icon medium_icons key
        end

    | LARGE ->
        begin
          let s = !!O.gtk_look_main_toolbar_icon_size in
          let key =  (icon, s, desat) in
          find_icon large_icons key
        end

(**********************************************************************************)
(*                                                                                *)
(*                          clean_icons                                           *)
(*                                                                                *)
(**********************************************************************************)

let clean_icons () =
  (if !!verbose then lprintf' "clean icons\n");
  Hashtbl.clear small_icons;
  Hashtbl.clear medium_icons;
  Hashtbl.clear large_icons

(**********************************************************************************)
(*                                                                                *)
(*                          avail_bars                                            *)
(*                                                                                *)
(**********************************************************************************)

let (avail_bars : ((string * VB.t option * bool), GdkPixbuf.pixbuf) Hashtbl.t) = Hashtbl.create 103

(* make a simple function to give a 3D effect *)
let highlight range i =
  if i < 8
    then (2 * i * range / 16)
    else (range - (i * range / 2 / 16))

(* define the colors we will use to display the availability *)
let color_red = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true ()
let _ =
  let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
  for i = 0 to 15 do
    let r = highlight 255 i in
    let s =  Printf.sprintf "0x%02X0000ff" r in
    GdkPixbuf.fill pixb (Int32.of_string s);
    GdkPixbuf.copy_area ~dest:color_red ~dest_x:0 ~dest_y:i ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
  done

let color_green = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true ()
let _ =
  let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
  for i = 0 to 15 do
    let r = highlight 255 i in
    let s =  Printf.sprintf "0x00%02X00ff" r in
    GdkPixbuf.fill pixb (Int32.of_string s);
    GdkPixbuf.copy_area ~dest:color_green ~dest_x:0 ~dest_y:i ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
  done

let color_black = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true ()
let _ =
  let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
  for i = 0 to 15 do
    let r = highlight 128 i in
    let s =  Printf.sprintf "0x%02X%02X%02Xff" r r r in
    GdkPixbuf.fill pixb (Int32.of_string s);
    GdkPixbuf.copy_area ~dest:color_black ~dest_x:0 ~dest_y:i ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
  done

let color_orange = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true ()
let _ =
  let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
  for i = 0 to 15 do
    let r = highlight 255 i in
    let g = 178 * r / 255 in
    let s =  Printf.sprintf "0x%02X%02X00ff" r g in
    GdkPixbuf.fill pixb (Int32.of_string s);
    GdkPixbuf.copy_area ~dest:color_orange ~dest_x:0 ~dest_y:i ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
  done

let color_yellow = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true ()
let _ =
  let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
  for i = 0 to 15 do
     let r = highlight 255 i in
    let g = 255 * r / 255 in
    let s =  Printf.sprintf "0x%02X%02X00ff" r g in
    GdkPixbuf.fill pixb (Int32.of_string s);
    GdkPixbuf.copy_area ~dest:color_yellow ~dest_x:0 ~dest_y:i ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
  done

let color_blue_relative = ref [||]

let create_color_blue_relative () =
  color_blue_relative := [||];
  for i = 0 to (!!O.gtk_misc_availability_max - 1) do
    let pixbuf = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true () in
    let col_step = i * 255 / (!!O.gtk_misc_availability_max - 1) in
    let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
    for j = 0 to 15 do
      let b = highlight 255 j in
      let g = highlight col_step j in
      let s =  Printf.sprintf "0x00%02X%02Xff" g b in
      GdkPixbuf.fill pixb (Int32.of_string s);
      GdkPixbuf.copy_area ~dest:pixbuf ~dest_x:0 ~dest_y:j ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
    done;
    color_blue_relative := Array.append !color_blue_relative [|pixbuf|]
  done

let _ = create_color_blue_relative ()

let color_grey = GdkPixbuf.create ~width:1 ~height:16 ~has_alpha:true ()
let _ =
  let pixb = GdkPixbuf.create ~width:1 ~height:1 ~has_alpha:true () in
  for i = 0 to 15 do
    let r = highlight 255 i in
    let s =  Printf.sprintf "0x%02X%02X%02Xff" r r r in
    GdkPixbuf.fill pixb (Int32.of_string s);
    GdkPixbuf.copy_area ~dest:color_grey ~dest_x:0 ~dest_y:i ~width:1 ~height:1 ~src_x:0 ~src_y:0 pixb
  done

let normalize_availability avail =
  let len = String.length avail in
  for i = 0 to (len - 1) do
    if !!O.gtk_misc_use_availability_height
      then if (int_of_char avail.[i]) > !!O.gtk_misc_availability_max
        then avail.[i] <- (char_of_int !!O.gtk_misc_availability_max)
        else ()
      else if (int_of_char avail.[i]) > 1
        then avail.[i] <- (char_of_int 1)
        else ()
  done

let get_availability_of availability chunks is_file =
  let height = 16 in
  let nchunks = match chunks with
    | None -> 1
    | Some chunks -> max 1 (VB.length chunks) in
  let avail =
    if is_file
      then begin
        let s = String.copy availability in
        normalize_availability s;
        s
      end else availability
  in
  let key = (avail, chunks, is_file) in
  try
    Hashtbl.find avail_bars key
  with Not_found ->
    begin
      (if !!verbose then lprintf' "Creating new availability bar\n");
      let dest = GdkPixbuf.create ~width:nchunks ~height ~has_alpha:true () in
      (try
        match chunks with
        | None -> ()
        | Some chunks ->
        for i = 0 to (nchunks - 1) do
          if is_file
          then 
            (match VB.get chunks i with
            | VB.State_complete | VB.State_verified ->
                GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_green
            | VB.State_missing | VB.State_partial -> 
                let h = int_of_char (avail.[i]) in
                match h, VB.get chunks i with
                | 0, VB.State_missing ->
                    GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_red
                | 0, _ ->
                    GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_orange
                | _, VB.State_missing ->
                    let color_blue = !color_blue_relative.(!!O.gtk_misc_availability_max - h) in
                    GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_blue
                | _, _ ->
                    GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_yellow
            )
          else 
            match int_of_char avail.[i] >= 49, VB.get chunks i with
            | true, VB.State_complete
            | true, VB.State_verified ->
                GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_black
            | true, VB.State_missing ->
                GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_green
            | true, VB.State_partial ->
                GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_yellow
            | false, _ ->
                GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_red
        done;

      with _ ->
          if !!verbose then 
            lprintf' "Exception in creating avail_bar avail: %s chunks: %s file: %b\n" 
              avail 
              (match chunks with
              |	None -> ""
              | Some chunks -> VB.to_string chunks) 
              is_file;
          for i = 0 to nchunks - 1 do
            GdkPixbuf.copy_area ~dest ~dest_x:i ~dest_y:0 ~width:1 ~height ~src_x:0 ~src_y:0 color_grey
          done);
    Hashtbl.add avail_bars key dest;
    dest
  end


let clean_avail_bars list =
  match list with
      [] -> Hashtbl.clear avail_bars
    | _ ->
       begin
         let l = Hashtbl2.to_list2 avail_bars in
         let normed_list =
           List.map (fun (avail, chunks, is_file) ->
             if is_file
               then begin
                 let s = String.copy avail in
                 normalize_availability s;
                 (s, chunks, is_file)
             end else (avail, chunks, is_file)
           ) list
         in
         (if !!verbose then lprintf' "Cleaning avail_bars\n   avail_bars : %d bars\n   new list   : %d items\n"
             (List.length l) (List.length list));
         List.iter (fun (tuple3, _) ->
           if not (List.mem tuple3 normed_list)
             then Hashtbl.remove avail_bars tuple3
         ) l;
         if !!verbose
           then begin
             let l = Hashtbl2.to_list2 avail_bars in
             lprintf' "   -----------------------\n   avail_bars : %d bars\n"  (List.length l)
           end
       end
