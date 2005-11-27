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

open Md4
open CommonTypes
open GuiTypes

type core_status =
  Core_connecting
| Core_notconnected
| Core_Connected

type button_types =
  DOUBLE_CLICKED
| LBUTTON_CLICKED
| RBUTTON_CLICKED

type tray =
  {
   mutable create_tray      : GdkPixbuf.pixbuf -> string -> unit;
   mutable set_icon_tray    : GdkPixbuf.pixbuf -> unit;
   mutable set_tooltip_tray : string -> unit;
   mutable destroy_tray     : unit -> unit;
  }

type gui_init =
  {
   mutable networks  : bool;
   mutable servers   : bool;
   mutable downloads : bool;
   mutable friends   : bool;
   mutable queries   : bool;
   mutable rooms     : bool;
   mutable uploads   : bool;
   mutable settings  : bool;
   mutable console   : bool;
  }

type gui =
  {
   window         : GWindow.window ;
   vbox           : GPack.box;
   wtool          : GuiTools.tool_bar;
   init           : gui_init;
   clear          : unit -> unit;
   set_corestatus : core_status -> unit;

   mutable current_page        : int;
   mutable switch_to_page      : int -> unit;
   mutable set_splash_screen   : string -> string -> unit;
   mutable update_current_page : unit -> unit;
  }

type net_info = {
    net_num                : int;
    net_name               : string;
    net_flags              : network_flag list;
    mutable net_enabled    : bool;
    mutable net_displayed  : bool;
    mutable net_uploaded   : int64;
    mutable net_downloaded : int64;
    mutable net_connected  : int;
  }

type net_box =
  {
   box_button            : GButton.button;
   box_uploaded          : GMisc.label;
   box_downloaded        : GMisc.label;
   box_connected_servers : GMisc.label;
  }

type gui_section =
  Main
| Interfaces
| Tools
| Mlchat
| Other
| Bittorrent
| Direct_connect
| Donkey
| Fasttrack
| Filetp
| Gnutella
| Open_napster
| Soulseek


type gui_subsection =
  General
| Bandwidth
| Network_config
| Security
| Mlgui
| Html_mods
| Paths
| Startup
| Download
| Mail
| Debug
| Gnutella1
| Gnutella2


type gui_group =
  General_
| Rates
| Connections
| Proxy
| User
| Gui
| Web
| Telnet
| Gift
| Display_conf
| Client
| Look
| Mail_setup
| Colors
| Fonts
| Graph
| Server
| Connection_param
| Uploads
| Downloads
| Sources
| Peers
| Servers
| Overnet
| Tracker
| Others


type res_info =
  {
    res_num                     : int;
    res_network                 : int;
    mutable res_computed        : bool;
    mutable res_name            : string;
    mutable res_uid             : string;
    mutable res_size            : int64;
    mutable res_format          : string;
    mutable res_type            : string;
    mutable res_duration        : string;
    mutable res_codec           : string;
    mutable res_bitrate         : int;
    mutable res_availability    : int;
    mutable res_completesources : int;
    mutable res_tags            : string;
    mutable res_comment         : string;
    mutable res_color           : string;
    mutable res_network_pixb    : GdkPixbuf.pixbuf option;
    mutable res_name_pixb       : GdkPixbuf.pixbuf option;
  }

type g_file_tree =
  {
   mutable g_file_tree_num  : int;
   mutable g_file_tree_name : string;
   mutable g_file_tree_list : g_file_tree_item list;
   mutable g_file_tree_pixb : GdkPixbuf.pixbuf option;
  }

and g_file_tree_item =
  GTreeDirectory of g_file_tree
| GTreeFile of  res_info

type source_slot = int
let source_only = 0
let source_has_slot = 1
let source_has_upload = 3

type source_info =
   {
    source_num : int;
    source_network : int;

    mutable source_kind            : location_kind;
    mutable source_state           : host_state;
    mutable source_type            : client_type;
    mutable source_tags            : CommonTypes.tag list;
    mutable source_name            : string;
    mutable source_files           :  g_file_tree option;
    mutable source_rating          : int;
    mutable source_chat_port       : int;
    mutable source_connect_time    : int;
    mutable source_last_seen       : float;
    mutable source_software        : string;
    mutable source_downloaded      : int64;
    mutable source_uploaded        : int64;
    mutable source_upload_rate     : float;
    mutable source_download_rate   : float;
    mutable source_upload          : string option;
    mutable source_has_upload      : source_slot;
    mutable source_availability    : (int * string) list;  (* file_num, availability *)
    mutable source_files_requested : int list;             (* file_num *)
  }

type stat_type =
  RazorBack
| FileDonkey
| IsoHunt

type stat =
  {
   stats_file_type         : stat_type;

   mutable stats_file_history      : string;
   mutable stats_file_rating       : string;
   mutable stats_file_availability : int;
   mutable stats_file_completed    : int;
  }

type g_file_info = {
    g_file_num     : int;
    g_file_network : int;

    mutable g_file_comment        : string;
    mutable g_file_name           : string;
    mutable g_file_names          : (string * ips_list) list;
    mutable g_file_size           : int64;
    mutable g_file_downloaded     : int64;
    mutable g_file_active_sources : int;
    mutable g_file_all_sources    : int;
    mutable g_file_state          : file_state;
    mutable g_file_chunks         : string;
    mutable g_file_availability   : (int * string) list;
    mutable g_file_sources        : int list option;
    mutable g_file_download_rate  : float;
    mutable g_file_format         : format;
    mutable g_file_chunks_age     : int array;
    mutable g_file_age            : int;
    mutable g_file_last_seen      : int;
    mutable g_file_priority       : int;
    mutable g_file_uids           : Uid.t list;

    mutable g_file_stats          : stat list;
  }

type item_info =
  File of g_file_info
| Source of (source_info * int)   (* source_info, file_num *)

type item_index =
  File_num of int                 (* file_num *)
| Source_num of (int * int)       (* source_num, file_num *)

type ent = GEdit.entry

type query_form =
  QF_AND of query_form list
| QF_OR of query_form list
| QF_ANDNOT of query_form * query_form  
| QF_MODULE of query_form

| QF_KEYWORDS of ent
| QF_MINSIZE of ent * ent (* number and unit *)
| QF_MAXSIZE of ent * ent (* number and unit *)
| QF_FORMAT of ent
| QF_MEDIA of ent

| QF_MP3_ARTIST of ent
| QF_MP3_TITLE of ent
| QF_MP3_ALBUM of ent
| QF_MP3_BITRATE of ent

| QF_COMBO of string ref

| QF_HIDDEN of query_form list

type shared_file =
  {
    g_shared_num               : int;
    g_shared_network           : int;
    mutable g_shared_filename  : string;
    mutable g_shared_size      : int64;
    mutable g_shared_uploaded  : int64;
    mutable g_shared_requests  : int;
    mutable g_shared_uids      : Uid.t list;
    mutable g_shared_last_seen : float;
  }
