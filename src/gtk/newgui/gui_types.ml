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

open Md4
open CommonTypes
open GuiTypes
  
type general_state =
  FDownloading
| FQueued
| FPaused
| FDownloaded
| FShared
| FCancelled
| FNew
| FAborted of string
| CNotConnected of BasicSocket.close_reason * int (* >= 0 Queued *)
| CConnecting
| CConnected_initiating
| CConnected of int    (* >= 0 Queued *)
| CConnected_downloading
| CNewHost
| CRemovedHost
| CBlackListedHost

type gui_file_info = {
    gfile_num : int list;

    mutable gfile_network : int;
    mutable gfile_name : string;
    mutable gfile_names : string list;
    mutable gfile_md4 : Md4.t;
    mutable gfile_size : int64;
    mutable gfile_downloaded : int64;
    mutable gfile_all_sources : int;
    mutable gfile_active_sources: int;
    mutable gfile_state : general_state;
    mutable gfile_chunks : VerificationBitmap.t option;
    mutable gfile_availability : (int * string) list;
    mutable gfile_download_rate : float;
    mutable gfile_format : format;
    mutable gfile_age : int;
    mutable gfile_last_seen : int;
    mutable gfile_priority : int;
    mutable gfile_type : client_type;
    mutable gfile_pixmap : GDraw.pixmap option;
    mutable gfile_net_pixmap : GDraw.pixmap option;
    mutable gfile_priority_pixmap : GDraw.pixmap option;
    mutable gfile_avail_pixmap : GDraw.pixmap option;
  }

type gui_client_info = {
    gclient_num : int;
    gclient_network : int;

    mutable gclient_kind : location_kind;
    mutable gclient_state : host_state;
    mutable gclient_type : client_type;
    mutable gclient_tags: CommonTypes.tag list;
    mutable gclient_name : string;
    mutable gclient_files:  file_tree option;
    mutable gclient_rating : int;
    mutable gclient_connect_time : int;
    mutable gclient_software : string;
    mutable gclient_release : string;
    mutable gclient_emulemod : string;
    mutable gclient_downloaded : int64;
    mutable gclient_uploaded : int64;
    mutable gclient_upload : string option;
    mutable gclient_sock_addr : string;
    mutable gclient_net_pixmap : GDraw.pixmap option;
    mutable gclient_pixmap : GDraw.pixmap option;
  }

  
type gui_server_info = {
    gserver_num : int;
    gserver_network : int;

    mutable gserver_addr : Ip.addr;
    mutable gserver_port : int;
    mutable gserver_score : int;
    mutable gserver_tags : CommonTypes.tag list;
    mutable gserver_nusers : int64;
    mutable gserver_nfiles : int64;
    mutable gserver_state : host_state;
    mutable gserver_name : string;
    mutable gserver_description : string;
    mutable gserver_users : int list option;
    mutable gserver_banner : string;
    mutable gserver_pixmap : GDraw.pixmap option;
    mutable gserver_net_pixmap : GDraw.pixmap option;
  }

type gui_result_info = {
    mutable gresult_num : int;
(* TODO RESULT: it should be a list so that we can know on which networks
  it was found
  gresult_network : int; *)

    mutable gresult_names : string list;
    mutable gresult_uids : Uid.t list;
    mutable gresult_size : int64;
    mutable gresult_format : string;
    mutable gresult_type : string;
    mutable gresult_duration : string;
    mutable gresult_codec : string;
    mutable gresult_bitrate : int;
    mutable gresult_availability : int;
    mutable gresult_completesources : int;
    mutable gresult_comment : string;
    mutable gresult_done : bool;
    mutable gresult_pixmap : GDraw.pixmap option;
    mutable gresult_net_pixmap : GDraw.pixmap option;
  }

type gui_room_info = {
    groom_num : int;
    groom_network : int;
    groom_name : string;
    mutable groom_state : room_state;
    mutable groom_users : int list;
    mutable groom_messages : room_message list;
    mutable groom_nusers : int;
    mutable groom_net_pixmap : GDraw.pixmap option;
  }
