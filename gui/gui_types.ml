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
open Mftp

let version = 16
  
module Mp3tag = Mp3tag.Id3v1
  
type avi_info = {
    mutable avi_codec : string;
    mutable avi_width : int;
    mutable avi_height : int;
    mutable avi_fps : int;
    mutable avi_rate : int;
  }

  
type format =
  AVI of avi_info
| Mp3 of Mp3tag.tag
| FormatType of string * string
| Unknown_format

and result = {
    mutable result_names : string list;
    result_md4 : Md4.t;
    mutable result_size : int32;
    mutable result_format : string;
    mutable result_type : string;
    mutable result_filtered_out : int;
    mutable result_tags : Mftp.tag list;
  }

and history_result = {
    mutable hresult_names : string list;
    hresult_md4 : Md4.t;
    mutable hresult_size : int32;
    hresult_tags : Mftp.tag list;
  }

and file_state =
  FileDownloading
| FileCancelled
| FilePaused
| FileDownloaded
| FileRemoved

    
type connection_state =
  NotConnected
| Connecting
| Connected_initiating
| Connected_busy
| Connected_idle
| Connected_queued
| Removed

and location_kind = 
  Known_location of Ip.t * int
| Indirect_location

and friend_kind =
  NotAFriend
| Friend
| FriendRemoved

  
and search_query = {
    mutable search_words : string list; 
    mutable search_minsize : int32 option;
    mutable search_maxsize : int32 option;
    mutable search_avail : int32 option;
    mutable search_media : string option;
    mutable search_format : string option;
    mutable search_fields : (string * string) list;
    mutable search_min_bitrate : int32 option;
    mutable search_title : string option;
    mutable search_album : string option;
    mutable search_artist : string option;    
    mutable search_max_hits : int;
  }
