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


type addr = {
    mutable addr_ip: Ip.t;
    mutable addr_name : string;
    mutable addr_age : float;
  }

type query =
  QAnd of query * query
| QOr of query * query
| QAndNot of query * query
| QHasWord of string
| QHasField of string * string
| QHasMinVal of string * int32
| QHasMaxVal of string * int32
| QNone (** temporary, used when no value is available ;
	   must be simplified before transforming into strings *)

type tag_value =
| Uint32 of int32
| Fint32 of int32
| String of string
| Addr of Ip.t
  
type tag = {
    mutable tag_name : string;
    mutable tag_value : tag_value;
  }

type client_type =
  NormalClient  (* all clients *)
| FriendClient  (* to be remembered as a friend *)
| ContactClient (* displayed with friends, but removed after disconnect *)

type query_entry = 
  Q_AND of query_entry list
| Q_OR of query_entry list
| Q_ANDNOT of query_entry * query_entry  
| Q_MODULE of string * query_entry
  
| Q_KEYWORDS of string * string
| Q_MINSIZE of string * string
| Q_MAXSIZE of string * string
| Q_FORMAT of string * string
| Q_MEDIA of string * string
  
| Q_MP3_ARTIST of string * string
| Q_MP3_TITLE of string * string
| Q_MP3_ALBUM of string * string
| Q_MP3_BITRATE of string * string

| Q_HIDDEN of query_entry list
  
    
type host_state =
  NotConnected
| Connecting
| Connected_initiating
| Connected_busy
| Connected_idle
| Connected_queued
  
| NewHost
| RemovedHost
  
type connection_control = {
    mutable control_next_try : float;
    mutable control_last_conn : float;
    mutable control_next_delay : float;
  }


type result_info = {
    mutable result_num : int;    
    result_network : int;
    
    mutable result_names : string list;
    mutable result_md4 : Md4.t;
    mutable result_size : int32;
    mutable result_format : string;
    mutable result_type : string;
    mutable result_tags : tag list;
    mutable result_comment : string;
    mutable result_done : bool;
  }
  
type output_type = TEXT | HTML
  
type sortvd_type = 
  BySize
| ByName
| ByRate
| ByDone
| ByPercent
| NotSorted
  
type connection_options = {
    mutable conn_output : output_type; 
    mutable conn_sortvd : sortvd_type;
    mutable conn_filter : (result_info -> unit);
    mutable conn_buf : Buffer.t;
  }
  
type room_state = 
  RoomOpened
| RoomClosed
| RoomPaused
    
type file
type server
type client
type result
type user
type shared  
type room

type room_message =
  ServerMessage of string
| PublicMessage of int * string
| PrivateMessage of int * string

type search_type = 
  RemoteSearch
| LocalSearch
| SubscribeSearch

type network_info = {
    network_netname : string;
    network_netnum : int;
    network_config_filename : string;
    mutable network_enabled : bool;
    mutable network_uploaded : int64;
    mutable network_downloaded : int64;
  }
    
type network = {
    network_name : string;
    network_num : int;
    mutable network_prefixes : string list;
    mutable network_config_file : Options.options_file option;
    mutable op_network_connected_servers : (unit -> server list);
    mutable op_network_is_enabled : (unit -> bool);
    mutable op_network_save_complex_options : (unit -> unit);
    mutable op_network_load_complex_options : (unit -> unit);
    mutable op_network_enable : (unit -> unit);
    mutable op_network_disable : (unit -> unit);
    
    mutable op_network_add_server : 
      ((string * Options.option_value) list -> server);
    mutable op_network_add_file : 
      bool -> ((string * Options.option_value) list -> file);
    mutable op_network_add_client : 
      bool -> ((string * Options.option_value) list -> client);
    mutable op_network_search : (search -> Buffer.t -> unit);
    mutable op_network_share : (shared -> unit);
    mutable op_network_private_message : (string -> string -> unit);
    mutable op_network_parse_url : (string -> bool);
    mutable op_network_connect_servers : (unit -> unit);
    mutable op_network_forget_search : (search -> unit);
    mutable op_network_close_search : (search -> unit);
    mutable op_network_extend_search : (unit -> unit);
    mutable op_network_clean_servers : (unit -> unit);
    mutable op_network_info : (unit -> network_info);
  }
  
  

and search = {
    search_num : int;
    mutable search_max_hits : int; (* total max allowed hits *)
    mutable search_type : search_type;
    mutable search_query : query;
    mutable search_nresults : int;
    mutable search_results : (int ref * result) Intmap.t;
    mutable search_waiting : int; (* how many replies are we waiting for *)
    mutable search_string : string;
    mutable search_closed : bool; (* should we continue to ask/wait for results *)
    mutable op_search_new_result_handlers : (result -> unit) list;
    mutable op_search_end_reply_handlers : (unit -> unit) list;
  }

exception CommandCloseSocket
  
  
type file_state =
  FileDownloading
| FilePaused
| FileDownloaded
| FileShared
  
| FileCancelled
| FileNew
  
let version = 19
  
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

and history_result = {
    mutable hresult_names : string list;
    hresult_md4 : Md4.t;
    mutable hresult_size : int32;
    hresult_tags : tag list;
  }

and location_kind = 
  Known_location of Ip.t * int
| Indirect_location of string * Md4.t
  
(*
We should add information about what has already been sent to the GUI to
prevent sending the same information several times (for example, 
to avoid the Client_info and Result_info message before the Client_file 
message.
*)
  
type gui_record = {
    mutable gui_num : int;
    mutable gui_search_nums : int list;
    mutable gui_searches : (int * search) list;
    mutable gui_sock : TcpBufferedSocket.t;
    mutable gui_files : file list;
    mutable gui_friends : client list;
    mutable gui_servers : server list; 
    mutable gui_sources : (client list * file) option;
    mutable gui_rooms : room list;
    mutable gui_version : int;
  }
  
exception Avifile_info of avi_info
exception Mp3_info of Mp3tag.tag
