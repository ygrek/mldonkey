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
open Mftp_comm
open Gui_types

type output_type = TEXT | HTML
  
exception Avifile_info of avi_info
exception Mp3_info of Mp3tag.tag

type connection_control = {
    mutable control_next_try : float;
    mutable control_last_conn : float;
    mutable control_next_delay : float;
  }
  
type server = {
    mutable server_ip : Ip.t;
    mutable server_cid : Ip.t;
    mutable server_port : int;
    mutable server_num : int;
    mutable server_sock : Mftp_comm.server_sock option;
    mutable server_nqueries : int;
    mutable server_search_queries : search_query_handler Fifo.t;
    mutable server_users_queries : users_query Fifo.t;
    mutable server_connection_control : connection_control;
    mutable server_score : int;
    mutable server_tags : Mftp.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_state : connection_state;
    mutable server_changed : server_change_kind;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users: user list;
    mutable server_next_udp : float;
    mutable server_master : bool;
  } 


and user = {
    user_md4 : Md4.t;
    mutable user_name : string;
    user_ip : Ip.t;
    user_port : int;
    user_tags : Mftp.tag list;
  }
    
and search_query_handler = (
    server -> server_sock -> Mftp_server.QueryReply.t -> unit)

and users_query = (
    server -> server_sock -> Mftp_server.QueryUsersReply.t -> unit)


and search_event =
  Result of result
| Waiting of int
  
and search = {
    mutable search_max_hits : int;
    mutable search_query : Mftp.query;
    mutable search_nresults : int;
    search_files : (Md4.t, int * (int ref)) Hashtbl.t;
    search_num : int;
    mutable search_waiting : int;
    mutable search_string : string;
    mutable search_handler : (search_event -> unit);
    mutable search_xs_servers : server list;
  }
  
and client_query = client -> Mftp_client.t -> unit

and file_change_kind = 
  NoFileChange
| FileAvailabilityChange
| FileInfoChange

and client_change_kind = 
  NoClientChange
| ClientStateChange
| ClientFriendChange
| ClientInfoChange
| ClientFilesChange

and server_change_kind = 
  NoServerChange
| ServerStateChange
| ServerUsersChange
| ServerInfoChange
| ServerBusyChange
  
and availability = bool array
  
and client = {
    mutable client_kind : location_kind;
    mutable client_md4 : Md4.t;
    mutable client_queries : client_query Fifo.t;
    mutable client_chunks : availability;
    mutable client_sock : client_sock option;
    mutable client_block : block option;
    mutable client_zones : zone list;
    mutable client_connection_control : connection_control;
    mutable client_state : connection_state;
    mutable client_file_queue : (file * availability) list;
    mutable client_files : file list;
    mutable client_num : int;
    mutable client_is_friend : friend_kind;
    mutable client_next_view_files :  float;
    mutable client_all_files : int list option;
    mutable client_tags: Mftp.tag list;
    mutable client_name : string;
    mutable client_all_chunks : string;
    mutable client_changed : client_change_kind;
    mutable client_rating : int32;
    mutable client_upload : upload_info option;
    mutable client_is_mldonkey : int;
    mutable client_alias : client option;
    mutable client_checked : bool;
    mutable client_aliases : int list;
  }
  
and upload_info = {
    mutable up_file : file;
    mutable up_pos : int32;
    mutable up_end_chunk : int32;
    mutable up_chunks : (int32 * int32) list;
    mutable up_waiting : bool;
  }
  
and chunk = 
  PresentTemp
| AbsentTemp
| PartialTemp of block
| PresentVerified
| AbsentVerified
| PartialVerified of block
  
and block = {
    mutable block_present: bool;
    block_begin : int32;
    block_end : int32;
    mutable block_zones : zone list;
    mutable block_nclients : int;
    mutable block_pos : int;
    block_file : file;
  }
  
and zone = {
    mutable zone_begin : int32;
    zone_end : int32;
    mutable zone_nclients : int;
    mutable zone_present : bool;
    zone_file : file;
  }

and file = {
    mutable file_hardname : string;
    file_md4 : Md4.t;
    file_num : int;
    file_exists : bool;
    mutable file_size : int32;
    mutable file_nchunks : int;
    mutable file_chunks : chunk array;
    mutable file_age : float;
    mutable file_chunks_age : float array;
    mutable file_fd : Unix32.t;
    mutable file_all_chunks : string;
    mutable file_absent_chunks : (int32 * int32) list;
    mutable file_filenames : string list;
    mutable file_known_locations : client Intmap.t;
    mutable file_nlocations : int;
    mutable file_indirect_locations : client Intmap.t;
    mutable file_md4s : Md4.t list;
    mutable file_downloaded : int32;
    mutable file_state : file_state;
    mutable file_format : format;
    mutable file_available_chunks : int array;

    mutable file_last_downloaded : int32;
    mutable file_last_time : float;
    mutable file_last_rate : float;
(* the time the file state was last computed and sent to guis *)
    mutable file_changed : file_change_kind; 
    mutable file_new_locations : bool;
    mutable file_shared : bool;
    
    mutable file_upload_requests : int;
    mutable file_upload_blocks : int;
  }

and shared_file = {
    shared_name : string;
    shared_size : int32;
    mutable shared_list : Md4.t list;
    mutable shared_pos : int32;
    mutable shared_fd : Unix32.t;
  }

module UdpClientMap = Map.Make(struct
      type t = location_kind
      let compare = compare
    end)

  
type udp_client = {
    udp_client_ip : Ip.t;
    udp_client_port : int;
    udp_client_is_mldonkey : bool;
    mutable udp_client_last_conn : float;
  }
  
and file_group = {
    mutable group : udp_client UdpClientMap.t;
  }

  
type gui_record = {
    mutable gui_search_nums : int list;
    mutable gui_searches : (int * int) list;
    mutable gui_sock : TcpBufferedSocket.t;
    mutable gui_files : file list;
    mutable gui_friends : client list;
    mutable gui_servers : server list; 
  }

type old_result = result
    
exception NoSpecifiedFile
exception Already_done

type shared_file_info = {
    sh_name : string;
    sh_md4s : Md4.t list;
    sh_mtime : float;
    sh_size : int32;
  }
  
  
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
    mutable conn_filter : (Gui_types.result -> unit);
  }

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