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

open CommonTypes


type options = {
    mutable connection_port : int;
    mutable control_port : int;
    mutable gui_port : int;
    
    mutable save_options_delay : float;
    mutable check_client_connections_delay : float;
    mutable check_server_connections_delay : float;
    mutable small_retry_delay : float;
    mutable medium_retry_delay : float;
    mutable long_retry_delay : float;
    
    mutable name : string;
    mutable max_connected_servers : int;
    mutable upload_limit : int;
    mutable features : string;
    
    mutable server_timeout: float;
    mutable client_timeout: float;
    mutable max_server_age : int;
    mutable password : string;
  }

  (*
type server_key = {
    key_ip: Ip.t;
    key_port : int;
  }
*)


type query_entry = CommonTypes.query_entry =
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

type search = {
    mutable search_num : int;
    mutable search_query : query_entry;
    mutable search_max_hits : int;
  }


type from_gui =
| ConnectMore_query
| CleanOldServers
| KillServer
| ExtendedSearch
| Password of int *  string
| Search_query of bool (* local or not *) * search
| Download_query of string list * int 
| AddServer_query of string * Ip.t * int
| AddNewFriend of string * Ip.t * int
| RemoveServer_query of int
| SaveOptions_query of (string * string) list (* options *)
| RemoveDownload_query of int
| ServerUsers_query of int
| SaveFile of int * string
| AddFriend of int
| AddUserFriend of int
| RemoveFriend of int
| RemoveAllFriends
| FindFriend of string
| ViewUsers of int
| ConnectAll of int
| ConnectServer of int
| DisconnectServer of int
| SwitchDownload of int
| VerifyAllChunks of int
| QueryFormat of int
| ModifyMp3Tags of int * Mp3tag.tag
| ForgetSearch of int
| SetOption of string * string
| Command of string
| SayFriends of string * int list
| Preview of int
| ConnectFriend of int  
| GetServer_users of int
| GetClient_files of int
| GetFile_locations of int
| GetServer_info of int
| GetClient_info of int
| GetFile_info of int
| SendMoreInfo of int list * int list
| GetUser_info of int
| SendMessage of int * room_message
  
and network_info = {
    network_num : int;
    network_name : string;
    mutable network_enabled : bool;
  }
  
and file_info = {
    file_num : int;    
    file_network : int;
    
    mutable file_names : string list;
    mutable file_md4 : Md4.t;        
    mutable file_size : int32;
    mutable file_downloaded : int32; (* LOT OF CHANGES *)
    mutable file_nlocations : int; (* MANY CHANGES *)
    mutable file_nclients: int;

    mutable file_state : file_state;
    mutable file_chunks : string;
    mutable file_availability : string; (* MANY CHANGES *)
    mutable file_sources : int list option;
    mutable file_download_rate : float; (* LOT OF CHANGES *)
    mutable file_format : format;
    mutable file_chunks_age : float array;
    mutable file_age : float;
  }
  
and user_info = {
    user_num : int;
    user_md4 : Md4.t;
    user_name : string;
    user_ip : Ip.t;
    user_port : int;
    user_tags : tag list;
    user_server : int;
    mutable user_state : host_state;
  }

and server_info = {
    server_num : int;
    server_network : int;
    
    mutable server_ip : Ip.t;
    mutable server_port : int;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_state : host_state;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users : int list option;
  } 

and room_info = {
    room_num : int;
    room_network : int;
    room_name : string;
    mutable room_state : room_state;
    mutable room_users : int list;
  }
  
and client_info = {
    client_num : int;
    client_network : int;
    
    mutable client_kind : location_kind;
(*    mutable client_md4 : Md4.t;                   *)
(*    mutable client_chunks : string;               *)
(*    mutable client_files : (Md4.t * string) list; *)
    mutable client_state : host_state;
    mutable client_type : client_type;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
    mutable client_files:  result_info list option;
    mutable client_rating : int32;
    mutable client_chat_port : int option;
  }

and local_info = {
    mutable upload_counter : int; 
    mutable shared_files : int;
  }

type to_gui =
| Connected of int
| Options_info of (string * string) list (*  options *)
| GuiConnected
| DefineSearches of (string * CommonTypes.query_entry) list

| Result_info of result_info
  
| Search_result of int * int
| Search_waiting of int * int
  
| File_info of file_info
| File_downloaded of int * int32 * float
| File_availability of int * string * string
| File_source of int * int
  
| Server_busy of int * int * int
| Server_user of int * int
| Server_state of int * host_state
| Server_info of server_info
  
| Client_info of client_info
| Client_state of int * host_state
| Client_friend of int * client_type
| Client_file of int * int

| LocalInfo of local_info
| Console of string

| Network_info of network_info
| User_info of user_info

| Room_info of room_info
| Room_message of int * room_message
| Room_user of int * int
  
type arg_handler =  connection_options -> string
type arg_kind = 
  Arg_none of arg_handler
| Arg_multiple of (string list -> arg_handler)
| Arg_one of (string -> arg_handler)
| Arg_two of (string -> string -> arg_handler)
