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

  
type server = {
    server_server: server CommonServer.server_impl;
    mutable server_name : string;
    mutable server_addr : addr;
    mutable server_info : string;
    mutable server_nusers : int;
    server_connection_control : CommonTypes.connection_control;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_port : int;
    mutable server_nick : int;
    mutable server_last_nick : string;
    mutable server_search : search option;
    mutable server_search_timeout : float;
    mutable server_users : user list;
  }
  
and room = {
    room_room: room CommonRoom.room_impl;
    room_name : string;
    mutable room_users : user list;
    mutable room_nusers : int;
    mutable room_messages : (int * room_message) list;
  }

and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int32;
    mutable result_sources : (user * string) list;
  }
  
and user = {
    user_user : user CommonUser.user_impl; 
    user_nick : string;
    mutable user_rooms : room list;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_name : string;
    file_id : Md4.t;
    mutable file_temp : string;
    mutable file_clients : client list;
  }

and client = {
    client_client : client CommonClient.client_impl;
    client_name : string;
    mutable client_addr : (Ip.t * int) option;
    mutable client_peer_sock : TcpBufferedSocket.t option;
    mutable client_downloads : download list;
    mutable client_result_socks : TcpBufferedSocket.t list;
    client_connection_control : CommonTypes.connection_control;
    mutable client_files : (file * string) list;
    mutable client_pos : int32;
    mutable client_all_files : (string * result) list option;
    mutable client_receiving : int32;
    mutable client_user : user;
    mutable client_requests : (int * file) list;
  }

and download = {
    download_client : client;
    download_file : file;
    mutable download_pos : int32;
    download_sock : TcpBufferedSocket.t;
    mutable download_ok : bool;
  }
  
open CommonNetwork

(*
    mutable op_network_connected_servers : (unit -> server list);
    mutable op_network_config_file : (unit -> Options.options_file);
    mutable op_network_is_enabled : (unit -> bool);
    mutable op_network_save_simple_options : (unit -> unit);
    mutable op_network_load_simple_options : (unit -> unit);
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
    mutable op_network_prefixed_args : 
      (unit -> (string * Arg.spec * string) list);    
    mutable op_network_search : (search -> Buffer.t -> unit);
    mutable op_network_share : (shared -> unit);
    mutable op_network_private_message : (string -> string -> unit);
    mutable op_network_connect_servers : (unit -> unit);
    mutable op_network_add_server_id : (Ip.t -> int -> unit);
    mutable op_network_forget_search : (search -> unit);
    mutable op_network_close_search : (search -> unit);
    mutable op_network_extend_search : (unit -> unit);
    mutable op_network_clean_servers : (unit -> unit);
    mutable op_network_add_friend_id : (Ip.t -> int -> unit);

*)
  
let network = new_network "Soulseek"  

  (*
     op_result_network : network;
     op_result_download : ('a -> string list -> unit);
     op_result_info : ('a -> CommonTypes.result_info);
  *)
      
let (result_ops : result CommonResult.result_ops) = 
  CommonResult.new_result_ops network
  
(*
     op_server_network : network;
     op_server_to_option : ('a -> (string * option_value) list);
     op_server_remove : ('a -> unit);
     op_server_info : ('a -> GuiProto.server_info);
     op_server_sort : ('a -> float);
     op_server_connect : ('a -> unit);
     op_server_disconnect : ('a -> unit);
     op_server_users : ('a -> user list);
     op_server_query_users : ('a -> unit);
     op_server_find_user : ('a -> string -> unit);
     op_server_new_messages : (unit -> (int * int * string) list);
*)
  
let (server_ops : server CommonServer.server_ops) = 
  CommonServer.new_server_ops network
  
(*
  
     op_room_close : ('a -> unit);
     op_room_pause : ('a -> unit);
     op_room_resume : ('a -> unit);
     op_room_messages : ('a -> room_message list);
     op_room_users : ('a -> user list);
     op_room_name : ('a -> string);
     op_room_info : ('a -> GuiProto.room_info);
     op_room_send_message : ('a -> room_message -> unit);

*)
let (room_ops : room CommonRoom.room_ops) = 
  CommonRoom.new_room_ops network
  
(*
     op_user_network : network;
     op_user_commit : ('a -> unit);
     op_user_save_as : ('a -> string -> unit);
     op_user_print : ('a -> CommonTypes.connection_options -> unit);
     op_user_to_option : ('a -> (string * option_value) list);
     op_user_remove : ('a -> unit);
     op_user_info : ('a -> GuiProto.user_info);
     op_user_set_friend : ('a -> unit);
     op_user_browse_files : ('a -> unit);
*)
  
let (user_ops : user CommonUser.user_ops) = 
  CommonUser.new_user_ops network
  
(*
     op_file_network : network;
     op_file_commit : ('a -> unit);
     op_file_save_as : ('a -> string -> unit);
     op_file_to_option : ('a -> (string * option_value) list);
     op_file_cancel : ('a -> unit);
     op_file_pause : ('a -> unit);
     op_file_resume : ('a -> unit);
     op_file_info : ('a -> GuiProto.file_info);
     op_file_disk_name : ('a -> string);
     op_file_best_name : ('a -> string);
     op_file_state : ('a -> CommonTypes.file_state);
     op_file_set_format : ('a -> CommonTypes.format -> unit);
     op_file_check : ('a -> unit);
     op_file_recover : ('a -> unit);
     op_file_sources : ('a -> client list);
*)
  
let (file_ops : file CommonFile.file_ops) = 
  CommonFile.new_file_ops network
  
(*
     op_client_network : network;
     op_client_commit : ('a -> unit);
     op_client_connect : ('a -> unit);
     op_client_save_as : ('a -> string -> unit);
     op_client_to_option : ('a -> (string * option_value) list);
     op_client_cancel : ('a -> unit);
     op_client_info : ('a -> GuiProto.client_info);
     op_client_say : ('a -> string -> unit);
     op_client_files : ('a -> (string * result) list);
     op_client_set_friend : ('a -> unit);
     op_client_remove_friend : ('a -> unit);
*)
  
let (client_ops : client CommonClient.client_ops) = 
  CommonClient.new_client_ops network

  