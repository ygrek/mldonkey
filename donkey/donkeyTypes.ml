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
    mutable server_server : server CommonServer.server_impl;
    mutable server_ip : Ip.t;
    mutable server_cid : Ip.t;
    mutable server_port : int;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_nqueries : int;
    mutable server_search_queries : server_search Fifo.t;
    mutable server_users_queries : bool Fifo.t;
    mutable server_connection_control : connection_control;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users: user list;
    mutable server_next_udp : float;
    mutable server_master : bool;
    mutable server_mldonkey : bool;
    mutable server_last_message : float; (* used only by mldonkey server *)
    
    mutable server_queries_credit : int;
    mutable server_waiting_queries : file list;
  } 


and user = {
    user_user : user CommonUser.user_impl;
    user_md4 : Md4.t;
    mutable user_name : string;
    user_ip : Ip.t;
    user_port : int;
    user_tags : CommonTypes.tag list;
    user_server : server;
  }
  
and server_search = {
    search : local_search;
    mutable nhits : int;
  }
  
and local_search = {
    search_search : CommonTypes.search;
    mutable search_handler : (search_event -> unit);
    mutable search_xs_servers : server list;
  }

  
and result = {
    result_result : result CommonResult.result_impl;
    mutable result_index : Store.index;
  }
  
and search_event =
(*  Result of result *)
| Waiting of int
  
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

(*
    mutable client_is_friend : friend_kind;
*)
  
and client = {
    client_client : client CommonClient.client_impl;
    mutable client_kind : location_kind;
    mutable client_md4 : Md4.t;
    mutable client_chunks : availability;
    mutable client_sock : TcpBufferedSocket.t option;
    mutable client_block : block option;
    mutable client_zones : zone list;
    mutable client_connection_control : connection_control;
    mutable client_file_queue : (file * availability) list;
    mutable client_source_for : file list;
    mutable client_next_view_files :  float;
    mutable client_all_files : result list option;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
    mutable client_all_chunks : string;
    mutable client_rating : int32;
    mutable client_upload : upload_info option;
    mutable client_is_mldonkey : int;
    mutable client_checked : bool;
    mutable client_chat_port : int;
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
    mutable zone_end : int32;
    mutable zone_nclients : int;
    mutable zone_present : bool;
    zone_file : file;
  }

and file = {
    file_file : file CommonFile.file_impl;
    mutable file_hardname : string;
    file_md4 : Md4.t;
    file_exists : bool;
(*    mutable file_size : int32; *)
    mutable file_nchunks : int;
    mutable file_chunks : chunk array;
(*    mutable file_age : float; *)
    mutable file_chunks_age : float array;
(*    mutable file_fd : Unix32.t; *)
    mutable file_all_chunks : string;
    mutable file_absent_chunks : (int32 * int32) list;
    mutable file_filenames : string list;
    mutable file_sources : client Intmap.t;
    mutable file_nlocations : int;
    mutable file_md4s : Md4.t list;
    mutable file_format : format;
    mutable file_available_chunks : int array;

(*
    mutable file_last_downloaded : (int32 * float) list;
    mutable file_last_time : float;
    mutable file_last_rate : float;
*)
    
(* the time the file state was last computed and sent to guis *)
    mutable file_changed : file_change_kind; 
    mutable file_new_locations : bool;
    mutable file_shared : file CommonShared.shared_impl option;

  }

and file_to_share = {
    shared_name : string;
    shared_size : int32;
    mutable shared_list : Md4.t list;
    mutable shared_pos : int32;
    mutable shared_fd : Unix32.t;
    shared_shared : file_to_share CommonShared.shared_impl;
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


type old_result = result
    
exception NoSpecifiedFile
exception Already_done

type shared_file_info = {
    sh_name : string;
    sh_md4s : Md4.t list;
    sh_mtime : float;
    sh_size : int32;
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
  
let network = CommonNetwork.new_network "Donkey"
  
let (shared_ops : file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network

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
let (room_ops : server CommonRoom.room_ops) = 
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

  
let (pre_shared_ops : file_to_share CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network
    
let (shared_ops : file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network
  