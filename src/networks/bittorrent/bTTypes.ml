(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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
open CommonSwarming

type client = {
    client_client : client CommonClient.client_impl;
    mutable client_file : file;
    mutable client_connection_control : connection_control;
    mutable client_sock : tcp_connection;
    mutable client_host : Ip.t * int;
    mutable client_chunks : (int64 * int64) list;
    mutable client_blocks : Int64Swarmer.block list;
    mutable client_ranges : Int64Swarmer.range list;
    mutable client_block : Int64Swarmer.block option;
    mutable client_chocked : bool;
    mutable client_interested : bool;
    mutable client_uid : Sha1.t;
    
    mutable client_bitmap : string;
    mutable client_new_chunks : int list;
    
    mutable client_upload_requests : (int * int64 * int64) list;
    mutable client_allowed_to_write : int64;
    
    mutable client_downloaded : int64;
    mutable client_uploaded : int64;
    
    mutable client_blocks_sent : Int64Swarmer.block list;
    mutable client_good : bool;
  }

and file_info = {
    file_info_tracker : string;
    file_info_id : Sha1.t;
    file_info_size : int64;
    file_info_chunks : Sha1.t array; 
    file_info_files : (string * int64 * int64) list;
    file_info_piece_size : int64;
    file_info_name : string;
  }
  
and file = {
    file_shared : CommonDownloads.SharedDownload.file;

    file_info : file_info;
    file_partition : CommonSwarming.Int64Swarmer.partition;
    mutable file_clients : (Sha1.t, client) Hashtbl.t ;
    mutable file_tracker_connected : bool;
    mutable file_tracker_interval : int;
    mutable file_tracker_last_conn : int;
    mutable file_blocks_downloaded : Int64Swarmer.block list;
  }
