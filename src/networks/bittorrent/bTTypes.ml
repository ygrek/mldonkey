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
open CommonDownloads
open BTRate

type torrent = {
    mutable torrent_name : string;
    mutable torrent_length : int64;
    mutable torrent_announce : string;
    mutable torrent_piece_size : int64;
    mutable torrent_files :  (string * int64) list;
    mutable torrent_pieces : Sha1.t array;
  }

type client = {
    client_client : client CommonClient.client_impl;
    mutable client_file : file;
    mutable client_connection_control : connection_control;
    mutable client_sock : tcp_connection;
    mutable client_host : Ip.t * int;
    mutable client_chunks : (int64 * int64) list;
    mutable client_uploader : Int64Swarmer.uploader option;
    mutable client_ranges_sent : (int64 * int64 * Int64Swarmer.range) list;
    mutable client_range_waiting : 
    (int64 * int64 * Int64Swarmer.range) option;
    mutable client_block : Int64Swarmer.block option;
    
    mutable client_received_peer_id : bool;
    mutable client_sent_choke : bool; (* we sent a Choke to the client *)
    mutable client_choked : bool;      (* we received a Choke from the client *)
    mutable client_interested : bool;
    mutable client_uid : Sha1.t;
    
    mutable client_bitmap : string option;
    mutable client_new_chunks : int list;
    
    mutable client_upload_requests : (int * int64 * int64) list;
    mutable client_allowed_to_write : int64;
    mutable client_upload_rate : Rate.t;
    mutable client_downloaded_rate :  Rate.t;
    mutable client_downloaded : int64;
    mutable client_uploaded : int64;
    mutable client_optimist_time : int;
    
    mutable client_blocks_sent : int list;
    mutable client_good : bool;
    mutable client_num_try : int;
    mutable client_alrd_sent_interested : bool;
    mutable client_alrd_sent_notinterested : bool;
    mutable client_interesting : bool;
    mutable client_incoming : bool;
    mutable client_registered_bitfield : bool;
    mutable client_last_optimist : int;
    mutable client_software : string;
  }

and tracker_info = {
    tracker_url : string;
    mutable tracker_interval : int;
    mutable tracker_last_conn : int;
    mutable tracker_last_clients_num : int;
  }
  
and file = {
    file_file : file CommonFile.file_impl;
    file_piece_size : int64;
    file_id : Sha1.t;
    file_name : string;
    mutable file_swarmer : Int64Swarmer.t option;
    mutable file_clients : ((Ip.t*int), client) Hashtbl.t ;
    mutable file_clients_num : int ;
    mutable file_chunks : Sha1.t array; 
    mutable file_files : (string * int64) list;
    mutable file_blocks_downloaded : int list;
    (* vvv probably a network specific value vvv ?what about file_downloaded?*)
    mutable file_uploaded : int64;
    mutable file_torrent_diskname : string;
    mutable file_trackers : tracker_info list;
    mutable file_tracker_connected : bool;
    mutable file_completed_hook : (file -> unit);
  }

and ft = {
    ft_file : ft CommonFile.file_impl;
    ft_id : int;
    ft_filename : string;
    mutable ft_retry : (ft -> unit);
  }
