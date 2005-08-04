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
    mutable torrent_name_utf8 : string;
    mutable torrent_filename : string;
    mutable torrent_length : int64;
    mutable torrent_announce : string;
    mutable torrent_announce_list : string list;
    mutable torrent_piece_size : int64;
    mutable torrent_files :  (string * int64) list;
    mutable torrent_pieces : Sha1.t array;
    mutable torrent_comment : string;
    mutable torrent_created_by : string;
    mutable torrent_creation_date : int64;
    mutable torrent_modified_by : string;
    mutable torrent_encoding : string;
    mutable torrent_private : int64;
(*
    mutable torrent_nodes : string;
*)
  }

type brand =
  Brand_unknown
| Brand_abc
| Brand_arctic
| Brand_azureus
| Brand_bitbuddy
| Brand_bitcomet
| Brand_bitkitten
| Brand_bitlord
| Brand_bitsonwheels
| Brand_bitspirit
| Brand_bittornado
| Brand_bittorrentx
| Brand_btplus
| Brand_btslave
| Brand_btugaxp
| Brand_burst
| Brand_ctorrent
| Brand_deadmanwalking
| Brand_exeem
| Brand_experimental
| Brand_g3torrent
| Brand_libtorrent
| Brand_mainline
| Brand_martiniman
| Brand_mldonkey
| Brand_moonlighttorrent
| Brand_plus
| Brand_shadow
| Brand_sharenet
| Brand_shareaza
| Brand_simplebt
| Brand_snark
| Brand_swarmscope
| Brand_swarmy
| Brand_swiftbit
| Brand_teeweety
| Brand_torrentdotnet
| Brand_torrentstorm
| Brand_turbobt
| Brand_upnp
| Brand_xantorrent
| Brand_xbt
| Brand_ziptorrent

let brand_count = 43

type brand_stat = {
  mutable brand_seen : int;
  mutable brand_banned : int;
  mutable brand_filerequest : int;
  mutable brand_download : Int64.t;
  mutable brand_upload : Int64.t;
}

let dummy_stats =
  {
    brand_seen = 0;
    brand_banned = 0;
    brand_filerequest = 0;
    brand_download = Int64.zero;
    brand_upload = Int64.zero
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

    mutable client_brand : brand;
    mutable client_release : string;

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
    mutable tracker_min_interval : int;
    mutable tracker_last_conn : int;
    mutable tracker_last_clients_num : int;
    mutable tracker_torrent_downloaded : int;
    mutable tracker_torrent_complete : int;
    mutable tracker_torrent_incomplete : int;
    mutable tracker_torrent_total_clients_count : int;
    mutable tracker_torrent_last_dl_req : int;
    mutable tracker_id : string;
    mutable tracker_key : string;
  }

and file = {
    file_file : file CommonFile.file_impl;
    file_piece_size : int64;
    file_id : Sha1.t;
    file_name : string;
    file_comment : string;
    file_created_by : string;
    file_creation_date : int64;
    file_modified_by : string;
    file_encoding : string;
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
    mutable file_shared : file CommonShared.shared_impl option;
  }

and ft = {
    ft_file : ft CommonFile.file_impl;
    ft_id : int;
    ft_filename : string;
    mutable ft_retry : (ft -> unit);
  }
