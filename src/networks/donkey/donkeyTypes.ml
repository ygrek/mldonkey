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

open Queues
open Md4
open CommonDownloads
open CommonTypes
  (*
  
module MyList = struct
    type 'a mylist = 
      Nil
    | Cons of int * 'a * 'a mylist

    type ('a,'b) assoc = 
      { 
        v1 : 'a;
        mutable v2 : 'b;
      }
      
    let nil = Nil
    let length list = 
      match list with Nil -> 0 | Cons (len,_,_) -> len
      
    let add v list = Cons (1+ length list, v, list)
    let head list =
      match list with
        Nil -> raise Not_found
      | Cons (_,v,_) -> v
          
    let tail list =
      match list with
        Nil -> raise Not_found
      | Cons (_,_,tail) -> tail

    let rec mem v list =
      match list with
        Nil -> raise Not_found
      | Cons (_,vv,tail) ->
          vv = v || mem v tail

    let rec memq v list =
      match list with
        Nil -> raise Not_found
      | Cons (_,vv,tail) ->
          vv == v || memq v tail

    let assoc x y = { v1 = x; v2 = y; }
    let assoc_get x = x.v2
    let assoc_set x y = x.v2 <- y
      
    let rec find_assoc v list =
      match list with
        Nil -> raise Not_found
      | Cons (_,vv,tail) ->
          if vv.v1 = v then vv else find_assoc v tail

    let rec find_assq v list =
      match list with
        Nil -> raise Not_found
      | Cons (_,vv,tail) ->
          if vv.v1 == v then vv else find_assq v tail
          
    let rec mem_assoc v list =
      match list with
        Nil -> raise Not_found
      | Cons (_,vv,tail) ->
          vv.v1 = v || mem_assoc v tail

    let rec mem_assq v list =
      match list with
        Nil -> raise Not_found
      | Cons (_,vv,tail) ->
          vv.v1 == v || mem_assq v tail
          
  end

open MyList
    *)

  (*
type client_tag_name =
  CT_CLIENT_NAME
| CT_CLIENT_PORT
| CT_CLIENT_VERSION
| CT_CLIENT_EXTENDED
| CT_CLIENT_UDPPORT
  
type server_tag_name =
| ST_SERVER_NAME
| ST_SERVER_DESCRIPTION
| ST_SERVER_PORT
| ST_SERVER_IP
| ST_SERVER_PING
| ST_SERVER_PROF
| ST_SERVER_HISTORY
  
type file_tag_name =
| FT_FILE_FILENAME
| FT_FILE_SIZE
| FT_FILE_TYPE
| FT_FILE_FORMAT
| FT_FILE_AVAILABILITY
| FT_FILE_TITLE
| FT_FILE_ARTIST
| FT_FILE_ALBUM
| FT_FILE_LOC
| FT_FILE_DOWNLOADED
| FT_FILE_DISKNAME
| FT_FILE_PRIORITY
| FT_FILE_STATUS
    *)  

type emule_proto = {
    mutable emule_comments : int;
    mutable emule_version : int;
    mutable emule_secident : int;
    mutable emule_noviewshared : int;
    mutable emule_supportpreview : int;

    mutable emule_compression : int;
    mutable emule_sourceexchange : int;
    mutable emule_multipacket : int;
    mutable emule_extendedrequest : int;
    mutable emule_features : int;
    mutable emule_udpver : int;
  }

type emule_tag_name =
| ET_COMPRESSION
| ET_UDPPORT
| ET_UDPVER
| ET_SOURCEEXCHANGE
| ET_COMMENTS
| ET_EXTENDEDREQUEST
| ET_COMPATABLECLIENT
| ET_MOD_FEATURESET
| ET_MOD_PROTOCOL
| ET_MOD_VERSION
| ET_MOD_TAROD
| ET_TAROD_VERSION
| ET_MOD_PLUS
(*  
type pref_tag_name =
| PT_PREF_NAME
| PT_PREF_PORT
| PT_PREF_VERSION
| PT_PREF_TEMP
    *)
type request_record = {
  mutable last_request : int;
  mutable nwarnings : int;
}
  
type client_score =
  Client_not_connected 
| Client_has_file
| Client_has_priority_file
| Client_has_chunk
| Client_has_priority_chunk
| Client_has_upload
| Client_has_priority_upload

type reliability =
  Reliability_neutral
| Reliability_reliable
| Reliability_suspicious of int

type brand = 
  Brand_unknown
| Brand_edonkey
| Brand_cdonkey
| Brand_mldonkey1
| Brand_mldonkey2
| Brand_mldonkey3
| Brand_overnet
| Brand_newemule
| Brand_lmule
| Brand_shareaza
| Brand_server
| Brand_amule
| Brand_lphant
  
let brand_count = 13
  
type brand_mod =
  Brand_mod_unknown
| Brand_mod_extasy
| Brand_mod_hunter
| Brand_mod_sivka
| Brand_mod_ice
| Brand_mod_plus
| Brand_mod_lsd
| Brand_mod_maella
| Brand_mod_pille
| Brand_mod_morphkad
| Brand_mod_efmod
| Brand_mod_xtreme
| Brand_mod_bionic
| Brand_mod_pawcio
| Brand_mod_zzul
| Brand_mod_blackhand
| Brand_mod_lovelace
| Brand_mod_morphnext
| Brand_mod_fincan
| Brand_mod_ewombat
| Brand_mod_morph
| Brand_mod_mortillo
| Brand_mod_lh
| Brand_mod_emulespana
| Brand_mod_blackrat
| Brand_mod_enkeydev
| Brand_mod_gnaddelwarz
| Brand_mod_phoenixkad
| Brand_mod_koizo
| Brand_mod_ed2kfiles
| Brand_mod_athlazan
| Brand_mod_cryptum
| Brand_mod_lamerzchoice
| Brand_mod_notdead
| Brand_mod_peace
| Brand_mod_goldicryptum
| Brand_mod_eastshare
| Brand_mod_mfck
| Brand_mod_echanblard
| Brand_mod_sp4rk
| Brand_mod_powermule
| Brand_mod_bloodymad
| Brand_mod_roman2k
| Brand_mod_gammaoh
| Brand_mod_elfenwombat
| Brand_mod_o2
| Brand_mod_dm
| Brand_mod_sfiom
| Brand_mod_magic_elseve
| Brand_mod_schlumpmule
| Brand_mod_lc
| Brand_mod_noamson
| Brand_mod_stormit
| Brand_mod_omax
| Brand_mod_mison
| Brand_mod_phoenix
| Brand_mod_spiders
| Brand_mod_iberica
| Brand_mod_mortimer
| Brand_mod_stonehenge
| Brand_mod_xlillo
| Brand_mod_imperator
| Brand_mod_raziboom
| Brand_mod_khaos
| Brand_mod_hardmule
| Brand_mod_sc
| Brand_mod_cy4n1d
| Brand_mod_dmx
| Brand_mod_ketamine
| Brand_mod_blackmule
| Brand_mod_morphxt
| Brand_mod_ngdonkey
| Brand_mod_hawkstar
| Brand_mod_neomule
| Brand_mod_cyrex
| Brand_mod_aldo
| Brand_mod_emulede
| Brand_mod_zx
| Brand_mod_ibericaxt
| Brand_mod_candymule
| Brand_mod_ackronic
| Brand_mod_rappis
| Brand_mod_overdose
| Brand_mod_hebmule
| Brand_mod_senfei
| Brand_mod_spoofmod
| Brand_mod_fusspilz
| Brand_mod_rocket
| Brand_mod_warezfaw
| Brand_mod_emusicmule
| Brand_mod_aideadsl
| Brand_mod_epo
| Brand_mod_kalitsch
| Brand_mod_raynz
| Brand_mod_serverclient
| Brand_mod_bl4ckbird
| Brand_mod_bl4ckf0x
| Brand_mod_rt

let brand_mod_count = 98

type source_uid = 
  Direct_address of Ip.t * int
| Indirect_address of Ip.t * int * int64
| Invalid_address of string * string

let id_of_ip ip = Ip.to_int64 (Ip.rev ip)
let ip_of_id id = Ip.rev (Ip.of_int64 id)
  
module DonkeySources = CommonSources.Make(struct
      
      open Options

      type source_brand = bool
      let value_to_source_brand = value_to_bool
      let source_brand_to_value = bool_to_value
      let dummy_source_brand = false
        
      type t = source_uid
      type source_uid = t

      let module_name = "DonkeySources"
        
      let direct_source s =
        match s with Direct_address _ -> true | _ -> false
      
      let dummy_source_uid = Direct_address (Ip.null, 0)
            
      let value_to_source_uid v = 
        match v with
          List [ip;port] | SmallList [ip;port] ->
            let ip = Ip.of_string (value_to_string ip) in
            let port = value_to_int port in
            Direct_address (ip, port)
        | List [ip;port; id] | SmallList [ip;port;id] ->
            let ip = Ip.of_string (value_to_string ip) in
            let port = value_to_int port in
            let id = try id_of_ip (Ip.of_string (value_to_string id))
              with _ -> value_to_int64 id in
            Indirect_address (ip, port, id)
        | _ -> 
            failwith "bad client address"
      
      let source_uid_to_value s = 
        match s with
          Direct_address (ip, port) -> 
            SmallList [string_to_value (Ip.to_string ip); int_to_value port]
        | Indirect_address (ip, port, id) -> 
            SmallList [
              string_to_value (Ip.to_string ip); 
              int_to_value port;
              int64_to_value id;
            ]
        | Invalid_address _ -> failwith "Invalid address"
    end)
  
type server = (*[]*){
    mutable server_server : server CommonServer.server_impl;
    mutable server_ip : Ip.t;
    mutable server_cid : Ip.t option;
    mutable server_port : int;
    mutable server_sock : tcp_connection;
    mutable server_nqueries : int;
    mutable server_search_queries : CommonTypes.search Fifo.t;
    mutable server_users_queries : bool Fifo.t;
    mutable server_connection_control : connection_control;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_max_users : int;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_banner : string;
    mutable server_users: user list;
    mutable server_next_udp : int;
    mutable server_master : bool;
    mutable server_mldonkey : bool;
    mutable server_last_message : int; (* used only by mldonkey server *)

    mutable server_id_requests : file option Fifo.t;
    
    mutable server_queries_credit : int;
    mutable server_waiting_queries : file list;
    mutable server_has_zlib : bool;
    
    mutable server_flags : int;
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

(*
and server_search = {
    search_search : CommonTypes.search;
    mutable nhits : int;
  }
*)

(*
and local_search = {
    search_search : CommonTypes.search;
    mutable search_handler : (search_event -> unit);
    mutable search_xs_servers : server list;
    mutable search_overnet : bool;
  }
*)

  (*
and result = {
    result_result : result CommonResult.result_impl;
    mutable result_index : Store.index;
  }
*)
  
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
and source = {
(*    source_num : int; *)
    source_addr : Ip.t * int;
(*    mutable source_client: client_kind;      *)
(* This field is not kept up-to-date when source_client = SourceClient c,
  change c.client_source_for *)
    mutable source_files : file_request list;
    mutable source_overnet : bool;
    mutable source_score : int;
    mutable source_last_score : int;
    mutable source_last_age : int;
    mutable source_client_num : int;
    mutable source_age : int;
    mutable source_in_queues : file list;
    mutable source_sock : tcp_connection;
  }
*)
  
and client = {
    client_client : client CommonClient.client_impl;
    mutable client_kind : source_uid;
    client_source : DonkeySources.source;
    mutable client_md4 : Md4.t;
(*    mutable client_chunks : availability; *)
(*     mutable client_sock : tcp_connection; *)
    mutable client_ip : Ip.t;
    mutable client_download : (file * Int64Swarmer.uploader) option; 
(*    mutable client_block : Int64Swarmer.block option; *)
(*    mutable client_zones : (int64 * int64 * Int64Swarmer.range) list; *)
(*    mutable client_connection_control : connection_control; *)
    mutable client_file_queue : (
      file * (* has displayed when connected *)
      availability *
      Int64Swarmer.uploader
      ) list;
    mutable client_next_view_files :  int;
    mutable client_all_files : result list option;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
(*    mutable client_all_chunks : string; *)
    mutable client_rating : int ;
    mutable client_upload : upload_info option;
    mutable client_checked : bool;
    mutable client_connected : bool;
(* statistics *)
    mutable client_last_filereqs : int;    
    mutable client_downloaded : Int64.t;
    mutable client_uploaded : Int64.t;
    mutable client_brand : brand;
    mutable client_mod_brand : brand_mod;
    mutable client_banned : bool;
    mutable client_score : int;
    mutable client_next_queue : int;
    mutable client_rank : int;
    mutable client_connect_time : int;
    mutable client_requests_received : int;
    mutable client_requests_sent: int;
    mutable client_indirect_address : (Ip.t * Ip.t * int) option;
    mutable client_slot : slot_status;
    mutable client_debug : bool;
    mutable client_pending_messages: string list;
    client_emule_proto : emule_proto;
    mutable client_comp : compressed_parts option;
    mutable client_connection_time : int;
  }
  
and compressed_parts = {
    comp_md4 : Md4.t;
    comp_pos : int64; 
    comp_total : int;
    mutable comp_len : int;
    mutable comp_blocs : string list;
  }

and slot_status = 
  SlotNotAsked
| SlotAsked
| SlotReceived
  
and upload_info = {
    mutable up_file : file;
    mutable up_pos : int64;
    mutable up_end_chunk : int64;
    mutable up_chunks : (int64 * int64) list;
    mutable up_waiting : bool;
  }

(*
  
and chunk = 
  PresentTemp
| AbsentTemp
| PartialTemp of block
| PresentVerified
| AbsentVerified
| PartialVerified of block
  
and block = {
    mutable block_present: bool;
    block_begin : int64;
    block_end : int64;
    mutable block_zones : zone list;
    mutable block_nclients : int;
    mutable block_contributors : Ip.t list;
    mutable block_legacy : bool;
    mutable block_pos : int;
    block_file : file;
  }
  
and zone = {
    mutable zone_begin : int64;
    mutable zone_end : int64;
    mutable zone_nclients : int;
  }
*)


  (*
and file_request = {
    request_file : file;
    mutable request_time : int;
    mutable  request_result : request_result;
  }

and request_result = 
| File_possible   (* we asked, but didn't know *)
| File_not_found  (* we asked, the file is not there *)
| File_expected   (* we asked, because it was announced *)
| File_new_source (* we never asked *)
| File_found      (* the file was found *)
| File_chunk      (* the file has chunks we want *)
| File_upload     (* we uploaded from this client *)
*)
  
and client_kind = 
  SourceClient of client
| SourceLastConnection of 
  int *
  int * (* last connection attempt *)
  int   (* booked client num *)
  
and file = {
    file_file : file CommonFile.file_impl;
    file_md4 : Md4.t;
(*    file_exists : bool; *)
    
    mutable file_swarmer : Int64Swarmer.t option;
    
    mutable file_nchunks : int;
(*    mutable file_chunks : chunk array; *)
(*    mutable file_chunks_order : int array; *)
(*    mutable file_chunks_age : int array; *)
(*    mutable file_all_chunks : string; *)
(*    mutable file_absent_chunks : (int64 * int64) list; *)
    mutable file_filenames : (string * GuiTypes.ips_list) list;
(*    mutable file_nsources : int; *)
    mutable file_md4s : Md4.t array;
    mutable file_format : format;
(*    mutable file_available_chunks : int array; *)
    mutable file_shared : file CommonShared.shared_impl option;
(*    mutable file_locations : client Intmap.t;  *)
(*    mutable file_mtime : float; *)
(*    mutable file_initialized : bool; *)
(* Source management number 3 !! *)
(*    mutable file_clients : (client * int) Fifo.t;  *)
(*    mutable file_sources : source Queue.t array; *)
    mutable file_sources : DonkeySources.file_sources_manager;
  }

and file_to_share = {
    shared_name : string;
    shared_size : int64;
    mutable shared_list : Md4.t list;
    mutable shared_pos : int64;
    mutable shared_fd : Unix32.t;
    shared_shared : file_to_share CommonShared.shared_impl;
  }
  
module UdpClientMap = Map.Make(struct
      type t = source_uid
      let compare = compare
    end)
  
type udp_client = {
    udp_client_ip : Ip.t;
    udp_client_port : int;
    udp_client_can_receive : bool;
    mutable udp_client_last_conn : int;
  }
  
and file_group = {
    mutable group : udp_client UdpClientMap.t;
  }


type old_result = result
    
exception NoSpecifiedFile
exception Already_done

type shared_file_info = {
    sh_name : string;
    sh_md4s : Md4.t array;
    sh_mtime : float;
    sh_size : int64;
  }

  
open CommonFile
open CommonClient
open CommonServer
open CommonResult 
        
let set_client_state c s =
  let cc = as_client c.client_client in
  set_client_state cc s
        
let set_client_disconnected c =
  let cc = as_client c.client_client in
  set_client_disconnected cc
   
let set_server_state s state =
  let ss = as_server s.server_server in
  if server_state ss <> state then begin
      set_server_state ss state;
    end
  
let file_state file =
  CommonFile.file_state (as_file file.file_file)
  
let file_last_seen file = file.file_file.impl_file_last_seen
  
let file_must_update file =
  file_must_update (as_file file.file_file)
    
let client_state client =
  CommonClient.client_state (as_client client.client_client)
    
let client_new_file client r =
  client_new_file (as_client client.client_client) ""
  r
    
let server_state server =
  CommonServer.server_state (as_server server.server_server)

(*    
module SourcesQueueCreate = Queues.Make (struct  

      type t = source
      let compare s1 s2 = compare s1.source_addr s2.source_addr
    end)
  
  (*
    let lifo = lifo 
    let fifo = fifo
    
    module SourcesSet = Set.Make (
        struct
          type t = int * source
          let compare (t1,s1) (t2,s2) = 
            if s1.source_addr = s2.source_addr then begin
                0 end else              
            let x = compare t1 t2 in
            if x = 0 then compare s1.source_addr s2.source_addr else x
        end
      )

    let oldest_first () = 
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ -> try SourcesSet.min_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try 
              let x = SourcesSet.min_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

    let oldest_last () = 
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ ->
            try SourcesSet.max_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try
              let x = SourcesSet.max_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

      (*
    let max_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.max_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.max_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }

    let min_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ -> SourcesSet.min_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.min_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }
*)
  end
*)
  *)

type brand_stat = {
  mutable brand_seen : int;
  mutable brand_banned : int;
  mutable brand_filerequest : int;
  mutable brand_download : Int64.t;
  mutable brand_upload : Int64.t;
}
  
type brand_mod_stat = {
  mutable brand_mod_seen : int;
  mutable brand_mod_banned : int;
  mutable brand_mod_filerequest : int;
  mutable brand_mod_download : Int64.t;
  mutable brand_mod_upload : Int64.t;
}

let dummy_stats =
  {
    brand_seen = 0;
    brand_banned = 0;
    brand_filerequest = 0;
    brand_download = Int64.zero;
    brand_upload = Int64.zero
  }

let dummy_mod_stats =
  {
    brand_mod_seen = 0;
    brand_mod_banned = 0;
    brand_mod_filerequest = 0;
    brand_mod_download = Int64.zero;
    brand_mod_upload = Int64.zero
  }

  (*
let string_of_file_tag_name name = 
  match name with
  | FT_FILE_SIZE -> "size"
  | FT_FILE_FILENAME -> "filename"
  | FT_FILE_TYPE -> "type"
  | FT_FILE_FORMAT -> "format"
  | FT_FILE_AVAILABILITY -> "availability"
  | FT_FILE_DOWNLOADED -> "downloaded"
  | FT_FILE_DISKNAME -> "diskname"
  | FT_FILE_PRIORITY -> "priority"
  | FT_FILE_STATUS -> "status"
  | FT_FILE_ARTIST -> "Artist"
  | FT_FILE_TITLE -> "Title"
  | FT_FILE_ALBUM -> "Album"
  | FT_FILE_LOC -> "loc"

let string_of_server_tag_name name =
  match name with
  | ST_SERVER_NAME -> "name"
  | ST_SERVER_DESCRIPTION -> "description"
  | ST_SERVER_PORT -> "port"
  | ST_SERVER_IP -> "ip"
  | ST_SERVER_PING -> "ping"
  | ST_SERVER_PROF -> "prof"
  | ST_SERVER_HISTORY -> "history"
    
let string_of_client_tag_name name =
  match name with
  | CT_CLIENT_NAME -> "name"
  | CT_CLIENT_PORT -> "port"
  | CT_CLIENT_VERSION -> "version"
  | CT_CLIENT_EXTENDED -> "extended"
  | CT_CLIENT_UDPPORT -> "udpport"

let string_of_emule_tag_name name =
  match name with
  | ET_COMPRESSION -> "compression"
  | ET_UDPPORT -> "udpport"
  | ET_UDPVER -> "udpver"
  | ET_SOURCEEXCHANGE -> "sourceexchange"
  | ET_COMMENTS -> "comments"
  | ET_EXTENDEDREQUEST -> "extendedrequest"
  | ET_COMPATABLECLIENT -> "compatableclient"
  | ET_MOD_FEATURESET -> "mod_featureset"
  | ET_MOD_PROTOCOL -> "mod_protocol"
  | ET_MOD_VERSION -> "mod_version"
  | ET_MOD_TAROD -> "mod_tarod"
  | ET_TAROD_VERSION -> "tarod_version"
  | ET_MOD_PLUS -> "mod_plus"
    
let string_of_pref_tag_name name =
  match name with
  | PT_PREF_NAME -> ""
  | PT_PREF_PORT -> ""
  | PT_PREF_VERSION -> ""
  | PT_PREF_TEMP -> ""
        *)
  (*
let dummy_source = {
(*    source_num = 0; *)
    source_addr = (Ip.null, 0);
    source_files = [];
              
    source_last_score = 0;
    source_last_age = 0;
    source_client_num = 0;

    source_overnet = false;
    source_score = 0;
    source_age = 0;
    source_in_queues = [];
    source_sock = NoConnection;
  }
    *)


let dummy_emule_proto = {
    emule_comments = 0;
    emule_version = 0;
    emule_secident = 0;
    emule_noviewshared = 0;
    emule_supportpreview = 0;
    
    emule_compression = 0; (* 1 *)
    emule_sourceexchange = 0; (* 3 *)
    emule_multipacket = 0; (* 1 *)
    emule_extendedrequest = 0; (* 2 *)
    emule_features = 0; (* 3 *)
    emule_udpver = 0; (* 4 *)
  }

let emule_proto () = 
  { dummy_emule_proto with emule_version = 0 }
  