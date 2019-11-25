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

open Md4
open CommonTypes
open CommonStats
open Printf2

let log_prefix = "[EDK]"
    
let lprintf_nl ?exn fmt =
  lprintf_nl2 ?exn log_prefix fmt
    
let lprintf_n fmt =
  lprintf2 log_prefix fmt

exception Donkey_large_file

type emule_proto = {
    mutable emule_version : int;
    mutable emule_release : string;
    mutable emule_osinfosupport : int;
    mutable emule_features : int;

(* emule_miscoptions1 *)
    mutable received_miscoptions1 : bool;
    mutable emule_aich : int;
    mutable emule_unicode : int;
    mutable emule_udpver : int;
    mutable emule_compression : int;
    mutable emule_secident : int;
    mutable emule_sourceexchange : int;
    mutable emule_extendedrequest : int;
    mutable emule_comments : int;
    mutable emule_peercache : int;
    mutable emule_noviewshared : int;
    mutable emule_multipacket : int;
    mutable emule_supportpreview : int;

(* emule_miscoptions2 *)
    mutable received_miscoptions2 : bool;
    mutable emule_require_crypt : int;
    mutable emule_request_crypt : int;
    mutable emule_support_crypt : int;
    mutable emule_extmultipacket : int;
    mutable emule_largefiles : int;
    mutable emule_kad_version : int;
    mutable emule_support_captcha : int;
  }

type emule_tag_name =
| ET_COMPRESSION
| ET_UDPPORT
| ET_UDPVER
| ET_SOURCEEXCHANGE
| ET_COMMENTS
| ET_EXTENDEDREQUEST
| ET_COMPATIBLECLIENT
| ET_MOD_FEATURESET
| ET_MOD_PROTOCOL
| ET_MOD_VERSION
| ET_MOD_TAROD
| ET_TAROD_VERSION
| ET_MOD_PLUS

type request_record = {
  mutable last_request : int;
  mutable nwarnings : int;
}

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
| Brand_emuleplus
| Brand_hydranode
| Brand_verycd
| Brand_imp

let brand_list = [
   ( Brand_unknown   , "unknown"      , "unk" ) ;
   ( Brand_edonkey   , "eDonkey"      , "eDK" ) ;
   ( Brand_mldonkey1 , "old mldonkey" , "oML" ) ;
   ( Brand_mldonkey2 , "new mldonkey" , "nML" ) ;
   ( Brand_overnet   , "Overnet"      , "OVR" ) ;
   ( Brand_newemule  , "eMule"        , "eMU" ) ;
   ( Brand_server    , "server"       , "SER" ) ;
   ( Brand_mldonkey3 , "trusted mld"  , "tML" ) ;
   ( Brand_cdonkey   , "cDonkey"      , "cDK" ) ;
   ( Brand_lmule     , "xMule"        , "xMU" ) ;
   ( Brand_shareaza  , "shareaza"     , "sZA" ) ;
   ( Brand_amule     , "aMule"        , "aMU" ) ;
   ( Brand_lphant    , "lPhant"       , "lPH" ) ;
   ( Brand_emuleplus , "ePlus"        , "eM+" ) ;
   ( Brand_hydranode , "Hydra"        , "Hyd" ) ;
   ( Brand_verycd    , "VeryCD"       , "VCD" ) ;
   ( Brand_imp       , "IMPmule"      , "IMP" ) ;
  ]

let brand_count = List.length brand_list

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
| Brand_mod_airionix
| Brand_mod_ionix
| Brand_mod_tornado
| Brand_mod_antifaker
| Brand_mod_netf
| Brand_mod_nextemf
| Brand_mod_proemule
| Brand_mod_szemule
| Brand_mod_darkmule
| Brand_mod_miragemod
| Brand_mod_nextevolution
| Brand_mod_pootzgrila
| Brand_mod_freeangel
| Brand_mod_enos
| Brand_mod_webys

let brand_mod_list = [
   ( Brand_mod_unknown       , "unknown"       , ""     ) ;
   ( Brand_mod_extasy        , "Extasy"        , "ext"  ) ;
   ( Brand_mod_hunter        , "Hunter"        , "hun"  ) ;
   ( Brand_mod_sivka         , "Sivka"         , "siv"  ) ;
   ( Brand_mod_ice           , "IcE"           , "ice"  ) ;
   ( Brand_mod_plus          , "Plus"          , "plu"  ) ;
   ( Brand_mod_lsd           , "LSD"           , "lsd"  ) ;
   ( Brand_mod_maella        , "Maella"        , "mae"  ) ;
   ( Brand_mod_pille         , "Pille"         , "pil"  ) ;
   ( Brand_mod_morphkad      , "MorphKad"      , "mo1"  ) ;
   ( Brand_mod_efmod         , "eF-MOD"        , "efm"  ) ;
   ( Brand_mod_xtreme        , "Xtreme"        , "xtr"  ) ;
   ( Brand_mod_bionic        , "Bionic"        , "bio"  ) ;
   ( Brand_mod_pawcio        , "Pawcio"        , "paw"  ) ;
   ( Brand_mod_zzul          , "ZZUL"          , "zzu"  ) ;
   ( Brand_mod_blackhand     , "Black Hand"    , "bla"  ) ;
   ( Brand_mod_lovelace      , "lovelace"      , "lov"  ) ;
   ( Brand_mod_morphnext     , "MorphNext"     , "mo2"  ) ;
   ( Brand_mod_fincan        , "fincan"        , "fin"  ) ;
   ( Brand_mod_ewombat       , "eWombat"       , "ewo"  ) ;
   ( Brand_mod_morph         , "Morph"         , "mo3"  ) ;
   ( Brand_mod_mortillo      , "MorTillo"      , "mot"  ) ;
   ( Brand_mod_lh            , "LionHeart"     , "lh"   ) ;
   ( Brand_mod_emulespana    , "emulEspa\241a" , "esp"  ) ;
   ( Brand_mod_blackrat      , "BlackRat"      , "blr"  ) ;
   ( Brand_mod_enkeydev      , "enkeyDev"      , "ekd"  ) ;
   ( Brand_mod_gnaddelwarz   , "Gnaddelwarz"   , "gna"  ) ;
   ( Brand_mod_phoenixkad    , "pHoeniX-KAD"   , "pkd"  ) ;
   ( Brand_mod_koizo         , "koizo"         , "koi"  ) ;
   ( Brand_mod_ed2kfiles     , "ed2kFiles"     , "edf"  ) ;
   ( Brand_mod_athlazan      , "Athlazan"      , "ath"  ) ;
   ( Brand_mod_cryptum       , "Cryptum"       , "cry"  ) ;
   ( Brand_mod_lamerzchoice  , "LamerzChoice"  , "lam"  ) ;
   ( Brand_mod_notdead       , "NotDead"       , "nod"  ) ;
   ( Brand_mod_peace         , "peace"         , "pea"  ) ;
   ( Brand_mod_goldicryptum  , "GoldiCryptum"  , "gcr"  ) ;
   ( Brand_mod_eastshare     , "EastShare"     , "eas"  ) ;
   ( Brand_mod_mfck          , "[MFCK]"        , "mfc"  ) ;
   ( Brand_mod_echanblard    , "eChanblard"    , "ech"  ) ;
   ( Brand_mod_sp4rk         , "Sp4rK"         , "sp4"  ) ;
   ( Brand_mod_powermule     , "PowerMule"     , "pow"  ) ;
   ( Brand_mod_bloodymad     , "bloodymad"     , "blo"  ) ;
   ( Brand_mod_roman2k       , "Roman2K"       , "rom"  ) ;
   ( Brand_mod_gammaoh       , "GaMMaOH"       , "gam"  ) ;
   ( Brand_mod_elfenwombat   , "ElfenWombat"   , "elf"  ) ;
   ( Brand_mod_o2            , "O2"            , "o2"   ) ;
   ( Brand_mod_dm            , "DM"            , "DM"   ) ;
   ( Brand_mod_sfiom         , "SF-IOM"        , "SFI"  ) ;
   ( Brand_mod_magic_elseve  , "Magic-Elseve"  , "MEl"  ) ;
   ( Brand_mod_schlumpmule   , "SchlumpMule"   , "sch"  ) ;
   ( Brand_mod_lc            , "LC"            , "LC"   ) ;
   ( Brand_mod_noamson       , "NoamSon"       , "NoS"  ) ;
   ( Brand_mod_stormit       , "Stormit"       , "Sto"  ) ;
   ( Brand_mod_omax          , "OMaX"          , "OMX"  ) ;
   ( Brand_mod_mison         , "Mison"         , "Mis"  ) ;
   ( Brand_mod_phoenix       , "Phoenix"       , "pPho" ) ;
   ( Brand_mod_spiders       , "Spiders"       , "spi"  ) ;
   ( Brand_mod_iberica       , "Ib\233rica"    , "Ib"   ) ;
   ( Brand_mod_mortimer      , "Mortimer"      , "mor"  ) ;
   ( Brand_mod_stonehenge    , "Stonehenge"    , "sto"  ) ;
   ( Brand_mod_xlillo        , "Xlillo"        , "Xli"  ) ;
   ( Brand_mod_imperator     , "ImperatoR"     , "Imp"  ) ;
   ( Brand_mod_raziboom      , "Raziboom"      , "Raz"  ) ;
   ( Brand_mod_khaos         , "Khaos"         , "Kha"  ) ;
   ( Brand_mod_hardmule      , "Hardmule"      , "Har"  ) ;
   ( Brand_mod_sc            , "SC"            , "SC"   ) ;
   ( Brand_mod_cy4n1d        , "Cy4n1d"        , "Cy4"  ) ;
   ( Brand_mod_dmx           , "DMX"           , "DMX"  ) ;
   ( Brand_mod_ketamine      , "Ketamine"      , "Ket"  ) ;
   ( Brand_mod_blackmule     , "Blackmule"     , "blm"  ) ;
   ( Brand_mod_morphxt       , "MorphXT"       , "Mxt"  ) ;
   ( Brand_mod_ngdonkey      , "ngdonkey"      , "ngd"  ) ;
   ( Brand_mod_cyrex         , "Cyrex"         , "haw"  ) ;
   ( Brand_mod_hawkstar      , "Hawkstar"      , "neo"  ) ;
   ( Brand_mod_neomule       , "Neo Mule"      , "cyr"  ) ;
   ( Brand_mod_aldo          , "aldo"          , "ald"  ) ;
   ( Brand_mod_emulede       , "emule.de"      , "ede"  ) ;
   ( Brand_mod_zx            , "zx"            , "zx"   ) ;
   ( Brand_mod_ibericaxt     , "ib\233ricaxt"  , "iBx"  ) ;
   ( Brand_mod_candymule     , "candy-mule"    , "can"  ) ;
   ( Brand_mod_ackronic      , "ackronic"      , "ack"  ) ;
   ( Brand_mod_rappis        , "rappis"        , "rap"  ) ;
   ( Brand_mod_overdose      , "overdose"      , "ove"  ) ;
   ( Brand_mod_hebmule       , "hebmule"       , "heb"  ) ;
   ( Brand_mod_senfei        , "senfei"        , "sen"  ) ;
   ( Brand_mod_spoofmod      , "spoofmod"      , "spo"  ) ;
   ( Brand_mod_fusspilz      , "fusspilz"      , "fus"  ) ;
   ( Brand_mod_rocket        , "rocket"        , "roc"  ) ;
   ( Brand_mod_warezfaw      , "warezfaw"      , "war"  ) ;
   ( Brand_mod_emusicmule    , "emusicmule"    , "emm"  ) ;
   ( Brand_mod_aideadsl      , "aideadsl"      , "aid"  ) ;
   ( Brand_mod_epo           , "epo"           , "epo"  ) ;
   ( Brand_mod_kalitsch      , "kalitsch"      , "kal"  ) ;
   ( Brand_mod_raynz         , "raynz"         , "ray"  ) ;
   ( Brand_mod_serverclient  , "serverclient"  , "sc"   ) ;
   ( Brand_mod_bl4ckbird     , "bl4ckbird"     , "b4b"  ) ;
   ( Brand_mod_bl4ckf0x      , "bl4ckf0x"      , "b4f"  ) ;
   ( Brand_mod_rt            , "rt"            , "rt"   ) ;
   ( Brand_mod_airionix      , "air-ionix"     , "aio"  ) ;
   ( Brand_mod_ionix         , "ionix"         , "ion"  ) ;
   ( Brand_mod_tornado       , "tornado"       , "tor"  ) ;
   ( Brand_mod_antifaker     , "anti-faker"    , "anf"  ) ;
   ( Brand_mod_netf          , "netf"          , "nef"  ) ;
   ( Brand_mod_nextemf       , "nextemf"       , "nxf"  ) ;
   ( Brand_mod_proemule      , "proemule"      , "pem"  ) ;
   ( Brand_mod_szemule       , "szemule"       , "sze"  ) ;
   ( Brand_mod_darkmule      , "darkmule"      , "dar"  ) ;
   ( Brand_mod_miragemod     , "miragemod"     , "mir"  ) ;
   ( Brand_mod_nextevolution , "nextevolution" , "nxe"  ) ;
   ( Brand_mod_pootzgrila    , "pootzgrila"    , "poo"  ) ;
   ( Brand_mod_freeangel     , "freeangel"     , "fre"  ) ;
   ( Brand_mod_enos          , "enos"          , "eno"  ) ;
   ( Brand_mod_webys         , "webys"         , "wys"  ) ;
  ]

let brand_mod_count = List.length brand_mod_list

let brand_to_string brand =
  find_brand_to_string brand brand_list false

let brand_to_string_short brand =
  find_brand_to_string brand brand_list true

let brand_mod_to_string brand =
  find_brand_to_string brand brand_mod_list false

let brand_mod_to_string_short brand =
  find_brand_to_string brand brand_mod_list true

let brand_to_int brand =
  find_int_of_brand brand brand_list

let brand_mod_to_int brand =
  find_int_of_brand brand brand_mod_list

type source_uid =
  Direct_address of Ip.t * int
| Indirect_address of Ip.t * int * int64 * int * Ip.t
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

      let indirect_source s =
        match s with Indirect_address _ -> true | _ -> false

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
            Indirect_address (ip, port, id, 0, Ip.null)
        | _ ->
            failwith "bad client address"

      let source_uid_to_value s =
        match s with
          Direct_address (ip, port) ->
            SmallList [string_to_value (Ip.to_string ip); int_to_value port]
        | Indirect_address (server_ip, server_port, id, port, _) ->
            SmallList [
              string_to_value (Ip.to_string server_ip);
              int_to_value server_port;
              int64_to_value id;
            ]
        | Invalid_address _ -> failwith "Invalid address"
    end)

type file = {
    file_file : file CommonFile.file_impl;
    file_md4 : Md4.t;
    mutable file_swarmer : CommonSwarming.t option;
    mutable file_nchunks : int;
    mutable file_nchunk_hashes : int;
    mutable file_diskname : string;
    mutable file_computed_md4s : Md4.t array;
    mutable file_format : format;
    mutable file_shared : file CommonShared.shared_impl option;
    mutable file_sources : DonkeySources.file_sources_manager;
    mutable file_comments : (Ip.t * string * int * string) list;
  }

and server = (*[]*){
    mutable server_server : server CommonServer.server_impl;
    mutable server_ip : Ip.t;
    mutable server_cid : Ip.t option;
    mutable server_port : int;
    mutable server_realport : int option; (* in case we connect through auxport; this is the true one *)
    mutable server_country_code : int option;
    mutable server_sock : tcp_connection;
    mutable server_search_queries : CommonTypes.search Fifo.t;
    mutable server_users_queries : bool Fifo.t;
    mutable server_connection_control : connection_control;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int64 option;
    mutable server_nfiles : int64 option;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_banner : string;
    mutable server_users: user list;
    mutable server_next_udp : int;
    mutable server_master : bool;
    mutable server_preferred : bool;
    mutable server_last_ping : float; (* time last PingServerUdpReq was sent *)
    mutable server_next_ping : float; (* time next PingServerUdpReq will be sent *)
    mutable server_descping_counter : int;
    mutable server_ping : int;
    mutable server_udp_ping_challenge : int64 option;
    mutable server_udp_desc_challenge : int64 option;
(* reset after answer received, increased after UDP ping sent, server deleted after 10 attempts *)
    mutable server_failed_count : int; 

    mutable server_id_requests : file option Fifo.t;

    mutable server_queries_credit : int;
    mutable server_waiting_queries : file list;
    mutable server_sent_all_queries : bool;
    mutable server_has_zlib : bool;
    mutable server_has_newtags : bool;
    mutable server_has_unicode : bool;
    mutable server_has_related_search : bool;
    mutable server_has_tag_integer : bool;
    mutable server_has_largefiles : bool;
    mutable server_obfuscation_tcp : int option;
    mutable server_obfuscation_udp : int option;
    mutable server_dynip : string;
    mutable server_auxportslist : string;
    mutable server_has_get_sources : bool;
    mutable server_has_get_files : bool;
    mutable server_has_get_sources2 : bool;

    mutable server_flags : int;
    mutable server_version : string;
    mutable server_lowid_users : int64 option;
    mutable server_soft_limit : int64 option;
    mutable server_hard_limit : int64 option;
    mutable server_max_users : int64 option;
    mutable server_sent_shared : file list
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

and client = {
    client_client : client CommonClient.client_impl;
    mutable client_kind : source_uid;
    client_source : DonkeySources.source;
    mutable client_md4 : Md4.t;
    mutable client_ip : Ip.t;
    mutable client_country_code : int option;
    mutable client_download : (file * CommonSwarming.uploader) option;
    mutable client_file_queue : (
      file * (* has displayed when connected *)
      Bitv.t *
      CommonSwarming.uploader
      ) list;
    mutable client_all_files : result list option;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
    mutable client_rating : int ;
    mutable client_upload : upload_info option;
    mutable client_checked : bool;
    mutable client_connected : bool;
(* statistics *)
    mutable client_session_downloaded : Int64.t;
    mutable client_session_uploaded : Int64.t;
    mutable client_total_downloaded : Int64.t;
    mutable client_total_uploaded : Int64.t;
    mutable client_brand : brand;
    mutable client_brand_mod : brand_mod;
    mutable client_osinfo_sent : bool;
    mutable client_osinfo : string option;
    mutable client_banned : bool;
    mutable client_rank : int;
    mutable client_connect_time : int;
    mutable client_requests_received : int;
    mutable client_requests_sent: int;
    mutable client_slot : slot_status;
    mutable client_debug : bool;
    mutable client_pending_messages: string list;
    client_emule_proto : emule_proto;
    mutable client_comp : compressed_parts option;
    mutable client_connection_time : int;
    mutable client_sent_challenge : Int64.t;
    mutable client_req_challenge : Int64.t;
    mutable client_public_key : string option;
    mutable client_sui_verified : bool option;
    mutable client_last_file_req_md4 : Md4.t option;
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
    (* zones sent but not yet received by other peer, oldest first *)
    mutable up_flying_chunks : (int64 * int64) list; 
    mutable up_current : int64;
    mutable up_finish : bool;
    mutable up_waiting : bool;
  }

and client_kind =
  SourceClient of client
| SourceLastConnection of
  int *
  int * (* last connection attempt *)
  int   (* booked client num *)

and file_to_share = {
    shared_name : string;
    shared_size : int64;
    mutable shared_list : Md4.t list;
    mutable shared_pos : int64;
    mutable shared_chunk : int;
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

let dummy_emule_proto = {
    emule_version = 0;
    emule_release = "";
    emule_osinfosupport = 0;
    emule_features = 0;

(* emule_miscoptions1 *)
    received_miscoptions1 = false;
    emule_aich = 0;
    emule_unicode = 0;
    emule_udpver = 0;
    emule_compression = 0; (* 1 *)
    emule_secident = 0;
    emule_sourceexchange = 0; (* 3 *)
    emule_extendedrequest = 0; (* 2 *)
    emule_comments = 0;
    emule_peercache = 0;
    emule_noviewshared = 0;
    emule_multipacket = 0;
    emule_supportpreview = 0;

(* emule_miscoptions2 *)
    received_miscoptions2 = false;
    emule_require_crypt = 0;
    emule_request_crypt = 0;
    emule_support_crypt = 0;
    emule_extmultipacket = 0;
    emule_largefiles = 0;
    emule_kad_version = 0;
    emule_support_captcha = 0;
  }

let emule_proto () =
  { dummy_emule_proto with emule_version = 0 }

let old_max_emule_file_size = 4290048000L
(* #define OLD_MAX_EMULE_FILE_SIZE 4290048000ui64  // (4294967295/PARTSIZE)*PARTSIZE = ~4GB *)

let max_emule_file_size = 0x4000000000L
(* #define MAX_EMULE_FILE_SIZE             0x4000000000ui64 // = 2^38 = 256GB *)
