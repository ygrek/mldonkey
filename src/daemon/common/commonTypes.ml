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

open Options
open Printf2
open Md4 

type activity = {
    activity_begin : int;
    mutable activity_client_overnet_connections : int;
    mutable activity_client_overnet_indirect_connections : int;
    mutable activity_client_overnet_successful_connections : int;
    
    mutable activity_client_edonkey_connections : int;
    mutable activity_client_edonkey_indirect_connections : int;
    mutable activity_client_edonkey_successful_connections : int;

    mutable activity_server_edonkey_connections : int;    
    mutable activity_server_edonkey_successful_connections : int;
  }
 
type uid_type =
| Bitprint of Sha1.t * TigerTree.t
| Sha1 of Sha1.t
| Md5 of Md5.t
| Ed2k of Md4.t
| TigerTree of TigerTree.t
| Md5Ext of Md5Ext.t     (* for Fasttrack *)
| BTUrl of Sha1.t
| FileTP of Md4.t
| NoUid
  
and file_uid_id = 
| BITPRINT
| SHA1
| ED2K
| MD5
| MD5EXT
| TIGER
| FILETP

let string_of_uid_sep uid sep =
  match uid with
    Bitprint (sha1,ttr) ->
      "urn" ^ sep ^ "bitprint" ^ sep ^ (Sha1.to_string sha1) ^ "." ^ (TigerTree.to_string ttr)
  | Sha1 sha1 ->
      "urn" ^ sep ^ "sha1" ^ sep ^ (Sha1.to_string sha1)
  | Ed2k ed2k ->
      "urn" ^ sep ^ "ed2k" ^ sep ^ (Md4.to_string ed2k)
  | Md5 md5 ->
      "urn" ^ sep ^ "md5" ^ sep ^ (Md5.to_string md5)
  | TigerTree ttr -> 
      "urn" ^ sep ^ "tree" ^ sep ^ "tiger" ^ sep ^ (TigerTree.to_string ttr)
  | Md5Ext md5 ->
      "urn" ^ sep ^ "sig2dat" ^ sep ^ (Md5Ext.to_base32 md5)
  | BTUrl url ->
      "urn" ^ sep ^ "btih" ^ sep ^ (Sha1.to_string url)
  | FileTP file ->
      "urn" ^ sep ^ "filetp" ^ sep ^ (Md4.to_string file)
  | NoUid -> ""

let string_of_uid uid =
  string_of_uid_sep uid ":"

(* This should be used for temp. files, because for some
   systems the character ":" may be illegal for filenames.
 *)
let file_string_of_uid uid =
  string_of_uid_sep uid "_"

exception Illegal_urn of string
exception Torrent_started of string
exception Torrent_already_exists of string

let uid_of_string s =
  let s = String.lowercase s in
  let urn = String2.before s 4 in
  let rem = String2.after s 4 in
  let sep = ref ':' in
  (match urn with
   | "urn:" -> sep := ':'
   | "urn_" -> sep := '_'
   | _ -> raise (Illegal_urn (s ^ " at " ^ urn ^ " should be urn: or urn_"))
  );
  let (sign, rem) = String2.cut_at rem !sep in
  match sign with
  | "ed2k" -> Ed2k (Md4.of_string rem)
(*   | "aich" -> ??? *)
  | "bitprint" | "bp" -> 
      let (sha1, ttr) = String2.cut_at rem '.' in
      let sha1 = Sha1.of_string sha1 in
      let tiger = TigerTree.of_string ttr in
      Bitprint (sha1, tiger)
  | "sha1" -> Sha1 (Sha1.of_string rem)
  | "tree" ->
      let (tiger, rem) = String2.cut_at rem ':' in
      if tiger <> "tiger" then 
        raise (Illegal_urn (s ^ " at " ^ tiger ^ " should be tiger:"));
      TigerTree (TigerTree.of_string rem)
  | "ttr" -> TigerTree (TigerTree.of_string rem)
  | "md5" ->  Md5 (Md5.of_string rem)
  | "sig2dat" -> Md5Ext (Md5Ext.of_base32 rem)
  | "bt" | "btih" ->
    (* accept both base32 (32 chars) and base16 (40 chars) *)
    BTUrl (if String.length rem = 40 then Sha1.of_hexa rem else Sha1.of_string rem)
  | "filetp" -> FileTP (Md4.of_string rem)
  | _ -> raise (Illegal_urn (s ^ " at " ^ sign ^ " is not known"))

module Uid : sig
    type t
    val create : uid_type -> t
    val to_string : t -> string
    val to_file_string : t -> string
    val of_string : string -> t
    val to_uid : t -> uid_type
    val mem : t -> t list -> bool
    val add : t -> t list -> t list
    val derive : t -> t list
    val expand : t list -> t list
    val option : t option_class

    val compare : t -> t -> int
    val no : t

  end = struct

    type t = {
        mutable uid_string : string;
        mutable uid_type : uid_type;
      }

    let no = {
        uid_type = NoUid;
        uid_string = "";
      }

    let create uid = {
        uid_string = string_of_uid uid ;
        uid_type = uid;
      }

    let of_string s = 
      let uid = uid_of_string s in
      let s = string_of_uid uid in
      {
        uid_string = s;
        uid_type = uid;
        }

    let to_string t = t.uid_string

    let to_file_string t =
      file_string_of_uid t.uid_type

    let to_uid t = t.uid_type

    let derive uid =
      match uid.uid_type with
         (Bitprint (sha1, tiger)) ->
          [create (Sha1 (sha1)); create (TigerTree(tiger))]
      | _ -> []

    let compare u1 u2 =
      compare (to_string u1) (to_string u2)

    let rec mem uid list =
      match list with
        [] -> false
      | t :: tail ->
          compare t uid = 0 || mem uid tail

    let add uid list =
      if not (mem uid list) then
        uid :: list
      else 
        list

    let rec expand_rec list uids =
      match list with
        [] -> uids
      | t :: tail ->
          let list = derive t in
          expand_rec (list@tail) (add t uids)

    let expand uids = expand_rec uids []

    let option = define_option_class "Uid"
        (fun v -> of_string (value_to_string v))
      (fun uid -> string_to_value (to_string uid))
  end

(* TODO group settings: xt.1 xt.2 .. *)
let parse_magnet_url url =
  let url = Url.of_string url in
  if url.Url.short_file = "magnet:" then
    let uids = ref [] in
    let name = ref "" in
    let size = ref None in
    let trackers = ref [] in
    let each k v =
      match String2.split k '.' with
      | "xt"::_ -> uids := Uid.of_string v :: !uids
      | "xl"::_ -> size := Some (Int64.of_string v) (* exact length *)
      | "dn"::_ -> name := Url.decode v
      | "as"::_ -> () (* acceptable source *)
      | "xs"::_ -> () (* eXtra source *)
      | "mt"::_ -> () (* manifest topic: url or urn, see http://rakjar.de/gnuticles/MAGMA-Specsv22.txt *)
      | "kt"::_ -> () (* keywords topic *)
      | "tr"::_ -> trackers := Url.decode v :: !trackers
      | "x"::_ -> () (* extensions *)
(*
      | _ when v = "" ->
(* This is an error in the magnet, where a & has been kept instead of being
  url-encoded *)
          name := Printf.sprintf "%s&%s" !name k
*)
      | _ -> lprintf_nl "MAGNET: unused field %S=%S" k v
    in
    List.iter (fun (k, v) ->
      try each k v
      with exn -> lprintf_nl "MAGNET: field %S=%S, exn %s" k v (Printexc2.to_string exn)
    ) url.Url.args;
    object
      method name = !name
      method size = !size
      method uids = List.map Uid.to_uid (Uid.expand !uids)
      method trackers = !trackers
    end
  else 
    raise Not_found

let show_magnet_url x =
  let args = ("dn", x#name) :: List.map (fun uid -> "xt", string_of_uid uid) x#uids in
  let args = match x#size with Some n -> ("xl", Int64.to_string n) :: args | None -> args in
  Url.put_args "magnet:" args

(* compatibility, used in G2 module *)
let parse_magnet url =
  let magnet = parse_magnet_url url in
  magnet#name, (List.map Uid.create magnet#uids)

let string_of_uids list =
  match list with
    [] -> "<unknown>"
  | uid :: _ -> Uid.to_string uid

type file_state =
  FileDownloading
| FileQueued
| FilePaused
| FileDownloaded
| FileShared
| FileCancelled
| FileNew
| FileAborted of string

  
let string_of_state file_state =
  match file_state with
  FileDownloading -> "FileDownloading"
| FileQueued -> "FileQueued"
| FilePaused -> "FilePaused"
| FileDownloaded -> "FileDownloaded"
| FileShared -> "FileShared"
| FileCancelled -> "FileCancelled"
| FileNew -> "FileNew"
| FileAborted _ -> "FileAborted"

  
  
  
  
  
type tag_value =
| Uint64 of int64
| Fint64 of int64  (* Why do we keep that one, it is supposed to be a float ? *)
| String of string
| Addr of Ip.t
| Uint16 of int
| Uint8 of int
| Pair of int64 * int64
  
type field =
| Field_Artist (* "Artist" *)
| Field_Title  (* "Title" *)
| Field_Album  (* "Album" *)
| Field_Format (* "format" *)
| Field_Type   (* "type" *)
| Field_Length (* "length" *)
| Field_Bitrate (* "bitrate" *)
| Field_Codec   (* "codec" *)
| Field_Availability (* "availability" or "avail" *)
| Field_Completesources (* "completesources" *)
| Field_Filename (* "filename" *)
| Field_Size
| Field_Size_Hi
| Field_Uid
| Field_Filerating
| Field_Lastseencomplete
| Field_Mediacodec
| Field_Medialength
| Field_KNOWN of string
| Field_UNKNOWN of string
  
type tag = {
    mutable tag_name : field;
    mutable tag_value : tag_value;
  }

  
type query =
  QAnd of query * query
| QOr of query * query
| QAndNot of query * query
| QHasWord of string
| QHasField of field * string
| QHasMinVal of field * int64
| QHasMaxVal of field * int64
| QNone (** temporary, used when no value is available ;
           must be simplified before transforming into strings *)

  
type client_type = int
let client_friend_tag = 1
let client_contact_tag = 2
let client_nolimit_tag = 4
let client_initialized_tag = 8
let client_must_browse_tag = 16

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

(* only internal to GUI *)
| Q_COMBO of string * string * string list
  
| Q_HIDDEN of query_entry list
  
(*
We have to add more information on what has happened to the
connection in the NotConnected state.
*)
type host_state =
| NotConnected of BasicSocket.close_reason * int (* >= 0 Queued *)
| Connecting
| Connected_initiating
| Connected of int    (* >= 0 Queued *)
| Connected_downloading of int
(* | ConnectionWaiting of int  *)
| NewHost
| RemovedHost
| BlackListedHost
| ServerFull

type compressor = 
  Deflate of Zlib.stream * Zlib.stream

type tcp_connection =
| NoConnection        (* No current connection *)
| ConnectionWaiting of TcpBufferedSocket.token
    (* Waiting for a connection slot to open locally *)
| Connection of TcpBufferedSocket.t (* We are connected *)

type sharing_strategy = {
    sharing_incoming : bool;
    sharing_directories : bool;
    sharing_recursive : bool;
    sharing_minsize : int64;
    sharing_maxsize : int64;
    sharing_extensions : string list;
  }

type shared_directory = {
    shdir_dirname : string;
    mutable shdir_priority : int;
    shdir_strategy : string;
    shdir_networks : string list;
  }
  
type connection_control = {
    mutable control_last_ok : int;
    mutable control_state : int;
    mutable control_last_try : int;
    mutable control_min_reask : int;
  }

type output_type = TEXT | HTML | ANSI | XML | XHTML

type connection_type = TELNET | WEB | GUI | GIFT | CALENDAR

let connection_type_to_text ct =
  match ct with
  | TELNET -> "Telnet"
  | WEB -> "Web"
  | GUI -> "GUI"
  | GIFT -> "Gift"
  | CALENDAR -> "Calendar"

type sortvd_type = 
  BySize
| ByName
| ByRate
| ByDone
| ByPercent
| ByPriority
| ByAge
| ByETA
| ByLast
| BySources
| ByASources
| ByNet
| ByAvail
| ByComments
| ByUser
| ByGroup
| NotSorted
  
type room_state = 
| RoomOpened
| RoomClosed
| RoomPaused
  
type file
type server
type client
  
type result_info = {
    mutable result_num : int;    
    
    mutable result_uids : Uid.t list; (* net file UID *)
    mutable result_names : string list;
    mutable result_size : int64;
    mutable result_format : string;
    mutable result_type : string;
    mutable result_tags : tag list;
    mutable result_comment : string;
    mutable result_done : bool;
    mutable result_force : bool;

    mutable result_time : int;
    mutable result_modified : bool;
    mutable result_source_network : int;
  }

type result = {
    stored_result_num : int;
    mutable stored_result_index : Store.index;
  }
  

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
  
type network_flag =
  NetworkHasServers
| NetworkHasRooms
| NetworkHasMultinet
| NetworkHasSearch
| VirtualNetwork
| UnknownNetworkFlag
| NetworkHasChat
| NetworkHasSupernodes
| NetworkHasUpload
| NetworkHasStats
  
type network_info = {
    network_netname : string;
    network_netnum : int;
    network_config_filename : string;
    network_netflags : network_flag list;
    network_enabled : bool;
    network_uploaded : int64;
    network_downloaded : int64;
    network_connected_servers : int;
  }

type extend_search =
  ExtendSearchLocally
| ExtendSearchRemotely
  
type network_stat_info  = {
  mutable string_long : string;
  mutable string_short : string;
  mutable seen : int;
  mutable banned : int;
  mutable filerequest : int;
  mutable download : Int64.t;
  mutable upload : Int64.t;
}

type network_porttest =
  PorttestNotAvailable
| PorttestNotStarted
| PorttestInProgress of int
| PorttestResult of int * string

type groupdb = {
  group_name : string;
  mutable group_admin : bool;
}

and userdb = {
  user_name : string;
  mutable user_pass : Md4.t;
  mutable user_groups : groupdb list;
  mutable user_default_group : groupdb option;
  mutable user_mail : string;
  mutable user_commit_dir : string;
  mutable user_max_concurrent_downloads : int;
}

and network = {
    network_name : string;
    network_num : int;
    network_connection_manager : TcpBufferedSocket.connection_manager;
    network_shortname: string;
    mutable network_flags : network_flag list;
    mutable network_config_file : Options.options_file list;
    mutable op_network_connected_servers : (unit -> server list);
    mutable op_network_is_enabled : (unit -> bool);
    mutable op_network_save_complex_options : (unit -> unit);
    mutable op_network_save_sources : (unit -> unit);
    mutable op_network_load_complex_options : (unit -> unit);
    mutable op_network_enable : (unit -> unit);
    mutable op_network_update_options : (unit -> unit);
    mutable op_network_disable : (unit -> unit);
    
    mutable op_network_server_of_option : 
      ((string * Options.option_value) list -> server);
    mutable op_network_add_server : 
      (Ip.addr -> int -> server);
    mutable op_network_file_of_option : 
      int64 -> file_state -> userdb -> groupdb option -> ((string * Options.option_value) list -> file);
    mutable op_network_client_of_option : 
      bool -> ((string * Options.option_value) list -> client);
    mutable op_network_recover_temp : (unit -> unit);
    mutable op_network_share : (
      string -> string -> int64 -> unit);
    mutable op_network_private_message : (string -> string -> unit);
    mutable op_network_parse_url : (string -> userdb -> groupdb option -> string * bool);
    mutable op_network_connect_servers : (unit -> unit);
    
    mutable op_network_search : (search -> Buffer.t -> unit);
    mutable op_network_forget_search : (search -> unit);
    mutable op_network_close_search : (search -> unit);
    mutable op_network_extend_search : (search -> extend_search -> unit);
    
    mutable op_network_clean_servers : (unit -> unit);
    mutable op_network_info : (unit -> network_info);
    
    mutable op_network_connected : (unit -> bool);
    mutable op_network_gui_message : (string -> userdb -> unit);
    
    mutable op_network_download : (result_info -> userdb -> groupdb option -> file);
    mutable op_network_display_stats : (ui_conn -> unit);
    mutable op_network_stat_info_list : unit -> (string * int * (network_stat_info list)) list;
    mutable op_network_clean_exit : (unit -> bool);
    mutable op_network_reset : (unit -> unit);
    mutable op_network_ports : (unit -> (int * string) list);
    mutable op_network_porttest_start : (unit -> unit);
    mutable op_network_porttest_result : (unit -> network_porttest);
    mutable op_network_check_upload_slots : (unit -> unit);
  }

and ui_user = {
    ui_user : userdb;
    mutable ui_user_searches : search list;
    mutable ui_last_search : search option;
    mutable ui_last_results : (int * result) list;
    mutable ui_http_conn : ui_conn option;
  }
      
and ui_conn = {
    mutable conn_output : output_type; 
    mutable conn_sortvd : sortvd_type;
    mutable conn_filter : (result_info -> unit);
    mutable conn_buf : Buffer.t;
    mutable conn_user : ui_user;
    mutable conn_width : int;
    mutable conn_height : int;
    mutable conn_info : (connection_type * (Ip.t * int)) option;
  }

  

and search = {
    search_num : int;
    search_time : int;
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
    mutable search_network : int;
  }

exception CommandCloseSocket
  
  
let version = 19
  

type avi_info = {
    mutable avi_codec : string;
    mutable avi_width : int;
    mutable avi_height : int;
    mutable avi_fps : int;
    mutable avi_rate : int;
  }

type ogg_stream_type =
  OGG_INDEX_STREAM                             (* as per OggDS new Header Format      *)
| OGG_TEXT_STREAM                              (* as per OggDS new Header Format      *)
| OGG_AUDIO_STREAM                             (* as per OggDS new Header Format      *)
| OGG_VORBIS_STREAM                            (* as per Ogg Vorbis I specification   *)
| OGG_THEORA_STREAM                            (* as per Theora I specification       *)
| OGG_VIDEO_STREAM                             (* as per OggDS new Header Format      *)

type vorbis_bitrates =
  Maximum_br of float                          (* maximum bitrate in kbit/s           *)
| Nominal_br of float                          (* nominal bitrate in kbit/s           *)
| Minimum_br of float                          (* minimum bitrate in kbit/s           *)

type theora_color_space =
  CSUndefined
| CSRec470M
| CSRec470BG

type ogg_tag =
  Ogg_codec of string
| Ogg_bits_per_samples of int                 (* in bits                             *)
| Ogg_duration of int                         (* duration in seconds                 *)
| Ogg_has_subtitle
| Ogg_has_index
| Ogg_audio_channels of int                   (* number of audio channels            *)
| Ogg_audio_sample_rate of float              (* in sample/s                         *)
| Ogg_audio_blockalign of int
| Ogg_audio_avgbytespersec of float           (* in bytes/s                          *)
| Ogg_vorbis_version of float                 (* version of vorbis codec. shall be 0 *)
| Ogg_vorbis_sample_rate of float             (* samplerate in Hertz                 *)
| Ogg_vorbis_bitrates of vorbis_bitrates list
| Ogg_vorbis_blocksize_0 of int
| Ogg_vorbis_blocksize_1 of int
| Ogg_video_width of float                    (* in pixel                            *)
| Ogg_video_height of float                   (* in pixel                            *)
| Ogg_video_sample_rate of float              (* in frames/s                         *)
| Ogg_aspect_ratio of float
| Ogg_theora_cs of theora_color_space
| Ogg_theora_quality of int
| Ogg_theora_avgbytespersec of int            (* in bytes/s for VBR streams          *)

type ogg_stream_info = {
  stream_no   : int;
  stream_type : ogg_stream_type;
  stream_tags : ogg_tag list;
}


(**********************************************************************************)
(*                                                                                *)
(*                  stream_type_to_string                                         *)
(*                                                                                *)
(**********************************************************************************)

let stream_type_to_string st =
  match st with
    OGG_VIDEO_STREAM -> "video"
  | OGG_AUDIO_STREAM -> "audio"
  | OGG_INDEX_STREAM -> "index"
  | OGG_TEXT_STREAM -> "text"
  | OGG_VORBIS_STREAM -> "audio"
  | OGG_THEORA_STREAM -> "video"

(**********************************************************************************)
(*                                                                                *)
(*                  vorbis_bitrates_to_string                                     *)
(*                                                                                *)
(**********************************************************************************)

let vorbis_bitrates_to_string vb =
  let s = ref "" in
  List.iter (fun b ->
    match b with
        Maximum_br r -> s := !s ^ (Printf.sprintf "%.0f kbit/s [max]" (r /. 1000.))
      | Nominal_br r -> s := !s ^ (Printf.sprintf "%.0f kbit/s [nom]" (r /. 1000.))
      | Minimum_br r -> s := !s ^ (Printf.sprintf "%.0f kbit/s [min]" (r /. 1000.))
  ) vb;
  !s

(**********************************************************************************)
(*                                                                                *)
(*                  theora_cs_to_string                                           *)
(*                                                                                *)
(**********************************************************************************)

let theora_cs_to_string cs =
  match cs with
      CSUndefined -> "Undefined"
    | CSRec470M -> "470M"
    | CSRec470BG -> "470BG"

(**********************************************************************************)
(*                                                                                *)
(*                  ogg_tag_to_string                                             *)
(*                                                                                *)
(**********************************************************************************)

let ogg_tag_to_string ogg_tag =
  match ogg_tag with
  Ogg_codec s  -> Printf.sprintf "%s [codec]" s
| Ogg_bits_per_samples n -> Printf.sprintf "%d bits/sample" n
| Ogg_duration n -> Printf.sprintf "%d s" n
| Ogg_has_subtitle -> Printf.sprintf "Stream has subtitles"
| Ogg_has_index -> Printf.sprintf "Stream has chapters"
| Ogg_audio_channels n -> Printf.sprintf "%d [channels]" n
| Ogg_audio_sample_rate r -> Printf.sprintf "%.3f samples/s" r
| Ogg_audio_blockalign n -> Printf.sprintf "block align: %d" n
| Ogg_audio_avgbytespersec r -> Printf.sprintf "%.0f kbps" (r *. 8. /. 1000.)
| Ogg_vorbis_version r -> Printf.sprintf "vorbis version: %.0f" r
| Ogg_vorbis_sample_rate r -> Printf.sprintf "%.0f Hz" r
| Ogg_vorbis_bitrates l -> vorbis_bitrates_to_string l
| Ogg_vorbis_blocksize_0 n -> Printf.sprintf "blocksize_0: %d" (int_of_float (2. ** float_of_int n))
| Ogg_vorbis_blocksize_1 n -> Printf.sprintf "blocksize_1: %d" (int_of_float (2. ** float_of_int n))
| Ogg_video_width r -> Printf.sprintf "width: %.0f pixels" r
| Ogg_video_height r -> Printf.sprintf "height: %.0f pixels" r
| Ogg_video_sample_rate r -> Printf.sprintf "%.3f fps" r
| Ogg_aspect_ratio r -> Printf.sprintf "1:%0.5f" r
| Ogg_theora_cs cs -> theora_cs_to_string cs
| Ogg_theora_quality n -> Printf.sprintf "%d [quality]" n
| Ogg_theora_avgbytespersec n -> Printf.sprintf "%d kbps" (n / 1000)

(**********************************************************************************)
(*                                                                                *)
(*                  print_ogg_info                                                *)
(*                                                                                *)
(**********************************************************************************)

let print_ogg_infos ogg_infos =
  List.iter (fun stream ->
    lprintf "-== %s info - stream number: %d ==-\n"
      (stream_type_to_string stream.stream_type) stream.stream_no;
    List.iter (fun tag ->
      lprintf "  %s\n" (ogg_tag_to_string tag)
    ) stream.stream_tags
  ) !ogg_infos

type format =
  AVI of avi_info
| MP3 of Mp3tag.Id3v1.tag * Mp3tag.info
| OGG of ogg_stream_info list
| FormatType of string * string
| FormatUnknown
| FormatNotComputed of int

and history_result = {
    mutable hresult_names : string list;
    hresult_md4 : Md4.t;
    mutable hresult_size : int64;
    hresult_tags : tag list;
  }

and location_kind = 
  Known_location of Ip.t * int
| Indirect_location of string * Md4.t * Ip.t * int
  
(*
We should add information about what has already been sent to the GUI to
prevent sending the same information several times (for example, 
to avoid the Client_info and Result_info message before the Client_file 
message.
*)

type numevents = {
    mutable num_map : bool Intmap.t;
    mutable num_list : int list;
  }
  
type gui_events = {

(* Some kind of FIFO for one time events. These events are uniq for a given
GUI, and are always sent after all other pending events. Thus, if they use
arguments, the events defining the arguments have already been sent *)
    mutable gui_new_events : event list;
    mutable gui_old_events : event list;

    mutable gui_interested_in_sources : bool;

(* Queues of pending events for particular objects: objects updates
are kept here before being sent, so that we can easily check if a
particular update is already pending for a given GUI to avoid
sending it twice. *)
    mutable gui_files : numevents;
    mutable gui_users : numevents;
    mutable gui_clients : numevents;
    mutable gui_servers : numevents; 
    mutable gui_rooms : numevents;
    mutable gui_results : numevents;
    mutable gui_shared_files : numevents;
    mutable gui_networks : numevents;
  
  }
  
and gui_result_handler = int -> result -> unit

and event = 
| Room_add_user_event of room * user
| Room_remove_user_event of room * user
| Room_message_event of int * room * room_message
  
| File_info_event of file
| User_info_event of user
| Client_info_event of client
| Server_info_event of server
| Room_info_event of room
| Result_info_event of result
| Shared_info_event of shared
| Network_info_event of network

| Client_new_file_event of client * string * result
| File_add_source_event of file * client
| File_update_availability of file * client * string
| File_remove_source_event of file * client
| Server_new_user_event of server * user
| Search_new_result_event of gui_result_handler * gui_events * int * result
| Root_console_message_event of string
| Console_message_event of string
  
exception FormatFound of format

       
type tagged_file =  {
    f_md4: Md4.t;
    f_ip: Ip.t;
    f_port: int;
    f_tags: tag list;
  }

  
let is_connected state =
  match state with
  | Connected_initiating
  | Connected_downloading _
  | Connected _ -> true
  | NotConnected _
  | Connecting
  | NewHost
  | BlackListedHost
  | ServerFull
  | RemovedHost -> false

let string_of_connection_state s = 
  match s with
  | Connected (-1) -> "Connected"
  | Connected (-2) -> "Connected"
  | NotConnected (_,n) -> 
      if n = -1 then "" else
      if n = 0 then  "Queued Out" else
      if n > 0 then
        Printf.sprintf "Ranked %d Out" n
      else
        Printf.sprintf "Failed %d" (- n - 1)
        
  | Connected  0 -> "Queued In"
  | Connected  n -> Printf.sprintf "Ranked %d" n
  | Connecting -> "Connecting"
  | Connected_initiating -> "Initiating"
  | Connected_downloading n -> Printf.sprintf "Downloading file %d" n
      
  | RemovedHost -> "Removed"
  | BlackListedHost -> "Black"
  | NewHost -> "New"
  | ServerFull -> "Server full"
      
let short_string_of_connection_state s = 
  match s with
  | Connected (-1) ->    "Cn'd"
  | Connected (-2) ->    "Cn'd"
  | NotConnected (_,-1) -> ""
  | NotConnected (_,0) ->    "Qout"
  | Connected  0 ->      "Qued"
  | NotConnected (_,n) ->    "Rout" 
  | Connected  n ->      "Rank" 
  | Connecting ->        "Cing"
  | Connected_initiating -> "Init"
  | Connected_downloading n -> "Down"
      
  | RemovedHost -> "Rem"
  | BlackListedHost -> "BL"
  | NewHost -> "New"
  | ServerFull -> "Full"
    
exception IgnoreNetwork
  
let string_of_tag_value tag =
  match tag with
    Uint64 i| Fint64 i -> Int64.to_string i
  | Addr x -> Ip.to_string x
  | String s -> String.escaped s
  | Uint16 n | Uint8 n -> string_of_int n
  | Pair (n1, n2) -> Printf.sprintf "%Ld x %Ld" n1 n2
      
type arg_handler =  ui_conn -> string
type arg_kind = 
  Arg_none of arg_handler
| Arg_multiple of (string list -> arg_handler)
| Arg_one of (string -> arg_handler)
| Arg_two of (string -> string -> arg_handler)
| Arg_three of (string -> string -> string -> arg_handler)

    
let string_of_kind kind =
  try
    match kind with
    | Known_location (ip,port) -> Ip.to_string ip
    | Indirect_location (server_ip, server_port, ip, port) -> Ip.to_string ip
  with _ -> ""

let string_of_kind_geo kind cc =
  let ip_cn_cc ip =
    match cc with
    | None ->
        let ccode, cname = Geoip.get_country ip in
        Ip.to_string ip, ccode, cname
    | Some cc ->
        Ip.to_string ip, 
        Geoip.country_code_array.(cc),
        Geoip.country_name_array.(cc)
  in
  try
    match kind with
    | Known_location (ip,port) -> 
        ip_cn_cc ip
    | Indirect_location (server_ip, server_port, ip, port) ->
        ip_cn_cc ip
  with _ -> "","X","Country error"

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
    brand_download = 0L;
    brand_upload = 0L;
  }

type country_stats =
  {
    country_code : string;
    country_name : string;
    country_continent : string;
    mutable country_session_upload : int64;
    mutable country_session_download : int64;
    mutable country_session_seen : int64;
    mutable country_total_upload : int64;
    mutable country_total_download : int64;
    mutable country_total_seen : int64;
  }

type kind_type = {
  f : string -> string -> unit;
  description : string
}

type slot_kind =
  NoSlot
| FriendSlot
| ReleaseSlot
| SmallFileSlot
| NormalSlot
| PrioSlot of string

let string_of_slot_kind slot_kind short =
  match slot_kind with
    NoSlot -> "NoSlot"
  | FriendSlot -> "FriendSlot"
  | ReleaseSlot -> "ReleaseSlot"
  | SmallFileSlot -> "SmallFileSlot"
  | NormalSlot -> if short then "" else "NormalSlot"
  | PrioSlot dir -> Printf.sprintf "Prio %s" dir

type swarming_strategy =
  LinearStrategy    (* one after the other one *)
| AdvancedStrategy

type web_infos_state =
| DownloadStarted
| FileLoaded

type web_infos = {
  kind : string;
  period : int;
  mutable url : string;
  mutable state : web_infos_state option;
}

let string_of_web_infos_state state =
  match state with
  | None -> "unknown"
  | Some DownloadStarted -> "DL started"
  | Some FileLoaded -> "File loaded"

