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

open Int64ops
open Printf2
open Md4

open CommonSwarming
open CommonInteractive
open CommonClient
open CommonTypes
open CommonOptions
open CommonFile
open CommonShared
open BasicSocket
open CommonGlobals
open Options

open BTRate
open BTTypes
open BTOptions
open BTProtocol
open CommonNetwork
open TcpMessages


let send_client c m = send_client c.client_sock m

let as_ft file = as_file file.ft_file
let ft_num file = file_num (as_ft file)
let ft_size file = file.ft_file.impl_file_size
let ft_state file = file_state (as_ft file)

let as_file file = as_file file.file_file
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file_fd (as_file file)
let file_disk_name file = file_disk_name (as_file file)
let file_state file = file_state (as_file file)
let file_num file = file_num (as_file file)
let file_must_update file = file_must_update (as_file file)


let set_file_state file state =
 CommonFile.set_file_state (as_file file) state

let as_client c = as_client c.client_client
let client_type c = client_type (as_client c)

let set_client_state client state =
  CommonClient.set_client_state (as_client client) state

let set_client_disconnected client =
  CommonClient.set_client_disconnected (as_client client)

let client_num c = client_num (as_client c)


let network = new_network "BT" "BitTorrent"
    [
    NetworkHasMultinet;
    NetworkHasUpload;
    NetworkHasStats;
  ]

let connection_manager = network.network_connection_manager

let (shared_ops : file CommonShared.shared_ops) =
  CommonShared.new_shared_ops network

let (server_ops : server CommonServer.server_ops) =
  CommonServer.new_server_ops network

let (room_ops : server CommonRoom.room_ops) =
  CommonRoom.new_room_ops network

let (user_ops : user CommonUser.user_ops) =
  CommonUser.new_user_ops network

let (file_ops : file CommonFile.file_ops) =
  CommonFile.new_file_ops network

let (ft_ops : ft CommonFile.file_ops) =
  CommonFile.new_file_ops network

let (client_ops : client CommonClient.client_ops) =
  CommonClient.new_client_ops network

let must_share_file file codedname has_old_impl =
  match file.file_shared with
  | Some _ -> ()
  | None ->
      begin
        let impl = {
          impl_shared_update = 1;
          impl_shared_fullname = file_disk_name file;
          impl_shared_codedname = codedname;
          impl_shared_size = file_size file;
          impl_shared_id = Md4.null;
          impl_shared_num = 0;
          impl_shared_uploaded = Int64.zero;
          impl_shared_ops = shared_ops;
          impl_shared_val = file;
          impl_shared_requests = 0;
          impl_shared_file = Some (as_file file);
          impl_shared_servers = [];
        } in
        file.file_shared <- Some impl;
        incr CommonGlobals.nshared_files;
        CommonShared.shared_calculate_total_bytes ();
        match has_old_impl with
          None -> update_shared_num impl
        | Some old_impl -> replace_shared old_impl impl
      end

let must_share_file file = must_share_file file (file_best_name (as_file file)) None

let unshare_file file =
  match file.file_shared with
    None -> ()
  | Some s ->
      begin
        file.file_shared <- None;
        decr CommonGlobals.nshared_files;
        CommonShared.shared_calculate_total_bytes ()
      end

module DO = CommonOptions

let current_files = ref ([] : BTTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)

let bt_dht = ref (None : BT_DHT.M.t option)

let files_by_uid = Hashtbl.create 13

let max_range_len = Int64.of_int (1 lsl 14)
let max_request_len = Int64.of_int (1 lsl 16)

let bt_download_counter = ref Int64.zero
let bt_upload_counter = ref Int64.zero

let log_prefix = "[BT]"

let lprintf_nl ?exn fmt =
  lprintf_nl2 ?exn log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt


let check_if_interesting file c =

  if not c.client_alrd_sent_notinterested then
    let up = match c.client_uploader with
        None -> assert false
      | Some up -> up
    in
    let swarmer = CommonSwarming.uploader_swarmer up in
    let must_send =
(* The client has nothing to propose to us *)
      (not (CommonSwarming.is_interesting up )) &&
(* All the requested ranges are useless *)
      (List.filter (fun (_,_,r) ->
            let x,y = CommonSwarming.range_range r in
            x < y) c.client_ranges_sent = []) &&
      (match c.client_range_waiting with
          None -> true
        | Some (x,y,r) ->
            let x,y = CommonSwarming.range_range r in
            x < y) &&
(* The current blocks are also useless *)
      (match c.client_chunk with
      | None -> true
      | Some (chunk, blocks) ->
          List.for_all (fun b ->
            let chunk_num = CommonSwarming.block_chunk_num swarmer b.up_block in
            let bitmap = CommonSwarming.chunks_verified_bitmap swarmer in
            VB.get bitmap chunk_num <> VB.State_verified) blocks)
    in
    if must_send then
      begin
        c.client_interesting <- false;
        c.client_alrd_sent_notinterested <- true;
        send_client c NotInterested
      end

let add_torrent_infos file trackers =
  file.file_trackers <- trackers @ file.file_trackers

let create_temp_file file_temp file_files file_state =
  if !verbose then lprintf_nl "create_temp_file %s - %s" file_temp (string_of_state file_state);
  let writable =
    if file_state = FileShared then
      false
    else
      true
  in
  let file_fd =
    if file_files <> [] then
      Unix32.create_multifile file_temp writable file_files
    else
      Unix32.create_diskfile file_temp writable
  in
  if Unix32.destroyed file_fd then
    failwith
      (Printf.sprintf
        "create_temp_file: Unix32.create returned a destroyed FD for %s\n"
        file_temp);
  file_fd

let make_tracker_url url =
  if String2.check_prefix (String.lowercase url) "http://" then 
    `Http url (* do not change the case of the url *)
  else
    try Scanf.sscanf (String.lowercase url) "udp://%s@:%d" (fun host port -> `Udp (host,port))
    with _ -> `Other url

(** invariant: [make_tracker_url (show_tracker_url url) = url] *)
let show_tracker_url : tracker_url -> string = function
  | `Http url | `Other url -> url
  | `Udp (host,port) -> Printf.sprintf "udp://%s:%d" host port

let can_handle_tracker = function
  | `Http _
  | `Udp _ -> true
  | `Other _ -> false

let set_trackers file file_trackers =
  List.iter (fun url ->
        let url = make_tracker_url url in
        if not (List.exists (fun tracker -> tracker.tracker_url = url) file.file_trackers) then
          let t = {
            tracker_url = url;
            tracker_interval = 600;
            tracker_min_interval = 600;
            tracker_last_conn = 0;
            tracker_last_clients_num = 0;
            tracker_torrent_downloaded = 0;
            tracker_torrent_complete = 0;
            tracker_torrent_incomplete = 0;
            tracker_torrent_total_clients_count = 0;
            tracker_torrent_last_dl_req = 0;
            tracker_id = "";
            tracker_key = "";
            tracker_status = if can_handle_tracker url then Enabled 
                             else Disabled_mld (intern "Tracker type not supported")
          } in
          file.file_trackers <-  t :: file.file_trackers)
  file_trackers

let new_file ?(metadata=false) file_id t torrent_diskname file_temp file_state user group =
  try
    Hashtbl.find files_by_uid file_id
  with Not_found ->
      let file_fd = create_temp_file file_temp t.torrent_files file_state in
      let rec file = {
          file_tracker_connected = false;
          file_file = file_impl;
          file_piece_size = t.torrent_piece_size;
          file_id = file_id;
          file_name = t.torrent_name;
          file_comment = t.torrent_comment;
          file_created_by = t.torrent_created_by;
          file_creation_date = t.torrent_creation_date;
          file_modified_by = t.torrent_modified_by;
          file_encoding = t.torrent_encoding;
          file_clients_num = 0;
          file_clients = Hashtbl.create 113;
          file_swarmer = None;
          file_trackers = [];
          file_chunks = t.torrent_pieces;
          file_files = (List.map (fun (file,size) -> (file,size,None)) t.torrent_files);
          file_blocks_downloaded = [];
          file_uploaded = Int64.zero;
          file_torrent_diskname = torrent_diskname;
          file_completed_hook = (fun _ -> ());
          file_shared = None;
          file_session_uploaded = Int64.zero;
          file_session_downloaded = Int64.zero;
          file_last_dht_announce = 0;
          file_metadata_size = 0L;
          file_metadata_piece = 0L;
          file_metadata_downloading = metadata;
          file_metadata_chunks = Array.make 20 "";
          file_private = t.torrent_private;
        } and file_impl =  {
          (dummy_file_impl ()) with
          impl_file_owner = user;
          impl_file_group = group;
          impl_file_fd = Some file_fd;
          impl_file_size = t.torrent_length;
          impl_file_downloaded = Int64.zero;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();
          impl_file_best_name = t.torrent_name;
        }
      in
      if t.torrent_announce_list <> [] then
        set_trackers file t.torrent_announce_list
      else
        set_trackers file [t.torrent_announce];
      if file_state <> FileShared then begin
          let kernel = CommonSwarming.create_swarmer file_temp (file_size file) in
          let swarmer = CommonSwarming.create kernel (as_file file)
            file.file_piece_size in
          file.file_swarmer <- Some swarmer;
          CommonSwarming.set_verified swarmer (fun _ num ->
              file.file_blocks_downloaded <- (num) ::
              file.file_blocks_downloaded;
              file_must_update file;
(*Automatically send Have to ALL clients once a piece is verified
            NB : will probably have to check if client can be interested*)
              Hashtbl.iter (fun _ c ->

                  if c.client_registered_bitfield then
                    begin
                      match c.client_bitmap with
                        None -> ()
                      | Some bitmap ->
                          if not (Bitv.get bitmap num) then
                            send_client c (Have (Int64.of_int num));
                          check_if_interesting file c
                    end
              ) file.file_clients

          );
          CommonSwarming.set_verifier swarmer (Verification
              (Array.map (fun sha1 -> Sha1 sha1) file.file_chunks));
        end;
      current_files := file :: !current_files;
      Hashtbl.add files_by_uid file_id file;
      file_add file_impl file_state;
      must_share_file file;
      file

let new_download ?(metadata=false) file_id t torrent_diskname user =
  let file_temp = Filename.concat !!DO.temp_directory
      (Printf.sprintf "BT-%s" (Sha1.to_string file_id)) in
  new_file ~metadata file_id t torrent_diskname file_temp FileDownloading user

let ft_by_num = Hashtbl.create 13
let ft_counter = ref 0

let new_ft file_name user =
  incr ft_counter;
  let rec ft = {
      ft_file = file_impl;
      ft_id = !ft_counter;
      ft_filename = file_name;
      ft_retry = (fun _ -> ());
    } and file_impl =  {
      (dummy_file_impl ()) with
      impl_file_owner = user;
      impl_file_group = user.user_default_group;
      impl_file_fd = None;
      impl_file_size = zero;
      impl_file_downloaded = Int64.zero;
      impl_file_val = ft;
      impl_file_ops = ft_ops;
      impl_file_age = last_time ();
      impl_file_best_name = file_name;
    }
  in
  Hashtbl.add ft_by_num !ft_counter ft;
  file_add file_impl FileDownloading;
  ft

let _dot_string s h =
  let len = String.length s in
  let char2hex c =
    let ic = int_of_char c in
    if ic >= 65 && ic <= 70 then
      string_of_int (ic - 55)
    else begin
      if ic >= 97 && ic <= 102 then
        string_of_int (ic - 87)
      else
        Printf.sprintf "%c" c
    end
  in
  let rec iter i b =
    if i < len then begin
      if h then Buffer.add_string b (char2hex s.[i])
           else Buffer.add_char b s.[i];
      if i < len-1 then Buffer.add_char b '.';
      iter (i+1) b;
    end else b;
  in
  Buffer.contents (iter 0 (Buffer.create (len*2)))

let dot_string s =
  _dot_string s false

let dot_string_h s =
  _dot_string s true

let dot_string_of_list s l =
  let buf = Buffer.create (List.length l) in
  List.iter (fun i -> Buffer.add_char buf s.[i]) l;
  dot_string (Buffer.contents buf)

let dot_string_of_string s =
  let buf = Buffer.create 20 in
  let found_non_int = ref false in
  String.iter (fun s ->
    match s with
    | '0' .. '9' ->
        if !found_non_int then Buffer.add_char buf '.';
        found_non_int := false;
        Buffer.add_char buf s
    | _ -> found_non_int := true
  ) s;
  Buffer.contents buf

(* check string s for char c (dec) at position l (list) *)
let check_all s c l =
  let ch = char_of_int c in
  List.for_all (fun i -> s.[i] = ch) l

let check_int s p =
  try
    ignore (int_of_string (String.sub s p 1));
    true
  with _ -> false

let strip_leading_zeroes s =
  let l = String.length s in
  let rec aux i =
    if i = l then "0"
    else if s.[i] <> '0' then String.sub s i (l - i)
    else aux (i + 1) in
  aux 0

(* from azureus/gpl *)
let decode_az_style s =
  if check_all s 45 [0;7] then begin
    let s_id = (String.sub s 1 2) in
    let brand = 
     match s_id with
      | "AR" -> Brand_arctic
      | "AZ" -> Brand_azureus
      | "BB" -> Brand_bitbuddy
      | "BC" -> Brand_bitcomet
      | "BR" -> Brand_bitrocket
      | "BS" -> Brand_btslave
      | "BX" -> Brand_bittorrentx
      | "CT" (* ctorrent *)
      | "CD" -> Brand_ctorrent
      | "lt" (* libtorrent *)
      | "LT" -> Brand_libtorrent
      | "MT" -> Brand_moonlighttorrent
      | "SB" -> Brand_swiftbit
      | "SN" -> Brand_sharenet
      | "SS" -> Brand_swarmscope
      | "SZ" (* shareaza *)
      | "S~" -> Brand_shareaza
      | "TN" -> Brand_torrentdotnet
      | "TS" -> Brand_torrentstorm
      | "XT" -> Brand_xantorrent
      | "ZT" -> Brand_ziptorrent
      | "bk" -> Brand_bitkitten
      | "MP" -> Brand_moopolice
      | "UM" -> Brand_utorrent_mac
      | "UT" -> Brand_utorrent
      | "KT" -> Brand_ktorrent
      | "LP" -> Brand_lphant
      | "TR" -> Brand_transmission
      | "HN" -> Brand_hydranode
      | "RT" -> Brand_retriever
      | "PC" -> Brand_cachelogic
      | "ES" -> Brand_electricsheep
      | "qB" -> Brand_qbittorrent
      | "QT" -> Brand_qt4
      | "UL" -> Brand_uleecher
      | "XX" -> Brand_xtorrent
      | "AG" (* ares *)
      | "A~" -> Brand_ares
      | "AX" -> Brand_bitpump
      | "DE" -> Brand_deluge
      | "TT" -> Brand_tuotu
      | "SD" (* Thunder (aka XùnLéi) *)
      | "XL" -> Brand_xunlei
      | "FT" -> Brand_foxtorrent
      | "BF" -> Brand_bitflu
      | "OS" -> Brand_oneswarm
      | "LW" -> Brand_limewire
      | "HL" -> Brand_halite
      | "MR" -> Brand_miro
      | "PD" -> Brand_pando
      | _ -> Brand_unknown
    in
    if brand = Brand_unknown then None else
      let version = 
        match brand with
          (* 4.56 *)
          | Brand_bitpump
          | Brand_bitcomet -> (String.sub s 4 1) ^ "." ^ (String.sub s 5 2)
          (* 3.4.5 *)
          | Brand_tuotu
          | Brand_utorrent_mac
          | Brand_oneswarm
          | Brand_utorrent -> (String.sub s 3 1) ^ "." ^ (String.sub s 4 1) ^ "." ^ (String.sub s 5 1)
          (* 3.45 *)
          | Brand_transmission -> (String.sub s 3 1) ^ "." ^ (String.sub s 4 2)
          (* 34.56 *)
          | Brand_ctorrent -> (strip_leading_zeroes (String.sub s 3 2)) ^ "." ^ (strip_leading_zeroes(String.sub s 5 2))
          (* 3.4.5->[R=RC.6|D=Dev|''] *)
          | Brand_ktorrent -> 
              let x = match s.[5] with 
                | 'R' -> " RC" ^ (String.sub s 6 1)
                | 'D' -> " Dev"
                | _ -> ""
              in
              (String.sub s 3 1) ^ "." ^ (String.sub s 4 1) ^ x
          (* 3.4(56) *)
          | Brand_bitrocket -> (String.sub s 3 1) ^ "." ^ (String.sub s 4 1) ^ "(" ^ (String.sub s 5 2) ^ ")"
          (* v 3456 *)
          | Brand_xtorrent -> "v" ^ (strip_leading_zeroes (String.sub s 3 4))
          (* BitFlu is too complicated YMDD (Y+M -> HEX) eg. 7224 is 2007.02.24 *)
          | Brand_bitflu -> ""
          (* 3.4.5.6 *)
          | _ -> (dot_string (String.sub s 3 4)) 
      in
      Some (brand, version)
  end else
  None

let decode_tornado_style s =
  if s.[5] = '-' then begin
    let check_brand c =
     match c with
     | 'T' -> Brand_bittornado
     | 'S' -> Brand_shadow
     | 'A' -> Brand_abc
     | 'U' -> Brand_upnp
     | 'O' -> Brand_osprey
     | 'R' -> Brand_tribler
     | _   -> Brand_unknown
    in
    let bv = ref None in

    if s.[5] ='-' && s.[6] ='-' && s.[7] ='-' then begin
      let brand = check_brand s.[0] in
      if not (brand = Brand_unknown) then
        bv := Some (brand, (dot_string_h (String.sub s 1 3)));
    end
    else if s.[6] = (char_of_int 48) then begin
      let brand = check_brand s.[0] in
      if not (brand = Brand_unknown) then
        bv := Some (brand, ("LM " ^ dot_string_h (String.sub s 1 3)));
    end;
    !bv
  end else
  None

let decode_mainline_style s =
  if check_all s 45 [2;7] && check_int s 1 then begin
    let c_id = s.[0] in
    let brand =
      match c_id with
     | 'M' -> Brand_mainline
     | _ -> Brand_unknown
    in
    if brand = Brand_unknown then None
    else Some (brand, (dot_string_of_string (String.sub s 1 6)))
  end else
  None

let decode_simple_style s =
  let simple_list = ref
    [ (0,  "martini",           Brand_martiniman,      "");
      (0,  "oernu",             Brand_btugaxp,         "");
      (0,  "BTDWV-",            Brand_deadmanwalking,  "");
      (0,  "PRC.P---",          Brand_btplus,          "II");
      (0,  "P87.P---",          Brand_btplus,          "");
      (0,  "S587Plus",          Brand_btplus,          "");
      (5,  "Azureus",           Brand_azureus,         "2.0.3.2");
      (0,  "-G3",               Brand_g3torrent,       "");
      (0,  "-AR",               Brand_arctic,          "");
      (4,  "btfans",            Brand_simplebt,        "");
      (0,  "btuga",             Brand_btugaxp,         "");
      (0,  "BTuga",             Brand_btugaxp,         "");
      (0,  "DansClient",        Brand_xantorrent,      "");
      (0,  "Deadman Walking-",  Brand_deadmanwalking,  "");
      (0,  "346-",              Brand_torrenttopia,    "");
      (0,  "271-",              Brand_greedbt,         "2.7.1");
      (10, "BG",                Brand_btgetit,         "");
      (0,  "a00---0",           Brand_swarmy,          "");
      (0,  "a02---0",           Brand_swarmy,          "");
      (0,  "10-------",         Brand_jvtorrent,       "");
      (0,  "T00---0",           Brand_teeweety,        "");
      (0,  "LIME",              Brand_limewire,        "");
      (0,  "AZ2500BT",          Brand_btyrant,         "");
      (0,  "Mbrst",             Brand_burst,           (dot_string_of_list s [5;7;9]));
      (0,  "Plus",              Brand_plus,            (dot_string_of_list s [4;5;6]));
      (0,  "OP",                Brand_opera,           (dot_string(String.sub s 2 4)));
      (0,  "eX",                Brand_exeem,           (String.sub s 2 18));
      (0,  "turbobt",           Brand_turbobt,         (String.sub s 7 5));
      (0,  "btpd",              Brand_btpd,            (dot_string(String.sub s 5 3)));
      (0,  "XBT",               Brand_xbt,             (dot_string(String.sub s 3 3)));
      (0,  "-FG",               Brand_flashget,        (dot_string(String.sub s 4 3)));
      (0,  "-SP",               Brand_bitspirit,       (dot_string(String.sub s 3 3)));
    ]
  in
  let len = List.length !simple_list in
  let rec check pos =
    if pos >= len then None
    else
      let (x,y,z,v) = List.nth !simple_list pos in
      if (String.sub s x (String.length y)) = y then Some (z,v)
        else check (pos+1);
  in
  check 0

let decode_rufus s =
  let release s =
    let minor = Char.code s.[1] in
    Printf.sprintf "%d.%d.%d" (Char.code s.[0]) (minor / 10) (minor mod 10) in
  if "RS" = String.sub s 2 2 then
     Some (Brand_rufus, release s)
  else None

let decode_bow s =
  if "BOW" = String.sub s 0 3 ||
  (check_all s 45 [0;7] && "BOW" = String.sub s 1 3) then
    Some (Brand_bitsonwheels, (String.sub s 4 3))
  else None
  
let decode_btuga s =
  if ("BTM" = String.sub s 0 3) && ("BTuga" = String.sub s 5 5) then
    Some (Brand_btuga, dot_string(String.sub s 3 2))
  else None

let decode_shadow s =
  if "S" = String.sub s 0 1 then begin
    let bv = ref None in
    if check_all s 45 [6;7;8] then begin
      let i1 = int_of_string ("0x" ^ String.sub s 1 1) in
      let i2 = int_of_string ("0x" ^ String.sub s 2 1) in
      let i3 = int_of_string ("0x" ^ String.sub s 3 1) in
      bv := Some (Brand_shadow, (Printf.sprintf "%d.%d.%d" i1 i2 i3))
    end;

    if s.[8] = (char_of_int 0) then begin
      let i1 = int_of_char s.[1] in
      let i2 = int_of_char s.[2] in
      let i3 = int_of_char s.[3] in
      bv := Some (Brand_shadow, (Printf.sprintf "%d.%d.%d" i1 i2 i3))
    end;
    !bv
  end else
  None

let decode_bitspirit s =
  if "BS" = String.sub s 2 2 then begin
    let bv = ref None in
    if s.[1] = (char_of_int 0) then bv := Some (Brand_bitspirit, "v1");
    if s.[1] = (char_of_int 2) then bv := Some (Brand_bitspirit, "v2");
    if s.[1] = (char_of_int 3) then bv := Some (Brand_bitspirit, "v3");
    !bv
  end else
  None

let decode_upnp s =
  if 'U' = s.[0] && s.[8] = '-' then
    Some (Brand_upnp, (dot_string (String.sub s 1 3)))
  else None

let decode_old_bitcomet s =
  let bitcomet = String.sub s 0 4 in
  if "exbc" = bitcomet || "FUTB" = bitcomet || "xUTB" = bitcomet then
    let brand = if "LORD" = String.sub s 6 4 then
      Brand_bitlord else Brand_bitcomet 
    in
    let versionMajorNumber = int_of_char s.[4] in
    let versionMinorNubmer = 
      match versionMajorNumber with
         0 -> (int_of_char s.[5])
      |  _ -> ((int_of_char s.[5]) mod 10)
    in
    let version = Printf.sprintf "%d.%d" 
      versionMajorNumber versionMinorNubmer in
    Some (brand, version)
  else None

let decode_shareaza s =
  let rec not_zeros pos =
    if pos > 15 then true else
      if s.[pos] = (char_of_int 0) then false else not_zeros (pos+1)
  in
  let rec weird_crap pos =
    if pos > 19 then true else
      let i1 = (int_of_char s.[pos]) in
      let i2 = (int_of_char s.[(pos mod 16)]) in
      let i3 = (int_of_char s.[(15 - (pos mod 16))]) in
      if not (i1 = (i2 lxor i3)) then false else weird_crap (pos+1)
  in
  if (not_zeros 0) && (weird_crap 16) then Some (Brand_shareaza, "") else None

let decode_non_zero s =
  let max_pos = ((String.length s) - 1) in
  let zero = char_of_int 0 in
  let rec find_non_zero pos =
    if pos > max_pos then max_pos else
      if not (s.[pos] = zero) then pos else
        find_non_zero (pos+1)
  in
  let bv = ref None in
  (match find_non_zero 0 with
     8 -> (if "UDP0" = String.sub s 16 4 then
            bv := Some (Brand_bitcomet, "UDP");
          if "HTTPBT" = String.sub s 14 6 then
            bv := Some (Brand_bitcomet, "HTTP"));
  |  9 -> if check_all s 3 [9;10;11] then
      bv := Some (Brand_snark, "");
  | 12 -> if check_all s 97 [12;13] then
        bv := Some (Brand_experimental, "3.2.1b2")
      else begin
        if check_all s 0 [12;13] then
          bv := Some (Brand_experimental, "3.1")
        else
          bv := Some (Brand_mainline, "")
      end;
  | _ -> ()
  );
  !bv

(* format is : "-ML" ^ version ( of unknown length) ^ "-" ^ random bytes ( of unknown length) *)
let decode_mldonkey_style s =
  if '-' = s.[0] then begin
    let s_id = String.sub s 1 2 in
    let brand =
     match s_id with
     | "ML" -> Brand_mldonkey
     | _ -> Brand_unknown
    in
    if brand = Brand_unknown then None else
      let len =
        (try String.index_from s 3 '-'
        with _ -> 8) - 3
      in
      let version = String.sub s 3 len in
    Some (brand, version)
  end else None


let decoder_list = [
    decode_az_style;
    decode_tornado_style;
    decode_mainline_style;
    decode_simple_style;
    decode_bow;
    decode_shadow;
    decode_bitspirit;
    decode_upnp;
    decode_old_bitcomet;
    decode_shareaza;
    decode_non_zero;
    decode_mldonkey_style;
    decode_rufus;
    decode_btuga;
  ]

let parse_software s =
  let default = (Brand_unknown, "") in
  let rec iter = function
  | [] ->
    if !verbose_msg_clienttags then lprintf_nl "BTUC: %S" s;
    default
  | d :: t ->
    match (d s) with
    | None -> iter t
    | Some (brand, version as bv) ->
      if !verbose_msg_clienttags then
        lprintf_nl "BTKC: %S; ID: %S; version: %S" s (brand_to_string brand) version;
      bv
  in
  if Sha1.direct_of_string s = Sha1.null then
    default
  else 
    try iter decoder_list
    with _ -> default

let check_client_country_code c =
  if Geoip.active () then
    match c.client_country_code with
    | None ->
        c.client_country_code <-
          Geoip.get_country_code_option (fst c.client_host)
    | _ -> ()

let new_client file peer_id kind cc =
  try
    let c = Hashtbl.find file.file_clients kind in
    let old_ip = fst c.client_host in
    c.client_host <- kind;
    if old_ip <> Ip.null && old_ip <> fst c.client_host then
      begin
        c.client_country_code <- None;
        check_client_country_code c
      end;
    c
  with _ ->
      let brand, release = parse_software (Sha1.direct_to_string peer_id) in
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
          client_upload_requests = [];
          client_connection_control = new_connection_control (());
          client_file = file;
          client_host = kind;
          client_country_code = cc;
          client_choked = true;
          client_received_peer_id = false;
          client_sent_choke = false;
          client_interested = false;
          client_uploader = None;
          client_chunks = [];
          client_ranges_sent = [];
          client_range_waiting = None;
          client_chunk = None;
          client_uid = peer_id;
          client_brand = brand;
          client_release = release;
          client_bitmap = None;
          client_allowed_to_write = zero;
          client_total_uploaded = zero;
          client_total_downloaded = zero;
          client_session_uploaded = zero;
          client_session_downloaded = zero;
          client_upload_rate = Rate.new_rate ();
          client_downloaded_rate = Rate.new_rate ();
          client_connect_time = last_time ();
          client_blocks_sent = [];
          client_new_chunks = [];
          client_good = false;
          client_num_try = 0;
          client_alrd_sent_interested = false;
          client_alrd_sent_notinterested = false;
          client_interesting = false;
          client_incoming = false;
          client_registered_bitfield = false;
          client_last_optimist = 0;
          client_dht = false;
          client_cache_extension = false;
          client_fast_extension = false;
          client_utorrent_extension = false;
          client_ut_metadata_msg = -1L;
          client_azureus_messaging_protocol = false;
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_upload = None;
        } in
      c.client_connection_control.control_min_reask <- 120;
      check_client_country_code c;
      new_client impl;
      Hashtbl.add file.file_clients kind c;
      file.file_clients_num <- file.file_clients_num + 1;
      file_add_source (as_file file) (as_client c);
      c

let remove_file file =
  Hashtbl.remove files_by_uid file.file_id;
  current_files := List2.removeq file !current_files

let remove_client c =
  Hashtbl.remove c.client_file.file_clients c.client_host ;
  c.client_file.file_clients_num <- c.client_file.file_clients_num  - 1;
  file_remove_source (as_file c.client_file) (as_client c)

let remove_tracker url file =
  if !verbose_msg_servers then
    List.iter (fun tracker ->
      lprintf_nl "Old tracker list: %s" (show_tracker_url tracker.tracker_url)
    ) file.file_trackers;
  List.iter (fun bad_tracker ->
    if bad_tracker.tracker_url = url then
    file.file_trackers <- List2.remove_first bad_tracker file.file_trackers;
  ) file.file_trackers;
  if !verbose_msg_servers then
    List.iter (fun tracker ->
      lprintf_nl "New tracker list: %s" (show_tracker_url tracker.tracker_url)
    ) file.file_trackers

let tracker_is_enabled t =
  match t.tracker_status with
  | Enabled -> true
  | Disabled_failure (i,_) ->
      if !!tracker_retries = 0 || i < !!tracker_retries then true else false
  | _ -> false

let torrents_directory = "torrents"
let new_torrents_directory = Filename.concat torrents_directory "incoming"
let downloads_directory = Filename.concat torrents_directory "downloads"
let tracked_directory = Filename.concat torrents_directory "tracked"
let seeded_directory = Filename.concat torrents_directory "seeded"
let old_directory = Filename.concat torrents_directory "old"

(*************************************************************

Define a function to be called when the "mem_stats" command
  is used to display information on structure footprint.

**************************************************************)

let () =
  Heap.add_memstat "BittorrentGlobals" (fun level buf ->
     Printf.bprintf buf "Number of old files: %d\n" (List.length !!old_files);
     let downloads = ref 0 in
     let tracked = ref 0 in
     let seeded = ref 0 in
     Unix2.iter_directory (fun file -> incr downloads ) downloads_directory;
     Unix2.iter_directory (fun file -> incr tracked ) tracked_directory;
     Unix2.iter_directory (fun file -> incr seeded ) seeded_directory;
     Printf.bprintf buf "Files in downloads directory: %d\n" ! downloads;
     Printf.bprintf buf "Files in tracked directory: %d\n" ! tracked;
     Printf.bprintf buf "Files in seeded directory: %d\n" ! seeded;
     Printf.bprintf buf "files_by_uid: %d\n" (Hashtbl.length files_by_uid);
     Printf.bprintf buf "ft_by_num: %d\n" (Hashtbl.length ft_by_num);
  )

open BT_DHT

let () =
  Heap.add_memstat "BittorrentDHT" (fun _level buf ->
    match !bt_dht with
    | None -> ()
    | Some dht ->
    let (buckets,nodes,keys,peers) = stat dht in
    Printf.bprintf buf "Routing : %d nodes in %d buckets\n" nodes buckets;
    Printf.bprintf buf "Storage : %d keys with %d peers\n" keys peers;
    List.iter (fun s -> Printf.bprintf buf "%s\n" s) (rpc_stats dht);
    let queries = ["PING",`Ping;"FIND_NODE",`FindNode;"GET_PEERS",`GetPeers;"ANNOUNCE",`Announce] in
    Printf.bprintf buf "Outgoing queries : ok/error/timeout\n";
    List.iter begin fun (name,qt) ->
      let get k = try Hashtbl.find dht.M.stats (qt,`Out k) with Not_found -> 0 in
      Printf.bprintf buf "%s: %d/%d/%d\n" name (get `Answer) (get `Error) (get `Timeout);
      end queries;
    Printf.bprintf buf "Incoming queries\n";
    List.iter begin fun (name,qt) ->
      let get () = try Hashtbl.find dht.M.stats (qt,`In) with Not_found -> 0 in
      Printf.bprintf buf "%s: %d\n" name (get ())
      end queries
  )

