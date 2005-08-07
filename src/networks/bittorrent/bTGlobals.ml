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
open CommonUser
open CommonTypes
open CommonOptions
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonFile
open BasicSocket
open CommonGlobals
open Options

open BTRate
open BTTypes
open BTOptions
open BTProtocol
open CommonDownloads
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
let file_fd file = file.file_file.impl_file_fd
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

module DO = CommonOptions

let current_files = ref ([] : BTTypes.file list)

let listen_sock = ref (None : TcpServerSocket.t option)

let files_by_uid = Hashtbl.create 13

let max_range_len = Int64.of_int (1 lsl 14)
let max_request_len = Int64.of_int (1 lsl 16)

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[BT] "
    (log_time ()); lprintf_nl2

let check_if_interesting file c =

  if not c.client_alrd_sent_notinterested then
    let up = match c.client_uploader with
        None -> assert false
      | Some up -> up
    in
    let swarmer = Int64Swarmer.uploader_swarmer up in
    let must_send =
(* The client has nothing to propose to us *)
      (not (Int64Swarmer.is_interesting up )) &&
(* All the requested ranges are useless *)
      (List.filter (fun (_,_,r) ->
            let x,y = Int64Swarmer.range_range r in
            x < y) c.client_ranges_sent = []) &&
      (match c.client_range_waiting with
          None -> true
        | Some (x,y,r) ->
            let x,y = Int64Swarmer.range_range r in
            x < y) &&
(* The current block is also useless *)
      (match c.client_block with
          None -> true
        | Some b ->
            let block_num = Int64Swarmer.block_num swarmer b in
            let bitmap = Int64Swarmer.verified_bitmap swarmer in
            bitmap.[block_num] <> '3')
    in
    if must_send then
      begin
        c.client_interesting <- false;
        c.client_alrd_sent_notinterested <- true;
        send_client c NotInterested
      end

let add_torrent_infos file trackers =
  List.iter (fun tracker ->
      file.file_trackers <- tracker :: file.file_trackers
  ) trackers

let create_temp_file file_temp file_files =
  if !CommonOptions.verbose_files then lprintf_nl () "create_temp_file %s" file_temp;
  let file_fd =
    if file_files <> [] then
      Unix32.create_multifile file_temp
        [Unix.O_RDWR; Unix.O_CREAT] 0o666 file_files
    else
      Unix32.create_rw file_temp
  in
  if Unix32.destroyed file_fd then
    failwith
      (Printf.sprintf
        "create_temp_file: Unix32.create returned a destroyed FD for %s\n"
        file_temp);
  file_fd

let set_trackers file file_trackers =
  file.file_trackers <- (List.map (fun url -> {
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
        } ) file_trackers) @ file.file_trackers

let new_file file_id t torrent_diskname file_temp file_state =
  try
    Hashtbl.find files_by_uid file_id
  with Not_found ->
      let file_fd = create_temp_file file_temp t.torrent_files in
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
          file_files = t.torrent_files;
          file_blocks_downloaded = [];
          file_uploaded = Int64.zero;
          file_torrent_diskname = torrent_diskname;
          file_completed_hook = (fun _ -> ());
          file_shared = None;
        } and file_impl =  {
          dummy_file_impl with
          impl_file_fd = file_fd;
          impl_file_size = t.torrent_length;
          impl_file_downloaded = Int64.zero;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();
          impl_file_best_name = t.torrent_name;
        }
      in
      if List.length t.torrent_announce_list > 1 then
        set_trackers file t.torrent_announce_list
      else
        set_trackers file [t.torrent_announce];
      if file_state <> FileShared then begin
          let kernel = Int64Swarmer.create_swarmer file_temp (file_size file)
            (min max_range_len file.file_piece_size) in
          let swarmer = Int64Swarmer.create kernel (as_file file)
            file.file_piece_size in
          file.file_swarmer <- Some swarmer;
          Int64Swarmer.set_verified swarmer (fun _ num ->
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
                          if (bitmap.[num] <> '1') then
                            send_client c (Have (Int64.of_int num));
                          check_if_interesting file c
                    end				
              ) file.file_clients

          );
          Int64Swarmer.set_verifier swarmer (Verification
              (Array.map (fun sha1 -> Sha1 sha1) file.file_chunks));
        end;
      current_files := file :: !current_files;
      Hashtbl.add files_by_uid file_id file;
      file_add file_impl file_state;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)
      file

let new_download file_id t torrent_diskname =
  let file_temp = Filename.concat !!DO.temp_directory
      (Printf.sprintf "BT-%s" (Sha1.to_string file_id)) in
  new_file file_id t torrent_diskname file_temp FileDownloading

let ft_by_num = Hashtbl.create 13
let ft_counter = ref 0

let new_ft file_name =
  incr ft_counter;
  let rec ft = {
      ft_file = file_impl;
      ft_id = !ft_counter;
      ft_filename = file_name;
      ft_retry = (fun _ -> ());
    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = Unix32.bad_fd;
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

let check_all s c l =
  let ch = char_of_int c in
  List.for_all (fun i -> s.[i] = ch) l

(* Old decoding routines *)

(* from azureus/gpl *)
let decode_az_style s =
  if check_all s 45 [0;7] then begin
    let s_id = (String.sub s 1 2) in
    let result = ref
    (match s_id with
      | "AR" -> "Arctic"
      | "AZ" -> "Azureus"
      | "BB" -> "BitBuddy"
      | "BC" -> "BitComet"
      | "BS" -> "BTSlave"
      | "BX" -> "Bittorrent X"
      | "CT" -> "CTorrent"
      | "LT" -> "libTorrent"
      | "MT" -> "MoonlightTorrent"
      | "SB" -> "Swiftbit"
      | "SN" -> "ShareNET"
      | "SS" -> "SwarmScope"
      | "TN" -> "Torrent.NET"
      | "TS" -> "TorrentStorm"
      | "XT" -> "XanTorrent"
      | "ZT" -> "ZipTorrent"
      | "bk" -> "BitKitten (libtorrent)"
      | _ -> "")
    in
    (if (s_id = "BC") then
      result := !result ^ " " ^ (String.sub s 4 1) ^ "." ^ (String.sub s 5 2)
    else
    if not (!result = "") then
      result := !result ^ " " ^ (dot_string (String.sub s 3 4)));
    !result;
  end else ""

let decode_tornado_style s =
  let result = ref "" in
  if check_all s 45 [4;5] then begin
    let check_id s =
     match s with
     | "T" -> "BitTornado"
     | "A" -> "ABC"
     | _ -> ""
    in
    if check_all s 45 [6;7;8] then
      let t = ref (check_id (String.sub s 0 1)) in
      if not (!t = "") then
        result := !t ^ " " ^ dot_string_h (String.sub s 1 3);
    else if s.[6] = (char_of_int 48) then
      let t = ref (check_id (String.sub s 0 1)) in
      if not (!t = "") then
        result := !t ^ " LM " ^ dot_string_h (String.sub s 1 3);
  end;
  !result

let decode_mainline_style s =
  if check_all s 45 [2;4;6;7] then begin
    let s_id = String.sub s 0 1 in
    let result = ref
     (match s_id with
     | "M" -> "Mainline"
     | _ -> "")
    in
    if !result != "" then
      result := !result ^ " " ^ dot_string_of_list s [1;3;5];
    !result
  end else ""

let decode_simple_style s =
  let simple_list = ref
    [ (0, "martini", "Martini Man");
      (0, "oernu", "BTugaXP");
      (0, "BTDWV-", "Deadman Walking");
      (0, "PRC.P---", "BitTorrent Plus! II");
      (0, "P87.P---", "BitTorrent Plus!");
      (0, "S587Plus", "BitTorrent Plus!");
      (5, "Azureus", "Azureus 2.0.3.2");
      (0, "-G3", "G3 Torrent");
      (4, "btfans", "SimpleBT");
      (0, "btuga", "BTugaXP");
      (0, "DansClient", "XanTorrent");
      (0, "Deadman Walking-", "Deadman");
      (0, "a00---0", "Swarmy");
      (0, "a02---0", "Swarmy");
      (0, "T00---0", "Teeweety") ]
  in
  let len = List.length !simple_list in
  let rec check pos =
    if pos >= len then ""
    else
      let (x,y,z) = List.nth !simple_list pos in
      if (String.sub s x (String.length y)) = y then z
        else check (pos+1);
  in
  check 0

let decode_mburst s =
  if "Mbrst" = String.sub s 0 5 then
     "Burst! " ^ (dot_string_of_list s [5;7;9])
  else ""

let decode_plus s =
  if "Plus" = String.sub s 0 4 then
     "Plus " ^ (dot_string_of_list s [4;5;6])
  else ""

let decode_bow s =
  if "BOW" = String.sub s 0 3 then
    "BitsOnWheels " ^ (String.sub s 4 3)
  else ""

let decode_exeem s =
  if "eX" = String.sub s 0 2 then
    "eXeem [" ^ (String.sub s 2 18) ^ "]"
  else ""

let decode_turbo s =
  if "turbobt" = String.sub s 0 7 then
    "TurboBT " ^ String.sub s 7 5
  else ""

let decode_xbt s =
  if "XBT" = String.sub s 0 3 then
    "XBT " ^ (dot_string (String.sub s 3 3))
  else ""

let decode_shadow s =
  if "S" = String.sub s 0 1 then
    let result = ref "" in
    if check_all s 45 [6;7;8] then begin
      let i1 = int_of_string ("0x" ^ String.sub s 1 1) in
      let i2 = int_of_string ("0x" ^ String.sub s 2 1) in
      let i3 = int_of_string ("0x" ^ String.sub s 3 1) in
      result := Printf.sprintf "Shad0w %d.%d.%d" i1 i2 i3
    end;

    if s.[8] = (char_of_int 0) then begin
      let i1 = int_of_char s.[1] in
      let i2 = int_of_char s.[2] in
      let i3 = int_of_char s.[3] in
      result := Printf.sprintf "Shad0w %d.%d.%d" i1 i2 i3
    end;

    !result;
  else ""

let decode_bitspirit s =
  let result = ref "" in
  if "BS" = String.sub s 2 2 then begin
    if s.[1] = (char_of_int 0) then result := "BitSpirit v1";
    if s.[1] = (char_of_int 2) then result := "BitSpirit v2";
  end;
  !result

let decode_upnp s =
  if "U" = String.sub s 0 1 && s.[8] = '-' then
    "UPnP " ^ (dot_string (String.sub s 1 3))
  else ""

let decode_old_bitcomet s =
  if "exbc" = String.sub s 0 4 then
    let bit_sub = if "LORD" = String.sub s 6 4 then
      "Lord" else "Comet" in
    Printf.sprintf "Bit%s %d.%d%d"
      (bit_sub)
      (int_of_char s.[4])
      ((int_of_char s.[5]) / 10)
      ((int_of_char s.[5]) mod 10)
  else ""

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
  if (not_zeros 0) && (weird_crap 16) then "Shareaza" else ""

let decode_non_zero s =
  let max_pos = ((String.length s) - 1) in
  let zero = char_of_int 0 in
  let rec find_non_zero pos =
    if pos > max_pos then max_pos else
      if not (s.[pos] = zero) then pos else
        find_non_zero (pos+1)
  in
  let result = ref "" in
  (match find_non_zero 0 with
     8 -> (if "UDP0" = String.sub s 16 4 then
            result := "BitComet UDP";
          if "HTTPBT" = String.sub s 14 6 then
            result := "BitComet HTTP";)
  |  9 -> if check_all s 3 [9;10;11] then
      result := "Snark";
  | 12 -> if check_all s 97 [12;13] then
        result := "Experimental 3.2.1b2"
      else begin
        if check_all s 0 [12;13] then
          result := "Experimental 3.1"
        else
          result := "Mainline"
      end;
  | _ -> ()
  );
  !result

(* format is : "-ML" ^ version ( of unknown length) ^ "-" ^ random bytes ( of unknown length) *)
let decode_mldonkey_style s =
  if check_all s 45 [0] then begin
    let s_id = String.sub s 1 2 in
    let result = ref
     (match s_id with
     | "ML" -> "MLDonkey"
     | _ -> "")
    in
    if !result != "" then
      result := !result ^ " " ^ String.sub s 3 ((
        try String.index_from s 1 s.[0] with Not_found -> 3) - 3);
    !result
  end else ""

let parse_software s =
  try
  let rec try_styles i =
    if i > 17 then
      begin
        lprintf_nl () "Unknown BT client found please report the next line to the dev team :\nBTUC:\"%s\"" (String.escaped s);
        "UNKNOWN"
      end
    else begin
    let res = ref
      (match i with
         | 0 -> decode_az_style s
         | 1 -> decode_tornado_style s
         | 2 -> decode_mainline_style s
         | 3 -> decode_simple_style s
         | 4 -> decode_mburst s
         | 5 -> decode_turbo s
         | 6 -> decode_plus s
         | 7 -> decode_xbt s
         | 8 -> decode_bow s
         | 9 -> decode_exeem s
         | 10 -> decode_shadow s
         | 11 -> decode_bitspirit s
         | 12 -> decode_upnp s
         | 13 -> decode_old_bitcomet s
         | 14 -> decode_shareaza s
         | 15 -> decode_non_zero s
         | 16 -> decode_mldonkey_style s
         | 17 -> if (Sha1.null) = (Sha1.direct_of_string s) then
                     "NULL" else ""
         | _ -> "ERROR"
       )
    in
    if !res = "" then try_styles (i+1) else !res;
  end
  in
  try_styles 0
  with _ -> "ERROR"

(* new decoding routines *)

let brand_to_int b =
  match b with
    Brand_unknown -> 0
  | Brand_abc -> 1
  | Brand_arctic -> 2
  | Brand_azureus -> 3
  | Brand_bitbuddy -> 4
  | Brand_bitcomet -> 5
  | Brand_bitkitten -> 6
  | Brand_bitlord -> 7
  | Brand_bitsonwheels -> 8
  | Brand_bitspirit -> 9
  | Brand_bittornado -> 10
  | Brand_bittorrentx -> 11
  | Brand_btplus -> 12
  | Brand_btslave -> 13
  | Brand_btugaxp -> 14
  | Brand_burst -> 15
  | Brand_ctorrent -> 16
  | Brand_deadmanwalking -> 17
  | Brand_exeem -> 18
  | Brand_experimental -> 19
  | Brand_g3torrent -> 20
  | Brand_libtorrent -> 21
  | Brand_mainline -> 22
  | Brand_martiniman -> 23
  | Brand_mldonkey -> 24
  | Brand_moonlighttorrent -> 25
  | Brand_plus -> 26
  | Brand_shadow -> 27
  | Brand_sharenet -> 28
  | Brand_shareaza -> 29
  | Brand_simplebt -> 30
  | Brand_snark -> 31
  | Brand_swarmscope -> 32
  | Brand_swarmy -> 33
  | Brand_swiftbit -> 34
  | Brand_teeweety -> 35
  | Brand_torrentdotnet -> 36
  | Brand_torrentstorm -> 37
  | Brand_turbobt -> 38
  | Brand_upnp -> 39
  | Brand_xantorrent -> 40
  | Brand_xbt -> 41
  | Brand_ziptorrent -> 42


let brand_of_int b =
  match b with
    0 -> Brand_unknown
  | 1 -> Brand_abc
  | 2 -> Brand_arctic
  | 3 -> Brand_azureus
  | 4 -> Brand_bitbuddy
  | 5 -> Brand_bitcomet
  | 6 -> Brand_bitkitten
  | 7 -> Brand_bitlord
  | 8 -> Brand_bitsonwheels
  | 9 -> Brand_bitspirit
  | 10 -> Brand_bittornado
  | 11 -> Brand_bittorrentx
  | 12 -> Brand_btplus
  | 13 -> Brand_btslave
  | 14 -> Brand_btugaxp
  | 15 -> Brand_burst
  | 16 -> Brand_ctorrent
  | 17 -> Brand_deadmanwalking
  | 18 -> Brand_exeem
  | 19 -> Brand_experimental
  | 20 -> Brand_g3torrent
  | 21 -> Brand_libtorrent
  | 22 -> Brand_mainline
  | 23 -> Brand_martiniman
  | 24 -> Brand_mldonkey
  | 25 -> Brand_moonlighttorrent
  | 26 -> Brand_plus
  | 27 -> Brand_shadow
  | 28 -> Brand_sharenet
  | 29 -> Brand_shareaza
  | 30 -> Brand_simplebt
  | 31 -> Brand_snark
  | 32 -> Brand_swarmscope
  | 33 -> Brand_swarmy
  | 34 -> Brand_swiftbit
  | 35 -> Brand_teeweety
  | 36 -> Brand_torrentdotnet
  | 37 -> Brand_torrentstorm
  | 38 -> Brand_turbobt
  | 39 -> Brand_upnp
  | 40 -> Brand_xantorrent
  | 41 -> Brand_xbt
  | 42 -> Brand_ziptorrent
  | _ -> raise Not_found

let brand_to_string b =
  match b with
    Brand_unknown -> "unknown"
  | Brand_abc -> "ABC"
  | Brand_arctic -> "Arctic"
  | Brand_azureus -> "Azureus"
  | Brand_bitbuddy -> "Bitbuddy"
  | Brand_bitcomet -> "BitComet"
  | Brand_bitkitten -> "BitKitten (libTorrent)"
  | Brand_bitlord -> "BitLord"
  | Brand_bitsonwheels -> "BitsOnWheels"
  | Brand_bitspirit -> "BitSpirit"
  | Brand_bittornado -> "BitTornado"
  | Brand_bittorrentx -> "BitTorrent X"
  | Brand_btplus -> "BitTorrent Plus!"
  | Brand_btslave -> "BTSlave"
  | Brand_btugaxp -> "BTugaXP"
  | Brand_burst -> "Burst !"
  | Brand_ctorrent -> "CTorrent"
  | Brand_deadmanwalking -> "Deadman Walking"
  | Brand_exeem -> "eXeem"
  | Brand_experimental -> "Experimental"
  | Brand_g3torrent -> "G3 Torrent"
  | Brand_libtorrent -> "libTorrent"
  | Brand_mainline -> "Mainline"
  | Brand_martiniman -> "Martini Man"
  | Brand_mldonkey -> "MLdonkey"
  | Brand_moonlighttorrent -> "MoonlightTorrent"
  | Brand_plus -> "Plus"
  | Brand_shadow -> "Shad0w"
  | Brand_sharenet -> "Sharenet"
  | Brand_shareaza -> "Shareaza"
  | Brand_simplebt -> "SimpleBT"
  | Brand_snark -> "Snark"
  | Brand_swarmscope -> "SwarmScope"
  | Brand_swarmy -> "Swarmy"
  | Brand_swiftbit -> "SwiftBit"
  | Brand_teeweety -> "Teeweety"
  | Brand_torrentdotnet -> "Torrent.NET"
  | Brand_torrentstorm -> "TorrentStorm"
  | Brand_turbobt -> "TurboBT"
  | Brand_upnp -> "UPNP"
  | Brand_xantorrent -> "XanTorrent"
  | Brand_xbt -> "XBT"
  | Brand_ziptorrent -> "ZipTorrent"

let parse_brand s =
  try
  let the_answer s =
  (* azureus *)
  if check_all s 45 [0;7] then begin
      let s_id = (String.sub s 1 2) in
      let result = ref
      (match s_id with
        | "AR" -> Brand_arctic
        | "AZ" -> Brand_azureus
        | "BB" -> Brand_bitbuddy
        | "BC" -> Brand_bitcomet
        | "BS" -> Brand_btslave
        | "BX" -> Brand_bittorrentx
        | "CT" -> Brand_ctorrent
        | "LT" -> Brand_libtorrent
        | "MT" -> Brand_moonlighttorrent
        | "SB" -> Brand_swiftbit
        | "SN" -> Brand_sharenet
        | "SS" -> Brand_swarmscope
        | "TN" -> Brand_torrentdotnet
        | "TS" -> Brand_torrentstorm
        | "XT" -> Brand_xantorrent
        | "ZT" -> Brand_ziptorrent
        | "bk" -> Brand_bitkitten
        | _ -> Brand_unknown)
      in
      !result;
    end
  else
  (* mainline *)
  if check_all s 45 [2;4;6;7] then begin
      let s_id = String.sub s 0 1 in
      let result = ref
       (match s_id with
       | "M" -> Brand_mainline
       | _ -> Brand_unknown)
      in
(*       if !result != "" then
        result := !result ^ " " ^ dot_string_of_list s [1;3;5];
 *)
      !result;
    end
  else
  (* simple style *)
  let simple_list = ref
    [
      (5, "Azureus", Brand_azureus);
      (0, "A", Brand_abc);
      (14, "HTTPBT", Brand_bitcomet);
      (16, "UDP0", Brand_bitcomet);
      (6, "LORD", Brand_bitlord);
      (0, "exbc", Brand_bitcomet);
      (0, "BOW", Brand_bitsonwheels);
      (0, "BS", Brand_bitspirit);
      (0, "T03", Brand_bittornado);
      (0, "Plus", Brand_btplus);
      (0, "PRC.P---", Brand_btplus);
      (0, "P87.P---", Brand_btplus);
      (0, "S587Plus", Brand_btplus);
      (0, "oernu", Brand_btugaxp);
      (0, "btuga", Brand_btugaxp);
      (4, "btuga", Brand_btugaxp);
      (0, "BTDWV-", Brand_deadmanwalking);
      (0, "Deadman Walking-", Brand_deadmanwalking);
      (0, "eX", Brand_exeem);
      (0, "-G3", Brand_g3torrent);
      (0, "martini", Brand_martiniman);
      (0, "Mbrst", Brand_burst);
      (0, "S", Brand_shadow);
      (4, "btfans", Brand_simplebt);
      (0, "a00---0", Brand_swarmy);
      (0, "a02---0", Brand_swarmy);
      (0, "turbobt", Brand_turbobt);
      (0, "T00---0", Brand_teeweety);
      (0, "U", Brand_upnp);
      (0, "DansClient", Brand_xantorrent);
      (0, "XBT", Brand_xbt);
    ]
  in
  let len = List.length !simple_list in
  let rec check pos =
    if pos >= len then Brand_unknown
    else
      let (x,y,z) = List.nth !simple_list pos in
      if (String.sub s x (String.length y)) = y then z
        else check (pos+1);
  in
  if (check 0) != Brand_unknown then
    check 0
  else
  (* shareaza *)
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
  if (not_zeros 0) && (weird_crap 16) then Brand_shareaza
  else
  if check_all s 3 [9;10;11] then Brand_snark
  else
  (* mldonkey style *)
  if check_all s 45 [0] then begin
      let s_id = String.sub s 1 2 in
      let result = ref
       (match s_id with
       | "ML" -> Brand_mldonkey
       | _ -> Brand_unknown)
      in
(*      if !result != "" then
        result := !result ^ " " ^ String.sub s 3 ((
          try String.index_from s 1 s.[0] with Not_found -> 3) - 3);
*)
      !result;
    end
  else
    Brand_unknown
  in
  the_answer s
  with _ -> Brand_unknown

let new_client file peer_id kind =
  try
    let c = Hashtbl.find file.file_clients kind in
    c.client_host <- kind;
    c
  with _ ->
      let rec c = {
          client_client = impl;
          client_sock = NoConnection;
          client_upload_requests = [];
          client_connection_control = new_connection_control (());
          client_file = file;
          client_host = kind;
          client_choked = true;
          client_received_peer_id = false;
          client_sent_choke = false;
          client_interested = false;
          client_uploader = None;
          client_chunks = [];
          client_ranges_sent = [];
          client_range_waiting = None;
          client_block = None;
          client_uid = peer_id;
          client_brand = if peer_id != Sha1.null then
              (parse_brand (Sha1.direct_to_string peer_id))
            else Brand_unknown;
          client_release = "";
          client_bitmap = None;
          client_allowed_to_write = zero;
          client_uploaded = zero;
          client_downloaded = zero;
          client_upload_rate = Rate.new_rate ();
          client_downloaded_rate = Rate.new_rate ();
          client_optimist_time=0;
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
          client_software = if peer_id != Sha1.null then
              (parse_software (Sha1.direct_to_string peer_id))
            else "NOT_RECEIVED";
        } and impl = {
          dummy_client_impl with
          impl_client_val = c;
          impl_client_ops = client_ops;
          impl_client_upload = None;
        } in
      c.client_connection_control.control_min_reask <- 120;
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
      lprintf_nl () "Old tracker list :%s" tracker.tracker_url
    ) file.file_trackers;
  List.iter (fun bad_tracker ->
    if bad_tracker.tracker_url = url then
    file.file_trackers <- List2.remove_first bad_tracker file.file_trackers;
  ) file.file_trackers;
  if !verbose_msg_servers then
    List.iter (fun tracker ->
      lprintf_nl () "New tracker list :%s" tracker.tracker_url
    ) file.file_trackers

let torrents_directory = "torrents"
let downloads_directory = Filename.concat torrents_directory "downloads"
let tracked_directory = Filename.concat torrents_directory "tracked"
let seeded_directory = Filename.concat torrents_directory "seeded"
let old_directory = Filename.concat torrents_directory "old"
