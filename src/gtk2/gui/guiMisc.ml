(* Copyright 2004 b8_bavard, INRIA *)
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

(* Miscellaneous functions for the GUI *)

open StdLabels

open CommonTypes
open GuiProto
open GuiTypes2
open GuiTypes
open Md4


module O = GuiOptions
module M = GuiMessages
module A = GuiArt
module U = GuiUtf8
module G = GuiGlobal
module VB = VerificationBitmap

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)


(**********************************************************************************)
(*                                                                                *)
(*                            Save GUI options                                    *)
(*                                                                                *)
(**********************************************************************************)

let save_gui_options gui =
  (* Compute layout *)
  (
   match (gui.window)#children with
     [] -> ()
   | child :: _ ->
       O.last_tab =:= gui.current_page;
  );
  Options.save_with_help O.gui_ini


(**********************************************************************************)
(*                                                                                *)
(*                      To pretty-print a file size                               *)
(*                                                                                *)
(**********************************************************************************)
let ko = Int32.of_int 1024
  
let unit_of_string s =
  match String.lowercase s with
    "mo" -> Int32.mul ko ko
  | "ko" -> ko
  | _ -> Int32.one

let ko = 1024.0
let mo = ko *. ko
let go = mo *. ko

(* To pretty-print a file size (int64) *)
let size_of_int64 size =
  let f = Int64.to_float size in
  if f > go
    then Printf.sprintf "%.2f Go" (f /. go)
    else if f > mo
      then Printf.sprintf "%.2f Mo" (f /. mo)
      else if f > ko
        then Printf.sprintf "%.2f ko" (f /. ko)
        else Int64.to_string size

let int64_of_size size =
  let len = String.length size in
  let u = String.sub size (len - 2) 2 in
  let s = String.sub size 0 (len - 3) in
  try
    match u with
      "ko" -> Int64.of_float (float_of_string s *. ko)
    | "Mo" -> Int64.of_float (float_of_string s *. mo)
    | _ -> Int64.of_float (float_of_string s *. go)
  with _ -> failwith "Error in converting size in Int64"

(*************************************************************************)
(*                                                                       *)
(*                         concat_strings                                *)
(*                                                                       *)
(*************************************************************************)

let concat_strings s1 s2 =
  if s2 = ""
    then U.utf8_of s1
    else U.utf8_of (Printf.sprintf "%s - %s" s1 s2)

(*************************************************************************)
(*                                                                       *)
(*                         To pretty display a file                      *)
(*                                                                       *)
(*************************************************************************)

let extension_of name = String.lowercase (Filename2.last_extension name)

let file_type_of_name name ~(size : GuiArt.icon_size) =
  if !!O.gtk_look_use_icons
    then begin
      let ext = extension_of name in
      let icon =
        match ext with
            ".exe"
          | ".py"
          | ".bat"
          | ".run"
          | ".com" -> M.icon_mime_binary
          | ".cda"
          | ".iso"
          | ".cue"
          | ".img"
          | ".nrg"
          | ".bin"
          | ".bwt"
          | ".ccd"
          | ".cif" -> M.icon_mime_cdimage
          | ".deb" -> M.icon_mime_debian
          | ".url"
          | ".swf"
          | ".xhtml"
          | ".xml"
          | ".html"
          | ".wml"
          | ".php"
          | ".htm" -> M.icon_mime_html
          | ".ai"
          | ".ag"
          | ".g3"
          | ".cgm"
          | ".eps"
          | ".epsi"
          | ".epsf"
          | ".gif"
          | ".xcf"
          | ".jng"
          | ".jpeg"
          | ".jpg"
          | ".msod"
          | ".pcx"
          | ".pcd"
          | ".png"
          | ".pbm"
          | ".pgm"
          | ".ppm"
          | ".pnm"
          | ".svg"
          | ".tif"
          | ".tiff"
          | ".tga"
          | ".bmp"
          | ".ico"
          | ".wmf"
          | ".xbm"
          | ".xpm"
          | ".fig"
          | ".dxf"
          | ".dwg" -> M.icon_mime_images
          | ".jar"
          | ".class"
          | ".js"
          | ".java" -> M.icon_mime_java
          | ".pdf" -> M.icon_mime_pdf
          | ".ps" -> M.icon_mime_postscript
          | ".ra"
          | ".ram"
          | ".rm"
          | ".rmvb"
          | ".rv9"
          | ".rt" -> M.icon_mime_real
          | ".bak"
          | ".old"
          | ".sik" -> M.icon_mime_recycled
          | ".rpm" -> M.icon_mime_rpm
          | ".sh"
          | ".csh" -> M.icon_mime_shellscript
          | ".xls"
          | ".xlc"
          | ".xll"
          | ".xlm"
          | ".xlw"
          | ".ppz"
          | ".ppt"
          | ".sdc"
          | ".sdd"
          | ".sdw"
          | ".doc"
          | ".rtf" -> M.icon_mime_soffice
          | ".ape"
          | ".ogg"
          | ".smil"
          | ".smi"
          | ".aiff"
          | ".mod"
          | ".s3m"
          | ".stm"
          | ".ult"
          | ".uni"
          | ".xm"
          | ".m15"
          | ".mtm"
          | ".669"
          | ".it"
          | ".mid"
          | ".pls"
          | ".mp3"
          | ".pcm"
          | ".m3u"
          | ".au"
          | ".snd"
          | ".wav" -> M.icon_mime_sound
          | ".o"
          | ".c"
          | ".cpp"
          | ".cxx"
          | ".cc"
          | ".h"
          | ".hh"
          | ".pl"
          | ".perl"
          | ".pm"
          | ".ml"
          | ".mli"
          | ".pyc"
          | ".p"
          | ".pas"
          | ".m" -> M.icon_mime_source
          | ".as"
          | ".gnumeric"
          | ".ksp"
          | ".wb1"
          | ".wb2"
          | ".wb3" -> M.icon_mime_spreadsheet
          | ".gf"
          | ".tex"
          | ".ltx"
          | ".sty"
          | ".cls"
          | ".bib" -> M.icon_mime_tex
          | ".txt"
          | ".csv"
          | ".sgml"
          | ".lyx"
          | ".dif"
          | ".diff"
          | ".patch"
          | ".css"
          | ".sst"
          | ".srt"
          | ".mpl"
          | ".scr"
          | ".sub"
          | ".dks"
          | ".jss"
          | ".pjs"
          | ".psb"
          | ".ssa"
          | ".tts"
          | ".vsf"
          | ".nfo"
          | ".zeg" -> M.icon_mime_text
          | ".a"
          | ".arj"
          | ".bz"
          | ".bz2"
          | ".z"
          | ".tgz"
          | ".gz"
          | ".zip"
          | ".ace"
          | ".lha"
          | ".lzh"
          | ".lzo"
          | ".tzo"
          | ".rar"
          | ".tar"
          | ".zoo" -> M.icon_mime_tgz
          | ".ogm"
          | ".asf"
          | ".fli"
          | ".flc"
          | ".avi"
          | ".mng"
          | ".mp2"
          | ".mp4"
          | ".mpe"
          | ".mpeg"
          | ".mpg"
          | ".qt"
          | ".movie"
          | ".mov"
          | ".moov"
          | ".qtvr"
          | ".vob"
          | ".wma"
          | ".wmv" -> M.icon_mime_video
          | ".abw"
          | ".zabw"
          | ".bzabw"
          | ".aw"
          | ".hwp"
          | ".kwd"
          | ".kwt"
          | ".sam"
          | ".wpd"-> M.icon_mime_wordprocessing
          | _ -> M.icon_mime_unknown
      in
      let pixb =  A.get_icon ~icon ~size () in
      Some pixb
    end else None

(**********************************************************************************)
(*                                                                                *)
(*                  Miscellaneous functions for a name                            *)
(*                                                                                *)
(**********************************************************************************)

let result_first_name names =
  let name = ref "<unknown>" in
  let _ =
    try
      List.iter (fun n ->
        if n <> "" && extension_of n <> ""
          then begin name := n; raise Exit end
      ) names
    with _ -> ()
  in
  U.utf8_of !name

(**********************************************************************************)
(*                                                                                *)
(*                  Miscellaneous functions for a search                          *)
(*                                                                                *)
(**********************************************************************************)

let create_search query_entry max_hits net search_type =
  incr GuiGlobal.search_counter;
  let s = {
      GuiTypes.search_num = !GuiGlobal.search_counter ;
      GuiTypes.search_query = query_entry ;
      GuiTypes.search_max_hits = max_hits ;
      GuiTypes.search_type = search_type;
      GuiTypes.search_network = net;
  } 
  in
  s

let rec rec_description_of_query q =
  match q with
  | Q_HIDDEN l
  | Q_AND l
  | Q_OR l -> List.flatten (List.map rec_description_of_query l)

  | Q_ANDNOT (q1, q2) -> rec_description_of_query q1
  | Q_MODULE (_,q) -> rec_description_of_query q

  | Q_KEYWORDS (_,s) -> [s]
  | Q_MINSIZE _ 
  | Q_MAXSIZE _ -> []
  | Q_FORMAT (_,s)
  | Q_MEDIA  (_,s)
  | Q_MP3_ARTIST (_,s)
  | Q_MP3_TITLE (_,s)
  | Q_MP3_ALBUM (_,s) -> [s]

  | Q_COMBO _ -> []

  | Q_MP3_BITRATE _ -> []

(* Return few words for a given query *)
let description_of_query q =
  match rec_description_of_query q with 
    [] -> "stupid query"
  | [s] -> s
  | [s1 ; s2] -> s1^" "^s2
  | s1 :: s2 :: s3 :: _ -> s1^" "^s2^" "^s3


(**********************************************************************************)
(*                                                                                *)
(*                        Functions on networks                                   *)
(*                                                                                *)
(**********************************************************************************)

let net_has_server n =
  let list = n.net_flags in
  try
    List.iter (fun flag ->
      match flag with
          NetworkHasServers -> raise Exit
        | _ -> ()
    ) list;
    false
  with Exit -> true

let net_has_search n =
  let list = n.net_flags in
  try
    List.iter (fun flag ->
      match flag with
          NetworkHasSearch -> raise Exit
        | _ -> ()
    ) list;
    false
  with Exit -> true

(*************************************************************************)
(*                                                                       *)
(*                         To remove the markups used in strings         *)
(*                                                                       *)
(*************************************************************************)

let remove_ s =
  String2.replace s '_' ""


(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a file state                  *)
(*                                                                       *)
(*************************************************************************)

let string_of_file_state state rate =
  match state with
  | FileDownloading -> if rate > 0.
                      then !M.dT_tx_downloading
                      else !M.dT_tx_waiting
  | FileCancelled -> !M.dT_tx_cancelled
  | FileQueued -> !M.dT_tx_queued
  | FilePaused -> !M.dT_tx_paused
  | FileDownloaded -> !M.dT_tx_complete
  | FileShared  -> !M.dT_tx_dl_done
  | FileNew -> assert false
  | FileAborted s -> U.simple_utf8_of (Printf.sprintf !M.dT_tx_dl_aborted s)


(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a source state                *)
(*                                                                       *)
(*************************************************************************)

let string_of_state state num =
  match state with
    Connected_downloading n when n = num -> !M.dT_tx_downloading
  | Connected_downloading _ -> ""
  | Connected (-1) -> !M.dT_tx_connected
  | Connected (-2) -> !M.dT_tx_connected_high
  | Connecting  -> !M.dT_tx_connecting
  | NewHost -> !M.dT_tx_new_host
  | Connected_initiating -> !M.dT_tx_initiating
  | Connected 0 -> !M.dT_tx_queued
  | Connected n -> U.simple_utf8_of (Printf.sprintf !M.dT_tx_ranked n)
  | ServerFull -> ""
  | NotConnected (_,n) ->
      if n = -1 then
        ""
      else
      if n = 0 then
        (!M.dT_tx_queued_out)
      else
      if n > 0 then
         U.simple_utf8_of (Printf.sprintf !M.dT_tx_ranked_out n)
      else
         U.simple_utf8_of (Printf.sprintf !M.dT_tx_failed (- n - 1))
       
  | RemovedHost -> !M.dT_tx_removed
  | BlackListedHost -> !M.dT_tx_black_listed

let is_connected state =
  match state with
  | Connected_initiating
  | Connected_downloading _
  | Connected _ -> true
  | NotConnected _
  | Connecting
  | RemovedHost
  | BlackListedHost
  | ServerFull
  | NewHost -> false

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print an uploader state             *)
(*                                                                       *)
(*************************************************************************)

let uploader_state_to_string state has_upload =
  if has_upload = source_has_upload
    then begin
      match state with
          Connected_downloading _  -> !M.dT_tx_updown
        | _ -> !M.dT_tx_uploading
   
    end else
      match state with
          Connected_downloading _ -> !M.dT_tx_downloading
        | Connected n when n < 0-> !M.dT_tx_connected
        | Connecting  -> !M.dT_tx_connecting
        | NewHost -> !M.dT_tx_new_host
        | Connected_initiating -> !M.dT_tx_initiating
        | Connected 0 -> !M.dT_tx_queued
        | Connected n -> U.simple_utf8_of (Printf.sprintf !M.dT_tx_ranked n)
        | ServerFull -> ""
        | NotConnected (_,n) ->
            if n = -1
              then ""
              else if n = 0
                then !M.dT_tx_queued_out
                else if n > 0
                  then U.simple_utf8_of (Printf.sprintf !M.dT_tx_ranked_out n)
                  else U.simple_utf8_of (Printf.sprintf !M.dT_tx_failed (- n - 1))

        | RemovedHost -> !M.dT_tx_removed
        | BlackListedHost -> !M.dT_tx_black_listed

(*************************************************************************)
(*                                                                       *)
(*                         To pretty display a client type               *)
(*                                                                       *)
(*************************************************************************)

let source_type_to_icon client_type ~(size : GuiArt.icon_size) =
  if !!O.gtk_look_use_icons
    then if client_friend_tag land client_type <> 0
      then Some (A.get_icon ~icon:M.icon_type_source_friend ~size ())
      else if client_contact_tag land client_type <> 0
        then Some (A.get_icon ~icon:M.icon_type_source_contact ~size ())
        else Some (A.get_icon ~icon:M.icon_type_source_normal ~size ())
    else None

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a client type                 *)
(*                                                                       *)
(*************************************************************************)

let source_type_to_string client_type =
  if client_friend_tag land client_type <> 0
    then !M.uT_tx_friend
    else if client_contact_tag land client_type <> 0
      then !M.uT_tx_contact
      else !M.uT_tx_normal

(*************************************************************************)
(*                                                                       *)
(*                         To pretty display a client state              *)
(*                                                                       *)
(*************************************************************************)

let client_state_to_icon has_file ~(size : GuiArt.icon_size) =
  if !!O.gtk_look_use_icons
    then begin
      let pixb =
        if has_file
          then Some (A.get_icon ~icon:M.icon_state_source_fileslisted ~size ())
          else None
      in
      pixb
    end else None

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print ip:port                       *)
(*                                                                       *)
(*************************************************************************)

let address_to_string addr port =
  U.simple_utf8_of (
    Printf.sprintf "%16s : %-5d"
      (Ip.string_of_addr addr) port
  )

let ip_to_string ip port =
  let addr =  Ip.addr_of_ip ip in
  match addr with
      Ip.AddrIp _ -> address_to_string addr port
    | _ -> "Low ID"

(*************************************************************************)
(*                                                                       *)
(*                         To pretty display a server state              *)
(*                                                                       *)
(*************************************************************************)

let server_state_of_server net_num state ~(size : GuiArt.icon_size) =
  if !!O.gtk_look_use_icons
    then begin
      let icon =
        match state with
            Connected (-2) -> M.icon_state_server_conh
          | Connected _ -> 
              if (Hashtbl.find G.networks net_num).net_name = "Donkey"
                then M.icon_state_server_conl
                else M.icon_state_server_conh
          | Connected_initiating
          | Connecting  -> M.icon_state_server_init
          | NotConnected _ -> M.icon_state_server_notcon
          | NewHost
          | ServerFull
          | Connected_downloading _
          | RemovedHost
          | BlackListedHost -> M.icon_state_server_unknown
      in
      let pixb =  A.get_icon ~icon ~size () in
      Some pixb
    end else None

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a network                     *)
(*                                                                       *)
(*************************************************************************)

let network_name num = try
    match (Hashtbl.find G.networks num).net_name with
      "Direct Connect" -> "DC"
    | "Donkey" -> "eDK"
    | "OpenNapster" -> "NAP"
    | "G2" -> "GTL2"
    | "Gnutella" -> "GTL1"
    | "Fasttrack" -> "FT"
    | "Soulseek" -> "SLSK"
    | "FileTP" -> "FTP"
    | n -> n
  with _ -> "?"

let network_full_name num = try
    match (Hashtbl.find G.networks num).net_name with
      "Direct Connect" -> !M.nT_lb_net_dc
    | "Donkey" -> !M.nT_lb_net_ed2k
    | "OpenNapster" -> !M.nT_lb_net_nap
    | "G2" -> !M.nT_lb_net_gnut2
    | "Gnutella" -> !M.nT_lb_net_gnut
    | "Fasttrack" -> !M.nT_lb_net_ftt
    | "Soulseek" -> !M.nT_lb_net_slsk
    | "BitTorrent" -> !M.nT_lb_net_bt
    | "FileTP" -> !M.nT_lb_net_filetp
    | n -> U.simple_utf8_of n
  with _ -> "?"

(*************************************************************************)
(*                                                                       *)
(*                        networkname_from_uids                          *)
(*                                                                       *)
(*************************************************************************)

let neworknum_from_uids uids =
  let nets_by_name = List.map (fun n -> (n.net_name, n.net_num)) (Hashtbl2.to_list G.networks) in
  let rec iter l =
    match l with
        [] -> 0
       | uid :: tail ->
           try
             match Uid.to_uid uid with
                 Md5Ext _ -> List.assoc "Fasttrack" nets_by_name
               | Ed2k _ -> List.assoc "Donkey" nets_by_name
               | Bitprint _
               | Sha1 _
               | TigerTree _ -> List.assoc "G2" nets_by_name
               | _ -> iter tail
            with _ -> iter tail
  in
  iter uids

(*************************************************************************)
(*                                                                       *)
(*                         To pretty display a network                   *)
(*                                                                       *)
(*************************************************************************)

let network_pixb num ~(size : GuiArt.icon_size) ?desat () =
  if !!O.gtk_look_use_icons
    then begin
      let icon =
        try
          match (Hashtbl.find G.networks num).net_name with
              "BitTorrent" -> M.icon_net_bittorrent
            | "Direct Connect" -> M.icon_net_dc
            | "Donkey" -> M.icon_net_ed2k
            | "OpenNapster" -> M.icon_net_napster
            | "G2" -> M.icon_net_gnutella2
            | "Gnutella" -> M.icon_net_gnutella1
            | "Fasttrack" -> M.icon_net_fasttrack
            | "Soulseek" -> M.icon_net_soulseek
            | "FileTP" -> M.icon_net_filetp
            | _ -> M.icon_mime_unknown
         with _ -> M.icon_mime_unknown
      in
      let pixb = A.get_icon ~icon ~size ?desat () in
      Some pixb
    end else None

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a percentage                  *)
(*                                                                       *)
(*************************************************************************)

let get_percent_of a b =
  if Int64.to_float a > 0. 
    then Printf.sprintf "%5.1f" 
          (Int64.to_float a /. Int64.to_float b *. 100.)
    else "0"

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a rate                        *)
(*                                                                       *)
(*************************************************************************)

let rate_to_string rate =
  if rate > 0.
    then Printf.sprintf "%5.1f ko/s" (rate /. 1024.)
    else ""

let source_upload_rate s_new s =
  if (s_new.source_last_seen -. s.source_last_seen) <= 0. ||
      Int64.to_int s_new.source_uploaded = 0
  then 0.
  else max 0.
    ((Int64.to_float s_new.source_uploaded -. Int64.to_float s.source_uploaded) /.
     (s_new.source_last_seen -. s.source_last_seen))

let source_download_rate s_new s =
  if (s_new.source_last_seen -. s.source_last_seen) <= 0. ||
      Int64.to_int s_new.source_downloaded = 0
  then 0.
  else max 0.
    ((Int64.to_float s_new.source_downloaded -. Int64.to_float s.source_downloaded) /.
     (s_new.source_last_seen -. s.source_last_seen))

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print chunks                        *)
(*                                                                       *)
(*************************************************************************)

let chunks_to_string chunks =
  match chunks with
  | None -> "0"
  | Some chunks -> string_of_int (VB.length chunks)

let completed_chunks_to_string chunks =
  match chunks with
  | None -> "0"
  | Some chunks ->
      let len = VB.length chunks in
      let p = VB.fold_lefti (fun acc _ s -> match s with
        | VB.State_missing | VB.State_partial -> acc
        | VB.State_complete | VB.State_verified -> acc + 1
      ) 0 chunks in
      Printf.sprintf "%d (%d%%)" p (p * 100 / len)

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print availability                  *)
(*                                                                       *)
(*************************************************************************)

let some_is_available availability chunks =
  if !!O.gtk_misc_relative_availability
  then
    String2.existsi (fun i a ->
      CommonGlobals.partial_chunk (VerificationBitmap.get chunks i) &&
        a <> (char_of_int 0)
    ) availability
  else
    String2.exists ((<>) (char_of_int 0)) availability

let relative_availability_of avail chunks =
  match chunks with
  | None -> "0."
  | Some c ->
      let rec loop i p n =
        if i < 0 then
          if n = 0 then "0." (* Watch out !! Don't modify, we have to keep a float format *)
          else Printf.sprintf "%5.1f" ((float p) /. (float n) *. 100.)
        else 
          loop (i - 1) 
            (try if CommonGlobals.partial_chunk (VerificationBitmap.get c i) then p + 1 else p with _ -> p)
            (if avail.[i] <> (char_of_int 0) then n + 1 else n) in
      loop ((String.length avail) - 1) 0 0


let absolute_availability_of s =
  let len = String.length s in
  let p = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> (char_of_int 0)
      then (incr p)
  done;
  if len = 0
    then "0." (* Watch out !! Don't modify, we have to keep a float format *)
    else Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)


let string_of_availability availability chunks =
  if !!O.gtk_misc_relative_availability
    then relative_availability_of availability chunks
    else absolute_availability_of availability

let main_availability_of net availabilities  =
  try
    List.assoc net availabilities
  with _ -> ""

let availability_bar availability chunks b =
  if !!O.gtk_look_graphical_availability
    then begin
      let pixb = A.get_availability_of availability chunks b in
      Some pixb
    end else None

let get_availability_bar_image (avail,chunks) av_max is_file =
  let avail = Bytes.of_string avail in
  (match chunks with
  | None ->
      for i = 0 to Bytes.length avail - 1 do
        Bytes.set avail i (char_of_int (av_max - 1))
      done
  | Some chunks ->
      for i = 0 to Bytes.length avail - 1 do
        Bytes.set avail i
          (char_of_int (match VB.get chunks i with
          | VB.State_complete | VB.State_verified ->
              av_max
          | VB.State_missing ->
              let avail_int =
                if is_file then int_of_char @@ Bytes.get avail i
                else if int_of_char @@ Bytes.get avail i > 48 then 1 else 0
              in
              min (av_max - 2) avail_int
          | VB.State_partial ->
              av_max - 1))
      done);
  Bytes.unsafe_to_string avail

let sort_availability_bar a1 a2 is_file =
  if !!O.gtk_misc_use_availability_height && is_file
    then begin
      let av_max = !!O.gtk_misc_availability_max + 2 in
      let avail1 = get_availability_bar_image a1 av_max is_file in
      let avail2 = get_availability_bar_image a2 av_max is_file in
      compare avail1 avail2
    end else begin
      let av_max = 3 in
      let avail1 = get_availability_bar_image a1 av_max is_file in
      let avail2 = get_availability_bar_image a2 av_max is_file in
      compare avail1 avail2
    end

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a file format                 *)
(*                                                                       *)
(*************************************************************************)

let format_to_string format =
  match format with
    AVI f ->
      U.simple_utf8_of (Printf.sprintf "AVI: %s %dx%d %g fps %d bpf"
        f.avi_codec f.avi_width f.avi_height 
        (float_of_int(f.avi_fps) *. 0.001) f.avi_rate)
  | MP3 (tag, _) ->
      let module MP = Mp3tag.Id3v1 in
      U.simple_utf8_of (Printf.sprintf "MP3: %s - %s (%d): %s"
        tag.MP.artist tag.MP.album 
        tag.MP.tracknum tag.MP.title)
  | OGG l ->
      let s = ref "" in
      List.iter (fun st ->
        if !s = ""
          then s := Printf.sprintf "[%d] %s"
                      st.stream_no (stream_type_to_string st.stream_type)
          else s := Printf.sprintf "%s :: [%d] %s"
                      !s st.stream_no (stream_type_to_string st.stream_type);
        List.iter (fun tag ->
          s := Printf.sprintf "%s %s" !s (ogg_tag_to_string tag)
        ) st.stream_tags
      ) l;
      U.simple_utf8_of !s
  | _ -> (!M.dT_tx_unknown)

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a time                        *)
(*                                                                       *)
(*************************************************************************)

let invalid_string = "---"

let time_to_string time =
  if time = max_int || time < 0
    then invalid_string
    else begin
      let days = time / 60 / 60 / 24 in
      let rest = time - days * 60 * 60 * 24 in
      let hours = rest / 60 / 60 in
      let rest = rest - hours * 60 * 60 in
      let minutes = rest / 60 in
      let seconds = rest - minutes * 60 in
      if days > 0
        then Printf.sprintf "%dd" days
        else if hours > 0
            then U.simple_utf8_of (Printf.sprintf "%dh:%02dmn:%02ds" hours minutes seconds)
            else U.simple_utf8_of (Printf.sprintf "%dmn:%02ds" minutes seconds)
    end

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print an ETA                        *)
(*                                                                       *)
(*************************************************************************)

let calc_eta_inst size dled rate =
  let size = Int64.to_float size in
  let downloaded = Int64.to_float dled in
  let missing = size -. downloaded in
  let eta =
     if rate <= 0.
       then invalid_string
       else begin
         let time = int_of_float (missing /. rate) in
         time_to_string time
       end
  in
  eta

let calc_eta_average size dled age =
  let size = Int64.to_float size in
  let downloaded = Int64.to_float dled in
  let missing = size -. downloaded in
  let rate =
    if age > 0
      then downloaded /. (float_of_int age)
      else 0.
  in
  let eta =
    if rate = 0.
      then invalid_string
      else begin
        let eta = missing /. rate in
        time_to_string (int_of_float eta)
      end
  in
  eta

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a file priority               *)
(*                                                                       *)
(*************************************************************************)

let priority_to_string prio =
  match prio with
      -20 -> !M.dT_tx_priority_verylow
    | -10 -> !M.dT_tx_priority_low
    | 0 -> !M.dT_tx_priority_normal
    | 10 -> !M.dT_tx_priority_high
    | 20 -> !M.dT_tx_priority_veryhigh
    | _ -> Printf.sprintf "%d" prio


(*************************************************************************)
(*                                                                       *)
(*                         To pretty print file_sources                  *)
(*                                                                       *)
(*************************************************************************)

let sources_to_string sources =
  match sources with
      None -> "0"
    | Some list -> Printf.sprintf "%d" (List.length list)

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print the average download rate     *)
(*                                                                       *)
(*************************************************************************)

let average_rate dled age = 
  let downloaded = Int64.to_float dled in
  let rate =
    if age > 0
      then downloaded /. (float_of_int age)
      else 0.
  in
  Printf.sprintf "%5.1f ko/s" (rate /. 1024.)

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a client_kind                 *)
(*                                                                       *)
(*************************************************************************)

let location_kind_to_string kind =
  match kind with
      Known_location _ -> "High ID"
    | Indirect_location _ -> "Low ID"

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a client hash                 *)
(*                                                                       *)
(*************************************************************************)

let hash_of_client kind =
  match kind with
      Known_location _ -> "-"
    | Indirect_location (_, md4, _, _) -> Printf.sprintf "%s" (Md4.to_string md4)

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a tag                         *)
(*                                                                       *)
(*************************************************************************)

let string_of_tag_name name =
  match name with
      Field_Artist -> "artist"
    | Field_Title -> "title"
    | Field_Album -> "album"
    | Field_Format -> "format"
    | Field_Type -> "type"
    | Field_Length -> "length"
    | Field_Bitrate -> "bitrate"
    | Field_Codec -> "codec"
    | Field_Availability -> "availability"
    | Field_Completesources -> "completesources"
    | Field_Filename -> "filename"
    | Field_Size -> "size"
    | Field_Size_Hi -> "size_hi"
    | Field_Uid -> "uid"
    | Field_Medialength -> "length"
    | Field_Mediacodec -> "codec"
    | Field_Lastseencomplete -> "lastseencompl"
    | Field_Filerating -> "rating"
    | Field_KNOWN s -> U.simple_utf8_of s
    | Field_UNKNOWN s -> U.simple_utf8_of s
  
let tags_to_string tags =
  let s = ref "" in
  List.iter (fun tag ->
    s := !s ^ " - " ^ (CommonTypes.string_of_tag_value tag.tag_value) ^ ": " ^ (string_of_tag_name tag.tag_name);
  ) tags;
  U.utf8_of !s

let int_to_string n =
  if n = 0
    then ""
    else string_of_int n

let string_to_int s =
  try
    int_of_string s
  with _ -> 0

let duration_of_tags tags =
  let value = ref "" in
  List.iter (fun t ->
    match t.tag_name with
        Field_Length -> value := U.simple_utf8_of (CommonTypes.string_of_tag_value t.tag_value)
      | _ -> ()
  ) tags;
  !value

let codec_of_tags tags =
  let value = ref "" in
  List.iter (fun t ->
    match t.tag_name with
        Field_Codec -> value := U.simple_utf8_of (CommonTypes.string_of_tag_value t.tag_value)
      | _ -> ()
  ) tags;
  !value

let bitrate_of_tags tags =
  let value = ref 0 in
  List.iter (fun t ->
    match t.tag_name with
        Field_Bitrate -> value := string_to_int (CommonTypes.string_of_tag_value t.tag_value)
      | _ -> ()
  ) tags;
  !value

let availability_of_tags tags =
  let value = ref 0 in
  List.iter (fun t ->
    match t.tag_name with
        Field_Availability -> value := string_to_int (CommonTypes.string_of_tag_value t.tag_value)
      | _ -> ()
  ) tags;
  !value

let completesources_of_tags tags =
  let value = ref 0 in
  List.iter (fun t ->
    match t.tag_name with
        Field_Completesources -> value := string_to_int (CommonTypes.string_of_tag_value t.tag_value)
      | _ -> ()
  ) tags;
  !value

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a result                      *)
(*                                                                       *)
(*************************************************************************)

let color_of_result avail b =
  if b
    then !!O.gtk_color_state_not_available
    else if avail = 0
      then !!O.gtk_color_default
      else if avail >= !!O.gtk_misc_availability_max * 10
        then !!O.gtk_color_state_files_listed
        else begin
          let avail = float_of_int avail  in
          let max_avail = float_of_int (!!O.gtk_misc_availability_max * 10) in
          let col =
            GDraw.color (`NAME !!O.gtk_color_state_files_listed)
              ~colormap:(Gdk.Color.get_system_colormap ())
          in
          let r = float_of_int ((Gdk.Color.red col) / 256) in
          let g = float_of_int ((Gdk.Color.green col) / 256) in
          let b = float_of_int ((Gdk.Color.blue col) / 256) in
          let r = int_of_float (r +. (185. -. r) *. (1. -. avail  /. max_avail)) in
          let g = int_of_float (g +. (185. -. g) *. (1. -. avail  /. max_avail)) in
          let b = int_of_float (b +. (185. -. b) *. (1. -. avail  /. max_avail)) in
          let s = Printf.sprintf "#%02X%02X%02X" r g b in
          s
      end

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print the client_upload             *)
(*                                                                       *)
(*************************************************************************)

let upload_to_string upload =
  match upload with
      None -> "-"
    | Some name -> U.utf8_of name

(*************************************************************************)
(*                                                                       *)
(*                         To pretty print a room state                  *)
(*                                                                       *)
(*************************************************************************)

let room_state_to_string state =
  match state with
      RoomPaused -> !M.rT_lb_paused_room 
    | RoomClosed -> !M.rT_lb_closed_room
    | RoomOpened -> !M.rT_lb_opened_room

(*************************************************************************)
(*                                                                       *)
(*                         To pretty display a room state                *)
(*                                                                       *)
(*************************************************************************)

let room_state_to_icon state ~(size : GuiArt.icon_size) =
  if !!O.gtk_look_use_icons
    then begin
      let icon = M.icon_menu_rooms in
      let desat =
        match state with
              RoomOpened -> false
            | _ -> true
      in
      let pixb = A.get_icon ~icon ~size ~desat () in
      Some pixb
    end else None

(*************************************************************************)
(*                                                                       *)
(*                         functions on client_files                     *)
(*                                                                       *)
(*************************************************************************)

(* Taken from GuiTypes.ml *)

let add_file tree dirname r =
  let path = Filename2.path_of_filename dirname in

  let rec iter list tree =
    match list with
      [] ->
        let r = GTreeFile r in
        (if not (List.mem r tree.g_file_tree_list) then
            tree.g_file_tree_list <- r :: tree.g_file_tree_list)
    | dirname :: tail ->
        iter2 tail tree dirname tree.g_file_tree_list

  and iter2 list tree dirname items =
    match items with
      [] ->
        let new_tree =
          {
           g_file_tree_num = 0;
           g_file_tree_name = dirname;
           g_file_tree_list = [];
           g_file_tree_pixb = None;
          }
        in
        tree.g_file_tree_list <- (GTreeDirectory new_tree) :: tree.g_file_tree_list;
        iter list new_tree
    | (GTreeDirectory old_tree) :: items ->
        if old_tree.g_file_tree_name = dirname then
          iter list old_tree
        else
          iter2 list tree dirname items
    | _ :: items ->
        iter2 list tree dirname items
  in
  iter path tree    

let list_directory_files tree =
  let rec iter list items =
    match items with
      [] -> list
    | (GTreeDirectory tree) :: items -> iter list items
    | (GTreeFile r) :: items -> iter (r :: list) items
  in
  iter [] tree.g_file_tree_list

let list_files tree =
  let rec iter list items =
    match items with
      [] -> list
    | (GTreeDirectory tree) :: items -> 
        iter (iter list tree.g_file_tree_list) items
    | (GTreeFile r) :: items -> iter (r :: list) items
  in
  iter [] tree.g_file_tree_list

(*************************************************************************)
(*                                                                       *)
(*                        client_to_source                               *)
(*                                                                       *)
(*************************************************************************)

let client_to_source c =
  let s =
    {
     source_num             = c.client_num;
     source_network         = c.client_network;

     source_kind            = c.client_kind;
     source_state           = c.client_state;
     source_type            = c.client_type;
     source_tags            = c.client_tags;
     source_name            = U.utf8_of c.client_name;
     source_files           = None;
     source_rating          = c.client_rating;
     source_chat_port       = c.client_chat_port;
     source_connect_time    = BasicSocket.last_time () - c.client_connect_time;
     source_last_seen       = BasicSocket.current_time ();
     source_software        = concat_strings c.client_software (concat_strings c.client_emulemod c.client_release);
     source_downloaded      = c.client_total_downloaded;
     source_uploaded        = c.client_total_uploaded;
     source_upload_rate     = 0.;
     source_download_rate   = 0.;
     source_upload          = c.client_upload;
     source_has_upload      = source_only;
     source_availability    = [];
     source_files_requested = [];
    }
  in
  s

(*************************************************************************)
(*                                                                       *)
(*                        uid_to_common_uid                              *)
(*                                                                       *)
(*************************************************************************)

let uid_to_common_uid uid =
  match uid with
    Bitprint (sha1,ttr) -> Sha1 sha1
  | BTUrl url -> Sha1 url
  | _ -> uid

(*************************************************************************)
(*                                                                       *)
(*                        ustring_of_uid                                 *)
(*                                                                       *)
(*************************************************************************)

let ustring_of_uid uid =
  let sep = ":" in
  match uid with
    Bitprint (sha1,ttr) ->
      "urn" ^ sep ^ "sha1" ^ sep ^ (Sha1.to_string sha1)
  | Sha1 sha1 ->
      "urn" ^ sep ^ "sha1" ^ sep ^ (Sha1.to_string sha1)
  | Ed2k ed2k ->
      "urn" ^ sep ^ "ed2k" ^ sep ^ (Md4.to_string ed2k)
  | Md5 md5 ->
      "urn" ^ sep ^ "md5" ^ sep ^ (Md5.to_string md5)
  | TigerTree ttr -> 
      "urn" ^ sep ^ "ttr" ^ sep ^ (TigerTree.to_string ttr)
  | Md5Ext md5 ->
      "urn" ^ sep ^ "sig2dat" ^ sep ^ (Md5Ext.to_base32 md5)
  | BTUrl url ->
      "urn" ^ sep ^ "sha1" ^ sep ^ (Sha1.to_string url)
  | FileTP file ->
      "urn" ^ sep ^ "filetp" ^ sep ^ (Md4.to_string file)
  | NoUid -> ""

(*************************************************************************)
(*                                                                       *)
(*                        normalize_uids                                 *)
(*                                                                       *)
(*************************************************************************)

let normalize_uids uid_list =
  let l = ref [] in
  List.iter (fun t ->
    let uid_type = uid_to_common_uid (Uid.to_uid t) in
    let _t = Uid.create uid_type in
    if not (List.mem _t !l) then l := _t :: !l
  ) uid_list;
  !l

(*************************************************************************)
(*                                                                       *)
(*                        uid_list_to_string                             *)
(*                                                                       *)
(*************************************************************************)

let uid_list_to_string l =
  match l with
      [] -> ""
    | uid :: _ -> U.simple_utf8_of (ustring_of_uid (Uid.to_uid uid))

(*************************************************************************)
(*                                                                       *)
(*                        to_uid_type                                    *)
(*                                                                       *)
(*************************************************************************)

let to_uid_type l =
  match l with
      [] -> NoUid
    | uid :: _ -> Uid.to_uid uid

(*************************************************************************)
(*                                                                       *)
(*                        shared_info_to_shared_file                     *)
(*                                                                       *)
(*************************************************************************)

let shared_info_to_shared_file si =
  {
    g_shared_num       = si.shared_num;
    g_shared_network   = si.shared_network;
    g_shared_filename  = si.shared_filename;
    g_shared_size      = si.shared_size;
    g_shared_uploaded  = si.shared_uploaded;
    g_shared_requests  = si.shared_requests;
    g_shared_uids      = normalize_uids si.shared_uids;
    g_shared_last_seen = BasicSocket.current_time ();
  }

(*************************************************************************)
(*                                                                       *)
(*                        file_info_to_g_file_info                       *)
(*                                                                       *)
(*************************************************************************)

let file_info_to_g_file_info f =
  {
    g_file_num             = f.file_num;
    g_file_network         = f.file_network;

    g_file_comment         = f.file_comment;
    g_file_name            = f.file_name;
    g_file_names           = f.file_names;
    g_file_size            = f.file_size;
    g_file_downloaded      = f.file_downloaded;
    g_file_active_sources  = f.file_active_sources;
    g_file_all_sources     = f.file_all_sources;
    g_file_state           = f.file_state;
    g_file_chunks          = f.file_chunks;
    g_file_availability    = f.file_availability;
    g_file_sources         = f.file_sources;
    g_file_download_rate   = f.file_download_rate;
    g_file_format          = f.file_format;
    g_file_chunks_age      = f.file_chunks_age;
    g_file_age             = (BasicSocket.last_time () - f.file_age);
    g_file_last_seen       = (BasicSocket.last_time () - f.file_last_seen);
    g_file_priority        = f.file_priority;
    g_file_uids            = normalize_uids f.file_uids;

    g_file_stats           = [];
  }
