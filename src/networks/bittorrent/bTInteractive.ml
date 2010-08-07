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
open Options
open Printf2
open Md4
open BasicSocket

open CommonSearch
open CommonGlobals
open CommonUser
open CommonClient
open CommonOptions
open CommonServer
open CommonResult
open CommonTypes
open CommonComplexOptions
open CommonFile
open CommonDownloads
open CommonShared
open CommonInteractive
open Autoconf

open BTTypes
open BTOptions
open BTGlobals
open BTComplexOptions
open BTProtocol

open Bencode

open Gettext
let _s x = _s "BTInteractive" x
let _b x = _b "BTInteractive" x

module VB = VerificationBitmap

let porttest_result = ref PorttestNotStarted

let interpret_azureus_porttest s =
  let failure_message fmt = 
    Printf.sprintf ("Port test failure, " ^^ fmt) in
  try
    let value = decode s in 
    match value with
    | Dictionary alist ->
        (try
           match List.assoc "result" alist with
           | Int 1L -> "Port test OK!"
           | Int 0L ->
               (try
                  match List.assoc "reason" alist with
                  | String reason -> failure_message "%s" reason
                  | _ -> raise Not_found
                with Not_found ->
                  failure_message "%s" "no reason given")
           | Int status ->
               failure_message "unknown status code (%Ld)" status
           | _ -> raise Not_found
         with Not_found ->
           failure_message "%s" "no status given")
    | _ ->
        failure_message "unexpected value type %s" (Bencode.print value)
  with _ ->
    failure_message "%s" "broken bencoded value"

let interpret_utorrent_porttest s =
  if String2.contains s "<div class=\"status-image\">OK!</div>" then
    "Port test OK!"
  else
    "Port is not accessible"

let perform_porttests tests =
  match tests with
  | [] -> porttest_result := PorttestResult (last_time(), "No tests available")
  | _ ->
  let module H = Http_client in
  porttest_result := PorttestInProgress (last_time ());
  let rec loop = function
  | [] -> ()
  | (url,interpret)::other ->
    let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_user_agent = get_user_agent ();
      (* no sense in using proxy anyway *)
(*       H.req_proxy = !CommonOptions.http_proxy; *)
      H.req_max_total_time = 45.;
    } in
    H.wget_string r 
      (fun s -> porttest_result := PorttestResult (last_time (), interpret s))
      ~ferr:(fun code -> 
        porttest_result := PorttestResult (last_time (), Printf.sprintf "Remote service error (%d)" code);
        loop other)
      (fun _ _ -> ())
  in
  loop tests

let op_file_all_sources file =
  let list = ref [] in
  Hashtbl.iter (fun _ c ->
      list := (as_client c) :: !list
  ) file.file_clients;
  !list

let op_file_active_sources file =
  let list = ref [] in
  Hashtbl.iter (fun _ c ->
      let as_c = as_client c in
      match client_state as_c with
        Connected_downloading _ -> list := as_c :: !list
      | _ -> ()
  ) file.file_clients;
  !list

let op_file_files file impl =
  match file.file_swarmer with
    None -> [CommonFile.as_file impl]
  | Some swarmer ->
      CommonSwarming.subfiles swarmer

let op_file_debug file =
  let buf = Buffer.create 100 in
(*      CommonSwarming.debug_print buf file.file_swarmer; *)
  Hashtbl.iter (fun _ c ->
      Printf.bprintf buf "Client %d: %s\n" (client_num c)
      (match c.client_sock with
          NoConnection -> "No Connection"
        | Connection _  -> "Connected"
        | ConnectionWaiting _ -> "Waiting for Connection"
      )
  ) file.file_clients;
  Buffer.contents buf

let op_file_commit file new_name =
  CommonSwarming.remove_swarmer file.file_swarmer;
  file.file_swarmer <- None;
  if file_state file <> FileShared then
    begin
      if not (List.mem (file.file_name, file_size file) !!old_files) then
        old_files =:= (file.file_name, file_size file) :: !!old_files;
      set_file_state file FileShared;

      if Unix32.destroyed (file_fd file) then
        if !verbose then lprintf_file_nl (as_file file) "op_file_commit: FD is destroyed... repairing";

(* During the commit operation, for security, the file_fd is destroyed. So
  we create it again to be able to share this file again. *)
      set_file_fd
        (as_file file)
        (create_temp_file new_name (List.map (fun (file,size,_) -> (file,size)) file.file_files) (file_state file));

      if Unix32.destroyed (file_fd file) then
        lprintf_file_nl (as_file file) "op_file_commit: FD is destroyed... could not repair!";

      let new_torrent_diskname =
        Filename.concat seeded_directory
          (Filename.basename file.file_torrent_diskname)
      in
      (try
          Unix2.rename file.file_torrent_diskname new_torrent_diskname;
        with _ ->
          (lprintf_file_nl (as_file file) "op_file_commit: failed to rename %s to %s"
              file.file_torrent_diskname new_torrent_diskname));
      file.file_torrent_diskname <- new_torrent_diskname;

      (* update file_shared with new path to committed file *)
      match file.file_shared with
      | None -> ()
      | Some old_impl ->
        begin
          let impl = {
            impl_shared_update = 1;
            impl_shared_fullname = file_disk_name file;
            impl_shared_codedname = old_impl.impl_shared_codedname;
            impl_shared_size = file_size file;
            impl_shared_id = Md4.null;
            impl_shared_num = 0;
            impl_shared_uploaded = old_impl.impl_shared_uploaded;
            impl_shared_ops = shared_ops;
            impl_shared_val = file;
            impl_shared_requests = old_impl.impl_shared_requests;
            impl_shared_file = Some (as_file file);
            impl_shared_servers = [];
          } in
          file.file_shared <- Some impl;
          replace_shared old_impl impl;
        end
    end 

let auto_links =
  let re = Str.regexp_case_fold "\\(https?://[a-zA-Z0-9_.!~*'();/?:@&=+$,%-]+\\)" in
  fun s -> Str.global_replace re "\\<a href=\\\"\\1\\\"\\>\\1\\</a\\>" s

let op_file_print file o =

  let buf = o.conn_buf in
  if use_html_mods o then begin
  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Filename", "sr br", "Filename");
    ("", "sr", file.file_name) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Torrent metadata hash", "sr", "Hash");
    ("", "sr", Sha1.to_hexa file.file_id) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Search for other possible Torrent Files", "sr br", "Torrent Srch");
    ("", "sr", Printf.sprintf "\\<a target=\\\"_blank\\\" href=\\\"http://isohunt.com/%s\\\"\\>IsoHunt\\</a\\>"
         (file.file_name)
      )
 ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  let tracker_header_printed = ref false in
  List.iter (fun tracker ->
    let tracker_text =
      match tracker.tracker_status with
        | Disabled s | Disabled_mld s ->
            Printf.sprintf "\\<font color=\\\"red\\\"\\>disabled: %s\\<br\\>\\--error: %s\\</font\\>" tracker.tracker_url s
        | Disabled_failure (i,s) -> 
            Printf.sprintf "\\<font color=\\\"red\\\"\\>disabled: %s\\<br\\>\\--error: %s (try %d)\\</font\\>" tracker.tracker_url s i
        | _ ->
            Printf.sprintf "enabled: %s" tracker.tracker_url

    in
    html_mods_td buf [
      (if not !tracker_header_printed then
        ("Tracker(s)", "sr br", "Tracker(s)")
       else
        ("", "sr br", "")
      );
      (tracker.tracker_url, "sr", tracker_text)];
    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
    tracker_header_printed := true;
  ) file.file_trackers;

  html_mods_td buf [
    ("Torrent Filename", "sr br", "Torrent Fname");
    ("", "sr", file.file_torrent_diskname) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());

  html_mods_td buf [
    ("Comment", "sr br", "Comment");
    ("", "sr", match file.file_comment with
        "" -> "-"
      | s -> auto_links s) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Created by", "sr br", "Created by");
    ("", "sr", match file.file_created_by with
        "" -> "-"
      | s -> auto_links s) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Creation date", "sr br", "Creation date");
    ("", "sr", Date.to_string (Int64.to_float file.file_creation_date) ) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Modified by", "sr br", "Modified by");
    ("", "sr", match file.file_modified_by with
        "" -> "-"
      | s -> auto_links s) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Encoding", "sr br", "Encoding");
    ("", "sr", match file.file_encoding with
        "" -> "-"
      | _ -> file.file_encoding) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Piece size", "sr br", "Piece size");
    ("", "sr", Int64.to_string file.file_piece_size) ];

  let rec print_first_tracker l =
    match l with
      | [] -> ()
      | t :: q ->
          if not (tracker_is_enabled t) then print_first_tracker q
          else begin
            Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
            html_mods_td buf [
              ("Last Tracker Announce", "sr br", "Last Announce");
              ("", "sr", string_of_date t.tracker_last_conn) ];

            if t.tracker_last_conn > 1 then
            begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
              ("Next Tracker Announce (planned)", "sr br", "Next Announce");
              ("", "sr", string_of_date (t.tracker_last_conn + t.tracker_interval)) ];
            end;

            Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
            html_mods_td buf [
              ("Tracker Announce Interval", "sr br", "Announce Interval");
              ("", "sr", Printf.sprintf "%d seconds" t.tracker_interval) ];

            Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
            html_mods_td buf [
              ("Minimum Tracker Announce Interval", "sr br", "Min Announce Interval");
              ("", "sr", Printf.sprintf "%d seconds" t.tracker_min_interval) ];

            (* show only interesting answers*)
            if t.tracker_torrent_downloaded > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Downloaded", "sr br", "Downloaded");
                ("", "sr", Printf.sprintf "%d" t.tracker_torrent_downloaded) ]
            end;
            if t.tracker_torrent_complete > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Complete (seeds)", "sr br", "Complete");
                ("", "sr", Printf.sprintf "%d" t.tracker_torrent_complete) ]
            end;
            if t.tracker_torrent_incomplete > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Incomplete (peers)", "sr br", "Incomplete");
                ("", "sr", Printf.sprintf "%d" t.tracker_torrent_incomplete) ]
            end;
            if t.tracker_torrent_total_clients_count > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Total client count", "sr br", "All clients");
                ("", "sr", Printf.sprintf "%d" t.tracker_torrent_total_clients_count) ]
            end;
            if t.tracker_torrent_last_dl_req > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Latest torrent request", "sr br", "Latest request");
                ("", "sr", Printf.sprintf "%ds" t.tracker_torrent_last_dl_req) ]
            end;
            if String.length t.tracker_id > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Tracker id", "sr br", "Tracker id");
                ("", "sr", t.tracker_id) ]
            end;
            if String.length t.tracker_key > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("Tracker key", "sr br", "Tracker key");
                ("", "sr", t.tracker_key) ]
            end 
          end in
  print_first_tracker file.file_trackers;

  (* This is bad.  Magic info should be automatically filled in when 
     the corresponding chunks complete. (see CommonSwarming)

     This code only fills in the magic info for subfiles when a user 
     manually performs a "vd #".  (interfaces out of sync)
  
     Magic info for shared files with subfiles is missing as well?
  *)
  if !Autoconf.magic_works then begin
  let check_magic file =
    match Magic.M.magic_fileinfo file false with
      None -> None
    | Some s -> Some (intern s)
  in
    let fdn = file_disk_name file in
    let new_file_files = ref [] in
  
    List.iter (fun (f, s, m) -> 
      let subfile = Filename.concat fdn f in
      new_file_files := (f,s, check_magic subfile) :: !new_file_files;
    ) file.file_files;

    file.file_files <- List.rev !new_file_files;
    file_must_update file; (* Send update to guis *)

  end;
  (* -- End bad -- *)

  let cntr = ref 0 in
  List.iter (fun (filename, size, magic) ->
    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
    let fs = Printf.sprintf "File %d" !cntr in
    let magic_string =
      match magic with 
            None -> ""
      | Some m -> Printf.sprintf " / %s" m;
    in
    html_mods_td buf [
      (fs, "sr br", fs);
      ("", "sr", (Printf.sprintf "%s (%Ld bytes)%s" filename size magic_string)) 
    ];
    incr cntr;
  ) file.file_files
  end (* use_html_mods *)
  else begin

  Printf.bprintf buf "Trackers:\n";
  List.iter (fun tracker ->
    match tracker.tracker_status with
    | Disabled s | Disabled_mld s ->
        Printf.bprintf buf "%s, disabled: %s\n" tracker.tracker_url s
    | Disabled_failure (i,s) -> 
        Printf.bprintf buf "%s, disabled (try %d): %s\n" tracker.tracker_url i s
    | _ -> Printf.bprintf buf "%s\n" tracker.tracker_url
  ) file.file_trackers;
  if file.file_torrent_diskname <> "" then
    Printf.bprintf buf "Torrent diskname: %s\n" file.file_torrent_diskname;
  if file.file_comment <> "" then Printf.bprintf buf "Comment: %s\n" file.file_comment;
  if file.file_created_by <> "" then Printf.bprintf buf "Created by %s\n" file.file_created_by;
  let s = Date.to_string (Int64.to_float file.file_creation_date) in
  if s <> "" then Printf.bprintf buf "Creation date: %s\n" s;
  if file.file_modified_by <> "" then Printf.bprintf buf "Modified by %s\n" file.file_modified_by;
  if file.file_encoding <> "" then Printf.bprintf buf "Encoding: %s\n" file.file_encoding;
  if file.file_files <> [] then Printf.bprintf buf "Subfiles: %d\n" (List.length file.file_files);
  let cntr = ref 0 in
  List.iter (fun (filename, size, magic) ->
    incr cntr;
    let magic_string =
      match magic with 
        None -> ""
      | Some m -> Printf.sprintf " / %s" m;
    in
    Printf.bprintf buf "File %d: %s (%Ld bytes)%s\n" !cntr filename size magic_string
  ) file.file_files
  end

let op_file_print_sources file o =
  let buf = o.conn_buf in

(* redefine functions for telnet output *)
  let html_mods_td buf l =
    if use_html_mods o then
      html_mods_td buf l
    else
      (* List *)
      List.iter (fun (t,c,d)  ->
          (* Title Class Value *)
          Printf.bprintf buf "%s "
          d;
      ) l
  in
  let html_mods_table_header buf n c l =
    if use_html_mods o then
      html_mods_table_header buf n c l
    else
      if List.length l > 0 then begin
        Printf.bprintf buf "\n";
        List.iter (fun (w,x,y,z)  ->
          (* Sort Class Title Value *)
          Printf.bprintf buf "%s "
          z;
        ) l;
        Printf.bprintf buf "\n"
    end
  in

  if Hashtbl.length file.file_clients > 0 then begin

      let header_list = [
        ( "1", "srh br ac", "Client number", "Num" ) ;
        ( "0", "srh br", "Client UID", "UID" ) ;
        ( "0", "srh br", "Client software", "Soft" ) ;
        ( "0", "srh", "IP address", "IP address" ) ;
        ( "0", "srh br ar", "Port", "Port" ) ;
        ] @ (if Geoip.active () then [( "0", "srh br ar", "Country Code/Name", "CC" )] else []) @ [
        ( "1", "srh ar", "Total UL bytes to this client for all files", "tUL" ) ;
        ( "1", "srh ar br", "Total DL bytes from this client for all files", "tDL" ) ;
        ( "1", "srh ar", "Session UL bytes to this client for all files", "sUL" ) ;
        ( "1", "srh ar br", "Session DL bytes from this client for all files", "sDL" ) ;
        ( "0", "srh ar", "Interested [T]rue, [F]alse", "I" ) ;
        ( "0", "srh ar", "Choked [T]rue, [F]alse", "C" ) ;
        ( "1", "srh br ar", "Allowed to write", "A" ) ;
        ( "0", "srh ar", "Interesting [T]rue, [F]alse", "I" );
        ( "0", "srh ar", "Already sent interested [T]rue, [F]alse", "A" );
        ( "0", "srh br ar", "Already sent not interested [T]rue, [F]alse", "N" );

        ( "0", "srh ar", "Good [T]rue, [F]alse", "G" );
        ( "0", "srh ar", "Incoming [T]rue, [F]alse", "I" );
        ( "0", "srh br ar", "Registered bitfield [T]rue, [F]alse", "B" );

        ( "0", "srh ar", "Connect Time", "T" );
        ( "0", "srh ar", "Last optimist", "L.Opt" );
        ( "0", "srh br ar", "Num try", "N" );

        ( "0", "srh", "DHT [T]rue, [F]alse", "D" );
        ( "0", "srh", "Cache extensions [T]rue, [F]alse", "C" );
        ( "0", "srh", "Fast extensions [T]rue, [F]alse", "F" );
        ( "0", "srh", "uTorrent extensions [T]rue, [F]alse", "U" );
        ( "0", "srh br", "Azureus messaging protocol [T]rue, [F]alse", "A" );
(*
        ( "0", "srh", "Bitmap (absent|partial|present|verified)", (colored_chunks
        (Array.init (String.length info.G.file_chunks)
        (fun i -> ((int_of_char info.G.file_chunks.[i])-48)))) ) ;
*)
        ( "1", "srh ar", "Number of full chunks", (Printf.sprintf "%d"
          (match file.file_swarmer with
          | None -> 0
          | Some swarmer ->
              let bitmap = 
                CommonSwarming.chunks_verified_bitmap swarmer in
              VB.fold_lefti (fun acc _ s ->
                if s = VB.State_verified then acc + 1 else acc)        0 bitmap)))
      ] in

      html_mods_table_header buf "sourcesTable" "sources al" header_list;

      Hashtbl.iter (fun _ c ->
          let cinfo = client_info (as_client c) in
          if use_html_mods o then
          Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr());

          let btos b = if b then "T" else "F" in
          let cc,cn = Geoip.get_country_code_name cinfo.GuiTypes.client_country_code in
          let td_list = [
            ("", "sr br ar", Printf.sprintf "%d" (client_num c));
            ("", "sr br", (Sha1.to_string c.client_uid));
            ("", "sr br", Printf.sprintf "%s %s" (brand_to_string c.client_brand) c.client_release);
            ("", "sr", (Ip.to_string (fst c.client_host)));
            ("", "sr br ar", Printf.sprintf "%d" (snd c.client_host));
            ] @ (if Geoip.active () then 
              [( cn, "sr br", if use_html_mods o then CommonPictures.flag_html cc else cc)]
                 else []) @ [
            ("", "sr ar", (size_of_int64 c.client_total_uploaded));
            ("", "sr ar br", (size_of_int64 c.client_total_downloaded));
            ("", "sr ar", (size_of_int64 c.client_session_uploaded));
            ("", "sr ar br", (size_of_int64 c.client_session_downloaded));
            ("", "sr", (btos c.client_interested));
            ("", "sr", (btos c.client_choked));
            ("", "sr br ar", (Int64.to_string c.client_allowed_to_write));
(* This is way too slow for 1000's of chunks on a page with 100's of sources
    ("", "sr", (CommonFile.colored_chunks (Array.init (String.length c.client_bitmap)
       (fun i -> (if c.client_bitmap.[i] = '1' then 2 else 0)) )) );
*)
            ("", "sr", (btos c.client_interesting));
            ("", "sr", (btos c.client_alrd_sent_interested));
            ("", "br sr", (btos c.client_alrd_sent_notinterested));

            ("", "sr", (btos c.client_good));
            ("", "sr", (btos c.client_incoming));
            ("", "br sr", (btos c.client_registered_bitfield));

            ("", "sr", Printf.sprintf "%d" c.client_connect_time);
            ("", "ar sr", string_of_date c.client_last_optimist);
            ("", "br sr", Printf.sprintf "%d" c.client_num_try);

            ("", "sr", (btos c.client_dht)); 
            ("", "sr", (btos c.client_cache_extension)); 
            ("", "sr", (btos c.client_fast_extension)); 
            ("", "sr", (btos c.client_utorrent_extension)); 
            ("", "br sr", (btos c.client_azureus_messaging_protocol)); 

            ("", "sr ar", (let fc = ref 0 in
                (match c.client_bitmap with
                    None -> ()
                  | Some bitmap ->
                      Bitv.iter (fun s -> if s then incr fc) bitmap);
                (Printf.sprintf "%d" !fc) ) ) 
          ] in

          html_mods_td buf td_list;
          if use_html_mods o then Printf.bprintf buf "\\</tr\\>"
          else Printf.bprintf buf "\n";

      ) file.file_clients;

      if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>\\<br\\>"
      else Printf.bprintf buf "\n";

    end

let op_file_check file =
  lprintf_file_nl (as_file file) "Checking chunks of %s" file.file_name;
  match file.file_swarmer with
    None ->
      lprintf_file_nl (as_file file) "verify_chunks: no swarmer to verify chunks"
  | Some swarmer ->
      CommonSwarming.verify_all_chunks_immediately swarmer

let remove_all_clients file =
  Hashtbl.clear file.file_clients; 
  file.file_clients_num <- 0

let op_file_cancel file =
  CommonSwarming.remove_swarmer file.file_swarmer;
  file.file_swarmer <- None;
  BTClients.file_stop file;
  remove_file file;
  BTClients.disconnect_clients file;
  remove_all_clients file;
  if Sys.file_exists file.file_torrent_diskname then Sys.remove file.file_torrent_diskname

let op_ft_cancel ft =
  Hashtbl.remove ft_by_num ft.ft_id

let op_ft_commit ft newname =
  Hashtbl.remove ft_by_num ft.ft_id

let op_file_info file =

  let module P = GuiTypes in

  let last_seen = match file.file_swarmer with
      None -> [| last_time () |]
    | Some swarmer -> CommonSwarming.compute_last_seen swarmer in

    { (impl_file_info file.file_file) with

    P.file_name = file.file_name;
    P.file_network = network.network_num;
    P.file_chunks = (match file.file_swarmer with
    | None -> None 
    | Some swarmer -> Some (CommonSwarming.chunks_verified_bitmap swarmer));
    P.file_chunk_size = (match file.file_swarmer with
    | None -> None 
    | Some t -> Some (List.map (fun t -> t.CommonSwarming.t_chunk_size) t.CommonSwarming.t_s.CommonSwarming.s_networks));
    P.file_availability =
    [network.network_num,(match file.file_swarmer with
          None -> "" | Some swarmer ->
            CommonSwarming.chunks_availability swarmer)];

    P.file_chunks_age = last_seen;
    P.file_uids = [Uid.create (BTUrl file.file_id)];
    P.file_sub_files = file.file_files;
    P.file_active_sources = List.length (op_file_active_sources file);
    P.file_all_sources = (Hashtbl.length file.file_clients);
    P.file_comment = file.file_comment;
  }

let op_ft_info ft =

  let module P = GuiTypes in

  {
    P.file_fields = P.Fields_file_info.all;

    P.file_comment = file_comment (as_ft ft);
    P.file_name = ft.ft_filename;
    P.file_num = ft_num ft;
    P.file_network = network.network_num;
    P.file_names = [ft.ft_filename];
    P.file_md4 = Md4.null;
    P.file_size = ft_size ft;
    P.file_downloaded = zero;
    P.file_all_sources = 0;
    P.file_active_sources = 0;
    P.file_state = ft_state ft;
    P.file_sources = None;
    P.file_download_rate = 0.;
    P.file_chunks = None;
    P.file_chunk_size = None;
    P.file_availability =  [network.network_num, ""];
    P.file_format = FormatNotComputed 0;
    P.file_chunks_age = [| last_time () |];
    P.file_age = 0;
    P.file_last_seen = BasicSocket.last_time ();
    P.file_priority = 0;
    P.file_uids = [];
    P.file_sub_files = [];
    P.file_magic = None;
    P.file_comments = [];
    P.file_user = "";
    P.file_group = "";
    P.file_release = file_release (as_ft ft);
  }



let load_torrent_string s user group =
  if !verbose then lprintf_nl "load_torrent_string";
  let file_id, torrent = BTTorrent.decode_torrent s in

  (* Save the torrent, because we later want to put
     it in the seeded directory. *)
  let torrent_is_usable = ref false in
  let can_handle_tracker url =
    String2.check_prefix url "http://" in
  List.iter (fun url -> if can_handle_tracker url then torrent_is_usable := true)
    (if torrent.torrent_announce_list <> [] then torrent.torrent_announce_list else [torrent.torrent_announce]);
  if not !torrent_is_usable then raise (Torrent_can_not_be_used torrent.torrent_name);

  let torrent_diskname =
    let fs = Unix32.filesystem downloads_directory in
    let namemax =
      match Unix32.fnamelen downloads_directory with
      | None -> 0
      | Some v -> v
    in
    Filename.concat downloads_directory
    (Filename2.filesystem_compliant torrent.torrent_name fs namemax) ^ ".torrent"
    in
  if Sys.file_exists torrent_diskname then
    begin
      if !verbose then lprintf_nl "load_torrent_string: %s already exists, ignoring" torrent_diskname;
      raise (Torrent_already_exists torrent.torrent_name)
    end;
  File.from_string torrent_diskname s;

  if !verbose then
    lprintf_nl "Starting torrent download with diskname: %s"
        torrent_diskname;
  let file = new_download file_id torrent torrent_diskname user group in
  BTClients.talk_to_tracker file true;
  CommonInteractive.start_download (file_find (file_num file));
  file

let load_torrent_file filename user group =
  if !verbose then
    lprintf_nl "load_torrent_file %s" filename;
  let s = File.to_string filename in
  (* Delete the torrent if it is in the downloads dir. because it gets saved
     again under the torrent name and we don't want to clutter up this dir. .*)
  if Sys.file_exists filename
      && (Filename.dirname filename) = downloads_directory then
    Sys.remove filename;
  ignore (load_torrent_string s user group)

(*
let parse_tracker_reply file t filename =
(*This is the function which will be called by the http client
for parsing the response*)
(* Interested only in interval*)
  if !verbose_msg_servers then lprintf_file_nl (as_file file) "Filename %s" filename;
    let tracker_reply =
      try
        File.to_string filename
      with e -> lprintf_file_nl (as_file file) "Empty reply from tracker"; ""
    in
    let v =
       match tracker_reply with
       | "" ->
        if !verbose_connect then
          lprintf_file_nl (as_file file) "Empty reply from tracker";
        Bencode.decode ""
       | _ -> Bencode.decode tracker_reply
    in
  if !verbose_msg_servers then lprintf_file_nl (as_file file) "Received: %s" (Bencode.print v);
  t.tracker_interval <- 600;
  match v with
    Dictionary list ->
      List.iter (fun (key,value) ->
          match (key, value) with
            String "interval", Int n ->
              t.tracker_interval <- Int64.to_int n;
              if !verbose_msg_servers then lprintf_file_nl (as_file file) ".. interval %d .." t.tracker_interval
          | String "failure reason", String failure ->
                lprintf_file_nl (as_file file) "Failure from Tracker in file: %s Reason: %s"  file.file_name failure
          (*TODO: merge with f from get_sources_from_tracker and parse the rest of the answer, too.
            also connect to the sources we receive or instruct tracker to send none, perhaps based
            on an config option. firewalled people could activate the option and then seed torrents, too.*)
          | _ -> ()
      ) list;
  | _ -> assert false
*)

let try_share_file torrent_diskname =
  if !verbose_share then lprintf_nl "try_share_file: %s" torrent_diskname;
  let s = File.to_string torrent_diskname in
  let file_id, torrent = BTTorrent.decode_torrent s in

  try
    let filename =
      let rec iter list =
        match list with
          [] -> raise Not_found
        | sh :: tail ->
            let s = sharing_strategy sh.shdir_strategy in
            if match torrent.torrent_files with
                [] -> not s.sharing_directories
              | _ ->  s.sharing_directories then
              let filename =
                Filename.concat sh.shdir_dirname torrent.torrent_name
              in
              if !verbose_share then lprintf_nl "Checking for %s" filename;
              if Sys.file_exists filename then filename else
                iter tail
            else
              iter tail
      in
      iter (shared_directories_including_user_commit ())
    in

    let user = CommonUserDb.admin_user () in
    let file = new_file file_id torrent torrent_diskname
        filename FileShared user user.user_default_group in

    if !verbose_share then 
      lprintf_file_nl (as_file file) "Sharing file %s" filename;
    BTClients.talk_to_tracker file false;
    `Ok torrent_diskname
  with
  | Not_found ->
      (* if the torrent is still there while the file is gone, remove the torrent *)
      if !verbose_share then lprintf_nl "Removing torrent for %s" s;
      let new_torrent_diskname =
        Filename.concat old_directory
          (Filename.basename torrent_diskname)
      in
      begin try
          Unix2.rename torrent_diskname new_torrent_diskname;
          `Ok new_torrent_diskname
      with _ ->
          let msg = Printf.sprintf "Failed to rename %S to %S" torrent_diskname new_torrent_diskname in
          lprintf_nl "%s" msg;
          `Err msg
      end
  | e ->
      let msg = Printf.sprintf "Cannot share %S - exn %s" torrent_diskname (Printexc2.to_string e) in
      lprintf_nl "%s" msg;
      `Err msg

(* Call one minute after start, and then every 20 minutes. Should
  automatically contact the tracker. *)
let share_files _ =
  if !verbose_share then lprintf_nl "share_files";
  List.iter (fun file ->
    ignore (try_share_file (Filename.concat seeded_directory file))
  ) (Unix2.list_directory seeded_directory);
  let shared_files_copy = !current_files in
 (* if the torrent is gone while the file is still shared, remove the share *)
  List.iter (fun file ->
      (* if !verbose_share then lprintf_nl "Checking torrent share for %s" file.file_torrent_diskname; *)
      if not (Sys.file_exists file.file_torrent_diskname) &&
        file_state file = FileShared then
        begin
          if !verbose_share then lprintf_nl "Removing torrent share for %s" file.file_torrent_diskname;
          BTClients.file_stop file;
          remove_file file;
          BTClients.disconnect_clients file;
          remove_all_clients file;
        end
  ) shared_files_copy

(** talk_to_tracker maintains timers and will connect to trackers only when allowed by rules *)
let announce_shared_files () =
  List.iter (fun file -> if file_state file = FileShared then BTClients.talk_to_tracker file false) !current_files

let scan_new_torrents_directory () =
  let filenames = Unix2.list_directory new_torrents_directory in
  List.iter (fun file ->
    let file = Filename.concat new_torrents_directory file in
    let file_basename = Filename.basename file in
    if not (Unix2.is_directory file) then
    try
      let file_owner = fst (Unix32.owner file) in
      let user =
        try
          CommonUserDb.user2_user_find file_owner
        with Not_found -> CommonUserDb.admin_user ()
      in
      load_torrent_file file user user.user_default_group;
      (try Sys.remove file with _ -> ())
    with 
      Torrent_can_not_be_used _ ->
        Unix2.rename file (Filename.concat old_directory file_basename);
        lprintf_nl "Torrent %s does not have valid tracker URLs, moved to torrents/old ..." file_basename
    | e ->
        Unix2.rename file (Filename.concat old_directory file_basename);
        lprintf_nl "Error %s in scan_new_torrents_directory for %s, moved to torrents/old ..."
          (Printexc2.to_string e) file_basename
  ) filenames

let retry_all_ft () =
  Hashtbl.iter (fun _ ft ->
      try ft.ft_retry ft with e ->
          lprintf_nl "ft_retry: exception %s" (Printexc2.to_string e)
  ) ft_by_num

let load_torrent_from_web r user group ft =
  let module H = Http_client in
  H.wget r (fun filename ->
      if ft_state ft = FileDownloading then begin
          load_torrent_file filename user group;
          file_cancel (as_ft ft) (CommonUserDb.admin_user ())
        end)

let valid_torrent_extension url =
  let ext = String.lowercase (Filename2.last_extension url) in
  ext = ".torrent" || ext = ".tor"

let get_regexp_string text r =
  ignore (Str.search_forward r text 0);
  let a = Str.group_beginning 1 in
  let b = Str.group_end 1 in
  String.sub text a (b - a)

let op_network_parse_url url user group =
  let location_regexp = "Location: \\(.*\\)" in
  try
    let real_url = get_regexp_string url (Str.regexp location_regexp) in
    if (valid_torrent_extension real_url)
       || (String2.contains url "Content-Type: application/x-bittorrent")
      then (
        let u = Url.of_string real_url in
        let module H = Http_client in
        let r = {
            H.basic_request with
            H.req_url = u;
            H.req_proxy = !CommonOptions.http_proxy;
            H.req_user_agent = get_user_agent ();
            H.req_referer = (
              let (rule_search,rule_value) =
                try (List.find(fun (rule_search,rule_value) ->
                        Str.string_match (Str.regexp rule_search) real_url 0
                    ) !!referers )
                with Not_found -> ("",real_url) in
              Some (Url.of_string rule_value) );
            H.req_headers = (try
              let cookies = List.assoc u.Url.server !!cookies in
              [ ( "Cookie", List.fold_left (fun res (key, value) ->
                      if res = "" then
                        key ^ "=" ^ value
                      else
                        res ^ "; " ^ key ^ "=" ^ value
                  ) "" cookies
                ) ]
            with Not_found -> []);
          H.req_max_retry = 10;
          } in

        let file_diskname = Filename.basename u.Url.short_file in
        let ft = new_ft file_diskname user in
        ft.ft_retry <- (load_torrent_from_web r user group);
        load_torrent_from_web r user group ft;
        "started download", true
      )
    else
      "", false
  with
    | Not_found ->
      if (valid_torrent_extension url) then
        try
          if !verbose then lprintf_nl "Not_found and trying to load %s" url;
          try
            load_torrent_file url user group;
            "", true
          with
            Torrent_already_exists _ -> "A torrent with this name is already in the download queue", false
          | Torrent_can_not_be_used _ -> "This torrent does not have valid tracker URLs", false
        with e ->
          lprintf_nl "Exception %s while 2nd loading" (Printexc2.to_string e);
          let s = Printf.sprintf "Can not load load torrent file: %s"
            (Printexc2.to_string e) in
          s, false
      else
        begin
          if !verbose then lprintf_nl "Not_found and url has non valid torrent extension: %s" url;
          "Not_found and url has non valid torrent extension", false
        end
    | e ->
       lprintf_nl "Exception %s while loading" (Printexc2.to_string e);
       let s = Printf.sprintf "Can not load load torrent file: %s"
         (Printexc2.to_string e) in
       s, false

let op_client_info c =
  check_client_country_code c;
  let module P = GuiTypes in
  let (ip,port) = c.client_host in
  { (impl_client_info c.client_client) with

    P.client_network = network.network_num;
    P.client_kind = Known_location (ip,port);
    P.client_country_code = c.client_country_code;
    P.client_state = client_state (as_client c);
    P.client_type = client_type c;
    P.client_name = (Printf.sprintf "%s:%d" (Ip.to_string ip) port);
    P.client_software = (brand_to_string c.client_brand);
    P.client_release = c.client_release;
    P.client_total_downloaded = c.client_total_downloaded;
    P.client_total_uploaded = c.client_total_uploaded;
    P.client_session_downloaded = c.client_session_downloaded;
    P.client_session_uploaded = c.client_session_uploaded;
    P.client_upload = Some (c.client_file.file_name);
    P.client_connect_time = c.client_connect_time;

  }

let op_client_connect c =
  BTClients.connect_client c

let op_client_disconnect c=
  BTClients.disconnect_client c Closed_by_user

let op_client_bprint c buf =
  let cc = as_client c in
  let cinfo = client_info cc in
  Printf.bprintf buf "%s (%s)\n"
    cinfo.GuiTypes.client_name
    (Sha1.to_string c.client_uid)

let op_client_dprint c o file =
  let info = file_info file in
  let buf = o.conn_buf in
  let cc = as_client c in
  client_print cc o;
  Printf.bprintf buf (_b "\n%18sDown  : %-10s                  Uploaded: %-10s  Ratio: %s%1.1f (%s)\n") ""
    (Int64.to_string c.client_total_downloaded)
  (Int64.to_string c.client_total_uploaded)
  (if c.client_total_downloaded > c.client_total_uploaded then "-" else "+")
  (if c.client_total_uploaded > Int64.zero then 
     Int64.to_float (c.client_total_downloaded // c.client_total_uploaded) 
   else 1.)
  ("BT");
  (Printf.bprintf buf (_b "%18sFile  : %s\n") "" info.GuiTypes.file_name)

let op_client_dprint_html c o file str =
  let info = file_info file in
  let buf = o.conn_buf in
  let ac = as_client c in
  let cinfo = client_info ac in
  Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\"
  onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>" str;

  let show_emulemods_column = ref false in
  if Autoconf.donkey = "yes" then begin
      if !!emule_mods_count then
        show_emulemods_column := true
    end;

    let cc,cn = Geoip.get_country_code_name cinfo.GuiTypes.client_country_code in

    html_mods_td buf ([
        ("", "srb ar", Printf.sprintf "%d" (client_num c));
        ((string_of_connection_state (client_state ac)), "sr",
          (short_string_of_connection_state (client_state ac)));
        ((Sha1.to_string c.client_uid), "sr", cinfo.GuiTypes.client_name);
        ("", "sr", (brand_to_string c.client_brand)); (* cinfo.GuiTypes.client_software *)
        ("", "sr", c.client_release); 
      ] @
        (if !show_emulemods_column then [("", "sr", "")] else [])
      @ [
        ("", "sr", "F");
        ("", "sr ar", Printf.sprintf "%d"
            (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
        ("", "sr", "D");
        ("", "sr", "N");
        ("", "sr", (Ip.to_string (fst c.client_host)));
        ] @ (if Geoip.active () then [(cn, "sr", CommonPictures.flag_html cc)] else []) @ [
        ("", "sr ar", (size_of_int64 c.client_total_uploaded));
        ("", "sr ar", (size_of_int64 c.client_total_downloaded));
        ("", "sr ar", (size_of_int64 c.client_session_uploaded));
        ("", "sr ar", (size_of_int64 c.client_session_downloaded));
        ("", "sr", info.GuiTypes.file_name); ]);
    true

let op_network_connected _ = true

let compute_torrent filename announce comment = 
  let announce = if announce = "" then BTTracker.get_default_tracker () else announce in
  if !verbose then lprintf_nl "compute_torrent: [%s] [%s] [%s]"
   filename announce comment;
  let basename = Printf.sprintf "%s.torrent" (Filename.basename filename) in
  let torrent = Filename.concat seeded_directory basename in
  let is_private = 0 in
  let file_id = BTTorrent.generate_torrent announce torrent comment (Int64.of_int is_private) filename in
  match try_share_file torrent with 
  | `Err msg -> failwith msg
  | `Ok torrent_path ->
    Filename.concat (Sys.getcwd ()) torrent_path,
    try `Ok (BTTracker.track_torrent basename file_id) with exn -> `Err (Printexc2.to_string exn)

(* let text fmt = Printf.ksprintf (fun s -> `Text s) fmt *)
(* 
  OCaml 3.08.3 compatibility (ksprintf not available)
  http://mldonkey.sourceforge.net/phpBB2/viewtopic.php?p=30453 
*)
let text s = `Text s
let link name url = `Link (name,url)

let output buf typ elements =
  let f = match typ with
  | HTML | XHTML | XML ->
    begin function 
    | `Text s -> Xml.buffer_escape buf s
    | `Link (name,url) -> 
        Printf.bprintf buf "<a href=\"%s\">%s</a>" 
          (Xml.escape url) (Xml.escape (match name with "" -> url | s -> s))
    | `Break -> Buffer.add_string buf "<br/>"
    end
  | TEXT | ANSI ->
    begin function
    | `Text s -> Buffer.add_string buf s
    | `Link ("",url) -> Printf.bprintf buf "%s" url
    | `Link (name,url) -> Printf.bprintf buf "%s <%s>" name url
    | `Break -> Buffer.add_string buf "\n"
    end
  in
  List.iter f elements

(* dirty hack *)
let output o l =
  match o.conn_output with
  | ANSI | TEXT -> output o.conn_buf o.conn_output l
  | HTML | XHTML | XML ->
    let buf = Buffer.create 1024 in
    output buf o.conn_output l;
    let s = Buffer.contents buf in
    for i = 0 to String.length s - 1 do
      begin match s.[i] with
      | '<' | '>' | '\\' | '"' | '&' -> Buffer.add_char o.conn_buf '\\'
      | _ -> () end;
      Buffer.add_char o.conn_buf s.[i]
    done

let commands =

    [
    "compute_torrent", "Network/Bittorrent", Arg_multiple (fun args o ->
      output o
      begin try
        let filename = ref "" in
        let comment = ref "" in
        (match args with
          fname :: [comm] -> filename := fname; comment := comm
        | [fname] -> filename := fname
        | _ -> raise Not_found);

        let (path,url) = compute_torrent !filename "" !comment in
        [
          text (Printf.sprintf "Torrent file generated : %s" path);
          `Break;
          (match url with
          | `Ok url -> link "Download" url
          | `Err s -> text (Printf.sprintf "Not tracked : %s" s));
          `Break
        ]
      with 
      | Not_found -> [text "Not enough parameters"; `Break]
      | exn -> [text (Printf.sprintf "Error: %s" (Printexc2.to_string exn)); `Break]
      end;
      ""
    ), _s "<filename> [<comment>] :\tgenerate the corresponding <filename> .torrent file with <comment>.\n\t\t\t\t\t\tThe file is automatically tracked if tracker is enabled and seeded if located in incoming/";

    "torrents", "Network/Bittorrent", Arg_none (fun o ->
      output o 
        begin try
          BTTracker.check_tracker ();
          let files_tracked = Unix2.list_directory tracked_directory in
          let files_downloading = Unix2.list_directory downloads_directory in
          let files_seeded = Unix2.list_directory seeded_directory in
          let files_old = Unix2.list_directory old_directory in
          let all_torrents_files = files_tracked @ files_downloading @ files_seeded @ files_old in

          let l = List.map (fun file -> [link file (BTTracker.tracker_url file); `Break]) all_torrents_files in

          (`Text (_s ".torrent files available:")) :: `Break :: List.flatten l
        with
          exn ->
          [`Text (Printexc2.to_string exn); `Break]
        end;
        _s ""
    ), _s ":\t\t\t\tprint all .torrent files on this server";

    "print_torrent", "Network/Bittorrent", Arg_one (fun arg o ->
      if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
      let file =
        try
          Some (as_file_impl (file_find (int_of_string arg)))
        with _ -> None
      in
      match file with
      | None -> Printf.sprintf "file %s not found" arg
      | Some file ->
        (
        if use_html_mods o then begin
          html_mods_cntr_init ();
          html_mods_table_header o.conn_buf "sourcesInfo" "sourcesInfo" [
            ( "0", "srh br", "File Info", "Info" ) ;
            ( "0", "srh", "Value", "Value" ) ]
        end;
        op_file_print file.impl_file_val o;
        if use_html_mods o then begin
          Printf.bprintf o.conn_buf "\\</tr\\>\\</table\\>\\</div\\>";
          Printf.bprintf o.conn_buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>\\<br\\>"
        end
        );
      ""
      end else
      begin print_command_result o "You are not allowed to use print_torrent";
      "" end
    ), _s "<num> :\t\t\tshow internal data of .torrent file";

    "seeded_torrents", "Network/Bittorrent", Arg_none (fun o ->
      if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
      List.iter (fun file ->
          if file_state file = FileShared then
              Printf.bprintf o.conn_buf "%s [U %Ld u/d %Ld/%Ld]\n" 
                file.file_name file.file_uploaded file.file_session_uploaded file.file_session_downloaded
      ) !current_files;
      _s "done"
      end else
      begin print_command_result o "You are not allowed to use seeded_torrents";
      "" end
    ), _s ":\t\t\tprint all seeded .torrent files on this server (output: name, total upload, session upload, session download)";

    "reshare_torrents", "Network/Bittorrent", Arg_none (fun o ->
      share_files ();
      _s "done"
    ), _s ":\t\t\trecheck torrents/* directories for changes";

    "rm_old_torrents", "Network/Bittorrent", Arg_none (fun o ->
      let files_outdated = Unix2.list_directory old_directory in
      let buf = o.conn_buf in
      if o.conn_output = HTML then begin
          (* TODO: really htmlize it *)
          Printf.bprintf buf "Removing old torrents...";
          List.iter (fun file ->
              Printf.bprintf buf "%s "
                file;
          ) files_outdated
        end
      else begin
          Printf.bprintf buf "Removing old torrents...\n";
          List.iter (fun file ->
              Printf.bprintf buf "%s\n"
               file
          ) files_outdated;
        end;
      List.iter (fun file ->
            Sys.remove (Filename.concat old_directory file)
      ) files_outdated;
      _s ""
    ), _s ":\t\t\tremove all old .torrent files";

    "startbt", "Network/Bittorrent", Arg_one (fun url o ->
      let buf = o.conn_buf in
      if Sys.file_exists url then
        begin
          load_torrent_file url o.conn_user.ui_user o.conn_user.ui_user.user_default_group;
          Printf.bprintf buf "loaded file %s\n" url
        end
      else
        begin
          let url = "Location: " ^ url ^ "\nContent-Type: application/x-bittorrent" in
          let result = fst (op_network_parse_url url o.conn_user.ui_user o.conn_user.ui_user.user_default_group) in
          Printf.bprintf buf "%s\n" result
        end;
      _s ""
     ),  "<url|file> :\t\t\tstart BT download";

    "stop_all_bt", "Network/Bittorrent", Arg_none (fun o ->
      List.iter (fun file -> BTClients.file_stop file ) !current_files;
      let buf = o.conn_buf in
      if o.conn_output = HTML then
          (* TODO: really htmlize it *)
          Printf.bprintf buf "started sending stops..."
      else
        Printf.bprintf buf "started sending stops...\n";
       _s ""
    ), _s ":\t\t\t\tstops all bittorrent downloads, use this if you want to make sure that the stop signal actually\n\t\t\t\t\tgets to the tracker when shutting mlnet down, but you have to wait till the stops get to the\n\t\t\t\t\ttracker and not wait too long, so mldonkey reconnects to the tracker :)";

    "tracker", "Network/Bittorrent", Arg_multiple (fun args o ->
        try
          let num = ref "" in
          let urls = ref [] in
          (match args with
          | nums :: [] -> raise Not_found
          | nums :: rest -> num := nums; urls := rest
          | _ -> raise Not_found);

          let num = int_of_string !num in
          Hashtbl.iter (fun _ file ->
              if file_num file = num then begin
                  if !verbose then
                    lprintf_file_nl (as_file file) "adding trackers for file %i" num;
                  set_trackers file !urls;
                  raise Exit
                end
          ) files_by_uid;
         let buf = o.conn_buf in
          if o.conn_output = HTML then
            html_mods_table_one_row buf "serversTable" "servers" [
              ("", "srh", "file not found"); ]
          else
            Printf.bprintf buf "file not found";
          _s ""
        with
        | Exit -> 
            let buf = o.conn_buf in
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "tracker added"); ]
            else
              Printf.bprintf buf "tracker added";
            _s ""
        | _ ->
            if !verbose then
              lprintf_nl  "Not enough or wrong parameters.";
            let buf = o.conn_buf in
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "Not enough or wrong parameters."); ]
            else
              Printf.bprintf buf "Not enough or wrong parameters.";
            _s ""        
    ), "<num> <url> <url>... :\t\tadd URLs as trackers for num";

(* TODO : add some code from make_torrent
    "print_torrent", Arg_one (fun filename o ->

        ".torrent file printed"
    ), "<filename.torrent> :\t\tprint the content of filename"
 *)

  ]

open LittleEndian
open GuiDecoding

let op_gui_message s user =
  match get_int16 s 0 with
    0 ->
      let text = String.sub s 2 (String.length s - 2) in
      if !verbose then lprintf_nl "received torrent from gui...";
      (try
        let file = load_torrent_string text user user.user_default_group in
        raise (Torrent_started file.file_name)
      with e -> (match e with
        | Torrent_can_not_be_used s -> lprintf_nl "Loading torrent from GUI: torrent %s can not be used" s
        | Torrent_already_exists s -> lprintf_nl "Loading torrent from GUI: torrent %s is already in download queue" s
        | _ -> ());
        raise e)
  | 1 -> (* 34+ *)
      let n = get_int s 2 in
      let a, pos = get_string s 6 in
      let c, pos = get_string s pos in
      let sf = CommonShared.shared_find n in
      let f = shared_fullname sf in
      ignore (compute_torrent f a c)
  | opcode -> failwith (Printf.sprintf "[BT] Unknown message opcode %d" opcode)

let _ =

  ft_ops.op_file_cancel <- op_ft_cancel;
  ft_ops.op_file_commit <- op_ft_commit;
  ft_ops.op_file_info <- op_ft_info;
  ft_ops.op_file_active_sources <- (fun _ -> []);
  ft_ops.op_file_all_sources <- (fun _ -> []);

  file_ops.op_file_all_sources <- op_file_all_sources;
  file_ops.op_file_files <- op_file_files;
  file_ops.op_file_active_sources <- op_file_active_sources;
  file_ops.op_file_debug <- op_file_debug;
  file_ops.op_file_commit <- op_file_commit;
  file_ops.op_file_print <- op_file_print;
  file_ops.op_file_print_sources <- op_file_print_sources;
  file_ops.op_file_check <- op_file_check;
  file_ops.op_file_cancel <- op_file_cancel;
  file_ops.op_file_info <- op_file_info;
  file_ops.op_file_save_as <- (fun file name -> ());
  file_ops.op_file_shared <- (fun file ->
      match file.file_shared with
        None -> None
      | Some sh -> Some (as_shared sh)
  );
  file_ops.op_file_download_order <- (fun file strategy ->
      match file.file_swarmer with
      | None -> None
      | Some s ->
          (match strategy with
          (* return current strategy *)
          | None -> Some (CommonSwarming.get_strategy s)
          | Some strategy -> CommonSwarming.set_strategy s strategy;
                      Some (CommonSwarming.get_strategy s))
  );

  network.op_network_gui_message <- op_gui_message;
  network.op_network_connected <- op_network_connected;
  network.op_network_parse_url <- op_network_parse_url;
  network.op_network_share <- (fun fullname codedname size -> ());
  network.op_network_close_search <- (fun s -> ());
  network.op_network_forget_search <- (fun s -> ());
  network.op_network_connect_servers <- (fun s -> ());
  network.op_network_search <- (fun ss buf -> ());
  network.op_network_download <- (fun r user group -> dummy_file);
  network.op_network_recover_temp <- (fun s -> ());
  let clean_exit_started = ref false in
  network.op_network_clean_exit <- (fun s ->
    if not !clean_exit_started then
      begin
        List.iter (fun file -> BTClients.file_stop file) !current_files;
        clean_exit_started := true;
      end;
    List.for_all (fun file -> not file.file_tracker_connected) !current_files;
  );
  network.op_network_reset <- (fun _ ->
    List.iter (fun file -> BTClients.file_stop file) !current_files);
  network.op_network_ports <- (fun _ ->
    [
    !!client_port, "client_port TCP";
    !!BTTracker.tracker_port, "tracker_port TCP";
    ]);
  network.op_network_porttest_result <- (fun _ -> !porttest_result);
  network.op_network_porttest_start <- (fun _ -> 
      azureus_porttest_random := (Random.int 100000);
      let tests = [
        Printf.sprintf "http://www.utorrent.com/testport?port=%d" !!client_port, interpret_utorrent_porttest;
        Printf.sprintf "http://azureus.aelitis.com/natcheck.php?port=%d&check=azureus_rand_%d"
          !!client_port !azureus_porttest_random, interpret_azureus_porttest;
      ] in
      perform_porttests tests
  );
  network.op_network_check_upload_slots <- (fun _ -> check_bt_uploaders ());
  client_ops.op_client_info <- op_client_info;
  client_ops.op_client_connect <- op_client_connect;
  client_ops.op_client_disconnect <- op_client_disconnect;
  client_ops.op_client_bprint <- op_client_bprint;
  client_ops.op_client_dprint <- op_client_dprint;
  client_ops.op_client_dprint_html <- op_client_dprint_html;
  client_ops.op_client_browse <- (fun _ _ -> ());
  client_ops.op_client_files <- (fun _ -> []);
  client_ops.op_client_clear_files <- (fun _ -> ());

  CommonNetwork.register_commands commands;

  shared_ops.op_shared_state <- (fun file o ->
    if o.conn_output = HTML then
      Printf.sprintf "\\<a href=\\\"submit?q=print_torrent+%d\\\"\\>Details\\</a\\>"
        (file_num file)
    else Printf.sprintf "Shared using %s" file.file_torrent_diskname
  );
  shared_ops.op_shared_unshare <- (fun file ->
      (if !verbose_share then lprintf_file_nl (as_file file) "unshare file");
      BTGlobals.unshare_file file);
  shared_ops.op_shared_info <- (fun file ->
   let module T = GuiTypes in
     match file.file_shared with
        None -> assert false
      | Some impl ->
          { (impl_shared_info impl) with
            T.shared_network = network.network_num;
            T.shared_filename = file_best_name (as_file file);
            T.shared_uids = [Uid.create (Sha1 file.file_id)];
            T.shared_sub_files = file.file_files;
            }
  )
