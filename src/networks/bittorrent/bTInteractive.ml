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

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[BT] "
      (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[BT] "
    (log_time ()); lprintf

let op_file_all_sources file =
(*      lprintf "file_sources\n"; *)
  let list = ref [] in
  Hashtbl.iter (fun _ c ->
      list := (as_client c) :: !list
  ) file.file_clients;
  !list

let op_file_files file impl =
  match file.file_swarmer with
    None -> [CommonFile.as_file impl]
  | Some swarmer ->
      Int64Swarmer.subfiles swarmer

let op_file_debug file =
  let buf = Buffer.create 100 in
(*      Int64Swarmer.debug_print buf file.file_swarmer; *)
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
  Int64Swarmer.remove_swarmer file.file_swarmer;
  file.file_swarmer <- None;
  if file_state file <> FileShared then
    begin
      if not (List.mem (file.file_name, file_size file) !!old_files) then
        old_files =:= (file.file_name, file_size file) :: !!old_files;
      set_file_state file FileShared;

      if Unix32.destroyed (file_fd file) then
        if !verbose then lprintf_nl () "op_file_commit: FD is destroyed... repairing";

(* During the commit operation, for security, the file_fd is destroyed. So
  we create it again to be able to share this file again. *)
      set_file_fd (as_file file) (create_temp_file new_name file.file_files);

      if Unix32.destroyed (file_fd file) then
        lprintf_nl () "op_file_commit: FD is destroyed... could not repair!";

      let new_torrent_diskname =
        Filename.concat seeded_directory
          (Filename.basename file.file_torrent_diskname)
      in
      (try
          Unix2.rename file.file_torrent_diskname new_torrent_diskname;
        with _ ->
          (lprintf_nl () "op_file_commit: failed to rename %s to %s"
              file.file_torrent_diskname new_torrent_diskname));
      file.file_torrent_diskname <- new_torrent_diskname;

    end 

let op_file_print_html file buf =

  html_mods_cntr_init ();

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Filename", "sr br", "Filename");
    ("", "sr", file.file_name) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Search for other possible Torrent Files", "sr br", "Torrent Srch");
    ("", "sr", Printf.sprintf "\\<a target=\\\"_blank\\\" href=\\\"http://isohunt.com/%s\\\"\\>IsoHunt\\</a\\>"
         (file.file_name)
      )
 ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Tracker(s)", "sr br", "Tracker(s)");
    ("", "sr",
      (let tracker_string = ref "" in
       List.iter (fun tracker ->
         tracker_string := (if tracker.tracker_enabled then "" else "*") ^ 
	   !tracker_string ^ (shorten tracker.tracker_url !!max_name_len) ^ " "
      ) file.file_trackers;
      Printf.sprintf "%s" !tracker_string)) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());

  html_mods_td buf [
    ("Torrent Filename", "sr br", "Torrent Fname");
    ("", "sr", file.file_torrent_diskname) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());

  html_mods_td buf [
    ("Comment", "sr br", "Comment");
    ("", "sr", match file.file_comment with
        "" -> "-"
      | _ -> file.file_comment) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Created by", "sr br", "Created by");
    ("", "sr", match file.file_created_by with
        "" -> "-"
      | _ -> file.file_created_by) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Creation date", "sr br", "Creation date");
    ("", "sr", Date.to_string (Int64.to_float file.file_creation_date) ) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Modified by", "sr br", "Modified by");
    ("", "sr", match file.file_modified_by with
        "" -> "-"
      | _ -> file.file_modified_by) ];

  Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
  html_mods_td buf [
    ("Encoding", "sr br", "Encoding");
    ("", "sr", match file.file_encoding with
        "" -> "-"
      | _ -> file.file_encoding) ];

  let rec print_first_tracker l =
    match l with
      | [] -> ()
      | t :: q ->
	  if not t.tracker_enabled then print_first_tracker q
	  else begin
	    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
	    html_mods_td buf [
              ("Last Connect", "sr br", "Last Connect");
              ("", "sr", string_of_date t.tracker_last_conn) ];

	    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
	    html_mods_td buf [
              ("Connect Interval", "sr br", "Con Interval");
              ("", "sr", Printf.sprintf "%d" t.tracker_interval) ];

	    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
	    html_mods_td buf [
              ("Connect Min Interval", "sr br", "Con Min Interval");
              ("", "sr", Printf.sprintf "%d" t.tracker_min_interval) ];
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
		("", "sr", Printf.sprintf "%s" t.tracker_id) ]
            end;
	    if String.length t.tracker_key > 0 then begin
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
		("Tracker key", "sr br", "Tracker key");
		("", "sr", Printf.sprintf "%s" t.tracker_key) ]
	    end 
	  end in
  print_first_tracker file.file_trackers;

  let cntr = ref 0 in
  List.iter (fun (filename, size) ->
    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
    let fs = Printf.sprintf "File %d" !cntr in
    html_mods_td buf [
      (fs, "sr br", fs);
      ("", "sr", (Printf.sprintf "%s (%Ld bytes)" filename size)) 
    ];
    incr cntr;
  ) file.file_files

let op_file_print_sources_html file buf =

  if Hashtbl.length file.file_clients > 0 then begin

      let chunks = (match file.file_swarmer with
            None -> "" | Some swarmer ->
              Int64Swarmer.verified_bitmap swarmer) in

      html_mods_table_header buf "sourcesTable" "sources al" [
        ( "1", "srh br ac", "Client number", "Num" ) ;
        ( "0", "srh br", "Client UID", "UID" ) ;
        ( "0", "srh br", "Client software", "Soft" ) ;
        ( "0", "srh", "IP address", "IP address" ) ;
        ( "0", "srh br ar", "Port", "Port" ) ;
        ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ;
        ( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ;
        ( "0", "srh ar", "Interested [T]rue, [F]alse", "I" ) ;
        ( "0", "srh ar", "Choked [T]rue, [F]alse", "C" ) ;
        ( "1", "srh br ar", "Allowed to write", "A" ) ;

        ( "0", "srh ar", "Interesting [T]rue, [F]alse", "I" );
        ( "0", "srh ar", "Already sent interested [T]rue, [F]alse", "A" );
        ( "0", "srh br ar", "Already sent not interested [T]rue, [F]alse", "N" );

        ( "0", "srh ar", "Good [T]rue, [F]alse", "G" );
        ( "0", "srh ar", "Incoming [T]rue, [F]alse", "I" );
        ( "0", "srh br ar", "Registered bitfield [T]rue, [F]alse", "B" );

        ( "0", "srh ar", "Optimist Time", "O" );
        ( "0", "srh ar", "Last optimist", "L.Opt" );
        ( "0", "srh br ar", "Num try", "N" );

(*
        ( "0", "srh", "Bitmap (absent|partial|present|verified)", (colored_chunks
        (Array.init (String.length info.G.file_chunks)
        (fun i -> ((int_of_char info.G.file_chunks.[i])-48)))) ) ;
*)
        ( "1", "srh ar", "Number of full chunks", (Printf.sprintf "%d"
              (String.length (String2.replace
                  (String2.replace chunks '0' "") '1' "")) )) ]      ;

      Hashtbl.iter (fun _ c ->
          Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr());

          html_mods_td buf [
            ("", "sr br ar", Printf.sprintf "%d" (client_num c));
            ("", "sr br", (Sha1.to_string c.client_uid));
            ("", "sr br", Printf.sprintf "%s %s" (brand_to_string c.client_brand) c.client_release);
            ("", "sr", (Ip.to_string (fst c.client_host)));
            ("", "sr br ar", Printf.sprintf "%d" (snd c.client_host));
            ("", "sr ar", (size_of_int64 c.client_uploaded));
            ("", "sr ar br", (size_of_int64 c.client_downloaded));
            ("", "sr", (if c.client_interested then "T" else "F"));
            ("", "sr", (if c.client_choked then "T" else "F"));
            ("", "sr br ar", (Int64.to_string c.client_allowed_to_write));
(* This is way too slow for 1000's of chunks on a page with 100's of sources
    ("", "sr", (CommonFile.colored_chunks (Array.init (String.length c.client_bitmap)
       (fun i -> (if c.client_bitmap.[i] = '1' then 2 else 0)) )) );
*)
            ("", "sr", (if c.client_interesting then "T" else "F"));
            ("", "sr", (if c.client_alrd_sent_interested then "T" else "F"));
            ("", "br sr", (if c.client_alrd_sent_notinterested then "T" else "F"));

            ("", "sr", (if c.client_good then "T" else "F"));
            ("", "sr", (if c.client_incoming then "T" else "F"));
            ("", "br sr", (if c.client_registered_bitfield then "T" else "F"));

            ("", "sr", Printf.sprintf "%d" c.client_optimist_time);
            ("", "ar sr", string_of_date c.client_last_optimist);
            ("", "br sr", Printf.sprintf "%d" c.client_num_try);

            ("", "sr ar", (let fc = ref 0 in
                (match c.client_bitmap with
                    None -> ()
                  | Some bitmap ->
                      Bitv.iter (fun s -> if s then incr fc) bitmap);
                (Printf.sprintf "%d" !fc) ) ) ];

          Printf.bprintf buf "\\</tr\\>";

      ) file.file_clients;

      Printf.bprintf buf "\\</table\\>\\</div\\>\\<br\\>";

    end

let op_file_check file =
  lprintf_nl () "Checking chunks of %s" file.file_name;
  match file.file_swarmer with
    None ->
      lprintf_nl () "verify_chunks: no swarmer to verify chunks"
  | Some swarmer ->
      Int64Swarmer.verify_all_chunks swarmer true

let remove_all_clients file =
  Hashtbl.clear file.file_clients; 
  file.file_clients_num <- 0

let op_file_cancel file =
  Int64Swarmer.remove_swarmer file.file_swarmer;
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
    | Some swarmer -> Int64Swarmer.compute_last_seen swarmer in
  {
    P.file_fields = P.Fields_file_info.all;

    P.file_comment = file_comment (as_file file);
    P.file_name = file.file_name;
    P.file_num = (file_num file);
    P.file_network = network.network_num;
    P.file_names = [file.file_name, P.noips()];
    P.file_md4 = Md4.null;
    P.file_size = file_size file;
    P.file_downloaded = file_downloaded file;
    P.file_all_sources = 0;
    P.file_active_sources = 0;
    P.file_state = file_state file;
    P.file_sources = None;
    P.file_download_rate = file_download_rate file.file_file;
    P.file_chunks = (match file.file_swarmer with
        None -> "" | Some swarmer ->
          Int64Swarmer.verified_bitmap swarmer);
    P.file_availability =
    [network.network_num,(match file.file_swarmer with
          None -> "" | Some swarmer ->
            Int64Swarmer.availability swarmer)];
    P.file_format = FormatNotComputed 0;
    P.file_chunks_age = last_seen;
    P.file_age = file_age file;
    P.file_last_seen = file.file_file.impl_file_last_seen;
    P.file_priority = file_priority (as_file file);
    P.file_uids = [Uid.create (BTUrl file.file_id)];
    P.file_sub_files = file.file_files;
  }

let op_ft_info ft =

  let module P = GuiTypes in

  {
    P.file_fields = P.Fields_file_info.all;

    P.file_comment = file_comment (as_ft ft);
    P.file_name = ft.ft_filename;
    P.file_num = ft_num ft;
    P.file_network = network.network_num;
    P.file_names = [ft.ft_filename, P.noips()];
    P.file_md4 = Md4.null;
    P.file_size = ft_size ft;
    P.file_downloaded = zero;
    P.file_all_sources = 0;
    P.file_active_sources = 0;
    P.file_state = ft_state ft;
    P.file_sources = None;
    P.file_download_rate = 0.;
    P.file_chunks = "";
    P.file_availability =  [network.network_num, ""];
    P.file_format = FormatNotComputed 0;
    P.file_chunks_age = [| last_time () |];
    P.file_age = 0;
    P.file_last_seen = BasicSocket.last_time ();
    P.file_priority = 0;
    P.file_uids = [];
    P.file_sub_files = [];
  }



let load_torrent_string s =
  let file_id, torrent = BTTorrent.decode_torrent s in

  (* Save the torrent, because we later want to put
     it in the seeded directory. *)
  let torrent_diskname = Filename.concat
    downloads_directory
    torrent.torrent_name ^ ".torrent"
    in
  File.from_string torrent_diskname s;

  if !verbose then
    lprintf_nl () "Starting torrent download with diskname: %s"
        torrent_diskname;
  let file = new_download file_id torrent torrent_diskname in
  BTClients.get_sources_from_tracker file;
  BTShare.must_share_file file;
  CommonInteractive.start_download (file_find (file_num file));
  file

let load_torrent_file filename =
  if !verbose then
    lprintf_nl () "load_torrent_file %s" filename;
  let s = File.to_string filename in
  (* Delete the torrent if it is in the downloads dir. because it gets saved
     again under the torrent name and we don't want to clutter up this dir. .*)
  if Sys.file_exists filename
      && (Filename.dirname filename) = downloads_directory then
    Sys.remove filename;
  ignore (load_torrent_string s)

let parse_tracker_reply file t filename =
(*This is the function which will be called by the http client
for parsing the response*)
(* Interested only in interval*)
  if !verbose_msg_servers then lprintf_nl () "Filename %s" filename;
    let tracker_reply =
      try
        File.to_string filename
      with e -> lprintf_nl () "Empty reply from tracker"; ""
    in
    let v =
       match tracker_reply with
       | "" ->
        if !verbose_connect then
          lprintf_nl () "Empty reply from tracker";
        Bencode.decode ""
       | _ -> Bencode.decode tracker_reply
    in
  if !verbose_msg_servers then lprintf_nl () "Received: %s" (Bencode.print v);
  t.tracker_interval <- 600;
  match v with
    Dictionary list ->
      List.iter (fun (key,value) ->
          match (key, value) with
            String "interval", Int n ->
              t.tracker_interval <- Int64.to_int n;
              if !verbose_msg_servers then lprintf_nl () ".. interval %d .." t.tracker_interval
          | String "failure reason", String failure ->
                lprintf_nl () "Failure from Tracker in file: %s Reason: %s" file.file_name failure
          (*TODO: merge with f from get_sources_from_tracker and parse the rest of the answer, too.
            also connect to the sources we receive or instruct tracker to send none, perhaps based
            on an config option. firewalled people could activate the option and then seed torrents, too.*)
          | _ -> ()
      ) list;
  | _ -> assert false

let try_share_file torrent_diskname =
  if !verbose_share then lprintf_nl () "try_share_file: %s" torrent_diskname;
  let s = File.to_string torrent_diskname in
  let file_id, torrent = BTTorrent.decode_torrent s in

  try
    let filename =
      let rec iter list =
        match list with
          [] -> raise Not_found
        | sh :: tail ->
            let s = sharing_strategies sh.shdir_strategy in
            if match torrent.torrent_files with
                [] -> not s.sharing_directories
              | _ ->  s.sharing_directories then
              let filename =
                Filename.concat sh.shdir_dirname torrent.torrent_name
              in
              if !verbose_share then lprintf_nl () "Checking for %s" filename;
              if Sys.file_exists filename then filename else
                iter tail
            else
              iter tail
      in
      iter !!shared_directories
    in

    let file = new_file file_id torrent torrent_diskname
        filename FileShared in
    BTShare.must_share_file file;
    if !verbose_share then lprintf_nl () "Sharing file %s" filename;
    BTClients.connect_trackers file "started"
      (parse_tracker_reply file)
  with
  | Not_found ->
      (* if the torrent is still there while the file is gone, remove the torrent *)
      if !verbose_share then lprintf_nl () "Removing torrent for %s" s;
      let new_torrent_diskname =
        Filename.concat old_directory
          (Filename.basename torrent_diskname)
      in
      (try
          Unix2.rename torrent_diskname new_torrent_diskname;
        with _ ->
          (lprintf_nl () "Failed to rename %s to %s"
              torrent_diskname new_torrent_diskname));
(*
      (try Sys.remove torrent_diskname with e ->
          if !verbose_share then
            lprintf_nl () "Error: %s while removing torrent %s" (Printexc2.to_string e) s)
*)
  | e ->
      lprintf_nl () "Cannot share torrent %s for %s"
        torrent_diskname (Printexc2.to_string e)

(* Call one minute after start, and then every 20 minutes. Should
  automatically contact the tracker. *)
let share_files _ =
  if !verbose_share then lprintf_nl () "share_files";
  List.iter (fun dir ->
      let filenames = Unix2.list_directory dir in
      List.iter (fun file ->
          let filename = Filename.concat dir file in
          try_share_file filename
      ) filenames
  ) [seeded_directory];
  let shared_files_copy = !current_files in
 (* if the torrent is gone while the file is still shared, remove the share *)
  List.iter (fun file ->
      (* if !verbose_share then lprintf_nl () "Checking torrent share for %s" file.file_torrent_diskname; *)
      if not (Sys.file_exists file.file_torrent_diskname) &&
        file_state file = FileShared then
        begin
          if !verbose_share then lprintf_nl () "Removing torrent share for %s" file.file_torrent_diskname;
          BTClients.file_stop file;
          remove_file file;
          BTClients.disconnect_clients file;
          remove_all_clients file;
        end
  ) shared_files_copy

let retry_all_ft () =
  Hashtbl.iter (fun _ ft ->
      try ft.ft_retry ft with e ->
          lprintf_nl () "ft_retry: exception %s" (Printexc2.to_string e)
  ) ft_by_num

let load_torrent_from_web r ft =
  if !verbose then
      lprintf_nl () "Loading torrent from web";
  let module H = Http_client in

  if !verbose then
      lprintf_nl () "calling...";
  H.wget r (fun filename ->
      if !verbose then
          lprintf_nl () "done...";
      if ft_state ft = FileDownloading then begin
          load_torrent_file filename;
          file_cancel (as_ft ft);
        end)

let valid_torrent_extension url =
  let ext = String.lowercase (Filename2.last_extension url) in
  if !verbose then lprintf_nl () "Last extension: %s" ext;
  if ext = ".torrent" || ext = ".tor" then true else false

let get_regexp_string text r =
  ignore (Str.search_forward r text 0);
  let a = Str.group_beginning 1 in
  let b = Str.group_end 1 in
  String.sub text a (b - a)

let op_network_parse_url url =
  let location_regexp = "Location: \\(.*\\)" in
  try
    let real_url = get_regexp_string url (Str.regexp location_regexp) in
    if !verbose then lprintf_nl () "Loading %s, really %s" url real_url;
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
              let referers = !!BTOptions.referers in
              let (rule_search,rule_value) =
                try (List.find(fun (rule_search,rule_value) ->
                        Str.string_match (Str.regexp rule_search) real_url 0
                    ) referers )
                with Not_found -> ("",real_url) in
              Some (Url.of_string rule_value) );
            H.req_headers = (try
              let cookies = List.assoc u.Url.server !!BTOptions.cookies in
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
        let ft = new_ft file_diskname in
        ft.ft_retry <- load_torrent_from_web r ;
        load_torrent_from_web r ft;
        if !verbose then lprintf_nl () "wget started";
        "started download", true
      )
    else
      "", false
  with
    | Not_found ->
      if (valid_torrent_extension url) then
        try
          if !verbose then lprintf_nl () "Not_found and trying to load %s" url;
          load_torrent_file url;
          "", true
        with e ->
          lprintf_nl () "Exception %s while 2nd loading" (Printexc2.to_string e);
	  let s = Printf.sprintf "Can not load load torrent file: %s"
	    (Printexc2.to_string e) in
          s, false
      else
        begin
          if !verbose then lprintf_nl () "Not_found and url has non valid torrent extension: %s" url;
          "Not_found and url has non valid torrent extension", false
        end
    | e ->
       lprintf_nl () "Exception %s while loading" (Printexc2.to_string e);
       let s = Printf.sprintf "Can not load load torrent file: %s"
	 (Printexc2.to_string e) in
       s, false

let op_client_info c =
  let module P = GuiTypes in
  let (ip,port) = c.client_host in
  {
    P.client_network = network.network_num;
    P.client_kind = Known_location (ip,port);
    P.client_state = client_state (as_client c);
    P.client_type = client_type c;
    P.client_tags = [];
    P.client_name =
    (Printf.sprintf "%s:%d" (Ip.to_string ip) port);
    P.client_files = None;
    P.client_num = (client_num c);
    P.client_rating = 0;
    P.client_chat_port = 0 ;
    P.client_connect_time = BasicSocket.last_time ();
    P.client_software = (brand_to_string c.client_brand);
    P.client_release = c.client_release;
    P.client_emulemod = "";
    P.client_downloaded = c.client_downloaded;
    P.client_uploaded = c.client_uploaded;
    P.client_upload = Some (c.client_file.file_name);
    P.client_sui_verified = None;
(*          P.client_sock_addr = (Ip.to_string ip); *)
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
    (Int64.to_string c.client_downloaded)
  (Int64.to_string c.client_uploaded)
  (if c.client_downloaded > c.client_uploaded then "-" else "+")
  (if c.client_uploaded > Int64.zero then 
     Int64.to_float (c.client_downloaded // c.client_uploaded) 
   else 1.)
  ("BT");
  (Printf.bprintf buf (_b "%18sFile  : %s\n") "" info.GuiTypes.file_name)

let op_client_dprint_html c o file str =
  let info = file_info file in
  let buf = o.conn_buf in
  let cc = as_client c in
  let cinfo = client_info cc in
  Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\"
  onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>" str;

  let show_emulemods_column = ref false in
  if Autoconf.donkey = "yes" then begin
      if !!emule_mods_count then
        show_emulemods_column := true
    end;

    html_mods_td buf ([
        ("", "srb ar", Printf.sprintf "%d" (client_num c));
        ((string_of_connection_state (client_state cc)), "sr",
          (short_string_of_connection_state (client_state cc)));
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
        ("", "sr ar", (size_of_int64 c.client_uploaded));
        ("", "sr ar", (size_of_int64 c.client_downloaded));
        ("", "sr", info.GuiTypes.file_name); ]);
    true

let op_network_connected _ = true


let get_default_tracker () = 
  if !!BTTracker.default_tracker = "" then
     Printf.sprintf "http://%s:%d/announce"
      (Ip.to_string (CommonOptions.client_ip None))
      !!BTTracker.tracker_port 
   else
     !!BTTracker.default_tracker

let compute_torrent filename announce comment = 
  let announce = if announce = "" then get_default_tracker () else announce in
  if !verbose then lprintf_nl () "compute_torrent: [%s] [%s] [%s]"
   filename announce comment;
  let basename = Filename.basename filename in
  let torrent = Filename.concat seeded_directory
    (Printf.sprintf "%s.torrent" basename) in
  let is_private = 0 in
  BTTorrent.generate_torrent announce torrent comment (Int64.of_int is_private) filename;
  try_share_file torrent

let commands =

    [
    "compute_torrent", "Network/Bittorrent", Arg_multiple (fun args o ->
      let buf = o.conn_buf in
      try
        let filename = ref "" in
        let comment = ref "" in
        (match args with
          fname :: [comm] -> filename := fname; comment := comm
        | [fname] -> filename := fname
        | _ -> raise Not_found);

        compute_torrent !filename "" !comment;

        if o.conn_output = HTML then
          (* TODO: really htmlize it *)
          Printf.bprintf buf ".torrent file generated"
        else
          Printf.bprintf buf ".torrent file generated\n";
      ""
      with Not_found ->
          if o.conn_output = HTML then
            (* TODO: really htmlize it *)
            Printf.bprintf buf "Not enough parameters"
          else
            Printf.bprintf buf "Not enough parameters\n";
      ""
    ), _s " <filename> <comment>:\t\tgenerate the corresponding <filename> .torrent file with <comment> in torrents/tracked/.\n\t\t\t\t\tThe file is automatically tracked, and seeded if in incoming/";

    "torrents", "Network/Bittorrent", Arg_none (fun o ->
      let buf = o.conn_buf in
      if !!BTTracker.tracker_port <> 0 then begin
          Printf.bprintf o.conn_buf (_b ".torrent files available:\n");
          let files_tracked = Unix2.list_directory tracked_directory in
          let files_downloading = Unix2.list_directory downloads_directory in
          let files_seeded = Unix2.list_directory seeded_directory in
          let all_torrents_files = files_tracked @ files_downloading @ files_seeded in

          if o.conn_output = HTML then
            (* TODO: really htmlize it *)
            List.iter (fun file ->
                Printf.bprintf buf "http://%s:%d/%s "
                  (Ip.to_string (CommonOptions.client_ip None))
                !!BTTracker.tracker_port
                  file
            ) all_torrents_files
          else
            List.iter (fun file ->
                Printf.bprintf buf "http://%s:%d/%s\n"
                  (Ip.to_string (CommonOptions.client_ip None))
                !!BTTracker.tracker_port
                  file
            ) all_torrents_files;
        end
      else
          if o.conn_output = HTML then
            (* TODO: really htmlize it *)
            Printf.bprintf buf "Tracker not activated (tracker_port = 0)"
          else
            Printf.bprintf buf "Tracker not activated (tracker_port = 0)\n";
        _s ""
    ), _s " :\t\t\t\tprint all .torrent files on this server";

    "seeded_torrents", "Network/Bittorrent", Arg_none (fun o ->
      List.iter (fun file ->
          if file_state file = FileShared then
              Printf.bprintf o.conn_buf "%s [%s]\n" file.file_name (Int64.to_string file.file_uploaded)
      ) !current_files;
      _s "done"

    ), _s " :\t\t\tprint all seeded .torrent files on this server";

    "reshare_torrents", "Network/Bittorrent", Arg_none (fun o ->
      share_files ();
      _s "done"
    ), _s " :\t\t\trecheck torrents/* directories for changes";

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
    ), _s " :\t\t\t\tremove all old .torrent files";

    "stop_all_bt", "Network/Bittorrent", Arg_none (fun o ->
      List.iter (fun file -> BTClients.file_stop file ) !current_files;
      let buf = o.conn_buf in
      if o.conn_output = HTML then
          (* TODO: really htmlize it *)
          Printf.bprintf buf "started sending stops..."
      else
        Printf.bprintf buf "started sending stops...\n";
       _s ""
    ), _s " :\t\t\t\tstops all bittorrent downloads, use this if you want to make sure that the stop signal actualy gets to the tracker\n\t\t\t\twhen shuting mlnet down, but you have to wait till the stops get to the tracker and not wait too long,\n\t\t\t\tso mldonkey reconnects to the tracker :)";

(* TODO : add some code from make_torrent
    "print_torrent", Arg_one (fun filename o ->

        ".torrent file printed"
    ), " <filename.torrent> : print the content of filename"
 *)

  ]

open LittleEndian
open GuiDecoding

let op_gui_message s =
  match get_int16 s 0 with
    0 ->
      let text = String.sub s 2 (String.length s - 2) in
      if !verbose then lprintf_nl () "received torrent from gui...";
      ignore (load_torrent_string text)
  | 1 -> (* 34+ *)
      let n = get_int s 2 in
      let a, pos = get_string s 6 in
      let c, pos = get_string s pos in
      let sf = CommonShared.shared_find n in
      let f = shared_fullname sf in
      compute_torrent f a c;
  | opcode -> failwith (Printf.sprintf "[BT] Unknown message opcode %d" opcode)

let _ =

  ft_ops.op_file_cancel <- op_ft_cancel;
  ft_ops.op_file_commit <- op_ft_commit;
  ft_ops.op_file_info <- op_ft_info;

  file_ops.op_file_all_sources <- op_file_all_sources;
  file_ops.op_file_files <- op_file_files;
  file_ops.op_file_active_sources <- op_file_all_sources;
  file_ops.op_file_debug <- op_file_debug;
  file_ops.op_file_commit <- op_file_commit;
  file_ops.op_file_print_html <- op_file_print_html;
  file_ops.op_file_print_sources_html <- op_file_print_sources_html;
  file_ops.op_file_check <- op_file_check;
  file_ops.op_file_cancel <- op_file_cancel;
  file_ops.op_file_info <- op_file_info;
  file_ops.op_file_save_as <- (fun file name -> ());

  network.op_network_gui_message <- op_gui_message;
  network.op_network_connected <- op_network_connected;
  network.op_network_parse_url <- op_network_parse_url;
  network.op_network_share <- (fun fullname codedname size -> ());
  network.op_network_forget_search <- (fun s -> ());
  network.op_network_search <- (fun ss buf -> ());
  network.op_network_download <- (fun r -> dummy_file);
  network.op_network_recover_temp <- (fun s -> ());
  network.op_network_clean_exit <- (fun s ->
    List.iter (fun file -> BTClients.file_stop file) !current_files;
    List.for_all (fun file -> not file.file_tracker_connected) !current_files;
  );

  client_ops.op_client_info <- op_client_info;
  client_ops.op_client_connect <- op_client_connect;
  client_ops.op_client_disconnect <- op_client_disconnect;
  client_ops.op_client_bprint <- op_client_bprint;
  client_ops.op_client_dprint <- op_client_dprint;
  client_ops.op_client_dprint_html <- op_client_dprint_html;

  CommonNetwork.register_commands commands;

  shared_ops.op_shared_unshare <- (fun file ->
      (if !verbose_share then lprintf_nl () "unshare file");
      BTShare.unshare_file file);
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
