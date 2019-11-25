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
open Gettext
open Printf2
open Options
open CommonTypes
open CommonGlobals
open CommonInteractive


type style = Old | New

let _s x = _s "CommonStats" x
let _b x = _b "CommonStats" x

let define_option a b ?desc ?restart ?public ?internal c d e =
  match desc with
    None -> define_option a b (_s c) d e ?restart ?public ?internal
  | Some desc -> define_option a b ~desc: (_s desc) (_s c) d e ?restart ?public ?internal

let statistics_ini = create_options_file "statistics.ini"

let country_stats_section = file_section statistics_ini
    ["Country statistics"] "Country-based traffic statistics"

module CountryStatsOption = struct

    let value_to_country v =
      match v with
      | Options.Module assocs ->
          let int64v s = try value_to_int64 (List.assoc s assocs) with Not_found -> 0L in
          let cc = value_to_string (List.assoc "country_code" assocs) in
          let cn = try
              Geoip.country_name_array.(Hashtbl.find Geoip.country_index cc)
            with Not_found -> "??" in
          let co = try
              Geoip.country_continent_name_array.(Hashtbl.find Geoip.country_index cc)
            with Not_found -> "??" in
          { country_code = cc;
            country_name = cn;
            country_continent = co;
            country_total_upload = int64v "country_upload";
            country_total_download = int64v "country_download";
            country_total_seen = int64v "country_seen";
            country_session_upload = 0L;
            country_session_download = 0L;
            country_session_seen = 0L;
          }
      | _ -> failwith "Options: invalid country statistics"

    let country_to_value c =
      Options.Module (
        ["country_code", string_to_value c.country_code] @
        (if c.country_total_upload = 0L then [] else
        ["country_upload", int64_to_value c.country_total_upload]) @
        (if c.country_total_download = 0L then [] else
        ["country_download", int64_to_value c.country_total_download]) @
        (if c.country_total_seen = 0L then [] else
        ["country_seen", int64_to_value c.country_total_seen])
      )

    let t = define_option_class "CountryStats" value_to_country country_to_value

  end

let country_stats_uptime = define_option country_stats_section ["guptime"]
  "Uptime" int_option 0

let country_stats = define_option country_stats_section ["country_stats"]
  "Country-based traffic statistics" (list_option CountryStatsOption.t) []

let country_stats_find cc =
  let cc = Geoip.country_code_array.(cc) in
  try
    List.find (fun c -> c.country_code = cc) !!country_stats
  with Not_found ->
    let cs = {
      country_code = cc;
      country_name = Geoip.country_name_array.(Hashtbl.find Geoip.country_index cc);
      country_continent = Geoip.country_continent_name_array.(Hashtbl.find Geoip.country_index cc);
      country_total_upload = 0L;
      country_total_download = 0L;
      country_total_seen = 0L;
      country_session_upload = 0L;
      country_session_download = 0L;
      country_session_seen = 0L;
    } in
    country_stats =:= cs :: !!country_stats;
    cs

let country_upload cc v =
  match cc with | None -> ()
  | Some cc ->
    let c = country_stats_find cc in
    c.country_session_upload <- c.country_session_upload ++ v;
    c.country_total_upload <- c.country_total_upload ++ v

let country_download cc v =
  match cc with | None -> ()
  | Some cc ->
    let c = country_stats_find cc in
    c.country_session_download <- c.country_session_download ++ v;
    c.country_total_download <- c.country_total_download ++ v

let country_seen cc =
  match cc with | None -> ()
  | Some cc ->
    let c = country_stats_find cc in
    c.country_session_seen <- c.country_session_seen ++ 1L;
    c.country_total_seen <- c.country_total_seen ++ 1L

let global_count_upload n cc v =
  upload_counter := !upload_counter ++ v;
  country_upload cc v;
  network_must_update n

let global_count_download n cc v =
  download_counter := !download_counter ++ v;
  country_download cc v;
  network_must_update n

let find_int_of_brand brand brand_list =
  let rec iter l i =
  match l with
    [] -> 0
    | (b,_,_) :: t -> if b = brand then i else iter t (i+1)
  in
  iter brand_list 0

let find_brand_to_string brand brand_list short =
  let rec iter l =
  match l with
    [] -> "?"
    | (b,l,s) :: t -> if b = brand then if short then s else l else iter t
  in
  iter brand_list 

let percent_of_ints x y =
  if y = 0 then 0. else
    max 0.0 (100. *. (float_of_int x /. float_of_int y))

let percent_of_int64s x y =
  if y = 0L then 0. else
    max 0.0 (100. *. (Int64.to_float x /. Int64.to_float y))

let build_title n t uptime =

  let b = Printf.sprintf "%s Uptime: %s" t (Date.time_to_string uptime "verbose") in
  let c = Printf.sprintf "%d seconds" uptime in

  [n;b;c]

let tl_to_string l = 
  String.concat " | " l

let build_all a =
  let s = 
  {
    brand_seen = 0;
    brand_banned = 0;
    brand_filerequest = 0;
    brand_download = 0L;
    brand_upload = 0L;
  } in

  for i = 0 to (Array.length a) - 1 do
    let r = a.(i) in 
    s.brand_seen <- s.brand_seen + r.brand_seen;
    s.brand_banned <- s.brand_banned + r.brand_banned;
    s.brand_filerequest <- s.brand_filerequest + r.brand_filerequest;
    s.brand_download <- s.brand_download ++ r.brand_download ;
    s.brand_upload <- s.brand_upload ++ r.brand_upload;
  done;
  s

let brandlist_int_to_2string l i =
  if List.length l > i then
    let (_,ls,ss) = List.nth l i in
    ls,ss
  else
    "Total","Ttl"

let brandlist_int_to_string l i =
  let (ls,_) = brandlist_int_to_2string l i in 
  ls

let print_stats_old buf arr l tl uptime =

  let title = tl_to_string tl in
  let nbrands = Array.length arr in

  let stats_all = build_all arr in

  Printf.bprintf buf "%s\n" title;

  if stats_all.brand_seen = 0 then
    Printf.bprintf buf "You haven't connected to any client yet\n"
  else begin
    Printf.bprintf buf "\n     Successful Connections: %18d\n" stats_all.brand_seen;
    for i=0 to nbrands-1  do
      let r = arr.(i) in
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n"
        (brandlist_int_to_string l i) r.brand_seen (percent_of_ints r.brand_seen stats_all.brand_seen)
    done
  end;

  if stats_all.brand_filerequest = 0 then
    Printf.bprintf buf "You weren't asked for any file yet\n"
  else begin
    Printf.bprintf buf "\nTotal filerequests received: %18d\n" stats_all.brand_filerequest;
    for i=0 to nbrands-1 do
      let r = arr.(i) in
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n"
        (brandlist_int_to_string l i) r.brand_filerequest
        (percent_of_ints r.brand_filerequest stats_all.brand_filerequest)
    done
  end;

  if stats_all.brand_download = Int64.zero then
    Printf.bprintf buf "You didn't download anything yet\n"
  else begin
    Printf.bprintf buf "\n            Total downloads: %18s (%5.1f KB/s)\n"
      (Int64.to_string stats_all.brand_download)
      ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0);
    for i=0 to nbrands-1 do
      let r = arr.(i) in
      Printf.bprintf buf "%27s: %18s (%5.1f %%)\n"
        (brandlist_int_to_string l i) (Int64.to_string r.brand_download)
        (percent_of_int64s r.brand_download stats_all.brand_download)
    done
  end;

  if stats_all.brand_upload = Int64.zero then
    Printf.bprintf buf "You didn't upload anything yet\n"
  else begin
      Printf.bprintf buf "\n              Total uploads: %18s (%5.1f KB/s)\n"
        (Int64.to_string stats_all.brand_upload)
        ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0);
    for i=0 to nbrands-1 do
      let r = arr.(i) in
      Printf.bprintf buf "%27s: %18s (%5.1f %%)\n"
        (brandlist_int_to_string l i) (Int64.to_string r.brand_upload)
        (percent_of_int64s r.brand_upload stats_all.brand_upload)
    done
  end;

  if stats_all.brand_banned = 0 then
    Printf.bprintf buf "You didn't ban any client yet\n"
  else begin
      Printf.bprintf buf "\n                 Total bans: %18d\n" stats_all.brand_banned;
    for i=0 to nbrands-1 do
      let r = arr.(i) in
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n"
        (brandlist_int_to_string l i) r.brand_banned
        (percent_of_ints r.brand_banned stats_all.brand_banned)
    done
  end

let print_stats_ascii buf arr l tl uptime =
  let title = tl_to_string tl in
  let stats_all = build_all arr in

  Printf.bprintf buf "%s\n" title;
  Printf.bprintf buf "Client Brand|    seen      |     Downloads      |      Uploads       |   Banned   |  Requests\n";
  Printf.bprintf buf "------------+--------------+--------------------+--------------------+------------+--------------\n";

  for i = 0 to ((Array.length arr)-1) do
    let r = arr.(i) in

    if r.brand_seen > 0 then
      Printf.bprintf buf "%-12s|%9d %3.f%%|%9.1f %5.1f %3.0f%%|%9.1f %5.1f %3.0f%%|%7d %3.0f%%|%9d %3.0f%%\n"
        (brandlist_int_to_string l i)
        (r.brand_seen)
        (percent_of_ints r.brand_seen stats_all.brand_seen)

        ((Int64.to_float r.brand_download) /. 1024.0 /. 1024.0)
        ((Int64.to_float r.brand_download) /. (float_of_int uptime) /. 1024.0)
        (percent_of_int64s r.brand_download stats_all.brand_download)

        ((Int64.to_float r.brand_upload) /. 1024.0 /. 1024.0)
        ((Int64.to_float r.brand_upload) /. (float_of_int uptime) /. 1024.0)
        (percent_of_int64s r.brand_upload stats_all.brand_upload)

        (r.brand_banned)
        (percent_of_ints r.brand_banned stats_all.brand_banned)

        (r.brand_filerequest)
        (percent_of_ints r.brand_filerequest stats_all.brand_filerequest)
  done;

  Printf.bprintf buf "------------+--------------+--------------------+--------------------+------------+--------------\n";
  Printf.bprintf buf "%-12s|%9d     |%9.1f %5.1f     |%9.1f %5.1f     |%7d     |%9d\n"
    "Total"
    (stats_all.brand_seen)
    ((Int64.to_float stats_all.brand_download) /. 1024.0 /. 1024.0)
    ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0)
    ((Int64.to_float stats_all.brand_upload) /. 1024.0 /. 1024.0)
    ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0)
    (stats_all.brand_banned)
    (stats_all.brand_filerequest)


let stats_html_header buf =
  html_mods_table_header buf ~total:"1" "csTable" "cs" [
   ( Str, "srh", "Client brand", "Brand" );
   ( Str, "srh", "Separator", ":" );
   ( Num, "srh ar", "Successful connections", "Seen" );
   ( Num, "srh", "Successful connections percent", "%" );
   ( Str, "srh", "Separator", "|" );
   ( Num, "srh ar", "File requests received", "Reqs" );
   ( Num, "srh", "File requests received percent", "%" );
   ( Str, "srh", "Separator", "|" );
   ( Num, "srh ar", "Total bans", "B" );
   ( Num, "srh", "Total bans percent", "%" );
   ( Str, "srh", "Separator", "|" ); 
   ( Num, "srh ar", "Total uploads", "UL" );
   ( Num, "srh", "Total uploads percent", "%" );
   ( Num, "srh ar", "Total uploads average KB/s", "KB/s" );
   ( Str, "srh", "Separator", "|" );
   ( Num, "srh ar", "Total downloads", "DL" );
   ( Num, "srh", "Total downloads percent", "%" );
   ( Num, "srh ar", "Total downloads average KB/s", "KB/s" );
   ( Str, "srh", "Separator", "|" );
   ( Num, "srh", "Total uploads:downloads ratio", "U:DL" );
  ]

let stats_list l arr = 
  let sl = ref [] in
  for i = 0 to (Array.length arr) - 1 do
    let r = arr.(i) in
    if r.brand_seen > 0 then begin
      let ls,ss = brandlist_int_to_2string l i in
      let s = {
        string_long = ls;
        string_short = ss;
        seen = r.brand_seen;
        banned = r.brand_banned;
        filerequest = r.brand_filerequest;
        download = r.brand_download;
        upload = r.brand_upload;
      } in
      sl := s :: !sl
    end
  done;
  !sl

let print_stats_html_mods buf arr l tl uptime =

  let stats_all = build_all arr in

  let a = Array.append arr [|stats_all|] in

  html_mods_big_header_start buf "cs" tl;

  stats_html_header buf;

  let r_all = a.((Array.length a)-1) in

  html_mods_cntr_init();

  for i=0 to (Array.length a) - 1 do

    let r = a.(i) in
    let is_total = i == (Array.length a) - 1 in
    if r.brand_seen > 0 then
    begin
      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr());

      let seen_percent = if is_total then 100.0 else percent_of_ints r.brand_seen r_all.brand_seen in
      let freq_percent = if is_total then 100.0 else percent_of_ints r.brand_filerequest r_all.brand_filerequest in

      let banned_percent = if is_total then 100.0 else percent_of_ints r.brand_banned r_all.brand_banned in

      let upload_percent = if is_total then 100.0 else percent_of_int64s r.brand_upload r_all.brand_upload in
      let upload_akbs = (Int64.to_float r.brand_upload) /.  (float_of_int uptime) /. 1024.0 in

      let download_percent = if is_total then 100.0 else percent_of_int64s r.brand_download r_all.brand_download in
      let download_akbs = (Int64.to_float r.brand_download) /.  (float_of_int uptime) /. 1024.0 in

      let ratio = if r.brand_upload = 0L then 0.0
                  else (Int64.to_float r.brand_download) /. (Int64.to_float r.brand_upload) in

      html_mods_td buf [
        ("", (if is_total then "sr total" else "sr"), brandlist_int_to_string l i);
        ("", (if is_total then "sr total" else "sr"), ":");
        ("", (if is_total then "sr ar total" else "sr ar"), Printf.sprintf "%d" r.brand_seen );
        ("", (if is_total then "srp total" else "srp"), Printf.sprintf "(%.f%%)" seen_percent );
        ("", (if is_total then "sr total" else "sr"), "|");
        ("", (if is_total then "sr ar total" else "sr ar"), Printf.sprintf "%d" r.brand_filerequest);
        ("", (if is_total then "srp total" else "srp"), Printf.sprintf "(%.f%%)" freq_percent);
        ("", (if is_total then "sr total" else "sr"), "|");
        ("", (if is_total then "sr ar total" else "sr ar"), Printf.sprintf "%d" r.brand_banned);
        ("", (if is_total then "srp total" else "srp"), Printf.sprintf "(%.f%%)" banned_percent);
        ("", (if is_total then "sr total" else "sr"), "|");
        ("", (if is_total then "sr ar total" else "sr ar"), size_of_int64 r.brand_upload);
        ("", (if is_total then "srp total" else "srp"), Printf.sprintf "(%.0f%%)" upload_percent);
        ("", (if is_total then "sr ar total" else "sr ar"), Printf.sprintf "%.1f" upload_akbs);
        ("", (if is_total then "sr total" else "sr"), "|");
        ("", (if is_total then "sr ar total" else "sr ar"), size_of_int64 r.brand_download);
        ("", (if is_total then "srp total" else "srp"), Printf.sprintf "(%.0f%%)" download_percent );
        ("", (if is_total then "sr ar total" else "sr ar"), Printf.sprintf "%.1f" download_akbs);
        ("", (if is_total then "sr total" else "sr"), "|");
        ("", (if is_total then "sr total" else "sr"), Printf.sprintf "1:%.2f" ratio);
      ];

      Printf.bprintf buf "\\</tr\\>\n";

    end;
  done;
  Buffer.add_string buf "\\</table\\>\\</div\\>\n";
  html_mods_big_header_end buf;
  Buffer.add_string buf "\\<P\\>\n"

let start_time = ref (BasicSocket.last_time ())
let diff_time = ref 0
let guptime () = !!country_stats_uptime - !diff_time
let config_files_loaded = ref false

let load () =
  Options.load statistics_ini;
  config_files_loaded := true

let save () =
  if !config_files_loaded then begin
      let time_diff = BasicSocket.last_time () - BasicSocket.start_time in
      country_stats_uptime =:= !!country_stats_uptime + time_diff - !diff_time;
      diff_time := time_diff;
    Options.save statistics_ini
  end

let country_reset () =
  country_stats =:= [];
  country_stats_uptime =:= 0;
  diff_time := 0;
  start_time := BasicSocket.last_time ();
  save ()

let _ =
  option_hook country_stats (fun _ ->
    try
      ignore (List.find (fun c -> c.country_name = "??") !!country_stats);
      country_stats =:= List.filter (fun c -> c.country_name <> "??") !!country_stats;
    with Not_found -> ());
  Heap.add_memstat "CommonStats" (fun level buf ->
    Printf.bprintf buf "  countries: %d%s\n"
      (List.length !!country_stats)
      (if not (Geoip.active ()) then " - Geoip not active" else "");
   )
