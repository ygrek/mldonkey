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

open Int64ops
open Printf2
open Options
open CommonTypes
open CommonOptions
open CommonNetwork
open CommonStats

open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions

let start_session = ref (BasicSocket.last_time ())

let stats_array = Array.init brand_count (fun _ ->
    { dummy_stats with brand_seen = 0 }
  )
let stats_mod_array = Array.init brand_mod_count (fun _ ->
    { dummy_stats with brand_seen = 0 }
  )

let count_seen c =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_seen <- stats_array.(i).brand_seen + 1;
  !!gstats_array.(i).brand_seen <- !!gstats_array.(i).brand_seen + 1;
  check_client_country_code c;
  CommonStats.country_seen c.client_country_code;
  
  if !!emule_mods_count then begin
    let i = brand_mod_to_int c.client_brand_mod in
    stats_mod_array.(i).brand_seen <- stats_mod_array.(i).brand_seen + 1;
    !!gstats_mod_array.(i).brand_seen <- !!gstats_mod_array.(i).brand_seen + 1
  end

let count_banned c =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_banned <- stats_array.(i).brand_banned + 1;
  !!gstats_array.(i).brand_banned <- !!gstats_array.(i).brand_banned + 1;

  if !!emule_mods_count then begin
    let i = brand_mod_to_int c.client_brand_mod in
    stats_mod_array.(i).brand_banned <- stats_mod_array.(i).brand_banned + 1;
    !!gstats_mod_array.(i).brand_banned <- !!gstats_mod_array.(i).brand_banned + 1
  end

let count_filerequest c =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_filerequest <- stats_array.(i).brand_filerequest + 1;
  !!gstats_array.(i).brand_filerequest <- !!gstats_array.(i).brand_filerequest + 1;

  if !!emule_mods_count then begin
    let i = brand_mod_to_int c.client_brand_mod in
    stats_mod_array.(i).brand_filerequest <- stats_mod_array.(i).brand_filerequest + 1;
    !!gstats_mod_array.(i).brand_filerequest <- !!gstats_mod_array.(i).brand_filerequest + 1
  end

let count_download c v =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_download <- stats_array.(i).brand_download ++ v;
  !!gstats_array.(i).brand_download <- !!gstats_array.(i).brand_download ++ v;

  if !!emule_mods_count then begin
    let i = brand_mod_to_int c.client_brand_mod in
    stats_mod_array.(i).brand_download <- stats_mod_array.(i).brand_download ++ v;
    !!gstats_mod_array.(i).brand_download <- !!gstats_mod_array.(i).brand_download ++ v;
  end;

  c.client_total_downloaded <- c.client_total_downloaded ++ v;
  c.client_session_downloaded <- c.client_session_downloaded ++ v;
  donkey_download_counter := !donkey_download_counter ++ v;
  check_client_country_code c;
  global_count_download network c.client_country_code v

let count_upload c v =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_upload <- stats_array.(i).brand_upload ++ v;
  !!gstats_array.(i).brand_upload <- !!gstats_array.(i).brand_upload ++ v;

  if !!emule_mods_count then begin
    let i = brand_mod_to_int c.client_brand_mod in
    stats_mod_array.(i).brand_upload <- stats_mod_array.(i).brand_upload ++ v;
    !!gstats_mod_array.(i).brand_upload <- !!gstats_mod_array.(i).brand_upload ++ v;
  end;

  c.client_total_uploaded <- c.client_total_uploaded ++ v;
  c.client_session_uploaded <- c.client_session_uploaded ++ v;
  donkey_upload_counter := !donkey_upload_counter ++ v;
  check_client_country_code c;
  global_count_upload network c.client_country_code v

let print_stats_mods o style =
  let buf = o.conn_buf in

  let u1 = BasicSocket.last_time () - !start_session in
  let u2 = (guptime() + u1) in

  let t1 = build_title "eMule Mods" "Session" u1 in
  let t2 = build_title "eMule Mods" "Total" u2 in

  let l = brand_mod_list in

  match style with
    | Old -> print_stats_old buf stats_mod_array l t1 u1
    | _ ->
      if !!emule_mods_count then begin

        if use_html_mods o then begin
          print_stats_html_mods buf stats_mod_array l t1 u1;
          print_stats_html_mods buf !!gstats_mod_array l t2 u2
        end else begin
          print_stats_ascii buf stats_mod_array l t1 u1;
          print_stats_ascii buf !!gstats_mod_array l t2 u2
        end
      
      end else 
      begin

        if use_html_mods o then begin
          Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
          html_mods_table_header buf "emodsTable" "cs" [];
          Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
          html_mods_td buf [
              ("", "sr", "eMule mods statistics are disabled, to activate set emule_mods_count true in the \\<a href=\\\"/submit?q=voo+8\\\"\\>misc options\\</a\\> tab." ); 
          ];
          Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>\n"
        end else
          Printf.bprintf buf "eMule mods statistics are disabled, to activate set emule_mods_count true \n"

      end

let print_stats o style mods =
  let buf = o.conn_buf in

  let u1 = BasicSocket.last_time () - !start_session in
  let u2 = (guptime() + u1) in

  let t1 = build_title "eDonkey" "Session" u1 in
  let t2 = build_title "eDonkey" "Total" u2 in

  let l = brand_list in

  if mods then print_stats_mods o style else
    match style with
    | Old -> print_stats_old buf stats_array l t1 u1
    | _ ->
          if use_html_mods o then begin
            print_stats_html_mods buf stats_array l t1 u1;
            print_stats_html_mods buf !!gstats_array l t2 u2
          end else begin
            print_stats_ascii buf stats_array l t1 u1;
            print_stats_ascii buf !!gstats_array l t2 u2
          end

let _ =
  network.op_network_display_stats <- (fun o -> print_stats o New false);

  network.op_network_stat_info_list <- (fun _ ->
    let r = ref [] in
    let l1 = stats_list brand_list stats_array in
    let l2 = stats_list brand_list !!gstats_array in
    let u1 = BasicSocket.last_time () - !start_session in
    let u2 = (guptime() + u1) in
    r := [("Session clients", u1, l1); ("Global clients", u2, l2)];
    if !!emule_mods_count then begin
      let l3 = stats_list brand_mod_list stats_mod_array in
      let l4 = stats_list brand_mod_list !!gstats_mod_array in
      r := !r @ [ ("Session mods", u1, l3); ("Global mods", u2, l4)]
    end;
    !r
  );

  register_commands
    [
    "client_stats", "Network/Donkey",Arg_none (fun o ->
        print_stats o Old false;
        ""
    ), ":\t\t\t\tshow breakdown of download/upload by clients brand";

    "cs", "Network/Donkey",Arg_none (fun o ->
        print_stats o New false;
        ""
    ), ":\t\t\t\t\tshow table of download/upload by ED2K clients brand";

    "csm", "Network/Donkey",Arg_none (fun o ->
        print_stats o New true;
        ""
    ), ":\t\t\t\t\tshow table of download/upload by eMule MODs";
    "reset_stats", "Network/Donkey",Arg_none (fun o ->

       Array.iteri  (fun x _ ->
         stats_array.(x).brand_seen <- 0;
         stats_array.(x).brand_banned <- 0;
         stats_array.(x).brand_filerequest <- 0;
         stats_array.(x).brand_download <- 0L;
         stats_array.(x).brand_upload <- 0L;
       ) stats_array;

       Array.iteri (fun x _ ->
         stats_mod_array.(x).brand_seen <- 0;
         stats_mod_array.(x).brand_banned <- 0;
         stats_mod_array.(x).brand_filerequest <- 0;
         stats_mod_array.(x).brand_download <- Int64.zero;
         stats_mod_array.(x).brand_upload <- Int64.zero
       ) stats_mod_array;

       start_session := BasicSocket.last_time ();

       "done"
    ), ":\t\t\t\treset session statistics";
  ]

(* 
let save_download_history file =

  let buf = Buffer.create 100 in

(* Some opcode for edonkey downloads *)
  buf_int8 buf 153;         (* opcode = stats *)
  buf_md4 buf !!client_md4; (* client md4, and NOT IP *)

  let time = Unix.time () in
  let time = Unix.localtime time in
  (* date on 5 bytes *)
  buf_int8 buf time.Unix.tm_hour;
  buf_int8 buf time.Unix.tm_mday;
  buf_int8 buf time.Unix.tm_mon;
  buf_int16 buf time.Unix.tm_year;
(* ANONYMISED Informations on the downloads: *)
(*   Send the SHA1 hash of the MD4+size, so that they cannot be recovered,
          but they can be used to compare downloads. *)
  let m = Printf.sprintf "%s%Ld"
      (Md4.Md4.direct_to_string file.file_md4) (file_size file) in
  let m = Md4.Sha1.string m in (* compute SHA1 of the string *)
(* SENT: 20 bytes *)
  Buffer.add_string buf (Md4.Sha1.direct_to_string m);

(* Send the magnitude of the size (power of 10) *)
  let size = ref (file_size file) in
  let m = ref 0 in
  while !size <> Int64.zero do
    incr m;
    size := !size // 10L
  done;
(* SENT: 1 byte *)
  buf_int8 buf !m;

  let current = ref [] in

  Intmap.iter (fun _ c ->
      let location =
        match c.client_kind with
          Indirect_location (name, md4) ->
            Printf.sprintf "%s%s" name
              (Md4.Md4.direct_to_string md4)
        | Known_location (ip,port) ->
            Printf.sprintf "%s%d"
              (Ip.to_string ip) port
      in

(* ANONYMISATION of the source: we compute the Sha1 digest of the source,
  which cannot be recovered from this information *)
      let location = Md4.Sha1.string location in (* compute SHA1 of the string *)
      current := location :: !current;
  ) file.file_locations;

  buf_list (fun buf s ->
      Buffer.add_string buf (Md4.Sha1.direct_to_string s)
  ) buf !current;


  let file_history = "downloads.stats" in
  let oc = append_out file_history in
  output_string oc (Buffer.contents buf);
  close_out oc
*)
