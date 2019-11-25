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
open Options
open CommonOptions
open CommonTypes
open CommonNetwork
open CommonStats

open BTTypes
open BTGlobals
open BTComplexOptions


let stats_array = Array.init brand_count (fun _ ->
    { dummy_stats with brand_seen = 0 }
  )

let count_seen c =
  let i = brand_to_int c.client_brand in
  check_client_country_code c;
  CommonStats.country_seen c.client_country_code;
  stats_array.(i).brand_seen <- stats_array.(i).brand_seen + 1;
  !!gstats_array.(i).brand_seen <- !!gstats_array.(i).brand_seen + 1

let count_banned c =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_banned <- stats_array.(i).brand_banned + 1;
  !!gstats_array.(i).brand_banned <- !!gstats_array.(i).brand_banned + 1

let count_filerequest c =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_filerequest <- stats_array.(i).brand_filerequest + 1;
  !!gstats_array.(i).brand_filerequest <- !!gstats_array.(i).brand_filerequest + 1 

let count_download c v =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_download <- stats_array.(i).brand_download ++ v;
  !!gstats_array.(i).brand_download <- !!gstats_array.(i).brand_download ++ v;

  c.client_total_downloaded <- c.client_total_downloaded ++ v;
  c.client_session_downloaded <- c.client_session_downloaded ++ v;
  bt_download_counter := !bt_download_counter ++ v;
  check_client_country_code c;
  global_count_download network c.client_country_code v

let count_upload c v =
  let i = brand_to_int c.client_brand in
  stats_array.(i).brand_upload <- stats_array.(i).brand_upload ++ v;
  !!gstats_array.(i).brand_upload <- !!gstats_array.(i).brand_upload ++ v;

  c.client_total_uploaded <- c.client_total_uploaded ++ v;
  c.client_session_uploaded <- c.client_session_uploaded ++ v;
  bt_upload_counter := !bt_upload_counter ++ v;
  check_client_country_code c;
  global_count_upload network c.client_country_code v

let print_stats o style = 
  let buf = o.conn_buf in

  let u1 = BasicSocket.last_time () - BasicSocket.start_time in
  let u2 = (guptime() + u1) in

  let t1 = build_title "BitTorrent" "Session" u1 in
  let t2 = build_title "BitTorrent" "Total" u2 in

  match style with
  | Old -> print_stats_old buf stats_array brand_list t1 u1
  | _ ->
        if use_html_mods o then begin
          print_stats_html_mods buf stats_array brand_list t1 u1;
          print_stats_html_mods buf !!gstats_array brand_list t2 u2
        end else begin
          print_stats_ascii buf stats_array brand_list t1 u1;
          print_stats_ascii buf !!gstats_array brand_list t2 u2
        end

let _ =
  network.op_network_display_stats <- (fun o -> print_stats o New);

  network.op_network_stat_info_list <- (fun _ ->
    let l1 = stats_list brand_list stats_array in
    let l2 = stats_list brand_list !!gstats_array in
    let u1 = BasicSocket.last_time () - BasicSocket.start_time in
    let u2 = (guptime() + u1) in
    [("Session clients", u1, l1); ("Global clients", u2, l2)]
  );

  register_commands
    [
    "client_stats_bt", "Network/Bittorrent",Arg_none (fun o ->
        print_stats o Old;
        ""
    ), ":\t\t\tshow breakdown of download/upload by clients brand";

    "csbt", "Network/Bittorrent",Arg_none (fun o ->
        print_stats o New;
        ""
    ), ":\t\t\t\t\tshow table of download/upload by BT clients brand";
  ]
