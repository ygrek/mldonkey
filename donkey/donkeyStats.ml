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

open Printf2
open DonkeyComplexOptions
open Options
open DonkeyGlobals
open CommonOptions 
open CommonTypes
open CommonGlobals
open CommonNetwork
open DonkeyTypes
open GuiTypes
open CommonMessages
open BasicSocket (* last_time *)
open CommonInteractive


let brand_to_int b =
  match b with
    Brand_unknown -> 0
  | Brand_edonkey -> 1
  | Brand_mldonkey1 -> 2
  | Brand_mldonkey2 -> 3
  | Brand_overnet -> 4
  | Brand_newemule -> 5
  | Brand_server -> 6
  | Brand_mldonkey3 -> 7
  | Brand_cdonkey -> 8
      
let brand_of_int b =
  match b with
    0 -> Brand_unknown
  | 1 -> Brand_edonkey
  | 2 -> Brand_mldonkey1
  | 3 -> Brand_mldonkey2
  | 4 -> Brand_overnet
  | 5 -> Brand_newemule
  | 6 -> Brand_server
  | 7 -> Brand_mldonkey3
  | 8 -> Brand_cdonkey
  | _ -> raise Not_found
      
let gbrand_to_string b =
  match b with
    Brand_unknown -> "unk"
  | Brand_edonkey -> "eDK"
  | Brand_cdonkey -> "cDK"
  | Brand_mldonkey1 -> "oML"
  | Brand_mldonkey2 -> "nML"
  | Brand_mldonkey3 -> "tML"
  | Brand_overnet -> "OVR"
  | Brand_newemule -> "nEM"
  | Brand_server -> "SER"

let stats_all = dummy_stats 
let stats_by_brand = Array.init brand_count (fun _ ->
  { dummy_stats with brand_seen = 0 }
  )
  
let count_seen c =
  stats_all.brand_seen <- stats_all.brand_seen + 1;
  match c.client_brand with
      Brand_unknown -> () (* be careful, raising an exception here will
abort all other operations after that point for this client...*)
    | b ->
      stats_by_brand.(brand_to_int b).brand_seen <-
        stats_by_brand.(brand_to_int b).brand_seen + 1;
      !!gstats_by_brand.(brand_to_int b).brand_seen <-
        !!gstats_by_brand.(brand_to_int b).brand_seen + 1

let count_banned c =
  stats_all.brand_banned <- stats_all.brand_banned + 1;
  match c.client_brand with
      Brand_unknown -> () 
    | b ->
      stats_by_brand.(brand_to_int b).brand_banned <-
        stats_by_brand.(brand_to_int b).brand_banned + 1;
      !!gstats_by_brand.(brand_to_int b).brand_banned <-
        !!gstats_by_brand.(brand_to_int b).brand_banned + 1

let count_filerequest c =
  stats_all.brand_filerequest <- stats_all.brand_filerequest + 1;
  match c.client_brand with
      Brand_unknown -> ()
    | b ->
      stats_by_brand.(brand_to_int b).brand_filerequest <-
        stats_by_brand.(brand_to_int b).brand_filerequest + 1;
      !!gstats_by_brand.(brand_to_int b).brand_filerequest <-
	    !!gstats_by_brand.(brand_to_int b).brand_filerequest + 1

let count_download c f v =
  download_counter := Int64.add !download_counter v;
  c.client_downloaded <- Int64.add c.client_downloaded v;
  stats_all.brand_download <- Int64.add stats_all.brand_download v;
  match c.client_brand with
      Brand_unknown -> ()
    | b ->
      stats_by_brand.(brand_to_int b).brand_download <-
        Int64.add stats_by_brand.(brand_to_int b).brand_download v;
      !!gstats_by_brand.(brand_to_int b).brand_download <-
        Int64.add !!gstats_by_brand.(brand_to_int b).brand_download v

let count_upload c f v =
  upload_counter := Int64.add !upload_counter v;
  c.client_uploaded <- Int64.add c.client_uploaded v;
  stats_all.brand_upload <- Int64.add stats_all.brand_upload v;
  match c.client_brand with
      Brand_unknown -> failwith "unknown client type"
    | b ->
      stats_by_brand.(brand_to_int b).brand_upload <-
        Int64.add stats_by_brand.(brand_to_int b).brand_upload v;
      !!gstats_by_brand.(brand_to_int b).brand_upload <-
        Int64.add !!gstats_by_brand.(brand_to_int b).brand_upload v

let percent_of_ints x y = 
  if y <> 0 then 100. *. (float_of_int x /. float_of_int y)
  else 0.

let percent_of_int64s x y = 
  if y <> Int64.zero then 100. *. (Int64.to_float x /. Int64.to_float y)
  else 0.
      
let print_stats buf =
  let one_minute = 60 in
  let one_hour = 3600 in
  let one_day = 86400 in
  let uptime = last_time () - start_time in
  let days = uptime / one_day in
  let rem = uptime - days * one_day in
  let hours = rem / one_hour in
  let rem = rem - hours * one_hour in
  let mins = rem / one_minute in
    Printf.bprintf buf "Uptime: %d seconds (%d+%02d:%02d)\n" uptime days hours mins;


  if stats_all.brand_seen = 0 then
    Printf.bprintf buf "You haven't connected to any client yet\n"
  else begin
    Printf.bprintf buf "     Successful Connections: %18d\n" stats_all.brand_seen;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_seen 
	(percent_of_ints stats_by_brand.(i).brand_seen stats_all.brand_seen)
    done
  end;

  if stats_all.brand_filerequest = 0 then
    Printf.bprintf buf "You weren't asked for any file yet\n"
  else begin
    Printf.bprintf buf "Total filerequests received: %18d\n" stats_all.brand_filerequest;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i))
	stats_by_brand.(i).brand_filerequest 
	(percent_of_ints stats_by_brand.(i).brand_filerequest stats_all.brand_filerequest)
    done
  end;

  if stats_all.brand_download = Int64.zero then
    Printf.bprintf buf "You didn't download anything yet\n"
  else begin
      Printf.bprintf buf "            Total downloads: %18s (%5.1f KB/s)\n"
      (Int64.to_string stats_all.brand_download) 
      ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0);
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18s (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i))
	(Int64.to_string stats_by_brand.(i).brand_download) 
	(percent_of_int64s stats_by_brand.(i).brand_download stats_all.brand_download)
    done
  end;

  if stats_all.brand_upload = Int64.zero then
    Printf.bprintf buf "You didn't upload anything yet\n"
  else begin
      Printf.bprintf buf "              Total uploads: %18s (%5.1f KB/s)\n"
      (Int64.to_string stats_all.brand_upload)
      ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0);
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18s (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i))
	(Int64.to_string stats_by_brand.(i).brand_upload) 
	(percent_of_int64s stats_by_brand.(i).brand_upload stats_all.brand_upload)
    done
  end;
  
  if stats_all.brand_banned = 0 then
    Printf.bprintf buf "You didn't ban any client yet\n"
  else begin
      Printf.bprintf buf "              Total banneds: %18d\n" stats_all.brand_banned;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_banned 
	(percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)
    done
  end
  


let new_print_stats buf o =
  let one_minute = 60 in
  let one_hour = 3600 in
  let one_day = 86400 in
  let uptime = last_time () - start_time in
  let days = uptime / one_day in
  let rem = maxi 1 (uptime - days * one_day) in
  
  let hours = rem / one_hour in
  let rem = rem - hours * one_hour in
  let mins = rem / one_minute in
  
  
  if o.conn_output = HTML && !!html_mods then
    begin
      
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>Session Uptime: %d seconds (%d+%02d:%02d)\n" uptime days hours mins;
      
      Printf.bprintf buf "\\<table class=\\\"cs\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>
\\<td title=\\\"Client Brand\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>C.B\\</td\\>
\\<td class=\\\"srh\\\"\\>:\\</td\\>
\\<td title=\\\"Successful Connections\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>Seen\\</td\\>
\\<td title=\\\"Successful Connections Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"File Requests Received\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>Reqs\\</td\\>
\\<td title=\\\"File Requests Received Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Bans\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>B\\</td\\>
\\<td title=\\\"Total Bans Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Uploads\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>UL\\</td\\>
\\<td title=\\\"Total Uploads Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td title=\\\"Total Uploads Average KB/s\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>KB/s\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Downloads\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>DL\\</td\\>
\\<td title=\\\"Total Downloads Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh \\\"\\>%%\\</td\\>
\\<td title=\\\"Total Downloads Average KB/s\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>KB/s\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Uploads:Downloads Ratio\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>U:DL\\</td\\>
\\</tr\\>";
      
      let counter = ref 0 in
      
      for i=1 to brand_count-1 do
        
        if brand_of_int i != Brand_server then (* dont print server stats *)
          let brandstr = brand_to_string (brand_of_int i) in
          
          incr counter;
          if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
          else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";
          
          Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>
\\</tr\\>\n"
            
            (brandstr)
          stats_by_brand.(i).brand_seen 
            (percent_of_ints stats_by_brand.(i).brand_seen stats_all.brand_seen)
          
          stats_by_brand.(i).brand_filerequest 
            (percent_of_ints stats_by_brand.(i).brand_filerequest stats_all.brand_filerequest)

          stats_by_brand.(i).brand_banned 
            (max 0.0 (percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)) 

          (size_of_int64 stats_by_brand.(i).brand_upload) 
          (max 0.0 (percent_of_int64s stats_by_brand.(i).brand_upload stats_all.brand_upload))
          ((Int64.to_float stats_by_brand.(i).brand_upload) /. (float_of_int uptime) /. 1024.0)
          
          (size_of_int64 stats_by_brand.(i).brand_download)
          (max 0.0 (percent_of_int64s stats_by_brand.(i).brand_download stats_all.brand_download))
          ((Int64.to_float stats_by_brand.(i).brand_download) /. (float_of_int uptime) /. 1024.0)

          (if stats_by_brand.(i).brand_upload = Int64.zero then 0.0 else 
			( (Int64.to_float stats_by_brand.(i).brand_download) /.  (Int64.to_float stats_by_brand.(i).brand_upload) ))
          
      done;
      
      incr counter;
      if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
      else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";
      
      
      Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>
\\</tr\\>\\</table\\>\n"
        
        "Total"
        stats_all.brand_seen
        100.0
        stats_all.brand_filerequest
        100.0
        stats_all.brand_banned 
        (max 0.0 (percent_of_ints stats_all.brand_banned stats_all.brand_seen))

        (size_of_int64 stats_all.brand_upload) 
        100.0
        ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0)
        
        (size_of_int64 stats_all.brand_download) 
        100.0 
        ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0)

        (if stats_all.brand_upload = Int64.zero then 0.0 else 
	    	( (Int64.to_float stats_all.brand_download) /.  (Int64.to_float stats_all.brand_upload) ));
      
      let gstats_all = 
        let stat = {
            brand_seen = 0;
            brand_banned = 0;
            brand_filerequest = 0;
            brand_download = Int64.zero;
            brand_upload = Int64.zero
          }
        in stat in
      
      for i=0 to brand_count-1 do
        
        gstats_all.brand_seen <- gstats_all.brand_seen + !!gstats_by_brand.(i).brand_seen;
        gstats_all.brand_filerequest <- gstats_all.brand_filerequest + !!gstats_by_brand.(i).brand_filerequest;
        gstats_all.brand_download <- Int64.add gstats_all.brand_download !!gstats_by_brand.(i).brand_download ;
        gstats_all.brand_upload <- Int64.add gstats_all.brand_upload !!gstats_by_brand.(i).brand_upload;
        gstats_all.brand_banned <- gstats_all.brand_banned + !!gstats_by_brand.(i).brand_banned;
      
      done; 
      let gdays = (guptime () + uptime) / one_day in
      let grem = maxi 1 ((guptime () + uptime) - gdays * one_day) in
      
      let ghours = grem / one_hour in
      let grem = grem - ghours * one_hour in
      let gmins = grem / one_minute in
      
      Printf.bprintf buf "\\<br\\>\\<br\\>Total Uptime: %d seconds (%d+%02d:%02d)\n" (guptime() + uptime) gdays ghours gmins;
      
      Printf.bprintf buf "\\<table class=\\\"cs\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>
\\<td title=\\\"Client Brand\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>C.B\\</td\\>
\\<td class=\\\"srh\\\"\\>:\\</td\\>
\\<td title=\\\"Successful Connections\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>Seen\\</td\\>
\\<td title=\\\"Successful Connections Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"File Requests Received\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>Reqs\\</td\\>
\\<td title=\\\"File Requests Received Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Bans\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>B\\</td\\>
\\<td title=\\\"Total Bans Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Uploads\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>UL\\</td\\>
\\<td title=\\\"Total Uploads Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>%%\\</td\\>
\\<td title=\\\"Total Uploads Average KB/s\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>KB/s\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Downloads\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>DL\\</td\\>
\\<td title=\\\"Total Downloads Percent\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh \\\"\\>%%\\</td\\>
\\<td title=\\\"Total Downloads Average KB/s\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>KB/s\\</td\\>
\\<td class=\\\"srh\\\"\\>|\\</td\\>
\\<td title=\\\"Total Uploads:Downloads Ratio\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>U:DL\\</td\\>
\\</tr\\>";
      
      for i=1 to brand_count-1 do
        
        if brand_of_int i != Brand_server then (* dont print server stats *)
          let brandstr = brand_to_string (brand_of_int i) in
          
          incr counter;
          if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
          else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";
          
          
          Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>


\\</tr\\>\n"

          (brandstr)
          (!!gstats_by_brand.(i).brand_seen) 
          (percent_of_ints (!!gstats_by_brand.(i).brand_seen) gstats_all.brand_seen)
          
          (!!gstats_by_brand.(i).brand_filerequest)
          (percent_of_ints (!!gstats_by_brand.(i).brand_filerequest) gstats_all.brand_filerequest)
          
          (!!gstats_by_brand.(i).brand_banned) 
          (max 0.0 (percent_of_ints (!!gstats_by_brand.(i).brand_banned) gstats_all.brand_banned)) 

          (size_of_int64 (!!gstats_by_brand.(i).brand_upload))
          (max 0.0 (percent_of_int64s (!!gstats_by_brand.(i).brand_upload) gstats_all.brand_upload))
          ((Int64.to_float (!!gstats_by_brand.(i).brand_upload)) /. (float_of_int (guptime() + uptime)) /. 1024.0)

          (size_of_int64 (!!gstats_by_brand.(i).brand_download))
          (max 0.0 (percent_of_int64s (!!gstats_by_brand.(i).brand_download) gstats_all.brand_download))
          ((Int64.to_float (!!gstats_by_brand.(i).brand_download)) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          
          (if !!gstats_by_brand.(i).brand_upload = Int64.zero then 0.0 else 
			( (Int64.to_float !!gstats_by_brand.(i).brand_download) /.  (Int64.to_float !!gstats_by_brand.(i).brand_upload) ))


      done;

      incr counter;
      if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
      else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";
      
      Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>
\\</tr\\>\\</table\\>\\</div\\>\n"
        
        "Total"
        gstats_all.brand_seen
        100.0
        gstats_all.brand_filerequest
        100.0
        gstats_all.brand_banned 
        (max 0.0 (percent_of_ints gstats_all.brand_banned gstats_all.brand_seen))

        (size_of_int64 gstats_all.brand_upload) 
        100.0
        ((Int64.to_float gstats_all.brand_upload) /. (float_of_int (guptime() + uptime)) /. 1024.0)
        
        (size_of_int64 gstats_all.brand_download) 
        100.0 
        ((Int64.to_float gstats_all.brand_download) /. (float_of_int (guptime() + uptime)) /. 1024.0)
      
        (if gstats_all.brand_upload = Int64.zero then 0.0 else 
		( (Int64.to_float gstats_all.brand_download) /.  (Int64.to_float gstats_all.brand_upload) ))
    
    end
  else
    begin
      Printf.bprintf buf "Uptime: %d seconds (%d+%02d:%02d)\n" uptime days hours mins;
      Printf.bprintf buf "      Client| seen      |  Downloads       |  Uploads         |  Banned\n";
      Printf.bprintf buf "------------+-----------+------------------+------------------+----------\n";
      Printf.bprintf buf "%-12s|%6d     |%7.1f %5.1f     |%7.1f %5.1f     |%5d %3.0f%%\n"
        
        "Total"
        stats_all.brand_seen
        ((Int64.to_float stats_all.brand_download) /. 1024.0 /. 1024.0)
      ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0)
      ((Int64.to_float stats_all.brand_upload) /. 1024.0 /. 1024.0)
      ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0)
      stats_all.brand_banned 
        (percent_of_ints stats_all.brand_banned stats_all.brand_seen);
      
      for i=1 to brand_count-1 do
        if brand_of_int i != Brand_server then (* dont print server stats *)
          let brandstr = 
            if brand_of_int i = Brand_mldonkey3 then 
              "trusted mld"
            else
              brand_to_string (brand_of_int i) in
          
          Printf.bprintf buf "%-12s|%6d %3.f%%|%7.1f %5.1f %3.0f%%|%7.1f %5.1f %3.0f%%|%5d %3.0f%%\n"
            (brandstr)
          stats_by_brand.(i).brand_seen 
            (percent_of_ints stats_by_brand.(i).brand_seen stats_all.brand_seen)
          ((Int64.to_float stats_by_brand.(i).brand_download) /. 1024.0 /. 1024.0)
          ((Int64.to_float stats_by_brand.(i).brand_download) /. (float_of_int uptime) /. 1024.0)
          (percent_of_int64s stats_by_brand.(i).brand_download stats_all.brand_download)
          ((Int64.to_float stats_by_brand.(i).brand_upload) /. 1024.0 /. 1024.0)
          ((Int64.to_float stats_by_brand.(i).brand_upload) /. (float_of_int uptime) /. 1024.0)
          (percent_of_int64s stats_by_brand.(i).brand_upload stats_all.brand_upload)
          stats_by_brand.(i).brand_banned 
            (percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)
      done
    end

let _ =
  register_commands 
    [
    "client_stats", Arg_none (fun o ->
        let buf = o.conn_buf in
        print_stats buf;
        ""
    ), ":\t\t\t\tshow breakdown of download/upload by clients brand";
    
    "cs", Arg_none (fun o ->
        let buf = o.conn_buf in
        new_print_stats buf o;
        ""
    ), ":\t\t\t\t\tshow table of download/upload by clients brand";
    
    "reload_messages", Arg_none (fun o ->
        begin
          
          try
            Options.load message_file
          with
            Sys_error _ ->
              (try Options.save message_file with _ -> ())
          | e ->
              lprintf "Error %s loading message file %s"
                (Printexc2.to_string e)
              (Options.options_file_name message_file);
              lprint_newline ();
              lprintf "Using default messages."; lprint_newline ();
        
        end;
        ""
    ), ":\t\t\treload messages file";
  ]
  
