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
open Options
open BasicSocket (* last_time *)
open GuiTypes
open CommonOptions 
open CommonTypes
open CommonGlobals
open CommonNetwork
open CommonMessages
open CommonInteractive
open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions


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
  | Brand_lmule -> 9
  | Brand_shareaza -> 10
      
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
  | 9 -> Brand_lmule
  | 10 -> Brand_shareaza
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
  | Brand_newemule -> "eMU"
  | Brand_lmule -> "lMU"
  | Brand_shareaza -> "sZA"
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
    Printf.bprintf buf "\n     Successful Connections: %18d\n" stats_all.brand_seen;
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
    Printf.bprintf buf "\nTotal filerequests received: %18d\n" stats_all.brand_filerequest;
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
      Printf.bprintf buf "\n            Total downloads: %18s (%5.1f KB/s)\n"
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
      Printf.bprintf buf "\n              Total uploads: %18s (%5.1f KB/s)\n"
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
      Printf.bprintf buf "\n                 Total bans: %18d\n" stats_all.brand_banned;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_banned 
	(percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)
    done
  end
  

let stats_html_header buf = 
  html_mods_table_header buf "csTable" "cs" [ 
   ( "0", "srh", "Client brand", "Brand" ) ; 
   ( "0", "srh", "Separator", ":" ) ; 
   ( "1", "srh ar", "Successful connections", "Seen" ) ; 
   ( "1", "srh", "Successful connections percent", "%" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "File requests received", "Reqs" ) ; 
   ( "1", "srh", "File requests received percent", "%" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "Total bans", "B" ) ; 
   ( "1", "srh", "Total bans percent", "%" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "Total uploads", "UL" ) ; 
   ( "1", "srh", "Total uploads percent", "%" ) ; 
   ( "1", "srh ar", "Total uploads average KB/s", "KB/s" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "Total downloads", "DL" ) ; 
   ( "1", "srh", "Total downloads percent", "%" ) ; 
   ( "1", "srh ar", "Total downloads average KB/s", "KB/s" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh", "Total uploads:downloads ratio", "U:DL" ) ]

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
  
  if use_html_mods o then
    begin
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>Session Uptime: %d seconds (%d+%02d:%02d)\\</div\\>" uptime days hours mins;
      stats_html_header buf;
      
      let counter = ref 0 in
      let showTotal = ref false in

      for i=1 to brand_count do
        if i=brand_count then showTotal := true;
       if i != 6 then begin (* dont print server stats - avoid the raise_failure of brand_of_int *)
          incr counter;
          Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>" (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
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
            
            (if !showTotal then "Total" else (brand_to_string (brand_of_int i))) 
            
            (if !showTotal then stats_all.brand_seen else
                    stats_by_brand.(i).brand_seen) 
                    
            (if !showTotal then 100.0 else (percent_of_ints
            stats_by_brand.(i).brand_seen stats_all.brand_seen))
          
            (if !showTotal then stats_all.brand_filerequest else stats_by_brand.(i).brand_filerequest)

            (if !showTotal then 100.0 else (percent_of_ints stats_by_brand.(i).brand_filerequest stats_all.brand_filerequest))

            (if !showTotal then stats_all.brand_banned else
                    stats_by_brand.(i).brand_banned)
                    
            (max 0.0 (if !showTotal then (percent_of_ints stats_all.brand_banned stats_all.brand_seen) 
             else (percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)))

            (size_of_int64 (if !showTotal then stats_all.brand_upload else
                    stats_by_brand.(i).brand_upload)) 
          
            (max 0.0 (if !showTotal then 100.0 else (percent_of_int64s
            stats_by_brand.(i).brand_upload stats_all.brand_upload)))

            (if !showTotal then ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0)
            else ((Int64.to_float stats_by_brand.(i).brand_upload) /.  (float_of_int uptime) /. 1024.0))
          
            (size_of_int64 (if !showTotal then stats_all.brand_download else
                    stats_by_brand.(i).brand_download)) 

            (max 0.0 (if !showTotal then 100.0 else (percent_of_int64s
            stats_by_brand.(i).brand_download stats_all.brand_download)))

            (if !showTotal then ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0)
            else ((Int64.to_float stats_by_brand.(i).brand_download) /.  (float_of_int uptime) /. 1024.0))

            (if !showTotal then 
             (if stats_all.brand_upload = Int64.zero then 0.0 else 
	    	  ( (Int64.to_float stats_all.brand_download) /.  (Int64.to_float stats_all.brand_upload) ))
            else 
             (if stats_by_brand.(i).brand_upload = Int64.zero then 0.0 else 
			 ( (Int64.to_float stats_by_brand.(i).brand_download) /.
             (Int64.to_float stats_by_brand.(i).brand_upload) )));
        end
      done;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
      
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
      
      Printf.bprintf buf "\n\\<div class=\\\"cs\\\"\\>Total Uptime: %d seconds (%d+%02d:%02d)\\</div\\>" (guptime() + uptime) gdays ghours gmins;
      stats_html_header buf;
      
      showTotal := false;
      for i=1 to brand_count do
       if i=brand_count then showTotal := true;
       if i != 6 then begin (* dont print server stats - avoid the raise_failure of brand_of_int *)
          incr counter;
          Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>" (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
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

          (if !showTotal then "Total" else (brand_to_string (brand_of_int i)) )

          (if !showTotal then gstats_all.brand_seen else !!gstats_by_brand.(i).brand_seen) 

          (if !showTotal then 100. else (percent_of_ints (!!gstats_by_brand.(i).brand_seen) gstats_all.brand_seen))
          
          (if !showTotal then gstats_all.brand_filerequest else !!gstats_by_brand.(i).brand_filerequest)

          (if !showTotal then 100. else (percent_of_ints (!!gstats_by_brand.(i).brand_filerequest) gstats_all.brand_filerequest))
          
          (if !showTotal then gstats_all.brand_banned else !!gstats_by_brand.(i).brand_banned) 

          (max 0.0 (if !showTotal then (percent_of_ints gstats_all.brand_banned
          gstats_all.brand_seen) else (percent_of_ints (!!gstats_by_brand.(i).brand_banned) gstats_all.brand_banned)))

          (size_of_int64 (if !showTotal then gstats_all.brand_upload else !!gstats_by_brand.(i).brand_upload))
          
          (if !showTotal then 100. else (max 0.0 (percent_of_int64s
          (!!gstats_by_brand.(i).brand_upload) gstats_all.brand_upload)))

          (if !showTotal then ((Int64.to_float gstats_all.brand_upload) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          else ((Int64.to_float (!!gstats_by_brand.(i).brand_upload)) /.  (float_of_int (guptime() + uptime)) /. 1024.0))

          (size_of_int64 (if !showTotal then gstats_all.brand_download else !!gstats_by_brand.(i).brand_download))
          
          (if !showTotal then 100. else (max 0.0 (percent_of_int64s
          (!!gstats_by_brand.(i).brand_download) gstats_all.brand_download)))

          (if !showTotal then ((Int64.to_float gstats_all.brand_download) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          else ((Int64.to_float (!!gstats_by_brand.(i).brand_download)) /.  (float_of_int (guptime() + uptime)) /. 1024.0))

         (if !showTotal then (if gstats_all.brand_upload = Int64.zero then 0.0 else 
		  ((Int64.to_float gstats_all.brand_download) /.  (Int64.to_float gstats_all.brand_upload) )) else  
          (if !!gstats_by_brand.(i).brand_upload = Int64.zero then 0.0 else 
			( (Int64.to_float !!gstats_by_brand.(i).brand_download) /.
            (Int64.to_float !!gstats_by_brand.(i).brand_upload) )))
        end
      done;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
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
  ]
  
