open CommonTypes
open CommonGlobals
open CommonNetwork
open DonkeyTypes
open GuiTypes
open BasicSocket (* last_time *)

let brand_count = 8

let brand_to_int b =
  match b with
      Brand_unknown -> 0
    | Brand_edonkey -> 1
    | Brand_mldonkey1 -> 2
    | Brand_mldonkey2 -> 3
    | Brand_overnet -> 4
    | Brand_oldemule -> 5
    | Brand_newemule -> 6
    | Brand_server -> 7

let brand_of_int b =
  match b with
      0 -> Brand_unknown
    | 1 -> Brand_edonkey
    | 2 -> Brand_mldonkey1
    | 3 -> Brand_mldonkey2
    | 4 -> Brand_overnet
    | 5 -> Brand_oldemule
    | 6 -> Brand_newemule
    | 7 -> Brand_server
    | _ -> raise Not_found

let brand_to_string b =
  match b with
      Brand_unknown -> "unknown"
    | Brand_edonkey -> "eDonkey"
    | Brand_mldonkey1 -> "old mldonkey"
    | Brand_mldonkey2 -> "new mldonkey"
    | Brand_overnet -> "Overnet"
    | Brand_oldemule -> "old eMule"
    | Brand_newemule -> "new eMule"
    | Brand_server -> "server"

type brand_stat = {
  mutable brand_seen : int;
  mutable brand_banned : int;
  mutable brand_filerequest : int;
  mutable brand_download : Int64.t;
  mutable brand_upload : Int64.t;
}
  
let dummy_stats =
  let stat = {
    brand_seen = 0;
    brand_banned = 0;
    brand_filerequest = 0;
    brand_download = Int64.zero;
    brand_upload = Int64.zero
  }
  in stat

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
	stats_by_brand.(brand_to_int b).brand_seen + 1

let count_banned c =
  stats_all.brand_banned <- stats_all.brand_banned + 1;
  match c.client_brand with
      Brand_unknown -> () 
    | b ->
	stats_by_brand.(brand_to_int b).brand_banned <-
	stats_by_brand.(brand_to_int b).brand_banned + 1

let count_filerequest c =
  stats_all.brand_filerequest <- stats_all.brand_filerequest + 1;
  match c.client_brand with
      Brand_unknown -> ()
    | b ->
	    stats_by_brand.(brand_to_int b).brand_filerequest <-
	    stats_by_brand.(brand_to_int b).brand_filerequest + 1

let count_download c f v =
  download_counter := Int64.add !download_counter v;
  c.client_downloaded <- Int64.add c.client_downloaded v;
  stats_all.brand_download <- Int64.add stats_all.brand_download v;
  match c.client_brand with
      Brand_unknown -> ()
    | b ->
	    stats_by_brand.(brand_to_int b).brand_download <-
	    Int64.add stats_by_brand.(brand_to_int b).brand_download v

let count_upload c f v =
  upload_counter := Int64.add !upload_counter v;
  c.client_uploaded <- Int64.add c.client_uploaded v;
  stats_all.brand_upload <- Int64.add stats_all.brand_upload v;
  match c.client_brand with
      Brand_unknown -> failwith "unknown client type"
    | b ->
	    stats_by_brand.(brand_to_int b).brand_upload <-
	    Int64.add stats_by_brand.(brand_to_int b).brand_upload v

let print_stats buf =
  let one_minute = 60 in
  let one_hour = 3600 in
  let one_day = 86400 in
  let uptime = int_of_float (last_time () -. boot_time) in
  let days = uptime / one_day in
  let rem = uptime - days * one_day in
  let hours = rem / one_hour in
  let rem = rem - hours * one_hour in
  let mins = rem / one_minute in
    Printf.bprintf buf "Uptime: %d seconds (%d+%02d:%02d)\n" uptime days hours mins;


  if stats_all.brand_seen = 0 then
    Printf.bprintf buf "You didn't see any client yet\n"
  else begin
    Printf.bprintf buf "                Total seens: %18d\n" stats_all.brand_seen;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%3.2f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_seen 
	(100. *. (float_of_int stats_by_brand.(i).brand_seen) /. (float_of_int stats_all.brand_seen))
    done
  end;

  if stats_all.brand_filerequest = 0 then
    Printf.bprintf buf "You weren't asked for any file yet\n"
  else begin
    Printf.bprintf buf "Total filerequests received: %18d\n" stats_all.brand_filerequest;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%3.2f %%)\n" 
	(brand_to_string (brand_of_int i))
	stats_by_brand.(i).brand_filerequest 
	(100. *. (float_of_int stats_by_brand.(i).brand_filerequest) /. (float_of_int stats_all.brand_filerequest))
    done
  end;

  if stats_all.brand_download = Int64.zero then
    Printf.bprintf buf "You didn't download anything yet\n"
  else begin
    Printf.bprintf buf "            Total downloads: %18s\n" (Int64.to_string stats_all.brand_download);
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18s (%3.2f %%)\n" 
	(brand_to_string (brand_of_int i))
	(Int64.to_string stats_by_brand.(i).brand_download) 
	(100. *. (Int64.to_float stats_by_brand.(i).brand_download) /. (Int64.to_float stats_all.brand_download))
    done
  end;

  if stats_all.brand_upload = Int64.zero then
    Printf.bprintf buf "You didn't upload anything yet\n"
  else begin
    Printf.bprintf buf "              Total uploads: %18s\n" (Int64.to_string stats_all.brand_upload);
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18s (%3.2f %%)\n" 
	(brand_to_string (brand_of_int i))
	(Int64.to_string stats_by_brand.(i).brand_upload) 
	(100. *. (Int64.to_float stats_by_brand.(i).brand_upload) /. (Int64.to_float stats_all.brand_upload))
    done
  end;
  
  if stats_all.brand_banned = 0 then
    Printf.bprintf buf "You didn't ban any client yet\n"
  else begin
    Printf.bprintf buf "                Total banneds: %18d\n" stats_all.brand_banned;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%3.2f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_banned 
	(100. *. (float_of_int stats_by_brand.(i).brand_banned) /. (float_of_int stats_all.brand_banned))
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
  ]
