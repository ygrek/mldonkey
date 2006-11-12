
open Printf2
open Ip
open Gettext

let _s x = _s "Ip_set" x
let _b x = _b "Ip_set" x

(* prints a new logline with date, module and starts newline *)
let log_prefix = "[IPblock]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

(* prints a new logline with date, module and does not start newline *)
let lprintf_n fmt =
  lprintf2 log_prefix fmt

module H = Weak.Make(struct
	type t = string
	let hash s = Hashtbl.hash s
	let equal x y = x = y
      end)

let descriptions = H.create 13

let shared_description s =
  (* Currently trims strings left and right;
     feel free to add other heuristics: convert to lowercase,
     remove punctuation, remove duplicate spaces,... *)
  let canonize s =
    let len = String.length s in
    let b = ref 0 in
    let e = ref len in
    while (!b < !e && s.[!b] = ' ') do incr b done;
    if !b < !e then
      while (s.[!e - 1] = ' ') do decr e done;
    if !b = 0 && !e = len then s
    else String.sub s !b (!e - !b)
    in
  H.merge descriptions (canonize s)

let unknown_description = shared_description "Unknown";

(* range name, ip min, ip max (inclusive) *)
type blocking_range = {
  blocking_description: string;
  blocking_begin: Ip.t;
  blocking_end: Ip.t;
  mutable blocking_hits: int
}

let dummy_range = {
  blocking_description = unknown_description;
  blocking_begin = Ip.null;
  blocking_end = Ip.null;
  blocking_hits = 0 }

let store_blocking_descriptions = ref true

(* Red-Black tree *)
type blocking_list =
    BL_Empty
  | BL_Range of bool * blocking_list * blocking_range * blocking_list

let bl_empty = BL_Empty

let ip_mini x y = if Ip.compare x y < 0 then x else y

let ip_maxi x y = if Ip.compare x y > 0 then x else y

let rec add_range bl br =
  blacken_root (ins bl br)

and ins bl br =
  match bl with
      BL_Empty -> BL_Range (true, BL_Empty, br, BL_Empty)
    | BL_Range (red, left, br2, right) ->
	if Ip.compare br.blocking_end br2.blocking_begin < 0 then
	  fixup (BL_Range (red, (ins left br), br2, right))
	else if Ip.compare br.blocking_begin br2.blocking_end > 0 then
	  fixup (BL_Range (red, left, br2, (ins right br)))
(* optimizer requires that ranges are sorted in increasing starting
   addresses *)
	else if Ip.compare br.blocking_begin br2.blocking_begin < 0 then 
	  fixup (BL_Range (red, (ins left br), br2, right))
	else
(* Be lazy. The optimizer will deal with it. *)
	  fixup (BL_Range (red, left, br2, (ins right br)))
(*	  let newr = {
	    blocking_description = br.blocking_description;
	    blocking_begin = ip_mini br.blocking_begin br2.blocking_begin;
	    blocking_end = ip_maxi br.blocking_end br2.blocking_end;
	    blocking_hits = br.blocking_hits + br2.blocking_hits } in
	  BL_Range (red, left, newr, right) *)

and fixup bl =
  match bl with
      BL_Range (false, a, x, BL_Range(true, b, y, BL_Range(true, c, z, d)))
    | BL_Range (false, BL_Range(true, a, x, BL_Range(true, b, y, c)), z, d)
    | BL_Range (false, BL_Range(true, BL_Range(true, a, x, b), y, c), z, d)
    | BL_Range (false, a, x, BL_Range(true, BL_Range(true, b, y, c), z, d))
      -> BL_Range(true, BL_Range(false, a, x, b), y, BL_Range(false, c, z, d))
    | _ -> bl

and blacken_root bl =
  match bl with
      BL_Empty -> BL_Empty
    | BL_Range(_, left, v, right) -> BL_Range(false, left, v, right)

let rec match_ip_aux bl ip =
  match bl with
      BL_Empty -> None
    | BL_Range (red, left, br, right) ->
	if Ip.compare ip br.blocking_begin < 0 then
	  match_ip_aux left ip
	else if Ip.compare ip br.blocking_end > 0 then
	  match_ip_aux right ip
	else
	  Some br

let match_ip bl ip =
  let m = match_ip_aux bl ip in
  (match m with
      Some br ->
	br.blocking_hits <- br.blocking_hits + 1
    | None -> ());
  m

let rec bl_fold_left f acc bl =
  match bl with
      BL_Empty -> acc
    | BL_Range (_, left, br, right) ->
	bl_fold_left f (f br (bl_fold_left f acc left)) right

let bl_optimize bl =
  let compact br (new_bl, pending_br) =
    match pending_br with
(* first range *)
	None -> (new_bl, Some br)
      | Some pbr ->
(* next range doesn't merge with pending one, commit pending range and
   go on with the next one *)
	  if Ip.compare br.blocking_begin (Ip.succ pbr.blocking_end) > 0 then
	    (add_range new_bl pbr, Some br)
	  else 
	    let hits_sum = br.blocking_hits + pbr.blocking_hits in
(* next range merge with pending one, does it extend it ? *)
	    if Ip.compare br.blocking_end pbr.blocking_end > 0 then
	    (new_bl, Some { pbr with blocking_end = br.blocking_end;
			    blocking_hits = hits_sum })
	  else 
(* no, it doesn't *)
	    (new_bl, Some { pbr with blocking_hits = hits_sum })
  in

(* start with no current range *)
  let new_bl, pending_br = bl_fold_left compact (bl_empty, None) bl in
(* finally, add the pending range *)
  match pending_br with
      None -> new_bl
    | Some pbr -> add_range new_bl pbr

let bl_length bl =
  bl_fold_left (fun br acc -> acc+1) 0 bl

let bl_optimizedp bl =
  let last_ip = ref None in
  let check br () =
    (match !last_ip with
	Some ip ->
	  assert(Ip.compare br.blocking_begin ip > 0)
      | None -> ());
    last_ip := Some (Ip.succ br.blocking_end)
  in bl_fold_left check () bl

let load_merge bl filename remove =
  let guardian_regexp = Str.regexp "^\\(.*\\): *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)-\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" in
  let ipfilter_regexp = Str.regexp "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) *- *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) *, *[0-9]+ *, *\\(.*\\)$" in

  let bl = ref bl in
  let nranges = ref 0 in
  let error = ref false in
  Unix2.tryopen_read filename (fun cin ->
  let nlines = ref 0 in
  try
    while true do
      let line = input_line cin in
        incr nlines;
	try
	  if Str.string_match ipfilter_regexp line 0 then begin
	    let br = {
	      blocking_description = if !store_blocking_descriptions then 
		shared_description (Str.matched_group 3 line) 
	      else 
		unknown_description;
	      blocking_begin = Ip.of_string (Str.matched_group 1 line);
	      blocking_end = Ip.of_string (Str.matched_group 2 line);
	      blocking_hits = 0 } in
	    bl := add_range !bl br;
	    incr nranges
	  end else 
	    if Str.string_match guardian_regexp line 0 then begin
	      let br = {
	        blocking_description = if !store_blocking_descriptions then 
		  shared_description (Str.matched_group 1 line) 
		else 
		  unknown_description;
	        blocking_begin = Ip.of_string (Str.matched_group 2 line);
	        blocking_end = Ip.of_string (Str.matched_group 3 line);
	        blocking_hits = 0 } in
	      bl := add_range !bl br;
	      incr nranges
	    end else 
	      raise Not_found
	with _ ->
	  if not !error then
	    begin
        lprintf_n "Syntax error while loading IP blocklist in line";
	      error := true
	    end;
	    lprintf " %d" !nlines;
    done
  with End_of_file -> ());
  if !error then lprint_newline ();
  if remove then (try Sys.remove filename with _ -> ());
  let optimized_bl = bl_optimize !bl in
  lprintf_nl (_b "%d ranges loaded - optimized to %d") !nranges (bl_length optimized_bl);
(*    bl_optimizedp optimized_bl;
    for i=0 to 999999 do
      let random_ip = Ip.of_ints (Random.int 256, Random.int 256, Random.int 256, Random.int 256) in
      match match_ip !bl random_ip, match_ip optimized_bl random_ip with
	  None, None 
	| Some _, Some _ -> ()
	| _ -> assert false
    done; *)
  optimized_bl

let load filename =
  lprintf_nl (_b "loading %s") filename;
  if Sys.file_exists filename then
    let last_ext = String.lowercase (Filename2.last_extension filename) in
    if last_ext = ".zip" then
      let filenames_list = 
	["guarding.p2p"; "guarding_full.p2p"; "ipfilter.dat"] in
      (try
	Unix2.tryopen_read_zip filename (fun ic ->
	  try
	    let rec find_in_zip l =
	      match l with
		| [] -> raise Not_found
		| h :: q ->
		    try
		      let file = Zip.find_entry ic h in
			lprintf_nl (_b "%s found in zip file") h;
		      ignore(Misc.archive_extract filename "zip");
		      load_merge bl_empty file.Zip.filename true
		    with Not_found ->
		      find_in_zip q in
	    find_in_zip filenames_list
	  with e ->
	    lprintf_nl "Exception %s while extracting %s from %s"
	      (Printexc2.to_string e) 
	      (String.concat "/" filenames_list)
	      filename;
	    lprintf_nl "One of the mentioned files has to be present in the zip file";
	    bl_empty)
      with e ->
	lprintf_nl "Exception %s while opening %s"
	  (Printexc2.to_string e)
	  filename;
	bl_empty)
    else     
      let ext = String.lowercase (Filename2.extension filename) in
      match ext with
	| ".bz2" | ".p2p.bz2" | ".dat.bz2" 
	| ".gz"  | ".p2p.gz"  | ".dat.gz" ->
	    let filetype =
	      if String2.check_suffix ext ".bz2" then "bz2" else "gz" in
	    (try
	      let s = Misc.archive_extract filename filetype in
	      load_merge bl_empty s true
	    with e ->
              lprintf_nl "Exception %s while extracting from %s"
		(Printexc2.to_string e) filename;
	      bl_empty)
        | ".tar.bz2" | ".p2p.tar.bz2" | ".dat.tar.bz2"
        | ".tar.gz" | ".p2p.tar.gz" | ".dat.tar.gz" ->
	    lprintf_nl "tar files are not (yet) supported, please untar %s" filename;
	    bl_empty
        | _ -> load_merge bl_empty filename false
  else
    begin
      lprintf_nl (_b "file %s not found") filename;
      bl_empty
    end

let of_list l =
  List.fold_left (fun acc r ->
    let range =
      match r with
      | Ip.RangeSingleIp ip -> {
	  dummy_range with
	  blocking_begin = ip;
	  blocking_end = ip }
      | Ip.RangeRange (ip1, ip2) -> {
	  dummy_range with
	  blocking_begin = ip1;
	  blocking_end = ip2 }
      | Ip.RangeCIDR (ip, shift) ->
	let mask = Ip.mask_of_shift shift in
	{ dummy_range with
	  blocking_begin = Ip.network_address ip mask;
	  blocking_end = Ip.broadcast_address ip mask }
    in 
    add_range acc range
  ) BL_Empty l

let print_list buf bl =
  let rec print_list_aux bl =
    match bl with
        BL_Empty -> 0
      | BL_Range (red, left, br, right) ->
          let nleft = print_list_aux left in
          if br.blocking_hits > 0 then
            Printf.bprintf buf "%s (%d hits): %s - %s\n" 
              br.blocking_description
              br.blocking_hits
              (Ip.to_string br.blocking_begin)
              (Ip.to_string br.blocking_end);
          let nright = print_list_aux right in
          nleft + 1 + nright in

  let nranges = print_list_aux bl in
  Printf.bprintf buf "%d ranges\n" nranges

(*
open Benchmark

let n = 10000

let ips = Array.init n (fun _ -> Ip.of_ints (Random.int 256, Random.int 256, Random.int 256, Random.int 256))

let check_ips ips =
  let blocked = ref 0 in
  for i=0 to n-1 do
    try
      match_ip !bl ips.(i)
    with MatchedIP s -> 
      incr blocked
  done;
    Printf2.lprintf "Blocked: %d\n" !blocked

let _ =
  latency1 10 check_ips ips;
  exit 0
*)
let _ =
  Heap.add_memstat "Ip_set" (fun level buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) descriptions;
      Printf.bprintf buf "  descriptions: %d\n" !counter)
