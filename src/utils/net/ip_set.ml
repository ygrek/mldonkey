
open Printf2
open Ip

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[IPblock] "
  (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[IPblock] "
  (log_time ()); lprintf

(* range name, ip min, ip max (inclusive) *)
type blocking_range = {
  blocking_description: string;
  blocking_begin: Ip.t;
  blocking_end: Ip.t;
  mutable blocking_hits: int
}

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
	else (* ok, comments aren't preserved. Beat me. *)
             (* left and right aren't simplified either *)
	  let newr = {
	    blocking_description = br.blocking_description;
	    blocking_begin = ip_mini br.blocking_begin br2.blocking_begin;
	    blocking_end = ip_maxi br.blocking_end br2.blocking_end;
	    blocking_hits = 0 } in
	  BL_Range (red, left, newr, right)

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
  let m = if Ip.local_ip ip then None else
  match_ip_aux bl ip in
  (match m with
      Some br ->
	br.blocking_hits <- br.blocking_hits + 1
    | None -> ());
  m

let load_merge bl filename remove =
  lprintf_nl () "creating block table from %s" filename;
  let guardian_regexp = Str.regexp "^\\(.*\\): *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)-\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" in
  let ipfilter_regexp = Str.regexp "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) *- *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) *, *[0-9]+ *, *\\(.*\\)$" in

  let cin = open_in filename in
  let bl = ref bl in
  let nranges = ref 0 in
  let nlines = ref 0 in
  let error = ref false in
  try
    while true do
      let line = input_line cin in
        incr nlines;
	try
	  if Str.string_match ipfilter_regexp line 0 then begin
	    let br = {
	      blocking_description = Str.matched_group 3 line;
	      blocking_begin = Ip.of_string (Str.matched_group 1 line);
	      blocking_end = Ip.of_string (Str.matched_group 2 line);
	      blocking_hits = 0 } in
	    bl := add_range !bl br;
	    incr nranges
	  end else 
	    if Str.string_match guardian_regexp line 0 then begin
	      let br = {
	        blocking_description = Str.matched_group 1 line;
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
	      lprintf_n () "Syntax error while loading IP blocklist in line";
	      error := true
	    end;
	    lprintf " %d" !nlines;
    done;
    bl_empty (* not reached *)
  with End_of_file ->
    if !error then lprint_newline ();
    close_in cin;
    if remove then (try Sys.remove filename with _ -> ());
    lprintf_nl () "%d ranges loaded" !nranges;
    !bl

let load filename =
   lprintf_nl () "loading %s" filename;
   if Sys.file_exists filename then
   let ext = String.lowercase (Filename2.extension filename) in
    let last_ext = String.lowercase (Filename2.last_extension filename) in
    let real_ext = if last_ext = ".zip" then
      last_ext
    else
      ext
    in
      match real_ext with
        ".zip" ->
	   begin
	   try
	     let ic = Zip.open_in filename in
	     try
	       let file = Zip.find_entry ic "guarding.p2p" in
	         Zip.close_in ic;
		 lprintf_nl () "guarding.p2p found in zip file";
		 let s = Misc.archive_extract filename "zip" in
		 load_merge bl_empty file.Zip.filename true
	       with e ->
		 begin
		   try
		     let file = Zip.find_entry ic "guarding_full.p2p" in
		       Zip.close_in ic;
		       lprintf_nl () "guarding_full.p2p found in zip file";
		       let s = Misc.archive_extract filename "zip" in
		       load_merge bl_empty file.Zip.filename true
		     with e ->
		       Zip.close_in ic;
		       lprintf_nl () "Exception %s while extracting guarding.p2p/guarding_full.p2p from %s"
			 (Printexc2.to_string e) filename;
		       lprintf_nl () "One of the mentioned files has to be present in the zip file";
		       bl_empty
		 end
	     with e ->
	       lprintf_nl () "Exception %s while opening %s"
		 (Printexc2.to_string e) filename;
	       bl_empty
	   end
       | ".bz2" | ".p2p.bz2" | ".gz" | ".p2p.gz" ->
	   begin
	     let filetype =
	       if ext = ".bz2" || ext = ".p2p.bz2" then "bz2" else "gz" in
	     try
		 let s = Misc.archive_extract filename filetype in
		   load_merge bl_empty s true
	       with e ->
		 lprintf_nl () "Exception %s while extracting from %s"
		   (Printexc2.to_string e) filename;
		   bl_empty
	   end
       | ".tar.bz2" | ".p2p.tar.bz2" | ".tar.gz" | ".p2p.tar.gz" ->
	    lprintf_nl () "tar files are not (yet) supported, please untar %s" filename;
	    bl_empty
       | _ -> load_merge bl_empty filename false
   else
     begin
       lprintf_nl () "file %s not found" filename;
       bl_empty
     end

let of_list l =
  let rec of_list_aux l bl =
    match l with
	[] -> bl
      | h :: q ->
	  let range = match Ip.to_ints h with
(* only the most standard usages of the old syntax are supported *)
	      255, 255, 255, 255 -> 
		{ blocking_description = "";
		  blocking_begin = Ip.of_ints (0, 0, 0, 0);
		  blocking_end = Ip.of_ints (255, 255, 255, 255);
		  blocking_hits = 0 }
	    | a, 255, 255, 255 -> 
		{ blocking_description = "";
		  blocking_begin = Ip.of_ints (a, 0, 0, 0);
		  blocking_end = Ip.of_ints (a, 255, 255, 255);
		  blocking_hits = 0 }
	    | a, b, 255, 255 -> 
		{ blocking_description = "";
		  blocking_begin = Ip.of_ints (a, b, 0, 0);
		  blocking_end = Ip.of_ints (a, b, 255,255);
		  blocking_hits = 0 }
	    | a, b, c, 255 -> 
		{ blocking_description = "";
		  blocking_begin = Ip.of_ints (a, b, c, 0);
		  blocking_end = Ip.of_ints (a, b, c, 255);
		  blocking_hits = 0 }
	    | _ -> 
		{ blocking_description = "";
		  blocking_begin = h;
		  blocking_end = h;
		  blocking_hits = 0 } in
	  of_list_aux q (add_range bl range) in
  of_list_aux l BL_Empty

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

  let count = print_list_aux bl in
  Printf.bprintf buf "%d ranges\n" count

let bl = ref BL_Empty

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
    Printf.printf "Blocked: %d\n" !blocked

let _ =
  latency1 10 check_ips ips;
  exit 0
*)
