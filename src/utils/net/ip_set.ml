
open Printf2

exception MatchedIP of string

(* range name, ip min, ip max (inclusive) *)
type blocking_range = string * Ip.t * Ip.t 

(* Red-Black tree *)
type blocking_list =
    BL_Empty
  | BL_Range of bool * blocking_list * blocking_range * blocking_list

let bl_empty = BL_Empty

let ip_mini x y = if Ip.compare x y < 0 then x else y

let ip_maxi x y = if Ip.compare x y > 0 then x else y

let rec add_range bl br =
  blacken_root (ins bl br)

and ins bl ((s, ip_begin, ip_end) as br) =
  match bl with
      BL_Empty -> BL_Range (true, BL_Empty, br, BL_Empty)
    | BL_Range (red, left, ((s2, ip_begin2, ip_end2) as br2), right) ->
	if Ip.compare ip_end ip_begin2 < 0 then
	  fixup (BL_Range (red, (add_range left br), br2, right))
	else if Ip.compare ip_begin ip_end2 > 0 then
	  fixup (BL_Range (red, left, br2, (add_range right br)))
	else (* ok, comments aren't preserved. Beat me. *)
             (* left and right aren't simplified either *)
	  let newr = (s, ip_mini ip_begin ip_begin2, ip_maxi ip_end ip_end2) in
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

let rec match_ip bl ip =
  match bl with
      BL_Empty -> ()
    | BL_Range (red, left, (s, ip_begin, ip_end), right) ->
	if Ip.compare ip ip_begin < 0 then
	  match_ip left ip
	else if Ip.compare ip ip_end > 0 then
	  match_ip right ip
	else raise (MatchedIP s)

(* Example: http://homepage.ntlworld.com/tim.leonard1/guarding.p2p
   That module could be improved to declare a new resource kind, that
   could be fetched periodically... *)

let load filename =
  let guardian_regexp = Str.regexp "^\\(.*\\): *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)-\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" in

  let cin = open_in filename in
  let bl = ref bl_empty in
  let nranges = ref 0 in
  try
    while true do
      let line = input_line cin in
	try
	  if Str.string_match guardian_regexp line 0 then begin
	    let comment = Str.matched_group 1 line in
	    let ip_begin = Ip.of_string (Str.matched_group 2 line) in
	    let ip_end = Ip.of_string (Str.matched_group 3 line) in
	    bl := add_range !bl (comment, ip_begin, ip_end);
	    incr nranges
	  end else 
	    raise Not_found
	with _ ->
	  lprintf "Syntax error: %s" line;
	  lprint_newline ()
    done;
    bl_empty (* not reached *)
  with End_of_file ->
    close_in cin;
    lprintf "%d ranges loaded" !nranges;
    lprint_newline ();
    !bl

let of_list l =
  let rec of_list_aux l bl =
    match l with
	[] -> bl
      | h :: q ->
	  let range = match Ip.to_ints h with
(* only the most standard usages of the old syntax are supported *)
	      255, 255, 255, 255 -> ("", Ip.of_ints (0, 0, 0, 0), Ip.of_ints (255, 255, 255, 255))
	    | a, 255, 255, 255 -> ("", Ip.of_ints (a, 0, 0, 0), Ip.of_ints (a, 255, 255, 255))
	    | a, b, 255, 255 -> ("", Ip.of_ints (a, b, 0, 0), Ip.of_ints (a, b, 255,255))
	    | a, b, c, 255 -> ("", Ip.of_ints (a, b, c, 0), Ip.of_ints (a, b, c, 255))
	    | _ -> ("", h, h) in
	  of_list_aux q (add_range bl range) in
  of_list_aux l BL_Empty

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
