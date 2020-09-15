
open Printf2
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
  blocking_hits: int
}

type short_entry = {
    short_begin_high: int; (* 31..3 bits of IP address *)
    sort_end_high: int; (* 31..3 bits of IP address *)    
    mutable short_low_hits: int; (* 0..2 -> 0..2 bits of start address, 3..5 -> 0..2 bits of end address, 6..30 -> 0..24 bits of counter *)
    short_description: string;    
  }

let dummy_range = {
  short_description = unknown_description;
  short_begin_high = 0;
  sort_end_high = 0;
  short_low_hits = 0 }

let store_blocking_descriptions = ref true

module Array2 = struct
  type 'a t = {
      mutable len: int;
      mutable prev_len: int;
      mutable a: 'a array
    }
  let length a = a.len
  let set a n x = 
    if n < a.len then a.a.(n) <- x
    else invalid_arg "Array2.set"

  let get a n = 
    if n < a.len then a.a.(n) 
    else invalid_arg "Array2.get"

  let rec append a x = 
    if a.len < Array.length a.a then begin
      a.a.(a.len) <- x;
      a.len <- a.len + 1;
    end else begin
      let new_len = a.len + a.prev_len 
      and b = a.a.(0) in
      (* lprintf_nl (_b "append resize[0] %d to %d") a.len new_len; *)

      let t = Array.init new_len (fun n -> if n < a.len then a.a.(n) else b) in
      a.prev_len <- a.len;
      a.a <- t;
      append a x;
    end

  let iter f a =
    Array.iteri (fun n b -> if n < a.len then f b) a.a

  let empty x = {len= 0 ; prev_len=1; a = Array.make 0 x}

  let make n x = {len = n ; prev_len = n+1; a = Array.make (n+2) x} 

  let init n f  = {len = n ; prev_len = n+1; a = Array.init n f}

  let copy a = {len = a.len; prev_len = a.prev_len; a = Array.copy a.a}

  let sort cmp a = 
    if Array.length a.a != a.len then  a.a <- Array.init a.len (Array.get a.a);
    Array.sort cmp a.a

    

end



type compact = {
    (* 
       Instead of storing array of records, keep record of arrays. 
       This way it eleminates need of boxing, so each entry occupies 3 words (if no description needed, the desc array isn't allocated).
       Also on lookup only first array is accesed, unless hit is detected (better cache locality) and when hit detected, 
       dara updated only in single array. 
     *)
    compact_begin_high: int array;
    compact_end_high: int array; 
    compact_low_hits: int array;
    compact_desc: string array;
  }

(* either simple array or compact (non boxed) representation *)
type blocking_list = 
    Short of short_entry Array2.t
  | Compact of compact
 

let bl_empty  = Short (Array2.make 0 dummy_range)

let bl_length bl = match bl with
  Short  a -> Array2.length a
| Compact  a -> Array.length a.compact_begin_high

let ip_split_hi ip =  (* msb 30 bits *)
  ((Ip.get_hi16 ip) lsl 14) lor ((Ip.get_lo16 ip) lsr 2) 

let ip_split_lo ip = (* lsb 2 bits *)
  (Ip.get_lo16 ip) land 0x3 

let ip_combine hi lo = 
  Ip.of_ints ( (hi lsr 22), ((hi lsr 14) land 0xFF), ((hi lsr 6) land 0xFF), (((hi land 0x3F) lsl 2) lor lo))

let init_low_bits begin_lo end_lo hits =
  begin_lo lor (end_lo lsl 2) lor (hits lsl 4)

let begin_low_bits n = 
  n land 0x3

let end_low_bits n = 
  (n lsr 2) land 0x3 

let get_hits n  =
    n lsr 4
  
let succ_hits n = 
  let count = (get_hits n) + 1 in
  (n land 0xF) lor (count lsl 4)

let make_br ip_begin ip_end desc = 
  let begin_hi = ip_split_hi ip_begin
  and begin_lo = ip_split_lo ip_begin
  and end_hi = ip_split_hi ip_end
  and end_lo = ip_split_lo ip_end in
  {short_begin_high=begin_hi; 
   sort_end_high=end_hi;
   short_low_hits= init_low_bits begin_lo end_lo 0;
   short_description=desc}


let compare_split a_hi a_low b_hi b_low  = 
  let hicompare = Pervasives.compare a_hi b_hi in
  if hicompare <> 0 then 
    hicompare
  else
    Pervasives.compare a_low b_low

(* increment and then compare *)
let compare_split_next a_hi a_low b_hi b_low  = 
  if b_low < 3 then compare_split a_hi a_low b_hi (b_low + 1)
  else compare_split a_hi a_low (b_hi+1) 0



let match_ip_aux bl ip = 
  let ip_hi = ip_split_hi ip 
  and ip_lo = ip_split_lo ip in
  match bl with
    Short a ->
      let rec short_march_aux a ip_hi ip_lo n =
        if n < Array2.length a then 
          let br = Array2.get a n in
          if (compare_split ip_hi ip_lo br.short_begin_high (begin_low_bits br.short_low_hits) >= 0) &&
            (compare_split ip_hi ip_lo br.sort_end_high (end_low_bits br.short_low_hits) <= 0) then begin
              br.short_low_hits <- succ_hits br.short_low_hits;
              n
            end else short_march_aux a ip_hi ip_lo (n+1)
        else -1 in
      short_march_aux a ip_hi ip_lo 0
  | Compact a ->      
      let compare_begin a ip_hi ip_lo n =
        let cmp_hi = Pervasives.compare ip_hi (Array.get a.compact_begin_high n) in
        if cmp_hi <> 0 then cmp_hi
        else Pervasives.compare ip_lo (begin_low_bits (Array.get a.compact_low_hits n))
      and compare_end a ip_hi ip_lo n =
        let cmp_hi = Pervasives.compare ip_hi (Array.get a.compact_end_high n) in
        if cmp_hi <> 0 then cmp_hi
        else Pervasives.compare ip_lo (end_low_bits (Array.get a.compact_low_hits n)) 
      and mark_entry a n =
        Array.set a.compact_low_hits n (succ_hits (Array.get a.compact_low_hits n));
        n in

      let rec binary_search_aux a ip_hi ip_lo lo hi =
        if lo <= hi then 
          let n = (lo + hi) / 2 in
          let cmp = compare_begin a ip_hi ip_lo n in
          if cmp < 0 then
            binary_search_aux a ip_hi ip_lo lo (n-1)
          else if cmp > 0 then
            binary_search_aux a ip_hi ip_lo (n+1) hi
          else mark_entry a n	  
        else begin
          (* Printf.printf "%d %d\n" lo hi; *)
          if hi >= 0 && hi < Array.length a.compact_begin_high then begin
            let cmp = compare_begin a ip_hi ip_lo hi in
            if cmp > 0 then
              if (compare_end a ip_hi ip_lo hi) <= 0 then mark_entry a hi
              else -1
            else if cmp < 0 then -1
            else mark_entry a hi
          end else -1
        end in
      binary_search_aux a ip_hi ip_lo 0 ((Array.length a.compact_begin_high) - 1)

let make_range desc begin_high end_high low_hits = {
   blocking_description = desc;
   blocking_begin = ip_combine begin_high (begin_low_bits low_hits);
   blocking_end = ip_combine end_high (end_low_bits low_hits);
   blocking_hits = get_hits low_hits; } 

let compact_get_desc a n = 
  if Array.length a.compact_desc > n then Array.get a.compact_desc n else unknown_description

let match_blocking_range bl ip =
  let n = match_ip_aux bl ip in
  if n >= 0 then Some (
    match bl with
      Short a -> 
        let br = Array2.get a n in
        make_range br.short_description br.short_begin_high br.sort_end_high br.short_low_hits
    | Compact a ->
        make_range (compact_get_desc a n) (Array.get a.compact_begin_high n) (Array.get a.compact_end_high n) (Array.get a.compact_low_hits n))
  else None
          
        



let match_ip bl ip =
  match_ip_aux bl ip >= 0

let append_range bl br =
  match bl with
    Short a ->
      Array2.append a br
  | Compact _ ->
      assert(false)

let copy_range bl = 
  match bl with
    Short a ->
      let b = Array2.copy a in
      Short b
  | Compact c ->
      let b = Array2.init (Array.length c.compact_begin_high)
          (fun n -> 
            {
             short_begin_high = Array.get c.compact_begin_high n;
             sort_end_high = Array.get c.compact_end_high n;
             short_low_hits = Array.get c.compact_low_hits n;
             short_description = if Array.length c.compact_desc > n then Array.get c.compact_desc n  else unknown_description
           }) in
      Short b

let add_range bl ip_begin ip_end desc =
  let bl = copy_range bl 
  and br = make_br ip_begin ip_end desc in
  append_range bl br

let br_compare r1 r2 = 
  let cmp_begin = compare_split
      r1.short_begin_high  (begin_low_bits r1.short_low_hits) 
      r2.short_begin_high (begin_low_bits r2.short_low_hits) in
  if cmp_begin <> 0 then 
    cmp_begin
  else    
    compare_split 
      r1.sort_end_high  (end_low_bits r1.short_low_hits) 
      r2.sort_end_high (end_low_bits  r2.short_low_hits)

  let bl_optimize a = 
  match a with
    Short a ->     
      (* lprintf_nl (_b "sort %d") (Array2.length a); *)
      Array2.sort br_compare a;
      let rec bl_optimize_aux a rd wr last_hi last_lo  =
        if rd < Array2.length a then
          let br = Array2.get a rd
          and rd = rd + 1 in
          if (compare_split br.sort_end_high (end_low_bits br.short_low_hits) last_hi last_lo) > 0 then begin (* new record is going further then last *)
            if wr >= 0 && (compare_split_next br.short_begin_high (begin_low_bits br.short_low_hits) last_hi  last_lo) <= 0 then begin (* but it starts inside previous block, so concatenate them *)
              let last_hi = br.sort_end_high
              and last_lo = (end_low_bits br.short_low_hits) in
              let prev = Array2.get a wr  in
              Array2.set a wr {short_begin_high = prev.short_begin_high;
                               sort_end_high = last_hi;
                               short_low_hits = init_low_bits  (begin_low_bits prev.short_low_hits)  last_lo
                                 ((get_hits prev.short_low_hits) + (get_hits br.short_low_hits));
                               short_description = prev.short_description};
              bl_optimize_aux a rd wr last_hi last_lo
            end else begin (* there is nothing to optimize *)
              let wr = wr+1 in
              Array2.set a wr br;
              bl_optimize_aux a rd wr br.sort_end_high (end_low_bits br.short_low_hits)
            end 
          end else (* just ignore current record *)
            bl_optimize_aux a rd wr last_hi last_lo
        else
          wr+1 in
      let len = bl_optimize_aux a 0 (-1) 0 0 in	
      (* lprintf_nl (_b "copy %d") (Array2.length a); *)
      Compact {
      compact_begin_high = Array.init len (fun n -> (Array2.get a n).short_begin_high);
      compact_end_high = Array.init len (fun n -> (Array2.get a n).sort_end_high);
      compact_low_hits  = Array.init len (fun n -> (Array2.get a n).short_low_hits);
      compact_desc = 
      if !store_blocking_descriptions then
        Array.init len (fun n -> (Array2.get a n).short_description)
      else Array.make 0 unknown_description      
    }
  | Compact _ -> a
  
        

let load_merge bl filename remove =
  let guardian_regexp = Str.regexp "^\\(.*\\): *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)-\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" in
  let ipfilter_regexp = Str.regexp "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) *- *\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) *, *[0-9]+ *, *\\(.*\\)$" in

  let bl = copy_range bl in
  let nranges = ref 0 in
  let error = ref false in
  Unix2.tryopen_read filename (fun cin ->
  let nlines = ref 0 in
  let append line ip_begin ip_end desc = 
    let ip_begin = Ip.of_string (Str.matched_group ip_begin line) 
    and ip_end = Ip.of_string (Str.matched_group ip_end line)
    and desc = 
      if !store_blocking_descriptions then 
        shared_description (Str.matched_group desc line) 
      else 
        unknown_description in
    append_range bl (make_br ip_begin ip_end desc);
    incr nranges in  
  try
    while true do
      let line = input_line cin in
        incr nlines;
        try
          if Str.string_match ipfilter_regexp line 0 then
            append line 1 2 3
          else if Str.string_match guardian_regexp line 0 then 
            append line 2 3 1
          else 
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
  let optimized_bl = bl_optimize bl in
  lprintf_nl (_b "%d ranges loaded - optimized to %d") !nranges (bl_length optimized_bl);
  optimized_bl

let load filename =
  lprintf_nl (_b "loading %s") filename;
  if Sys.file_exists filename then
    let last_ext = String.lowercase (Filename2.last_extension filename) in
    if last_ext = ".zip" then
      let filenames_list =
        Unix2.tryopen_read_zip filename (fun ic ->
          try
            List.map (fun e -> e.Zip.filename) (Zip.entries ic)
          with _ -> []) in
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
                      let bl = load_merge bl_empty file.Zip.filename true in
                      if bl_length bl = 0 then
                        raise Not_found
                      else
                        bl
                    with Not_found ->
                      find_in_zip q in
            find_in_zip filenames_list
          with e ->
            lprintf_nl "Exception %s while extracting %s from %s"
              (Printexc2.to_string e) 
              (String.concat "/" filenames_list)
              filename;
            lprintf_nl "One of the mentioned files has to be a valid IP blocklist";
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
  let bl = copy_range  bl_empty in
  List.iter (fun r ->
    let range =
      match r with
      | Ip.RangeSingleIp ip -> 
          make_br ip ip unknown_description
      | Ip.RangeRange (ip1, ip2) -> 
          make_br ip1 ip2 unknown_description
      | Ip.RangeCIDR (ip, shift) ->
        let mask = Ip.mask_of_shift shift in
        make_br (Ip.network_address ip mask) (Ip.broadcast_address ip mask) unknown_description
    in 
    append_range bl range
  ) l;
  bl_optimize bl

let bl_fold_left f a bl = 
  let a' = ref a in
  let f desc begin_high end_high low_hits = 
    a' := f !a' (make_range desc begin_high end_high low_hits) in

  (match bl with
    Short a ->   
      Array2.iter (fun br -> 	
        if  get_hits br.short_low_hits > 0 then 
          f br.short_description br.short_begin_high br.sort_end_high br.short_low_hits;
                  ) a
  | Compact a ->
      Array.iteri (fun n low_hits ->
        if  get_hits low_hits > 0 then
          f (compact_get_desc a n)
            (Array.get a.compact_begin_high n) (Array.get a.compact_end_high n) low_hits
                  ) a.compact_low_hits);
  !a'


let print_list buf bl =
  let print_entry () br = 
    Printf.bprintf buf "%s (%d hits): %s - %s\n" 
      br.blocking_description
      br.blocking_hits
      (Ip.to_string br.blocking_begin)
      (Ip.to_string br.blocking_end) in
  bl_fold_left print_entry () bl;
  let nranges = bl_length bl in
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

