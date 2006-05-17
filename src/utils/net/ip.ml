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
type t =  int * int * int * int

external of_string : string -> t  = "ml_ints_of_string"

let allow_local_network = ref false

let of_inet_addr t =
  of_string (Unix.string_of_inet_addr t)

let any = of_inet_addr Unix.inet_addr_any

let null = (0,0,0,0)

let of_ints t = t

let to_ints t = t
let to_string (a4, a3, a2, a1) =
  Printf.sprintf "%d.%d.%d.%d" a4 a3 a2 a1

let to_inet_addr t =
  Unix.inet_addr_of_string (to_string t)

let hostname_table = Hashtbl.create 997

let to_fixed_string ((a4, a3, a2, a1) as t)=
  try
    Hashtbl.find hostname_table t
  with _ ->
      Printf.sprintf "%03d.%03d.%03d.%03d" a4 a3 a2 a1

let to_int64 (a4, a3, a2, a1) =
  let small = a1 lor (a2 lsl 8) lor (a3 lsl 16) in
  (Int64.of_int small) ++ (Int64.shift_left (Int64.of_int a4) 24)

let of_int64 i =
  let a4 = Int64.to_int (Int64.logand (Int64.shift_right i 24) 0xffL)
  in
  let a3 = Int64.to_int (Int64.logand (Int64.shift_right i 16) 0xffL)
  in
  let a2 = Int64.to_int (Int64.logand (Int64.shift_right i 8) 0xffL)
  in
  let a1 = Int64.to_int (Int64.logand i 0xffL)
  in
  (a4, a3, a2, a1)

let resolve_one t =
  try
    Hashtbl.find hostname_table t
  with _ ->
      let addr = to_inet_addr t in
      begin
        try
          let h = Unix.gethostbyaddr addr in
          let name = h.Unix.h_name in
          if name <> "" then
            Hashtbl.add hostname_table t name
        with _ -> ()
      end;
      to_fixed_string t

let valid (j,k,l,i) =
  j > 0 && j < 224 &&
  k >= 0 && k <= 255 &&
  l >= 0 && l <= 255 &&
  i >= 0 && i <= 255

let local_ip ip =
  match ip with
    192, 168,_,_ -> true
  | 10, _, _, _ | 127, _,_,_ -> true
  | 172, v, _, _ when v > 15 && v < 32 -> true
  | _ -> false

let reachable ip =
  !allow_local_network || not (local_ip ip)

let usable ip =
  reachable ip && valid ip

let rec matches ((a4,a3,a2,a1) as a) ips =
  match ips with
    [] -> false
  | (b4,b3,b2,b1) :: tail ->
      ( (a4 = b4 || b4 = 255) &&
        (a3 = b3 || b3 = 255) &&
        (a2 = b2 || b2 = 255) &&
        (a1 = b1 || b1 = 255))
      || (matches a tail)

let compare (a4,a3,a2,a1) (b4,b3,b2,b1) =
  let c4 = compare a4 b4 in
  if c4 <> 0 then c4 else
  let c3 = compare a3 b3 in
  if c3 <> 0 then c3 else
  let c2 = compare a2 b2 in
  if c2 <> 0 then c2 else
  compare a1 b1

let succ (a4,a3,a2,a1) =
  if a1 < 255 then
    (a4,a3,a2,a1+1)
  else if a2 < 255 then
    (a4,a3,a2+1,0)
  else if a3 < 255 then
    (a4,a3+1,0,0)
  else if a4 < 255 then
    (a4+1,0,0,0)
  else
    (0,0,0,0) (* or exception ? *)

let pred (a4,a3,a2,a1) =
  if a1 > 0 then
    (a4,a3,a2,a1-1)
  else if a2 > 0 then
    (a4,a3,a2-1,255)
  else if a3 > 0 then
    (a4,a3-1,255,255)
  else if a4 > 0 then
    (a4-1,255,255,255)
  else
    (255,255,255,255) (* or exception ? *)

let banned = ref (fun (ip:t) -> None)

let localhost = of_string "127.0.0.1"

let to_sockaddr ip port =
  Unix.ADDR_INET (to_inet_addr ip, port)

let get_non_local_ip list =
  let list = List.filter ((<>) (127,0,0,1)) list in
  match list with
  | [] -> raise Not_found
  | l -> l

let gethostbyname name = 
  let h = Unix.gethostbyname name in
  let list = Array.to_list h.Unix.h_addr_list in
  get_non_local_ip (List.map of_inet_addr list)

type ip_cache_entry = {
  ips : t array;
  mutable next : int; (* for DNS round robin *)
  time_limit : float (* freshness *)
}

let ip_cache = Hashtbl.create 13

let make_entry_from_ips ?(time_limit = Unix.gettimeofday () +. 3600.) ips =
  {
    ips = ips;
    next = Random.int (Array.length ips);
    time_limit = time_limit 
  }

let make_entry_from_name ?(time_limit = Unix.gettimeofday () +. 3600.) name =
  make_entry_from_ips ~time_limit (Array.of_list (gethostbyname name))

let get_entry_cached_immediate name =
  let cache_entry = Hashtbl.find ip_cache name in
  let current_time = Unix.gettimeofday () in
  if cache_entry.time_limit < current_time then begin
    (* found, but no longer fresh *)
    Hashtbl.remove ip_cache name;
    raise Not_found
  end else
    cache_entry

let get_entry_cached name =
  let current_time = Unix.gettimeofday () in
  try
    let cache_entry = Hashtbl.find ip_cache name in
    if cache_entry.time_limit < current_time then
      (* found, but no longer fresh *)
      try
	let new_entry = 
	  make_entry_from_name ~time_limit:(current_time +. 3600.) name in
	(* update cache *)
        Hashtbl.replace ip_cache name new_entry;
	new_entry
      with Not_found -> 
	(* new lookup failed, return old information *)
	cache_entry
    else 
      (* fresh from cache *)
      cache_entry
  with Not_found ->
    (* not in cache *)
    lprintf_nl "[DNS] Resolving [%s] ..." name;
    let new_entry =
      make_entry_from_name ~time_limit:(current_time +. 3600.) name in
    Hashtbl.add ip_cache name new_entry;
    new_entry

(* Simple round robin *)
let get_entry_ip entry =
  let ip = entry.ips.(entry.next) in
  entry.next <- (entry.next + 1) mod (Array.length entry.ips);
  ip

let resolve_name_immediate name =
  get_entry_ip (get_entry_cached_immediate name)

let resolve_name name =
  get_entry_ip (get_entry_cached name)

let from_name name =
  try
    if String.length name > 0 && name.[0] >= '0' && name.[0] <= '9' then
      of_string name
    else
      raise Not_found
  with _ ->
      try
(*        lprintf "resolve_name...%s\n" name; *)
        let ip = resolve_name name in
(*        lprintf "..name resolved\n";  *)
        ip
      with _ ->
          raise Not_found

let my () =
  try
    let name = Unix.gethostname () in
    try
      resolve_name name 
    with Not_found -> 
        if String.length name > 0 && name.[0] >= '0' && name.[0] <= '9' then
          of_string name 
        else
          localhost
  with _ -> localhost


open Options

    let value_to_ip v = of_string (value_to_string v)
      
    let ip_to_value ip = string_to_value (to_string ip)

let option = define_option_class "Ip" value_to_ip ip_to_value

let rev (a1,a2,a3,a4) = (a4,a3,a2,a1)

let equal a b = 
  let (a1,a2,a3,a4) = a in
  let (b1,b2,b3,b4) = b in
    ( a1=b1 &&  a2=b2 &&  a3=b3 && a4=b4)

type job = {
    name : string;
    mutable entries : Unix.inet_addr array;
    mutable error : bool;
    handler : (t -> unit);
  }


external job_done : job -> bool = "ml_ip_job_done"
external job_start : job -> unit = "ml_ip_job_start"

let current_job = ref None
let ip_fifo = Fifo.create ()

let async_ip name f =
  try
(*    lprintf "async_ip [%s]\n" name; *)
    let ip = resolve_name_immediate name in
    (try f ip with _ -> ())
  with Not_found ->
    Fifo.put ip_fifo (name, f)

(* We check for names every 1/10 second. Too long ? *)
let _ =
  Heap.add_memstat "Ip" (fun level buf ->
      Printf.bprintf buf "  %d IPs in ip_cache\n" (Hashtbl.length ip_cache);
      Printf.bprintf buf "  %d entries in hostname_table\n" (Hashtbl.length hostname_table);
      Printf.bprintf buf "  %d entries in ip_fifo\n" (Fifo.length ip_fifo);
  );

  BasicSocket.add_infinite_timer 0.1 (fun _ ->
    (* let current_time = Unix.gettimeofday () in *)
    while true do
      match !current_job with
      | None ->
          let (name, f) = Fifo.take ip_fifo in
          (try
	    let ip = resolve_name_immediate name in
            (try f ip with _ -> ())
          with Not_found ->
(*                  lprintf "resolving name...\n"; *)
            if !BasicSocket.use_threads && 
              BasicSocket.has_threads () then
                let job = {
                  handler = f;
                  name = name;
                  entries = [||];
                  error = false;
                } in
                current_job := Some job;
                job_start job
            else begin
(*                      lprintf "from_name ...\n"; *)
              f (from_name name)
		
            end
          )
      | Some job ->
          if job_done job then begin
            current_job := None;
            if not job.error then begin
	      let ips =
		get_non_local_ip (
		  List.map of_inet_addr (Array.to_list job.entries)) in
	      let entry = make_entry_from_ips (Array.of_list ips) in
              Hashtbl.add ip_cache job.name entry;
	      let ip = get_entry_ip entry in
(*        lprintf "Ip found for %s: %s\n" job.name (to_string ip);  *)
              job.handler ip
            end else begin
              lprintf_nl "[DNS] could not resolve %s, check URL" job.name;
              raise Not_found
            end
          end else raise Exit
    done
  )


type addr =
  AddrIp of t
| AddrName of string

let string_of_addr ip =
  match ip with
    AddrIp ip -> to_string ip
  | AddrName name -> name

let addr_of_string s =
  try AddrIp (of_string s) with _ -> AddrName s

let addr_of_ip ip = AddrIp ip
let ip_of_addr addr =
  match addr with
    AddrIp ip -> ip
  | AddrName name -> from_name name

let async_ip_of_addr addr f =
  match addr with
    AddrIp ip -> f ip
  | AddrName name -> async_ip name f


let value_to_addr v = addr_of_string (value_to_string v)

let addr_to_value ip = string_to_value (string_of_addr ip)

let addr_option = define_option_class "Addr" value_to_addr addr_to_value
