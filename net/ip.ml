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
type t =  int * int * int * int

external of_string : string -> t  = "ml_ints_of_string"
  
let of_inet_addr t = 
  of_string (Unix.string_of_inet_addr t)

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

let to_int64  ((a4, a3, a2, a1) as t) =
  let small = a1 + 256 * (a2 + 256 * a3) in
  Int64.add (Int64.of_int small) (Int64.shift_left (Int64.of_int a4) 24)

let const_int32_255 = Int64.of_int 255
  
let of_int64 i =
  let a4 = Int64.to_int (Int64.logand (Int64.shift_right i 24) const_int32_255)
  in
  let a3 = Int64.to_int (Int64.logand (Int64.shift_right i 16) const_int32_255)
  in
  let a2 = Int64.to_int (Int64.logand (Int64.shift_right i 8) const_int32_255)
  in
  let a1 = Int64.to_int (Int64.logand i const_int32_255)
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

let valid (j,_,_,i) = i != 0 && j != 0 && i != 255 && j < 224
  
let reachable ip = match ip with
    192, 168,_,_ -> false
  | 10, _, _, _ | 127, _,_,_ -> false
  | 172, v, _, _ when v > 15 && v < 32 -> false
  | _ -> true
  
  
let rec matches ((a4,a3,a2,a1) as a) ips =
  match ips with
    [] -> false
  | ((b4,b3,b2,b1) as b) :: tail ->
      ( (a4 = b4 || b4 = 255) &&
        (a3 = b3 || b3 = 255) &&
        (a2 = b2 || b2 = 255) &&
        (a1 = b1 || b1 = 255))
      || (matches a tail)
      
let localhost = of_string "127.0.0.1"
  
let to_sockaddr ip port =
  Unix.ADDR_INET (to_inet_addr ip, port)

let get_non_local_ip list =
  let rec iter list = 
    match list with
      [] -> raise Not_found
    | ip :: tail ->
        let ip = of_inet_addr ip in
        if ip = (127,0,0,1) then
          iter tail
        else ip
  in
  try iter list
  with _ -> match list with [] -> raise Not_found  | ip :: _ -> of_inet_addr ip
    
let gethostbyname name = 
  let h = Unix.gethostbyname name in
  let list = Array.to_list h.Unix.h_addr_list in
  get_non_local_ip list        

let ip_cache = Hashtbl.create 13
let resolve_name name =
  let current_time = Unix.gettimeofday () in
  try
    let (ip, time) = Hashtbl.find ip_cache name in
    if time < current_time then
      try
        Printf.printf "Resolving %s ..." name; flush stdout;
        let ip = gethostbyname name in
        Printf.printf "done"; print_newline ();
        Hashtbl.remove ip_cache name;
        Hashtbl.add ip_cache name (ip, current_time +. 3600.);
        ip
      with _ -> ip
    else ip
  with _ ->
      let ip = gethostbyname name in
      Hashtbl.add ip_cache name (ip, current_time +. 3600.);
      ip
      

let from_name name =
  if String.length name > 0 && name.[0] >= '0' && name.[0] <= '9' then
    of_string name 
  else  try
    let ip = resolve_name name in
    ip
  with _ -> 
      raise Not_found
      
let my () =
  try
    let name = Unix.gethostname () in
    try
      resolve_name name 
    with _ -> 
        if String.length name > 0 && name.[0] >= '0' && name.[0] <= '9' then
          of_string name 
        else
          localhost
  with _ -> localhost
      

open Options
        
    let value_to_ip v = of_string (value_to_string v)
      
    let ip_to_value ip = string_to_value (to_string ip)
      
let option = define_option_class "Ip" value_to_ip ip_to_value      
  
let any = of_inet_addr Unix.inet_addr_any
  
let null = of_string ""
  
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
      let current_time = Unix.gettimeofday () in
      let (ip, time) = Hashtbl.find ip_cache name in
      if time < current_time then begin
          Hashtbl.remove ip_cache name;
          raise Not_found
        end;
      (try f ip with _ -> ())
    with _ ->
        Fifo.put ip_fifo (name, f)
        
(* We check for names every 1/10 second. Too long ? *)
let _ = 
  BasicSocket.add_infinite_timer 0.1 (fun _ ->
      let current_time = Unix.gettimeofday () in
      while true do
        match !current_job with
          None -> 
            let (name, f) = Fifo.take ip_fifo in
            (try
                let (ip, time) = Hashtbl.find ip_cache name in
                if time < current_time then begin
                    Hashtbl.remove ip_cache name;
                    raise Not_found
                  end;
                (try f ip with _ -> ())
              with _ ->
                  if !BasicSocket.use_threads && 
                    BasicSocket.has_threads () then    
                    let job = {
                        handler = f;
                        name = name;
                        entries = [||];
                        error = false;
                      }
                    in 
                    current_job := Some job;
                    job_start job
                  else
                    f (from_name name)
            )
        | Some job ->
            if job_done job then begin
                current_job := None;
                if not job.error then begin
                    let ip = 
                      let list = Array.to_list job.entries in
                      get_non_local_ip list       
                    in
                    Printf.printf "Ip found for %s: %s"
                      job.name (to_string ip); print_newline ();
                    Hashtbl.add ip_cache job.name (ip, current_time +. 3600.);
                    job.handler ip
                  end else begin
                    Printf.printf "Error: %s: address not found" job.name;
                    print_newline ();
                  end
              end else raise Exit
      done
  )
  