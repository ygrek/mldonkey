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

external of_string : string -> t  = "ints_of_string"
  
open Int32ops

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

let to_int32  ((a4, a3, a2, a1) as t) =
  let small = a1 + 256 * (a2 + 256 * a3) in
  Int32.add (Int32.of_int small) (Int32.shift_left (Int32.of_int a4) 24)

let const_int32_255 = Int32.of_int 255
  
let of_int32 i =
  let a4 = Int32.to_int (Int32.logand (Int32.shift_right i 24) const_int32_255)
  in
  let a3 = Int32.to_int (Int32.logand (Int32.shift_right i 16) const_int32_255)
  in
  let a2 = Int32.to_int (Int32.logand (Int32.shift_right i 8) const_int32_255)
  in
  let a1 = Int32.to_int (Int32.logand i const_int32_255)
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

let rec get_non_local_ip list =
  match list with
    [] -> raise Not_found
  | ip :: tail ->
      let ip = of_inet_addr ip in
      if ip = (127,0,0,1) then
        get_non_local_ip tail
      else ip
  
let from_name name =
  if String.length name > 0 && name.[0] >= '0' && name.[0] <= '9' then
    of_string name 
  else  try
    Printf.printf "Resolving %s ..." name; flush stdout;
    let h = Unix.gethostbyname name in
    Printf.printf "done"; print_newline ();
    let list = Array.to_list h.Unix.h_addr_list in
    get_non_local_ip list
  with _ -> 
      raise Not_found
      
let my () =
  try
    let name = Unix.gethostname () in
    try
      let h = Unix.gethostbyname name in
      let list = Array.to_list h.Unix.h_addr_list in
      get_non_local_ip list
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
