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

external of_string : string -> t
  = "ints_of_string"
open Int32ops

let of_inet_addr t = 
  of_string (Unix.string_of_inet_addr t)

let of_ints t = t
let to_int32 (a4, a3, a2, a1 ) =
  let i1 = Int32.of_int (a1 + 256 * (a2 + 256 * a3)) in
  i1 +. (Int32.of_int a4)

let to_int t = Int32.to_int (to_int32 t)
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

let valid (j,_,_,i) = i != 0 && j != 0

      