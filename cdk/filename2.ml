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

open String2
  
let normalize filename =
  let l = split filename '/' in
  let is_absolute = match l with
      "" :: _ -> true
    | _ -> false
  in
  let rec iter l =
    match l with
      [] -> [], false
    | "" :: l -> iter l
    | "." :: l -> iter l
    | ".." :: l -> let l,_ = iter l in ("..":: l), false
    | x :: ".." :: l -> 
        Printf.printf "remove .."; print_newline ();
        let l,_ = iter l in l, true
    | x :: l -> 
        let l, redo = iter l in if redo then iter (x :: l) else (x :: l), false
  in
  let l, _ = iter l in
  let l = 
    if is_absolute then
      let rec iter_abs l =
        match l with
          ".." :: l -> iter_abs l
        | _ -> l
      in
      "" :: (iter_abs l)
    else l
  in
  match l with
    [] -> "."
  | [""] -> "/"
  | _ -> unsplit l '/'
;;

let rec dirname name =
  let name = normalize name in
  try
    match String.rindex name '/' with
      0 -> "/"
    | n -> String.sub name 0 n
  with Not_found -> "."

      
let last_extension file =
  try
    let pos = String.rindex file '.' in
    let pos2 = try String.rindex file '/' with _ -> 0 in 
    if pos < pos2 then raise Not_found;
    let len = String.length file in
    String.sub file pos (len -pos)
  with _ -> ""

let extension file =
  try
    let pos2 = try String.rindex file '/' with _ -> 0 in 
    let pos = String.index_from file pos2 '.' in
    let len = String.length file in
    String.sub file pos (len -pos)
  with _ -> ""
      
let extensions file =
  let ext = extension file in
  let len = String.length ext in
  if len > 0 then
    String2.split_simplify (String.sub ext 1 (len-1)) '.'
  else []
      
let from_strings = ref []
let to_strings = ref []

let register_conversions from_string to_string =
  from_strings := from_string :: !from_strings;
  to_strings := to_string :: !to_strings

let from_string filename =
  List.fold_left (fun file f -> f file) filename !from_strings
  
let to_string filename =
  List.fold_left (fun file f -> f file) filename !to_strings
