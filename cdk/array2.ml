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

let min a =
  if a = [||] then raise Not_found;
  let min_val = ref a.(0) in
  for i = 1 to Array.length a - 1 do
    if a.(i) < !min_val then min_val := a.(i)
  done;
  !min_val
  
let max a =
  if a = [||] then raise Not_found;
  let max_val = ref a.(0) in
  for i = 1 to Array.length a - 1 do
    if a.(i) > !max_val then max_val := a.(i)
  done;
  !max_val
  