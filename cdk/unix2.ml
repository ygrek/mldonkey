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
open Unix
  
let list_directory filename =
  let dir = opendir filename in
  let list = ref [] in
  try
    while true do
      let file = readdir dir in 
      if file <> "." && file <> ".." then begin
          list := file :: !list 
        end;
    done;
    assert false
  with _ -> 
      closedir dir;
      !list

let is_directory filename =
  try let s = Unix.stat filename in s.st_kind = S_DIR with _ -> false

let is_link filename =
  try let s = Unix.lstat filename in s.st_kind = S_LNK with _ -> false
