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
type 'a t = 'a hole_array array
and 'a hole_array =
  Hole 
| Array of 'a option array
  
let bucket_size = 16

let create n = Array.create (n / bucket_size) Hole

let not_found = Not_found
  
let set t n v =
  let array =
    match t.(n / bucket_size) with
      Hole -> 
        let array = Array.create bucket_size None in
        t.(n / bucket_size) <- Array array;
        array
    | Array array -> array
  in
  array.(n mod bucket_size) <- Some v
      
let get t n =
  match t.(n / bucket_size) with
    Hole -> raise not_found
  | Array array -> 
      match array.(n mod bucket_size) with
        None -> raise not_found
      | Some v -> v
  
  