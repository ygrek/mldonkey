(* Copyright 2001, Maxence Guesdon, INRIA Rocquencourt, FRANCE *)
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
(***********************************************************************)
(*                               Mp3tag                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Various useful functions. *)


(** Return the given string without the blanks from the given pos. *)
let chop_whitespace str from =
  let i = ref (String.length str) in
  while !i > from &&
        (let c = str.[!i-1] in c = '\000' || c = ' ' || c = '\t')
  do decr i done;
  String.sub str from (!i - from)

let string_of_genre n = 
  try List.assoc n Mp3_genres.genres
  with Not_found -> ""

let genre_of_string g = 
  try List.assoc g Mp3_genres.inv_genres
  with Not_found -> 255
