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

(* This module implements Printf like functions that take care of the
level of verbosity to choose when something should be printed. *)

open Printf

exception DEBUG_END
  
let debug_end = DEBUG_END

let rec find_dend arg = 
  if Obj.magic arg == debug_end then Obj.magic () else Obj.magic find_dend
  
let fprintf cond chan fmt =
  if cond then
    let fmt = (Obj.magic fmt : string) in
    let len = String.length fmt in
    let rec doprn i =
      if i >= len then 
        (output_char chan '\n'; flush chan; Obj.magic ()) else
        match String.unsafe_get fmt i with
      | '%' -> scan_format fmt i cont_s cont_a cont_t
      |  c  -> output_char chan c; doprn (succ i)
    and cont_s s i =
      output_string chan s; doprn i
    and cont_a printer arg i =
      output_string chan (printer arg); doprn i
    and cont_t printer i =
      printer chan; doprn i
    in doprn 0
  else 
    (Obj.magic find_dend)
    
let debug cond = fprintf cond stderr
