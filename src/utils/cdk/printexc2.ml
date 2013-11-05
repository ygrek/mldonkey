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

open Printf2
open Printf

let to_string = function
  | Unix.Unix_error (e, f, arg) -> sprintf "%s failed%s: %s" f (if arg = "" then "" else " on " ^ arg) (Unix.error_message e)
  | exn -> Printexc.to_string exn

let catch s f x =
  try f x with
    e -> 
      lprintf_nl "Uncaught exception in %s: %s" s (to_string e)

let catch2 s f x y =
  try f x y with
    e -> 
      lprintf_nl "Uncaught exception in %s: %s" s (to_string e)
