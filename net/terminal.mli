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

(*
type command =
  | Message of string
  | Command of string

 val init : unit -> unit
 val set_prompt : string -> unit 
 val set_header : string list -> unit  (* the first lines of the terminal *)
 val set_trailer : string list -> unit      (* the last line before the entry point *)
 val set_reader : (command -> unit) -> unit 
   (* param: what has been typed, return: what should remain *)
val print : string -> unit
val update : unit -> unit
    *)

module ANSI :
  sig
    val esc : string
    val esc_CHAR : char
    val ansi_CLRSCR : string
    val ansi_CLREOL : string
    val ansi_NORMAL : string
    val ansi_BOLD : string
    val ansi_BLINK : string
    val ansi_UNDERLINE : string
    val ansi_REVERSE : string
    val ansi_HIGH_REVERSE : string
    val ansi_BLACK : string
    val ansi_RED : string
    val ansi_GREEN : string
    val ansi_YELLOW : string
    val ansi_BLUE : string
    val ansi_MAGENTA : string
    val ansi_CYAN : string
    val ansi_WHITE : string
    val ansi_HIGH_RED : string
    val ansi_HIGH_GREEN : string
    val ansi_HIGH_YELLOW : string
    val ansi_HIGH_BLUE : string
    val ansi_HIGH_MAGENTA : string
    val ansi_HIGH_CYAN : string
    val ansi_HIGH_WHITE : string
    val ansi_BACKGROUND_BLACK : string
    val ansi_BACKGROUND_RED : string
    val ansi_BACKGROUND_GREEN : string
    val ansi_BACKGROUND_YELLOW : string
    val ansi_BACKGROUND_BLUE : string
    val ansi_BACKGROUND_MAGENTA : string
    val ansi_BACKGROUND_CYAN : string
    val ansi_BACKGROUND_WHITE : string
    val ansi_HIGH_BACKGROUND_RED : string
    val ansi_HIGH_BACKGROUND_GREEN : string
    val ansi_HIGH_BACKGROUND_YELLOW : string
    val ansi_HIGH_BACKGROUND_BLUE : string
    val ansi_HIGH_BACKGROUND_MAGENTA : string
    val ansi_HIGH_BACKGROUND_CYAN : string
    val ansi_HIGH_BACKGROUND_WHITE : string
  end

val size : Unix.file_descr -> int * int
val gotoxy : int -> int -> string