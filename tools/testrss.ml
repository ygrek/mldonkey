(*********************************************************************************)
(*                OCaml-RSS                                                      *)
(*                                                                               *)
(*    Copyright (C) 2003 Institut National de Recherche en Informatique et       *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as published by       *)
(*    the Free Software Foundation; either version 2 of the License, or          *)
(*    any later version.                                                         *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*********************************************************************************)

let main () =
  if Array.length Sys.argv < 2 then
    (
     Printf.eprintf "usage: %s <file to parse>\n" Sys.argv.(0);
     exit 1
    );
  try
    let ch = Rss.channel_of_file Sys.argv.(1) in
    Rss.print_channel Format.std_formatter ch;
    print_newline ()
  with
    Xml.Error e ->
      prerr_endline (Xml.error e);
      exit 1
  | Xml_dtd.Parse_error e ->
      prerr_endline (Xml_dtd.parse_error e);
      exit 2

let _ = main ()
