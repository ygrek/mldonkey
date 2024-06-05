(* Copyright 2004 b8_bavard, INRIA *)
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


open Filename2
open Zlib

let load_svg file =
  Printf.printf "Converting file %s\n" file;
  flush stdout;
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let buf = String.create len in
  really_input ic buf 0 len;
  close_in ic;
  let bufz = Zlib2.compress_string buf in
  let basename = basename file in
  let extension = last_extension basename in
  let dirname = String.sub file 0 (String.length file - String.length basename) in
  let len = String.length basename - String.length extension in
  let name = String.sub basename 0 len in
  let oc = open_out_bin (dirname ^ name ^ "_svg.ml") in
  output_string oc (Printf.sprintf "let t =" );
  output_string oc (Printf.sprintf "%S" (Bytes.to_string bufz));
  close_out oc

let _ =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage : %s <file>\n" Sys.argv.(0);
    exit 2;
  end;
  load_svg Sys.argv.(1)
