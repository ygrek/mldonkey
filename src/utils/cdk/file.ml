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

(* read a whole file *)
let to_string name =
  Bytes.to_string (Unix2.tryopen_read_bin name (fun chan ->
  let buf_size = 1024 in
  let buf = String.create buf_size in
  let rec iter buf nb_read =
    let buf_size = Bytes.length buf in
    let to_read = min (buf_size - nb_read) 8192 in
    let tmp = input chan buf nb_read to_read in
    if tmp = 0 then
      Bytes.sub buf 0 nb_read
    else
    let nb_read = nb_read + tmp in
    let buf =
      if nb_read = buf_size then
        String2.resize_bytes buf (2 * buf_size)
      else buf
    in
    iter buf nb_read
  in
  iter buf 0))

let read_whole_chan chan =
  let buf = Buffer.create 1024 in
  let rec loop () =
    Buffer.add_char buf (input_char chan);
    loop ()
  in
  try
    loop ()
  with
    End_of_file -> close_in chan; buf

let to_string_alt name =
  let chan = open_in_bin name in
  read_whole_chan chan

let to_copy in_name out_name =
  Unix2.tryopen_read_bin in_name (fun in_chan ->
  Unix2.tryopen_write_bin out_name (fun out_chan ->
  try
    let rec rcpy () =
      let c = input_byte in_chan in
      output_byte out_chan c;
      flush out_chan;
      rcpy ()
    in
    rcpy ()
  with End_of_file -> ()))

let from_string name s =
  Unix2.tryopen_write_bin name (fun oc -> output_string oc s)

let iter f name =
  Unix2.tryopen_read_bin name (fun ic ->
    try
      while true do
        let line = input_line ic in
        f line
      done
    with End_of_file -> ())

let from_value name s =
  Unix2.tryopen_write_bin name (fun oc -> output_value oc s)

let to_value name =
  Unix2.tryopen_read_bin name (fun ic -> input_value ic)
