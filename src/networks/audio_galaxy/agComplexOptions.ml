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

open CommonTypes
open CommonFile
open Options
open AgGlobals
open AgOptions
open AgTypes  

let value_to_file is_done assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_name = get_value "file_name" value_to_string in
  let file_id = 
    try
      Md4.of_string (get_value "file_id" value_to_string)
    with _ -> failwith "Bad file_id"
  in
  let file_size = try
      value_to_int32 (List.assoc "file_size" assocs) 
    with _ -> failwith "Bad file size"
  in
  
  let file = AgGlobals.new_file file_id file_name file_size in
  as_file file.file_file

let file_to_value file =
  [
    "file_id", string_to_value (Md4.to_string file.file_hash);
    "file_size", int32_to_value (file_size file);
    "file_name", string_to_value file.file_name;
    "file_downloaded", int32_to_value (file_downloaded file);
  ]
  
let old_files = 
  define_option audiogal_ini ["old_files"]
    "" (list_option (tuple2_option (string_option, int32_option))) []
        
let _ =
  network.op_network_add_file <- value_to_file;
  file_ops.op_file_to_option <- file_to_value
  