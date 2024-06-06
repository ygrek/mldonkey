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

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)

let rec octal x =
  if x < 8 then x else (x mod 10) + 8 * (octal (x / 10))

let int_of_octal_string s =
  let l = String.length s in
  let octal_of_char c =
    int_of_char c - int_of_char '0' in
  let rec octal_aux acc i =
    if i < l then octal_aux (acc * 8 + (octal_of_char s.[i])) (i+1)
    else acc in
  octal_aux 0 0

(* taken from http://kazaan.no-ip.com/~kazaan/item-1094206424.html *)
let dec2bin num len =
  let rec d2b v fig =
    if fig < 0 then ""
    else
      match (v asr fig) land 1 with
          0 -> "0" ^ (d2b v (fig - 1))
        | x -> "1" ^ (d2b v (fig - 1))
  in
    d2b num (len-1)

(* taken from http://www.hh.iij4u.or.jp/~kazaan/old/ObjectiveCaml.html *)
let bin2dec num =
  let s = string_of_int num in
    int_of_string ("0b" ^ s)

let percentage_of_ints v percent =
  int_of_float (
  (float_of_int v *. float_of_int percent /. 100.0) +. 0.5)

let zip_extract_entry ifile e =
  if e.Zip.is_directory then begin
    try
      Unix.mkdir e.Zip.filename 0o777
    with Unix.Unix_error(Unix.EEXIST, _, _) -> ()
  end else begin
    Zip.copy_entry_to_file ifile e e.Zip.filename
  end

let zip_extract zipfile =
  Unix2.tryopen_read_zip zipfile (fun ic ->
    List.iter (zip_extract_entry ic) (Zip.entries ic))

let rec zip_add_entry oc file =
  let module U = Unix.LargeFile in
  let s = U.stat file in
  match s.U.st_kind with
    Unix.S_REG ->
      Zip.copy_file_to_entry file oc ~mtime:s.U.st_mtime file
  | Unix.S_DIR ->
      Zip.add_entry "" oc ~mtime:s.U.st_mtime
        (if Filename.check_suffix file "/" then file else file ^ "/");
      Unix2.tryopen_dir file (fun d ->
        try
          while true do
            let e = Unix.readdir d in
            if e <> "." && e <> ".." then 
              zip_add_entry oc (Filename.concat file e)
          done
        with End_of_file -> ())
  | _ -> ()  

let zip_create zipfile files =
  Unix2.tryopen_write_zip zipfile (fun oc ->
    Array.iter (zip_add_entry oc) files)

let gz_extract filename =
  let file = ref "" in
  try
    let buffer = String.create 4096 in
    let file_out = Filename2.temp_file "arch_" ".tmp" in
    file := file_out;
    Unix2.tryopen_read_gzip filename (fun ic ->
      Unix2.tryopen_write_bin file_out (fun oc ->
        let rec decompress () =
          let n = Gzip.input ic buffer 0 (Bytes.length buffer) in
          if n = 0 then ()
          else
            begin
              output oc buffer 0 n;
              decompress()
            end
        in decompress()));
    file_out
  with e -> 
    (try if !file <> "" then Sys.remove !file with _ -> ()); 
    raise e

let archive_extract filename archive_type =
  match archive_type with
    "zip" -> zip_extract filename; ""
  | "bz2" -> Misc2.bz2_extract filename
  | "gz" -> gz_extract filename
  | _ -> failwith (Printf.sprintf "wrong archive type %s" archive_type)

let bytes_equal_string (b : Bytes.t) (s : string) =
  String.equal (Bytes.unsafe_to_string b) s
