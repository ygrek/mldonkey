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

open Md4
open Printf2
open Options
open BasicSocket
open TcpBufferedSocket

open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open CommonGlobals
open CommonOptions
open CommonSwarming

open MultinetTypes
open MultinetGlobals
open MultinetBitzi
open MultinetFunctions

(* This should be a per-network command... For example, the BitTorrent
URN should contain both the tracker and the info, so that the
tracker can be different for two URNs, but the info still the same. *)

open GuiTypes  
let _ =
  let commands = [
      
      "debug_download", Arg_none (fun o ->
          let buf = o.conn_buf in
          List.iter (fun file ->
              Int64Swarmer.debug_print buf file.file_swarmer  
          ) !current_files;
          lprintf "DEBUG: \n%s\n" (Buffer.contents buf);
          ""
      ), " : print info on downloads";
      
      "add_uid", Arg_multiple (fun args o ->
          let buf = o.conn_buf in
          Printf.bprintf buf "nargs\n";
          match args with
            file_num :: ( (_ :: _) as tail) ->
              Printf.bprintf buf "file_num: %s\n" file_num;
              let file_num = int_of_string file_num in
              Printf.bprintf buf "file_num: %d\n" file_num;
              let file = Hashtbl.find files_by_num file_num in
              Printf.bprintf buf "file found\n";
              List.iter (fun urn ->
                  Printf.bprintf buf "urn: %s\n" urn;
                  let uids = 
                    let (header, _) = String2.cut_at urn ':' in
                    match String.lowercase header with
                      "urn" -> [uid_of_string urn]
                    | "ed2k" -> []
                    | "magnet" ->
                        let (_,uids) = parse_magnet urn in
                        uids
                    | "sig2dat" -> []
                    | _ -> failwith "Unknown URN kind"
                  in
                  if uids = [] then
                    Printf.bprintf buf "Not yet implemented\n";
                  add_uids file uids
              ) tail;
              Printf.bprintf buf "done\n";
              List.iter (fun f -> 
                  try f.op_download_start file with _ -> ()) !networks;
              "URNs added"
          | _ -> "Not enough arguments"
      ), " <file_num> <urns>: add URNs to a given downloads:\n
  Correct URNs are of the form:\n
urn:ed2k:ed2k_in_hexa\n
urn:bitprint:sha1_in_hexa.tiger_in_base32\n
urn:sha1:sha1_in_hexa\n
urn:sig2dat:sig2dat_in_base32\n
or URLS (ed2k://, magnet?, sig2dat://).
          ";
      
      
      "recover_temp", Arg_none (fun o ->
          let buf = o.conn_buf in
          let files = Unix2.list_directory !!temp_directory in
          List.iter (fun filename ->
              try
                let file_size = Unix32.getsize64 (Filename.concat 
                      !!temp_directory filename) in
                let len = String.length filename in
                let uid =
                  if String2.starts_with filename "COMMON-" then
                    let uid = String.sub filename 7 (len-7) in
                    uid_of_string uid
                  else if String.length filename = 32 then
                    uid_of_uid (Ed2k ("",Md4.of_string filename))
                  else raise Not_found
                in
                let file_shared = 
                  new_download None [string_of_uid uid] file_size 
                    (Some filename) []
                in
                lprintf "File %s recovered to %s" filename 
                  (file_disk_name file_shared)
              with e ->
                  lprintf "exception %s in recover_temp\n"
                    (Printexc2.to_string e);
          ) files;
          "done"
      ), ":\t\t\t\trecover lost files from temp directory";
    
    
    ] in
  
  CommonNetwork.register_commands commands;
  ()
  