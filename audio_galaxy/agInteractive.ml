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
open AgComplexOptions
open AgTypes
open AgGlobals
  
  (*
let print_downloads buf =
  Printf.bprintf  buf "Downloaded %d files from Audio-Galaxy\n"
    (List.length !!done_files);
  List.iter (fun file ->
      Printf.bprintf buf "[%5d] %50s %10s %10s\n" 
        file.file_num file.file_name 
        (Int32.to_string file.file_size)
      (Int32.to_string file.file_downloaded)
  ) !!done_files
  
    
let commit_downloads () = 
  done_files =:= []
    *)

module P = GuiTypes
  
let _ =
  file_ops.op_file_info <- (fun file ->
       {
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = "0";
        P.file_availability = "0";
        P.file_format = Unknown_format;
        P.file_chunks_age = [|0.0|];
        P.file_age = file_age file;
      }    
  )
  