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

let _ =
  let module P = GuiTypes in
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
      (try  Unix32.remove (file_fd file)  with e -> ());
      file_cancel (as_file file);
  );
  file_ops.op_file_info <- (fun file ->
      let chunks = Int64Swarmer.verified_bitmap file.file_verified_partition
      in
      let avail = Int64Swarmer.availability file.file_swarmer in
      
      let file_size = file_size file in
      let avail = 
        
(* I hope the cost of these operations is not too expensive for the core to
  compute for each file_info sent to the GUI. *)
        let n = String.length chunks in
        let global_avail = Array.create n 0 in
        List.iter (fun (_, avail) ->
            let nn = String.length avail in
            for i = 0 to n - 1 do
              global_avail.(i) <- global_avail.(i) + int_of_char avail.[i * nn / n]
            done;
        ) avail;
        let s = String.create n in
        for i = 0 to n-1 do
          let avail = global_avail.(i) in
          let avail = if avail > 200 then 200 else avail in
          s.[i] <- char_of_int avail
        done;
        
        (network.network_num, s) :: avail 
      in
      
      
      {
        P.file_name = file_best_name file;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = file.file_filenames;
        P.file_md4 = Md4.null;
        P.file_size = file_size;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = chunks;
        P.file_availability = avail;
        P.file_format = FormatNotComputed 0;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority  file;
        P.file_uids = file.file_uids;
      }    
  );
  file_ops.op_file_sources <- (fun file ->
      let list = ref [] in
      List.iter (fun (n,v) ->
          list := (n.op_download_sources v) @ !list) file.file_impls;
      !list
  );
  file_ops.op_file_save_as <- (fun file name ->
      file.file_filenames <- [name];
      set_file_best_name file name
  );      
  file_ops.op_file_recover <- (fun file ->
      if file_state file = FileDownloading then
        List.iter (fun (n,v) ->
            n.op_download_recover v) file.file_impls;
  );
  file_ops.op_file_pause <- (fun file ->
      List.iter (fun (n,v) -> n.op_download_pause v) file.file_impls
  );
  file_ops.op_file_resume <- (fun file ->
      List.iter (fun (n,v) -> n.op_download_resume v) file.file_impls
  );
  file_ops.op_file_debug <- (fun file ->
      let buf = Buffer.create 100 in
      Int64Swarmer.debug_print buf file.file_swarmer;
      List.iter (fun (n,v) ->
          try n.op_download_debug v buf with _ -> ()
      ) file.file_impls;
      Buffer.contents buf
  );
  file_ops.op_file_commit <- (fun file new_name ->
      try
        if file.file_files <> [] then 
          let old_file = new_name ^ ".torrent" in
          Sys.rename new_name old_file;
          let old_fd = Unix32.create_ro old_file in
          List.iter (fun (filename, begin_pos, end_pos) ->
              let filename = Filename.concat new_name filename in
              lprintf "Would save file as %s\n" filename;
              let dirname = Filename.dirname filename in
              Unix2.safe_mkdir dirname;
              lprintf "Copying %Ld %Ld to 0\n"
                begin_pos (end_pos -- begin_pos);
              let fd = Unix32.create
                  filename [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
              Unix32.copy_chunk old_fd fd begin_pos zero (end_pos -- begin_pos);
              Unix32.close fd
          ) file.file_files;
          Unix32.close old_fd;
          if !!delete_original then Sys.remove old_file
      with e ->
          lprintf "Exception %s while commiting BitTorrent file"
            (Printexc.to_string e)
  ) 

let _ =
  network.op_network_file_of_option <- MultinetComplexOptions.value_to_file;
  file_ops.op_file_to_option <- MultinetComplexOptions.file_to_value
  