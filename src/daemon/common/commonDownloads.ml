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
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonOptions
  
module SimpleDownload  = struct
  
(*
A common function for all networks were the file is got in one single piece,
  from only one client  and the connection is closed at the end.
*)
    
    type ('file,'client) download = {
        download_file : 'file; (* the file being downloaded *)
        download_client : 'client;
        mutable download_min_read : int;
        mutable download_pos : int64; (* the position in the file *)
        mutable download_sock : TcpBufferedSocket.t option;
      }
    
    module Make(M: sig
          type f
          type c
          val file : f -> file
          val client :   c -> client
          val client_disconnected :  (f, c) download -> unit
          val subdir_option : string Options.option_record
          val download_finished :  (f, c) download -> unit
        end) = 
      struct
        
        let disconnect_download (d : (M.f, M.c) download) reason =
          match d.download_sock with
            None -> ()
          | Some sock ->
              close sock reason;
              (try M.client_disconnected d with _ -> ());
              lprintf "DISCONNECTED FROM SOURCE\n"; 
              d.download_sock <- None
        
        let file_complete d =
(*
  lprintf "FILE %s DOWNLOADED\n" f.file_name;
  *)
          file_completed (M.file d.download_file);
          (try M.download_finished d with _ -> ())
        
        let download_reader d sock nread = 
          lprint_string ".";
          if d.download_sock = None then  raise Exit;
          let file = M.file d.download_file in
          if nread >= d.download_min_read then
            let b = TcpBufferedSocket.buf sock in
            d.download_min_read <- 1;
            set_rtimeout sock 120.;
            set_client_state (M.client d.download_client) 
            (Connected_downloading);
(*
        begin
          let fd = try
              Unix32.force_fd (file_fd file) 
            with e -> 
                lprintf "In Unix32.force_fd\n"; 
                raise e
          in
          let final_pos = Unix32.seek64 (file_fd file) d.download_pos
              Unix.SEEK_SET in *)
            Unix32.write (file_fd file) d.download_pos b.buf b.pos b.len;
(*        end; *)
(*      lprintf "DIFF %d/%d\n" nread b.len; *)
            d.download_pos <- Int64.add d.download_pos (Int64.of_int b.len);
(*
      lprintf "NEW SOURCE POS %s\n" (Int64.to_string c.client_pos);
  *)
            TcpBufferedSocket.buf_used sock b.len;
            if d.download_pos > file_downloaded file then 
              add_file_downloaded (as_file_impl file)
              (Int64.sub d.download_pos (file_downloaded file));
            if file_downloaded file = file_size file then
              file_complete d
        
        
        let new_download sock (c :M.c) (file : M.f) min_read =
          let d = {
              download_client = c;
              download_file = file;
              download_sock = Some sock;
              download_pos = file_downloaded (M.file file);
              download_min_read = min_read;
            } in
          set_closer sock (fun _ reason -> disconnect_download d reason);
          TcpBufferedSocket.set_read_controler sock download_control;
          TcpBufferedSocket.set_write_controler sock upload_control;
          set_rtimeout sock 30.;
          d  
      
      end
  
  end


open Md4
open CommonSwarming
  
module SharedDownload = struct

(* This module defines a file that can be downloaded from several network
simultaneously. Thus, it defines a superset of the operations that are
possible on files in all networks.

Currently, it is used by the Fasttrack, Gnutella and BitTorrent plugins.

Each plugin must provide the two sets of operation 'network_file' and
'file_network', and register itself using the 'register_network' function.
When a new download is created, the 'op_download_start' function of
each registered network is called, and this function must try to
identify one the 'file_uids' so that it can locate the file on its
network.
  *)
    
    
    type impl
    
    type file = {        
        file_file : file CommonFile.file_impl;
        file_id : Md4.t;
        mutable file_name : string;
        file_swarmer : Int64Swarmer.t;
        
        mutable file_uids : file_uid list;
        mutable file_filenames : string list;
        
        mutable file_impls : (impl network_file * impl) list;
        mutable file_files : (string * int64 * int64) list;
        mutable file_verified_partition : Int64Swarmer.partition option;  
      }
    
    and 'a network_file = {
        mutable op_download_network : file_network;
        mutable op_download_finish : ('a -> unit); 
        mutable op_download_recover : ('a -> unit);
        mutable op_download_pause : ('a -> unit);
        mutable op_download_resume : ('a -> unit);
        mutable op_download_sources : ('a -> client list);
        mutable op_download_debug : ('a -> Buffer.t -> unit);
        mutable op_download_to_value : ('a -> (string * option_value) list);
      }
    
    and file_network = {
        mutable op_download_netname : string;
        mutable op_download_start : (file -> unit);
        mutable op_download_of_value : (file ->
        (string * option_value) list -> unit);
      }
    
    let file_num file = file.file_file.impl_file_num
    let file_size file = file.file_file.impl_file_size
    let file_downloaded file = file_downloaded (as_file file.file_file)
    let file_age file = file.file_file.impl_file_age
    let file_fd file = file.file_file.impl_file_fd
    let file_disk_name file = file_disk_name (as_file file.file_file)
    let set_file_disk_name file = set_file_disk_name (as_file file.file_file)
    let file_state file =  file_state (as_file file.file_file)      
    let file_must_update file =
      file_must_update (as_file file.file_file)
    
    
    let network = CommonUploads.network
    
    let (file_ops : file CommonFile.file_ops) = 
      CommonFile.new_file_ops network    
    
    let networks = ref []
    let current_files = ref []
    let files_by_num = Hashtbl.create 13
    
    let register_network f =
      networks := f :: !networks;
      ()
    
    let new_network_file n = {
        op_download_finish = (fun _ -> ());
        op_download_sources = (fun _ -> []);
        op_download_recover = (fun _ -> ());
        op_download_pause = (fun _ -> ());
        op_download_resume = (fun _ -> ());
        op_download_to_value = (fun _ -> []);
        op_download_debug = (fun _ _ -> ());
        op_download_network = n;
      }
    
    let new_file_network name = {
        op_download_netname = name;
        op_download_start = (fun _ -> ());
        op_download_of_value = (fun  _ _ -> ());
      }
    
    let add_file_impl file (f : 'a network_file) (v : 'a) =
      file.file_impls <- (Obj.magic (f,v)) :: file.file_impls

(* We should have a Hashtbl keeping the files by their UID *)
    
    let new_download  file_name file_size file_uids = 
      let file_id = Md4.random () in
      let file_temp = Filename.concat !!temp_directory 
          (Printf.sprintf "COMMON-%s" (Md4.to_string file_id)) in
      let t = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
      let swarmer = Int64Swarmer.create () in
      let rec file = {
          file_file = file_impl;
          file_id = file_id;
          file_name = file_name;
          file_swarmer = swarmer;
          file_uids = expand_uids file_uids;
          file_filenames = [file_name];
          file_impls = [];
          file_files = [];
          file_verified_partition = None;
        } and file_impl =  {
          dummy_file_impl with
          impl_file_fd = t;
          impl_file_size = file_size;
          impl_file_downloaded = Int64.zero;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_best_name = file_name;
        } 
      in
      Int64Swarmer.set_size swarmer file_size;  
      Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
          
          if !!CommonOptions.buffer_writes then 
            Unix32.buffered_write_copy t offset s pos len
          else
            Unix32.write  t offset s pos len
      );
      current_files := file :: !current_files;
      Hashtbl.add files_by_num (file_num file) file;
      file_add file_impl FileDownloading;

(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)

(* We should probably recall this part every few minutes... *)
      
      List.iter (fun f -> try f.op_download_start file with _ -> ()) !networks;
      file      
    
    let remove_file file =
      current_files := List2.removeq file !current_files;
      List.iter (fun (n,v) -> n.op_download_finish v) file.file_impls
    
    let _ =
      let module P = GuiTypes in
      file_ops.op_file_cancel <- (fun file ->
          remove_file file;
          (try  Unix32.remove (file_fd file)  with e -> ());
          file_cancel (as_file file.file_file);
      );
      file_ops.op_file_info <- (fun file ->
          let avail = match file.file_verified_partition with
              None -> "0"
            | Some partition ->
                Int64Swarmer.verified_bitmap partition
          in
          {
            P.file_name = file.file_name;
            P.file_num = (file_num file);
            P.file_network = network.network_num;
            P.file_names = file.file_filenames;
            P.file_md4 = Md4.null;
            P.file_size = file_size file;
            P.file_downloaded = file_downloaded file;
            P.file_nlocations = 0;
            P.file_nclients = 0;
            P.file_state = file_state file;
            P.file_sources = None;
            P.file_download_rate = file_download_rate file.file_file;
            P.file_chunks = avail;
            P.file_availability = avail;
            P.file_format = FormatNotComputed 0;
            P.file_chunks_age = [|0|];
            P.file_age = file_age file;
            P.file_last_seen = BasicSocket.last_time ();
            P.file_priority = file_priority (as_file file.file_file);
          }    
      );
      file_ops.op_file_sources <- (fun file ->
          let list = ref [] in
          List.iter (fun (n,v) ->
              list := (n.op_download_sources v) @ !list) file.file_impls;
          !list
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
    
    let value_to_int32pair v =
      match v with
        List [v1;v2] | SmallList [v1;v2] ->
          (value_to_int64 v1, value_to_int64 v2)
      | _ -> 
          failwith "Options: Not an int32 pair"
    
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
          value_to_int64 (List.assoc "file_size" assocs) 
        with _ -> failwith "Bad file size"
      in
      let file_uids = try
          value_to_list (fun v ->
              uid_of_string (value_to_string v)  
          ) (List.assoc "file_hashes" assocs)
        with _ -> 
            lprintf "[ERROR]: Could not load hash for %s\n"
              file_name;
            []
      in
      
      let file = new_download file_name file_size file_uids in
      
      
      let _ = 
        try
          List.iter (fun v ->
              file.file_files <- v :: file.file_files
          )
          (get_value "file_files" 
              (value_to_list (fun v ->
                  match v with
                    SmallList [name; p1;p2]
                  | List [name; p1;p2] ->
                      value_to_string name, value_to_int64 p1, value_to_int64 p2
                  | _ -> assert false
              )))
        with _ -> ()
      in
      
      (try 
          Int64Swarmer.set_present file.file_swarmer 
            (get_value "file_present_chunks" 
              (value_to_list value_to_int32pair));
          add_file_downloaded file.file_file
            (Int64Swarmer.downloaded file.file_swarmer)      
        with _ -> ()                
      );
      (try
          match (List.assoc "file_on_networks" assocs) with
            List l | SmallList l ->
              List.iter (fun v ->
                  match v with
                    Module assocs ->
                      let netname = value_to_string (List.assoc "network" assocs) in
                      List.iter (fun n ->
                          if n.op_download_netname = netname then
                            n.op_download_of_value file assocs
                      ) !networks
                  | _ -> ()
              ) l
          | _ -> ()
        with _ -> ());
      
      as_file file.file_file
    
    let file_to_value file =
      [
        "file_size", int64_to_value (file_size file);
        "file_name", string_to_value file.file_name;
        "file_downloaded", int64_to_value (file_downloaded file);
        "file_id", string_to_value (Md4.to_string file.file_id);
        "file_hashes", list_to_value "Hashes" (fun uid ->
            string_to_value (string_of_uid uid)
        ) file.file_uids;
        "file_present_chunks", List
          (List.map (fun (i1,i2) -> 
              SmallList [int64_to_value i1; int64_to_value i2])
          (Int64Swarmer.present_chunks file.file_swarmer));
        "file_files", list_to_value ""
          (fun (name, p1,p2) ->
            SmallList [string_to_value name; int64_to_value p1; int64_to_value p2])
        file.file_files;
        
        "file_on_networks", 
        List (List.map (fun (n,v) ->
              Module (
                ("network", string_to_value n.op_download_network.op_download_netname) ::
                (n.op_download_to_value v)
              )) file.file_impls);
        
      ]
    
    let _ =
      network.op_network_file_of_option <- value_to_file;
      file_ops.op_file_to_option <- file_to_value
    
    let download_is_finished file = 
      if List.memq file !current_files then begin
          file_completed (as_file file.file_file);
          remove_file file;
          List.iter (fun (n,v) -> n.op_download_finish v) file.file_impls
        end    
    
    let check_finished file = 
      try
        if file_state file <> FileDownloaded &&
          file_size file = Int64Swarmer.downloaded file.file_swarmer then
          (match file.file_verified_partition with
              Some partition ->
                let bitmap = Int64Swarmer.verified_bitmap partition in
                for i = 0 to String.length bitmap - 1 do
                  if bitmap.[i] <> '3' then raise Not_found;
                done;  
            | None -> ());
        download_is_finished file
      with _ -> ()      
        
    let downloaded file = Int64Swarmer.downloaded file.file_swarmer

(* This should be a per-network command... For example, the BitTorrent
URN should contain both the tracker and the info, so that the
tracker can be different for two URNs, but the info still the same. *)
      
    open GuiTypes  
    let _ =
      let commands = [
          "add_uid", Arg_multiple (fun args o ->
              match args with
                file_num :: ( (_ :: _) as tail) ->
                  let file_num = int_of_string file_num in
                  let file = Hashtbl.find files_by_num file_num in
                  List.iter (fun urn ->
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
                      List.iter (fun uid ->
                          if not (List.mem uid file.file_uids) then
                            file.file_uids <- uid :: file.file_uids
                      ) uids
                  ) tail;
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
        ] in
      
      CommonNetwork.register_commands commands;
      
  end
