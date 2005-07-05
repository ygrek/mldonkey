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

open CommonGlobals
open Printf2
open Md4
open CommonDownloads
open CommonFile
open CommonShared
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open DonkeyMftp
open DonkeyImport
open DonkeyProtoCom
open DonkeyTypes
open DonkeyOptions
open CommonOptions
open DonkeyComplexOptions
open DonkeyGlobals

let new_shared_files = ref []

let must_share_file file codedname has_old_impl =
  match file.file_shared with
  | Some _ -> ()
  | None ->
      new_shared := true;
      let impl = {
          impl_shared_update = 1;
          impl_shared_fullname = file_disk_name file;
          impl_shared_codedname = codedname;
          impl_shared_size = file_size file;
          impl_shared_id = file.file_md4;
          impl_shared_num = 0;
          impl_shared_uploaded = Int64.zero;
          impl_shared_ops = shared_ops;
          impl_shared_val = file;
          impl_shared_requests = 0;
        } in
      file.file_shared <- Some impl;
      new_shared_files := file :: !new_shared_files;
      incr CommonGlobals.nshared_files;
      CommonShared.shared_calculate_total_bytes ();
      match has_old_impl with
        None -> update_shared_num impl
      | Some old_impl -> replace_shared old_impl impl


let md4_of_array md4s =
  let len = Array.length md4s in
  let s = String.create (len * 16) in
  for i = 0 to len-1 do
    let md4 = Md4.direct_to_string md4s.(i) in
    String.blit md4 0 s (i*16) 16;
  done;
  Md4.string s
  
let new_file_to_share sh codedname old_impl =
  try
(* How do we compute the total MD4 of the file ? *)
    
    let md4s = sh.sh_md4s in
    let md4 = match Array.length md4s with
        1 -> md4s.(0)
      | 0 -> assert false
      | _ -> md4_of_array md4s
    in
    
    if !verbose_share then begin
    lprintf "Sharing file with MD4: %s\n" (Md4.to_string md4);
    end;
    
    let file = new_file sh.sh_name FileShared md4 sh.sh_size
        [Filename.basename sh.sh_name, GuiTypes.noips()]
        false in
    must_share_file file codedname old_impl;
    file.file_computed_md4s <- md4s;
    file_md4s_to_register := file :: !file_md4s_to_register;
    let sh_name = Filename.basename sh.sh_name in
    if not (List.mem_assoc sh_name file.file_filenames) then begin
        file.file_filenames <- file.file_filenames @ [sh_name, GuiTypes.noips()];
        update_best_name file;
      end;
(*  file.file_chunks <- Array.make file.file_nchunks PresentVerified; *)
(*    file.file_absent_chunks <- []; *)
(*    file.file_all_chunks <- String.make file.file_nchunks '1'; *)
    (* Should we trust mtimes, or reverify each file.  If we trust
     * mtimes, I guess we have to call
     * Int64Swarmer.set_verified_bitmap "333..."
     * this seems unspeakably ugly, but the alternative is to reverify 
     * every shared file every hour.
     *
     * If, however, we could somehow avoid regenerating shared files
     * when the directory is scanned, verifying everything on startup
     * might be acceptable.
     *
     * Also, timestamps would be more resilient if we took the maximum
     * of mtime and ctime.  (Touch will set ctime to the current time
     * regardless of the mtime being set.)
     *)
    match file.file_swarmer with
      Some s -> (let len = Array.length md4s in
		 let ver_str = String.make len '3' in
		     Int64Swarmer.set_verified_bitmap s ver_str;
		 (*
		 Int64Swarmer.set_present s [(Int64.zero, file_size file)];
		 (* If we don't verify now, it will never happen! *)
		 Int64Swarmer.verify_all_blocks s true;
		 *)
                if !verbose_share then
                  lprintf "verified map of %s = %s\n"
		         (codedname) (Int64Swarmer.verified_bitmap s))
      | None -> if !verbose_share then lprintf "no swarmer for %s\n" codedname;
    (try 
        file.file_format <- CommonMultimedia.get_info 
          (file_disk_name file)
      with _ -> ());
    (*
    (try 
        DonkeyOvernet.publish_file file
      with e -> 
          lprintf "DonkeyOvernet.publish_file: %s" (Printexc2.to_string e);
lprint_newline ());
  *)
    if !verbose_share then begin
        lprintf "Sharing %s\n" sh.sh_name;
    end;
  with e ->
      lprintf "Exception %s while sharing %s\n" (Printexc2.to_string e)
      sh.sh_name
      
  
let all_shared () =  
  let shared_files = ref [] in
  Hashtbl.iter (fun md4 file ->
      match  file.file_shared with
        None -> ()
      | Some _ ->  shared_files := file :: !shared_files
  ) files_by_md4;
  if !verbose_share then lprintf "%d files shared\n" (List.length !shared_files);
  !shared_files

(* Check whether new files are shared, and send them to connected servers.
   Do it only once per 5 minutes to prevent sending to many times all files.
   Change: Just send *new* shared files to servers, they never forget a
     clients files until disconnection.
   Should I only do it for master servers, no ?
*)
let send_new_shared () =
  if !new_shared then
    begin
      new_shared := false;
      let socks = ref [] in
      if !new_shared_files <> [] then
        begin
          List.iter (fun s ->
            if s.server_master then
              begin
                if !verbose_share then
                  lprintf "donkey send_new_shared: found master server\n";
                do_if_connected s.server_sock (fun sock ->
                  server_send_share s.server_has_zlib sock !new_shared_files)
              end  
                )
          (connected_servers ());
          if !verbose_share then
              lprintf "donkey send_new_shared: Sent %d new shared files to servers\n"
                    (List.length !new_shared_files);
          new_shared_files := []
        end
      else
          lprintf "donkey send_new_share: No new shared files to send to servers\n"
    end

(*
The problem: sh.shared_fd might be closed during the execution of the
thread. Moreover, we don't want to open all the filedescs for all the
files being shared !
*)

let computation = ref false
    
(*   Compute (at most) one MD4 chunk if needed. *)
let check_shared_files () =  
  let module M = CommonHasher in
  if not !computation then
    match !shared_files with
      [] -> ()  
    | sh :: files ->
        shared_files := files;

(*      lprintf "check_shared_files"; lprint_newline (); *)
        
        let rec job_creater _ =
          try
            if not (Sys.file_exists sh.shared_name) then begin
                lprintf "Shared file doesn't exist\n"; 
                raise Not_found;
              end;
            if Unix32.getsize sh.shared_name false <> sh.shared_size then begin
                lprintf "Bad shared file size\n" ; 
                raise Not_found;
              end;
            computation := true;
            let end_pos = Int64.add sh.shared_pos block_size in
            let end_pos = if end_pos > sh.shared_size then sh.shared_size
              else end_pos in
            let len = Int64.sub end_pos sh.shared_pos in
(*          lprintf "compute next md4"; lprint_newline (); *)
            
            M.compute_md4 (Unix32.filename sh.shared_fd) sh.shared_pos len
              (fun job ->
                computation := false;
                if job.M.job_error then begin
                    lprintf "Error prevent sharing %s\n" sh.shared_name
                  end else 
                let _ = () in
(*              lprintf "md4 computed"; lprint_newline (); *)
                let new_md4 = job.M.job_result in
                
                sh.shared_list <- new_md4 :: sh.shared_list;
                sh.shared_pos <- end_pos;
                if end_pos = sh.shared_size then begin
                    let s = {
                        sh_name = sh.shared_name;
                        sh_size = sh.shared_size;
                        sh_md4s = Array.of_list (List.rev sh.shared_list);
                        sh_mtime = Unix32.mtime sh.shared_name;
                      } in
                    lprintf "Donkey: NEW SHARED FILE %s\n" sh.shared_name; 
                    Hashtbl.add shared_files_info 
                      (s.sh_name, s.sh_size, s.sh_mtime) s;
                    known_shared_files =:= s :: !!known_shared_files;
                    new_file_to_share s sh.shared_shared.impl_shared_codedname (Some  sh.shared_shared);
                    shared_remove  sh.shared_shared;
                  end
                else
                  job_creater ())
          with e ->
              lprintf "Exception %s prevents sharing\n"
                (Printexc2.to_string e);
        in
        job_creater ()
        
let local_dirname = Sys.getcwd ()
  
let _ =
  network.op_network_share <- (fun fullname codedname size ->
      if !verbose_share then
        lprintf "donkeyShare: Sharing %s\n" fullname; 
      try
(*
lprintf "Searching %s" fullname; lprint_newline ();
*)
        let mtime = Unix32.mtime fullname in
        let s = Hashtbl.find shared_files_info 
            (fullname, size, mtime) in
        (* if s.sh_mtime = mtime && s.sh_size = size then begin *)
            if !verbose_share then begin 
                lprintf "donkeyShare: Using old MD4s for %s\n" fullname;
              end;
            new_file_to_share s codedname None
(*          end else begin
            if !verbose_share then begin                
                lprintf "Shared file %s has been modified\n" fullname;
              end;
            Hashtbl.remove shared_files_info fullname;
            known_shared_files =:= List2.removeq s !!known_shared_files;
            raise Not_found
          end *)
      with Not_found ->
          if !verbose_share then begin
              lprintf "donkeyShare: No info on %s\n" fullname; 
            end;
          
          let rec iter list left =
            match list with
              [] -> List.rev left
            | sh :: tail ->
                if sh.shared_name = fullname then iter tail left
                else iter tail (sh :: left)
          in
          shared_files := iter !shared_files [];
          
          let rec impl = {
              impl_shared_update = 1;
              impl_shared_fullname = fullname;
              impl_shared_codedname = codedname;
              impl_shared_size = size;
              impl_shared_num = 0;
              impl_shared_uploaded = Int64.zero;
              impl_shared_ops = pre_shared_ops;
          	  impl_shared_id = Md4.null;
              impl_shared_val = pre_shared;
              impl_shared_requests = 0;
            } and
            pre_shared = {
              shared_shared = impl;
              shared_name = fullname;              
              shared_size = size;
              shared_list = [];
              shared_pos = Int64.zero;
              shared_fd = Unix32.create_ro fullname;
            } in
          update_shared_num impl;  
          shared_files := pre_shared :: !shared_files;
  )
  
let remember_shared_info file new_name =
(* changed 2.5.24: normalization of file names is done in 
    CommonShared.new_shared, so it should be done here too. *)
  let new_name = Filename2.normalize new_name in
(*  lprintf "******  remember_shared_info for new file %s\n" new_name; *)
  if file.file_computed_md4s <> [||] then
    try
      let mtime = 
        try
          let disk_name = file_disk_name file in
          Unix32.mtime disk_name 
        with _ ->          
            if !verbose_hidden_errors then
              lprintf "Trying mtime on new name\n";
            Unix32.mtime new_name
      in
      
      if !verbose_share then begin
          lprintf "Remember %s\n" new_name; 
        end;
      
      let size = file_size file in
      if not (Hashtbl.mem shared_files_info 
            (new_name, size, mtime) ) then
        let s = {
            sh_name = new_name;
            sh_size = size;
            sh_mtime = mtime;
            sh_md4s = file.file_computed_md4s;
          } in

        known_shared_files =:= s :: !!known_shared_files;    
        Hashtbl.add shared_files_info (new_name, size, mtime) s
    with e ->
        lprintf "Exception %s in remember_shared_info\n"
          (Printexc2.to_string e)

let must_share_file file = must_share_file file (file_best_name file) None
