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
open Md4
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
  
let must_share_file file codedname has_old_impl =
  match file.file_shared with
  | Some _ -> ()
  | None ->
      new_shared := file :: !new_shared;
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
      incr CommonGlobals.nshared_files;
      match has_old_impl with
        None -> update_shared_num impl
      | Some old_impl -> replace_shared old_impl impl


let md4_of_list md4s =
  let len = List.length md4s in
  let s = String.create (len * 16) in
  let rec iter list i =
    match list with
      [] -> ()
    | md4 :: tail ->
        let md4 = Md4.direct_to_string md4 in
        String.blit md4 0 s i 16;
        iter tail (i+16)
  in
  iter md4s 0;
  Md4.string s
  
let new_file_to_share sh codedname old_impl =
  try
(* How do we compute the total MD4 of the file ? *)
    
    let md4s = List.rev sh.sh_md4s in
    let md4 = match md4s with
        [md4] -> md4
      | [] -> lprintf "No md4 for %s" sh.sh_name;
          lprint_newline ();
          raise Not_found
      | _ -> md4_of_list md4s
    in
    
    let file = new_file FileShared sh.sh_name md4 sh.sh_size false in
    must_share_file file codedname old_impl;
    file.file_md4s <- md4s;
    file_md4s_to_register := file :: !file_md4s_to_register;
    let sh_name = Filename.basename sh.sh_name in
    if not (List.mem sh_name file.file_filenames) then begin
        file.file_filenames <- file.file_filenames @ [sh_name];
        update_best_name file;
      end;
    file.file_chunks <- Array.make file.file_nchunks PresentVerified;
    file.file_absent_chunks <- [];
(*    file.file_all_chunks <- String.make file.file_nchunks '1'; *)
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
    lprintf "Sharing %s" sh.sh_name;
    lprint_newline ();
  with e ->
      lprintf "Exception %s while sharing %s" (Printexc2.to_string e)
      sh.sh_name; lprint_newline () 
      
  
let all_shared () =  
  let shared_files = ref [] in
  Hashtbl.iter (fun md4 file ->
      match  file.file_shared with
        None -> ()
      | Some _ ->  shared_files := file :: !shared_files
  ) files_by_md4;
  !shared_files

(*  Check whether new files are shared, and send them to connected servers.
Do it only once per 5 minutes to prevent sending to many times all files.
  Should I only do it for master servers, no ?
  *)
let send_new_shared () =
  if !new_shared != [] then
    begin
      new_shared := [];
      let socks = ref [] in
      let list = all_shared () in
      List.iter (fun s ->
          if s.server_master then
            match s.server_sock with
              None -> ()
            | Some sock ->
                direct_server_send_share sock list) (connected_servers ());
    end
          
(*
The problem: sh.shared_fd might be closed during the execution of the
thread. Moreover, we don't want to open all the filedescs for all the
files being shared !
*)
          
(*   Compute (at most) one MD4 chunk if needed. *)
let check_shared_files () =  
  let module M = CommonHasher in
  match !shared_files with
    [] -> ()  
  | sh :: files ->
      shared_files := files;

(*      lprintf "check_shared_files"; lprint_newline (); *)
      
      let rec job_creater _ =
        try
          if not (Sys.file_exists sh.shared_name) then begin
              lprintf "Shared file doesn't exist"; lprint_newline ();
              raise Not_found;
            end;
          if Unix32.getsize64 sh.shared_name <> sh.shared_size then begin
              lprintf "Bad shared file size" ; lprint_newline ();
              raise Not_found;
            end;
          let end_pos = Int64.add sh.shared_pos block_size in
          let end_pos = if end_pos > sh.shared_size then sh.shared_size
            else end_pos in
          let len = Int64.sub end_pos sh.shared_pos in
(*          lprintf "compute next md4"; lprint_newline (); *)
          
          M.compute_md4 (Unix32.filename sh.shared_fd) sh.shared_pos len
            (fun job ->
              if job.M.job_error then begin
                lprintf "Error prevent sharing %s\n" sh.shared_name
              end else 
              let _ = () in
(*              lprintf "md4 computed"; lprint_newline (); *)
              let new_md4 = Md4.direct_of_string job.M.job_result in
              
              sh.shared_list <- new_md4 :: sh.shared_list;
              sh.shared_pos <- end_pos;
              if end_pos = sh.shared_size then begin
                  let s = {
                      sh_name = sh.shared_name;
                      sh_size = sh.shared_size;
                      sh_md4s = sh.shared_list;
                      sh_mtime = Unix32.mtime64 sh.shared_name;
                    } in
                  lprintf "NEW SHARED FILE %s" sh.shared_name; 
                  lprint_newline ();
                  Hashtbl.add shared_files_info sh.shared_name s;
                  known_shared_files =:= s :: !!known_shared_files;
                  new_file_to_share s sh.shared_shared.impl_shared_codedname (Some  sh.shared_shared);
                  shared_remove  sh.shared_shared;
                end
              else
                job_creater ())
        with e ->
            lprintf "Exception %s prevents sharing"
              (Printexc2.to_string e);
            lprint_newline ();
      in
      job_creater ()
      
let local_dirname = Sys.getcwd ()
  
let _ =
  network.op_network_share <- (fun fullname codedname size ->
      if !verbose_share then begin
          lprintf "FULLNAME %s" fullname; lprint_newline ();
        end;
(*      let codedname = Filename.basename codedname in*)
      if !verbose_share then begin
          lprintf "CODEDNAME %s" codedname; lprint_newline ();
        end;
      try
(*
lprintf "Searching %s" fullname; lprint_newline ();
*)
        let s = Hashtbl.find shared_files_info fullname in
        let mtime = Unix32.mtime64 fullname in
        if s.sh_mtime = mtime && s.sh_size = size then begin
            if !verbose_share then begin
                lprintf "USING OLD MD4s for %s" fullname;
                lprint_newline (); 
              end;
            new_file_to_share s codedname None
          end else begin
            if !verbose_share then begin                
                lprintf "Shared file %s has been modified" fullname;
                lprint_newline ();
              end;
            Hashtbl.remove shared_files_info fullname;
            known_shared_files =:= List2.removeq s !!known_shared_files;
            raise Not_found
          end
      with Not_found ->
          if !verbose_share then begin
              lprintf "No info on %s" fullname; lprint_newline (); 
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
              shared_fd = Unix32.create fullname [Unix.O_RDONLY] 0o444;
            } in
          update_shared_num impl;  
          shared_files := pre_shared :: !shared_files;
  )
  
let remember_shared_info file new_name =
  if file.file_md4s <> [] then
    try
      let disk_name = file_disk_name file in
      let mtime = Unix32.mtime64 disk_name in
      
      if !verbose_share then begin
          lprintf "Remember %s" new_name; lprint_newline ();
        end;
	let s = {
        sh_name = new_name;
        sh_size = file_size file;
        sh_mtime = mtime;
        sh_md4s = file.file_md4s;
      } in
      known_shared_files =:= s :: !!known_shared_files;    
      Hashtbl.add shared_files_info new_name s
    with e ->
        lprintf "Exception %s in remember_shared_info"
          (Printexc2.to_string e);
        lprint_newline ()
        
let must_share_file file = must_share_file file (file_best_name file) None
  
