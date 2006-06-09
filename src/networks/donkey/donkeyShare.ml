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

open Int64ops
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
      let full_name = file_disk_name file in
      let magic =
        match Magic.M.magic_fileinfo full_name false with
          None -> None
        | Some magic -> Some (HashMagic.merge files_magic magic)
      in

      let impl = {
          impl_shared_update = 1;
          impl_shared_fullname = full_name;
          impl_shared_codedname = codedname;
          impl_shared_size = file_size file;
          impl_shared_id = file.file_md4;
          impl_shared_num = 0;
          impl_shared_uploaded = Int64.zero;
          impl_shared_ops = shared_ops;
          impl_shared_val = file;
          impl_shared_requests = 0;
          impl_shared_magic = magic;
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

    if !verbose_share then
      lprintf_nl "Sharing file with MD4: %s" (Md4.to_string md4);

    let file = new_file sh.sh_name FileShared md4 sh.sh_size
        [Filename.basename sh.sh_name, GuiTypes.noips()]
        false in
    must_share_file file codedname old_impl;
    file.file_computed_md4s <- md4s;
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
     * CommonSwarming.set_chunks_verified_bitmap "333..."
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
     CommonSwarming.set_chunks_verified_bitmap s
       (CommonSwarming.VerificationBitmap.of_string ver_str);
     (*
     CommonSwarming.set_present s [(Int64.zero, file_size file)];
     (* If we don't verify now, it will never happen! *)
     CommonSwarming.verify_all_blocks s true;
     *)
                if !verbose_share then
                  lprintf_nl "verified map of %s = %s"
             (codedname) (CommonSwarming.chunks_verified_bitmap s))
      | None -> if !verbose_share then lprintf_nl "no swarmer for %s" codedname;
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
    if !verbose_share then
      lprintf_nl "new_file_to_share: Sharing %s" sh.sh_name;
  with e ->
      lprintf_nl "Exception %s while sharing %s" (Printexc2.to_string e)
      sh.sh_name

let all_shared () =
  let shared_files = ref [] in
  Hashtbl.iter (fun md4 file ->
      match file.file_shared with
        None -> ()
      | Some _ ->  shared_files := file :: !shared_files
  ) files_by_md4;
  if !verbose_share then lprintf_nl "%d files shared" (List.length !shared_files);
  !shared_files

(* Check whether new files are shared, and send them to connected servers.
   Do it only once per 5 minutes to prevent sending to many times all files.
   Change: Just send *new* shared files to servers, they never forget a
     clients files until disconnection.
   Should I only do it for master servers, no ?
*)
let send_new_shared () =
  let tag = ref false in
  if !new_shared then
    begin
      new_shared := false;
      if !new_shared_files <> [] then
        begin
          List.iter (fun s ->
            if s.server_master then
              begin
                if !verbose_share || !verbose then
                  lprintf_nl "send_new_shared: found master server %s:%d"
                   (Ip.to_string s.server_ip) s.server_port;
                tag := true;
                do_if_connected s.server_sock (fun sock ->
                  server_send_share s.server_has_zlib sock !new_shared_files)
              end
          ) (connected_servers ());
          if !tag && (!verbose_share || !verbose) then
              lprintf_nl "send_new_shared: Sent %d new shared files to servers"
                (List.length !new_shared_files);
          new_shared_files := []
        end
      else
          lprintf_nl "donkey send_new_share: No new shared files to send to servers"
    end

(*
The problem: sh.shared_fd might be closed during the execution of the
thread. Moreover, we don't want to open all the filedescs for all the
files being shared !
*)

exception Wrong_file_size

let computing = ref false

(*   Compute (at most) one MD4 chunk if needed. *)
let check_shared_files () =
  let module M = CommonHasher in
  if not !computing then
    match !shared_files with
      [] -> ()
    | sh :: files ->
        let rec job_creater _ =
          try
            if not (Sys.file_exists sh.shared_name) then
                raise Not_found;
            if Unix32.getsize sh.shared_name <> sh.shared_size then
                raise Wrong_file_size;

            computing := true;

            let end_pos = sh.shared_pos ++ block_size in
            let end_pos = min end_pos sh.shared_size in
            let len = end_pos -- sh.shared_pos in

            if !verbose_md4 then
              lprintf_nl "Hashing chunk %d: %Ld-%Ld (%Ld) of %s"
                sh.shared_chunk sh.shared_pos end_pos len sh.shared_name;

            M.compute_md4 (Unix32.filename sh.shared_fd) sh.shared_pos len
              (fun job ->
                computing := false;
                if job.M.job_error then begin
                  lprintf_nl "Error prevent sharing %s" sh.shared_name
                end else
                  let new_md4 = job.M.job_result in

                  sh.shared_list <- new_md4 :: sh.shared_list;
                  sh.shared_pos <- end_pos;
                  sh.shared_chunk <- sh.shared_chunk + 1;

                  if sh.shared_chunk = get_nchunks sh.shared_size then begin
                    let s = {
                        sh_name = sh.shared_name;
                        sh_size = sh.shared_size;
                        sh_md4s = Array.of_list (List.rev sh.shared_list);
                        sh_mtime = Unix32.mtime sh.shared_name;
                    } in
                    lprintf_nl "New shared file %s" sh.shared_name;
                    Hashtbl.add shared_files_info
                      (s.sh_name, s.sh_size, s.sh_mtime) s;
                    known_shared_files =:= s :: !!known_shared_files;
                    shared_files := files;
                    new_file_to_share s sh.shared_shared.impl_shared_codedname (Some sh.shared_shared);
                  end
                else
                  job_creater ()
            )
          with e ->
            shared_files := files;
            lprintf_nl "Exception %s prevents sharing of %s"
              (Printexc2.to_string e) sh.shared_name
        in
        job_creater ()

let _ =
  network.op_network_share <- (fun fullname codedname size ->
      if !verbose_share then
        lprintf_nl "op_network_share: Sharing %s" fullname;
      try
(*
lprintf "Searching %s" fullname; lprint_newline ();
*)
        let mtime = Unix32.mtime fullname in

        let s = Hashtbl.find shared_files_info
            (fullname, size, mtime) in
        (* if s.sh_mtime = mtime && s.sh_size = size then begin *)
            if !verbose_share then
                lprintf_nl "donkeyShare: Using old MD4s for %s" fullname;
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
          if !verbose_share then
              lprintf_nl "donkeyShare: No info on %s" fullname;

    let found = ref false in
    List.iter (fun sh -> if sh.shared_name = fullname then found := true) !shared_files;
    if not !found then begin
        let magic =
          match Magic.M.magic_fileinfo fullname false with
            None -> None
          | Some magic -> Some (HashMagic.merge files_magic magic)
        in

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
              impl_shared_magic = magic;
            } and
            pre_shared = {
              shared_shared = impl;
              shared_name = fullname;
              shared_size = size;
              shared_list = [];
              shared_pos = 0L;
              shared_chunk = 0;
              shared_fd = Unix32.create_ro fullname;
            } in
          update_shared_num impl;
          shared_files := pre_shared :: !shared_files;
    end
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
            if !verbose then
              lprintf_nl "Share: Trying mtime on new name %s, disk_name %s"
          new_name (file_disk_name file);
            Unix32.mtime new_name
      in

      if !verbose_share then
          lprintf_nl "Remember %s" new_name;

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
        lprintf_nl "Exception %s in remember_shared_info"
          (Printexc2.to_string e)

let must_share_file file = must_share_file file (file_best_name file) None
