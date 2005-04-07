(* Copyright 2001, 2002, 2005 b8_bavard, b8_fee_carabine, INRIA *)
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
open BTTypes
open BTGlobals

let must_share_file file codedname has_old_impl =
  match file.file_shared with
  | Some _ -> ()
  | None ->
      begin
        let impl = {
          impl_shared_update = 1;
          impl_shared_fullname = file_disk_name file;
          impl_shared_codedname = codedname;
          impl_shared_size = file_size file;
          impl_shared_id = Md4.null;
          impl_shared_num = 0;
          impl_shared_uploaded = Int64.zero;
          impl_shared_ops = shared_ops;
          impl_shared_val = file;
          impl_shared_requests = 0;
        } in
        file.file_shared <- Some impl;
        incr CommonGlobals.nshared_files;
        CommonShared.shared_calculate_total_bytes ();
        match has_old_impl with
          None -> update_shared_num impl
        | Some old_impl -> replace_shared old_impl impl
      end

let must_share_file file = must_share_file file (file_best_name (as_file file)) None

let unshare_file file =
  match file.file_shared with
    None -> ()
  | Some s ->
      begin
        file.file_shared <- None;
        decr CommonGlobals.nshared_files;
        CommonShared.shared_calculate_total_bytes ()
      end
