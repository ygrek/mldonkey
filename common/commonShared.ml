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

open Options
open CommonTypes

type t
  
type shared_impl = {
    impl_shared_filename : string;
    mutable impl_shared_update : bool;
    mutable impl_shared_num : int;
    mutable impl_shared_ops : (t * (t shared_ops)) list;
  }
  
and 'a shared_ops = {
    mutable op_shared_unshare : (shared -> 'a -> unit);
  }
  
let shared_num = ref 0
let shareds_by_num = Hashtbl.create 1027
let shareds_by_filename = Hashtbl.create 1027
  
let ni n m = 
  let s = Printf.sprintf "Shared.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni  n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)

let as_shared  (shared : shared_impl) =
  let (shared : shared) = Obj.magic shared in
  shared
  
let as_shared_impl  (shared : shared) =
  let (shared : shared_impl) = Obj.magic shared in
  shared

    
let shareds_update_list = ref []
  
let shared_must_update shared =
  let impl = as_shared_impl shared in
  if not impl.impl_shared_update then
    begin
      impl.impl_shared_update <- true;
      shareds_update_list := shared :: !shareds_update_list
    end

  
let new_shared filename =
  if not (Hashtbl.mem shareds_by_filename filename) then begin
      incr shared_num;
      let impl = {
          impl_shared_update = false;
          impl_shared_filename = filename;
          impl_shared_num = !shared_num;
          impl_shared_ops = [];
        } in
      let s = as_shared impl in
      shared_must_update (as_shared impl);
      Hashtbl.add shareds_by_num !shared_num s;
      Hashtbl.add shareds_by_filename filename s;
      CommonNetwork.networks_iter (fun n -> CommonNetwork.network_share n s)
    end
    
let shared_num shared =
  let impl = as_shared_impl  shared in
  impl.impl_shared_num

let shared_filename s =
  let impl = as_shared_impl s in
  impl.impl_shared_filename  
  
let shared_unshare s =
  let impl = as_shared_impl s in
  List.iter (fun (v,o) -> 
      try o.op_shared_unshare s v with _ -> ()) impl.impl_shared_ops;
  Hashtbl.remove shareds_by_num impl.impl_shared_num;
  Hashtbl.remove shareds_by_filename impl.impl_shared_filename
  
let new_shared_ops network = {
    op_shared_unshare = (fun _ _ -> ni_ok network "shared_unshare");
  }

let shared_find num = 
  Hashtbl.find shareds_by_num num
  
let shared_check_files () =
  let list = ref [] in
  Hashtbl.iter (fun _ s ->
      let name = shared_filename s in
      if not (Sys.file_exists name) then list := s :: !list
  ) shareds_by_num;
  List.iter (fun s -> shared_unshare s) !list
  
let com_shareds_by_num = shareds_by_num
let shareds_by_num = ()

let file_size filename = Unix32.getsize32 filename
let local_dirname = Sys.getcwd ()
  
let rec shared_add_directory dirname =
  let files = Unix2.list_directory dirname in
  List.iter (fun file ->
      let name =  Filename.concat dirname file in
      try
        if Unix2.is_directory name then
          shared_add_directory name
        else
        let real_name =  
          Filename2.normalize (
            if Filename.is_relative name then
              Filename.concat local_dirname name
            else name) in
        let size = file_size name in
        if size > Int32.zero then
          new_shared real_name
      with _ -> ()
  ) files
  
let set_shared_ops s f v =
  let impl = as_shared_impl s in
  impl.impl_shared_ops <- (f,v) :: impl.impl_shared_ops
  