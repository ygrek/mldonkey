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

(*
For each file on disk that has to be shared, call
  network_share file_name codedname file_size
  
*)

open Md4

open CommonOptions
open CommonGlobals
open Options
open CommonTypes

type 'a shared_impl = {
    impl_shared_fullname : string;
    impl_shared_codedname : string;
    mutable impl_shared_val : 'a;
    mutable impl_shared_update : int;
    mutable impl_shared_num : int;
    mutable impl_shared_ops : 'a shared_ops;
    mutable impl_shared_uploaded : int64;
    mutable impl_shared_size : int32;
    mutable impl_shared_requests : int;
  }
  
and 'a shared_ops = {
    mutable op_shared_info : ('a -> GuiTypes.shared_info);
    mutable op_shared_unshare : ('a -> unit);
  }
  
let as_shared  (shared : 'a shared_impl) =
  let (shared : shared) = Obj.magic shared in
  shared
  
let as_shared_impl  (shared : shared) =
  let (shared : 'a shared_impl) = Obj.magic shared in
  shared

let shared_num s = (as_shared_impl s).impl_shared_num
  
module H = Weak2.Make(struct
      type t = shared
      let hash file = Hashtbl.hash (shared_num file)
      
      let equal x y  = (shared_num x) = (shared_num y)
    end)

let shared_counter = ref 0
let shareds_by_num = H.create 1027
  
let ni n m = 
  let s = Printf.sprintf "Shared.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni  n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)

    
let shared_must_update shared =
  let impl = as_shared_impl shared in
  if impl.impl_shared_update <> 0 then
    begin
      impl.impl_shared_update <- 0;
    end

let shared_must_update_downloaded shared =
  let impl = as_shared_impl shared in
  if impl.impl_shared_update <> 0 then
    begin
      impl.impl_shared_update <- - impl.impl_shared_update;
    end

let update_shared_num impl =
  if impl.impl_shared_num = 0 then begin
      if !!verbose then begin
          Printf.printf "NEW SHARED %s/%s" impl.impl_shared_codedname
            impl.impl_shared_fullname; print_newline ();
        end;
      incr shared_counter;
      impl.impl_shared_num <- !shared_counter;
      H.add shareds_by_num (as_shared impl);
      shared_must_update (as_shared impl);
    end

let replace_shared old_impl impl =
  H.remove shareds_by_num (as_shared old_impl);
  impl.impl_shared_num <- old_impl.impl_shared_num;
  shared_must_update (as_shared impl)
    
let shared_remove impl =
  H.remove shareds_by_num (as_shared impl)
    
let dirnames = Hashtbl.create 13
let dirname_counter = ref 0
    
let new_shared dirname filename fullname =
  let fullname = Filename2.normalize fullname in
  let filename = Filename2.normalize filename in
  let dirname = try
      Hashtbl.find dirnames dirname with _ ->
        incr dirname_counter;
        let name = Printf.sprintf "shared%d" !dirname_counter in
        Hashtbl.add dirnames dirname name;
        name in
  let codedname = Filename.concat dirname filename in
  let size = Unix32.getsize32 fullname in
  CommonNetwork.networks_iter (fun n -> 
      CommonNetwork.network_share n fullname codedname size)

let shared_num shared =
  let impl = as_shared_impl  shared in
  impl.impl_shared_num

let shared_fullname s =
  let impl = as_shared_impl s in
  impl.impl_shared_fullname  

let shared_codedname s =
  let impl = as_shared_impl s in
  impl.impl_shared_codedname  
  
let shared_unshare s =
  let impl = as_shared_impl s in
  try impl.impl_shared_ops.op_shared_unshare impl.impl_shared_val with _ -> ()

let new_shared_ops network = {
    op_shared_unshare = (fun _ -> ni_ok network "shared_unshare");
    op_shared_info = (fun _ -> fni network "shared_info");
  }

let dummy_shared = {
    impl_shared_fullname = "";
    impl_shared_codedname = "";
    impl_shared_val = 0;
    impl_shared_update = 0;
    impl_shared_num = 0;
    impl_shared_ops = {
      op_shared_unshare = (fun _ -> raise Not_found);
      op_shared_info = (fun _ -> raise Not_found);
    };
    impl_shared_uploaded = Int64.zero;
    impl_shared_size = Int32.zero;
    impl_shared_requests = 0;
  }
  
  
let shared_find num = 
  H.find shareds_by_num (as_shared { dummy_shared with impl_shared_num = num })

let shared_iter f =
  H.iter f shareds_by_num

let file_size filename = Unix32.getsize32 filename
let local_dirname = Sys.getcwd ()
  
(* Prevent sharing of temp directory to avoid sending incomplete files *)
let can_share dirname =
  Filename2.normalize dirname <> Filename2.normalize 
  !!CommonOptions.temp_directory
  
  
let rec shared_add_directory dirname local_dir =
  let dirname = 
    if Filename.is_relative dirname then
      Filename.concat file_basedir dirname
    else dirname 
  in
  if can_share dirname then
    let full_dir = Filename.concat dirname local_dir in
    let files = Unix2.list_directory full_dir in
    List.iter (fun file ->
        if file <> "" && file.[0] <> '.' then
          let full_name = Filename.concat full_dir file in
          let local_name = Filename.concat local_dir file in
          try
            if Unix2.is_directory full_name then
              shared_add_directory dirname local_name
            else
            try
              let size = file_size full_name in
              if size > Int32.zero && ( !!shared_extensions = [] ||
                  List.mem (String.lowercase (Filename2.last_extension full_name)) !!shared_extensions)
              then
                new_shared dirname local_name full_name
            with e -> 
                Printf.printf "%s will not be shared (exception %s)"
                  full_name (Printexc2.to_string e);
                print_newline ();
          with _ -> ()
    ) files
    
let shared_add_directory dirname =
  Printf.printf "SHARING %s" dirname; print_newline ();
  shared_add_directory dirname ""
  
let shared_check_files () =
  let list = ref [] in
  H.iter (fun s ->
      let name = shared_fullname s in
      if not (Sys.file_exists name) then list := s :: !list
  ) shareds_by_num;
  List.iter (fun s -> shared_unshare s) !list;
  List.iter shared_add_directory !!shared_directories
  
let impl_shared_info impl =
  let module T = GuiTypes in
  {
    T.shared_num  = impl.impl_shared_num;
    T.shared_network = 0;
    T.shared_filename = impl.impl_shared_fullname;
    T.shared_size = impl.impl_shared_size;
    T.shared_uploaded = impl.impl_shared_uploaded;
    T.shared_requests = impl.impl_shared_requests; 
    T.shared_id = Md4.null;
  }
  
let shared_info s =
  let impl = as_shared_impl s in
  impl.impl_shared_ops.op_shared_info impl.impl_shared_val
  
    
let com_shareds_by_num = shareds_by_num
let shareds_by_num = ()
