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


open CommonClient
open Options
open CommonTypes
  
type 'a file_impl = {
    mutable impl_file_update : bool;
    mutable impl_file_state : file_state;
    mutable impl_file_num : int;
    mutable impl_file_val : 'a;
    mutable impl_file_ops : 'a file_ops;
  }
  
and 'a file_ops = {
    mutable op_file_network : network;
    mutable op_file_commit : ('a -> unit);
    mutable op_file_save_as : ('a -> string -> unit);
(*    mutable op_file_print : ('a -> CommonTypes.connection_options -> unit); *)
    mutable op_file_to_option : ('a -> (string * option_value) list);
    mutable op_file_cancel : ('a -> unit);
    mutable op_file_pause : ('a -> unit);
    mutable op_file_resume : ('a -> unit);
    mutable op_file_info : ('a -> Gui_proto.file_info);
    mutable op_file_disk_name : ('a -> string);
    mutable op_file_best_name : ('a -> string);
    mutable op_file_state : ('a -> CommonTypes.file_state);
    mutable op_file_set_format : ('a -> CommonTypes.format -> unit);
    mutable op_file_check : ('a -> unit);
    mutable op_file_recover : ('a -> unit);
    mutable op_file_sources : ('a -> client list);
  }
  
let file_num = ref 0
let files_by_num = Hashtbl.create 1027
  
  

let ni n m = 
  let s = Printf.sprintf "File.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m = failwith (ni n m)
let ni_ok n m = ignore (ni n m)

  
let as_file  (file : 'a file_impl) =
  let (file : file) = Obj.magic file in
  file
  
let as_file_impl  (file : file) =
  let (file : 'a file_impl) = Obj.magic file in
  file

let files_update_list = ref []
  
let file_must_update file =
  let impl = as_file_impl file in
  if not impl.impl_file_update then
    begin
      impl.impl_file_update <- true;
      files_update_list := file :: !files_update_list
    end

let update_file_num impl =
  if impl.impl_file_num = 0 then begin
      incr file_num;
      impl.impl_file_num <- !file_num;
      Hashtbl.add files_by_num !file_num (as_file impl);
      file_must_update (as_file impl);
    end

  
let file_num file =
  let impl = as_file_impl  file in
  impl.impl_file_num
    
let update_file_state impl state =
  impl.impl_file_state <- state;
  file_must_update (as_file impl)
  
let file_to_option (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_to_option file.impl_file_val

  (*
let file_print (file : file) buf =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_print file.impl_file_val buf
    *)

let file_save_as (file : file) name =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_save_as file.impl_file_val name

let file_network (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_network

let file_info (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_info file.impl_file_val

let file_pause (file : file) =
  let file = as_file_impl file in
  if file.impl_file_state = FileDownloading then begin
      update_file_state file FilePaused;
      file.impl_file_ops.op_file_pause file.impl_file_val
    end

let file_resume (file : file) =
  let file = as_file_impl file in
  if file.impl_file_state = FilePaused then begin
      update_file_state file FileDownloading;
      file.impl_file_ops.op_file_resume file.impl_file_val
    end

let file_disk_name (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_disk_name file.impl_file_val

let file_best_name (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_best_name file.impl_file_val

let file_state (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_state file.impl_file_val

let file_set_format (file : file) format =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_set_format file.impl_file_val format


let file_check (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_check file.impl_file_val


let file_recover (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_recover file.impl_file_val

let file_sources file =
  let impl = as_file_impl file in
  impl.impl_file_ops.op_file_sources impl.impl_file_val
  
let new_file_ops network = {
    op_file_network =  network;
    op_file_commit = (fun _ -> ni_ok network "file_commit");
    op_file_save_as = (fun _ _ -> ni_ok network "file_save_as");
(*    op_file_print = (fun _ _ -> ni_ok network "file_print"); *)
    op_file_to_option = (fun _ -> fni network "file_to_option");
    op_file_cancel = (fun _ -> ni_ok network "file_cancel");
    op_file_info = (fun _ -> fni network "file_info");
    op_file_pause = (fun _ -> ni_ok network "file_pause");
    op_file_state = (fun _ -> fni network "file_state");
    op_file_resume = (fun _ -> ni_ok network "file_resume");
    op_file_disk_name = (fun _ -> fni network "file_disk_name");
    op_file_best_name = (fun _ -> fni network "file_best_name");
    op_file_check = (fun _ -> ni_ok network "file_check");
    op_file_recover = (fun _ -> ni_ok network "file_recover");
    op_file_set_format = (fun _ -> fni network "file_set_format");
    op_file_sources = (fun _ -> fni network "file_sources");
  }

let file_find num = 
  Hashtbl.find files_by_num num

let file_state file =
  let impl = as_file_impl file in
  impl.impl_file_state  
  
  
let file_new_sources = ref []
    
let file_new_source file c =
  client_must_update c;
  file_new_sources := (file_num file, 
    CommonClient.client_num c) :: !file_new_sources  

  
let com_files_by_num = files_by_num
let files_by_num = ()

  (*
    file_ops.op_file_print <- (fun file o ->
      let buf = o.conn_buf in
      let f = file.file_result.result_file in
      Printf.bprintf buf "[Opennap %5d] %-50s %10s %10s\n" 
      (file_num file) f.file_name 
      (Int32.to_string f.file_size)
      (Int32.to_string file.file_downloaded)      
);

    file_ops.op_file_print <- (fun file o ->
      let buf = o.conn_buf in
      let f = file.file_result.result_file in
      Printf.bprintf buf "[LimeWire %5d] %-50s %10s %10s\n" 
        (file_num file) f.file_name 
        (Int32.to_string f.file_size)
      (Int32.to_string file.file_downloaded)      
  );
  file_ops.op_file_print <- (fun file o ->
  );
file_ops.op_file_print <- (fun f o ->
      let buf = o.conn_buf in
      Printf.bprintf buf "[Donkey %5d] %-50s %10s %10s\n" 
        (file_num f) (first_name f) 
        (Int32.to_string f.file_size)
      (Int32.to_string f.file_downloaded)      
  );
*)

module G = Gui_proto
let file_print file o = 
  let impl = as_file_impl file in
  let info = file_info file in
  let n = impl.impl_file_ops.op_file_network in
  let buf = o.conn_buf in
  
  Printf.bprintf buf "[%-s %5d] %-50s %10s %10s\n" 
    n.network_name (file_num file) (match info.G.file_names with
      [] -> Md4.to_string info.G.file_md4
    | name :: _ -> name)
  (Int32.to_string info.G.file_size)
  (Int32.to_string info.G.file_downloaded)      
  
  
  