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
open CommonClient
open Options
open CommonTypes
  
type 'a file_impl = {
    mutable impl_file_update : int;
    mutable impl_file_state : file_state;

    mutable impl_file_num : int;
    mutable impl_file_val : 'a;
    mutable impl_file_ops : 'a file_ops;
    mutable impl_file_size : int64;
    mutable impl_file_age : int;
    mutable impl_file_fd : Unix32.t;
    mutable impl_file_downloaded : int64;
    mutable impl_file_last_downloaded : (int64 * int) list;
    mutable impl_file_last_rate : float;
    mutable impl_file_best_name : string;
    mutable impl_file_priority: int;
    mutable impl_file_last_seen : int;
  }


and 'a file_ops = {
    mutable op_file_network : network;
    
(* This method is called just before the file is moved to the incomming
section, and thus, before it is shared. *)
    mutable op_file_commit : ('a -> string -> unit);
    
(* This method is called when the name under which the file should be saved
has been changed. The method should not perform the move, just know that
it will happen soon. *)
    mutable op_file_save_as : ('a -> string -> unit);
    mutable op_file_to_option : ('a -> (string * option_value) list);
    mutable op_file_cancel : ('a -> unit);
    mutable op_file_pause : ('a -> unit);
    mutable op_file_resume : ('a -> unit);
    mutable op_file_info : ('a -> GuiTypes.file_info);
    mutable op_file_set_format : ('a -> CommonTypes.format -> unit);
    mutable op_file_check : ('a -> unit);
    mutable op_file_recover : ('a -> unit);
    mutable op_file_sources : ('a -> client list);
    mutable op_file_comment : ('a -> string);
  }

  
  
  

  
let as_file  (file : 'a file_impl) =
  let (file : file) = Obj.magic file in
  file
  
let as_file_impl  (file : file) =
  let (file : 'a file_impl) = Obj.magic file in
  file
  
let file_num file =
  let impl = as_file_impl  file in
  impl.impl_file_num

let dummy_file_impl = {
    impl_file_update = 1;
    impl_file_state = FileNew;
    impl_file_num = 0;
    impl_file_val = 0;
    impl_file_ops = Obj.magic 0;
    impl_file_size = Int64.zero;
    impl_file_age = 0;
    impl_file_fd = Unix32.create "" [Unix.O_RDONLY] 0o666;
    impl_file_downloaded = Int64.zero;
    impl_file_last_downloaded = [];
    impl_file_last_rate = 0.0;
    impl_file_best_name = "<UNKNOWN>";
    impl_file_priority = 0;
    impl_file_last_seen = 0;
  }
  
let dummy_file = as_file dummy_file_impl  

module H = Weak2.Make(struct
      type t = file
      let hash file = Hashtbl.hash (file_num file)
      
      let equal x y  = (file_num x) = (file_num y)
    end)

let file_counter = ref 0
let files_by_num = H.create 1027
  
  

let ni n m = 
  let s = Printf.sprintf "File.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m = failwith (ni n m)
let ni_ok n m = ignore (ni n m)

let file_must_update file =
  let impl = as_file_impl file in
  if impl.impl_file_update <> 0 then
    CommonEvent.add_event (File_info_event file);
  impl.impl_file_update <- 0

let file_must_update_downloaded file =
  let impl = as_file_impl file in
  if impl.impl_file_update > 0 then
    begin
      impl.impl_file_update <- - impl.impl_file_update;
      CommonEvent.add_event (File_info_event file);
    end

let update_file_num impl =
  if impl.impl_file_num = 0 then begin
      incr file_counter;
      impl.impl_file_num <- !file_counter;
      H.add files_by_num (as_file impl);
      file_must_update (as_file impl);
    end
    
    
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

let file_comment (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_comment file.impl_file_val

let file_network (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_network

let file_info (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_info file.impl_file_val

let file_pause (file : file) =
  let file = as_file_impl file in
  match file.impl_file_state with
    FileDownloading ->
      update_file_state file FilePaused;
      file.impl_file_ops.op_file_pause file.impl_file_val
  | _ -> ()

let file_resume (file : file) =
  let file = as_file_impl file in
  if file.impl_file_state = FilePaused then begin
      update_file_state file FileDownloading;
      file.impl_file_ops.op_file_resume file.impl_file_val
    end

let file_best_name (file : file) =
  let file = as_file_impl file in
  file.impl_file_best_name
  
let set_file_best_name file name = 
  let file = as_file_impl file in
  let name = String2.replace name '/' "::" in
  file.impl_file_best_name <- name

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
  try impl.impl_file_ops.op_file_sources impl.impl_file_val with _ -> []

let files_ops = ref []
  
let new_file_ops network = 
  let f = 
    {
      op_file_network =  network;
      op_file_commit = (fun _ _ -> ni_ok network "file_commit");
      op_file_save_as = (fun _ _ -> ni_ok network "file_save_as");
(*    op_file_print = (fun _ _ -> ni_ok network "file_print"); *)
      op_file_to_option = (fun _ -> fni network "file_to_option");
      op_file_cancel = (fun _ -> ni_ok network "file_cancel");
      op_file_info = (fun _ -> fni network "file_info");
      op_file_pause = (fun _ -> ni_ok network "file_pause");
      op_file_resume = (fun _ -> ni_ok network "file_resume");
(*      op_file_disk_name = (fun _ -> fni network "file_disk_name"); *)
      op_file_check = (fun _ -> ni_ok network "file_check");
      op_file_recover = (fun _ -> ni_ok network "file_recover");
      op_file_set_format = (fun _ -> fni network "file_set_format");
      op_file_sources = (fun _ -> fni network "file_sources");
      op_file_comment = (fun _ -> ni_ok network "file_comment"; "");
    }
  in
  let ff = (Obj.magic f : int file_ops) in
  files_ops := (ff, { ff with op_file_network = ff.op_file_network })
  :: ! files_ops;
  f

let check_file_implementations () =
  Printf.printf "\n---- Methods not implemented for CommonFile ----\n";
  print_newline ();
  List.iter (fun (c, cc) ->
      let n = c.op_file_network.network_name in
      Printf.printf "\n  Network %s\n" n; print_newline ();
      if c.op_file_to_option == cc.op_file_to_option then 
        Printf.printf "op_file_to_option\n";
      if c.op_file_info == cc.op_file_info then
        Printf.printf "op_file_info\n";
      if c.op_file_commit == cc.op_file_commit then
        Printf.printf "op_file_commit\n";
      if c.op_file_save_as == cc.op_file_save_as then
        Printf.printf "op_file_save_as\n";
      if c.op_file_cancel == cc.op_file_cancel then
        Printf.printf "op_file_cancel\n";
      if c.op_file_pause == cc.op_file_pause then
        Printf.printf "op_file_pause\n";
      if c.op_file_resume == cc.op_file_resume then
        Printf.printf "op_file_resume\n";
(*      if c.op_file_disk_name == cc.op_file_disk_name then
        Printf.printf "op_file_disk_name\n"; *)
      if c.op_file_check == cc.op_file_check then
        Printf.printf "op_file_check\n";
      if c.op_file_recover == cc.op_file_recover then
        Printf.printf "op_file_recover\n";
      if c.op_file_set_format == cc.op_file_set_format then
        Printf.printf "op_file_set_format\n";
      if c.op_file_sources == cc.op_file_sources then
        Printf.printf "op_file_sources\n";
  ) !files_ops;
  print_newline () 

  
let file_find num = 
  H.find files_by_num (as_file {
    dummy_file_impl   with impl_file_num = num
  })

let file_state file =
  let impl = as_file_impl file in
  impl.impl_file_state  
  
let file_add_source (file : file) c =
  client_must_update c;
  CommonEvent.add_event (File_add_source_event (file,c))
  
let file_remove_source (file : file) c =
  CommonEvent.add_event (File_remove_source_event (file,c))

let rec last = function
    [x] -> x
  | _ :: l -> last l
  | _ -> (Int64.zero, 0)
    
let sample_timer () =
  let trimto list length =
    let (list, _) = List2.cut length list in
    list 
  in
  let time = BasicSocket.last_time () in
  H.iter (fun file ->
      let impl = as_file_impl file in
      impl.impl_file_last_downloaded <-
        trimto ((impl.impl_file_downloaded, time) :: 
        impl.impl_file_last_downloaded) 
      !!CommonOptions.download_sample_size;
      match impl.impl_file_last_downloaded with
        _ :: (last_downloaded, _) :: _ ->
          if last_downloaded = impl.impl_file_downloaded &&
            impl.impl_file_last_rate > 0. then
            file_must_update_downloaded file
      | _ -> ()

  ) files_by_num

let file_download_rate impl =
  let time = BasicSocket.last_time () in
  let (last_downloaded, file_last_time) = last impl.impl_file_last_downloaded in
  let time = time - file_last_time in
  let diff = Int64.sub impl.impl_file_downloaded last_downloaded in
  let rate = if time > 0 && diff > Int64.zero then begin
        (Int64.to_float diff) /. (float_of_int time);
      end else 0.0
  in
  impl.impl_file_last_rate <- rate;
  rate
  
let add_file_downloaded impl n =
  impl.impl_file_downloaded <- Int64.add impl.impl_file_downloaded n;
  file_must_update_downloaded (as_file impl)
    

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

module G = GuiTypes
let file_print file o = 
  let impl = as_file_impl file in
  let info = file_info file in
  let n = impl.impl_file_ops.op_file_network in
  let buf = o.conn_buf in
  
  Printf.bprintf buf "[%-s %5d] %-50s %10s %10s\n" 
    n.network_name (file_num file) (match info.G.file_names with
      [] -> Md4.to_string info.G.file_md4
    | name :: _ -> name)
  (Int64.to_string info.G.file_size)
  (Int64.to_string info.G.file_downloaded);

  Printf.bprintf buf "Chunks: [%-s]\n" info.G.file_chunks;
  
  List.iter (fun name -> 
      Printf.bprintf buf "    (%s)\n" name) info.G.file_names;
  (try
      
      let srcs = file_sources file in
      Printf.bprintf buf "%d sources:\n" (List.length srcs);
      List.iter (fun c ->
          client_bprint buf c
      ) srcs
    with _ -> ())

let file_size file = 
  (as_file_impl file).impl_file_size
  
let file_disk_name file =
  Unix32.filename (as_file_impl file).impl_file_fd
      
let file_fd file =
  (as_file_impl file).impl_file_fd

let set_file_disk_name file filename=
  Unix32.set_filename (file_fd file) filename
  
let file_downloaded file = 
  (as_file_impl file).impl_file_downloaded  

let file_network file =
  (as_file_impl file).impl_file_ops.op_file_network

let file_priority file = 
  (as_file_impl file).impl_file_priority
  
let set_file_priority file p = 
  (as_file_impl file).impl_file_priority <- p
  