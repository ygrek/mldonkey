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
open Printf2
open Options
open CommonTypes

let next_result_num = ref 0
    
let (store: CommonTypes.result_info Store.t) = 
  Store.create (Filename.concat CommonOptions.file_basedir "store")
  
module Document = struct
    type t = Store.index
      
    let num t = Store.index t
    let filtered t = Store.get_attrib store t
    let filter t bool = Store.set_attrib store t bool
    let doc_value t = Store.get store t
  end

let (results_by_uid : (uid_type, result) Hashtbl.t) = Hashtbl.create 1027
let (known_uids : (uid_type, int) Hashtbl.t) = Hashtbl.create 1027
let results_by_num = Hashtbl.create 1027

let set_result_name r name =
  if not (List.mem name r.result_names) then begin
      r.result_modified <- true;
      r.result_names <- r.result_names @ [name]
    end

let set_result_tag r tag =
  try
    ignore (CommonGlobals.find_tag tag.tag_name r.result_tags)
  with Not_found ->
      r.result_modified <- true;
      r.result_tags <- r.result_tags @ [tag]

let declare_result rs r uid =
  (try
      let time = Hashtbl.find known_uids uid in
      if time < r.result_time then begin
          r.result_time <- time;
          r.result_modified <- false;
          Store.update store rs.stored_result_index r;
        end
    with _ ->
        Hashtbl.add known_uids uid r.result_time);
  Hashtbl.add results_by_uid uid rs
  
let update_result_num r =
  let rec iter uids =
    match uids with
      [] ->
        r.result_num <- !next_result_num;
        r.result_modified <- false;
        let index = Store.add store r in
        let rs = { 
            stored_result_index = index;
            stored_result_num = !next_result_num;
          } in
        List.iter (fun uid -> declare_result rs r (Uid.to_uid uid)) 
        r.result_uids;
        Hashtbl.add results_by_num r.result_num rs;
        incr next_result_num;
        rs
    | uid :: tail ->
        let uid = Uid.to_uid uid in
        try
          let rs = Hashtbl.find results_by_uid uid in
          let rr = Store.get store rs.stored_result_index in
          List.iter (set_result_name rr) r.result_names;
          List.iter (set_result_tag rr) r.result_tags;
          rs
        with Not_found -> iter tail
  in
  iter r.result_uids

let find_result num =
  Hashtbl.find results_by_num num
  
let get_result rs =
  Store.get store rs.stored_result_index
  
let dummy_result = {
    result_num = 0;
    result_uids = [];
    result_names = [];
    result_format = "";
    result_type = "";
    result_tags = [];
    result_comment = "";
    result_done = false;
    result_size = zero;
    result_modified = true;
    result_time = BasicSocket.last_time ();
  }
  
let result_download rs names force =
  let r = Store.get store rs.stored_result_index in
  let files = ref [] in
  CommonNetwork.networks_iter (fun n ->
      files := (n.op_network_download r) :: !files
  );
  !files  
    
let results_iter f =
  Hashtbl.iter (fun _ r -> 
(*      let r = Store.get store rs.stored_result_index in *)
      f r.stored_result_num r) 
  results_by_num

let update_result r =
  let num = r.result_num in
  let rs = Hashtbl.find results_by_num num in
  r.result_modified <- false;
  Store.update store rs.stored_result_index r

let update_result2 rs r =
  if r.result_modified then begin
      r.result_modified <- false;
      Store.update store rs.stored_result_index r
    end
  
  
let _ = 
  Heap.add_memstat "CommonResult" (fun level buf ->
      let counter = ref 0 in
      Hashtbl.iter (fun _ _ -> incr counter) results_by_num;
      Printf.bprintf buf "  results_by_num: %d\n" !counter;

      let counter = ref 0 in
      Hashtbl.iter (fun _ _ -> incr counter) results_by_uid;
      Printf.bprintf buf "  results_by_uid: %d\n" !counter;

      let counter = ref 0 in
      Hashtbl.iter (fun _ _ -> incr counter) known_uids;
      Printf.bprintf buf "  known_uids: %d\n" !counter;
      
      let in_mem, total = Store.stats store in
      Printf.bprintf buf "  store: %d loaded/ %d max\n" in_mem total
  )
