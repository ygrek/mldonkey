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
open CommonTypes

module StoredResult = struct
    type result = CommonTypes.result_info
    type stored_result = CommonTypes.result
    type search = CommonTypes.search
    
    let store_name = "store"
    let result_names r = r.result_names
    let result_size r = r.result_size
    let result_uids r = r.result_uids
    let result_tags r = r.result_tags
      
    let result_index r = r.stored_result_index
      
    let search_query s = s.search_query
      
  end
  
module IndexedResults = CommonIndexing.Make(StoredResult)
  
let next_result_num = ref 0

let (results_by_uid : (uid_type, result) Hashtbl.t) = Hashtbl.create 1027
let (known_uids : (uid_type, int) Hashtbl.t) = Hashtbl.create 1027
let results_by_num = Hashtbl.create 1027

let set_result_name r name =
  if not (List.mem name r.result_names) then begin
      r.result_modified <- true;
      r.result_names <- r.result_names @ [name]
    end

let int64_of_tagvalue v = 
  match v with 
   | Uint64 n -> n
   | _ -> 0L

(* Update specific tags to highest value *)
let rec find_tag2 new_tag tags =
  match tags with
    [] -> raise Not_found
  | tag :: tail -> begin
    match tag with 
      { tag_name = (Field_Availability | Field_Completesources); tag_value = tag_value } 
        when tag.tag_name = new_tag.tag_name ->
            let x = int64_of_tagvalue tag_value in
            let y = int64_of_tagvalue new_tag.tag_value in
            if y > x then tag.tag_value <- Uint64 y;
            true
    | { tag_name = tag_name; tag_value = _ } when tag_name = new_tag.tag_name -> false
    | _ -> find_tag2 new_tag tail
  end

let set_result_tag r tag =
  try
    let updated = find_tag2 tag r.result_tags in
    if updated then r.result_modified <- true;
  with Not_found ->
      r.result_modified <- true;
      r.result_tags <- r.result_tags @ [tag]

let declare_result rs r uid =
  (try
      let time = Hashtbl.find known_uids uid in
      if time < r.result_time then begin
          r.result_time <- time;
          r.result_modified <- false;
          IndexedResults.update_result rs r;
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
        let index = IndexedResults.add r in
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
          let rr = IndexedResults.get_result rs in
          List.iter (set_result_name rr) r.result_names;
          List.iter (set_result_tag rr) r.result_tags;
          IndexedResults.update_result rs rr;
          rs
        with Not_found -> iter tail
  in
  iter r.result_uids

let find_result num =
  Hashtbl.find results_by_num num
    
let dummy_result = {
    result_num = 0;
    result_uids = [];
    result_names = [];
    result_format = "";
    result_type = "";
    result_tags = [];
    result_comment = "";
    result_done = false;
    result_force = false;
    result_size = zero;
    result_modified = true;
    result_time = BasicSocket.last_time ();
    result_source_network = 0;
  }
  
let result_download rs names force user =
  let r = IndexedResults.get_result rs in
  let files = ref [] in
  CommonNetwork.networks_iter (fun n ->
      (* Temporarily download results only from the network that returned the result *)
      if (n.network_num = r.result_source_network) then
      files := (CommonNetwork.network_download n r user user.user_default_group) :: !files
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
  IndexedResults.update_result rs r

let update_result2 rs r =
  if r.result_modified then begin
      r.result_modified <- false;
      IndexedResults.update_result rs r
    end

let rec find_avail tags =
  match tags with
    [] -> raise Not_found
  | tag :: tail -> begin
    match tag with
      { tag_name = Field_Availability ; tag_value = tag_value } ->
      tag
    | _ -> find_avail tail
  end

let increment_avail r =
  let rr = IndexedResults.get_result r in
  begin
    try
      let tag = find_avail rr.result_tags in
      let x = int64_of_tagvalue tag.tag_value in
      tag.tag_value <- Uint64 (x ++ 1L);
     with Not_found -> ();
    end;
  update_result_num rr

let update_or_create_avail tags =
  let tag =
    try
      let tag = find_avail tags in
      let x = int64_of_tagvalue tag.tag_value in
      tag.tag_value <- Uint64 (x ++ 1L);
      tag
   with Not_found ->
      { tag_name = Field_Availability; tag_value = Uint64 1L } 
  in
  tag :: tags
  
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
      
      let in_mem, total = IndexedResults.stats () in
      Printf.bprintf buf "  store: %d loaded/ %d max\n" in_mem total
  )
