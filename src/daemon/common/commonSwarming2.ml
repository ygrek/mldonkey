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
open Int64ops
open Options
open Printf2
  
open CommonOptions
open CommonSwarming

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         MODULE Make                                   *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(* let debug_all = true *)
let exit_on_error = ref false
  
open CommonTypes
  
module Make(M: CommonEnv) = struct

    open M
    open CommonFile
    open CommonTypes
    open CommonClient
      
        
let ( ** ) x y = Int64.mul x (Int64.of_int y)

(* Worst scenario?: 1 GB splitted in small ranges of 64 KB = 16 000 ranges.
  In eDonkey, ranges are 180 kB long.
*)

(* If we want to become 'multinet', we should:
* there shouldn't be any block_size, instead blocks should correspond
  to the largest blocks which are completely included in one block for
  every network.
* the range_size should be the same for all networks.
* a completed block should not be verified until all the blocks are 
   completed for all networks.
* swarmers should be loaded before other files, so that they can be
used and shared by different files.

* Verification:
  - must_verify_chunk

TODO: s_last_seen is useless, only t_last_seen is useful, at least in the
  first version.

*)


type chunk = {
    chunk_uid : uid_type;
    chunk_size : int64;
  }

type t = {
    mutable t_primary : bool;
    t_file : file;
    mutable t_s : swarmer;
    t_block_size : int64;
    
    t_nchunks : int;
    mutable t_converted_verified_bitmap : string;
    mutable t_last_seen : int array;
    mutable t_ncomplete_blocks : int;
    mutable t_nverified_blocks : int;    
    
    mutable t_verifier : verification;
    mutable t_verified : (int -> int -> unit);
    
(* conversion from network blocks to swarmer blocks *)
    mutable t_t2s_blocks : int list array;    
(* conversion from swarmer blocks to network blocks *)
    mutable t_s2t_blocks : int array;    
  }

and swarmer = {
    mutable s_num : int;
    mutable s_filename : string;
    
    mutable s_networks : t list;
    mutable s_size : int64;
    mutable s_range_size : int64;
    mutable s_strategy : strategy;
    
    mutable s_verified_bitmap : string;
    mutable s_availability : int array;
    mutable s_nuploading : int array;
(*    mutable s_last_seen : int array; *)
    
    mutable s_blocks : block_v array;
    mutable s_block_pos : int64 array;    
  }
  
and block_v = 
  EmptyBlock
| PartialBlock of block
| CompleteBlock
| VerifiedBlock

and block = {
    block_s : swarmer;
    mutable block_num : int;
    mutable block_begin : Int64.t;
    mutable block_end : Int64.t;
    mutable block_ranges : range;
    mutable block_remaining : int64;
  }

and range = {
    mutable range_block : block;
    mutable range_begin : Int64.t; (* official begin int64 *)
    mutable range_end : Int64.t;
    mutable range_prev : range option;
    mutable range_next : range option;
    mutable range_current_begin : Int64.t; (* current begin pos *)
(*        mutable range_verified : bool; *)
    mutable range_nuploading : int;
  }

and uploader = {
    up_t : t;
    up_client : client;
    
    mutable up_declared : bool;
    
    mutable up_chunks : chunks;
    mutable up_complete_blocks : int array;
    mutable up_ncomplete : int;
    
    mutable up_partial_blocks : (int * int64 * int64) array;
    mutable up_npartial : int;
    
    mutable up_block : block option;
    mutable up_block_begin : int64;
    mutable up_block_end : int64;
    
    mutable up_ranges : (int64 * int64 * range) list;
  }


(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)

module HS = Weak2.Make(struct
      type t = swarmer
      let hash file = Hashtbl.hash file.s_filename
      
      let equal x y  = x.s_filename = y.s_filename
    end)

let swarmers_by_name = HS.create 31

module HU = Weak2.Make(struct
      type t = uploader
      let hash u = Hashtbl.hash (client_num u.up_client)
      
      let equal x y  =  (client_num x.up_client) =  (client_num y.up_client)
    end)

let uploaders_by_num = HU.create 113

let edonkey_range_size = Int64.of_int (180 * 1024)  
  
let swarmer_counter = ref 0
    
let has_multinet = true

(*************************************************************************)
(*                                                                       *)
(*                         dummy_swarmer                                 *)
(*                                                                       *)
(*************************************************************************)

let dummy_swarmer = {    
    s_num = 0;
    s_filename = "";
    s_networks = [];
    s_size = zero;
    s_range_size = zero;
    s_strategy = AdvancedStrategy;
    s_verified_bitmap = "";
    s_blocks = [||];
    s_block_pos = [||];
    s_availability = [||];
    s_nuploading = [||];
  }

(*************************************************************************)
(*                                                                       *)
(*                         print_uploader                                *)
(*                                                                       *)
(*************************************************************************)

let print_uploader up =
  lprintf "  interesting complete_blocks: %d\n     " up.up_ncomplete;
  Array.iter (fun i -> lprintf " %d " i) up.up_complete_blocks;
  lprintf "  interesting partial_blocks: %d\n     " up.up_npartial;
  Array.iter (fun (i, begin_pos, end_pos) -> 
      lprintf " %d[%s...%s] " i 
        (Int64.to_string begin_pos)
      (Int64.to_string end_pos)) up.up_partial_blocks

(*************************************************************************)
(*                                                                       *)
(*                         last_seen                                     *)
(*                                                                       *)
(*************************************************************************)

let compute_last_seen t = 
  let last_seen_total = ref (BasicSocket.last_time ()) in
  for i = 0 to String.length t.t_converted_verified_bitmap - 1 do
    if t.t_converted_verified_bitmap.[i] > '2' then
      t.t_last_seen.(i) <- BasicSocket.last_time ()
    else
      last_seen_total := min !last_seen_total t.t_last_seen.(i)
  done;
  set_file_last_seen t.t_file !last_seen_total;
  t.t_last_seen
  
(*************************************************************************)
(*                                                                       *)
(*                         create_swarmer                                *)
(*                                                                       *)
(*************************************************************************)

let create_swarmer file_name file_size range_size =
  
  try
    HS.find swarmers_by_name 
      { dummy_swarmer with 
      s_filename = file_name 
    }
  with Not_found ->
      incr swarmer_counter;

(* Let be VERY conservative... *)
      let range_size = edonkey_range_size in
      
      let nchunks = 1 in
      let rec s = {
          
          s_num = !swarmer_counter;
          s_filename = file_name;
          
          s_networks = [];
          
          s_size = file_size;
          s_range_size = range_size;
          s_strategy = AdvancedStrategy;
          
          s_verified_bitmap = String.make nchunks '0';
          s_blocks = Array.create nchunks EmptyBlock ;
          s_block_pos = Array.create nchunks zero;
          s_availability = Array.create nchunks 0;
          s_nuploading = Array.create nchunks 0;
(*      s_last_seen = Array.create nchunks 0; *)
        }
      in 
      HS.add swarmers_by_name s;
      s
      
(*************************************************************************)
(*                                                                       *)
(*                         compute_block_end (internal)                  *)
(*                                                                       *)
(*************************************************************************)

let compute_block_end s i =
  let b = s.s_block_pos in
  if Array.length b = i + 1 then
    s.s_size
  else
    b.(i+1)
  
(*************************************************************************)
(*                                                                       *)
(*                         compute_block_end (internal)                  *)
(*                                                                       *)
(*************************************************************************)

let compute_block_begin s i =
  let b = s.s_block_pos in
  b.(i)
  
(*************************************************************************)
(*                                                                       *)
(*                         void_range (internal)                         *)
(*                                                                       *)
(*************************************************************************)
  
let void_range b pos =
  let r = {
      range_prev = None;
      range_next = None;
      range_begin = pos;
      range_end = pos;
      range_block = b;
      range_nuploading = 0;
      range_current_begin = pos;
    }
  in
  r
  
(*************************************************************************)
(*                                                                       *)
(*                         cut_ranges (internal)                         *)
(*                                                                       *)
(*************************************************************************)

let rec own_ranges b r =
  r.range_block <- b;
  match r.range_next with
    None -> ()
  | Some r -> own_ranges b r
  
(*************************************************************************)
(*                                                                       *)
(*                         get_after_ranges (internal)                   *)
(*                                                                       *)
(*************************************************************************)

let rec get_after_ranges b r cut_pos =
  if r.range_begin >= cut_pos then begin
      r.range_prev <- None;
      own_ranges b r;
      r 
    end else
  if r.range_end <= cut_pos then
    match r.range_next with
      None -> void_range b cut_pos
    | Some r -> get_after_ranges b r cut_pos
  else
  let r = { r with 
      range_prev = None;
      range_begin = cut_pos;
      range_current_begin = max r.range_current_begin cut_pos
    } in
  own_ranges b r;
  r

    
(*************************************************************************)
(*                                                                       *)
(*                         get_before_ranges (internal)                  *)
(*                                                                       *)
(*************************************************************************)

let get_before_ranges b r cut_pos =
  if r.range_current_begin >= cut_pos then
    void_range b cut_pos
  else
  let rec iter b r cut_pos =
    if r.range_end > cut_pos then begin
        r.range_current_begin <- min cut_pos r.range_current_begin;
        r.range_end <- cut_pos;
        r.range_next <- None;
      end else 
    if r.range_end = cut_pos then
      r.range_next <- None
    else
    match r.range_next with
      None -> ()
    | Some rr ->
        if rr.range_current_begin >= cut_pos then
          r.range_next <- None
        else
          iter b rr cut_pos
  in
  iter b r cut_pos;
  r

(*************************************************************************)
(*                                                                       *)
(*                         empty_block                                   *)
(*                                                                       *)
(*************************************************************************)

let empty_block b =
  let rec iter begin_pos r =
    r.range_current_begin = begin_pos &&
    (match r.range_next with
        Some rr -> iter r.range_end rr
      | None -> r.range_end = b.block_end)
  in
  iter b.block_begin b.block_ranges

(*************************************************************************)
(*                                                                       *)
(*                         create                                        *)
(*                                                                       *)
(*************************************************************************)

let split_blocks s chunk_size =

  let size = s.s_size in
  
  let nblocks = Array.length s.s_blocks in
  let rec iter index_s chunk_begin new_blocks =
(*    lprintf "iter (1) %d/%d %Ld\n" index_s nblocks chunk_begin; *)
    if index_s = nblocks then List.rev new_blocks else
    let block_begin = compute_block_begin s index_s in
    let block_end = compute_block_end s index_s in
    
    let chunk_end = chunk_begin ++ chunk_size in
    let chunk_end = min chunk_end size in
    
    if chunk_end > block_end then begin
        
        let new_blocks = (
            s.s_blocks.(index_s), 
            block_begin,
            s.s_verified_bitmap.[index_s]
          ) :: new_blocks in
        iter (index_s+1) chunk_begin new_blocks
      
      end else
    
    if chunk_end = block_end then begin
        
        let new_blocks =  (
            s.s_blocks.(index_s), 
            block_begin,
            s.s_verified_bitmap.[index_s]
          ) :: new_blocks in
        iter (index_s+1) chunk_end new_blocks
      
      end else begin

(* We need to split this block in two parts *)
        
        s.s_block_pos.(index_s) <- chunk_end;
        match s.s_blocks.(index_s) with
          EmptyBlock | CompleteBlock | VerifiedBlock ->
            
            let new_blocks =  (
                s.s_blocks.(index_s), 
                block_begin,
                s.s_verified_bitmap.[index_s]
              ) :: new_blocks in
            iter index_s chunk_end new_blocks
        
        | PartialBlock b1 ->
            
            let b2 = {
                block_s = s;
                
                block_begin = chunk_end;
                block_end = b1.block_end;
                block_ranges = b1.block_ranges;
                block_num = index_s + 1;
                block_remaining = zero;
              } in
            b1.block_end <- chunk_end;
            
            b2.block_ranges <- get_after_ranges b2 b2.block_ranges chunk_end;
            b1.block_ranges <- get_before_ranges b1 b1.block_ranges chunk_end;
            
            
            if empty_block b2 then begin
(* lprintf "Partial block b2 should become EmptyBlock\n"; *)
                s.s_blocks.(index_s) <- EmptyBlock;
                s.s_verified_bitmap.[index_s] <- '0';
              end else begin
                s.s_blocks.(index_s) <- PartialBlock b2;
              end;
            
            let new_blocks = 
              (if empty_block b1 then 
(* lprintf "Partial block b1 should become EmptyBlock\n"; *)
                  (
                    EmptyBlock,
                    block_begin,
                    '0'
                  ) else (
                    PartialBlock b1,
                    block_begin,
                    s.s_verified_bitmap.[index_s]
                  ))
              :: new_blocks in
            iter index_s chunk_end new_blocks
      
      end
  
  
  in
  let blocks = iter 0 zero [] in
  
  let nblocks = List.length blocks in
(*  lprintf "%d blocks to generate\n" nblocks; *)
  
  s.s_blocks <- Array.create nblocks EmptyBlock;
  s.s_verified_bitmap <- String.make nblocks '0';
  s.s_block_pos <- Array.create nblocks zero;
  s.s_availability <- Array.create nblocks 0;
  s.s_nuploading <- Array.create nblocks 0;
(*  s.s_last_seen <- Array.create nblocks 0; *)
  
  let rec iter i list =
    match list with
      [] -> ()
    | (b, pos, c) :: tail ->
        begin
          match b with 
            PartialBlock b -> b.block_num <- i
          | _ -> ()
        end;
        s.s_blocks.(i) <- b;
        s.s_verified_bitmap.[i] <- c;
        s.s_block_pos.(i) <- pos;
        
        iter (i+1) tail
  in
  iter 0 blocks
  
(*************************************************************************)
(*                                                                       *)
(*                         associate                                     *)
(*                                                                       *)
(*************************************************************************)
  
let associate primary t s =
  
  if not (List.memq t s.s_networks) then
  let size = file_size t.t_file in
    
    begin
      if t.t_s != s then begin
          t.t_s.s_networks <- [];
        end;
    end;
  
  assert (s.s_size = size);
  
  t.t_s <- s;  
  t.t_converted_verified_bitmap <- String.make t.t_nchunks '0';
  t.t_last_seen <- Array.create t.t_nchunks 0;
  t.t_s2t_blocks <- [||];
  t.t_t2s_blocks <- Array.create t.t_nchunks [];
  
  if primary then begin    
        t.t_primary <- true;
        s.s_networks <- t :: s.s_networks;  
    end else begin
        t.t_primary <- false;
        s.s_networks <- s.s_networks @ [t];        
        Unix32.remove (file_fd t.t_file);
    end;
(* at this point, we are supposed to split the blocks in the swarmer
in smaller blocks depending on the block_size of this network, and compute
the t_s2t_blocks and t_t2s_blocks fields. *)
  
  let chunk_size = t.t_block_size in
 
  split_blocks s chunk_size;
  
  let nblocks = Array.length s.s_blocks in
(* For all networks, adjust the blocks *)
  List.iter (fun t ->
      let nchunks = String.length t.t_converted_verified_bitmap in
      t.t_s2t_blocks <- Array.create nblocks 0;
      t.t_t2s_blocks <- Array.create nchunks [];
      
      let chunk_size = t.t_block_size in
      for i = 0 to nblocks - 1 do
        let block_begin = compute_block_begin s i in
        let chunk = Int64.to_int (Int64.div block_begin chunk_size) in
        t.t_s2t_blocks.(i) <- chunk;
        t.t_t2s_blocks.(chunk) <- i :: t.t_t2s_blocks.(chunk)
      done
    ) s.s_networks;  
    
(* TODO: If not primary, set_file_downloaded should be called *)
  if not primary then 
      add_file_downloaded t.t_file (zero -- file_downloaded t.t_file);
    
    begin
      match s.s_networks with
        t :: tail when primary ->
          List.iter (fun tt ->
              assert (not tt.t_primary);
              set_file_fd tt.t_file (file_fd t.t_file)
          ) tail
          
      | tt :: tail when tt.t_primary ->
          assert (not primary);
          set_file_fd t.t_file (file_fd tt.t_file)
      | _ -> ()
    end;

  ()
  
(*************************************************************************)
(*                                                                       *)
(*                         create                                        *)
(*                                                                       *)
(*************************************************************************)
  
let create ss file chunk_size = 
  
  let size = file_size file in
  let nchunks = 
    1 + Int64.to_int (
      Int64.div (Int64.sub size Int64.one) chunk_size) in
  
  let rec t = {
      
      t_s = ss;
      t_primary = true;
      t_file = file;
      
      t_nchunks = nchunks;
      t_block_size = chunk_size;
      
      t_ncomplete_blocks = 0;
      t_nverified_blocks = 0;
      
      t_converted_verified_bitmap = String.make nchunks '0';
      t_last_seen = Array.create nchunks 0;
      
      t_verifier = NoVerification;
      t_verified = (fun _ _ -> ());
      
      t_s2t_blocks = [||];
      t_t2s_blocks = Array.create nchunks [];
    }
  in 
  associate true t ss;  
  t

          
(*************************************************************************)
(*                                                                       *)
(*                         clear_uploader_ranges                         *)
(*                                                                       *)
(*************************************************************************) 

let clear_uploader_ranges up = 
  List.iter (fun (_,_,r) ->
      r.range_nuploading <- r.range_nuploading - 1
  ) up.up_ranges;
  up.up_ranges <- []

(*************************************************************************)
(*                                                                       *)
(*                         clear_uploader_block                          *)
(*                                                                       *)
(*************************************************************************) 

let clear_uploader_block up = 
  match up.up_block with
    None -> ()
  | Some b ->
      up.up_block <- None;
      let num = b.block_num in
      let t = up.up_t in
      let s = t.t_s in
      s.s_nuploading.(num) <- s.s_nuploading.(num) - 1

(*************************************************************************)
(*                                                                       *)
(*                         compute_block_num (internal)                  *)
(*                                                                       *)
(*************************************************************************)

let compute_block_num s chunk_pos = 
  let b = s.s_block_pos in
  let rec iter min max =
    if min = max then min else
    let medium = (max + min) / 2 in
    let pos = b.(medium) in
    if pos > chunk_pos then
      iter min (medium - 1)
    else
    if min = medium then
      iter min (max - 1)
    else
      iter medium max
  in
  let i = iter 0 (Array.length b - 1) in
  if debug_all then  
    lprintf "%Ld is block %d [%Ld-%Ld]\n" chunk_pos i
      (compute_block_begin s i) (compute_block_end s i); 
  i

  
(*************************************************************************)
(*                                                                       *)
(*                         apply_intervals (internal)                    *)
(*                                                                       *)
(*************************************************************************)

let apply_intervals s f chunks =
  let nchunks = Array.length s.s_blocks in
  let rec iter chunks =
    match chunks with
      [] -> ()
    | (chunk_begin, chunk_end) :: tail ->
        let chunk_begin = min chunk_begin s.s_size in
        let chunk_end = min chunk_end s.s_size in
(*        lprintf "apply on %Ld-%Ld\n" chunk_begin chunk_end; *)
        if chunk_begin < chunk_end then begin
            let i0 = compute_block_num s chunk_begin in
            let block_begin = compute_block_begin s i0 in
            let rec iter_blocks i block_begin chunk_begin =
              
(*              lprintf "iter_blocks %d %Ld %Ld\n" i block_begin chunk_begin; *)
              if i < nchunks && block_begin < chunk_end then
                let block_end = compute_block_end s i in
                
                let current_end =  min block_end chunk_end in
                
                if debug_all then 
                  lprintf "Apply: %d %s-%s %s-%s\n"
                    i 
                    (Int64.to_string block_begin)
                  (Int64.to_string block_end)
                  (Int64.to_string chunk_begin)
                  (Int64.to_string current_end);
                
                f i block_begin block_end chunk_begin current_end;
                
                iter_blocks (i+1) block_end block_end
            in
            iter_blocks i0 block_begin chunk_begin;
          end;
        iter tail
  in
  iter chunks


(*************************************************************************)
(*                                                                       *)
(*                         print_s                                       *)
(*                                                                       *)
(*************************************************************************)

let print_s str s =
  lprintf "Ranges after %s:\n" str;
  
  let rec iter r =
    lprintf " %s(%s)-%s(%d)" 
      (Int64.to_string r.range_begin)
    (Int64.to_string r.range_current_begin)
    (Int64.to_string r.range_end) r.range_nuploading;
    match r.range_next with
      None -> lprintf "\n"
    | Some r -> iter r
  in
  
  Array.iteri (fun i b ->
      lprintf "   %d: " i;
      let block_begin = compute_block_begin s i in
      let block_end = compute_block_end s i in
      lprintf "%Ld - %Ld [%Ld] %c " block_begin block_end
        (block_end -- block_begin) s.s_verified_bitmap.[i];
      List.iter (fun t ->
          let j = t.t_s2t_blocks.(i) in
          lprintf "(b %d %c [" j t.t_converted_verified_bitmap.[j];
          List.iter (fun ii -> lprintf "%d " ii) t.t_t2s_blocks.(j);
          lprintf "]";
      ) s.s_networks;
      
      match b with
        PartialBlock b ->
          lprintf " [%s .. %s] --> " 
            (Int64.to_string b.block_begin)
          (Int64.to_string b.block_end);
          iter b.block_ranges
      | EmptyBlock -> lprintf "_\n"
      | CompleteBlock -> lprintf "C\n"
      | VerifiedBlock -> lprintf "V\n"
  ) s.s_blocks;
  
  lprintf "Files:\n";
  List.iter (fun t ->
      lprintf "  File num: %d\n" (file_num t.t_file);
      lprintf "  %s\n" (if t.t_primary then "primary" else "secondary");
      lprintf "  Downloaded: %Ld\n" (file_downloaded t.t_file);
      lprintf "  Bitmap: %s\n" t.t_converted_verified_bitmap
  ) s.s_networks

(*************************************************************************)
(*                                                                       *)
(*                         iter_block_ranges                             *)
(*                                                                       *)
(*************************************************************************)

let iter_block_ranges f b =
  let rec iter_range f r =
    f r;
    match r.range_next with
      None -> ()
    | Some rr -> iter_range f rr
  in
  iter_range f b.block_ranges

(*************************************************************************)
(*                                                                       *)
(*                         print_block                                   *)
(*                                                                       *)
(*************************************************************************)

let print_block b =
  lprintf "Block %d: %s-%s" 
    b.block_num
    (Int64.to_string b.block_begin)
  (Int64.to_string b.block_end);
  lprintf "\n";
  lprintf "  ranges:\n";
  let rec iter_range r =
    lprintf "   %Ld-%Ld\n" r.range_current_begin r.range_end;
    match r.range_next with
      None -> ()
    | Some rr -> iter_range rr
  in
  iter_range b.block_ranges;
  lprintf "\n"
      
(*************************************************************************)
(*                                                                       *)
(*                         add_file_downloaded                           *)
(*                                                                       *)
(*************************************************************************)

let add_file_downloaded maybe_t s size =
(*  lprintf "add_file_downloaded %Ld\n" size; *)
  match s.s_networks with
    t :: _ when t.t_primary -> 
      add_file_downloaded t.t_file size;
      begin
        match maybe_t with
          None -> ()
        | Some tt when t != tt ->
            add_file_downloaded tt.t_file size;
        | _ -> ()
      end;
      if file_downloaded t.t_file < zero then
          lprintf "ERROR: file_downloaded < zero !!!\n";
          
  | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         close_ranges (internal)                       *)
(*                                                                       *)
(*************************************************************************)

let rec close_ranges maybe_t s r =
  
  let added = r.range_end -- r.range_current_begin in
  add_file_downloaded maybe_t s added;
  let b = r.range_block in
  b.block_remaining <- b.block_remaining -- added;
  
  r.range_current_begin <- r.range_end;
  match r.range_next with
    None -> ()
  | Some rr -> 
      r.range_prev <- None;
      r.range_next <- None;
      close_ranges maybe_t s rr

      
(*************************************************************************)
(*                                                                       *)
(*                         set_downloaded_block                          *)
(*                                                                       *)
(*************************************************************************)

let set_downloaded_block maybe_t s i =
  match s.s_blocks.(i) with
    EmptyBlock ->
      let block_begin = compute_block_begin s i in
      let block_end = compute_block_end s i in
      add_file_downloaded maybe_t s (block_end -- block_begin)
  | PartialBlock b ->
      let rec iter r =
        add_file_downloaded maybe_t s (r.range_end -- r.range_current_begin);
        r.range_current_begin <- r.range_end;
        match r.range_next with
          None -> r.range_prev <- None; r
        | Some rr -> 
            r.range_prev <- None;
            r.range_next <- None;
            iter rr
      in
      b.block_ranges <- iter b.block_ranges
  | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         set_verified_bitmap (internal)                *)
(*                                                                       *)
(*************************************************************************)

(* For every swarmer, there is a "primary" verifier, and secondary verifiers.
When a block is downloaded, it is tagged '2' in the verified_bitmap, and
this '2' is propagated to the primary bitmap if possible (if all sub-blocks
are also '2'). If the primary chunk becomes '2', then a verification is
needed on the primary. If the verification works, the verified_bitmap
becomes '3', and the secondary verifiers are tagged with '2' (if they use
a different verification scheme) or '3' (if no verification scheme or
a verification scheme that has already been used). *)
  
(* corruption has been detected, and the block has been reset to 0 *)
let set_bitmap_0 s i =
  if s.s_verified_bitmap.[i] > '1' then begin
      s.s_verified_bitmap.[i] <- '0';
      List.iter (fun t ->
          let j = t.t_s2t_blocks.(i) in
          if List.for_all (fun i -> s.s_verified_bitmap.[i] = '0')
            t.t_t2s_blocks.(j) then
            t.t_converted_verified_bitmap.[j] <- '0'
      ) s.s_networks
    end
    
(* we have started downloading this block, so mark all containing blocks
  also as started. *)
let set_bitmap_1 s i =
  if s.s_verified_bitmap.[i] = '0' then begin
      s.s_verified_bitmap.[i] <- '1';
      List.iter (fun t ->
          let j = t.t_s2t_blocks.(i) in
          if t.t_converted_verified_bitmap.[j] = '0' then
            t.t_converted_verified_bitmap.[j] <- '1'
      ) s.s_networks
    end
    
(* we finished this block, we need know to verify it *)
let set_bitmap_2 s i =
  if s.s_verified_bitmap.[i] < '2' then begin
      s.s_verified_bitmap.[i] <- '2';
      match s.s_networks with
      | t :: _ when t.t_primary ->
          let j = t.t_s2t_blocks.(i) in
          if List.for_all (fun i -> s.s_verified_bitmap.[i] = '2')
            t.t_t2s_blocks.(j) &&
            t.t_converted_verified_bitmap.[j] <> '3' then begin
              t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
              t.t_converted_verified_bitmap.[j] <- '2'
            end
      | [] -> assert false
      | _ -> ()
    end
    
(* the primary verifier has worked, so let ask secondary ones for 
verification too *)
let set_bitmap_3 s i =
  if s.s_verified_bitmap.[i] < '3' then begin
      s.s_verified_bitmap.[i] <- '3';  
(*      lprintf "set_bitmap_3 %d done\n" i; *)
      match s.s_networks with
        [] -> assert false
      | tprim :: tail ->
          List.iter (fun t ->
              let j = t.t_s2t_blocks.(i) in
              if List.for_all (fun i -> s.s_verified_bitmap.[i] = '3')
                t.t_t2s_blocks.(j) then
                match t.t_verifier with
                  NoVerification ->
                    t.t_converted_verified_bitmap.[j] <- '3'
                | _ ->
                    t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
                    t.t_converted_verified_bitmap.[j] <- '2'
          ) tail
    end
      
(*************************************************************************)
(*                                                                       *)
(*                         set_toverify_block (internal)                 *)
(*                                                                       *)
(*************************************************************************)

    (*
let set_toverify_block s i = set_bitmap_2 s i
    *)

(*************************************************************************)
(*                                                                       *)
(*                         set_completed_block (internal)                *)
(*                                                                       *)
(*************************************************************************)

let set_completed_block maybe_t s i =
  begin
    match s.s_blocks.(i) with
      PartialBlock b -> close_ranges maybe_t s b.block_ranges
    | _ -> ()
  end;  
  match s.s_blocks.(i) with
    CompleteBlock | VerifiedBlock -> ()
  | _ ->
      set_downloaded_block maybe_t s i;
      set_bitmap_2 s i;
      s.s_blocks.(i) <- CompleteBlock

(*************************************************************************)
(*                                                                       *)
(*                         set_verified_block (internal)                 *)
(*                                                                       *)
(*************************************************************************)
      
let set_verified_block s j =
  match s.s_blocks.(j) with
    VerifiedBlock -> ()
  | _ ->
      set_completed_block None s j;
      s.s_blocks.(j) <- VerifiedBlock;
      set_bitmap_3 s j

(*************************************************************************)
(*                                                                       *)
(*                         set_verified_chunk (internal)                 *)
(*                                                                       *)
(*************************************************************************)
      
let set_verified_chunk t i =
  t.t_nverified_blocks <- t.t_nverified_blocks + 1;
  t.t_converted_verified_bitmap.[i] <- '3';
  let s = t.t_s in
  if t.t_primary then begin
    (* The primary is supposed to propagate verified chunks to the file *)
      List.iter (fun j -> set_verified_block s j) t.t_t2s_blocks.(i);
      if !verbose_swarming then
        print_s "VERIFIED" s
    end

(*************************************************************************)
(*                                                                       *)
(*                         verify (internal)                             *)
(*                                                                       *)
(*************************************************************************)

let verify t chunks num begin_pos end_pos =
  let file = t.t_file in
  file_verify t.t_file chunks.(num) begin_pos end_pos
  
(*************************************************************************)
(*                                                                       *)
(*                         verify_chunk (internal)                       *)
(*                                                                       *)
(*************************************************************************)
    
let verify_chunk t i = 
  if t.t_converted_verified_bitmap.[i] = '2' then
    let nblocks = String.length t.t_converted_verified_bitmap in
    match t.t_verifier with
      NoVerification
    | VerificationNotAvailable -> ()
    
    | Verification chunks when Array.length chunks = nblocks ->
        
        begin try
            let s = t.t_s in
            let block_begin = t.t_block_size ** i in
            let block_end = block_begin ++  t.t_block_size in
            let block_end = min block_end s.s_size in
            if verify t chunks i block_begin block_end then begin
                set_verified_chunk t i;
                t.t_verified t.t_nverified_blocks i;
              
              end else begin
                
                lprintf "VERIFICATION OF BLOC %d OF %s FAILED\n"
                  i (file_best_name t.t_file);
                t.t_ncomplete_blocks <- t.t_ncomplete_blocks - 1;
                
                if List.for_all (fun i ->
                      s.s_verified_bitmap.[i] = '2'
                  ) t.t_t2s_blocks.(i) then begin
                    
                    lprintf "  Swarmer block was complete. Removing data...\n";
                    
                    t.t_converted_verified_bitmap.[i] <- '0';
                    
                    List.iter (fun i ->
                        match s.s_blocks.(i) with
                          EmptyBlock -> set_bitmap_0 s i
                        | PartialBlock _ ->  set_bitmap_1 s i
                        | CompleteBlock ->
                            let block_begin = compute_block_begin s i in
                            let block_end = compute_block_end s i in
                            add_file_downloaded None s (block_begin -- block_end);
                            
                            s.s_blocks.(i) <- EmptyBlock;
                            set_bitmap_0 s i
                        
                        | VerifiedBlock -> assert false
                    ) t.t_t2s_blocks.(i)
                  end else begin
                    let nsub = ref 0 in
                    
                    lprintf "  Swarmer was incomplete: ";
                    List.iter (fun i ->
                        lprintf "%c" s.s_verified_bitmap.[i];
                        if s.s_verified_bitmap.[i] = '2' then incr nsub;
                    ) t.t_t2s_blocks.(i);
                    lprintf "   = %d/%d\n" !nsub (List.length t.t_t2s_blocks.(i));
                    
                    t.t_converted_verified_bitmap.[i] <- '1'
                  end;
              end
          with VerifierNotReady -> ()
        end
        
    | Verification chunks ->
        assert (Array.length chunks = 1);
        let can_verify = ref true in
        let nblocks= String.length t.t_converted_verified_bitmap in
        for i = 0 to nblocks - 1 do
          if t.t_converted_verified_bitmap.[i] < '2' then
            can_verify := false
        done;
        if !can_verify then begin
            try
              let s = t.t_s in
              if verify t chunks 0 zero s.s_size then begin
                  for i = 0 to nblocks - 1 do
                    if t.t_converted_verified_bitmap.[i] = '2' then begin
                        
                        set_verified_chunk t i;
                        t.t_verified t.t_nverified_blocks i;
                      end
                  done
              
              end else begin
                
                lprintf "VERIFICATION OF BLOCS OF %s FAILED\n"
                    (file_best_name t.t_file);
                  
                  for i = 0 to nblocks - 1 do
                    if t.t_converted_verified_bitmap.[i] = '2' then begin
                  
                        t.t_ncomplete_blocks <- t.t_ncomplete_blocks - 1;
                        if List.for_all (fun i ->
                              s.s_verified_bitmap.[i] = '2'
                          ) t.t_t2s_blocks.(i) then begin
                    
                            t.t_converted_verified_bitmap.[i] <- '0';
                            
                            List.iter (fun i ->
                                match s.s_blocks.(i) with
                                  EmptyBlock -> set_bitmap_0 s i
                                | PartialBlock _ ->  set_bitmap_1 s i
                                | CompleteBlock ->
                                    let block_begin = t.t_block_size ** i in
                                    let block_end = block_begin ++  t.t_block_size in
                                    let block_end = min block_end s.s_size in
                                    let block_begin = compute_block_begin s i in
                                    let block_end = compute_block_end s i in
                                    add_file_downloaded None s (block_begin -- block_end);
                                    
                                    s.s_blocks.(i) <- EmptyBlock;
                                    set_bitmap_0 s i
                        
                                | VerifiedBlock -> assert false
                            ) t.t_t2s_blocks.(i)
                          end else begin
                            let nsub = ref 0 in
                            
                            lprintf "  Swarmer was incomplete: ";
                            List.iter (fun i ->
                                lprintf "%c" s.s_verified_bitmap.[i];
                                if s.s_verified_bitmap.[i] = '2' then incr nsub;
                                ) t.t_t2s_blocks.(i);
                            lprintf "   = %d/%d\n" !nsub (List.length t.t_t2s_blocks.(i));
                    
                            t.t_converted_verified_bitmap.[i] <- '1'
                          end;
                      end
                  done
              end
          with VerifierNotReady -> ()
        end

            
(*************************************************************************)
(*                                                                       *)
(*                         must_verify_chunk (internal)                  *)
(*                                                                       *)
(*************************************************************************)

            (*
let must_verify_chunk t i immediatly = 
  match t.t_verifier with
    NoVerification -> ()
  | _ ->
      if t.t_converted_verified_bitmap.[i] < '2' then 
        set_toverify_chunk t i;
      if t.t_converted_verified_bitmap.[i] = '2' && immediatly then 
        verify_chunk t i
          *)

(*************************************************************************)
(*                                                                       *)
(*                         must_verify_block                             *)
(*                                                                       *)
(*************************************************************************)
    
let must_verify_block s i immediatly = 
  set_bitmap_2 s i;
  if immediatly then
    match s.s_networks with
      [] -> assert false
    | t :: _ when t.t_primary ->
        let i = t.t_s2t_blocks.(i) in
        t.t_converted_verified_bitmap.[i] <- '2';
(*        List.iter (fun j ->
            if s.s_verified_bitmap.[j] <> '2' then begin
                lprintf "   block %d not downloaded\n" j; 
                exit_on_error := false;
              end;
        ) t.t_t2s_blocks.(i);  *)
        verify_chunk t i;
(*      exit_on_error := true; *)
    | _ -> ()    
        
(*************************************************************************)
(*                                                                       *)
(*                         verify_all_blocks                             *)
(*                                                                       *)
(*************************************************************************)

let verify_all_chunks t immediatly =
  let s = t.t_s in
  for i = 0 to String.length s.s_verified_bitmap - 1 do
    must_verify_block s i immediatly
  done

(*************************************************************************)
(*                                                                       *)
(*                         compute_bitmap                                *)
(*                                                                       *)
(*************************************************************************)


let compute_bitmap t =
  if t.t_ncomplete_blocks > t.t_nverified_blocks then begin
      for i = 0 to String.length t.t_converted_verified_bitmap - 1 do
        if t.t_converted_verified_bitmap.[i] = '2' then
            verify_chunk t i
      done
    end


(*************************************************************************)
(*                                                                       *)
(*                         split_range (internal)                        *)
(*                                                                       *)
(*************************************************************************)

let rec split_range r range_size =
  assert (r.range_current_begin = r.range_begin);
  let next_range = r.range_begin ++ range_size in
(*  lprintf "   split_range: next_range %Ld\n" next_range; *)
  if r.range_end > next_range then 
    let rr = {
        range_block = r.range_block;
        range_nuploading = 0;
        range_next = r.range_next;
        range_prev = Some r; 
        range_begin = next_range;
        range_current_begin = next_range;
        range_end = r.range_end;
      } in
    begin
      match r.range_next with
        None -> ()
      | Some rrr -> 
(*          lprintf "Another one ??\n"; *)
          rrr.range_prev <- Some rr;
    end;
    r.range_next <- Some rr;
    r.range_end <- next_range;
(*    lprintf "      NEW RANGE: %Ld- OLD RANGE: %Ld-%Ld\n" 
      rr.range_begin r.range_begin r.range_end; *)
    
    split_range rr range_size


(*************************************************************************)
(*                                                                       *)
(*                         new_block (internal)                          *)
(*                                                                       *)
(*************************************************************************)

let new_block s i =
  let block_begin = compute_block_begin s i in
  let block_end = compute_block_end s i in
  let rec b = {
      block_s = s;
      
      block_begin = block_begin;
      block_end = block_end;
      block_ranges = range;
      block_num = i;
      block_remaining = block_end -- block_begin;
    } 
  
  and range = {
      range_prev = None;
      range_next = None;
      range_begin = block_begin;
      range_end = block_end;
      range_block = b;
      range_nuploading = 0;
      range_current_begin = block_begin;
    }
  in
  
(*  lprintf "New block %Ld-%Ld\n" block_begin block_end; *)
  split_range range s.s_range_size;
  
(*
  let rec iter r =
    lprintf "  Range %Ld-%Ld\n" r.range_begin r.range_end; 
    match r.range_next with
      None -> ()
    | Some r -> iter r
  in
  iter b.block_ranges;
*)
  
  s.s_blocks.(i) <- PartialBlock b;
  if s.s_verified_bitmap.[i] < '1' then
    set_bitmap_1 s i;
  if debug_all then lprintf "NB[%s]" s.s_verified_bitmap;
  b


(*************************************************************************)
(*                                                                       *)
(*                         next_range (internal)                         *)
(*                                                                       *)
(*************************************************************************)
  
(*
let next_range f r =
  match r.range_next with
    None -> ()
  | Some rr -> f rr
        *)

        
(*************************************************************************)
(*                                                                       *)
(*                         add_all_downloaded                            *)
(*                                                                       *)
(*************************************************************************)

(*
let add_all_downloaded t old_downloaded = 
  let new_downloaded = t.t_downloaded in
  if new_downloaded <> old_downloaded then 
    add_file_downloaded t.t_file (new_downloaded -- old_downloaded)
    *)

(*************************************************************************)
(*                                                                       *)
(*                         range_received (internal)                     *)
(*                                                                       *)
(*************************************************************************)

let range_received maybe_t r chunk_begin chunk_end =
(*  lprintf "   range_received: %Ld-%Ld for %Ld-%Ld\n"
    chunk_begin chunk_end r.range_begin r.range_end;  *)
  if r.range_begin < chunk_end && r.range_end > chunk_begin then begin
      
(*      lprintf "... entered\n"; *)
      let new_current_begin = 
        max (min chunk_end r.range_end) r.range_current_begin in
      let downloaded = new_current_begin -- r.range_current_begin in
      let b = r.range_block in
      let s = b.block_s in
      add_file_downloaded maybe_t s downloaded;
      b.block_remaining <- b.block_remaining -- downloaded;
      r.range_current_begin <- new_current_begin;
      if r.range_current_begin = r.range_end then begin
          (match r.range_next with
              None -> ()
            | Some rr -> rr.range_prev <- r.range_prev);
          (match r.range_prev with
              None -> 
                begin
                  match r.range_next with
                    None ->
                      begin
                        match s.s_blocks.(b.block_num) with
                          PartialBlock _ | EmptyBlock ->
                            
                            begin
                              match s.s_networks with 
                                [] -> assert false
                              | t :: _ when t.t_primary ->
                                  begin
                                    match t.t_verifier with
                                      NoVerification ->
                                        set_verified_block s b.block_num
                                    | _ ->
                                        set_completed_block (Some t) s b.block_num;
                                        must_verify_block s b.block_num false
                                  end
                              | _ -> ()
                            end
                        | _ -> ()
                      end
                  | Some rr -> b.block_ranges <- rr
                end;
            | Some rr -> rr.range_next <- r.range_next);
          r.range_next <- None;
          r.range_prev <- None;
        end (* else begin
          lprintf " ... new range %Ld-%Ld\n" r.range_current_begin r.range_end;
        end *)
    end


(*************************************************************************)
(*                                                                       *)
(*                         set_present_block (internal)                  *)
(*                                                                       *)
(*************************************************************************)

(* Assumption: we never download ranges from the middle, so present chunks
  can only overlap the beginning of a range *)
let set_present_block b chunk_begin chunk_end =
  let rec iter r =
    let range_next = r.range_next in
(*    lprintf "iter range %Ld-%Ld\n" r.range_begin r.range_end; *)
    (try range_received None r chunk_begin chunk_end
      with e ->
          lprintf "EXCEPTION IN range_received: %s\n"
            (Printexc2.to_string e);
          exit 2);
    match range_next with
      None -> ()
    | Some rr ->
        iter rr
  in
(*  lprintf "BEFORE:  ";  print_block b; *)
  iter b.block_ranges;
(*  lprintf "AFTER:  ";  print_block b *)
  ()


(*************************************************************************)
(*                                                                       *)
(*                         set_present                                   *)
(*                                                                       *)
(*************************************************************************)

let set_present s chunks = 
  
  apply_intervals s (fun i block_begin block_end chunk_begin chunk_end ->
(*      lprintf "interval: %Ld-%Ld in block %d [%Ld-%Ld]\n"
      chunk_begin chunk_end i block_begin block_end;  *)
      match s.s_blocks.(i) with
        EmptyBlock ->
(*          lprintf "  EmptyBlock"; *)
          if block_begin >= chunk_begin && block_end <= chunk_end then
            begin
(*              lprintf " --> CompleteBlock\n"; *)
              s.s_blocks.(i) <- CompleteBlock;
              must_verify_block s i false;
              add_file_downloaded None s (block_end -- block_begin)
            end
          else
          let b = new_block s i in
(*          lprintf " ... set_present_block\n";  *)
          set_present_block b chunk_begin chunk_end
      | PartialBlock b ->
(*          lprintf "  PartialBlock\n"; *)
          set_present_block b chunk_begin chunk_end
      | _ -> 
(*          lprintf "  Other\n"; *)
          ()
  ) chunks
  
(*************************************************************************)
(*                                                                       *)
(*                         end_present (internal)                        *)
(*                                                                       *)
(*************************************************************************)

let rec end_present present begin_present end_file list =
  match list with
    [] ->
      let present = 
        if begin_present = end_file then present else
          (begin_present, end_file) :: present
      in
      List.rev present
  | (begin_absent, end_absent) :: tail ->
      let present = 
        if begin_present = begin_absent then present
        else (begin_present, begin_absent) :: present
      in
      end_present present end_absent end_file tail

(*************************************************************************)
(*                                                                       *)
(*                         set_absent                                    *)
(*                                                                       *)
(*************************************************************************)

let set_absent s list = 
(* reverse absent/present in the list and call set_present *)
  let list = 
    match list with [] -> [ Int64.zero, s.s_size ]
    | (t1,t2) :: tail ->
        if t1 = zero then
          end_present [] t2 s.s_size tail
        else
          end_present [zero, t1] t2 s.s_size tail
  in
  set_present s list

(*************************************************************************)
(*                                                                       *)
(*                         update_uploader_chunks (internal)             *)
(*                                                                       *)
(*************************************************************************) 

let update_uploader_chunks up chunks =
  if not up.up_declared then
    let t = up.up_t in
    let s = t.t_s in
(* INVARIANT: complete_blocks must be in reverse order *)
    
    let complete_blocks = ref [] in
    let partial_blocks = ref [] in
    
    begin
      match chunks with
        AvailableRanges chunks ->
          
          apply_intervals s (fun i block_begin block_end 
                chunk_begin chunk_end ->
(*              lprintf "apply_intervals %d %Ld-%Ld %Ld-%Ld\n"
                i block_begin block_end chunk_begin chunk_end; *)
              s.s_availability.(i) <- s.s_availability.(i) + 1;
              
              match s.s_blocks.(i) with
                CompleteBlock | VerifiedBlock -> ()
              | _ ->
                  if block_begin = chunk_begin && block_end = chunk_end then
                    complete_blocks := i :: !complete_blocks
                  else
                    partial_blocks := 
                    (i, chunk_begin, chunk_end) :: !partial_blocks
          ) chunks;
      
      | AvailableCharBitmap string ->
          for i = 0 to String.length string - 1 do
            if string.[i] = '1' then 
              List.iter (fun i ->            
                  s.s_availability.(i) <- s.s_availability.(i) + 1;
                  complete_blocks := i :: !complete_blocks
              ) t.t_t2s_blocks.(i)
          done;
      | AvailableBoolBitmap bitmap ->
          for i = 0 to Array.length bitmap - 1 do
            if bitmap.(i) then 
              List.iter (fun i ->
                  s.s_availability.(i) <- s.s_availability.(i) + 1;
                  complete_blocks := i :: !complete_blocks
              ) t.t_t2s_blocks.(i)
          done;
    end;
    
    List.iter (fun i ->
(*        s.s_last_seen.(i) <- BasicSocket.last_time (); *)
        
        let i = t.t_s2t_blocks.(i) in
        t.t_last_seen.(i) <- BasicSocket.last_time ()
        
    ) !complete_blocks;
    
    let complete_blocks = Array.of_list !complete_blocks in
    let partial_blocks = Array.of_list !partial_blocks in
    up.up_chunks <- chunks;
    
    up.up_complete_blocks <- complete_blocks;
    up.up_ncomplete <- Array.length complete_blocks;
    
    if Array.length partial_blocks > 0 then
      lprintf "WARNING: partial_blocks = %d\n" (Array.length partial_blocks);
    up.up_partial_blocks <- partial_blocks;
    up.up_npartial <- Array.length partial_blocks;
    
    up.up_block <- None;
    up.up_block_begin <- zero;
    up.up_block_end <- zero;
    
    up.up_declared <- true;
    
    if debug_all then print_uploader up

(*************************************************************************)
(*                                                                       *)
(*                         clean_uploader_chunks (internal)              *)
(*                                                                       *)
(*************************************************************************) 


let clean_uploader_chunks up = 
  
  if up.up_declared then 
    
    let decr_availability s i =
      s.s_availability.(i) <- s.s_availability.(i) - 1
    in
(*          lprintf "clean_uploader_chunks:\n"; *)
    
    let t = up.up_t in
    let s = t.t_s in
    for i = 0 to Array.length up.up_complete_blocks - 1 do
(*            lprintf "decr_availability complete[%d] %d\n" i
              up.up_complete_blocks.(i); *)
      decr_availability s up.up_complete_blocks.(i)
    done;
    for i = 0 to Array.length up.up_partial_blocks - 1 do
      let b,_,_ = up.up_partial_blocks.(i) in
(*            lprintf "decr_availability partial[%d] %d\n" i b; *)
      decr_availability s b
    done;
    clear_uploader_block up;
    up.up_declared <- false

(*************************************************************************)
(*                                                                       *)
(*                         register_uploader                             *)
(*                                                                       *)
(*************************************************************************) 

let register_uploader t client chunks =
  
  let up =
    {
      up_t = t;
      up_client = client;
      
      up_declared = false;
      up_chunks = chunks;
      
      up_complete_blocks = [||];
      up_ncomplete = 0;
      
      up_partial_blocks = [||];
      up_npartial = 0;
      
      up_block = None;
      up_block_begin = zero;
      up_block_end = zero;
      up_ranges = [];
    }
  in
  HU.add uploaders_by_num up;
  update_uploader_chunks up chunks;
  up

(*************************************************************************)
(*                                                                       *)
(*                         unregister_uploader                           *)
(*                                                                       *)
(*************************************************************************) 

let unregister_uploader up = 
  clean_uploader_chunks up;
  clear_uploader_block up;
  clear_uploader_ranges up

(*************************************************************************)
(*                                                                       *)
(*                         update_uploader                               *)
(*                                                                       *)
(*************************************************************************) 

let update_uploader up chunks = 
  
  clean_uploader_chunks up;
  update_uploader_chunks up chunks

(*************************************************************************)
(*                                                                       *)
(*                         print_uploaders                               *)
(*                                                                       *)
(*************************************************************************)

let print_uploaders s =
  let nblocks = Array.length s.s_blocks in
  for i = 0 to nblocks - 1 do
    
    match s.s_blocks.(i) with
      EmptyBlock -> lprintf "_"
    | CompleteBlock -> lprintf "C"
    | VerifiedBlock -> lprintf "V"
    | PartialBlock b ->
        if s.s_nuploading.(i) > 9 then
          lprintf "X"
        else
          lprintf "%d" s.s_nuploading.(i)
  done;
  lprintf "\n";
  for i = 0 to nblocks - 1 do
    
    match s.s_blocks.(i) with
      EmptyBlock -> lprintf "_"
    | CompleteBlock -> lprintf "C"
    | VerifiedBlock -> lprintf "V"
    | PartialBlock b ->
        lprintf "{%d : %d=" b.block_num 
          s.s_nuploading.(b.block_num);
        
        let rec iter_range r =
          lprintf "(%d)" r.range_nuploading;
          match r.range_next with
            None -> ()
          | Some rr -> iter_range rr
        in
        iter_range b.block_ranges;
        lprintf " }";
  
  done;
  lprintf "\n"

(*************************************************************************)
(*                                                                       *)
(*                         permute_and_return (internal)                 *)
(*                                                                       *)
(*************************************************************************)

let max_clients_per_block = 3

let permute_and_return up n =
  let b = up.up_complete_blocks.(n) in
  if debug_all then  lprintf "permute_and_return %d <> %d" n b;
  up.up_complete_blocks.(n) <- up.up_complete_blocks.(up.up_ncomplete-1);
  up.up_complete_blocks.(up.up_ncomplete-1) <- b;
  up.up_ncomplete <- up.up_ncomplete - 1;
  let t = up.up_t in
  let s = t.t_s in
  match s.s_blocks.(b) with
    EmptyBlock -> 
      let b = new_block s b in
      b, b.block_begin, b.block_end
  | PartialBlock b -> 
      b, b.block_begin, b.block_end
  | VerifiedBlock -> 
      lprintf "ERROR: verified block in permute_and_return %d\n" b; 
      assert false
  | CompleteBlock -> 
      lprintf "ERROR: complete block in permute_and_return %d\n" b;
      assert false
      
(*************************************************************************)
(*                                                                       *)
(*                         LinearStrategy.select_block (internal)            *)
(*                                                                       *)
(*************************************************************************)

module LinearStrategy = struct
    let select_block  up =
      let rec iter_complete up =
        let n = up.up_ncomplete in
        if n = 0 then iter_partial up
        else
        let b = up.up_complete_blocks.(n-1) in
        up.up_ncomplete <- n-1;
        let t = up.up_t in
        let s = t.t_s in
        match s.s_blocks.(b) with
          CompleteBlock | VerifiedBlock -> 
            iter_complete up
        | PartialBlock b -> 
            b, b.block_begin, b.block_end
        | EmptyBlock ->
            let b = new_block s b in
            b, b.block_begin, b.block_end
      
      and iter_partial up =
        let n = up.up_npartial in
        if n = 0 then raise Not_found;
        let t = up.up_t in
        let b, block_begin, block_end = up.up_partial_blocks.(n-1) in
        let t = up.up_t in
        let s = t.t_s in
        match s.s_blocks.(b) with
          CompleteBlock | VerifiedBlock -> 
            iter_partial up
        | PartialBlock b -> 
            b, block_begin, block_end
        | EmptyBlock ->
            let b = new_block s b in
            b, block_begin, block_end
      in
      iter_complete up
  end

(*************************************************************************)
(*                                                                       *)
(*                         should_download_block (internal)              *)
(*                                                                       *)
(*************************************************************************)

let should_download_block s n =
(*  lprintf "should_download_block %d\n" n; *)
  let result = 
    match s.s_verified_bitmap.[n] with
      '2' ->
        begin
          match s.s_networks with
            [] -> assert false
          | t :: _ when t.t_primary ->
              begin
                try
                  let n = t.t_s2t_blocks.(n) in
                  if t.t_converted_verified_bitmap.[n] = '2' then
                    verify_chunk t n
                with VerifierNotReady -> ()
              end
          | _ -> ()
        end;
        s.s_verified_bitmap.[n] < '2'
    | '0' | '1' -> true
    | _ -> false
  in
(*  if result then
    lprintf "should_download_block %d\n" n; *)
  result
  
(*************************************************************************)
(*                                                                       *)
(*                         select_block (internal)                       *)
(*                                                                       *)
(*************************************************************************)

exception BlockFound of int

let random_int max = 
  let x = Random.int max in
  if debug_all then lprintf "(Random %d -> %d)" max x;
  x

let select_block up =
  let t = up.up_t in
  let s = t.t_s in
  try
    match s.s_strategy with
      LinearStrategy ->
        LinearStrategy.select_block up
    | _ -> 
        if up.up_ncomplete = 0 && up.up_npartial = 0 then raise Not_found;

(**************

This strategy sucks. It has to be improved.
Important: 
1) never give a block to 2 clients if another one has 0 client.
2) try to complete partial blocks as soon as possible.
3) comfigure the chooser depending on the network (maybe BT might
better work at the beginning if the first incomplete blocks are offered
    to several clients.

***************)
        
        
        
        if up.up_ncomplete > 1 then begin
(*          let debug_all = true in *)
            try
              
              let rec iter_max_uploaders max_nuploaders =
                let t = up.up_t in
                let nblocks = Array.length s.s_blocks in

(*************   Try to download the movie index and the first minute to
   allow preview of the file as soon as possible *)
                
                if debug_all then lprintf "{First}";
                
                let download_first n b =
(*                lprintf "download_first %d\n" n; *)
                  if 
                    up.up_complete_blocks.(n) = b &&
                    s.s_nuploading.(b) < max_nuploaders &&
                    should_download_block s b then
                    raise (BlockFound n)
                in

(*              lprintf "up_complete_blocks: %d\n" 
                (Array.length up.up_complete_blocks); *)

(* This must be the position of the last block of the file *)
                download_first 0 (nblocks-1);

(* This can be the position of the first block of the file *)
                download_first (up.up_ncomplete-1) 0;

(* This can be the position of the first block of the file *)
                download_first 0 0;

(* These can be the positions of the second block of the file *)
                download_first 0 1;
                download_first (up.up_ncomplete-1) 1;
                download_first (up.up_ncomplete-2) 1;

(************* If the file can be verified, and we don't have a lot of blocks
    yet, try to download the partial ones as soon as possible *)
                
                if debug_all then lprintf "{PartialBlock}";
                
                let download_partial max_uploaders =
                  let partial_block = ref (-1) in
                  let partial_remaining = ref zero in
                  for i = 0 to up.up_ncomplete - 1 do
                    let n = up.up_complete_blocks.(i) in
                    match s.s_blocks.(n) with
                      PartialBlock b ->
                        if (!partial_block = -1 ||
                            !partial_remaining > b.block_remaining) &&
                          s.s_nuploading.(n) < max_uploaders
                        then
                          begin
                            partial_block := i;
                            partial_remaining := b.block_remaining
                          end                          
                    | _ -> ()
                  done;
                  if !partial_block <> -1 then
                    raise (BlockFound !partial_block)
                in
                
                if t.t_verifier <> NoVerification &&
                  t.t_nverified_blocks  < 2  then begin
                    download_partial max_nuploaders;
                  end;

(************* Download partial chunks from the verification point of view *)
                
                if List.length s.s_networks > 1 then begin
                    if debug_all then lprintf "{PartialChunk}";
                    
                    let my_t = if t.t_verifier <> NoVerification then t
                      else match s.s_networks with t :: _ -> t | _ -> t in
                    
                    let download_partial max_uploaders =
                      let partial_block = ref (-1) in
                      let partial_remaining = ref 0 in
                      for i = 0 to up.up_ncomplete - 1 do
                        let n = up.up_complete_blocks.(i) in
(* TODO move this after the first if... *)
                        let t_index = t.t_s2t_blocks.(n) in                      
                        let bs = List.filter (fun s_index ->
                              s.s_verified_bitmap.[s_index] = '2'
                          ) t.t_t2s_blocks.(t_index) in
                        let nbs = List.length bs in

(* TODO remove this *)
                        let b = should_download_block s n in
                        
                        if !verbose_swarming then begin
                            lprintf "  test %d %c %d %b %d\n"
                              n s.s_verified_bitmap.[n] s.s_nuploading.(n)
                            b nbs;
                          end;
                        
                        if s.s_verified_bitmap.[n] < '2' &&
                          s.s_nuploading.(n) < max_uploaders &&
                          should_download_block s n then
                          
                          if (!partial_block = -1 || !partial_remaining < nbs)
                          then
                            begin
                              partial_block := i;
                              partial_remaining := nbs
                            end
                      done;
                      if !partial_block <> -1 then begin
                          if !verbose_swarming then
                            lprintf "PartialChunk won for %d waiting blocks"
                              !partial_remaining;
                          raise (BlockFound !partial_block)
                        end
                    in
                    
                    if my_t.t_verifier <> NoVerification then begin
                        download_partial max_nuploaders;
                      end;
                  end;              

(************* Download rarest first only if other blocks are much more
  available *)
                
                if debug_all then lprintf "{Rarest}";
                
                let sum_availability = ref 0 in
                let min_availability = ref max_int in
                for i = 0 to up.up_ncomplete - 1 do
                  let n = up.up_complete_blocks.(i) in
                  sum_availability := !sum_availability +
                    s.s_availability.(n);
                  min_availability := min !min_availability 
                    s.s_availability.(n);
                done;
                
                let mean_availability = 
                  !sum_availability / up.up_ncomplete in
                
                if mean_availability > 5 && !min_availability < 3 then
                  for i = 0 to up.up_ncomplete - 1 do
                    let n = up.up_complete_blocks.(i) in
                    if s.s_availability.(n) < 3 
                        && should_download_block s n
                    then
                      raise (BlockFound i)
                  done;

(************* Otherwise, download in random order *)
                
                if debug_all then lprintf "{Random}";
                let find_random max_uploaders =
                  let list = ref [] in
                  if debug_all then lprintf " {NC: %d}" up.up_ncomplete;
                  for i = 0 to up.up_ncomplete - 1 do
                    let n = up.up_complete_blocks.(i) in
                    if s.s_nuploading.(n) < max_uploaders
                        && should_download_block s n
                    then
                      list := i :: !list
                  done;
                  match !list with
                    [] -> ()
                  | [i] -> raise (BlockFound i)
                  | list ->
                      let array = Array.of_list list in
                      raise (BlockFound (array.(
                            random_int (Array.length array))))
                in
                
                find_random max_nuploaders

(************* Fall back on linear download if nothing worked *)
              
              in
              iter_max_uploaders 1;
              iter_max_uploaders max_clients_per_block;
              iter_max_uploaders max_int;
              raise Not_found
            with 
              BlockFound n ->
                if debug_all then lprintf "\nBlockFound %d\n" 
                    up.up_complete_blocks.(n); 
                permute_and_return up n
          end else
          LinearStrategy.select_block up
  with Not_found ->
      
      (* print_s "NO BLOCK FOUND" s; *)
      raise Not_found
      
    (*************************************************************************)
(*                                                                       *)
(*                         find_block                                    *)
(*                                                                       *)
(*************************************************************************)

let find_block up =
  try      
    if debug_all then begin
        lprintf "C: ";
        for i = 0 to up.up_ncomplete - 1 do
          lprintf "%d " up.up_complete_blocks.(i)
        done;
      end;
    
    let t = up.up_t in
    let s = t.t_s in
    match file_state t.t_file with
    | FilePaused 
    | FileAborted _ 
    | FileCancelled -> raise Not_found
    | _ -> 
        
        
        (match up.up_block with
            None -> ()
          | Some b ->
              let num = b.block_num in
              s.s_nuploading.(num) <- s.s_nuploading.(num) - 1;
              up.up_block <- None;
        );
        
        let (b,block_begin,block_end) as result = select_block up in
        let num = b.block_num in
        s.s_nuploading.(num) <- s.s_nuploading.(num) + 1;
        up.up_block <- Some b;
        up.up_block_begin <- block_begin;
        up.up_block_end <- block_end;
        if debug_all then lprintf " = %d \n" num;
        b
  with e ->
      if debug_all then lprintf "Exception %s\n" (Printexc2.to_string e);
      raise e

(*************************************************************************)
(*                                                                       *)
(*                         clean_ranges (internal)                       *)
(*                                                                       *)
(*************************************************************************)

let clean_ranges up =
  
  let rec iter list left =
    match list with
      [] -> List.rev left
    | ((_,_,r) as rr) :: tail ->
        iter tail
          (if r.range_current_begin < r.range_end then rr :: left
          else begin
              r.range_nuploading <- r.range_nuploading - 1;
              left
            end)
  in
  up.up_ranges <- iter up.up_ranges []

(*************************************************************************)
(*                                                                       *)
(*                         current_ranges                                *)
(*                                                                       *)
(*************************************************************************)

let current_ranges up =  up.up_ranges

(*************************************************************************)
(*                                                                       *)
(*                         current_block                                 *)
(*                                                                       *)
(*************************************************************************)

let current_block up =  
  match up.up_block with
    None -> raise Not_found
  | Some b -> b

(*************************************************************************)
(*                                                                       *)
(*                         in_uploader_ranges                            *)
(*                                                                       *)
(*************************************************************************)

let rec in_uploader_ranges r list =
  match list with
    [] -> false
  | (_,_,r') :: tail when r' == r -> true
  | _ :: tail -> in_uploader_ranges r tail

(*************************************************************************)
(*                                                                       *)
(*                         find_range                                    *)
(*                                                                       *)
(*************************************************************************)

let find_range up =
  clean_ranges up;
  
  let b =
    match up.up_block with
      None -> raise Not_found
    | Some b -> b
  in
  let r = b.block_ranges in
  
  let t = up.up_t in
  
  match file_state t.t_file with
  | FilePaused 
  | FileAborted _ 
  | FileCancelled -> raise Not_found
  | _ -> 
      
      let rec iter limit r =

(* let use a very stupid heuristics: ask for the first non-used range.
we thus might put a lot of clients on the same range !
*)
        
        if not (in_uploader_ranges r up.up_ranges) &&
          r.range_current_begin < r.range_end &&
          r.range_current_begin >= up.up_block_begin &&
          r.range_end <= up.up_block_end &&
          r.range_nuploading < limit
        then begin
            let key = r.range_current_begin, r.range_end, r in
            up.up_ranges <- up.up_ranges @ [key];
            r.range_nuploading <- r.range_nuploading + 1;
            if r.range_current_begin = r.range_end then
              lprintf "CS error: range is empty <<<<<<<<<<<--------- error\n";
            key
          end else
        match r.range_next with
          None -> raise Not_found
        | Some rr -> iter limit rr          
      in
      try
(* first try to find ranges with 0 uploaders *)
        iter 1 r
      with Not_found ->
          try
(* try normal ranges *)
            iter max_clients_per_block r
          with Not_found ->
(* force maximal uploading otherwise to finish it *)
              iter max_int r 

(*************************************************************************)
(*                                                                       *)
(*                         range_range                                   *)
(*                                                                       *)
(*************************************************************************)

let range_range r = (r.range_current_begin, r.range_end)

(*************************************************************************)
(*                                                                       *)
(*                         received                                      *)
(*                                                                       *)
(*************************************************************************)

let received (up : uploader) (file_begin : Int64.t)
  (str:string) (string_begin:int) (string_len:int) =
  
  if string_len > 0 then
    let file_end = file_begin ++ (Int64.of_int string_len) in
    
    if !verbose_swarming then
      lprintf "received on %s-%s\n" 
        (Int64.to_string file_begin)
      (Int64.to_string file_end);

(* TODO: check that everything we received has been required *)
    let t = up.up_t in
    let s = t.t_s in
    try  
      
      List.iter (fun (_,_,r) ->
          if r.range_current_begin < file_end &&
            r.range_end > file_begin then begin
              
              let file_end = min file_end r.range_end in
              let written_len = file_end -- r.range_current_begin in
              
              begin
                match file_state t.t_file with
                | FilePaused 
                | FileAborted _ 
                | FileCancelled -> ()
                | _ -> 
                    
                    let string_pos = string_begin + 
                        Int64.to_int (r.range_current_begin -- file_begin) in
                    let string_length = Int64.to_int written_len in
                    
                    if 
                      string_pos < 0 ||
                      string_pos < string_begin || 
                      string_len < string_length then begin
                        lprintf "[ERROR CommonSwarming]: BAD WRITE\n";
                        lprintf "   for range %Ld-%Ld (string_pos %d)\n"
                          r.range_begin r.range_end string_pos;
                        
                        lprintf "  received: file_pos:%Ld string:%d %d\n"
                          file_begin string_begin string_len;
                        lprintf "  ranges:\n";
                        List.iter (fun (_,_,r) ->
                            lprintf "     range: %Ld-[%Ld]-%Ld"
                              r.range_begin r.range_current_begin
                              r.range_end;
                            (match r.range_next with
                                None -> ()
                              | Some rr -> 
                                  lprintf "  next: %Ld" rr.range_begin);
                            (match r.range_prev with
                                None -> ()
                              | Some rr -> 
                                  lprintf "  prev: %Ld" rr.range_begin);
                            lprintf "\n";
                            let b = r.range_block in
                            lprintf "        block: %d[%c] %Ld-%Ld [%s]"
                              b.block_num 
                              s.s_verified_bitmap.[b.block_num]
                              b.block_begin b.block_end
                              (match s.s_blocks.(b.block_num) with
                                EmptyBlock -> "empty"
                              | PartialBlock _ -> "partial"
                              | CompleteBlock -> "complete"
                              | VerifiedBlock -> "verified"
                            );
                            let br = b.block_ranges in
                            lprintf " first range: %Ld(%Ld)"
                              br.range_begin
                              (br.range_end -- br.range_current_begin);
                            lprintf "\n";
                        ) up.up_ranges;
                        if !exit_on_error then exit 2
                      end else
                    if string_length > 0 then
                      match s.s_networks with
                        [] -> assert false
                      | t :: _ when t.t_primary ->
                          file_write t.t_file
                            r.range_current_begin  
                            str string_pos string_length;
                      | _ -> ()
              end;
              range_received (Some t) r r.range_current_begin file_end;
              
            end
      ) up.up_ranges;
      clean_ranges up
    with e -> 
        raise e
  
        
(*************************************************************************)
(*                                                                       *)
(*                         present_chunks                                *)
(*                                                                       *)
(*************************************************************************)

let present_chunks s =
  let nblocks = Array.length s.s_blocks in
(*  lprintf "present_chunks...%d blocks\n" nblocks; *)
  
  let rec iter_block_out i block_begin list = 
    if debug_present_chunks then
      lprintf "iter_block_out %d bb: %s\n"
        i
        (Int64.to_string block_begin); 
    
    if i = nblocks then List.rev list else
    let block_end = compute_block_end s i in
    match s.s_blocks.(i) with
      EmptyBlock ->
        iter_block_out (i+1) block_end list
    | CompleteBlock | VerifiedBlock ->
        let block_begin = compute_block_begin s i in
        iter_block_in (i+1) block_end block_begin list
    | PartialBlock b ->
        iter_range_out i block_end block_begin b.block_ranges  list
  
  and iter_block_in i block_begin chunk_begin list =
    if debug_present_chunks then
      lprintf "iter_block_in %d bb: %s cb:%s \n"
        i
        (Int64.to_string block_begin) 
      (Int64.to_string chunk_begin) 
      ; 
    
    if i = nblocks then 
      let list = (chunk_begin, s.s_size) :: list in
      List.rev list
    else
    let block_end = compute_block_end s i in
    match s.s_blocks.(i) with
      EmptyBlock ->
        iter_block_out (i+1) block_end ( (chunk_begin, block_begin) :: list)
    | CompleteBlock | VerifiedBlock ->
        iter_block_in (i+1) block_end chunk_begin list
    | PartialBlock b ->
        iter_range_in i block_end 
          chunk_begin block_begin b.block_ranges list
  
  and iter_range_out i block_end block_begin r list =
    if debug_present_chunks then
      lprintf "iter_range_out %d nb: %s bb:%s\n"
        i
        (Int64.to_string block_end) 
      (Int64.to_string block_begin) 
      ; 
    
    if r.range_begin > block_begin then
      iter_range_in i block_end block_begin r.range_begin r list
    else
    
    if r.range_current_begin > block_begin then begin
        if r.range_current_begin < r.range_end then
          let list = (r.range_begin, r.range_current_begin) :: list in
          match r.range_next with
            None ->
              iter_block_out (i+1) block_end list
          | Some rr ->
              iter_range_out i block_end r.range_end rr list              
        else
        match r.range_next with
          None ->
            iter_block_in (i+1) block_end r.range_begin list
        | Some rr ->
            iter_range_in i block_end
              r.range_begin r.range_end rr list                
      end else
    match r.range_next with
      None ->
        iter_block_out (i+1) block_end list
    | Some rr ->
        iter_range_out i block_end r.range_end rr list
  
  
  and iter_range_in i block_end chunk_begin chunk_end r list =
    if debug_present_chunks then
      lprintf "iter_range_in %d bn: %s cb:%s ce: %s\n"
        i
        (Int64.to_string block_end) 
      (Int64.to_string chunk_begin) 
      (Int64.to_string chunk_end) ; 
    
    if r.range_current_begin < r.range_end then
      let list = (chunk_begin, r.range_current_begin) :: list in
      match r.range_next with
        None ->
          iter_block_out (i+1) block_end list
      | Some rr ->
          iter_range_out i block_end r.range_end rr list
    else
    match r.range_next with
      None ->
        iter_block_in (i+1) block_end chunk_begin list
    | Some rr ->
        iter_range_in i block_end chunk_begin r.range_end rr list
  in
  let chunks = iter_block_out 0 zero [] in
(*  lprintf "present_chunks done\n"; *)
  chunks

(*************************************************************************)
(*                                                                       *)
(*                         propagate_chunk                               *)
(*                                                                       *)
(*************************************************************************) 

let propagate_chunk t1 ts pos1 size =
  
  (*
  List.iter (fun (t2, i2, pos2) ->
      lprintf "Should propagate chunk from %s %Ld to %s %Ld [%Ld]\n" 
        (file_best_name t1.t_file) pos1
        (file_best_name t2.t_file) pos2 size;
      Unix32.copy_chunk (file_fd t1.t_file)  (file_fd t2.t_file)
      pos1 pos2 (Int64.to_int size);

      set_toverify_block t2 i2;
      set_verified_block t2 i2;
  ) ts
*)
  ()
  
(*************************************************************************)
(*                                                                       *)
(*                         duplicate_chunks                              *)
(*                                                                       *)
(*************************************************************************) 

(* This is the least aggressive version. I was thinking of computing
checksums for all possible schemas for all files, to be able to
move chunks from/to BT files from/to ED2k files. *)
  
let duplicate_chunks () =
  (*
  let chunks = Hashtbl.create 100 in
  HS.iter (fun t ->
      let rec iter i len pos bsize size =
        if i < len then
          let c = {
              chunk_uid = t.t_checksums.(i);
              chunk_size = min (size -- pos) bsize;
            } in
          let (has, has_not) = try
              Hashtbl.find chunks c
            with _ -> 
                let sw = (ref [], ref []) in
                Hashtbl.add chunks c sw;
                sw
          in
          let sw = if t.t_verified_bitmap.[i] = '3' then has else has_not in
          sw := (t, i, pos) :: !sw;
          iter (i+1) len (pos ++ bsize) bsize size
      in
      iter 0 (Array.length t.t_checksums) zero t.t_block_size t.t_size
  ) swarmers_by_num;
  Hashtbl.iter (fun c (has, has_not) ->
      match !has, !has_not with
        _ ,  []
      | [], _ -> ()
      | (t, _, pos) :: _, ts ->
          propagate_chunk t ts pos c.chunk_size
  ) chunks
*)
  ()
  
(*************************************************************************)
(*                                                                       *)
(*                         set_checksums                                 *)
(*                                                                       *)
(*************************************************************************) 

  
(* TODO: where is this used ? check that the fact of using the UID for
  small files does not create any problem. *)
let get_checksums t =
  match t.t_verifier with
    Verification tab -> tab
  | _ -> [||]

      
      
(*************************************************************************)
(*                                                                       *)
(*                         primary (internal)                            *)
(*                                                                       *)
(*************************************************************************)

let primary t = t.t_primary
      
(*************************************************************************)
(*                                                                       *)
(*                         set_verified_bitmap                           *)
(*                                                                       *)
(*************************************************************************)

let set_verified_bitmap primary t bitmap =
(*  t.t_verified_bitmap <- bitmap; *)
  
  for i = 0 to String.length bitmap - 1 do
    
    match bitmap.[i] with
    | '2' -> 
        if t.t_converted_verified_bitmap.[i] < '2' then begin
            t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
            t.t_converted_verified_bitmap.[i] <- '2'
          end
    
    | '3' ->
(*        lprintf "Setting 3 on %d\n" i;        *)
        t.t_converted_verified_bitmap.[i] <- '3';
        if primary then
          let s = t.t_s in
          List.iter (fun i ->
(*              lprintf "Should set %d\n" i; *)
              match s.s_blocks.(i) with
                CompleteBlock -> 
(*                  lprintf "CompleteBlock\n"; *)
                  set_verified_block s i
              | EmptyBlock | PartialBlock _ ->
(*                  lprintf "EmptyBlock/PartialBlock\n"; *)
                  set_completed_block None s i;
(*                  lprintf "set_verified_block\n"; *)
                  set_verified_block s i
              | VerifiedBlock -> 
(*                  lprintf "Block already verified\n" *)
                  ()
          ) t.t_t2s_blocks.(i);
          if t.t_converted_verified_bitmap.[i] <> '3' then
            lprintf "FIELD AS BEEN CLEARED\n"
    | _ -> ()
  done      
  
(*************************************************************************)
(*                                                                       *)
(*                         verified_bitmap                               *)
(*                                                                       *)
(*************************************************************************)

let verified_bitmap t = t.t_converted_verified_bitmap

(*************************************************************************)
(*                                                                       *)
(*                         set_verifier                                  *)
(*                                                                       *)
(*************************************************************************)

let set_verifier t f = 
  t.t_verifier <- f;
(* TODO: check that false as primary is a good value to start with *)
  set_verified_bitmap false t t.t_converted_verified_bitmap

(*************************************************************************)
(*                                                                       *)
(*                         set_verifier                                  *)
(*                                                                       *)
(*************************************************************************)

let set_verified t f = 
  t.t_verified <- f

(*************************************************************************)
(*                                                                       *)
(*                         downloaded                                    *)
(*                                                                       *)
(*************************************************************************)

let downloaded t = file_downloaded t.t_file

(*************************************************************************)
(*                                                                       *)
(*                         block_block                                   *)
(*                                                                       *)
(*************************************************************************)

let block_num t b = 
  let n = t.t_s2t_blocks.(b.block_num) in
  n

(*************************************************************************)
(*                                                                       *)
(*                         partition_size                                *)
(*                                                                       *)
(*************************************************************************)

let partition_size t = String.length t.t_converted_verified_bitmap

let uploader_swarmer up = up.up_t

  
(*************************************************************************)
(*                                                                       *)
(*                         availability                                  *)
(*                                                                       *)
(*************************************************************************)

let availability t =
  
  let s = t.t_s in
  let len = String.length t.t_converted_verified_bitmap in
  let str = String.make len '\000' in
  for i = 0 to len - 1 do
    str.[i] <- char_of_int (
      let v = List2.min 
          (List.map (fun i -> s.s_availability.(i)) t.t_t2s_blocks.(i)) in
      if v < 0 then 0 else
      if v > 200 then 200 else v)
  done;
  str

(*************************************************************************)
(*                                                                       *)
(*                         is_interesting                                *)
(*                                                                       *)
(*************************************************************************)

(*return true if s is interesting for p1
    NB: works when s is a mask of 0s(absent bloc) and 1s(present bloc) 
    p1 can be a string 0(absent) 1(partial) 2(present unverified) or 
      3(present verified)
                        s : 00001111
                       p1 : 01230123
           is_interesting : 00001110
*)

let is_interesting up =
  up.up_ncomplete > 0 || up.up_npartial > 0


(*************************************************************************)
(*                                                                       *)
(*                         value_to_int64_pair (internal)                *)
(*                                                                       *)
(*************************************************************************)

let value_to_int64_pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int64 v1, value_to_int64 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"
      
(*************************************************************************)
(*                                                                       *)
(*                         WRAPPERS                                      *)
(*                                                                       *)
(*************************************************************************)

let set_present t = set_present t.t_s
let set_absent t = set_absent t.t_s
let present_chunks t = present_chunks t.t_s
let print_t str t = print_s str t.t_s
let print_uploaders t = print_uploaders t.t_s
  
(*************************************************************************)
(*                                                                       *)
(*                         value_to_swarmer                              *)
(*                                                                       *)
(*************************************************************************)

let value_to_swarmer t assocs = 
  let get_value name conv = conv (List.assoc name assocs) in
  
  
  let primary = 
    try get_value "file_primary" value_to_bool with _ -> true 
  in
  
  (try
      let file_name = get_value "file_swarmer" value_to_string in
      let s = 
        HS.find swarmers_by_name { dummy_swarmer with s_filename = file_name }
      in
      associate primary t s;
(* TODO: make as many checks as possible to ensure the file and the swarmers
  are correctly associed. *)
    with Not_found -> ());
  
  let set_bitmap =
    let mtime = try file_mtime t.t_file with _ -> 0. in
    let old_mtime = 
      try 
        value_to_float (List.assoc "file_mtime" assocs) 
      with Not_found -> mtime
    in
    old_mtime = mtime
  in
(* TODO: if set_bitmap is false, we should the bitmap to 2222222222 so that
it is verified as soon as possible. *)
  (try
      try
        set_verified_bitmap primary t
          (get_value  "file_chunks" value_to_string)
      with Not_found ->
          set_verified_bitmap primary t
            (get_value  "file_all_chunks" value_to_string)
    
    with e -> 
        lprintf "Exception %s while loading bitmap\n"
          (Printexc2.to_string e); 
  );

  (*
  lprintf "set_verified_bitmap: t = %s\n" t.t_converted_verified_bitmap;
  lprintf "set_verified_bitmap: s = %s\n" t.t_s.s_verified_bitmap;
*)
  
  if primary then begin
      lprintf "Loading present...\n";
      let present = try 
          let present = 
            (get_value "file_present_chunks" 
                (value_to_list value_to_int64_pair))
          in
          set_present t present;
          present
        with e ->
            lprintf "Exception %s while set present\n"
              (Printexc2.to_string e);         
            []
      in
      lprintf "Downloaded after present %Ld\n" (downloaded t);
      
      lprintf "Loading absent...\n";
      (try 
          set_absent t 
            (get_value "file_absent_chunks" 
              (value_to_list value_to_int64_pair));
        with e ->
            lprintf "Exception %s while set absent\n"
              (Printexc2.to_string e);         
      );
      lprintf "Downloaded after absent %Ld\n" (downloaded t);
      (try 
          let d = get_value "file_downloaded" value_to_int64 in
          
          if d <> downloaded t then begin
              lprintf "ERROR: CommonSwarming: stored downloaded value not restored  !!! (%Ld/%Ld)\n" (downloaded t) d;
              lprintf "ERROR: CommonSwarming: present:\n";
              List.iter (fun (x,y) ->
                  lprintf "     (%Ld,%Ld);\n" x y  
              ) present;
              
              let p = present_chunks t in
              lprintf "ERROR: CommonSwarming: present now:\n";
              
              let total = ref zero in
              List.iter (fun (x,y) ->
                  lprintf "     (%Ld,%Ld);\n" x y;
                  total := !total ++ (y -- x);
              ) p;

              lprintf "ERROR: total %Ld\n" !total;
              if p = present then begin
                  lprintf "ERROR: both appear to be the same !!!\n";
                end;
              
              

(*          exit 2 *)
            end
        
        with e -> ());
    end;

(* TODO re-implement this
  (try
      let last_seen = get_value "file_chunks_age"
          (value_to_list value_to_int) in
      t.t_last_seen <- Array.of_list last_seen
    with _ -> ());
*)
  
  ()      

(*************************************************************************)
(*                                                                       *)
(*                         set_verified_bitmap                           *)
(*                                                                       *)
(*************************************************************************)

let set_verified_bitmap t bitmap = 
  set_verified_bitmap (primary t) t bitmap
  
(*************************************************************************)
(*                                                                       *)
(*                         swarmer_to_value                              *)
(*                                                                       *)
(*************************************************************************)

let swarmer_to_value t other_vals = 
  ("file_primary", bool_to_value (primary t)) ::
  ("file_swarmer", string_to_value t.t_s.s_filename) ::
  ("file_mtime", float_to_value (try file_mtime t.t_file with _ -> 0.)) ::
  ("file_chunks", string_to_value (verified_bitmap t)) ::
  ("file_present_chunks", List
      (List.map (fun (i1,i2) -> 
          SmallList [int64_to_value i1; int64_to_value i2])
      (present_chunks t))) ::
  ("file_downloaded", int64_to_value (downloaded t)) ::
  
  ("file_chunks_age", List (Array.to_list 
        (Array.map int_to_value t.t_last_seen))) ::
  
  other_vals

(*************************************************************************)
(*                                                                       *)
(*                         verify_one_chunk                              *)
(*                                                                       *)
(*************************************************************************)

let verify_one_chunk s =
(*  lprintf "verify_one_chunk: %d networks\n" (List.length s.s_networks);  *)
  List.iter (fun t ->
(*      lprintf "verify_one_chunk of file %d\n" (file_num t.t_file); *)
      let bitmap = t.t_converted_verified_bitmap in
      for i = 0 to String.length bitmap - 1 do
        if bitmap.[i] = '2' then begin
(*            lprintf "  verifying...\n"; *)
            verify_chunk t i;
            raise Exit
          end
      done
  ) s.s_networks;
(*  lprintf "verify_one_chunk: nothing done\n"; *)
  ()
  
(*************************************************************************)
(*                                                                       *)
(*                         verify_some_chunks                            *)
(*                                                                       *)
(*************************************************************************)

let verify_some_chunks () =
  HS.iter (fun s ->
      try
        verify_one_chunk s
      with _ -> ()) swarmers_by_name

(*************************************************************************)
(*                                                                       *)
(*                         verify_one_chunk                              *)
(*                                                                       *)
(*************************************************************************)

let verify_one_chunk t =
  verify_one_chunk t.t_s
  
(*************************************************************************)
(*                                                                       *)
(*                         merge                                         *)
(*                                                                       *)
(*************************************************************************)

let merge f1 f2 = 
  
  let s1 = HS.find swarmers_by_name { dummy_swarmer with s_filename = file_disk_name f1 } in
  let s2 = HS.find swarmers_by_name { dummy_swarmer with s_filename = file_disk_name f2 } in
  
  if s1 == s2 then 
    failwith "Files are already sharing their swarmer";
  
  if s1.s_size <> s2.s_size then
    failwith "Files don't have the same size";
  
  let t2 = match s2.s_networks with
      [t] -> t
    | list -> 
        lprintf "s_networks: %d files\n" (List.length list);
        failwith "Second file is already merged with other files"
  in
  
  let t1 =
    match s1.s_networks with
      [] -> assert false
    | t1 :: _ -> 
        match t1.t_verifier with
          NoVerification -> 
            failwith "Cannot use first file as a primary for swarming (no verification scheme)"
        | _ -> t1
  in
  
  begin
    List.iter (fun (s, filename) ->
        for i = 0 to Array.length s.s_nuploading -  1 do
          if s.s_nuploading.(i) > 0 then
            failwith (Printf.sprintf "%s is currently being downloaded" filename)
        done
    ) [
      s1, "First file";
      s2, "Second file" ];
  end;
  
(* replace T2 swarmer *)
  associate false t2 t1.t_s
  
(*************************************************************************)
(*                                                                       *)
(*                         has_secondaries                               *)
(*                                                                       *)
(*************************************************************************)

let has_secondaries t =
  primary t && List.length t.t_s.s_networks > 1
  
(*************************************************************************)
(*                                                                       *)
(*                         subfiles                                      *)
(*                                                                       *)
(*************************************************************************)
  
let subfiles t =
  List.map (fun t -> t.t_file) t.t_s.s_networks    
  
(*************************************************************************)
(*                                                                       *)
(*                         SwarmerOption                                 *)
(*                                                                       *)
(*************************************************************************)
  
module SwarmerOption = struct
    
    let value_to_swarmer v =
      match v with
        Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let file_size = get_value "file_size" value_to_int64 in
          let file_name = get_value "file_name" value_to_string in
          let s = create_swarmer file_name file_size edonkey_range_size in
          let block_sizes = get_value "file_chunk_sizes" 
              (value_to_list value_to_int64) in
          List.iter (fun bsize ->
              split_blocks s bsize
          ) block_sizes;
          s
          
      | _ -> assert false
          
    let swarmer_to_value s =
      Module [
        ("file_size", int64_to_value s.s_size);
        ("file_name", string_to_value s.s_filename);
        ("file_bitmap", string_to_value s.s_verified_bitmap);
        ("file_chunk_sizes", list_to_value int64_to_value 
            (List.map (fun t -> t.t_block_size) s.s_networks));
        ]
    
    let t =
      define_option_class "Swarmer" value_to_swarmer swarmer_to_value
    
  end
  
(*************************************************************************)
(*                                                                       *)
(*                         check_swarmer                                 *)
(*                                                                       *)
(*************************************************************************)

let check_swarmer s =
  try
    match s.s_networks with
      [] -> ()
    | t :: tail ->
        assert  t.t_primary;
        
        for i = 0 to t.t_nchunks - 1 do
          List.iter (fun j ->
              if t.t_converted_verified_bitmap.[i] = '3' then begin
                  if s.s_verified_bitmap.[j] <> '3' then
                    failwith "Bad propagation of 3 from primary to main";
                end
              else
              if s.s_verified_bitmap.[j] = '3' then begin
                  failwith "Main has 3 not coming from primary";
                end
          ) t.t_t2s_blocks.(i)
        done;
        
        let fd = file_fd t.t_file in
        
        List.iter (fun t ->
            assert (not t.t_primary);
            assert (file_fd t.t_file == fd);
            
            for i = 0 to t.t_nchunks - 1 do
              List.iter (fun j ->
                  if t.t_converted_verified_bitmap.[i] = '3' then begin
                      if s.s_verified_bitmap.[j] <> '3' then 
                        failwith "3 in secondary without 3 in primary";
                    end
                  else
                  if t.t_converted_verified_bitmap.[i] = '2' then begin
                      if s.s_verified_bitmap.[j] <> '3' then
                        failwith "2 in secondary without 3 in primary";
                    end
              ) t.t_t2s_blocks.(i)
            done;          
        ) tail
  with e ->
      print_s "ERROR"s;
      raise e
  
(*************************************************************************)
(*                                                                       *)
(*                         Option swarmers                               *)
(*                                                                       *)
(*************************************************************************)
  
let swarmers =
  define_option CommonComplexOptions.swarmers_section
    ["swarmers"] "All the swarmers used" (list_option SwarmerOption.t) []
  
(*************************************************************************)
(*                                                                       *)
(*                         Options hooks                                 *)
(*                                                                       *)
(*************************************************************************)
  
let _ =
  set_after_save_hook files_ini (fun _ -> swarmers =:= []);
  set_before_save_hook files_ini (fun _ ->
      let list = ref [] in
      HS.iter (fun s -> 
          if s.s_networks <> [] then
          list := s :: !list) swarmers_by_name;
      swarmers =:= !list
  );
  set_after_load_hook files_ini (fun _ -> 
      List.iter (fun s ->
          check_swarmer s;
      ) !!swarmers;
      
      swarmers =:= [])
  
  
(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

(* Compute an approximation of the storage used by this module *)
    
let _ = 
  BasicSocket.add_infinite_timer 300. duplicate_chunks;
  Heap.add_memstat "CommonSwarming2" (fun level buf ->
      let counter = ref 0 in
      let nchunks = ref 0 in
      let nblocks = ref 0 in
      let nranges = ref 0 in
      HS.iter (fun s -> 
          let n = String.length s.s_verified_bitmap in
          nchunks := !nchunks + n;
          
          Array.iter (fun b ->
              match b with 
              | PartialBlock b ->
                  incr nblocks;
                  iter_block_ranges (fun _ -> incr nranges) b
              | _ -> ()                    
          ) s.s_blocks;
          
          incr counter
      ) swarmers_by_name;
      let block_storage = 64 * !nblocks in
      
      Printf.bprintf buf "  Swarmers: %d\n" !counter;
      Printf.bprintf buf "    nchunks: %d nblocks: %d nranges: %d\n"
        !nchunks !nblocks !nranges;
      Printf.bprintf buf "  Storage (without blocks): %d bytes\n"
        ( !counter * 108 +
          !nchunks * 17 + 
          !nblocks * 64 +
          !nranges * 84);
      
      let counter = ref 0 in
      let storage = ref 0 in
      HU.iter (fun up -> 
          storage := !storage + 76 + 
            Array.length up.up_complete_blocks * 4 +
            List.length up.up_ranges * (12 + 16 + 12 + 12 +  4) +
            Array.length up.up_partial_blocks * (16 + 12 + 12) +
            (8 + match up.up_chunks with
              AvailableRanges list -> List.length list * (12 + 12 + 12 + 12)
            | AvailableCharBitmap s -> 8 + String.length s
            | AvailableBoolBitmap a -> 4 * 4 * Array.length a
          ) ;
          incr counter;
      ) uploaders_by_num;
      Printf.bprintf buf "  Uploaders: %d\n" !counter;
      Printf.bprintf buf "  Storage: %d bytes\n" !storage;
  )

let check_finished t = 
  try
    let file = t.t_file in
    match file_state file with
      FileCancelled | FileShared | FileDownloaded -> false
    | _ ->
        let bitmap = verified_bitmap t in
        for i = 0 to String.length bitmap - 1 do
          if bitmap.[i] <> '3' then raise Not_found;
        done;  
        if file_size file <> downloaded t then
          lprintf "Downloaded size differs after complete verification\n";
        true
  with _ -> false      
      
  
end 


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         MODULE Make                                   *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(*
module Check = struct
    
    type f = {
        file_fd : Unix32.t;
        file_num : int;
        file_name : string;
        file_size : int64;
        mutable file_downloaded : int64;
        file_nchunks : int;
      }
    
    let ndownloaders = 10
    let seed = 4475
    let file = "test_download.tmp"      
    let range_size = Int64.of_int 150
    let block_sizes = [| 1000; 400; 299 |] 
    let block_sizes = Array.map Int64.of_int block_sizes
    
    let file_check_fd = Unix32.create_diskfile file Unix32.rw_flag 0o444
    
    module S = Make(struct
          
          module CommonTypes = struct
              type file = f
              type client = int
            end
          
          
          module CommonFile = struct
              let file_num f = f.file_num
              let file_size f = f.file_size
              let file_downloaded f = f.file_downloaded
              let add_file_downloaded f s =
                f.file_downloaded <- f.file_downloaded ++ s
(*                lprintf "File %d downloaded : %Ld\n" 
                  f.file_num f.file_downloaded *)
              
              let file_best_name f = f.file_name
              let file_verify file m fpos epos = 
                let len = epos -- fpos in
                let len = Int64.to_int len in
                let s = String.create len in
                let ss = String.create len in
                lprintf "file_verify: READ %Ld %d\n" fpos len;
                Unix32.flush_fd file.file_fd;
                Unix32.read file.file_fd fpos ss 0 len;
                Unix32.read file_check_fd fpos s 0 len;
                let result = (ss = s) in
                if not result then begin
                    lprintf "ERROR in file_verify %d %Ld-%Ld\n" file.file_num fpos epos;
(*                    lprintf "[%s]\n" (String.escaped ss);
                    lprintf "[%s]\n" (String.escaped s); *)
(*                     if !exit_on_error then exit 2; *)
                  end;
                result
              
              let file_mtime file = 0.
              
              let file_write f fpos s spos len = 
                lprintf "file_write: WRITE %d\n" len;
                Unix32.write f.file_fd fpos s spos len;
                let ss = String.create len in
                lprintf "file_write: READ %Ld %d\n" fpos len;
                Unix32.read file_check_fd fpos ss 0 len;
                if ss <> (String.sub s spos len) then
                  lprintf "Writting bad content !\n"
              
              let file_copy _ _ _ _ _ = ()
              let file_state file = FileDownloading
              let set_file_last_seen _ _ = ()
            end
          
          module CommonClient = struct
              let client_num c = c
            end
        
        
        end)
    
    
    let nfiles = Array.length block_sizes
    let file_size = Unix32.getsize file       
    
    let _ =
      
      Random.init 0;
      let first_seed = 21 in
      let last_seed = 200 in
      let seeds = ref [] in
      for i = 0 to last_seed do
        let seed = Random.int 65000 in
        if i >= first_seed then
          seeds := seed :: !seeds;
      done;
      
      List.iter (fun seed ->
          Random.init seed;
          
          let kernel = S.create_swarmer file_size range_size in
          let temp_filename =
            let filename = Printf.sprintf "temp/toto"  in
            (try Sys.remove filename with _ -> ());
            filename
          in
          
          let files = Array.init nfiles (fun i ->
                lprintf "Creating swarmer %d\n" i;
                
                let block_size = block_sizes.(i) in
                let nchunks = Int64.to_int (Int64.div 
                      (Int64.sub file_size Int64.one) block_size) + 1 in
                
                let file = {
                    file_fd = Unix32.create_diskfile temp_filename Unix32.rw_flag 0o666;
                    file_num = i;
                    file_name = "toto";
                    file_size = file_size;
                    file_downloaded = zero;
                    file_nchunks = nchunks;
                  }
                in
                Unix32.ftruncate64 file.file_fd file_size;
                let swarmer = S.create kernel file block_size in
                S.set_checksums (Some swarmer) (Array.create nchunks (Ed2k Md4.null));
                swarmer)
          in            
          lprintf "Swarmers created\n";
          S.print_t "--" files.(0);
          
          let check_finished swarmer = 
            try
              let bitmap = S.verified_bitmap swarmer in
              for i = 0 to String.length bitmap - 1 do
                if bitmap.[i] <> '3' then raise Not_found;
              done;  
              if file_size <> S.downloaded swarmer then
                lprintf "Downloaded size differs after complete verification (%Ld - %Ld)\n" file_size (S.downloaded swarmer);
              raise Exit
            with Not_found -> ()                  
          in
          
          let downloaders_counter = ref 0 in
          
          let downloader swarmer = 
            incr downloaders_counter;
            let d = !downloaders_counter in
            lprintf "Creating downloader %d\n" d;
            
            let nchunks = S.partition_size swarmer in
            let bitmap = String2.init nchunks (fun i ->
                  if (i lxor d) land 1 = 1 then '1' else '0') in
            let up = S.register_uploader swarmer d
                (AvailableCharBitmap bitmap) in
            let block = ref None in
            let ranges = ref [] in
            
            let receive_range () =
              match !ranges with
                [] -> assert false
              | (x,y,r) :: rs ->
                  
                  let maxlen = y -- x in
                  let maxlen = Int64.to_int maxlen in
                  if maxlen = 0 then begin
                      lprintf "(%d) ERROR range nul asked\n" d;
                      exit 0
                    end;
                  let len = max (Random.int maxlen) 1 in
                  if len = maxlen then
                    ranges := rs
                  else 
                    ranges := (x ++ Int64.of_int len, y, r) :: rs;
                  
                  let s = String.create len in
                  lprintf "(%d) reading %Ld %d\n" d x len;
                  Unix32.read file_check_fd x s 0 len;
                  
                  if Random.int 100 = 0 then begin
                      lprintf "CORRUPTING data at %Ld\n" x;
                      s.[0] <- '\000';
                    end;
                  
                  lprintf "(%d) received %Ld %d\n" d x len;
                  S.received up x s 0 len;            
                  check_finished swarmer            
            in
            let ask_range () =
              try
                let rec iter () =
                  match !block with
                    None ->
                      
                      lprintf "(%d) find_block\n" d;
                      let b = S.find_block up in
                      block := Some b;
                      iter ()
                  
                  | Some b ->
                      
                      try
                        lprintf "(%d) find_range\n" d;
                        let (x,y,r) = S.find_range up in
                        lprintf "(%d) asked %Ld-%Ld\n" d x y;
                        ranges := !ranges @ [x,y,r]
                      
                      with Not_found ->
                          block := None;
                          iter ()
                in
                iter ()
              with Not_found -> 
                  lprintf "(%d) Unable to find a block to download\n" d;
                  S.compute_bitmap swarmer;
                  check_finished swarmer            
            in
            (fun receive -> 
                lprintf "downloader %d %d for seed %d\n" d receive seed;
                let nranges = List.length !ranges in
                if receive = 1 && nranges > 0 then receive_range () else
                if receive = 0 && nranges < 3 then ask_range () else
                if nranges < 3 then ask_range () else
                if nranges > 0 then receive_range ();
                
                lprintf "downloader done\n";
            )
          in    
          lprintf "Downloaders created\n";
          let downloaders = Array.init ndownloaders (fun _ -> 
                downloader files.(Random.int nfiles)) in
          try
            lprintf "Start loop\n";
            while true do
              downloaders.(Random.int ndownloaders) (Random.int 2)
            done
          with Exit -> 
              lprintf "File correctly downloaded\n";
      ) !seeds ;
      exit 0
  end
*)