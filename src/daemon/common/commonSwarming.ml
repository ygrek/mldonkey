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

PROBLEMS:
  After 'update_uploader_chunks', an uploader can get a block with
  'find_block' that it has already used.
  *)

open Int64ops
open Options
open Printf2
  
open CommonClient
open CommonFile
open CommonTypes
open CommonOptions

  
let check_swarming = false
let debug_present_chunks = false
let debug_all = false
        

type strategy =
  LinearStrategy    (* one after the other one *)
| AdvancedStrategy  (* first chunks first, rarest chunks second, 
     complete first third, and random final *)
    
exception VerifierNotReady

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         MODULE Integer                                *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
  
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         MODULE Swarmer                                *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
  
module type Swarmer =
  sig
    type t
    and range
    and uploader
    and block
      
    type chunks =
      AvailableRanges of (int64 * int64) list
(* A bitmap is encoded with '0' for empty, '1' for present *)
    | AvailableCharBitmap of string 
(* A bitmap encoded as an array of boolean *)
    | AvailableBoolBitmap of bool array
      
    val create : file -> int64 -> int64 -> t
    val set_writer : t -> (int64 -> string -> int -> int -> unit) -> unit
    val set_verifier : t -> (int -> int64 -> int64 -> bool) -> unit
      
    val set_present : t -> (int64 * int64) list -> unit
    val set_absent : t -> (int64 * int64) list -> unit
      
    val verified_bitmap : t -> string
    val set_verified_bitmap : t -> string -> unit
      
    val register_uploader : t -> client -> chunks -> uploader
    val update_uploader : uploader -> chunks -> unit
    val disconnect_uploader : uploader -> unit      
      
    val find_block : uploader -> block
    val find_range : uploader -> range

    val received : uploader -> int64 -> string -> int -> int -> unit
    val print_t : string -> t -> unit
    val range_range : range -> int64 * int64

    val print_block : block -> unit
    val block_block : block -> int * int64 * int64 
    val availability : t -> string
    val downloaded : t -> int64
    val present_chunks : t -> (int64 * int64) list
    val partition_size : t -> int
    val compute_bitmap : t -> unit
    val is_interesting : uploader -> bool	
    val print_uploader : uploader -> unit

    val last_seen : t -> int array
      
    val print_uploaders : t -> unit

    val uploader_swarmer : uploader -> t
      
    val swarmer_to_value : t ->
      (string * Options.option_value) list ->
      (string * Options.option_value) list
    val value_to_swarmer : t -> (string * Options.option_value) list -> unit
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
  
module Int64Swarmer = (struct
      
      
      let (++) = Int64.add
      let ( ** ) x y = Int64.mul x (Int64.of_int y)
      let (--) = Int64.sub
      
      let zero = Int64.zero

(* Worst scenario?: 1 GB splitted in small ranges of 64 KB = 16 000 ranges.
  In eDonkey, ranges are 180 kB long.
*)

(*    let range_size = ref (Int64.of_int (64 * 1024)) *)
      let block_size = ref (Int64.of_int (1024 * 1024))
      let min_range_size = ref  (Int64.of_int (64 * 1024))
      
      type chunks =
        AvailableRanges of (int64 * int64) list
(* A bitmap is encoded with '0' for empty, '1' for present *)
      | AvailableCharBitmap of string 
(* A bitmap encoded as an array of boolean *)
      | AvailableBoolBitmap of bool array
        
      
      type t = {
          t_file : file;
          
          mutable t_size : int64;
          mutable t_block_size : int64;
          mutable t_range_size : int64;
          mutable t_strategy : strategy;
          
          mutable t_write : (int64 -> string -> int -> int -> unit);
          mutable t_verifier : (int -> int64 -> int64 -> bool) option;
          
          mutable t_verified_bitmap : string;
          mutable t_blocks : block_v array;
          mutable t_availability : int array;
          mutable t_nuploading : int array;
          mutable t_last_seen : int array;
          
          mutable t_ncomplete_blocks : int;
          mutable t_nverified_blocks : int;
          mutable t_downloaded : int64;
        }
      
      and block_v = 
        EmptyBlock
      | PartialBlock of block
      | CompleteBlock
      | VerifiedBlock
      
      and block = {
          block_t : t;
          block_num : int;
          block_begin : Int64.t;
          block_end : Int64.t;
          mutable block_ranges : range;
          mutable block_remaining : int64;
        }
      
      and range = {
          range_block : block;
          range_begin : Int64.t; (* official begin int64 *)
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
          
          mutable up_ranges : range list;
        }
      
      
      let basic_write _ _ _ _ = ()


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

      let last_seen t =
        for i = 0 to String.length t.t_verified_bitmap - 1 do
          if t.t_verified_bitmap.[i] = '2' then
            t.t_last_seen.(i) <- BasicSocket.last_time ()
        done;
        t.t_last_seen
        
(*************************************************************************)
(*                                                                       *)
(*                         create                                        *)
(*                                                                       *)
(*************************************************************************)
      
      let create file chunk_size range_size = 
        
        let size = file_size file in
        let nchunks = 
          1 + Int64.to_int (
            Int64.div (Int64.sub size Int64.one) chunk_size) in
        
        let rec t = {
            
            t_file = file;
            
            t_size = size;
            t_block_size = chunk_size;
            t_range_size = range_size;
            t_strategy = AdvancedStrategy;
            
            t_downloaded = zero;
            t_ncomplete_blocks = 0;
            t_nverified_blocks = 0;
            
            t_verified_bitmap = String.make nchunks '0';
            t_blocks = Array.create nchunks EmptyBlock ;
            t_availability = Array.create nchunks 0;
            t_nuploading = Array.create nchunks 0;
            t_last_seen = Array.create nchunks 0;
            
            t_verifier = None;
            t_write = basic_write;
          }
        in 
        t


(*************************************************************************)
(*                                                                       *)
(*                         apply_intervals (internal)                    *)
(*                                                                       *)
(*************************************************************************)
      
      let apply_intervals t f chunks =
        let nchunks = Array.length t.t_blocks in
        let find_block chunk_pos =
          let pos = Int64.to_int (Int64.div chunk_pos t.t_block_size) in
          if debug_all then 
            lprintf "%s is block %d\n" (Int64.to_string chunk_pos) pos;
          pos
        in
        let rec iter chunks =
          match chunks with
            [] -> ()
          | (chunk_begin, chunk_end) :: tail ->
              let chunk_begin = min chunk_begin t.t_size in
              let chunk_end = min chunk_end t.t_size in
              if chunk_begin < chunk_end then begin
                  let i0 = find_block chunk_begin in
                  let block_begin = t.t_block_size ** i0 in
                  let rec iter_blocks i block_begin chunk_begin =
                    if i < nchunks && block_begin < chunk_end then
                      let block_end = block_begin ++ t.t_block_size in
                      let block_end = min block_end t.t_size in
                      
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
(*                         set_writer                                    *)
(*                                                                       *)
(*************************************************************************)
      
      let set_writer t f = 
        t.t_write <- f


(*************************************************************************)
(*                                                                       *)
(*                         print_t                                       *)
(*                                                                       *)
(*************************************************************************)
      
      let print_t s t =
        lprintf "Ranges after %s: " s;
        
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
            match b with
              PartialBlock b ->
                lprintf " [%s .. %s] --> " 
                  (Int64.to_string b.block_begin)
                (Int64.to_string b.block_end);
                iter b.block_ranges
            | EmptyBlock -> lprintf "_\n"
            | CompleteBlock -> lprintf "C\n"
            | VerifiedBlock -> lprintf "V\n"
        ) t.t_blocks


(*************************************************************************)
(*                                                                       *)
(*                         print_block                                   *)
(*                                                                       *)
(*************************************************************************)
      
      let print_block b =
        lprintf "Block %d: %s-%s\n" 
          b.block_num
          (Int64.to_string b.block_begin)
        (Int64.to_string b.block_end);
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
(*                         close_ranges (internal)                       *)
(*                                                                       *)
(*************************************************************************)
      let rec close_ranges t r =
        
        let added = r.range_end -- r.range_current_begin in
        t.t_downloaded <- t.t_downloaded ++ added;
        let b = r.range_block in
        b.block_remaining <- b.block_remaining -- added;
        
        r.range_current_begin <- r.range_end;
        match r.range_next with
          None -> ()
        | Some rr -> close_ranges t rr

(*************************************************************************)
(*                                                                       *)
(*                         verify_block (internal)                       *)
(*                                                                       *)
(*************************************************************************)
      
      let verify_block t i = 
        match t.t_verifier with
          None -> ()
        | Some f ->
            try
              let block_begin = t.t_block_size ** i in
              let block_end = block_begin ++  t.t_block_size in
              let block_end = min block_end t.t_size in
              if f i block_begin block_end then begin
                  t.t_nverified_blocks <- t.t_nverified_blocks + 1;
                  begin
                    match t.t_blocks.(i) with
                      PartialBlock b ->
                        
                        t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
                        close_ranges t b.block_ranges
                    | EmptyBlock ->                 
                        t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
                    | _ -> ()
                  end;
                  
                  t.t_verified_bitmap.[i] <- '3';          
                  t.t_blocks.(i) <- VerifiedBlock;
                
                end else begin
                  match t.t_blocks.(i) with
                    EmptyBlock | PartialBlock _ -> ()
                  | CompleteBlock ->
                      t.t_ncomplete_blocks <- t.t_ncomplete_blocks - 1;
                      t.t_downloaded <- 
                        t.t_downloaded -- (block_end -- block_begin);
                      
                      t.t_blocks.(i) <- EmptyBlock;
                      t.t_verified_bitmap.[i] <- '0'
                  
                  | _ -> assert false
                end
            with VerifierNotReady -> ()

(*************************************************************************)
(*                                                                       *)
(*                         compute_bitmap                                *)
(*                                                                       *)
(*************************************************************************)
      
      
      let compute_bitmap t =
        if t.t_ncomplete_blocks > t.t_nverified_blocks then begin
            for i = 0 to Array.length t.t_blocks do
              match t.t_blocks.(i) with
                CompleteBlock ->
                  verify_block t i
              | _ -> ()
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
                rrr.range_prev <- Some rr;
          end;
          r.range_next <- Some rr;
          r.range_end <- next_range;
          
          split_range rr range_size


(*************************************************************************)
(*                                                                       *)
(*                         new_block (internal)                          *)
(*                                                                       *)
(*************************************************************************)
      
      let new_block t i =
        let block_begin = t.t_block_size ** i in
        let block_end = block_begin ++ t.t_block_size in
        let block_end = min block_end t.t_size in
        let rec b = {
            block_t = t;
            
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
        
        split_range range t.t_range_size;
        
        t.t_blocks.(i) <- PartialBlock b;
        t.t_verified_bitmap.[i] <- '1';
        if debug_all then lprintf "NB[%s]" t.t_verified_bitmap;
        b


(*************************************************************************)
(*                                                                       *)
(*                         next_range (internal)                         *)
(*                                                                       *)
(*************************************************************************)
      
      let next_range f r =
        match r.range_next with
          None -> ()
        | Some r -> f r


(*************************************************************************)
(*                                                                       *)
(*                         range_received (internal)                     *)
(*                                                                       *)
(*************************************************************************)
      
      let range_received r chunk_begin chunk_end =
        if r.range_begin < chunk_end && r.range_end > chunk_begin then begin
            
            let new_current_begin = 
              max (min chunk_end r.range_end) r.range_current_begin in
            let downloaded = new_current_begin -- r.range_current_begin in
            let b = r.range_block in
            let t = b.block_t in
            t.t_downloaded <- t.t_downloaded ++ downloaded;
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
                              match t.t_blocks.(b.block_num) with
                                PartialBlock _ | EmptyBlock ->
                                  t.t_ncomplete_blocks <- 
                                    t.t_ncomplete_blocks + 1;
                                  begin match t.t_verifier with
                                      Some _ ->
                                        t.t_blocks.(b.block_num) <- CompleteBlock;
                                        t.t_verified_bitmap.[b.block_num] <- '2';
                                        verify_block t (b.block_num)
                                    
                                    | _ ->
                                        t.t_blocks.(b.block_num) <- VerifiedBlock;
                                        t.t_verified_bitmap.[b.block_num] <- '3';
                                        t.t_nverified_blocks <- t.t_nverified_blocks + 1;
                                  end
                              | _ -> ()
                            end
                        | Some rr -> b.block_ranges <- rr
                      end;
                  | Some rr -> rr.range_next <- r.range_next);
              
              end
          end


(*************************************************************************)
(*                                                                       *)
(*                         recompute_downloaded (internal)               *)
(*                                                                       *)
(*************************************************************************)

      let recompute_downloaded t = ()

(*************************************************************************)
(*                                                                       *)
(*                         set_present_block (internal)                  *)
(*                                                                       *)
(*************************************************************************)

(* Assumption: we never download ranges from the middle, so present chunks
  can only overlap the beginning of a range *)
      let set_present_block b chunk_begin chunk_end =
        let rec iter r =
          range_received r chunk_begin chunk_end;
          next_range iter r
        in
        iter b.block_ranges


(*************************************************************************)
(*                                                                       *)
(*                         set_present                                   *)
(*                                                                       *)
(*************************************************************************)
      
      let set_present t chunks = 
        apply_intervals t (fun i block_begin block_end chunk_begin chunk_end ->
            match t.t_blocks.(i) with
              EmptyBlock ->
                if block_begin >= chunk_begin && block_end <= chunk_end then
                  begin
                    t.t_blocks.(i) <- CompleteBlock;
                    t.t_verified_bitmap.[i] <- '2';
                    
                    t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
                    t.t_downloaded <- 
                      t.t_downloaded ++ (block_end -- block_begin)
                  end
                else
                let b = new_block t i in
                set_present_block b chunk_begin chunk_end
            | PartialBlock b ->
                set_present_block b chunk_begin chunk_end
            | _ -> ()
        ) chunks;
        recompute_downloaded ()

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
      
      let set_absent t list = 
(* reverse absent/present in the list and call set_present *)
        let list = 
          match list with [] -> [ Int64.zero, t.t_size ]
          | (t1,t2) :: tail ->
              if t1 = zero then
                end_present [] t2 t.t_size tail
              else
                end_present [zero, t1] t2 t.t_size tail
        in
        set_present t list

(*************************************************************************)
(*                                                                       *)
(*                         update_uploader_chunks (internal)             *)
(*                                                                       *)
(*************************************************************************) 
      
      let update_uploader_chunks up chunks =
        if not up.up_declared then
          let t = up.up_t in

(* INVARIANT: complete_blocks must be in reverse order *)
          
          let complete_blocks = ref [] in
          let partial_blocks = ref [] in
          
          begin
            match chunks with
              AvailableRanges chunks ->
                
                apply_intervals t (fun i block_begin block_end 
                      chunk_begin chunk_end ->
                    t.t_availability.(i) <- t.t_availability.(i) + 1;
                    
                    match t.t_blocks.(i) with
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
                  if string.[i] = '1' then begin
                      t.t_availability.(i) <- t.t_availability.(i) + 1;
                      complete_blocks := i :: !complete_blocks
                    end
                done;
            | AvailableBoolBitmap bitmap ->
                for i = 0 to Array.length bitmap - 1 do
                  if bitmap.(i) then begin
                      t.t_availability.(i) <- t.t_availability.(i) + 1;
                      complete_blocks := i :: !complete_blocks
                    end
                done;
          end;
          
          List.iter (fun i ->
              t.t_last_seen.(i) <- BasicSocket.last_time ()
          ) !complete_blocks;
          
          let complete_blocks = Array.of_list !complete_blocks in
          let partial_blocks = Array.of_list !partial_blocks in
          up.up_chunks <- chunks;
          
          up.up_complete_blocks <- complete_blocks;
          up.up_ncomplete <- Array.length complete_blocks;
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
          
          let decr_availability t i =
            t.t_availability.(i) <- t.t_availability.(i) - 1
          in
(*          lprintf "clean_uploader_chunks:\n"; *)
          
          let t = up.up_t in
          for i = 0 to Array.length up.up_complete_blocks - 1 do
(*            lprintf "decr_availability complete[%d] %d\n" i
              up.up_complete_blocks.(i); *)
            decr_availability t up.up_complete_blocks.(i)
          done;
          for i = 0 to Array.length up.up_partial_blocks - 1 do
            let b,_,_ = up.up_partial_blocks.(i) in
(*            lprintf "decr_availability partial[%d] %d\n" i b; *)
            decr_availability t b
          done;
          (match up.up_block with
              None -> ()
            | Some b ->
                let num = b.block_num in
                t.t_nuploading.(num) <- t.t_nuploading.(num) - 1);
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
        update_uploader_chunks up chunks;
        up

(*************************************************************************)
(*                                                                       *)
(*                         unregister_uploader                           *)
(*                                                                       *)
(*************************************************************************) 
      
      let disconnect_uploader up = 
        clean_uploader_chunks up;
        List.iter (fun r ->
            r.range_nuploading <- r.range_nuploading - 1
        ) up.up_ranges;
        up.up_block <- None;
        up.up_ranges <- []

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
      
      let print_uploaders t =
        let nblocks = Array.length t.t_blocks in
        for i = 0 to nblocks - 1 do
          
          match t.t_blocks.(i) with
            EmptyBlock -> lprintf "_"
          | CompleteBlock -> lprintf "C"
          | VerifiedBlock -> lprintf "V"
          | PartialBlock b ->
              if t.t_nuploading.(i) > 9 then
                lprintf "X"
              else
                lprintf "%d" t.t_nuploading.(i)
        done;
        lprintf "\n";
        for i = 0 to nblocks - 1 do
          
          match t.t_blocks.(i) with
            EmptyBlock -> lprintf "_"
          | CompleteBlock -> lprintf "C"
          | VerifiedBlock -> lprintf "V"
          | PartialBlock b ->
              lprintf "{%d : %d=" b.block_num 
                t.t_nuploading.(b.block_num);
              
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
        if debug_all then lprintf "permute_and_return %d <> %d" n b;
        up.up_complete_blocks.(n) <- up.up_complete_blocks.(up.up_ncomplete-1);
        up.up_complete_blocks.(up.up_ncomplete-1) <- b;
        up.up_ncomplete <- up.up_ncomplete - 1;
        let t = up.up_t in
        match t.t_blocks.(b) with
          EmptyBlock -> 
            let b = new_block t b in
            b, b.block_begin, b.block_end
        | PartialBlock b -> 
            b, b.block_begin, b.block_end
        | _ -> assert false

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
              match t.t_blocks.(b) with
                CompleteBlock | VerifiedBlock -> 
                  iter_complete up
              | PartialBlock b -> 
                  b, b.block_begin, b.block_end
              | EmptyBlock ->
                  let b = new_block t b in
                  b, b.block_begin, b.block_end
            
            and iter_partial up =
              let n = up.up_npartial in
              if n = 0 then raise Not_found;
              let t = up.up_t in
              let b, block_begin, block_end = up.up_partial_blocks.(n-1) in
              let t = up.up_t in
              match t.t_blocks.(b) with
                CompleteBlock | VerifiedBlock -> 
                  iter_partial up
              | PartialBlock b -> 
                  b, block_begin, block_end
              | EmptyBlock ->
                  let b = new_block t b in
                  b, block_begin, block_end
            in
            iter_complete up
        end

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
        match t.t_strategy with
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
                
                try
                  
                  let rec iter_max_uploaders max_nuploaders =
                    let t = up.up_t in
                    let nblocks = Array.length t.t_blocks in

(*************   Try to download the movie index and the first minute to
   allow preview of the file as soon as possible *)
                    
                    if debug_all then lprintf "{First}";
                    
                    let download_first n b =
                      if 
                        up.up_complete_blocks.(n) = b &&
                        t.t_nuploading.(nblocks - 1) < max_nuploaders &&
                        t.t_verified_bitmap.[nblocks - 1] < '2' then
                        raise (BlockFound n)
                    in

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
                    
                    if debug_all then lprintf "{Partial}";
                    
                    let download_partial max_uploaders =
                      let partial_block = ref (-1) in
                      let partial_remaining = ref zero in
                      for i = 0 to up.up_ncomplete - 1 do
                        let n = up.up_complete_blocks.(i) in
                        match t.t_blocks.(n) with
                          PartialBlock b ->
                            if (!partial_block = -1 ||
                                !partial_remaining > b.block_remaining) &&
                              t.t_nuploading.(n) < max_uploaders
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
                    
                    if t.t_verifier <> None &&
                      t.t_nverified_blocks + t.t_ncomplete_blocks < 2  then begin
                        download_partial max_nuploaders;
                      end;

(************* Download rarest first only if other blocks are much more
  available *)
                    
                    if debug_all then lprintf "{Rarest}";
                    
                    let sum_availability = ref 0 in
                    let min_availability = ref max_int in
                    for i = 0 to up.up_ncomplete - 1 do
                      let n = up.up_complete_blocks.(i) in
                      sum_availability := !sum_availability +
                        t.t_availability.(n);
                      min_availability := min !min_availability 
                        t.t_availability.(n);
                    done;
                    
                    let mean_availability = 
                      !sum_availability / up.up_ncomplete in
                    
                    if mean_availability > 5 && !min_availability < 3 then
                      for i = 0 to up.up_ncomplete - 1 do
                        let n = up.up_complete_blocks.(i) in
                        if t.t_availability.(n) < 3 
                            && t.t_verified_bitmap.[n] < '2'
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
                        if t.t_nuploading.(n) < max_uploaders
                            && t.t_verified_bitmap.[n] < '2'
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
                    permute_and_return up n
              end else
              LinearStrategy.select_block up

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
          (match up.up_block with
              None -> ()
            | Some b ->
                let num = b.block_num in
                t.t_nuploading.(num) <- t.t_nuploading.(num) - 1;            
          );
          let (b,block_begin,block_end) as result = select_block up in
          let num = b.block_num in
          t.t_nuploading.(num) <- t.t_nuploading.(num) + 1;
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
          | r :: tail ->
              iter tail
                (if r.range_current_begin < r.range_end then r :: left
                else begin
                    r.range_nuploading <- r.range_nuploading - 1;
                    left
                  end)
        in
        up.up_ranges <- iter up.up_ranges []

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
        
        let rec iter limit r =

(* let use a very stupid heuristics: ask for the first non-used range.
we thus might put a lot of clients on the same range !
*)
          
          if not (List.memq r up.up_ranges) &&
            r.range_current_begin < r.range_end &&
            r.range_current_begin >= up.up_block_begin &&
            r.range_end <= up.up_block_end &&
            r.range_nuploading < limit
          then begin
              up.up_ranges <- up.up_ranges @ [r];
              r.range_nuploading <- r.range_nuploading + 1;
              if r.range_current_begin = r.range_end then
                lprintf "CS error: range is empty <<<<<<<<<<<--------- error\n";
              r
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
        (s:string) (string_begin:int) (string_len:int) =
        
        if string_len > 0 then
          let file_end = file_begin ++ (Int64.of_int string_len) in
          
          if !verbose_swarming then
            lprintf "received on %s-%s\n" 
              (Int64.to_string file_begin)
            (Int64.to_string file_end);
          
          List.iter (fun r ->
              if r.range_current_begin < file_end &&
                r.range_end > file_begin then begin
                  
                  let new_current_begin = min file_end r.range_end in
                  let written_len = new_current_begin -- r.range_current_begin in
                  
                  let t = up.up_t in
                  
                  t.t_write r.range_current_begin s
                    (string_begin + 
                      Int64.to_int (r.range_current_begin -- file_begin))
                  (Int64.to_int written_len);
                  
                  range_received r r.range_current_begin file_end;
                
                end
          ) up.up_ranges

(*************************************************************************)
(*                                                                       *)
(*                         present_chunks                                *)
(*                                                                       *)
(*************************************************************************)
      
      let present_chunks t =
        let nblocks = Array.length t.t_blocks in
        
        let rec iter_block_out i block_begin list = 
          if debug_present_chunks then
            lprintf "iter_block_out %d bb: %s\n"
              i
              (Int64.to_string block_begin); 
          
          let block_end = block_begin ++ t.t_block_size in
          let block_end = min block_end t.t_size in
          if i = nblocks then List.rev list else
          match t.t_blocks.(i) with
            EmptyBlock ->
              iter_block_out (i+1) block_end list
          | CompleteBlock | VerifiedBlock ->
              let block_begin = t.t_block_size ** i in
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
          
          let block_end = block_begin ++ t.t_block_size in
          let block_end = min block_end t.t_size in
          if i = nblocks then 
            let list = (chunk_begin, t.t_size) :: list in
            List.rev list
          else
          match t.t_blocks.(i) with
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
        iter_block_out 0 zero []
        
(*************************************************************************)
(*                                                                       *)
(*                         set_downloaded_block                          *)
(*                                                                       *)
(*************************************************************************)

      let set_downloaded_block t i =
        match t.t_blocks.(i) with
          EmptyBlock ->
            let block_begin = t.t_block_size ** i in
            let block_end = min (block_begin ++ t.t_block_size) t.t_size in
            t.t_downloaded <- t.t_downloaded ++ (block_end -- block_begin)
        | PartialBlock b ->
            let rec iter r =
              t.t_downloaded <- t.t_downloaded ++ 
                (r.range_end -- r.range_current_begin);
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
(*                         set_verified_bitmap                           *)
(*                                                                       *)
(*************************************************************************)
      
      let set_verified_bitmap t bitmap =
        t.t_verified_bitmap <- bitmap;
        
        for i = 0 to String.length bitmap - 1 do
          match t.t_blocks.(i) with
            VerifiedBlock -> ()
          | CompleteBlock when bitmap.[i] = '3' ->
              t.t_nverified_blocks <- t.t_nverified_blocks + 1;
              t.t_blocks.(i) <- VerifiedBlock
          | EmptyBlock | PartialBlock _ when bitmap.[i] = '3' ->
              t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
              t.t_nverified_blocks <- t.t_nverified_blocks + 1;
              set_downloaded_block t i;
              t.t_blocks.(i) <- VerifiedBlock
          | EmptyBlock | PartialBlock _ when bitmap.[i] = '2' ->
              t.t_ncomplete_blocks <- t.t_ncomplete_blocks + 1;
              set_downloaded_block t i;
              t.t_blocks.(i) <- CompleteBlock;
              verify_block t i
          | _ -> ()
        done;
        recompute_downloaded t

(*************************************************************************)
(*                                                                       *)
(*                         verified_bitmap                               *)
(*                                                                       *)
(*************************************************************************)
      
      let verified_bitmap t = t.t_verified_bitmap

(*************************************************************************)
(*                                                                       *)
(*                         set_verifier                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let set_verifier t f = 
        t.t_verifier <- Some f;
        set_verified_bitmap t t.t_verified_bitmap

(*************************************************************************)
(*                                                                       *)
(*                         downloaded                                    *)
(*                                                                       *)
(*************************************************************************)
      
      let downloaded t = t.t_downloaded

(*************************************************************************)
(*                                                                       *)
(*                         block_block                                   *)
(*                                                                       *)
(*************************************************************************)
      
      let block_block b = b.block_num, b.block_begin, b.block_end

(*************************************************************************)
(*                                                                       *)
(*                         partition_size                                *)
(*                                                                       *)
(*************************************************************************)
      
      let partition_size t = Array.length t.t_blocks

      let uploader_swarmer up = up.up_t
        
(*************************************************************************)
(*                                                                       *)
(*                         availability                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let availability t =
        let len = Array.length t.t_blocks in
        let s = String.make len '\000' in
        for i = 0 to len - 1 do
          s.[i] <- char_of_int (
            if t.t_availability.(i) < 0 then 0 else
            if t.t_availability.(i) > 200 then 200 else t.t_availability.(i));
        done;
        s

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
(*                         value_to_swarmer                              *)
(*                                                                       *)
(*************************************************************************)
      
      let value_to_swarmer t assocs = 
        let get_value name conv = conv (List.assoc name assocs) in

        lprintf "Setting bitmap\n";
        (try
            
            let set_bitmap =
              let mtime = Unix32.mtime64 (file_fd t.t_file) in
              let old_mtime = 
                try 
                  value_to_float (List.assoc "file_mtime" assocs) 
                with Not_found -> mtime
              in
              old_mtime = mtime
            in
            
            set_verified_bitmap t
              (get_value  "file_chunks" value_to_string)
          with e -> 
              lprintf "Exception %s while loading bitmap\n"
                (Printexc2.to_string e); 
        );
        
        lprintf "Loading present...\n";
        (try 
            set_present t 
              (get_value "file_present_chunks" 
                (value_to_list value_to_int64_pair));
          with e ->
              lprintf "Exception %s while set present\n"
                (Printexc2.to_string e);         
        );
        lprintf "Downloaded after present %Ld\n" (downloaded t);
        
        ()      

(*************************************************************************)
(*                                                                       *)
(*                         swarmer_to_value                              *)
(*                                                                       *)
(*************************************************************************)
      
      let swarmer_to_value t other_vals = 
        ("file_mtime", float_to_value (Unix32.mtime64 (file_fd t.t_file))) ::
        ("file_chunks", string_to_value (verified_bitmap t)) ::
        ("file_present_chunks", List
            (List.map (fun (i1,i2) -> 
                SmallList [int64_to_value i1; int64_to_value i2])
            (present_chunks t))) ::
        other_vals
        
  end : Swarmer)

(*
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         MODULE Check                                  *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
  
(* Fake a client downloading from another one to verify that CommonSwarming
  functions work correctly *)
  
module Check = struct  
    
    open CommonGlobals
    open Md4
    open Int64Swarmer
    
    let zero = Int64.zero
    let one = Int64.one
    let (++) = Int64.add
    let (--) = Int64.sub
    let ( ** ) x y = Int64.mul x (Int64.of_int y)
    let ( // ) x y = Int64.div x (Int64.of_int y)
    
    let block_size = Int64.of_int 9728000
    
    let ed2k_hash_file fd file_size =
      let nchunks = Int64.to_int (Int64.div 
            (Int64.sub file_size Int64.one) block_size) + 1 in
      if nchunks = 1 then
        let md4 = Md4.digest_subfile fd zero file_size in
        md4 , [| md4 |]
      else
      let chunks = String.create (nchunks*16) in
      let md4s = Array.create nchunks Md4.null in
      for i = 0 to nchunks - 1 do
        let begin_pos = block_size ** i  in
        let end_pos = begin_pos ++ block_size in
        let end_pos = min end_pos file_size in
        let len = end_pos -- begin_pos in
        let md4 = Md4.digest_subfile fd begin_pos len in
        md4s.(i) <- md4;
        lprintf "  Partial %3d : %s\n" i (Md4.to_string md4);
        let md4 = Md4.direct_to_string md4 in
        String.blit md4 0 chunks (i*16) 16;
      done;
      Md4.string chunks, md4s
    
    
    
    let check_swarming size =
      
      let nchunks = 1024 in
      let test_string_len = 43676 in
      let dummy_string = "bonjourhello1" in
      
      let create_diskfile filename size =
        Unix32.create_diskfile filename Unix32.rw_flag 0o066
      in
      
      let test_string = String.create test_string_len in
      let rec iter pos =
        if pos < test_string_len then
          let end_pos = min test_string_len (2*pos) in
          let len = end_pos - pos in        
          String.blit test_string 0 test_string pos len;
          iter end_pos
      in
      
      let dummy_string_len = String.length dummy_string in
      String.blit dummy_string 0 test_string 0 dummy_string_len;
      iter dummy_string_len;
      
      let test_string_len64 = Int64.of_int test_string_len in
      let file_size = (Int64.of_int size) ** 1024 in
      let rec iter pos waves =
        if pos < file_size then
          let end_pos = min file_size (pos ++ test_string_len64) in
          let len64 = end_pos -- pos in        
          let len = Int64.to_int len64 in
          iter end_pos
            ((pos, len) :: waves)
        else
          waves
      in
      let waves = iter zero [] in
      
      let filename = "origin_file.test" in
      
      lprintf "Creating file %s\n" filename;
      let file = create_diskfile filename file_size in
      Unix32.ftruncate64 file file_size;
      
      lprintf "Filling file\n";
      List.iter (fun (pos, len) ->
          Unix32.write file pos test_string 0 len;                
      ) waves;
      
      lprintf "Computing ed2k hash\n";
      let md4, chunks = ed2k_hash_file file file_size in
      lprintf "ed2k://|file|%s|%Ld|%s|\n" 
        filename
        file_size
        (Md4.to_string md4);
      
      let origin_filename = filename in
      
      let new_file = create_diskfile (filename ^ ".new") file_size in
      
      let uploader (_,_,u,v) =
        match !u with
          [] -> ()
        | (begin_pos, len) :: tail ->
            u := tail;
            lprintf "!";
            let s = String.create len in
            Unix32.read file begin_pos s 0 len;
            v := (begin_pos, s) :: !v
      in
      
      let uploaders = [
          (zero, file_size, ref [], ref []);
        ]
      in
      
      
      
      let swarmer = Int64Swarmer.create file_size megabyte kilobytes256 in
      Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
          Unix32.write new_file offset s pos len);
      
      let ups = List.map (fun (begin_pos, end_pos, u, v) ->
            let up = Int64Swarmer.register_uploader swarmer 
                (Int64Swarmer.AvailableRanges [(begin_pos, end_pos)])
            in
            (up, ref None, ref [], u, v)
        ) uploaders in
      let downloader () =
        List.iter (fun (up, b, rs, r, v) ->

(* Use current requests *)
            List.iter (fun (begin_pos, s) ->
                let len = String.length s in
                lprintf "R[%Ld : %d]\n" begin_pos len;
                let d0 = downloaded swarmer in
                Int64Swarmer.received up begin_pos s 0 len;
                let d1 = downloaded swarmer in
                if d1 -- d0 <> Int64.of_int len then begin
                    lprintf "Error: %Ld - %Ld <> %d\n"
                      d1 d0 len
                  end
            ) !v;
            v := [];
            
            if List.length !r < 3 then begin
                
                rs := List.filter (fun r ->
                    let (x,y) = range_range r in
                    x < y) !rs;
                
                if List.length !rs < 3 then begin
                    
                    try
                      let rr = 
                        try
                          find_range up 
                        with Not_found ->
                            let bb = find_block up in
                            let (num,_,_) = block_block bb in
                            lprintf "B[%d]" num;
                            print_t "" swarmer;
                            b := Some bb;
                            find_range up
                      in
                      
                      rs := rr :: !rs;
                      
                      let (x,y) = range_range rr in
                      
                      let rec iter x y =
                        if x < y then begin
                            lprintf ".";                            
                            if x ++ kilobytes64 < y then begin
                                r := !r @ [x, Int64.to_int kilobytes64];
                                iter (x++kilobytes64) y
                              end else 
                              r := !r @ [x, Int64.to_int (y--x)]
                          end
                      in
                      iter x y
                    with Not_found -> lprintf "-"                    
                    
                  end
                
              end;
            
        ) ups
      in
            
      while downloaded swarmer < file_size do
        lprintf "_";
        List.iter (fun u -> uploader u) uploaders;
        downloader ();
        let l = present_chunks swarmer in
        lprintf "PRESENT (%Ld): " (downloaded swarmer);
        let sum = ref zero in
        let rec iter previous list =
          match list with
            [] -> ()
          | (bb, be) :: tail ->
              sum := !sum ++ (be -- bb);
              lprintf "[%Ld .. %Ld] " bb be;
              assert (bb > previous);
              iter be tail
        in
        iter (Int64.of_int (-1)) l;
        if !sum <> (downloaded swarmer) then lprintf "  XXXXXXXXXXXXXX";
        lprintf "\n"
      done;
      lprintf "File downloaded\n"
      
  end  

let _ =
  if check_swarming then Check.check_swarming 20000
      *)