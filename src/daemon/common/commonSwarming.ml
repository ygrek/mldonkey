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
  How to improve:
* Instead of itering on all ranges, we could build a second
  doubly-linked list of  'non-downloaded ranges'. Thus, operations
  of finding blocks and ranges would go faster.
* When consecutive ranges have been downloaded, we could merge them.
  What happens when a block is corrupted and we need to put back the
   ranges ?
*)



open CommonTypes
open Printf2

let verbose_swarming = ref false
  
module type Integer = sig
    type t
    val add : t -> t -> t
    val sub : t -> t -> t
    val zero : t 
    val of_int : int -> t
    val to_int : t -> int
    val to_string : t -> string
  end
  
module type Swarmer = sig
    type pos
    type t
    type block
    type range
    type partition
    
    val create : unit -> t
    val set_writer : t -> (pos -> string -> int -> int -> unit) -> unit
    val set_size : t -> pos -> unit
    val set_present : t -> (pos * pos) list -> unit
    val partition : t -> (pos -> pos) -> partition
          
    val set_verifier : partition -> (block -> bool) -> unit
    val verified_bitmap : partition -> string
    val set_verified_bitmap : partition -> string -> unit

    val register_uploader :     partition -> (pos * pos) list -> block list
    val unregister_uploader : t -> (pos * pos) list -> block list -> unit

    val register_uploader_bitmap : 
      partition -> string -> block list
    val unregister_uploader_bitmap : 
      partition -> string -> unit
    
    val get_block: block list -> block
    val find_range: block -> (pos * pos) list -> range list -> pos -> range
    val find_range_bitmap: block -> range list -> pos -> range
    
    val alloc_range : range -> unit
    val free_range : range -> unit
    val received : t -> pos -> string -> int -> int -> unit
    val sort_chunks :  (pos * pos) list ->  (pos * pos) list
    
    val print_t : string -> t -> unit
    val print_block : block -> unit
    val range_range: range ->  pos * pos
    val block_block: block -> int * pos * pos 
    val availability : partition -> string
     
    val downloaded : t -> pos
    val present_chunks : t -> (pos * pos) list
    val partition_size : partition -> int
    val debug_print : t -> string
    val compute_bitmap : partition -> unit
  end
  
module Make(Integer: Integer) = struct
    
    type pos = Integer.t
    
    let (++) = Integer.add
    let (--) = Integer.sub
    
    let zero = Integer.zero

(* Worst scenario?: 1 GB splitted in small ranges of 64 KB = 16 000 ranges.
  In eDonkey, ranges are 180 kB long.
*)
    
    let range_size = ref (Integer.of_int (64 * 1024))
    let block_size = ref (Integer.of_int (1024 * 1024))
    let min_range_size = ref  (Integer.of_int (64 * 1024))
    
    type t = {
        mutable t_write : (pos -> string -> int -> int -> unit);
        mutable t_size : Integer.t;
        mutable t_ranges : range;
        mutable t_partitions : partition list;
        mutable t_downloaded : pos;
      }
    
    and range = {
        range_t : t;
        range_begin : Integer.t; (* official begin pos *)
        mutable range_end : Integer.t;
        mutable range_prev : range option;
        mutable range_next : range option;
        mutable range_current_begin : Integer.t; (* current begin pos *)
(*        mutable range_verified : bool; *)
        mutable range_nuploaders : int;
        mutable range_ndownloaders : int;
      }
    
    and partition = {
        part_t : t;
        part_splitter : (Integer.t -> Integer.t);
        mutable part_blocks : block;
        mutable part_nblocks : int;
        mutable part_verifier : (block -> bool);
        mutable part_bitmap : string;
      }
    
    and block = {
        mutable block_prev : block option;
        mutable block_next : block option;
        block_begin : Integer.t;
        mutable block_end : Integer.t;
        mutable block_ranges : range;
        mutable block_partition : partition;
        mutable block_clients : client list;
        mutable block_num : int;
        mutable block_nuploaders : int;
      }
    exception VerifierNotImplemented
          
    let basic_write _ _ _ _ = ()
    
    let create _  = 
      let rec t = {
          t_write = basic_write;
          t_size = zero;
          t_ranges = range;
          t_partitions = [];
          t_downloaded = zero;
        } 
      and range = {
          range_prev = None;
          range_next = None;
          range_begin = zero;
          range_end = zero;
          range_t = t;
          range_nuploaders = 0;
          range_ndownloaders = 0;
(*          range_verified = false; *)
          range_current_begin = zero;
        }
      in t
    
    let set_writer t f = 
      t.t_write <- f
    
    let print_t s t =
      lprintf "Ranges after %s: " s;
      let rec iter r =
        lprintf " %s(%s)-%s(%d)" 
          (Integer.to_string r.range_begin)
        (Integer.to_string r.range_current_begin)
        (Integer.to_string r.range_end) r.range_nuploaders;
        match r.range_next with
          None -> lprintf "\n"
        | Some r -> iter r
      in
      iter t.t_ranges
    
    let print_block b =
      lprintf "Block %d: %s-%s\n" 
        b.block_num
        (Integer.to_string b.block_begin)
      (Integer.to_string b.block_end)
    
    let split_range r b_end =  
      if r.range_end = b_end || r.range_begin = b_end then r else
      let rr = {
          range_prev = Some r;
          range_next = r.range_next;
          range_begin = b_end;
          range_end = r.range_end;
          range_t = r.range_t;
          range_nuploaders = r.range_nuploaders;
          range_ndownloaders = r.range_ndownloaders;
(*          range_verified = false; *)
          range_current_begin = (
            if b_end < r.range_current_begin then
              r.range_current_begin else b_end);
        } in
      r.range_next <- Some rr;
      r.range_end <- b_end;
      if r.range_current_begin > b_end then
        r.range_current_begin <- b_end;
      rr
    
    let verify_block b = 
      let p = b.block_partition in
      let t = p.part_t in
      try
        if p.part_verifier b then
          p.part_bitmap.[b.block_num] <- '3'
        else begin
(* Currently, we set all the ranges to 0. In the future, it might be more
interesting to do that only for ranges that are not correct for other
partitions. *)
            
            
            let rec iter_block b =
              iter_ranges b b.block_ranges
            
            and iter_ranges b r =
              if r.range_begin < b.block_end then begin
                  t.t_downloaded <- t.t_downloaded --
                    (r.range_current_begin -- r.range_begin);
                  r.range_current_begin <- r.range_begin;
                  match r.range_next with
                    None -> ()
                  | Some rr -> iter_ranges b rr
                end
            in
            iter_block b;
            p.part_bitmap.[b.block_num] <- '0'
          end
      with 
      | VerifierNotImplemented -> 
          p.part_bitmap.[b.block_num] <- '3'
      | e -> 
          lprintf "ERROR: Exception %s in verify_block %d\n"
            (Printexc2.to_string e) b.block_num
    
    let compute_bitmap p =
      
      let rec iter_blocks b =
        iter_ranges b b.block_ranges (-1)
      
      and iter_ranges b r current =
        if r.range_begin >= b.block_end then begin
            if current = 0 then
              p.part_bitmap.[b.block_num] <- '0'
            else
            if current = 1 then
              p.part_bitmap.[b.block_num] <- '1'
            else
            if p.part_bitmap.[b.block_num] <> '3' then begin
                p.part_bitmap.[b.block_num] <- '2';
                verify_block b
              end;
            next_block b            
          end
        else
          next_range b r (
            if r.range_current_begin = r.range_begin then 
              begin
                if current = -1 || current = 0 then 0 else 1
              end          
            else
            if r.range_current_begin < r.range_end then
              begin
                1
              end
            else
            if current = 2 || current = -1 then 2 else 1
          )
      
      and next_range b r current =
        match r.range_next with
          None -> 
            if current = 0 then
              p.part_bitmap.[b.block_num] <- '0'
            else
            if current = 1 then
              p.part_bitmap.[b.block_num] <- '1'
            else
            if p.part_bitmap.[b.block_num] <> '3' then begin
                p.part_bitmap.[b.block_num] <- '2';
                verify_block b
              end
        | Some rr ->
            iter_ranges b rr current
      
      and next_block b =
        match b.block_next with
          None -> ()
        | Some bb -> iter_blocks bb
      in
      iter_blocks p.part_blocks
    
    let apply_partition t part = 
      if !verbose_swarming then
        lprintf "apply_partition\n";
      
      let rec iter_block b num =
        let r = b.block_ranges in
        let b_end = part.part_splitter b.block_begin in
        if b_end < b.block_end then 
          iter_range b r b_end num
        else num
      
      and iter_range b r b_end num =
        if r.range_end <= b_end then
          match r.range_next with
            None -> num (* we have finished *)
          | Some r -> iter_range b r b_end num
        else
        if r.range_begin = b_end then
          let bb = {
              block_prev = Some b;
              block_next = None;
              block_begin = b_end;
              block_end = b.block_end;
              block_ranges = r;
              block_partition = part;
              block_clients = b.block_clients;
              block_num = num;
              block_nuploaders = 0;
            }
          in
          b.block_next <- Some bb;
          b.block_end <- b_end;
          iter_block bb (num+1)
        else
          iter_range b (split_range r b_end) b_end num
      in
      part.part_nblocks <- iter_block part.part_blocks 1;
      if part.part_bitmap = "" then begin
          part.part_bitmap <- String.make part.part_nblocks '0';
          compute_bitmap part
        end
    
    let set_size t size = 
      if t.t_size <> size then 
        let r = t.t_ranges in
        if !verbose_swarming then
          lprintf "Setting size\n";
        t.t_size <- size;
        r.range_end <- size;
        List.iter (fun part ->
            if !verbose_swarming then
              lprintf "test partition\n";
            part.part_blocks.block_end <- size;
            apply_partition t part
        ) t.t_partitions
    
    let set_present t chunks = 
      let rec iter_before chunks r =
        match chunks with 
          [] -> ()
        | (chunk_begin, chunk_end) :: tail ->
            if !verbose_swarming then
              lprintf "chunk: %s-%s range: %s(%s)-%s\n"
                (Integer.to_string chunk_begin)
              (Integer.to_string chunk_end)
              (Integer.to_string r.range_begin)
              (Integer.to_string r.range_current_begin)
              (Integer.to_string r.range_end);
            if r.range_end <= chunk_begin then
              match r.range_next with
                None -> assert false
              | Some rr -> 
                  if !verbose_swarming then
                    lprintf "next range\n";
                  iter_before  chunks rr
            else
            if r.range_begin >= chunk_end then begin
                if !verbose_swarming then
                  lprintf "next chunk\n";
                iter_before tail r
              end
            else
            if r.range_current_begin < chunk_begin then
(* There is a hole in the range before the current chunk *)
              let r = split_range r chunk_begin in
              if !verbose_swarming then
                lprintf "range splitted\n";
              
              iter_before  chunks r
            
            else
            if r.range_end <= chunk_end then begin
(* This range is already downloaded *)
                t.t_downloaded <- t.t_downloaded ++ 
                  (r.range_end -- r.range_current_begin);
                if !verbose_swarming then 
                  lprintf "range completed %s\n" (Integer.to_string t.t_downloaded);
                r.range_current_begin <- r.range_end;
                match r.range_next with
                  None -> 
                    if not (r.range_end = chunk_end && 
                        List.length chunks <= 1) then
                      begin
                        lprintf "STRANGE ***********************\n";
                        lprintf "STRANGE ***********************\n";
                        lprintf "STRANGE ***********************\n";
                        lprintf "Last range: %s <> %s or chunks = %d>1\n"
                          (Integer.to_string r.range_end)
                        (Integer.to_string chunk_end)
                        (List.length chunks);
                      
                      
                      end
                | Some rr ->
                    iter_before chunks rr
              end
            else
              begin
                t.t_downloaded <- t.t_downloaded ++ 
                  (chunk_end -- r.range_current_begin);
                if !verbose_swarming then 
                  lprintf "range advanced %s\n" (Integer.to_string t.t_downloaded);
                r.range_current_begin <- chunk_end;
                iter_before tail r
              end
      
      in
      iter_before chunks t.t_ranges;
      List.iter compute_bitmap t.t_partitions


    let partition t f =
      let rec p = {
          part_t = t;
          part_splitter = f;
          part_blocks = b;
          part_nblocks = 0;
          part_verifier = (fun _ -> raise VerifierNotImplemented);
          part_bitmap = "";
        }
      
      and b = {
          block_prev = None;
          block_next = None;
          block_begin = zero;
          block_end = t.t_size;
          block_ranges = t.t_ranges;
          block_partition = p;
          block_clients = [];
          block_num = 0;
          block_nuploaders = 0;
        }
      in
      if !verbose_swarming then
        lprintf "partition\n";
      t.t_partitions <- p :: t.t_partitions;
      if t.t_size <> zero then 
        apply_partition t p;
      p
    
    let register_uploader p chunks =
      
      let rec iter_before b bs chunks r =
        match chunks with 
          [] -> List.rev bs
        | (chunk_begin, chunk_end) :: tail ->
            if r.range_end <= chunk_begin then
              next_range b bs chunks r
            else
            if r.range_begin >= chunk_end then
              iter_before b bs tail r
            else    
            if r.range_begin >= chunk_begin && r.range_end <= chunk_end then
              begin
                r.range_nuploaders <- 1 + r.range_nuploaders;
                next_range b (match bs with
                    [] -> 
                      b.block_nuploaders <- b.block_nuploaders + 1;
                      [b]
                  | bb :: _ -> 
                      if bb == b then bs else begin
                          b.block_nuploaders <- b.block_nuploaders + 1;
                          b :: bs
                        end)
                chunks r
              end
            else
            
            let new_chunk_begin =
              if r.range_begin < chunk_begin then 
                if chunk_begin -- r.range_begin < !min_range_size then
                  r.range_begin ++ !min_range_size
                else chunk_begin
              else r.range_begin
            in
            let new_chunk_end = 
              if r.range_end > chunk_end then
                if r.range_end -- chunk_end < !min_range_size then
                  r.range_end -- !min_range_size
                else chunk_end
              else r.range_end
            in
            if r.range_ndownloaders = 0 &&
              new_chunk_end -- new_chunk_begin >= !min_range_size then begin
                let r1 = split_range r new_chunk_begin in
                let r2 = split_range r1 new_chunk_end in
                iter_before b bs chunks r1
              end else
            if r.range_end >= chunk_end then
              iter_before b bs tail r
            else
              next_range b bs chunks r
      
      and next_range b bs chunks r =        
        match r.range_next with
        | Some rr ->  
            let b = match b.block_next with
                None -> b
              | Some bb -> if bb.block_ranges = rr then bb else b
            in
            iter_before b bs chunks rr        
        
        | None -> 
            match chunks with
              [] -> List.rev bs
            | [_, chunk_end] -> 
                assert (r.range_end >= chunk_end);
                List.rev bs
            | _ -> assert false
      in
      let t = p.part_t in
      iter_before p.part_blocks [] chunks t.t_ranges
    
    let register_uploader_bitmap p map =
      let len = String.length map in
      let rec iter b bs map len =
        if b.block_num >= len then List.rev bs else
        let bs =
          if map.[b.block_num] <> '0' then begin
              b.block_nuploaders <- b.block_nuploaders + 1;
              b :: bs
            end
          else bs
        in
        match b.block_next with
          None -> List.rev bs
        | Some bb -> iter bb bs map len
      in
      iter p.part_blocks [] map len
    
    let unregister_uploader_bitmap p map =
      let len = String.length map in
      let rec iter b map len =
        if b.block_num < len then  
          if map.[b.block_num] <> '0' then 
            b.block_nuploaders <- b.block_nuploaders - 1;
          match b.block_next with
            None -> ()
          | Some bb -> iter bb map len
      in
      iter p.part_blocks map len
    
    let sort_chunks chunks =
      Sort.list (fun ((a: Integer.t),_) ((b: Integer.t),_) -> a <= b) chunks   
    
    let unregister_uploader t chunks bs =
      List.iter (fun b ->
          b.block_nuploaders <- b.block_nuploaders - 1;
      ) bs;
      let rec iter_before chunks r =
        match chunks with 
          [] -> ()
        | (chunk_begin, chunk_end) :: tail ->
            if r.range_end <= chunk_begin then
              next_range chunks r
            else
            if r.range_begin >= chunk_end then
              iter_before tail r
            else    
            if r.range_begin >= chunk_begin && r.range_end <= chunk_end then
              begin
                r.range_nuploaders <- r.range_nuploaders - 1;
                next_range chunks r
              end
            else
            if r.range_end >= chunk_end then
              iter_before tail r
            else
              next_range chunks r
      
      and next_range chunks r =        
        match r.range_next with
        | Some rr ->  iter_before chunks rr        
        | None -> 
            match chunks with
              [] -> ()
            | [_, chunk_end] -> 
                assert (r.range_end >= chunk_end);
            | _ -> assert false            
      in
      iter_before chunks t.t_ranges
    
    let get_block bs =
      
      let rec iter_blocks bs max_downloaders offset b_min =
        match bs with
          [] -> raise Not_found
        | b :: tail ->
            if b.block_num >= b_min then
              let r = b.block_ranges in
              iter_ranges r b tail true max_downloaders offset
            else
              iter_blocks tail max_downloaders offset b_min          
      
      and iter_ranges r b tail downloaded max_downloaders offset =
        if r.range_begin < b.block_end then
          if r.range_ndownloaders <= max_downloaders then
            let downloaded = downloaded &&
              (r.range_current_begin >= r.range_end) in
            match r.range_next with
              None -> assert (r.range_end = b.block_end && tail = []);
                if downloaded then raise Not_found else b
            | Some rr ->
                iter_ranges rr b tail downloaded max_downloaders offset
          
          else
            iter_blocks tail max_downloaders offset (b.block_num+offset)
        else
        if downloaded then iter_blocks tail max_downloaders offset 0 else b
      in
      match bs with 
        [] -> raise Not_found
      | b :: _ -> 
          let p = b.block_partition in
          let rec iter_offset bs max_downloaders offset =
            try
              iter_blocks bs max_downloaders offset 0
            with Not_found ->
                if offset = 0 then raise Not_found;
                iter_offset bs max_downloaders (offset/2)
          in
          try
            iter_offset bs 0 (p.part_nblocks/2)
          with Not_found -> 
              try
                iter_offset bs 1 (p.part_nblocks/2)
              with Not_found -> 
                  iter_offset bs 100000 (p.part_nblocks/2)
    
    let find_range b chunks ranges max_range_size =
      
      let rec iter_before chunks r max_downloaders =
        match chunks with 
          [] -> raise Not_found
        | (chunk_begin, chunk_end) :: tail ->
            if r.range_end <= chunk_begin then
              next_range chunks r max_downloaders
            else
            if r.range_begin >= chunk_end then
              iter_before tail r max_downloaders
            else    
            if r.range_begin >= chunk_begin && r.range_end <= chunk_end then
              begin
                if !verbose_swarming then
                  lprintf "possible range: %s-%s(%d)\n"
                    (Integer.to_string r.range_begin)
                  (Integer.to_string r.range_end)
                  r.range_ndownloaders;
                if r.range_current_begin < r.range_end &&
                  not (List.memq r ranges) && 
                  r.range_ndownloaders <= max_downloaders then begin
                    
                    if r.range_end -- r.range_current_begin > 
                      max_range_size ++ !min_range_size  && 
                      r.range_ndownloaders = 0 
                    then
                      ignore (split_range r 
                          (r.range_current_begin ++ max_range_size));
                    r
                  end
                else
                  next_range chunks r max_downloaders
              end
            else
            if r.range_end >= chunk_end then
              iter_before tail r max_downloaders
            else
              next_range chunks r max_downloaders
      
      and next_range chunks r max_downloaders =        
        match r.range_next with
        | Some rr ->  
            if rr.range_begin < b.block_end then
              iter_before chunks rr max_downloaders
            else raise Not_found
        | None -> 
            match chunks with
              [] -> raise Not_found
            | [_, chunk_end] -> 
                assert (r.range_end >= chunk_end);
                raise Not_found
            | _ -> assert false
      
      in
      let p = b.block_partition in
      let bitmap = p.part_bitmap in
      if bitmap.[b.block_num] >= '2' then raise Not_found;
      try
        iter_before chunks b.block_ranges 0
      with Not_found ->
          try
            iter_before chunks b.block_ranges max_int
          with Not_found ->
              compute_bitmap p;
              raise Not_found
    
    let find_range_bitmap b ranges max_range_size =
      
      let rec iter_before r max_downloaders =
        if !verbose_swarming then
          lprintf "possible range: %s-%s(%d)\n"
            (Integer.to_string r.range_begin)
          (Integer.to_string r.range_end)
          r.range_ndownloaders;
        if r.range_current_begin < r.range_end &&
          not (List.memq r ranges) && 
          r.range_ndownloaders <= max_downloaders then begin
            
            if r.range_end -- r.range_current_begin > 
              max_range_size ++ !min_range_size  && 
              r.range_ndownloaders = 0 
            then
              ignore (split_range r 
                  (r.range_current_begin ++ max_range_size));
            r
          end
        else
          next_range r max_downloaders
      
      and next_range r max_downloaders =        
        match r.range_next with
        | Some rr ->  
            if rr.range_begin < b.block_end then
              iter_before rr max_downloaders
            else raise Not_found
        | None -> raise Not_found
      
      in
      let p = b.block_partition in
      let bitmap = p.part_bitmap in
      if bitmap.[b.block_num] >= '2' then raise Not_found;
      try
        iter_before b.block_ranges 0
      with Not_found ->
          try
            iter_before b.block_ranges max_int
          with Not_found ->
              compute_bitmap p;
              raise Not_found
    
    let alloc_range r =
      r.range_ndownloaders <- r.range_ndownloaders + 1  
    
    let free_range r =
      r.range_ndownloaders <- r.range_ndownloaders - 1
    
    let range_range r = (r.range_current_begin, r.range_end)


(* Before doing a receive, a client should do a free_range on all
his ranges, so that these ranges can be split if the download didn't 
start at the beginning of the range. *)
    let received (t : t) (file_begin : Integer.t)
      (s:string) (string_begin:int) (string_len:int) =
      if string_len > 0 then
        let file_end = file_begin ++ (Integer.of_int string_len) in
        
        if !verbose_swarming then
          lprintf "received on %s-%s\n" 
            (Integer.to_string file_begin)
          (Integer.to_string file_end);
        
        let rec iter_ranges r =
          if !verbose_swarming then
            lprintf "testing range %s-%s (%s-%s)\n" 
              (Integer.to_string r.range_begin)
            (Integer.to_string r.range_end)
            (Integer.to_string file_begin)
            (Integer.to_string file_end);
          if r.range_begin < file_end then
            if r.range_end <= file_begin then
              match r.range_next with
                None -> assert false
              | Some rr -> iter_ranges rr
            else
            if r.range_current_begin < file_end then
              if r.range_current_begin >= file_begin then
                let new_range_current_begin = 
                  if r.range_end < file_end then
                    r.range_end
                  else file_end 
                in
                if !verbose_swarming then
                  lprintf "received at begin of range\n";
                let writen_len = new_range_current_begin -- 
                    r.range_current_begin in
                t.t_write r.range_current_begin s
                  (string_begin + 
                    Integer.to_int (r.range_current_begin -- file_begin))
                (Integer.to_int writen_len);
                r.range_current_begin <- new_range_current_begin;
                t.t_downloaded <- t.t_downloaded ++ writen_len;
                match r.range_next with
                  None -> assert (r.range_end >= file_end)
                | Some rr -> iter_ranges rr
              else begin
                  if !verbose_swarming then
                    lprintf "Received at end of range\n";
                  if r.range_ndownloaders = 0 then begin
                      let new_range_begin =
                        if file_begin -- r.range_begin > !min_range_size then
                          file_begin else
                          r.range_begin ++ !min_range_size
                      in
                      if r.range_end -- new_range_begin > !min_range_size &&
                        file_end > new_range_begin then
                        ignore (split_range r new_range_begin)
                      else 
                      if !verbose_swarming then
                        lprintf "Cannot split range\n";
                    end;
                  match r.range_next with
                    None -> assert (r.range_end >= file_end)
                  | Some rr -> iter_ranges rr
                end
        in
        iter_ranges t.t_ranges
    
    let present_chunks t =
      let rec iter_out r list =
        if r.range_current_begin > r.range_begin then
          if r.range_current_begin < r.range_end then
            let list = (r.range_begin, r.range_current_begin) :: list in
            match r.range_next with
              None -> List.rev list
            | Some rr -> iter_out rr list
          else
          match r.range_next with
            None -> List.rev ((r.range_begin, r.range_end) :: list)
          | Some rr ->
              iter_in rr r.range_begin list
        else
        match r.range_next with
          None -> List.rev list
        | Some rr -> iter_out rr list
      
      and iter_in r range_begin list =
        if r.range_current_begin < r.range_end then
          match r.range_next with
            None -> List.rev ((range_begin, r.range_current_begin) :: list)
          | Some rr ->
              iter_out rr  ((range_begin, r.range_current_begin) :: list)
        else
        match r.range_next with
          None -> List.rev ((range_begin, r.range_end) :: list)
        | Some rr ->
            iter_in rr range_begin list
      in
      iter_out t.t_ranges []
    
    
    let set_verified_bitmap p bitmap =
      let rec iter_blocks b =
        if p.part_bitmap.[b.block_num] = '2' then
          verify_block b;
        if p.part_bitmap.[b.block_num] >= '2' then
          iter_ranges b b.block_ranges
        else
          next_block b
      
      and iter_ranges b r =
        if r.range_begin >= b.block_end then 
          next_block b
        else begin
            let t = b.block_partition.part_t in
            t.t_downloaded <- t.t_downloaded ++ (
              r.range_end -- r.range_current_begin);
            r.range_current_begin <- r.range_end;
            next_range b r
          end
      
      and next_range b r =
        match r.range_next with
          None -> ()
        | Some rr -> iter_ranges b rr 
      
      and next_block b =
        match b.block_next with
          None -> ()
        | Some bb -> iter_blocks bb
      in
      p.part_bitmap <- bitmap;
      iter_blocks p.part_blocks;
      compute_bitmap p
    
    let verified_bitmap p = p.part_bitmap
    let set_verifier p f = 
      p.part_verifier <- f;
      set_verified_bitmap p p.part_bitmap
    
    let downloaded t = t.t_downloaded
    let block_block b = b.block_num, b.block_begin, b.block_end
    let partition_size p = p.part_nblocks
    
    let availability p =
      let rec iter_blocks b s =
        s.[b.block_num] <- char_of_int (
          if b.block_nuploaders > 200 then 200 else b.block_nuploaders);
        match b.block_next with
          None -> s
        | Some bb -> iter_blocks bb s
      in
      iter_blocks p.part_blocks (String.make p.part_nblocks '\000')
    
    let rec debug_block_ranges buf b r =
      Printf.bprintf buf "(%s-%s%s) "
        (Integer.to_string r.range_begin)
      (Integer.to_string r.range_end)
      (if r.range_current_begin = r.range_end then "[DONE]" else      
        if r.range_current_begin = r.range_begin then "" else
          Printf.sprintf "[%s]" (Integer.to_string 
              (r.range_end -- r.range_current_begin)));
      match r.range_next with
        None -> ()
      | Some rr -> 
          if rr.range_begin < b.block_end then
            debug_block_ranges buf b rr
    
    let rec debug_blocks buf b = 
      Printf.bprintf buf "      Block %d: %s-%s %s\n"
        b.block_num (Integer.to_string b.block_begin)
      (Integer.to_string b.block_end)
      (match b.block_partition.part_bitmap.[b.block_num] with
          '2' -> "[PRESENT]"
        | '3' -> "[VERIFIED]"
        | _ -> "");
      Printf.bprintf buf "      Ranges: ";
      debug_block_ranges buf b b.block_ranges;
      match b.block_next with
        None -> ()
      | Some b -> debug_blocks buf b
      
    let rec debug_partitions buf ps =
      match ps with
        [] -> ()
      | p :: tail -> 
          Printf.bprintf buf "  Partition\n";
          Printf.bprintf buf "  BEGIN\n";
          Printf.bprintf buf "    Blocks: %d\n" p.part_nblocks;
          Printf.bprintf buf "    Bitmap: %s\n" p.part_bitmap;
          Printf.bprintf buf "    Blocks:\n";
          debug_blocks buf p.part_blocks;
          Printf.bprintf buf "  END\n";
          debug_partitions buf tail

    let rec debug_ranges buf r =
      Printf.bprintf buf "(%s-%s%s) "
        (Integer.to_string r.range_begin)
      (Integer.to_string r.range_end)
      (if r.range_current_begin = r.range_end then "[DONE]" else      
        if r.range_current_begin = r.range_begin then "" else
          Printf.sprintf "[%s]" (Integer.to_string 
              (r.range_end -- r.range_current_begin)));
      match r.range_next with
        None -> ()
      | Some rr -> debug_ranges buf rr

    let debug_print t =
      let buf = Buffer.create 1000 in
      Printf.bprintf buf "Swarmer:\n";
      Printf.bprintf buf "BEGIN\n";
      Printf.bprintf buf "  File downloaded/size: %s/%s\n" 
        (Integer.to_string t.t_downloaded)
        (Integer.to_string t.t_size);
      Printf.bprintf buf "  Ranges: ";     
      debug_ranges buf t.t_ranges;    
      Printf.bprintf buf "  \n";
      debug_partitions buf t.t_partitions;
      Printf.bprintf buf "END\n";      
      Buffer.contents buf      
      
  end
  
module Int = struct
    type t = int
    let add = (+)
    let sub = (-)
    let zero = 0
    let of_int x = x
    let to_int x = x
    let to_string = string_of_int
  end
  
module M = Make(Int)
  
let _ =
  if !verbose_swarming then

  let t = M.create (fun offset s pos len ->
        lprintf "Would write at %d: %d[%d]\n"
          offset pos len
        ) in
  let size = 9998656 in
  M.set_size t size;
  M.print_t "size" t;
  let present = [ (0,100000); (6770976, 6771000) ] in
  let present = M.sort_chunks present in
  M.set_present t present;
  M.print_t "set_present" t;
  let p = M.partition t  (fun x -> 
        let next = x + 2 * 1024 * 1024 in
        lprintf "block: %d --> %d\n" x next;
        next
        )  in
  M.print_t "partition" t;
  
  let chunks = [ (4200000, 6000000) ] in
  let chunks = M.sort_chunks chunks in
  let blocks = M.register_uploader p chunks in
  M.print_t "register_uploader" t;
  List.iter M.print_block blocks;
  
  let b = M.get_block blocks in
  M.print_block b;
  M.print_t "get_block" t;
  
  let r = M.find_range b chunks [] 200000 in
  M.print_t "find_range" t;
  let (x,y) = M.range_range r in
  lprintf "Range: %d-%d\n" x y;
  M.alloc_range r;
  
  let r1 = M.find_range b chunks [] 200000 in
  M.print_t "find_range" t;
  let (x1,y1) = M.range_range r1 in
  lprintf "Range: %d-%d\n" x y;
  
  M.free_range r;
  M.received t (x+1000) "" 0 100000;
  M.received t (x+110000) "" 0 100000;
  M.received t (x+210000) "" 0 100000;
  M.received t (x+310000) "" 0 100000;
  M.print_t "received" t
  
  
module Int64Swarmer = Make(Int64)
  
let fixed_partition t n = 
  Int64Swarmer.partition t 
    (fun x -> 
      Int64.add x n)
  
