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
  The jobs of swarmers are :
  * select what data to ask from each uploader
  * merge data coming from uploaders, potentially from different
  networks, into a single Unix32 backend.

                            OVERALL SCHEMA

Each network frontend can have a different (fixed) chunk size
t1 +--------+--------+--------+--------+--------+--------+-------- chunks
t2 +------+------+------+------+------+------+------+------+------ chunks

 each block is contained in at most /\ chunk_of_block
         one chunk, for any network ||                            mappings 
                                    \/ blocks_of_chunk
swarmer
   +------+-+----+---+--+------+------++-----+--+---+----+-+------ blocks
    |      | |    |   |  |      |    ...                      variable size
    v      v v    v   v  v      v
    r<>r   r r    r   r  r<>r   r<>r                               ranges
                            ^                      one dbl linked list/block
                            |                    encoding missing data ranges
uploaders physically     uploader
reference ranges
*)

open Int64ops
open Options
open Printf2
  
open CommonOptions

  
let check_swarming = false
let debug_all = false

open CommonTypes

type strategy =
  LinearStrategy    (* one after the other one *)
| AdvancedStrategy  (* first chunks first, rarest chunks second, 
     complete first third, and random final *)

exception VerifierNotReady

type intervals =
  AvailableIntervals of (int64 * int64) list
| AvailableBitv of Bitv.t

type verification =
  NoVerification
| VerificationNotAvailable
| ForceVerification
| Verification of uid_type array
  
(* let debug_all = true *)
let exit_on_error = ref false

let log_prefix = "[cSw]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

open CommonTypes

open CommonFile
open CommonTypes
open CommonClient


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

(* only used in code (currently disabled) for finding duplicate chunks *)

type chunk = {
    chunk_uid : uid_type;
    chunk_size : int64;
  }

(* network "frontend"/"view"/... to a swarmer *)
(* glossary:
   network frontend use "chunks" of data,
   swarmer use "blocks" of data *)
(* frontends are compared using physical equality (==) *)
type t = {
    mutable t_primary : bool;
    t_file : file;
    mutable t_s : swarmer;
    t_chunk_size : int64;

    t_nchunks : int;
    mutable t_converted_verified_bitmap : string;
    mutable t_last_seen : int array;
    mutable t_ncomplete_chunks : int;
    mutable t_nverified_chunks : int;

    mutable t_verifier : verification; (* information available to
					  check data correctness *)
    mutable t_verified : (int -> int -> unit); (* function to call
						  when a chunk is verified;
						  receives the number
						  of verified chunks,
						  and the index of the
						  chunk just verified *)

(* mapping from network chunks to swarmer blocks *)
    mutable t_blocks_of_chunk : int list array;
(* mapping from swarmer blocks to network chunks *)
    mutable t_chunk_of_block : int array;
  }

and swarmer = {
    s_num : int;
    s_filename : string;
    s_size : int64;

    mutable s_networks : t list; (** list of frontends, primary at head 
				     t.t_s = s <=> t in s.s_networks *)
    mutable s_strategy : strategy;

    mutable s_verified_bitmap : string;
    mutable s_availability : int array;
    mutable s_nuploading : int array;
(*    mutable s_last_seen : int array; *)

    mutable s_blocks : block_v array;
    mutable s_block_pos : int64 array; (** offset of the beginning of
					   each block *)
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
    mutable block_ranges : range; (** [range] of the double-linked
				      list of ranges associated to the
				      [block] 
				      what about using a standard list
				      instead ? 
				      or a balanced tree ? *)
    mutable block_remaining : int64; (* amount of bytes missing. *)
  }

and range = {
    mutable range_block : block;
    mutable range_begin : Int64.t;
    mutable range_end : Int64.t;
    mutable range_prev : range option;
    mutable range_next : range option;
    mutable range_nuploading : int; (* number of uploaders currently
				       referencing that range *)
  }

and uploader = {
    up_t : t;
    up_client : client;

    mutable up_declared : bool;

    mutable up_intervals : intervals;
    mutable up_complete_blocks : int array; (** block numbers *)
    mutable up_ncomplete : int; (** number of blocks not yet handled,
				    at the beginning of
				    up_complete_blocks *)

    mutable up_partial_blocks : (int * int64 * int64) array; (** block
								 number,
								 begin_pos,
								 end_pos
								 *)
    mutable up_npartial : int; (** number of blocks not yet handled,
				   at the beginning of
				   up_partial_blocks *)

    mutable up_block : block option;
    mutable up_block_begin : int64;
    mutable up_block_end : int64;

    mutable up_ranges : (int64 * int64 * range) list; (* ranges referenced by
							 that uploader, see
							 range_nuploading *)
  }

(* range invariants: 
   Ranges represent "holes" of missing data in a block; Data is
   missing between offsets range_begin and range_end.

   [block]'s [block_ranges] reference the first (smallest offsets) of
   the [range]s associated with it. 

   [range]s are double-linked together thru [range_prev] and
   [range_next]:

   r.range_next.range_prev = r.range_prev.range_next = r
   ( when links are different from None )

   [range]s have a backlink to their "owner" [block]:

   b.block_ranges.{range_next}*.{range_prev}*.range_block = b

   ranges offsets are all within their block's offsets limits, do not
   overlap, and are sorted in increasing offsets order:

   b.block_begin <= b.block_ranges.block_begin ... <=
   r.range_prev.range_end <= r.range_begin <=
   r.range_end <= r.range_next.range_begin <= ...
   <= b.block_end *)
   
(* range owners are only used thru uploaders.up_ranges. blocks could be
   saved in [uploaders]' [up_ranges] along range, but would
   need uploading when the swarmer is splitted.

   Removing [range] from [up_ranges] and [range_nuploading] from
   [range] could be good too, because they're not correctly updated
   when the swarmer is splitted. Again, getting rid of them is a
   problem of performance.
*)

(* block invariants
   Data missing for a block is the sum of the "sizes" of its ranges.

   b.block_remaining = sum (r.range_end - r.range_begin) b.block_ranges
*)

(* swarmer invariants ?
   s.s_verified_bitmap.[i] = 0 <=> s_blocks.[i] = EmptyBlock
   s.s_verified_bitmap.[i] = 1 <=> s_blocks.[i] = PartialBlock _
   s.s_verified_bitmap.[i] = 2 <=> s_blocks.[i] = CompletedBlock
   s.s_verified_bitmap.[i] = 3 <=> s_blocks.[i] = VerifiedBlock
   If so, why not drop s_verified_bitmap, and replace it by some
   verified_bitmap s i and verified_bitmap_all s functions ?
*)

(* frontend invariants ?
   t_ncomplete_chunks = 
   List.length (List.filter (fun x -> x >= '2') t_converted_verified_bitmap)
   t_nverified_chunks = 
   List.length (List.filter (fun x -> x = '3') t_converted_verified_bitmap)

   hence t_ncomplete_chunks >= t_nverified_chunks

   All chunks are [t_chunk_size] bytes in size, and first start at
   offset 0. This is assumed in [create], [associate], [verify_chunk],
   [duplicate_chunks], maybe more.
*)

(* uploaders invariants ?
   uploader block numbers are stored in reverse order in
   up_complete_blocks and up_partial_blocks (first blocks at the end
   of arrays), then array is processed from end to begin.

   0 <= up_ncomplete < Array.length up_complete_blocks 
   0 <= up.up_npartial < Array.length up_partial_blocks

   When a block has been selected, it's pushed out of the first
   up_ncomplete first elements of up_complete_blocks by swapping it
   with the #(up_ncomplete-1) element, then up_ncomplete is
   decreased. (and similarly with s/complete/partial/ ?)

   The question is now, aren't there better datastructures than
   arrays for the job ? ;)
*)

(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)

module HS = Weak.Make(struct
      type t = swarmer
      let hash file = Hashtbl.hash file.s_filename

      let equal x y  = x.s_filename = y.s_filename
    end)

let swarmers_by_name = HS.create 31

module HU = Weak.Make(struct
      type t = uploader
      let hash u = Hashtbl.hash (client_num u.up_client)

      let equal x y  =  (client_num x.up_client) =  (client_num y.up_client)
    end)

let uploaders_by_num = HU.create 113

let edonkey_range_size = Int64.of_int (180 * 1024)

let swarmer_counter = ref 0

let string_init n f =
  let s = String.create n in
  let rec aux i =
    if i < n then begin
      s.[i] <- f i;
      aux (i+1)
    end in 
  aux 0;
  s

let string_iter f s =
  let l = String.length s in
  let rec aux i =
    if i < l then begin
      f i s.[i];
      aux (i+1)
    end in 
  aux 0

let string_existsi p s =
  let l = String.length s in
  let rec aux i =
    i < l && (p i s.[i] || aux (i+1)) in
  aux 0

let string_for_all p s =
  let l = String.length s in
  let rec aux i =
    i >= l || p s.[i] && aux (i+1) in
  aux 0

(** sets [t.t_last_seen] of the verified blocks to current time, and 
    associated file's [t.t_s.s_file] last seen value to the oldest of the
    remaining last seen values *)

let compute_last_seen t =
  let last_seen_total = ref (BasicSocket.last_time ()) in
  string_iter (fun i c -> 
    if c > '2' then
      t.t_last_seen.(i) <- BasicSocket.last_time ()
    else
      last_seen_total := min !last_seen_total t.t_last_seen.(i)
  ) t.t_converted_verified_bitmap;
  set_file_last_seen t.t_file !last_seen_total;
  t.t_last_seen

(** (internal) return a 0 sized range at offset [pos], and assigned to
    block [b] *)

let void_range b pos =
  let r = {
      range_prev = None;
      range_next = None;
      range_begin = pos;
      range_end = pos;
      range_block = b;
      range_nuploading = 0;
    }
  in
  r

let rec ranges_iter f r =
  f r;
  match r.range_next with
  | None -> ()
  | Some r ->
      ranges_iter f r

let rec ranges_fold f acc r =
  let acc = f acc r in
  match r.range_next with
  | None -> acc
  | Some rr -> ranges_fold f acc rr

let rec ranges_for_all p r =
  p r && 
    (match r.range_next with
     | None -> true
     | Some r -> ranges_for_all p r)

let block_ranges_for_all p b =
  ranges_for_all p b.block_ranges

let block_ranges_fold f acc b =
  ranges_fold f acc b.block_ranges

(** (internal) assigns range [r], and all other ranges along
    [range_next] links, to block [b] *)

let rec own_ranges b r =
  ranges_iter (fun r -> r.range_block <- b) r

(** (internal) 
    Find ranges that are after [cut_pos] offset, unlink them from r
    double-linked list of ranges, set their owner to [b] and return
    the first of the removed ranges.

    If all ranges are before [cut_pos] return a 0-sized range.

    If [cut_pos] is within one of the ranges, that range is cut in
    two at [cut_pos] offset, and link each half to its side.

    Also, what do to if range_nuploading is not 0 ?
    => [cut_ranges_after] is being called from [split_blocks] that
    does not preserve [s_nuploading] for blocks either
*)

let cut_ranges_after b r cut_pos =
  let rec iter r =
    if r.range_begin >= cut_pos then begin
      (match r.range_prev with
        | None -> ()
        | Some rp ->
            rp.range_next <- None;
            r.range_prev <- None);
      r
    end 
    else if r.range_end <= cut_pos then
      match r.range_next with
	| None -> void_range b cut_pos
	| Some r -> iter r
    else
      (* "right" half *)
      let split_r = { r with
	range_prev = None;
	range_begin = max r.range_begin cut_pos
      } in
      (* "left" half *)
      r.range_next <- None;
      r.range_end <- cut_pos;
      r.range_begin <- min r.range_begin cut_pos;

      if r.range_nuploading <> 0 then
	lprintf_n "WARNING: Splitting a range currently being uploaded, don't know what to do with range_nuploading :/\n";

      split_r in
  let cut_ranges = iter r in
  own_ranges b cut_ranges;
  cut_ranges
  
(** (internal) return the offset of the end of the [i]th block of
    swarmer [s] *)

let compute_block_end s i =
  let b = s.s_block_pos in
  if Array.length b = i + 1 then
    s.s_size
  else
    b.(i+1)

(** (internal) return the offset of the beginning of the [i]th block
    of swarmer [s] *)

let compute_block_begin s i =
  let b = s.s_block_pos in
  b.(i)

(** Finds the number of the block containing [chunk_pos] offset, using
    dichotomy. Blocks are half opened [block_begin, block_end[ *)

(* 0 <= chunk_pos < s.s_size *)
let compute_block_num s chunk_pos =
  assert (0L <= chunk_pos && chunk_pos < s.s_size);
  let b = s.s_block_pos in
(* invariants:
   0 <= min <= max <= Array.length b - 1
   compute_block_begin s min <= chunk_pos < compute_block_end s max *)

  let rec iter min max =
    if min = max then min
    else (* from now on, min < max *)
      let medium = (min + max) / 2 in
      (* Euclide        => 2*medium <= min + max <= 2*medium + 1 *)
      (* min < max      => 2*min < min + max < 2*max
                        => min <= medium < max                   *)
      if min < medium then
        if chunk_pos < b.(medium) then
          iter min (medium - 1)
        else
          iter medium max
      else
        (* min = medium < max  => 2*min < min + max <= 2*min + 1
                              <=>   min <       max <=   min + 1
                              <=> min + 1 = max                  *)
        if chunk_pos < b.(max) then
          min else max
  in
  let i = iter 0 (Array.length b - 1) in
  if debug_all then
    lprintf_nl "%Ld is block %d [%Ld-%Ld]" chunk_pos i
      (compute_block_begin s i) (compute_block_end s i);
  i

(** Return true if ranges fully "cover" their block
    ("the block is made of holes") *)

let empty_block b =
  let rec iter begin_pos r =
    r.range_begin = begin_pos &&
    (match r.range_next with
     | Some rr -> iter r.range_end rr
     | None -> r.range_end = b.block_end)
  in
  iter b.block_begin b.block_ranges

let iter_intervals s f intervals =
  let nchunks = Array.length s.s_blocks in
  List.iter (fun (interval_begin, interval_end) ->
    let interval_begin = min interval_begin s.s_size in
    let interval_end = min interval_end s.s_size in
(*  lprintf "apply on %Ld-%Ld\n" interval_begin interval_end; *)
    if interval_begin < interval_end then
      let i0 = compute_block_num s interval_begin in
      let block_begin = compute_block_begin s i0 in
      let rec iter_blocks i block_begin interval_begin =
(*      lprintf "iter_blocks %d %Ld %Ld\n" i block_begin interval_begin; *)
        if i < nchunks && block_begin < interval_end then
          let block_end = compute_block_end s i in
          let current_end =  min block_end interval_end in
	  
          if debug_all then
            lprintf_nl "Apply: %d %Ld-%Ld %Ld-%Ld"
              i block_begin block_end interval_begin current_end;
	  
          f i block_begin block_end interval_begin current_end;
          iter_blocks (i+1) block_end block_end
      in
      iter_blocks i0 block_begin interval_begin
  ) intervals

(*************************************************************************)
(*                                                                       *)
(*                              Swarmers                                 *)
(*                                                                       *)
(*************************************************************************)

let dummy_swarmer = {
    s_num = 0;
    s_filename = "";
    s_size = zero;
    s_networks = [];
    s_strategy = AdvancedStrategy;
    s_verified_bitmap = "";
    s_blocks = [||];
    s_block_pos = [||];
    s_availability = [||];
    s_nuploading = [||];
  }

(** if a swarmer is already associated with that [file_name], return it;
    Otherwise create a new one with default values, that will be fixed
    by the first frontend association *)

let create_swarmer file_name file_size =
  try
    HS.find swarmers_by_name
      { dummy_swarmer with
	s_filename = file_name
      }
  with Not_found ->
    incr swarmer_counter;

    let nblocks = 1 in
    let rec s = {

      s_num = !swarmer_counter;
      s_filename = file_name;
      s_size = file_size;

      s_networks = [];

      s_strategy = AdvancedStrategy;

      s_verified_bitmap = String.make nblocks '0';
      s_blocks = Array.create nblocks EmptyBlock ;
      s_block_pos = Array.create nblocks zero;
      s_availability = Array.create nblocks 0;
      s_nuploading = Array.create nblocks 0;
(*      s_last_seen = Array.create nblocks 0; *)
    }
    in
    HS.add swarmers_by_name s;
    s

(** Split swarmer existing blocks in at [chunk_size] boundaries *)
let split_blocks s chunk_size =

  let size = s.s_size in

  let nblocks = Array.length s.s_blocks in
  (* Split existing blocks at [chunk_size] boundaries
     invariants:
     [index_s] is the index of the existing block being analysed
     [chunk_begin] is the offset of the beginning of the current containing chunk
     [new_blocks] is the list of new blocks already splitted, in
     reverse order. 
     List contains tuples: block, beginning offset, verified status char *)
  let rec iter index_s chunk_begin new_blocks =
(*    lprintf "iter (1) %d/%d %Ld\n" index_s nblocks chunk_begin; *)
    if index_s = nblocks then List.rev new_blocks else

    (* existing block *)
    let block_begin = compute_block_begin s index_s in
    let block_end = compute_block_end s index_s in

    (* current chunk *)
    let chunk_end = chunk_begin ++ chunk_size in
    let chunk_end = min chunk_end size in

    if chunk_end > block_end then
        let new_blocks = (
            s.s_blocks.(index_s),
            block_begin,
            s.s_verified_bitmap.[index_s]
          ) :: new_blocks in
        iter (index_s+1) chunk_begin new_blocks

    else if chunk_end = block_end then
        let new_blocks =  (
            s.s_blocks.(index_s),
            block_begin,
            s.s_verified_bitmap.[index_s]
          ) :: new_blocks in
        iter (index_s+1) chunk_end new_blocks

    else begin
(* We need to split this block in two parts *)
        s.s_block_pos.(index_s) <- chunk_end;
        match s.s_blocks.(index_s) with
	| EmptyBlock | CompleteBlock | VerifiedBlock ->

(* s.s_blocks.(index_s) will appear twice in the result list *)
            let new_blocks =  (
                s.s_blocks.(index_s),
                block_begin,
                s.s_verified_bitmap.[index_s]
              ) :: new_blocks in
            iter index_s chunk_end new_blocks

        | PartialBlock b1 ->
	    (* split b1 in two; b2 is the part after [chunk_end] offset *)
            let b2 = {
                block_s = s;

                block_begin = chunk_end;
                block_end = b1.block_end;
                block_ranges = b1.block_ranges; (* fixed below *)
                block_num = 0; (* fixed below *)
                block_remaining = zero; (* unused ? *)
              } in
	    b2.block_ranges <- cut_ranges_after b2 b1.block_ranges chunk_end;
            b1.block_end <- chunk_end;

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

            if empty_block b2 then begin
(* lprintf "Partial block b2 should become EmptyBlock\n"; *)
                s.s_blocks.(index_s) <- EmptyBlock;
                s.s_verified_bitmap.[index_s] <- '0';
              end else
                s.s_blocks.(index_s) <- PartialBlock b2;
            iter index_s chunk_end new_blocks
      end
  in
  let blocks = iter 0 zero [] in

(* blocks have been splitted, now rebuild swarmer *)
  let nblocks = List.length blocks in
(*  lprintf "%d blocks to generate\n" nblocks; *)

  let array_exist p a =
    let l = Array.length a in
    let rec aux i = (i < l) && (p a.(i) || aux (i+1)) in
    aux 0 in

  if array_exist ((<>) 0) s.s_availability then
    lprintf_nl "WARNING: splitting swarmer discarded availability counters";
  if array_exist ((<>) 0) s.s_nuploading then
    lprintf_nl "WARNING: splitting a swarmer beging uploaded to";

  s.s_blocks <- Array.create nblocks EmptyBlock;
  s.s_verified_bitmap <- String.make nblocks '0';
  s.s_block_pos <- Array.create nblocks zero;
  s.s_availability <- Array.create nblocks 0; (* not preserved ? *)
  s.s_nuploading <- Array.create nblocks 0; (* not preserved ? *)
(*  s.s_last_seen <- Array.create nblocks 0; *)

  let rec iter i list =
    match list with
    | [] -> ()
    | (b, pos, c) :: tail ->
        begin
          match b with
	  | PartialBlock b -> b.block_num <- i
          | EmptyBlock | CompleteBlock | VerifiedBlock -> ()
        end;
        s.s_blocks.(i) <- b;
        s.s_verified_bitmap.[i] <- c;
        s.s_block_pos.(i) <- pos;

        iter (i+1) tail
  in
  iter 0 blocks

(** Associate a(n additional) frontend to a swarmer *)

let associate is_primary t s =
(* a swarmer cannot be associated more than once to a network *)
  if not (List.memq t s.s_networks) then
  let size = file_size t.t_file in

(* what about raising an exception instead ? *)
  if s.s_size <> size then begin
    lprintf_nl "file_size for %s does not match: swarmer %Ld / real %Ld" s.s_filename s.s_size size;
    exit 2
  end;

  t.t_s <- s;
  t.t_converted_verified_bitmap <- String.make t.t_nchunks '0';
  t.t_last_seen <- Array.create t.t_nchunks 0;
  t.t_chunk_of_block <- [||];
  t.t_blocks_of_chunk <- Array.create t.t_nchunks [];

(* invariant: primary frontend is at the head of swarmer's [s_networks] *)
  if is_primary then begin
    t.t_primary <- true;
    s.s_networks <- t :: s.s_networks;
  end else begin
    (* TODO: transfer data into swarmer instead of discarding it *)
    Unix32.remove (file_fd t.t_file);
    t.t_primary <- false;
    s.s_networks <- s.s_networks @ [t];
  end;

  (match s.s_networks with
   | t :: tail ->
       assert(t.t_primary);
       List.iter (fun tt -> assert(not tt.t_primary)) tail
   | [] -> assert false);

(* at this point, we are supposed to split the blocks in the swarmer
in smaller blocks depending on the block_size of this network, and compute
the t_chunk_of_block and t_blocks_of_chunk fields. *)

  let chunk_size = t.t_chunk_size in
  split_blocks s chunk_size;

  let nblocks = Array.length s.s_blocks in
  (* For all networks, adjust the chunks and mappings *)
  List.iter (fun t ->
      let nchunks = String.length t.t_converted_verified_bitmap in
      t.t_chunk_of_block <- Array.create nblocks 0;
      t.t_blocks_of_chunk <- Array.create nchunks [];

      let chunk_size = t.t_chunk_size in
      for i = 0 to nblocks - 1 do
        let block_begin = compute_block_begin s i in
        let chunk = Int64.to_int (block_begin // chunk_size) in
        t.t_chunk_of_block.(i) <- chunk;
        t.t_blocks_of_chunk.(chunk) <- i :: t.t_blocks_of_chunk.(chunk)
      done
    ) s.s_networks;

(* TODO: If not primary, set_file_downloaded should be called *)
  if not is_primary then
    add_file_downloaded t.t_file (zero -- file_downloaded t.t_file);

(* check that all frontends use the primary's file backend *)
  (match s.s_networks with
   | t :: tail when is_primary ->
       List.iter (fun tt ->
         set_file_fd tt.t_file (file_fd t.t_file)
       ) tail
   | tprim :: tail ->
       set_file_fd t.t_file (file_fd tprim.t_file)
   | [] -> assert false)

(** Create a primary frontend and its swarmer *)

let create ss file chunk_size =

  let size = file_size file in
  (* wrong if size is a multiple of chunk_size, or on purpose ? *)
  let nchunks =
    1 + Int64.to_int (Int64.pred size // chunk_size) in

  let t = {

      t_s = ss;
      t_primary = true;
      t_file = file;

      t_nchunks = nchunks;
      t_chunk_size = chunk_size;

      t_ncomplete_chunks = 0;
      t_nverified_chunks = 0;

      t_converted_verified_bitmap = String.make nchunks '0';
      t_last_seen = Array.create nchunks 0;

      t_verifier = NoVerification;
      t_verified = (fun _ _ -> ());

      t_chunk_of_block = [||];
      t_blocks_of_chunk = Array.create nchunks [];
    }
  in
  associate true t ss;
  t

(** iter function f over all the blocks contained in the list of [intervals]

    f with receive block number, block beginning and ending offsets,
    and overlapping interval beginning and ending offsets.

    If an interval starts halfway of a block, iteration starts on the
    next block, with interval_begin < block_begin indicating where the
    interval really started.

    If an interval ends halfway of a block, iteration ends on that
    block, with interval_end < block_end indicating where the interval
    really ended.
*)

let check_finished t =
  let file = t.t_file in
  match file_state file with
  | FileNew 
  | FileCancelled 
  | FileAborted _ 
  | FileShared 
  | FileDownloaded 
  | FileQueued 
  | FilePaused -> 
      false
  | FileDownloading ->
      if string_existsi (fun i c -> c <> '3')
	t.t_converted_verified_bitmap then false
      else begin
        if file_size file <> file_downloaded t.t_file then
          lprintf_nl "Downloaded size differs after complete verification";
        true
      end

(** (debug) output a [swarmer] to current log *)

let print_s str s =
  lprintf_nl "Ranges after %s:" str;

  let rec iter r =
    lprintf_n " %Ld-%Ld(%d)"
      r.range_begin r.range_end r.range_nuploading;
    match r.range_next with
    | None -> lprint_newline ()
    | Some r -> iter r in

  Array.iteri (fun i b ->
      lprintf_n "   %d: " i;
      let block_begin = compute_block_begin s i in
      let block_end = compute_block_end s i in
      lprintf "%Ld - %Ld [%Ld] %c " block_begin block_end
        (block_end -- block_begin) s.s_verified_bitmap.[i];
      List.iter (fun t ->
          let j = t.t_chunk_of_block.(i) in
          lprintf "(b %d %c [" j t.t_converted_verified_bitmap.[j];
          List.iter (fun ii -> lprintf "%d " ii) t.t_blocks_of_chunk.(j);
          lprintf "]";
      ) s.s_networks;

      match b with
      | PartialBlock b ->
          lprintf " [%Ld .. %Ld] --> "
            b.block_begin b.block_end;
          iter b.block_ranges
      | EmptyBlock -> lprintf_nl "_"
      | CompleteBlock -> lprintf_nl "C"
      | VerifiedBlock -> lprintf_nl "V"
  ) s.s_blocks;

  lprintf_nl "Files:";
  List.iter (fun t ->
    lprintf_nl "  File num: %d" (file_num t.t_file);
    lprintf_nl "  %s" (if t.t_primary then "primary" else "secondary");
    lprintf_nl "  Downloaded: %Ld" (file_downloaded t.t_file);
    lprintf_nl "  Bitmap: %s" t.t_converted_verified_bitmap
  ) s.s_networks

(** iter function f over all the ranges of a block *)

let iter_block_ranges f b =
  let rec iter_range f r =
    let next = r.range_next in (* keep next range in case f mutates it *)
    f r;
    match next with
    | None -> ()
    | Some rr -> iter_range f rr
  in
  iter_range f b.block_ranges

(** (debug) output a [block] to current log *)

let print_block b =
  lprintf_n "Block %d: %Ld-%Ld"
    b.block_num b.block_begin b.block_end;
  lprint_newline ();
  lprintf_nl "  ranges:";
  iter_block_ranges (fun r ->
    lprintf_nl "   %Ld-%Ld" r.range_begin r.range_end) b;
  lprint_newline ()

(** (shadows CommonFile.add_file_downloaded) 
    increments amount downloaded of the primary frontend of the swarmer,
    and of maybe_t, if provided, and if it's different from the primary. *)

let add_file_downloaded maybe_t s size =
(*  lprintf "add_file_downloaded %Ld\n" size; *)
  match s.s_networks with
  | t :: _ ->
      assert(t.t_primary);
      add_file_downloaded t.t_file size;
      (match maybe_t with
       | None -> ()
       | Some tt ->
	   if t != tt then
             add_file_downloaded tt.t_file size);
      if file_downloaded t.t_file < zero then
        lprintf_nl "ERROR: file_downloaded < zero!";
      
  | _ -> assert false

(** Close all the ranges of a block, adding their size to the
    downloaded amount *)

let close_block_ranges maybe_t s b =
  iter_block_ranges (fun r ->
    let added = r.range_end -- r.range_begin in
    add_file_downloaded maybe_t s added;
    b.block_remaining <- b.block_remaining -- added;
    r.range_begin <- r.range_end;
    r.range_prev <- None;
    r.range_next <- None) b;
  if b.block_remaining <> 0L then
    lprintf_nl "WARNING: block_remaining should be 0 after close_block_ranges"

(*************************************************************************)
(*                                                                       *)
(*                         swarmers verified bitmaps                     *)
(*                                                                       *)
(*************************************************************************)

(* For every swarmer, there is a "primary" verifier, and secondary
   verifiers.  When a block is downloaded, it is tagged '2' in the
   verified_bitmap, and this '2' is propagated to the primary bitmap if
   possible (if all sub-blocks are also '2'). If the primary chunk
   becomes '2', then a verification is needed on the primary. If the
   verification works, the verified_bitmap becomes '3', and the secondary
   verifiers are tagged with '2' (if they use a different verification
   scheme) or '3' (if no verification scheme or a verification scheme
   that has already been used). *)

(* corruption has been detected, and the block has been reset to 0 *)
let set_swarmer_bitmap_0 s i =
  (* shouldn't it be > '0' ? *)
  if s.s_verified_bitmap.[i] > '1' then begin
      s.s_verified_bitmap.[i] <- '0';
      List.iter (fun t ->
          let j = t.t_chunk_of_block.(i) in
	  match t.t_converted_verified_bitmap.[j] with
	  | '0' -> ()
	  | '1' ->
	      if List.for_all (fun i -> s.s_verified_bitmap.[i] = '0')
		t.t_blocks_of_chunk.(j) then
		  t.t_converted_verified_bitmap.[j] <- '0'
	  | '2' -> lprintf_nl "set_swarmer_bitmap_0: invalidating a block within a completed chunk?"
	  | '3' -> lprintf_nl "set_swarmer_bitmap_0: invalidating a block within a verified chunk?"
	  | _ -> assert false
      ) s.s_networks
    end

(* we have started downloading this block, so mark all containing blocks
  also as started. *)
let set_swarmer_bitmap_1 s i =
  match s.s_verified_bitmap.[i] with
  | '0' ->
      s.s_verified_bitmap.[i] <- '1';
      List.iter (fun t ->
        let j = t.t_chunk_of_block.(i) in
        match t.t_converted_verified_bitmap.[j] with
	| '0' -> t.t_converted_verified_bitmap.[j] <- '1'
	| '1' -> ()
	| '2' -> lprintf_nl "set_bitmap1: partial block within a completed chunk?"
	| '3' -> lprintf_nl "set_bitmap1: partial block within a verified chunk?"
	| _ -> assert false
      ) s.s_networks
  | '1' -> ()
  | '2' -> lprintf_nl "set_swarmer_bitmap_1: trying to demote a completed block?"
  | '3' -> lprintf_nl "set_swarmer_bitmap_1: trying to demote a verified block?"
  | _ -> assert false
	

(* we finished this block, trying to escalate to primary frontend
   verification bitmap *)
let set_swarmer_bitmap_2 s i =
  match s.s_verified_bitmap.[i] with
  | '0' | '1' ->
      (s.s_verified_bitmap.[i] <- '2';
      match s.s_networks with
      | t :: _ ->
	  assert (t.t_primary);
          let j = t.t_chunk_of_block.(i) in
	  (match t.t_converted_verified_bitmap.[j] with
	  | '0' | '1' -> 
	      if List.for_all (fun i -> s.s_verified_bitmap.[i] = '2')
		t.t_blocks_of_chunk.(j) then begin
		  t.t_ncomplete_chunks <- t.t_ncomplete_chunks + 1;
		  t.t_converted_verified_bitmap.[j] <- '2'
		end
	  | '2' -> ()
	  | '3' -> 
	      (* lprintf_nl "set_swarmer_bitmap_2: trying to demote a verified block? (1)" *)
	      ()
	  | _ -> assert false)
      | [] -> assert false)
  | '2' -> ()
  | '3' -> lprintf_nl "set_swarmer_bitmap_2: trying to demote a verified block? (2)"
  | _ -> assert false

(* the primary verifier has worked, so let ask secondary ones for
   verification too *)
let set_swarmer_bitmap_3 s i =
  match s.s_verified_bitmap.[i] with
  | '0' | '1' | '2' ->
      (s.s_verified_bitmap.[i] <- '3';
(*      lprintf "set_swarmer_bitmap_3 %d done\n" i; *)
      match s.s_networks with
      | [] -> assert false
      | tprim :: secondaries ->
	  assert (tprim.t_primary);
	  (* that test is somewhat redundant, since only primary
	     frontends with verification can have merged secondary
	     frontends; See merge *)
	  match tprim.t_verifier with
	  | NoVerification | VerificationNotAvailable -> ()
	  | Verification _ | ForceVerification ->
	      let jprim = tprim.t_chunk_of_block.(i) in
	      assert (tprim.t_converted_verified_bitmap.[jprim] = '3');
	      List.iter (fun t ->
		assert (not t.t_primary);
		let j = t.t_chunk_of_block.(i) in
		if List.for_all (fun i -> s.s_verified_bitmap.[i] = '3')
                  t.t_blocks_of_chunk.(j) then
		    match t.t_verifier with
		    | NoVerification | VerificationNotAvailable ->
			(* we have no way to check data integrity
			   for this network, assume other(s) know
			   better *)
			(match t.t_converted_verified_bitmap.[j] with
			 | '0' | '1' ->
			     t.t_converted_verified_bitmap.[j] <- '3';
			     t.t_ncomplete_chunks <- t.t_ncomplete_chunks + 1;
			     t.t_nverified_chunks <- t.t_nverified_chunks + 1
			 | '2' ->
			     t.t_converted_verified_bitmap.[j] <- '3';
			     t.t_nverified_chunks <- t.t_nverified_chunks + 1
			 | '3' -> ()
			 | _ -> assert false)
		    | ForceVerification
		    | Verification _ ->
			(* all chunks are verified, so set
			   converted_verified_bitmap to '2',
			   probably to trigger data verification later.
			   
			   Is that code necessary at all ? *)
			(match t.t_converted_verified_bitmap.[j] with
			 | '0' | '1' ->
			     t.t_converted_verified_bitmap.[j] <- '2';
			     t.t_ncomplete_chunks <- t.t_ncomplete_chunks + 1
			 | '2' -> ()
			 | '3' -> lprintf_nl "set_swarmer_bitmap_3: trying to demote a verified block in another frontend?"
			 | _ -> assert false)
	      ) secondaries)
  | '3' -> ()
  | _ -> assert false

(** set block as completed, closing all remaining ranges, and
    incrementing amount downloaded by their total size.
    If the block was empty its whole size is added *)

let set_completed_block maybe_t s i =
  let mark_completed () =
    set_swarmer_bitmap_2 s i;
    s.s_blocks.(i) <- CompleteBlock in
  match s.s_blocks.(i) with
  | CompleteBlock | VerifiedBlock -> ()
  | EmptyBlock ->
      let block_begin = compute_block_begin s i in
      let block_end = compute_block_end s i in
      add_file_downloaded maybe_t s (block_end -- block_begin);
      mark_completed ()
  | PartialBlock b ->
      close_block_ranges maybe_t s b;
      mark_completed ()

(** set block as verified, closing all remaining ranges, and
    incrementing amount downloaded by their total size.
    If the block was empty its whole size is added 

    (is it normal that no maybe_t can be provided ? my guess is that
    this function is always called on behalf of a primary frontend) *)

let set_verified_block s j =
  match s.s_blocks.(j) with
  | VerifiedBlock -> ()
  | _ ->
      set_completed_block None s j;
      s.s_blocks.(j) <- VerifiedBlock;
      set_swarmer_bitmap_3 s j

(*************************************************************************)
(*                                                                       *)
(*                        frontends verified bitmaps                     *)
(*                                                                       *)
(*************************************************************************)

(* We've seen how swarmer verification propagates to the frontend(s)
   verifications, now let's see the reverse *)
	
let set_frontend_bitmap_0 t j =
  assert(t.t_converted_verified_bitmap.[j] = '2');
  let s = t.t_s in
  assert(List.for_all (fun i -> s.s_verified_bitmap.[i] <> '3') t.t_blocks_of_chunk.(j));
  t.t_ncomplete_chunks <- t.t_ncomplete_chunks - 1;
  if List.for_all (fun i -> s.s_verified_bitmap.[i] = '2') t.t_blocks_of_chunk.(j) then begin
      if !verbose_swarming || !verbose then
        lprintf_nl "Complete block %d/%d of %s failed verification, reloading..."
          (j + 1) t.t_nchunks (file_best_name t.t_file);
      
      t.t_converted_verified_bitmap.[j] <- '0';
      List.iter (fun i ->
        match s.s_blocks.(i) with
	| EmptyBlock -> set_swarmer_bitmap_0 s i
        | PartialBlock _ ->  set_swarmer_bitmap_1 s i
        | CompleteBlock ->
            let block_begin = compute_block_begin s i in
            let block_end = compute_block_end s i in
	    (* negative *)
            add_file_downloaded None s (block_begin -- block_end);
	      
            s.s_blocks.(i) <- EmptyBlock;
            set_swarmer_bitmap_0 s i
	      
        | VerifiedBlock -> assert false
      ) t.t_blocks_of_chunk.(j)
    end
  else begin
    (* afaiu not supposed to happen, so this code is for debugging ? *)
    if !verbose_swarming then begin
      let nsub = ref 0 in
      lprintf_n "  Swarmer was incomplete: ";
      List.iter (fun i ->
        lprintf "%c" s.s_verified_bitmap.[i];
        if s.s_verified_bitmap.[i] = '2' then incr nsub;
      ) t.t_blocks_of_chunk.(j);
      lprintf_nl "   = %d/%d" !nsub (List.length t.t_blocks_of_chunk.(j))
    end;
    t.t_converted_verified_bitmap.[j] <- '1'
  end

(* aka set_completed_chunk (internal) *)
let set_frontend_bitmap_2 t j =
  match t.t_converted_verified_bitmap.[j] with
  | '0' | '1' ->
      if !verbose_swarming || !verbose then
	lprintf_nl "Completed block %d/%d of %s"
          (j + 1) t.t_nchunks (file_best_name t.t_file);
      let s = t.t_s in
      List.iter (fun i -> set_completed_block None s i)
	t.t_blocks_of_chunk.(j)
  | '2' | '3' -> ()
  | _ -> assert false

(* aka set_verified_chunk (internal) *)
let set_frontend_bitmap_3 t j =
  let mark_verified () =
    t.t_converted_verified_bitmap.[j] <- '3';
    if !verbose_swarming || !verbose then
      lprintf_nl "Verified block %d/%d of %s"
        (j + 1) t.t_nchunks (file_best_name t.t_file);
    if t.t_primary then begin
      let s = t.t_s in
      (* The primary is supposed to propagate verified chunks to the file *)
      List.iter (fun i -> set_verified_block s i) t.t_blocks_of_chunk.(j);
      if !verbose_swarming then
	print_s "VERIFIED" s
    end;
    t.t_verified t.t_nverified_chunks j in
  match t.t_converted_verified_bitmap.[j] with
  | '0' | '1' ->
      t.t_ncomplete_chunks <- t.t_ncomplete_chunks + 1;
      t.t_nverified_chunks <- t.t_nverified_chunks + 1;
      mark_verified ();
  | '2' ->
      t.t_nverified_chunks <- t.t_nverified_chunks + 1;
      mark_verified ();
  | '3' -> ()
  | _ -> assert false

let set_chunks_verified_bitmap t bitmap =
  string_iter (fun j c ->
    match c with
    | '0' | '1' -> 
	()
    | '2' ->
	set_frontend_bitmap_2 t j
    | '3' ->
	set_frontend_bitmap_3 t j;
        if t.t_converted_verified_bitmap.[j] <> '3' then
          lprintf_nl "FIELD AS BEEN CLEARED"
    | _ -> assert false
  ) bitmap

let chunks_verified_bitmap t = t.t_converted_verified_bitmap

(** Check the equality of the hash of [t]'s data between offsets
    [begin_pos] and [end_pos] against the value of [uid] *)

(*************************************************************************)
(*                                                                       *)
(*                         verify_chunk (internal)                       *)
(*                                                                       *)
(*************************************************************************)

let verify_chunk t j =
  let verify t uid begin_pos end_pos =
    file_verify t.t_file uid begin_pos end_pos in

  if t.t_converted_verified_bitmap.[j] = '2' then
    let nchunks = String.length t.t_converted_verified_bitmap in
    match t.t_verifier with
    | NoVerification
    | VerificationNotAvailable -> ()

    | ForceVerification ->
        set_frontend_bitmap_3 t j

    | Verification uids when Array.length uids = nchunks ->

        (try
          let s = t.t_s in
          let chunk_begin = t.t_chunk_size *.. j in
          let chunk_end = chunk_begin ++ t.t_chunk_size in
          let chunk_end = min chunk_end s.s_size in
          if verify t uids.(j) chunk_begin chunk_end then
            set_frontend_bitmap_3 t j
	  else
	    set_frontend_bitmap_0 t j
        with VerifierNotReady -> ())

    | Verification chunks ->
	(* network only provides a hash for the whole file ? *)
        assert (Array.length chunks = 1);
(*        let nchunks = String.length t.t_converted_verified_bitmap in *)

	if string_for_all (fun x -> x >= '2') t.t_converted_verified_bitmap then
          try
            let s = t.t_s in
            if verify t chunks.(0) zero s.s_size then
	      string_iter (fun j _ ->
		set_frontend_bitmap_3 t j
	      ) t.t_converted_verified_bitmap
	    else
	      string_iter (fun j c ->
		if c = '2' then set_frontend_bitmap_0 t j
	      ) t.t_converted_verified_bitmap
          with VerifierNotReady -> ()


(** mark a block as completed, ready for verification *)

let must_verify_block s i =
  set_swarmer_bitmap_2 s i

(** mark all blocks as completed, ready for verification *)

let verify_all_chunks t =
  let s = t.t_s in
  string_iter (fun i _ -> must_verify_block s i) s.s_verified_bitmap

(** same, and synchronously calls the verification of all chunks *)

let verify_all_chunks_immediately t =
  verify_all_chunks t;
  string_iter (fun i _ -> verify_chunk t i) t.t_converted_verified_bitmap
    

(** synchronously verify all completed chunks not yet verified *)

let compute_bitmap t =
  if t.t_ncomplete_chunks > t.t_nverified_chunks then 
    string_iter (fun i c -> 
      if c = '2' then verify_chunk t i) t.t_converted_verified_bitmap


(** Replaces the ith block of the swarmer with a PartialBlock
    ranges are created with s_range_size size *)

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
    }
  in
(*  lprintf "New block %Ld-%Ld\n" block_begin block_end; *)

  s.s_blocks.(i) <- PartialBlock b;
  if s.s_verified_bitmap.[i] < '1' then
    set_swarmer_bitmap_1 s i;
  if debug_all then lprintf_nl "NB[%s]" s.s_verified_bitmap;
  b

(** Remove an interval from the beginning of a range, adding the size
    of the removed part to the downloaded amount
    Closed ranges are removed
    When last range is removed, mark the block for verification *)

let range_received maybe_t r interval_begin interval_end =
(*  lprintf "   range_received: %Ld-%Ld for %Ld-%Ld\n"
    interval_begin interval_end r.range_begin r.range_end;  *)
  (* interval overlap with the beginning of range ? *)
  (* was: r.range_begin < interval_end && r.range_end > interval_begin *)
  if r.range_begin >= interval_begin && 
    r.range_begin < interval_end then begin
(*      lprintf "... entered\n"; *)
    let new_current_begin =
      max (min interval_end r.range_end) r.range_begin in
    let downloaded = new_current_begin -- r.range_begin in
    let b = r.range_block in
    let s = b.block_s in
    add_file_downloaded maybe_t s downloaded;
    b.block_remaining <- b.block_remaining -- downloaded;
    r.range_begin <- new_current_begin;
    if r.range_begin = r.range_end then begin
      (* range completed, unlink it *)
      (match r.range_next with
       | Some rr -> rr.range_prev <- r.range_prev
       | None -> ());
      (match r.range_prev with
       | Some rr -> rr.range_next <- r.range_next
       | None ->
	   (* that was the first range of the block *)
           match r.range_next with
           | Some rr -> (* fix block's first range *)
	       b.block_ranges <- rr
           | None ->
	       (* that was the last remaining range of the block *)
               (match s.s_blocks.(b.block_num) with
                | PartialBlock _ | EmptyBlock ->
                    (match s.s_networks with
		     | t :: _ -> 
			 assert(t.t_primary);
                         (match t.t_verifier with
			  | NoVerification ->
                              set_verified_block s b.block_num
			  | _ ->
                              set_completed_block (Some t) s b.block_num;
                              must_verify_block s b.block_num)
		     | [] -> assert false)
                | _ -> () ));
      r.range_next <- None;
      r.range_prev <- None;
    end (* else begin
           lprintf " ... new range %Ld-%Ld\n" r.range_begin r.range_end;
           end *)
  end

(** Split a range at [cut_pos] offset, if needed;
    ranges stay linked together *)

let rec split_range r cut_pos =
(*  lprintf "   split_range: cut_pos %Ld\n" cut_pos; *)
  if r.range_begin < cut_pos && r.range_end > cut_pos then
    (* "right" half *)
    let split_r = {
      range_block = r.range_block;
      range_nuploading = 0;
      range_next = r.range_next;
      range_prev = Some r;
      range_begin = cut_pos;
      range_end = r.range_end;
    } in
    (match r.range_next with
     | None -> ()
     | Some old_next_range ->
         old_next_range.range_prev <- Some split_r);
    (* "left" half *)
    r.range_next <- Some split_r;
    r.range_end <- cut_pos
(*    lprintf "      NEW RANGE: %Ld- OLD RANGE: %Ld-%Ld\n"
      split_r.range_begin r.range_begin r.range_end; *)

(** Remove an interval from the ranges of a block, calling
    range_received over all of them

    Assumption: we never download ranges from the middle, so present
    intervals can only overlap the beginning of ranges

    A (double linked) list is definitely not the most efficient
    datastructure for this operation... *)

let set_present_block b interval_begin interval_end =
  let interval_size = interval_end -- interval_begin in
  let old_remaining = b.block_remaining in
  (* download can only happen at the beginning of ranges, so we must
     first split at each interval beginning *)
  iter_block_ranges (fun r ->
    split_range r interval_begin) b;
  iter_block_ranges (fun r ->
    range_received None r interval_begin interval_end) b;
  let new_present = old_remaining -- b.block_remaining in
  if new_present <> interval_size then
    lprintf_nl "set_present_block: %Ld added <> %Ld effectively added"
      interval_size new_present

(** Remove a list of intervals from the ranges of a swarmer *)

let set_present s intervals =
  iter_intervals s (fun i block_begin block_end interval_begin interval_end ->
(*      lprintf "interval: %Ld-%Ld in block %d [%Ld-%Ld]\n"
      interval_begin interval_end i block_begin block_end;  *)
    match s.s_blocks.(i) with
    | EmptyBlock ->
(*          lprintf "  EmptyBlock"; *)
        if block_begin >= interval_begin && block_end <= interval_end
	then begin
(*              lprintf " --> CompleteBlock\n"; *)
          s.s_blocks.(i) <- CompleteBlock;
          must_verify_block s i;
          add_file_downloaded None s (block_end -- block_begin)
        end
        else
          let b = new_block s i in
(*          lprintf " ... set_present_block\n";  *)
          set_present_block b interval_begin interval_end
    | PartialBlock b ->
(*          lprintf "  PartialBlock\n"; *)
        set_present_block b interval_begin interval_end
    | CompleteBlock | VerifiedBlock ->
(*          lprintf "  Other\n"; *)
        ()
  ) intervals

(** reverse absent/present in the list and call set_present *)

let set_absent s list_absent =
(** Build the complementary list of intervals of [intervals] in 
    [set_begin, set_end[ *)
  let rec complementary acc set_begin set_end intervals =
    match intervals with
    | [] ->
	let acc =
          if set_begin = set_end then acc else
            (set_begin, set_end) :: acc
	in
	List.rev acc
    | (interval_begin, interval_end) :: other_intervals ->
	let acc =
          if set_begin = interval_begin then acc
          else (set_begin, interval_begin) :: acc
	in
	complementary acc interval_end set_end other_intervals in
  let list_present = complementary [] Int64.zero s.s_size list_absent in
  set_present s list_present

let intervals_to_string s intervals =
  match intervals with
  | AvailableIntervals intervals ->
      let st = String.make (Array.length s.s_blocks) '0' in
      iter_intervals s (fun i _ _ _ _ -> st.[i] <- '1') intervals;
      st
  | AvailableBitv b -> Bitv.to_string b

(*************************************************************************)
(*                                                                       *)
(*                               Uploaders                               *)
(*                                                                       *)
(*************************************************************************)

(** (debug) output an [uploader] to current log *)

let print_uploader up =
  lprintf_n "  interesting complete_blocks: %d\n     " up.up_ncomplete;
  Array.iter (fun i -> lprintf " %d " i) up.up_complete_blocks;
  lprint_newline ();
  lprintf_n "  interesting partial_blocks: %d\n     " up.up_npartial;
  Array.iter (fun (i, begin_pos, end_pos) ->
      lprintf " %d[%Ld...%Ld] " i begin_pos end_pos
  ) up.up_partial_blocks;
  lprint_newline ()

(** if not [up_declared], 
    sets [up_intervals], [up_complete_blocks], [up_ncomplete],
    [up_partial_blocks], [up_npartial] according to [intervals],
    resets [up_block], [up_block_begin], [up_block_end], and calls
    [client_has_bitmap] on associated client.

    My feeling is that if all those fields only make sense when
    up_declared is true, they should be regrouped in a record option.
*)

let set_uploader_intervals up intervals =
  if up.up_declared then
    lprintf_nl "set_uploader_intervals: called on an already declared uploader\n"
  else
    let t = up.up_t in
    let s = t.t_s in
    (* INVARIANT: complete_blocks must be in reverse order *)

  let complete_blocks = ref [] in
  let partial_blocks = ref [] in

  let incr_availability s i =
    s.s_availability.(i) <- s.s_availability.(i) + 1 in

  (match intervals with
   | AvailableIntervals intervals ->
       iter_intervals s (fun i block_begin block_end interval_begin interval_end ->
(*              lprintf "iter_intervals %d %Ld-%Ld %Ld-%Ld\n"
                i block_begin block_end interval_begin interval_end; *)
         incr_availability s i;

         match s.s_blocks.(i) with
	 | CompleteBlock | VerifiedBlock -> ()
         | EmptyBlock | PartialBlock _ ->
             if block_begin = interval_begin && block_end = interval_end then
               complete_blocks := i :: !complete_blocks
             else
               partial_blocks :=
                 (i, interval_begin, interval_end) :: !partial_blocks
       ) intervals

   | AvailableBitv bitmap ->
       Bitv.iteri_true (fun i ->
         List.iter (fun j ->
           incr_availability s j;
           complete_blocks := j :: !complete_blocks
         ) t.t_blocks_of_chunk.(i)
       ) bitmap
  );

  List.iter (fun i ->
(*        s.s_last_seen.(i) <- BasicSocket.last_time (); *)
    let i = t.t_chunk_of_block.(i) in
    t.t_last_seen.(i) <- BasicSocket.last_time ()
  ) !complete_blocks;

  let complete_blocks = Array.of_list !complete_blocks in
  let partial_blocks = Array.of_list !partial_blocks in
  up.up_intervals <- intervals;

  up.up_complete_blocks <- complete_blocks;
  up.up_ncomplete <- Array.length complete_blocks;
  
  if Array.length partial_blocks > 0 then
    lprintf_nl "WARNING: partial_blocks = %d" (Array.length partial_blocks);
  up.up_partial_blocks <- partial_blocks;
  up.up_npartial <- Array.length partial_blocks;

  up.up_block <- None;
  up.up_block_begin <- zero;
  up.up_block_end <- zero;

  up.up_declared <- true;
  
  let bm = intervals_to_string s intervals in
  client_has_bitmap up.up_client up.up_t.t_file bm;

  if debug_all then print_uploader up

(*************************************************************************)
(*                                                                       *)
(*                         register_uploader                             *)
(*                                                                       *)
(*************************************************************************)

let register_uploader t client intervals =
  let up =
    {
      up_t = t;
      up_client = client;

      up_declared = false;
      up_intervals = intervals;

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
  set_uploader_intervals up intervals;
  up

(*************************************************************************)
(*                                                                       *)
(*                         unregister_uploader                           *)
(*                                                                       *)
(*************************************************************************)

let clear_uploader_ranges up =
  List.iter (fun (_,_,r) ->
    if r.range_nuploading > 0 then
      r.range_nuploading <- r.range_nuploading - 1
    else
      lprintf_nl "clear_uploader_ranges: some range_nuploading was about to become negative\n"
  ) up.up_ranges;
  up.up_ranges <- []

let clear_uploader_block up =
  match up.up_block with
  | None -> ()
  | Some b ->
      let num = b.block_num in
      let t = up.up_t in
      let s = t.t_s in
      if s.s_nuploading.(num) > 0 then
	s.s_nuploading.(num) <- s.s_nuploading.(num) - 1
      else
	lprintf_nl "clear_uploader_block: some s_nuploading was about to become negative\n";
      up.up_block <- None;
      up.up_block_begin <- zero;
      up.up_block_end <- zero

let clear_uploader_intervals up =
  if up.up_declared then
    let decr_availability s i =
      if s.s_availability.(i) > 0 then
	s.s_availability.(i) <- s.s_availability.(i) - 1 
      else 
	lprintf_nl "clear_uploader_intervals: some s_availability was about to become negative\n" in
(*          lprintf "clean_uploader_chunks:\n"; *)
    let t = up.up_t in
    let s = t.t_s in
    Array.iter (decr_availability s) up.up_complete_blocks;
    up.up_complete_blocks <- [||];
    up.up_ncomplete <- 0;
    Array.iter (fun (b,_,_) -> decr_availability s b) up.up_partial_blocks;
    up.up_partial_blocks <- [||];
    up.up_npartial <- 0;
    clear_uploader_block up;
    up.up_declared <- false

let update_uploader_intervals up intervals =
  clear_uploader_intervals up;
  set_uploader_intervals up intervals

let unregister_uploader up =
  clear_uploader_intervals up;
  clear_uploader_ranges up

(** (debug) output the uploaders of a swarmer to current log *)

let print_uploaders s =
  Array.iteri (fun i b ->
    match b with
    | EmptyBlock -> lprintf "_"
    | CompleteBlock -> lprintf "C"
    | VerifiedBlock -> lprintf "V"
    | PartialBlock b ->
        if s.s_nuploading.(i) > 9 then
          lprintf "X"
        else
          lprintf "%d" s.s_nuploading.(i)
  ) s.s_blocks;
  lprint_newline ();
  Array.iteri (fun i b ->
    match b with
    | EmptyBlock -> lprintf "_"
    | CompleteBlock -> lprintf "C"
    | VerifiedBlock -> lprintf "V"
    | PartialBlock b ->
        lprintf "{ %d : %d=" b.block_num
          s.s_nuploading.(b.block_num);
	iter_block_ranges (fun r -> 
          lprintf "(%d)" r.range_nuploading
	) b;
        lprintf " }";
  ) s.s_blocks;
  lprint_newline ()

(** (see uploaders invariants above)
    Drop the [n]th element from the [up.up_ncomplete] first elements
    of [up.complete_blocks] by swapping it with the
    ([up.up_ncomplete]-1)th element, then decrease [up.up_ncomplete];
    Then return that element, after converting associated block to
    PartialBlock if necessary.
*)

let permute_and_return up n =
  assert (n <= up.up_ncomplete-1);
  let b = up.up_complete_blocks.(n) in
  if debug_all then lprintf_nl "permute_and_return %d <> %d" n b;
  if n < up.up_ncomplete then begin
    up.up_complete_blocks.(n) <- up.up_complete_blocks.(up.up_ncomplete-1);
    up.up_complete_blocks.(up.up_ncomplete-1) <- b
  end;
  up.up_ncomplete <- up.up_ncomplete - 1;
  let t = up.up_t in
  let s = t.t_s in
  match s.s_blocks.(b) with
  | EmptyBlock ->
      let b = new_block s b in
      b, b.block_begin, b.block_end
  | PartialBlock b ->
      b, b.block_begin, b.block_end
  | VerifiedBlock ->
      lprintf_nl "ERROR: verified block in permute_and_return %d\n" b;
      assert false
  | CompleteBlock ->
      lprintf_nl "ERROR: complete block in permute_and_return %d\n" b;
      assert false

(** find a block in up_complete_blocks that's not already
    CompleteBlock or VerifiedBlock.
    If none can be found, do the same with up_partial_blocks.
    If none can be found still, raise Not_found exception 
    
    up_ncomplete and up_npartial are used as in the same way as in
    permute_and_return, but no element is ever permuted.

    Since set_uploader_intervals puts the blocks with the lowest
    offsets at the end of up_complete_blocks and up_partial_blocks,
    this also selects the blocks in increasing offsets order.
*)

let linear_select_block up =
  let rec iter_partial up =
    let n = up.up_npartial in
    if n = 0 then raise Not_found;
    let b, block_begin, block_end = up.up_partial_blocks.(n-1) in
    up.up_npartial <- n-1;
    let t = up.up_t in
    let s = t.t_s in
    (* priority bitmap <> 0 here ? *)
    match s.s_blocks.(b) with
    | CompleteBlock | VerifiedBlock ->
        iter_partial up
    | PartialBlock b ->
        b, block_begin, block_end
    | EmptyBlock ->
        let b = new_block s b in
        b, block_begin, block_end in
  let rec iter_complete up =
    let n = up.up_ncomplete in
    if n = 0 then iter_partial up
    else
      let b = up.up_complete_blocks.(n-1) in
      up.up_ncomplete <- n-1;
      let t = up.up_t in
      let s = t.t_s in
      (* priority bitmap <> 0 here ? *)
      match s.s_blocks.(b) with
      | CompleteBlock | VerifiedBlock ->
          iter_complete up
      | PartialBlock b ->
          b, b.block_begin, b.block_end
      | EmptyBlock ->
          let b = new_block s b in
          b, b.block_begin, b.block_end
  in
  iter_complete up

(** Check whether block [n] of swarmer [s] is not completed yet,
    calling chunk verification first if block still need verification *)

let should_download_block s n =
(*  lprintf "should_download_block %d\n" n; *)
  let result =
    match s.s_verified_bitmap.[n] with
    | '0' | '1' -> true
    | '2' ->
        (match s.s_networks with
         | t :: _ ->
	     assert(t.t_primary);
             (try
               let n = t.t_chunk_of_block.(n) in
               if t.t_converted_verified_bitmap.[n] = '2' then
                 verify_chunk t n
             with VerifierNotReady -> ());
	 | [] -> assert false);
        s.s_verified_bitmap.[n] < '2'
    | '3' -> false
    | _ -> assert false
  in
(*  if result then
    lprintf "should_download_block %d\n" n; *)
  result

(*************************************************************************)
(*                                                                       *)
(*                         select_block (internal)                       *)
(*                                                                       *)
(*************************************************************************)

(* Would it be faster not to build those records, and use functions of
   the block number ? *)

type choice = {
  choice_num : int;
  choice_user_priority : int;
  choice_nuploaders : int;
  choice_remaining : int64;
  choice_remaining_per_uploader : int64; (* algo 1 only *)
  choice_saturated : bool; (* has enough uploades *) (* algo 2 only *)
  choice_other_complete : int Lazy.t; (* ...blocks in the same chunk *)
  choice_availability : int;
}

let dummy_choice = {
  choice_num = 0;
  choice_user_priority = 0;
  choice_nuploaders = 0;
  choice_remaining = 0L;
  choice_remaining_per_uploader = 0L; (* algo 1 only *)
  choice_saturated = true; (* algo 2 only *)
  choice_other_complete = lazy 0;
  choice_availability = 0
}

(* based on Array.fold_left code *)
let array_fold_lefti f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f !r i (Array.unsafe_get a i)
  done;
  !r

let subarray_fold_lefti f x a firstidx lastidx =
  let len = Array.length a in
  assert(firstidx >= 0 && firstidx < len);
  assert(lastidx >= 0 && lastidx < len);
  let r = ref x in
  for i = firstidx to lastidx do
    r := f !r i (Array.unsafe_get a i)
  done;
  !r

(* DEBUGGING *)
let delta_needed = ref 0
let delta_undecided = ref 0

let select_block up =
(* DEBUGGING *)
  let compare_choices_saturation = ref 0 in
  let compare_choices_priority = ref 0 in
  let compare_choices_rarity = ref 0 in
  let compare_choices_completion = ref 0 in
  let compare_choices_siblings = ref 0 in
  let compare_choices_failure = ref 0 in

  let t = up.up_t in
  let s = t.t_s in
  try
    match s.s_strategy with
    | LinearStrategy ->
        linear_select_block up
    | _ ->
        if up.up_ncomplete = 0 && up.up_npartial = 0 then raise Not_found;

(* to evaluate the relative rarity of a block, we must compare it to
   the availability of *all* blocks, not only those available from
   that uploader *)
	let sum_availability = Array.fold_left (+) 0 s.s_availability in
	let mean_availability = sum_availability / Array.length s.s_blocks in

	let my_t = if t.t_verifier <> NoVerification then t else 
	  match s.s_networks with
	    | tprim :: _ ->
		assert(tprim.t_primary);
		tprim
	    | [] -> assert false in
	let verification_available = my_t.t_verifier <> NoVerification in

	let several_frontends = List.length s.s_networks > 1 in
	(* many results may not be useful, evaluate them as needed *)
	let completed_blocks_in_chunk = 
	  Array.init my_t.t_nchunks (fun i ->
            lazy (
              List.fold_left (fun acc b ->
		if s.s_verified_bitmap.[b] = '2' then acc + 1 else acc
              ) 0 my_t.t_blocks_of_chunk.(i))) in

	let preview_beginning = 10000000L in
	let preview_end = (s.s_size ** 98L) // 100L in

	(* sources_per_chunk was initially for edonkey only *)
	let data_per_source = 9728000L // (Int64.of_int !!sources_per_chunk) in
	
	let need_to_complete_some_blocks_quickly = 
	  match !!swarming_block_selection_algorithm with
	  | 1 -> true
	  | 2 -> verification_available && t.t_nverified_chunks < 2
	  | _ -> assert false in

	(** > 0 == c1 is best, < 0 = c2 is best, 0 == they're equivalent *)
	let compare_choices1 c1 c2 =

	  (* avoid overly unbalanced situations *)
	  let cmp = 
	    if c1.choice_remaining_per_uploader < data_per_source ||
	      c2.choice_remaining_per_uploader < data_per_source then
		compare c1.choice_remaining_per_uploader 
		  c2.choice_remaining_per_uploader else 0 in
	  if cmp <> 0 then begin
	    incr compare_choices_saturation;
	    cmp 
	  end else

	  (* Do what Master asked for *)
	  let cmp = compare c1.choice_user_priority c2.choice_user_priority in
	  if cmp <> 0 then begin
	    incr compare_choices_priority;
	    cmp 
	  end else

	  (* Pick really rare gems: if average availability of all
	     blocks is higher than 5 connected sources, pick in
	     priority blocks present in at most 3 connected sources;
	     is that too restrictive ? *)
	  let cmp = 
	    if not need_to_complete_some_blocks_quickly && 
	      mean_availability > 5 &&
	      (c1.choice_availability <= 3 || c2.choice_availability <= 3) then
		compare c2.choice_availability c1.choice_availability 
	    else 0 in
	  if cmp <> 0 then begin
	    incr compare_choices_rarity;
	    cmp 
	  end else

	  (* try to quickly complete blocks *)
	  let cmp = 
	    compare c2.choice_remaining c1.choice_remaining in
	  if cmp <> 0 then begin 
	    incr compare_choices_completion;
	    cmp 
	  end else

	  (* try to quickly complete (and validate) chunks; 
	     if there's only one frontend, each chunk has only one
	     block, and looking at siblings make no sense *)
	  let cmp = 
	    if verification_available && several_frontends then 
	      compare (Lazy.force c1.choice_other_complete)
		(Lazy.force c2.choice_other_complete)
	    else 0 in
	  if cmp <> 0 then begin
	    incr compare_choices_siblings;
	    cmp 
	  end else

	  begin
	    (* Can't tell *)
	    incr compare_choices_failure;
	    0 
	  end in

	let compare_choices2 c1 c2 =

	  (* avoid overly unbalanced situations *)
	  let cmp = 
	    match c1.choice_saturated, c2.choice_saturated with
	    | false, false -> 0
	    | true, false -> -1
	    | false, true -> 1
	    | true, true ->
		let result =
		(* both are saturated, try to balance situation *)
		incr delta_needed;
		let delta = 
		  c1.choice_remaining ** Int64.of_int c2.choice_nuploaders -- 
		  c2.choice_remaining ** Int64.of_int c1.choice_nuploaders in
		if delta > c2.choice_remaining then 1
		else if delta < Int64.neg c1.choice_remaining then -1
		else begin
		  (* either way we'll unbalance the situation *)
		  incr delta_undecided;
		  0 
		end in
		lprintf_nl "compare_choices needed delta %d times, which couldn't decide %d times" !delta_needed !delta_undecided;
		result in
	  if cmp <> 0 then begin
	    incr compare_choices_saturation;
	    cmp 
	  end else

	  (* Do what Master asked for *)
	  let cmp = compare c1.choice_user_priority c2.choice_user_priority in
	  if cmp <> 0 then begin
	    incr compare_choices_priority;
	    cmp 
	  end else

	  (* Pick really rare gems: if average availability of all
	     blocks is higher than 5 connected sources, pick in
	     priority blocks present in at most 3 connected sources;
	     is that too restrictive ? *)
	  let cmp = 
	    if not need_to_complete_some_blocks_quickly && 
	      mean_availability > 5 &&
	      (c1.choice_availability <= 3 || c2.choice_availability <= 3) then
		compare c2.choice_availability c1.choice_availability 
	    else 0 in
	  if cmp <> 0 then begin
	    incr compare_choices_rarity;
	    cmp 
	  end else

	  (* try to quickly complete blocks *)
	  let cmp = 
	    compare c2.choice_remaining c1.choice_remaining in
	  if cmp <> 0 then begin 
	    incr compare_choices_completion;
	    cmp 
	  end else

	  (* try to quickly complete (and validate) chunks; 
	     if there's only one frontend, each chunk has only one
	     block, and looking at siblings make no sense *)
	  let cmp = 
	    if verification_available && several_frontends then 
	      compare (Lazy.force c1.choice_other_complete)
		(Lazy.force c2.choice_other_complete)
	    else 0 in
	  if cmp <> 0 then begin
	    incr compare_choices_siblings;
	    cmp 
	  end else

	  begin
	    (* Can't tell *)
	    incr compare_choices_failure;
	    0 
	  end in

	let compare_choices =
	  match !!swarming_block_selection_algorithm with
	  | 1 -> compare_choices1
	  | 2 -> compare_choices2
	  | _ -> assert false in

	let best_choices, specimen = 
	  subarray_fold_lefti (fun ((best_choices, specimen) as acc) n b ->
	  (* priority bitmap <> 0 here ? *)
	  if not (should_download_block s b) then acc else
	    let nchunk = my_t.t_chunk_of_block.(b) in
	    let block_begin = compute_block_begin s b in
	    let block_end = compute_block_end s b in
	    let size = block_end -- block_begin in
	    let remaining = match s.s_blocks.(b) with
		| EmptyBlock -> size
		| PartialBlock b -> b.block_remaining
		| CompleteBlock | VerifiedBlock -> 0L in
	    let nuploaders = s.s_nuploading.(b) in
	    let this_choice = {
	      choice_num = n;
	      choice_user_priority = (* priority bitmap here instead ? *)
		if block_begin < preview_beginning then 3 else
		  if block_end > preview_end then 2 else 1;
	      choice_nuploaders = nuploaders;
	      choice_remaining = remaining;
	      choice_remaining_per_uploader = 
		if !!swarming_block_selection_algorithm = 1 then
		  remaining //
		    (Int64.of_int (nuploaders + 1)) (* planned value *)
		else 0L;
	      choice_saturated =
		if !!swarming_block_selection_algorithm = 2 then
		  not need_to_complete_some_blocks_quickly &&
		    remaining <= Int64.of_int nuploaders ** data_per_source
	      (*
		nuploaders >= Int64.to_int (
		Int64.pred (
		remaining ** Int64.of_int !!sources_per_chunk ++ size) 
		// size)
	      *)
		else true;
	      choice_other_complete = completed_blocks_in_chunk.(nchunk);
	      choice_availability = s.s_availability.(b);
	    } in
	    match best_choices with
	    | [] -> [n], this_choice
	    | _ :: _ ->
		(* all the choices in the accumulator are supposed to
		   be equivalent, compare against the specimen *)
		let cmp = compare_choices this_choice specimen in
		if cmp > 0 then [n], this_choice
		else if cmp < 0 then acc
		else n :: best_choices, specimen
	  ) ([], dummy_choice) up.up_complete_blocks 0 (up.up_ncomplete - 1) in
	(* what about up_partial_blocks ? 
	   currently they're taken care of by linear_select_block
	   fallback below *)

	if debug_all then begin
	  let nbest_choices = List.length best_choices in
	  lprintf_nl "compare_choices: %d choices left based on saturation:%d priority:%d rarity:%d completion:%d siblings:%d failed:%d"
	    nbest_choices
	    !compare_choices_saturation !compare_choices_priority
	    !compare_choices_rarity !compare_choices_completion
	    !compare_choices_siblings !compare_choices_failure;
	  let print_choice c =
	    lprintf_nl "selected %d:%d priority:%d nup:%d rem:%Ld rpu:%Ld sat:%B sib:%s av:%d" 
	      c.choice_num up.up_complete_blocks.(c.choice_num)
	      c.choice_user_priority 
	      c.choice_nuploaders 
	      c.choice_remaining 
	      c.choice_remaining_per_uploader
	      c.choice_saturated
	      (if Lazy.lazy_is_val c.choice_other_complete then
		string_of_int (Lazy.force c.choice_other_complete) else "?") 
	      c.choice_availability in
	  print_choice specimen
	end;

	try
	  let n = 
	    match best_choices with
	      | [] -> raise Not_found
	      | [choice] -> choice
	      | _::_ -> 
		  let nchoices = List.length best_choices in
		  List.nth best_choices (Random.int nchoices) in

          if debug_all then lprintf_nl "\nBlockFound %d"
            up.up_complete_blocks.(n);
          permute_and_return up n
	with Not_found ->
	  if !verbose_swarming then
	    lprintf_nl "select_block: fallback to linear strategy";
	  linear_select_block up
  with Not_found ->

      (* print_s "NO BLOCK FOUND" s; *)
    raise Not_found

(** If uploader is associated to a file being downloaded,
    clear previously selected block (in any) and select best available
    block, according to block selection strategy 
    @param up the uploader *)

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
    | FileCancelled 
    | FileShared
    | FileNew 
    | FileDownloaded -> 
	raise Not_found
    | FileDownloading
    | FileQueued ->
        (match up.up_block with
	 | None -> ()
         | Some b ->
             let num = b.block_num in
             s.s_nuploading.(num) <- s.s_nuploading.(num) - 1;
             up.up_block <- None;
        );

        let b, block_begin, block_end = select_block up in
        let num = b.block_num in
        s.s_nuploading.(num) <- s.s_nuploading.(num) + 1;
        up.up_block <- Some b;
        up.up_block_begin <- block_begin;
        up.up_block_end <- block_end;
        if debug_all then lprintf " = %d \n" num;
        b
  with e ->
    if debug_all then lprintf_nl "Exception %s" (Printexc2.to_string e);
    raise e

(** Remove completed ranges from an uploader's range list, and
    decrease their reference counter *)

let remove_completed_uploader_ranges up =
  let not_completed_ranges, completed_ranges = 
    List.partition (fun (_,_,r) -> 
      r.range_begin < r.range_end) up.up_ranges in
  up.up_ranges <- not_completed_ranges;
  List.iter (fun (_,_,r) -> 
    r.range_nuploading <- r.range_nuploading - 1) completed_ranges

(** uploader accessors *)

let current_ranges up =  up.up_ranges

let current_block up =
  match up.up_block with
  | None -> raise Not_found
  | Some b -> b

(** Check whether a range is in a list *)

let in_uploader_ranges r list =
  List.exists (fun (_,_,r') -> r' == r) list

(*************************************************************************)
(*                                                                       *)
(*                         find_range                                    *)
(*                                                                       *)
(*************************************************************************)

let uploader_ranges_fold_left f acc l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | h :: q -> aux (f acc h) q
  in aux acc l

(** Find a range to upload from [up], that is at most [range_size]
    bytes long (split some range if necessary) *)

(* Is merging at all useful ? Once range starts downloading, they can
   no longer be merged, so it should be very rare... *)
let allow_merge_ranges = true

type ranges_cluster = {
  cluster_ranges: range list;
  cluster_nuploading: int;
  cluster_size: Int64.t
}    

let dummy_ranges_cluster = {
  cluster_ranges = [];
  cluster_nuploading = 0;
  cluster_size = 0L
}

let find_range up range_size =

  (** merge two consecutive ranges in the first, if possible;
      Return true if successful *)
  let merge_ranges r r2 =
    match r.range_next with
    | None -> false
    | Some rr ->
	if rr != r2 ||
	  r.range_end < r2.range_begin ||
	  r2.range_nuploading > 0 then false
	else begin
	  r.range_end <- r2.range_end;
	  r.range_next <- r2.range_next;
	  (match r.range_next with
	  | None -> ()
	  | Some r3 ->
	      r3.range_prev <- Some r);
	  true
	end in

  remove_completed_uploader_ranges up;

  let b =
    match up.up_block with
    | None -> 
	if debug_all then
	  lprintf_nl "find_range: uploader had no block selected";
	raise Not_found
    | Some b -> b
  in
  let t = up.up_t in
  match file_state t.t_file with
  | FilePaused
  | FileAborted _
  | FileCancelled 
  | FileShared
  | FileNew
  | FileDownloaded -> 
      lprintf_nl "find_range: file in bad state";
      raise Not_found
  | FileDownloading
  | FileQueued ->
      if debug_all then
	lprintf_nl "find_range: is there a range of size %Ld in [%Ld-%Ld] for %d ?" 
	  range_size up.up_block_begin up.up_block_end (client_num up.up_client);
      let correct_range r =
	not (in_uploader_ranges r up.up_ranges) &&
	  r.range_begin < r.range_end &&
	  r.range_begin >= up.up_block_begin &&
	  r.range_begin < up.up_block_end in
      (* pick the first correct cluster with fewest uploaders 
	 We're not trying to get a range that's at least as big as
	 [range_size] bytes - that would prevent partially downloaded
	 ranges from being completed first *)
      let rec iter acc r =
	let best_cluster =
	  if not (correct_range r) then acc
	  else 
	    (* find if they're ranges to merge ahead *)
	    let rec iter_cluster r cluster =
	      if debug_all then
		lprintf_nl "[%Ld-%Ld] " r.range_begin r.range_end;
	      let cluster = { cluster with
		cluster_ranges = r :: cluster.cluster_ranges;
		cluster_size = cluster.cluster_size ++ 
		  (r.range_end -- r.range_begin)
	      } in
	      if not allow_merge_ranges ||
		cluster.cluster_size >= range_size then cluster 
	      else
		match r.range_next with
		| None -> cluster
		| Some rr ->
		    if rr.range_begin = r.range_end &&
		      correct_range rr && rr.range_nuploading = 0 then 
		      iter_cluster rr cluster
		    else cluster in

	    let cluster = 
	      iter_cluster r { dummy_ranges_cluster with
		cluster_nuploading = r.range_nuploading } in
	    if debug_all then
	      lprint_newline ();
	    if acc.cluster_ranges = [] then cluster
	    else
	      (* find a range with as few uploaders as possible *)
	      let cmp = compare acc.cluster_nuploading
		cluster.cluster_nuploading in
	      if cmp < 0 then acc
	      else cluster in

	(* fast exit, and why I didn't use an iterator :/ 
	   Could have used an exception, but I don't like that ;) *)
	if best_cluster.cluster_ranges <> [] &&
	  best_cluster.cluster_nuploading = 0 then best_cluster
	else
	  match r.range_next with
	  | None -> best_cluster
	  | Some rr -> iter best_cluster rr in

      let best_cluster = iter dummy_ranges_cluster b.block_ranges in
      match List.rev best_cluster.cluster_ranges with
      | [] -> 
	  if debug_all then
	    lprintf_nl "find_range: no correct range found!";
	  raise Not_found
      | r :: q ->
	  if not (List.for_all (merge_ranges r) q) then
	    lprintf_nl "find_range: ranges did not merge as well as planned";
	  split_range r (min (r.range_begin ++ range_size)
	   up.up_block_end);
	  if debug_all then begin
	    lprintf "=> [%Ld-%Ld], left:" r.range_begin r.range_end;
	    iter_block_ranges (fun r ->
	      lprintf " [%Ld-%Ld]" r.range_begin r.range_end
	    ) b;
	    lprint_newline ();
	  end;
	  let key = r.range_begin, r.range_end, r in
	  up.up_ranges <- up.up_ranges @ [key];
	  r.range_nuploading <- r.range_nuploading + 1;
	  key

(** range accessor(s) *)

let range_range r = (r.range_begin, r.range_end)

(** Data has been received from uploader [up]. Transfer data to file
    and update uploader ranges.
    Data = String.sub [str] [string_begin] [string_len] *)

let received up file_begin str string_begin string_len =
  assert (string_begin >= 0);
  assert (string_len >= 0);
  assert (string_begin + string_len <= String.length str);

(*
  let debug_bad_write r string_pos =
    if !verbose then begin
      let t = up.up_t in
      let s = t.t_s in
      lprintf_nl "BAD WRITE in %s for range %Ld-%Ld (string_pos %d)"
        (file_best_name t.t_file) r.range_begin r.range_end string_pos;
      lprintf_nl "  received: file_pos:%Ld string:%d %d"
        file_begin string_begin string_len;
      lprintf_nl "  ranges:";
      List.iter (fun (_,_,r) ->
        lprintf_n "     range: %Ld-%Ld"
          r.range_begin
          r.range_end;
        (match r.range_next with
          | None -> ()
          | Some rr ->
              lprintf "  next: %Ld" rr.range_begin);
        (match r.range_prev with
          | None -> ()
          | Some rr ->
              lprintf "  prev: %Ld" rr.range_begin);
        lprint_newline ();
        let b = r.range_block in
        lprintf_n "        block: %d[%c] %Ld-%Ld [%s]"
          b.block_num
          s.s_verified_bitmap.[b.block_num]
          b.block_begin b.block_end
          (match s.s_blocks.(b.block_num) with
            | EmptyBlock -> "empty"
            | PartialBlock _ -> "partial"
            | CompleteBlock -> "complete"
            | VerifiedBlock -> "verified"
          );
        let br = b.block_ranges in
        lprintf " first range: %Ld(%Ld)"
          br.range_begin
          (br.range_end -- br.range_begin);
        lprint_newline ();
      ) up.up_ranges
    end;
  if !exit_on_error then exit 2 in *)

  if string_len > 0 then
    let file_end = file_begin ++ (Int64.of_int string_len) in

    if !verbose_swarming then
      lprintf_nl "received on %Ld-%Ld" file_begin file_end;

(* TODO: check that everything we received has been required *)
    let t = up.up_t in
    let s = t.t_s in
    match file_state t.t_file with
    | FilePaused
    | FileAborted _
    | FileCancelled 
    | FileShared
    | FileNew
    | FileDownloaded -> 
	if !verbose then
	  lprintf_nl "CommonSwarming.received: wrong file state";
	()
    | FileDownloading
    | FileQueued ->
	try
	  List.iter (fun (_,_,r) ->
    (* was: r.range_begin < file_end && r.range_end > file_begin *)
            if r.range_begin >= file_begin &&
              r.range_begin < file_end then 
		let file_end = min file_end r.range_end in
		let written_len = file_end -- r.range_begin in
                let string_pos = string_begin +
                  Int64.to_int (r.range_begin -- file_begin) in
                let string_length = Int64.to_int written_len in
		(* None of those conditions can happen anymore *)
(*                if string_pos < 0 || 
                  string_pos < string_begin || 
                  string_len < string_length then 
		    debug_bad_write r string_pos
		  else *)
                  if string_length > 0 then
		    match s.s_networks with
                    | [] -> assert false
		    | tprim :: _ ->
			assert (tprim.t_primary);
                        file_write tprim.t_file
                          r.range_begin
                          str string_pos string_length;
			range_received (Some t) r r.range_begin file_end;
	  ) up.up_ranges;
	  remove_completed_uploader_ranges up
	with e ->
	  remove_completed_uploader_ranges up;
          raise e

(** compute the list of present intervals of a swarmer *)

let present_intervals s =
  (* intervals is a reversed list of intervals *)
  let append_interval ((interval_begin, interval_end) as interval) intervals =
    (* remove void intervals *)
    if interval_begin = interval_end then intervals
    else
      match intervals with
      | [] -> [interval]
      | (last_interval_begin, last_interval_end) :: other_intervals  ->
	  if last_interval_end < interval_begin then
	    interval :: intervals
	  else 
	    (* coalescing intervals *)
	    (last_interval_begin, interval_end) :: other_intervals in

  List.rev (
    array_fold_lefti (fun acc i b -> 
      match s.s_blocks.(i) with
      | EmptyBlock -> acc
      | CompleteBlock | VerifiedBlock ->
	  append_interval (compute_block_begin s i, compute_block_end s i) acc
      | PartialBlock b ->
	  let acc, last_interval_end =
	    block_ranges_fold (fun (acc,  lie) r ->
	      (append_interval (lie, r.range_begin) acc, r.range_end)
	    ) (acc, compute_block_begin s i) b in
	  append_interval (last_interval_end, compute_block_end s i) acc
    ) [] s.s_blocks)

(*************************************************************************)
(*                                                                       *)
(*                         propagate_chunk                               *)
(*                                                                       *)
(*************************************************************************)

type chunk_occurrence = t * int * Int64.t (* frontend, chunk number, offset *)

type chunk_occurrences = {
  mutable occurrence_present : chunk_occurrence list;
  mutable occurrence_missing : chunk_occurrence list;
}

let propagate_chunk t1 pos1 size destinations =
  List.iter (fun (t2, j2, pos2) ->
    if t1 != t2 || pos1 <> pos2 then begin
      lprintf_nl "Should propagate chunk from %s %Ld to %s %Ld [%Ld]"
        (file_best_name t1.t_file) pos1
        (file_best_name t2.t_file) pos2 size;
      Unix32.copy_chunk (file_fd t1.t_file)  (file_fd t2.t_file)
      pos1 pos2 (Int64.to_int size);
      set_frontend_bitmap_2 t2 j2
    end
  ) destinations

let dummy_chunk_occurrences () = 
  { occurrence_present = []; occurrence_missing = [] }

let duplicate_chunks () =
  let chunks = Hashtbl.create 100 in
  HS.iter (fun s ->
    List.iter (fun t ->
      let nchunks = String.length t.t_converted_verified_bitmap in
      match t.t_verifier with
      | Verification uids when Array.length uids = nchunks ->
	  let rec iter j len pos =
            if j < len then
              let c = {
		chunk_uid = uids.(j);
		chunk_size = min (s.s_size -- pos) t.t_chunk_size;
              } in
              let occurrences = 
		try
		  Hashtbl.find chunks c
		with Not_found ->
                  let occurrences = dummy_chunk_occurrences () in
                  Hashtbl.add chunks c occurrences;
                  occurrences in
	      (match t.t_converted_verified_bitmap.[j] with
	      | '0' | '1' ->
		occurrences.occurrence_missing <- 
		  (t, j, pos) :: occurrences.occurrence_missing
	      | '2' -> ()
	      | '3' ->
		occurrences.occurrence_present <- 
		  (t, j, pos) :: occurrences.occurrence_present
	      | _ -> assert false);
              iter (j+1) len (pos ++ t.t_chunk_size)
	  in
	  iter 0 (String.length t.t_converted_verified_bitmap) zero
      | _ -> ()
    ) s.s_networks
  ) swarmers_by_name;
  Hashtbl.iter (fun c occurrences ->
    match occurrences.occurrence_present, occurrences.occurrence_missing with
    | _ , []
    | [], _ -> ()
    | (t, _, pos) :: _, missing ->
        propagate_chunk t pos c.chunk_size missing
  ) chunks


let set_verifier t f =
  t.t_verifier <- f;
(* TODO: check that false as t_primary is a good value to start with *)
  set_chunks_verified_bitmap t t.t_converted_verified_bitmap

let set_verified t f =
  t.t_verified <- f

let downloaded t = file_downloaded t.t_file

let block_chunk_num t b =
  t.t_chunk_of_block.(b.block_num)

let partition_size t = String.length t.t_converted_verified_bitmap

let uploader_swarmer up = up.up_t

(** Return the availability of the chunks of [t] as a string *)

let chunks_availability t =
  let s = t.t_s in
  string_init (partition_size t) (fun i ->
    char_of_int (
      let v = List2.min
        (List.map (fun i -> s.s_availability.(i)) t.t_blocks_of_chunk.(i)) in
      if v < 0 then 0 else
	if v > 200 then 200 else v))

let is_interesting up =
  up.up_ncomplete > 0 || up.up_npartial > 0


(*************************************************************************)
(*                                                                       *)
(*                         value_to_int64_pair (internal)                *)
(*                                                                       *)
(*************************************************************************)

let value_to_int64_pair v =
  match v with
  | List [v1;v2] | SmallList [v1;v2] ->
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
let present_intervals t = present_intervals t.t_s
let print_t str t = print_s str t.t_s
let print_uploaders t = print_uploaders t.t_s

(*************************************************************************)
(*                                                                       *)
(*                         value_to_frontend                             *)
(*                                                                       *)
(*************************************************************************)

let value_to_frontend t assocs =

  let debug_wrong_downloaded t present d =
    lprintf_nl "ERROR: stored downloaded value not restored  !!! (%Ld/%Ld)" (downloaded t) d;
    lprintf_nl "ERROR: present:";
    List.iter (fun (x,y) ->
      lprintf_nl "     (%Ld,%Ld);" x y
    ) present;
    
    let p = present_intervals t in
    lprintf_nl "ERROR: present now:";
    
    let total = 
      List.fold_left (fun acc (x,y) ->
	lprintf_nl "     (%Ld,%Ld);" x y;
	acc ++ (y -- x)
      ) zero p in
    
    lprintf_nl "ERROR: total %Ld" total;
    if p = present then begin
      lprintf_nl "ERROR: both appear to be the same!";
    end;
    if !exit_on_error then exit 2 in

  let get_value name conv = conv (List.assoc name assocs) in

  let primary =
    try get_value "file_primary" value_to_bool with _ -> true in

  (try
    let file_name = get_value "file_swarmer" value_to_string in
    let s = HS.find swarmers_by_name 
	{ dummy_swarmer with s_filename = file_name } in
    associate primary t s
    (* TODO: make as many checks as possible to ensure the file and the swarmers
       are correctly associed. *)
  with Not_found -> ());

  let _ =
    let mtime = try file_mtime t.t_file with _ -> 0. in
    let old_mtime =
      try
        value_to_float (List.assoc "file_mtime" assocs)
      with Not_found -> mtime
    in
    old_mtime = mtime
  in
  
  (try
    try
      set_chunks_verified_bitmap t
        (get_value  "file_chunks" value_to_string)
    with Not_found ->
      set_chunks_verified_bitmap t
        (get_value  "file_all_chunks" value_to_string)
	
  with e ->
    lprintf_nl "Exception %s while loading bitmap"
      (Printexc2.to_string e);
    (* force everything to be checked ASAP ? *)
    set_chunks_verified_bitmap t (String.make (partition_size t) '2')
  );

  (*
  lprintf "set_verified_bitmap: t = %s\n" t.t_converted_verified_bitmap;
  lprintf "set_verified_bitmap: s = %s\n" t.t_s.s_verified_bitmap;
*)

  if primary then begin
    if !verbose_swarming then lprintf_nl "Loading present...";
    let present = try
      let present =
        (get_value "file_present_chunks"
          (value_to_list value_to_int64_pair))
      in
      set_present t present;
      present
    with e ->
      lprintf_nl "Exception %s while set present"
        (Printexc2.to_string e);
      verify_all_chunks t;
      []
    in
    if !verbose_swarming then lprintf_nl "Downloaded after present %Ld" (downloaded t);

    (try
      let d = get_value "file_downloaded" value_to_int64 in
      if d <> downloaded t && !verbose then
	debug_wrong_downloaded t present d
    with Not_found -> ());
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
(*                         frontend_to_value                             *)
(*                                                                       *)
(*************************************************************************)

let frontend_to_value t other_vals =
  [("file_primary", bool_to_value t.t_primary);
   ("file_swarmer", string_to_value t.t_s.s_filename);
   ("file_mtime", float_to_value (try file_mtime t.t_file with _ -> 0.));
   ("file_chunks", string_to_value (chunks_verified_bitmap t))] @
  (if t.t_primary then
    [("file_present_chunks", List
      (List.map (fun (i1,i2) ->
        SmallList [int64_to_value i1; int64_to_value i2])
	(present_intervals t)))] 
   else []) @
  [("file_downloaded", int64_to_value (downloaded t));
   ("file_chunks_age", List (Array.to_list
      (Array.map int_to_value t.t_last_seen)))] @
  other_vals

(** Verify one chunk of swarmer [s], if any frontend of that swarmer
    has a chunk to verify *)

let verify_one_chunk s =
  (*  lprintf "verify_one_chunk: %d networks\n" (List.length s.s_networks);  *)
  List.exists (fun t ->
(*      lprintf "verify_one_chunk of file %d\n" (file_num t.t_file); *)
    string_existsi (fun i c ->
      if c = '2' then verify_chunk t i;
      c = '2') t.t_converted_verified_bitmap
  ) s.s_networks
(*  lprintf "verify_one_chunk: nothing done\n"; *)

(** Verify one chunk of each swarmer that needs it *)

let verify_some_chunks () =
  HS.iter (fun s ->
    try
      ignore(verify_one_chunk s)
    with _ -> ()
  ) swarmers_by_name

(** Verify one chunk of the swarmer associated with [t], if needed *)

let verify_one_chunk t =
  ignore(verify_one_chunk t.t_s)

(** Merge a second frontend [f2] to a first one [f1], so they share
    the same swarmer.

    First swarmer [f1] must support some hashing scheme.
    Data of the second swarmer [f2] is currently lost during merging, so
    you'd better merge in swarmers quickly.
    Merging is denied if any of the two frontends is being used, so it
    may be necessary to pause them first, to get rid of any downloads.
*)

let merge f1 f2 =

  let s1 = HS.find swarmers_by_name { dummy_swarmer with 
    s_filename = file_disk_name f1 } in
  let s2 = HS.find swarmers_by_name { dummy_swarmer with 
    s_filename = file_disk_name f2 } in

  if s1.s_filename = s2.s_filename then
    failwith "Files are already sharing their swarmer";

  if s1.s_size <> s2.s_size then
    failwith "Files don't have the same size";

  let t2 = 
    match s2.s_networks with
    | [t] -> t
    | list ->
        lprintf_nl "s_networks: %d files" (List.length list);
        failwith "Second file is already merged with other files"
  in

  let t1 =
    match s1.s_networks with
    | [] -> assert false
    | t1 :: _ ->
        match t1.t_verifier with
	| NoVerification | VerificationNotAvailable ->
            failwith "Cannot use first file as a primary for swarming (no verification scheme)"
        | Verification _ | ForceVerification -> t1
  in

  List.iter (fun (s, filename) ->
    Array.iteri (fun i nuploading ->
      if nuploading > 0 then
        failwith (Printf.sprintf "%s is currently being downloaded" filename)
    ) s.s_nuploading
  ) [
    s1, "First file";
    s2, "Second file"];

(* replace T2 swarmer *)
  associate false t2 t1.t_s

(*************************************************************************)
(*                                                                       *)
(*                         has_secondaries                               *)
(*                                                                       *)
(*************************************************************************)

let has_secondaries t =
  t.t_primary && List.length t.t_s.s_networks > 1

(*************************************************************************)
(*                                                                       *)
(*                         Remove swarmer                                *)
(*                                                                       *)
(*************************************************************************)

let remove_swarmer file_swarmer =
  match file_swarmer with 
  | None -> () 
  | Some sw -> if not (has_secondaries sw)
                then HS.remove swarmers_by_name sw.t_s
                else lprintf_nl "Tried to remove swarmer with secondaries"

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
      | Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let file_size = get_value "file_size" value_to_int64 in
          let file_name = get_value "file_name" value_to_string in
          let s = create_swarmer file_name file_size in
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
            (List.map (fun t -> t.t_chunk_size) s.s_networks));
        ]

    let t =
      define_option_class "Swarmer" value_to_swarmer swarmer_to_value

  end

(** Checks most variants of a swarmer, nobably verification bitmaps
    consistency; Raise an exception if a problem is found *)

let check_swarmer s =
  try
    match s.s_networks with
    | [] -> assert false
    | tprim :: tail ->
        assert(tprim.t_primary);

	string_iter (fun i c ->
	    if c = '3' then begin
	      if List.exists (fun j -> s.s_verified_bitmap.[j] <> '3') 
		tprim.t_blocks_of_chunk.(i) then
                  failwith "Bad propagation of 3 from primary to swarmer";
            end
            else if List.exists (fun j -> s.s_verified_bitmap.[j] = '3')
	      tprim.t_blocks_of_chunk.(i) then
                failwith "Swarmer has 3 not coming from primary";
	) tprim.t_converted_verified_bitmap;

        let fd = file_fd tprim.t_file in

        List.iter (fun t ->
          assert (not t.t_primary);
          assert (file_fd t.t_file == fd);
	  
	  string_iter (fun i c ->
	    if c = '3' then begin
	      if List.exists (fun j -> s.s_verified_bitmap.[j] <> '3')
		t.t_blocks_of_chunk.(i) then
                  failwith "3 in secondary without 3 in primary"
	    end 
	    else if c = '2' then begin
	      if List.exists (fun j -> s.s_verified_bitmap.[j] <> '3')
		t.t_blocks_of_chunk.(i) then
                  failwith "2 in secondary without 3 in primary"
	    end 
	  ) t.t_converted_verified_bitmap
        ) tail
  with e ->
    print_s "ERROR" s;
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
    swarmers =:= []
  )

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

(* Compute an approximation of the storage used by this module *)

let _ =
  BasicSocket.add_infinite_timer 300. duplicate_chunks;
  Heap.add_memstat "CommonSwarming" (fun level buf ->
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
            (8 + match up.up_intervals with
            | AvailableIntervals list -> List.length list * (12 + 12 + 12 + 12)
            | AvailableBitv b -> let ws = Sys.word_size in (ws/8) + ((ws / 8) * (Bitv.length b / (ws - 2))) 
          ) ;
          incr counter;
      ) uploaders_by_num;
      Printf.bprintf buf "  Uploaders: %d\n" !counter;
      Printf.bprintf buf "  Storage: %d bytes\n" !storage;
  )
