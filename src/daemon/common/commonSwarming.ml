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
open Options
open Printf2
  
open CommonOptions

  
let check_swarming = false
let debug_present_chunks = false
let debug_all = false

open CommonTypes

type strategy =
  LinearStrategy    (* one after the other one *)
| AdvancedStrategy  (* first chunks first, rarest chunks second, 
     complete first third, and random final *)

exception VerifierNotReady

type chunks =
  AvailableRanges of (int64 * int64) list
(* A bitmap is encoded with '0' for empty, '1' for present '2' complete '3' verified *)
| AvailableCharBitmap of string 
(* A bitmap encoded as a bit vector *)
| AvailableBitv of Bitv.t

type verification =
  NoVerification
| VerificationNotAvailable
| ForceVerification
| Verification of uid_type array
  
(* let debug_all = true *)
let exit_on_error = ref false

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[cSw2] "
    (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[cSw2] "
    (log_time ()); lprintf

open CommonTypes

open Int64ops
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

type chunk = {
    chunk_uid : uid_type;
    chunk_size : int64;
  }

(* network "frontend"/"view"/... to a swarmer *)
(* glossary:
   network frontend use "chunks" of data,
   swarmer use "blocks" of data *)
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

(* mapping from network chunks to swarmer blocks *)
    mutable t_blocks_of_chunk : int list array;
(* mapping from swarmer blocks to network chunks *)
    mutable t_chunk_of_block : int array;
  }

and swarmer = {
    mutable s_num : int;
    mutable s_filename : string;

    mutable s_networks : t list; (** list of frontends, primary at head 
				     t.t_s = s <=> t in s.s_networks *)
    mutable s_size : int64;
    mutable s_range_size : int64;
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
				      [block] *)
    mutable block_remaining : int64; (* unused ? *)
  }

and range = {
    mutable range_block : block;
    mutable range_begin : Int64.t; (* official begin int64 *)
    mutable range_end : Int64.t;
    mutable range_prev : range option;
    mutable range_next : range option;
    mutable range_current_begin : Int64.t; (* current begin pos *)
(*        mutable range_verified : bool; *)
    mutable range_nuploading : int; (* current number of clients
				       filling that range ? *)
  }

and uploader = {
    up_t : t;
    up_client : client;

    mutable up_declared : bool;

    mutable up_chunks : chunks;
    mutable up_complete_blocks : int array; (** block numbers *)
    mutable up_ncomplete : int;

    mutable up_partial_blocks : (int * int64 * int64) array; (** block
								 number,
								 begin_pos,
								 end_pos
								 *)
    mutable up_npartial : int;

    mutable up_block : block option;
    mutable up_block_begin : int64;
    mutable up_block_end : int64;

    mutable up_ranges : (int64 * int64 * range) list;
  }

(* range invariants: 
   Ranges represent "holes" of missing data in a block.

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
   r.range_prev.range_end <= r.range_begin <= r.range_current_begin <=
   r.range_end <= r.range_next.range_begin <= ...
   <= b.block_end
   
   Role played by r.range_current_begin is unclear for now. *)


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

(** (debug) output an [uploader] to current log *)

let print_uploader up =
  lprintf_n () "  interesting complete_blocks: %d\n     " up.up_ncomplete;
  Array.iter (fun i -> lprintf " %d " i) up.up_complete_blocks;
  lprint_newline ();
  lprintf_n () "  interesting partial_blocks: %d\n     " up.up_npartial;
  Array.iter (fun (i, begin_pos, end_pos) ->
      lprintf " %d[%Ld...%Ld] " i begin_pos end_pos
  ) up.up_partial_blocks;
  lprint_newline ()

(** sets [t.t_last_seen] of the verified blocks to current time, and 
    associated file's [t.t_file] last seen value to the oldest of the
    remaining last seen values *)

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

(** if a swarmer is already associated with that [file_name], return it;
    Otherwise create a new one with default values (including a default
    [range_size] instead of the provided value ??) *)

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
      range_current_begin = pos;
    }
  in
  r

(** (internal) assigns range [r], and all other ranges along
    [range_next] links, to block [b] *)

let rec own_ranges b r =
  r.range_block <- b;
  match r.range_next with
    None -> ()
  | Some r -> own_ranges b r

(** (internal) 
    Find ranges that are after [cut_pos] offset, unlink them from r
    double-linked list of ranges, set their owner to [b] and return
    the first of the removed ranges.

    If all ranges are before [cut_pos] return a 0-sized range.

    If [cut_pos] is within one of the ranges, that range is cut in
    two at [cut_pos] offset, and link each half to its side.

    What should happen to range_begin is unclear.

    Also, what do to if range_nuploaders is not 0 ?
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
	range_begin = cut_pos;
	range_current_begin = max r.range_current_begin cut_pos
      } in
      (* "left" half *)
      r.range_next <- None;
      r.range_end <- cut_pos;
      r.range_current_begin <- min r.range_current_begin cut_pos;

      if r.range_nuploading <> 0 then
	lprintf_n () "WARNING: Splitting a range currently being uploaded, don't know what to do with range_nuploaders :/\n";

      split_r in
  let cut_ranges = iter r in
  own_ranges b cut_ranges;
  cut_ranges
  
(** Return true if ranges fully "cover" their block
    ("the block is made of holes") *)

let empty_block b =
  let rec iter begin_pos r =
    r.range_current_begin = begin_pos &&
    (match r.range_next with
        Some rr -> iter r.range_end rr
      | None -> r.range_end = b.block_end)
  in
  iter b.block_begin b.block_ranges

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
          EmptyBlock | CompleteBlock | VerifiedBlock ->

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
    lprintf_nl () "WARNING: splitting swarmer discarded availability counters\n";
  if array_exist ((<>) 0) s.s_nuploading then
    lprintf_nl () "WARNING: splitting a swarmer beging uploaded to\n";

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
            PartialBlock b -> b.block_num <- i
          | _ -> ()
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
  if s.s_size <> size then
    begin
      lprintf_nl () "file_size for %s does not match: swarmer %Ld / real %Ld" s.s_filename s.s_size size;
      exit 2
    end;

  (* shouldn't just [t] be removed from the list ? *)
  (* t.t_s.s_networks <- []; *)
  t.t_s.s_networks <- List.filter ((!=) t) t.t_s.s_networks;

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
    t.t_primary <- false;
    s.s_networks <- s.s_networks @ [t];
    (* TODO: transfer data into swarmer instead of discarding it *)
    Unix32.remove (file_fd t.t_file);
  end;
(* at this point, we are supposed to split the blocks in the swarmer
in smaller blocks depending on the block_size of this network, and compute
the t_chunk_of_block and t_blocks_of_chunk fields. *)

  let chunk_size = t.t_block_size in

  split_blocks s chunk_size;

  let nblocks = Array.length s.s_blocks in
  (* For all networks, adjust the chunks and mappings *)
  List.iter (fun t ->
      let nchunks = String.length t.t_converted_verified_bitmap in
      t.t_chunk_of_block <- Array.create nblocks 0;
      t.t_blocks_of_chunk <- Array.create nchunks [];

      let chunk_size = t.t_block_size in
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
    begin
      match s.s_networks with
        t :: tail when is_primary ->
          List.iter (fun tt ->
              assert (not tt.t_primary);
              set_file_fd tt.t_file (file_fd t.t_file)
          ) tail

      | tt :: tail when tt.t_primary ->
          assert (not is_primary);
          set_file_fd t.t_file (file_fd tt.t_file)
      | _ -> ()
    end;

  ()

(** Create a primary frontend and its swarmer *)

let create ss file chunk_size =

  let size = file_size file in
  (* wrong if size is a multiple of chunk_size ? *)
  let nchunks =
    1 + Int64.to_int (Int64.pred size // chunk_size) in

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

      t_chunk_of_block = [||];
      t_blocks_of_chunk = Array.create nchunks [];
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

(** Finds the number of the block containing [chunk_pos] offset, using
    dichotomy *)

let compute_block_num s chunk_pos =
  let b = s.s_block_pos in
  let rec iter min max =
    if min = max then min 
    else
      let medium = (min + max + 1) / 2 in
      if chunk_pos < b.(medium) then
	iter min (medium - 1)
      else
	let medium1 = medium + 1 in
	if chunk_pos >= b.(medium1) then
	  iter medium1 max
	else
	  medium
  in
  let i = iter 0 (Array.length b - 1) in
  if debug_all then
    lprintf_nl () "%Ld is block %d [%Ld-%Ld]" chunk_pos i
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
                  lprintf_nl () "Apply: %d %Ld-%Ld %Ld-%Ld"
                    i block_begin block_end chunk_begin current_end;

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
  lprintf_nl () "Ranges after %s:" str;

  let rec iter r =
    lprintf_n () " %Ld(%Ld)-%Ld(%d)"
      r.range_begin r.range_current_begin r.range_end r.range_nuploading;
    match r.range_next with
      None -> lprint_newline()
    | Some r -> iter r
  in

  Array.iteri (fun i b ->
      lprintf_n () "   %d: " i;
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
        PartialBlock b ->
          lprintf " [%Ld .. %Ld] --> "
            b.block_begin b.block_end;
          iter b.block_ranges
      | EmptyBlock -> lprintf_nl2 "_"
      | CompleteBlock -> lprintf_nl2 "C"
      | VerifiedBlock -> lprintf_nl2 "V"
  ) s.s_blocks;

  lprintf_nl () "Files:";
  List.iter (fun t ->
      lprintf_nl () "  File num: %d" (file_num t.t_file);
      lprintf_nl () "  %s" (if t.t_primary then "primary" else "secondary");
      lprintf_nl () "  Downloaded: %Ld" (file_downloaded t.t_file);
      lprintf_nl () "  Bitmap: %s" t.t_converted_verified_bitmap
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
  lprintf_n () "Block %d: %Ld-%Ld"
    b.block_num b.block_begin b.block_end;
  lprint_newline ();
  lprintf_nl () "  ranges:";
  let rec iter_range r =
    lprintf_nl () "   %Ld-%Ld" r.range_current_begin r.range_end;
    match r.range_next with
      None -> ()
    | Some rr -> iter_range rr
  in
  iter_range b.block_ranges;
  lprint_newline ()

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
          lprintf_nl () "ERROR: file_downloaded < zero!";

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
          let j = t.t_chunk_of_block.(i) in
          if List.for_all (fun i -> s.s_verified_bitmap.[i] = '0')
            t.t_blocks_of_chunk.(j) then
            t.t_converted_verified_bitmap.[j] <- '0'
      ) s.s_networks
    end

(* we have started downloading this block, so mark all containing blocks
  also as started. *)
let set_bitmap_1 s i =
  if s.s_verified_bitmap.[i] = '0' then begin
      s.s_verified_bitmap.[i] <- '1';
      List.iter (fun t ->
          let j = t.t_chunk_of_block.(i) in
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
          let j = t.t_chunk_of_block.(i) in
          if List.for_all (fun i -> s.s_verified_bitmap.[i] = '2')
            t.t_blocks_of_chunk.(j) &&
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
              let j = t.t_chunk_of_block.(i) in
              if List.for_all (fun i -> s.s_verified_bitmap.[i] = '3')
                t.t_blocks_of_chunk.(j) then
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
      List.iter (fun j -> set_verified_block s j) t.t_blocks_of_chunk.(i);
      if !verbose_swarming then
        print_s "VERIFIED" s
    end

(*************************************************************************)
(*                                                                       *)
(*                         verify (internal)                             *)
(*                                                                       *)
(*************************************************************************)

let verify t chunks num begin_pos end_pos =
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

    | ForceVerification ->
        set_verified_chunk t i;
        t.t_verified t.t_nverified_blocks i

    | Verification chunks when Array.length chunks = nblocks ->

        begin try
            let s = t.t_s in
            let block_begin = t.t_block_size *.. i in
            let block_end = block_begin ++ t.t_block_size in
            let block_end = min block_end s.s_size in
            if verify t chunks i block_begin block_end then
	      begin
                set_verified_chunk t i;
                t.t_verified t.t_nverified_blocks i;
                if !verbose_swarming || !verbose then
		  lprintf_nl () "Completed block %d/%d of %s"
                    (i + 1) t.t_nchunks (file_best_name t.t_file)
              end
	    else
	      begin
                t.t_ncomplete_blocks <- t.t_ncomplete_blocks - 1;

                if List.for_all (fun i ->
                      s.s_verified_bitmap.[i] = '2'
                  ) t.t_blocks_of_chunk.(i)
		then
		  begin
		    if !verbose_swarming || !verbose then
                      lprintf_nl () "Complete block %d/%d of %s failed verification, reloading..."
                            (i + 1) t.t_nchunks (file_best_name t.t_file);

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
                    ) t.t_blocks_of_chunk.(i)
                  end
		else
		  begin
		    if !verbose_swarming then begin
                      let nsub = ref 0 in
                        lprintf_n () "  Swarmer was incomplete: ";
                        List.iter (fun i ->
                          lprintf "%c" s.s_verified_bitmap.[i];
                          if s.s_verified_bitmap.[i] = '2' then incr nsub;
                        ) t.t_blocks_of_chunk.(i);
                        lprintf_nl2 "   = %d/%d" !nsub (List.length t.t_blocks_of_chunk.(i))
		    end;
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

                lprintf_nl () "Verification of blocks for file %s FAILED\n"
                    (file_best_name t.t_file);

                  for i = 0 to nblocks - 1 do
                    if t.t_converted_verified_bitmap.[i] = '2' then begin

                        t.t_ncomplete_blocks <- t.t_ncomplete_blocks - 1;
                        if List.for_all (fun i ->
                              s.s_verified_bitmap.[i] = '2'
                          ) t.t_blocks_of_chunk.(i) then begin

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
                            ) t.t_blocks_of_chunk.(i)
                          end else begin
                            let nsub = ref 0 in

                            lprintf_n () "  Swarmer was incomplete: ";
                            List.iter (fun i ->
                                lprintf "%c" s.s_verified_bitmap.[i];
                                if s.s_verified_bitmap.[i] = '2' then incr nsub;
                                ) t.t_blocks_of_chunk.(i);
                            lprintf_nl2 "   = %d/%d" !nsub (List.length t.t_blocks_of_chunk.(i));

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
        let i = t.t_chunk_of_block.(i) in
        t.t_converted_verified_bitmap.[i] <- '2';
(*        List.iter (fun j ->
            if s.s_verified_bitmap.[j] <> '2' then begin
                lprintf "   block %d not downloaded\n" j;
                exit_on_error := false;
              end;
        ) t.t_blocks_of_chunk.(i);  *)
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
  if debug_all then lprintf_nl () "NB[%s]" s.s_verified_bitmap;
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
          lprintf_nl () "EXCEPTION IN range_received: %s"
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
(*                         chunks_to_string (internal)                   *)
(*                                                                       *)
(*************************************************************************)

let chunks_to_string s chunks =
  match chunks with
      AvailableRanges chunks ->
        begin
          let st = String.make (Array.length s.s_blocks) '0' in
          apply_intervals s (fun i block_begin block_end chunk_begin chunk_end -> st.[i] <- '1') chunks;
          st
        end
  | AvailableCharBitmap st -> st
  | AvailableBitv b -> Bitv.to_string b

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
              ) t.t_blocks_of_chunk.(i)
          done;
      | AvailableBitv bitmap ->
          for i = 0 to Bitv.length bitmap - 1 do
            if Bitv.get bitmap i then
              List.iter (fun i ->
                  s.s_availability.(i) <- s.s_availability.(i) + 1;
                  complete_blocks := i :: !complete_blocks
              ) t.t_blocks_of_chunk.(i)
          done;
    end;

    List.iter (fun i ->
(*        s.s_last_seen.(i) <- BasicSocket.last_time (); *)

        let i = t.t_chunk_of_block.(i) in
        t.t_last_seen.(i) <- BasicSocket.last_time ()

    ) !complete_blocks;

    let complete_blocks = Array.of_list !complete_blocks in
    let partial_blocks = Array.of_list !partial_blocks in
    up.up_chunks <- chunks;

    up.up_complete_blocks <- complete_blocks;
    up.up_ncomplete <- Array.length complete_blocks;

    if Array.length partial_blocks > 0 then
      lprintf_nl () "WARNING: partial_blocks = %d" (Array.length partial_blocks);
    up.up_partial_blocks <- partial_blocks;
    up.up_npartial <- Array.length partial_blocks;

    up.up_block <- None;
    up.up_block_begin <- zero;
    up.up_block_end <- zero;

    up.up_declared <- true;

    let bm = chunks_to_string s chunks in
    client_has_bitmap up.up_client up.up_t.t_file bm;

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
  lprint_newline ();
  for i = 0 to nblocks - 1 do

    match s.s_blocks.(i) with
      EmptyBlock -> lprintf "_"
    | CompleteBlock -> lprintf "C"
    | VerifiedBlock -> lprintf "V"
    | PartialBlock b ->
        lprintf "{ %d : %d=" b.block_num
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
  lprint_newline ()

(*************************************************************************)
(*                                                                       *)
(*                         permute_and_return (internal)                 *)
(*                                                                       *)
(*************************************************************************)

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
      lprintf_nl () "ERROR: verified block in permute_and_return %d\n" b;
      assert false
  | CompleteBlock ->
      lprintf_nl () "ERROR: complete block in permute_and_return %d\n" b;
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
                  let n = t.t_chunk_of_block.(n) in
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
  if debug_all then lprintf_nl () "(Random %d -> %d)" max x;
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

                if debug_all then lprintf_nl () "{First}";

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

(* This must be the position of the second last block of the file *)
                download_first 0 (nblocks-2);

(* These can be the positions of the second block of the file *)
                download_first 0 1;
                download_first (up.up_ncomplete-1) 1;
                download_first (up.up_ncomplete-2) 1;

(************* If the file can be verified, and we don't have a lot of blocks
    yet, try to download the partial ones as soon as possible *)

                if debug_all then lprintf_nl () "{PartialBlock}";

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
                    if debug_all then lprintf_n () "{PartialChunk}";

                    let my_t = if t.t_verifier <> NoVerification then t
                      else match s.s_networks with t :: _ -> t | _ -> t in

                    let download_partial max_uploaders =
                      let partial_block = ref (-1) in
                      let partial_remaining = ref 0 in
                      for i = 0 to up.up_ncomplete - 1 do
                        let n = up.up_complete_blocks.(i) in
(* TODO move this after the first if... *)
                        let t_index = t.t_chunk_of_block.(n) in
                        let bs = List.filter (fun s_index ->
                              s.s_verified_bitmap.[s_index] = '2'
                          ) t.t_blocks_of_chunk.(t_index) in
                        let nbs = List.length bs in

(* TODO remove this *)
                        let b = should_download_block s n in

                        if !verbose_swarming then
                            lprintf_nl2 "  test %d %c %d %b %d"
                              n s.s_verified_bitmap.[n] s.s_nuploading.(n)
                            b nbs;

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
                            lprintf_n () "PartialChunk won for %d waiting blocks"
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
              iter_max_uploaders !!sources_per_chunk;
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

        let (b,block_begin,block_end) (* as result *) = select_block up in
        let num = b.block_num in
        s.s_nuploading.(num) <- s.s_nuploading.(num) + 1;
        up.up_block <- Some b;
        up.up_block_begin <- block_begin;
        up.up_block_end <- block_end;
        if debug_all then lprintf " = %d \n" num;
        b
  with e ->
      if debug_all then lprintf_nl () "Exception %s" (Printexc2.to_string e);
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
              lprintf_nl () "error: range is empty error";
            key
          end else
        match r.range_next with
          None -> raise Not_found
        | Some rr -> iter limit rr
      in
      try
(* try normal ranges *)
        iter !!sources_per_chunk r
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
      lprintf_nl () "received on %Ld-%Ld" file_begin file_end;

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
                        if !verbose then
                        begin
                        lprintf_nl () "BAD WRITE in %s for range %Ld-%Ld (string_pos %d)"
                          (file_best_name t.t_file) r.range_begin r.range_end string_pos;
                        lprintf_nl () "  received: file_pos:%Ld string:%d %d"
                          file_begin string_begin string_len;
                        lprintf_nl () "  ranges:";
                        List.iter (fun (_,_,r) ->
                            lprintf_n () "     range: %Ld-[%Ld]-%Ld"
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
                            lprint_newline ();
                            let b = r.range_block in
                            lprintf_n () "        block: %d[%c] %Ld-%Ld [%s]"
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
                            lprint_newline ();
                        ) up.up_ranges;
                        end;
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
      lprintf_nl () "iter_block_out %d bb: %Ld"
        i block_begin;

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
      lprintf_nl () "iter_block_in %d bb: %Ld cb:%Ld"
        i block_begin chunk_begin
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
      lprintf_nl () "iter_range_out %d nb: %Ld bb:%Ld"
        i block_end block_begin;

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
      lprintf_nl () "iter_range_in %d bn: %Ld cb:%Ld ce: %Ld"
        i block_end chunk_begin chunk_end;

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
          ) t.t_blocks_of_chunk.(i);
          if t.t_converted_verified_bitmap.[i] <> '3' then
            lprintf_nl () "FIELD AS BEEN CLEARED"
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
  let n = t.t_chunk_of_block.(b.block_num) in
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
          (List.map (fun i -> s.s_availability.(i)) t.t_blocks_of_chunk.(i)) in
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

  let _ =
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
        lprintf_nl () "Exception %s while loading bitmap"
          (Printexc2.to_string e);
  );

  (*
  lprintf "set_verified_bitmap: t = %s\n" t.t_converted_verified_bitmap;
  lprintf "set_verified_bitmap: s = %s\n" t.t_s.s_verified_bitmap;
*)

  if primary then begin
      if !verbose_swarming then lprintf_nl () "Loading present...";
      let present = try
          let present =
            (get_value "file_present_chunks"
                (value_to_list value_to_int64_pair))
          in
          set_present t present;
          present
        with e ->
            lprintf_nl () "Exception %s while set present"
              (Printexc2.to_string e);
            []
      in
      if !verbose_swarming then lprintf_nl () "Downloaded after present %Ld" (downloaded t);

(*
      if !verbose then lprintf_nl () "Loading absent...";
      (try
          set_absent t
            (get_value "file_absent_chunks"
              (value_to_list value_to_int64_pair));
        with e ->
            if !verbose_hidden_errors then lprintf_nl () "Exception %s while set absent"
              (Printexc2.to_string e);
      );
      if !verbose then lprintf_nl () "Downloaded after absent %Ld" (downloaded t);
*)
      (try
          let d = get_value "file_downloaded" value_to_int64 in

          if d <> downloaded t && !verbose then begin
              lprintf_nl () "ERROR: stored downloaded value not restored  !!! (%Ld/%Ld)" (downloaded t) d;
              lprintf_nl () "ERROR: present:";
              List.iter (fun (x,y) ->
                  lprintf_nl () "     (%Ld,%Ld);" x y
              ) present;

              let p = present_chunks t in
              lprintf_nl () "ERROR: present now:";

              let total = ref zero in
              List.iter (fun (x,y) ->
                  lprintf_nl () "     (%Ld,%Ld);" x y;
                  total := !total ++ (y -- x);
              ) p;

              lprintf_nl () "ERROR: total %Ld" !total;
              if p = present then begin
                  lprintf_nl () "ERROR: both appear to be the same!";
                end;
        if !exit_on_error then exit 2
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
        lprintf_nl () "s_networks: %d files" (List.length list);
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
(*                         Remove swarmer                                *)
(*                                                                       *)
(*************************************************************************)

let remove_swarmer file_swarmer =
  match file_swarmer with 
    None -> () 
  | Some sw -> if not (has_secondaries sw)
                then HS.remove swarmers_by_name sw.t_s
                else lprintf_nl () "Tried to remove swarmer with secondaries"

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
          ) t.t_blocks_of_chunk.(i)
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
              ) t.t_blocks_of_chunk.(i)
            done;
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

      swarmers =:= [])


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
            (8 + match up.up_chunks with
              AvailableRanges list -> List.length list * (12 + 12 + 12 + 12)
            | AvailableCharBitmap s -> 8 + String.length s
            | AvailableBitv b -> let ws = Sys.word_size in (ws/8) + ((ws / 8) * (Bitv.length b / (ws - 2))) 
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
          lprintf_nl () "Downloaded size differs after complete verification";
        true
  with _ -> false


