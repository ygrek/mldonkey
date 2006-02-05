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


open Printf2
open Md4
open Int64ops
open Options

open BasicSocket
open TcpBufferedSocket

open CommonSources
open CommonRoom
open CommonShared
open CommonGlobals
open CommonFile
open CommonClient
open CommonComplexOptions
open CommonOptions
open CommonResult
open CommonTypes

open GuiTypes
open GuiProto

open DonkeyTypes
open DonkeyMftp
open DonkeyProtoCom
open DonkeySources
open DonkeyOptions
open DonkeyComplexOptions
open DonkeyGlobals

type proposal =
  GlobalLRU
| History
| SizeLRU
| ExtLRU

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[EDK] "
    (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[EDK] "
    (log_time ()); lprintf

(* This means that we have just received the fact that this client
has this file. *)

type file_info = {
    fi_size : int;
    fi_ext : string;
    mutable fi_time : int;
  }

type neighbour = {
    mutable nb_files : (int * file_info) list;
    mutable nb_sort : int;
    mutable nb_client : client;
  }

let neighbours = Hashtbl.create 131

let file_info file time =
  {
    fi_time = time;
    fi_size = int_of_float (log (
        Int64.to_float (file_size file ++ one)));
    fi_ext = String.lowercase (
      Filename2.last_extension (file_best_name file));
  }

exception Continue

let ngood_propositions = Array.create 4 zero
let nbad_propositions = Array.create 4 zero

let incr_propositions kind result =
  let propositions = match result with
      File_not_found -> nbad_propositions
    | File_found -> ngood_propositions
    | _ -> assert false
  in
  let kind = match kind with
      GlobalLRU -> 0
    | History -> 1
    | ExtLRU -> 2
    | SizeLRU -> 3
  in
  propositions.(kind) <- propositions.(kind) ++ one

let propositions = Fifo.create ()

let rec compute_stats () =
  if not (Fifo.empty propositions) then
    let (s, file, kind, time) = Fifo.head propositions in
    try
      let r = DonkeySources.find_request_result s file.file_sources in
      match r with
      | File_new_source ->
          if time + 1800 < last_time () then begin
              if !verbose then
                lprintf_nl () "WARNING, source was not tested";
              raise Continue
            end
      (* expected not used anymore
      | File_expected  ->
          if time + 1800 < last_time () then begin
              incr_propositions kind File_not_found;
              raise Continue
            end *)
      | File_possible ->
          if !verbose then
            lprintf_nl () "WARNING, source was unknown";
          if time + 1800 < last_time () then raise Continue
      | File_not_found ->
          incr_propositions kind File_not_found;
          raise Continue
      | File_found
      | File_chunk
      | File_upload ->
          incr_propositions kind File_found;
          raise Continue
    with
    | Not_found ->
(* For some reason, the request was forgotten. Forget it... *)
        if !verbose then
          lprintf_nl () "ERROR, request was forgotten";
        let _ = Fifo.take propositions in
        compute_stats ()
    | Continue ->
        let _ = Fifo.take propositions in
        compute_stats ()

let propose_source file c kind =
  try
    List.iter (fun r ->
        if r.request_file == file.file_sources then raise Exit
    ) c.client_source.source_files;

    match c.client_kind with
      Direct_address (ip, port) ->
        let s = DonkeySources.find_source_by_uid c.client_kind in
        DonkeySources.set_request_result s file.file_sources File_new_source;
        Fifo.put propositions (s, file, kind, last_time ())
    | _ ->
        if !verbose then
          lprintf_nl () "Neighbours: error: proposed client (%s) is indirect for file %s"
	    (full_client_identifier c) (file_best_name file);
        raise Exit

  with Exit ->
(* This client might already have queried this source *)
      ()

let new_neighbour c file =
  compute_stats ();
  match c.client_kind with
    Indirect_address _ -> ()
  | _ ->

      let client_num = client_num c in
      let file_num = file_num file in
      let time = last_time () in

      try
        let nb = Hashtbl.find neighbours client_num in
        try
          let fi = List.assq file_num nb.nb_files in
          fi.fi_time <- time
        with Not_found ->
            nb.nb_files <- (file_num, file_info file time)
            :: nb.nb_files
      with _ ->
          Hashtbl.add neighbours client_num {
            nb_files  = [file_num, file_info file time];
            nb_sort = min_int;
            nb_client = c;
          }

(*
  We should try to sort files by "proximity", and then try to move sources
from one to another, maybe depending on the popularity of the file.
Before adding a client as a source for a file, we should check that
we didn't request already that file from that client.

Currently, the function does:
* Propose the 5 best Global LRU sources to all current files
* Propose the 5 best History sources to all current files
* For every file, propose 5 best LRU sources by extension, and 5 best LRU
    sources by size
  *)

let recover_downloads current_files =

(* Global LRU *)
  let list = ref [] in
  Hashtbl.iter (fun _ nb ->
      nb.nb_sort <- min_int;
      List.iter (fun (_, fi) ->
          if fi.fi_time > nb.nb_sort then nb.nb_sort <- fi.fi_time
      ) nb.nb_files;
      list := nb :: !list;
  ) neighbours;

(* Sort with last used first *)
  let glru = List.sort (fun nb1 nb2 ->
        let s1 = nb1.nb_sort in
        let s2 = nb2.nb_sort in
        if s1 > s2 then -1 else
        if s2 > s1 then 1 else 0) !list in

  let keep_glru,_ = List2.cut 333 glru in
  let best_glru,_ = List2.cut 5 glru in
(* For each file, propose the 5 first elements of the GLRU as a potential
source. *)
  List.iter (fun file ->
      List.iter (fun nb ->
          propose_source file nb.nb_client GlobalLRU) best_glru
  ) current_files;

(* History *)
  let list = ref [] in
  Hashtbl.iter (fun _ nb ->
      nb.nb_sort <- List.length nb.nb_files;
      list := nb :: !list;
  ) neighbours;
(* Sort with last used first *)
  let history = List.sort (fun nb1 nb2 ->
        let s1 = nb1.nb_sort in
        let s2 = nb2.nb_sort in
        if s1 > s2 then -1 else
        if s2 > s1 then 1 else 0) !list in

  let keep_history,_ = List2.cut 333 history in
  let best_history,_ = List2.cut 5 history in
(* For each file, propose the 5 first elements of the GLRU as a potential
source. *)
  List.iter (fun file ->
      List.iter (fun nb ->
          propose_source file nb.nb_client History) best_history
  ) current_files;

(* SplitLRU *)
  let extensions_list = ref [] in
  let sizes_list = ref [] in
  Hashtbl.iter (fun _ nb ->
      List.iter (fun (_, fi) ->
          (try
              let list = List.assq fi.fi_size !sizes_list in
              match !list with
                [] -> assert false
              | nb2 :: _ ->
                  if nb2 != nb then list := nb :: !list
            with Not_found ->
                sizes_list := (fi.fi_size, ref [nb]) :: !sizes_list
          );

          (try
              let list = List.assq fi.fi_ext !extensions_list in
              match !list with
                [] -> assert false
              | nb2 :: _ ->
                  if nb2 != nb then list := nb :: !list
            with Not_found ->
                extensions_list := (fi.fi_ext, ref [nb]) :: !extensions_list
          )

      ) nb.nb_files
  ) neighbours;

  List.iter (fun (ext, list) ->
      List.iter (fun nb ->
          nb.nb_sort <- min_int;
          List.iter (fun (_, fi) ->
              if fi.fi_ext = ext &&
                fi.fi_time > nb.nb_sort then nb.nb_sort <- fi.fi_time
          ) nb.nb_files
      ) !list;
      list := List.sort (fun nb1 nb2 ->
          let s1 = nb1.nb_sort in
          let s2 = nb2.nb_sort in
          if s1 > s2 then -1 else
          if s2 > s1 then 1 else 0) !list

  ) !extensions_list;

  List.iter (fun (size, list) ->
      List.iter (fun nb ->
          nb.nb_sort <- min_int;
          List.iter (fun (_, fi) ->
              if fi.fi_size = size &&
                fi.fi_time > nb.nb_sort then nb.nb_sort <- fi.fi_time
          ) nb.nb_files
      ) !list;
      list := List.sort (fun nb1 nb2 ->
          let s1 = nb1.nb_sort in
          let s2 = nb2.nb_sort in
          if s1 > s2 then -1 else
          if s2 > s1 then 1 else 0) !list
  ) !sizes_list;

  List.iter (fun file ->
      let fi_size = int_of_float (log (
            Int64.to_float (file_size file ++ one))) in
      let fi_ext = String.lowercase (
          Filename2.last_extension (file_best_name file)) in

      (try
          let list = List.assoc fi_size !sizes_list in
          let best_slru, _ = List2.cut 5 !list in
          List.iter (fun nb ->
              propose_source file nb.nb_client SizeLRU) best_slru
        with _ -> ());

      (try
          let list = List.assoc fi_ext !extensions_list in
          let best_slru, _ = List2.cut 5 !list in
          List.iter (fun nb ->
              propose_source file nb.nb_client ExtLRU) best_slru
        with _ -> ());
  ) current_files;


(* Sort with last used first *)
  let list_list = ref [] in
  List.iter (fun (_, list) ->
      list_list := !list :: !list_list) !extensions_list;
  List.iter (fun (_, list) ->
      list_list := !list :: !list_list) !sizes_list;

  let rec iter rem todo_list done_list keep =
    if rem = 0 then keep else
    match todo_list with
      list :: todo_tail ->
        (match list with
            [] -> iter rem todo_tail done_list keep
          | nb :: tail ->
              iter (rem-1) todo_tail (tail :: done_list) (nb :: keep))
    | [] ->
        match done_list with
          [] -> keep
        | _ -> iter rem done_list [] keep
  in
  let keep_slru = iter 333 !list_list [] [] in

(* Clean the table, only keep at most 999 clients in this table,
i.e. the table can only retain at most 1 MB of data after every call
  to this function. *)
  Hashtbl.clear neighbours;

  List.iter (fun list ->
      List.iter (fun nb ->
          let cnum = client_num nb.nb_client in
          if not (Hashtbl.mem neighbours cnum) then
            Hashtbl.add neighbours cnum nb
      ) list)
  [keep_glru; keep_history; keep_slru]
