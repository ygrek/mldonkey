(* Copyright 2001, 2002 Simon, INRIA *)
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

New idea: what about trying to connect anyway if not all the slots
where tried ? We could reconnect more frequently to bad sources if we
have time to do it.

*)

open Printf2
open Options
open CommonOptions
open CommonGlobals
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals

let verbose_sources = verbose_src_manager
let source_counter = ref 0
        
  
let outside_queue = ref (Intmap.empty: client Intmap.t)
let indirect_connections = ref 0
  
  
module H = Weak2.Make(struct
      type t = source
      let hash s = Hashtbl.hash s.source_addr
      
      let equal x y = x.source_addr = y.source_addr
    end)

let sources = H.create 13557
  
let dummy_source = {
    source_num = 0;
    source_addr = (Ip.null, 0);
    source_client = SourceLastConnection (0, 0, 0);
    source_files = [];
    source_overnet = false;
    source_score = 0;
    source_age = 0;
    source_in_queues = [];
  }

let string_of_result r =
  match r with
  | File_possible -> "File_possible"
  | File_not_found -> "File_not_found"
  | File_expected -> "File_expected"
  | File_new_source -> "File_new_source"
  | File_found -> "File_found"
  | File_chunk -> "File_chunk"
  | File_upload -> "File_upload"
    
let rec iter_has_request rs file =
  match rs with
    [] -> raise Not_found
  | r :: tail ->
      if r.request_file == file then r else
        iter_has_request tail file
  
let has_client_request c file =
  try
    ignore (iter_has_request c.client_files file); true
  with _ -> false
  
let find_client_request c file = iter_has_request c.client_files file
    
let has_source_request s file =
  try
    ignore (iter_has_request s.source_files file); true
  with _ -> false
  
let find_source_request s file = 
  let files = match s.source_client with
      SourceLastConnection _ -> s.source_files 
    | SourceClient c -> c.client_files
  in
  iter_has_request files file

let add_client_request c file time result =
  try
    let r = find_client_request c file in
    r.request_result <- result;
    r.request_time <- time;
  with _ ->
      let r = {
          request_file = file;
          request_time = time;
          request_result = result;
        } in
      c.client_files <- r :: c.client_files

let add_request s file time result =
  let r = {
      request_file = file;
      request_time = time;
      request_result = result;
    } in
  match s.source_client with
    SourceLastConnection _ -> s.source_files <- r :: s.source_files
  | SourceClient c -> c.client_files <- r :: c.client_files
      
let add_source_request s file time result =
  try
    let r = find_source_request s file in
    r.request_result <- result;
    r.request_time <- time;
  with _ ->
      add_request s file time result
      
let set_request_result c file rs =
  try
    List.iter (fun r ->
        if r.request_file == file then begin

            (*
            (match r.request_result, rs with
              | _, (File_not_found | File_possible | File_expected ) -> ()
              | File_possible, _ ->
                  lprintf "adding client to queue"; lprint_newline ();
                  c.client_from_queues <- file :: c.client_from_queues
              | _, _ -> ()
            ); *)
            (r.request_result <- rs; raise Exit)
          end
    ) c.client_files;
    let r = {
        request_file = file;
        request_time = last_time ();
        request_result = rs;
      } in
    c.client_files <- r :: c.client_files;
    (*
    match rs with
    | File_not_found | File_possible | File_expected  -> ()
    |  _ ->
        lprintf "adding client to queue"; lprint_newline ()
        (*
        c.client_from_queues <- file :: c.client_from_queues
*)
*)
    
  with Exit -> ()

let really_query_file c file r =
  if r.request_time + !!min_reask_delay <= last_time () then 
    match r.request_result with
      File_not_found when last_time () - r.request_time > 3600 -> ()
(* one hour ago, it did not have the file.... *)
    | _ ->
        c.client_requests_sent <- c.client_requests_sent + 1;
        DonkeyProtoCom.direct_client_send c (
          let module M = DonkeyProtoClient in
          M.QueryFileReq file.file_md4);      
        
        r.request_time <- last_time ();
        match r.request_result with
          File_possible -> ()
        | _ -> r.request_result <- File_expected
  else begin
(*
        lprintf "%d: Too Early for this request %s" 
          (client_num c) 
        (file_best_name file);
        lprint_newline ();
        lprintf "   Previous: %d seconds ago" (last_time () - r.request_time); 
lprint_newline ();
  *) ()
    end
    
let query_file c file =
  if file_state file = FileDownloading then
    let r = 
      try
        find_client_request c file
      with _ -> 
          let r = {
              request_file = file;
              request_time = 0;
              request_result = File_possible;
            } in
          c.client_files <- r :: c.client_files;
          r
    in
    really_query_file c file r
      
let add_file_location file c =
  if not (Intmap.mem (client_num c) file.file_locations) then begin
      file.file_locations <- Intmap.add (client_num c) c file.file_locations;
      CommonFile.file_add_source (CommonFile.as_file file.file_file) 
      (CommonClient.as_client c.client_client);
      do_if_connected c.client_sock (fun sock ->
          match client_state c with
            Connected_downloading _
          | Connected _ ->
              query_file c file
          | _ -> ())
    end
    
let remove_file_location file c = 
  CommonFile.file_remove_source (CommonFile.as_file file.file_file)
    (CommonClient.as_client c.client_client);
  file.file_locations <- Intmap.remove (client_num c) file.file_locations

let purge_requests files =
  let rec iter downloading files all_files =
    match files with
      [] -> all_files, downloading
    | r :: tail ->
        match file_state r.request_file with
        | FileDownloading -> iter true tail (r :: all_files)
        | FileAborted _
        | FileQueued
        | FilePaused -> iter downloading tail (r :: all_files)
        | FileNew
        | FileShared
        | FileCancelled
        | FileDownloaded -> iter downloading tail all_files
  in
  iter false files []
      
(*****

Define some interesting queues: 
   - FIFO
   - LIFO
   - Oldest Connection First
   - Earliest Connection First

******)
  

(* Connect to a client. Test before if the connection is really useful. *)
let useful_client source_of_client reconnect_client c = 
  let v =
(*  lprintf "Test %d: " (client_num c);  *)
  if !verbose_sources then begin
      lprintf "Testing source"; lprint_newline ();
    end;
  let (files, downloading) = purge_requests c.client_files in
  c.client_files <- files;
  try
(*
      if not downloading then (lprintf "Not downloading"; 
        lprint_newline ()); *)
    if downloading || 
      (client_browsed_tag land client_type c <> 0 &&
        c.client_next_view_files < last_time ()) then
      (
        if !verbose_sources then begin
            lprintf "************** Connect to source"; lprint_newline ();
            (match c.client_kind with Indirect_location _ -> 
                  lprintf "Indirect localtion ?"; lprint_newline ();
              | _ -> ());
          end;
(*        lprintf "connect"; *)
        reconnect_client c; 
        match  client_state c with
        | NotConnected _ ->
(*            lprintf " failed"; *)
            if !verbose_sources then begin
                lprintf "--------- Connection to source failed"; lprint_newline ();
              end;
            source_of_client c; false
        | _ ->
(*            lprintf " done"; *)
            outside_queue := Intmap.add  (client_num c) c !outside_queue;
            true
      )
    else raise Not_found
    with _ ->
(*         lprintf " exception"; *)
        source_of_client c;
        false
  in
(*  lprint_newline ();  *)
  v
  
let rank_level rank =
  if rank = 0 then 0 else
  if rank = 1 then 1 else
  if rank < 10 then 2 else
  if rank < 50 then 3 else
  if rank < 100 then 4 else
  if rank < 200 then 5 else 
  if rank < 500 then 6 else
  if rank < 1000 then 7 else
  if rank < 1500 then 8 else 9

let stats_ranks = ref (Array.create 10 0)
    
let keep_client c =
  client_browsed_tag land client_type c <> 0 ||
  (                
    List.exists (fun r -> 
        if r.request_result >= File_chunk then
          let level = rank_level c.client_rank in
          !stats_ranks.(level)  <- !stats_ranks.(level) + 1;
          true
        else false
    )
    c.client_files &&
    c.client_rank < 300
  )

  
(**************************************************************


         SPECIFIC TO THIS SOURCE MANAGEMENT SYSTEM


***************************************************************)

(*
Another method: two queues, one for sources that can be connected immediatly,
 and one for sources that cannot be connected immediatly.

In the two first queues, sources have a score, and
the one with the highest score is taken to be connected. 

Every minute, sources that can be connected are removed from the fifo, 
and gathered with the other sources. For each source, a score function is
computed and the source is added again to the client/sources queues.
*)


let stats_connect_good_clients = ref 0
let stats_connect_new_sources = ref 0
let stats_connect_good_sources = ref 0
let stats_connect_old_sources = ref 0
let stats_new_sources = ref 0

let stats_saved_connect_good_clients = ref 0
let stats_saved_connect_new_sources = ref 0
let stats_saved_connect_good_sources = ref 0
let stats_saved_connect_old_sources = ref 0    
let stats_saved_new_sources = ref 0

let stats_files = ref Intmap.empty
let stats_saved_files = ref (Intmap.empty : int ref Intmap.t)
let stats_saved_files_size = ref 1

let stats_saved_ranks = ref (Array.create 10 0)

let stats_remove_too_old_sources = ref 0
let stats_remove_old_sources = ref 0
let stats_remove_useless_sources = ref 0
let stats_sources = ref 0


let rec stats_register_files list =
  match list with
    [] -> ()
  | r :: tail ->
      (match r.request_result with
          File_not_found | File_possible -> ()
        | _ ->
            let file_num = file_num r.request_file in
            try
              incr (Intmap.find file_num !stats_files)
            with _ ->
                stats_files := Intmap.add  file_num (ref 1) !stats_files
      );
      stats_register_files tail

let half_day = 12 * 3600

exception SourceTooOld
  
  

let create_source new_score source_age addr = 
  let ip, port = addr in
  if !verbose_sources then begin
      lprintf "queue_new_source %s:%d" (Ip.to_string ip) port; 
      lprint_newline ();
    end;
  try
    let finder =  { dummy_source with source_addr = addr } in
    let s = H.find sources finder in
    
    incr stats_new_sources;
    s
  
  with _ ->
      let s = { dummy_source with
          source_num = (incr source_counter;!source_counter);
          source_addr = addr;
          source_age = source_age;
          source_client = SourceLastConnection (
            new_score, source_age, CommonClient.book_client_num ());
          source_files = [];
        }  in
      H.add sources s;
      incr stats_sources;
      if !verbose_sources then begin
          lprintf "Source %d added" s.source_num; lprint_newline ();
        end;
      s


let ask_indirect_connection_by_udp ip port id =
  let client_ip = client_ip None in
  if ip_reachable client_ip then
    let module Q = DonkeyProtoUdp.QueryCallUdp in
      
    DonkeyProtoCom.udp_send (get_udp_sock ())
    ip (port+4)
    (DonkeyProtoUdp.QueryCallUdpReq {
        Q.ip = client_ip;
        Q.port = !client_port;
        Q.id = id;
      })
  