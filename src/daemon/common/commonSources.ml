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

This is a copy of DonkeySources3.ml, such that all the dependencies
have been made explicit in the functor. The hope is to be able to
use this source management for all networks.
*)


open Queues
open Printf2
open Md4
open Options
open BasicSocket

open CommonOptions
open CommonTypes

module Make(M:sig
      
      type client = {
          mutable client_files : file_request list;          
          mutable client_requests_sent: int;
          mutable client_sock : TcpBufferedSocket.t option;
          mutable client_next_view_files :  int;
          mutable client_kind : location_kind;
          mutable client_rank : int;
          mutable client_source : source option;
          mutable client_indirect_address : (Ip.t * Ip.t * int) option;
          mutable client_score : int;
          mutable client_connected : bool;
          mutable client_next_queue : int;
          mutable client_brand : int;
          mutable client_mod_brand : int;
          mutable client_name : string;
        }
      
      and source = {
          source_num : int;
          source_addr : Ip.t * int;
          mutable source_client: client_kind;     
(* This field is not kept up-to-date when source_client = SourceClient c,
  change c.client_source_for *)
          mutable source_files : file_request list;
          mutable source_brand : int;
          mutable source_score : int;
          mutable source_age : int;
          mutable source_in_queues : file list;
        }      
      
      and file_request = {
          request_file : file;
          mutable request_time : int;
          mutable  request_result : request_result;
        }
      
      and request_result = 
      | File_possible (* we asked, but didn't know *)
      | File_not_found (* we asked, the file is not there *)
      | File_expected (* we asked, because it was announced *)
      | File_new_source (* we never asked *)
      | File_found    (* the file was found *)
      | File_chunk    (* the file has chunks we want *)
      | File_upload   (* we uploaded from this client *)
      
      and client_kind = 
        SourceClient of client
      | SourceLastConnection of 
        int *
        int * (* last connection attempt *)
        int   (* booked client num *)
      
      and file = {
          mutable file_locations : client Intmap.t; 
          mutable file_clients : (client * int) Fifo.t;
          mutable file_sources : source Queue.t array;
        }
      
      val query_client_for_file : client -> file -> unit
        
(*
                DonkeyProtoCom.direct_client_send c (
          let module M = DonkeyProtoClient in
          M.QueryFileReq file.file_md4);      
        DonkeyProtoCom.direct_client_send c (
          let module M = DonkeyProtoClient in
          M.QueryChunksReq file.file_md4);      
*)
        
      val query_indirect_client_from_server : Ip.t -> int -> Ip.t -> unit
        (*
          try
            
            
            let s = find_server ip port in
            match s.server_sock with
              None -> raise Not_found
            | Some sock ->
                DonkeyProtoCom.direct_server_send sock (
                  let module M = DonkeyProtoServer in
                  let module C = M.QueryID in
                  M.QueryIDReq id
                );
          with _ ->
              
              let module Q = DonkeyProtoUdp.QueryCallUdp in
              
              
              DonkeyProtoCom.udp_send (get_udp_sock ())
              ip (port+4)
              (DonkeyProtoUdp.QueryCallUdpReq {
                  Q.ip = client_ip None;
                  Q.port = !client_port;
                  Q.id = id;
                })        
            *)
  
      val file_num : file -> int
      val file_state : file -> CommonTypes.file_state
      val file_add_source : file -> client -> unit
      val file_best_name : file -> string
        
      val client_num : client -> int
      val client_state : client -> CommonTypes.host_state        
      val client_type : client -> CommonTypes.client_type

      val current_files : unit -> file list

      val new_client_with_num : CommonTypes.location_kind -> int -> client
        
      val good_client_rank : int Options.option_record
      val max_sources_per_file : int Options.option_record
      val max_clients_per_second : int Options.option_record
        
        
    end) = struct


open M    
    
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
    source_brand = 0;
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
        
        M.query_client_for_file c file;
        
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
      file_add_source file c;
      match c.client_sock, client_state c with
        Some sock, (Connected_downloading
          | Connected _) ->
          query_file c file
      | _ -> ()
    end
    
let remove_file_location file c = 
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
      (client_type c <> NormalClient &&
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
  client_type c <> NormalClient ||
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

  
let stats_remove_bad_sources_of_popular_file = ref 0
  
let add_new_source new_score source_age addr = 
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


let new_sources_queue = 0
let good_sources_queue = 1
let good_saved_sources_queue = 2
let old_saved_sources_queue = 3
let old_sources1_queue = 4
let old_sources2_queue = 5
let old_sources3_queue = 6

let indirect_fifo = Fifo.create ()
  
let queue_name = [|
    "new_sources";
    "good_sources";
    "good_saved_sources";
    "old_saved_sources";
    "old_sources1";
    "old_sources2";
    "old_sources3";
  |]
  
let queue_period = [|
    0;
    600;
    600;
    0;
    600;
    600;
    1200;
  |]

let nqueues = Array.length queue_name

let need_new_sources file = 
  Fifo.length file.file_clients + 
  Queue.length file.file_sources.(new_sources_queue) < 200

let add_source_request s file time result =
  try
    let r = find_source_request s file in
(*
    r.request_result <- result;
r.request_time <- time;
*)
    if !verbose_location then lprint_char 
        (match r.request_result with
          File_possible -> '?'
        | File_not_found -> '-'
        | File_expected -> '!'
        | _ -> '_');
    ()
  with _ ->
      add_request s file time result;
      Queue.put file.file_sources.(match result with
          File_new_source -> 
            if !verbose_location then lprint_char '!';
            new_sources_queue
        | _ -> 
            if last_time () - time < !!min_reask_delay then
              good_saved_sources_queue
            else 
              old_saved_sources_queue) (time, s);
      if List.memq file s.source_in_queues then begin
          if !verbose_sources then begin
              lprintf "Source is already queued for this file"; 
              lprint_newline ();
            end
        end else
      s.source_in_queues <- file :: s.source_in_queues

let old_source old_source_score source_age addr file = 
  (*
  lprintf "New source connected %d seconds ago" (last_time () - source_age);
  lprint_newline ();
*)
  
  let s = add_new_source old_source_score source_age addr in
  add_source_request s file source_age File_expected;
  s

let new_source  addr file = 
  if !verbose_location then lprint_char 'n'; 
  let last_conn = last_time () - !!min_reask_delay in
  let s = add_new_source 0 last_conn addr in
  add_source_request s file last_conn File_new_source;
  s

let iter f =
  Intmap.iter (fun _ c ->
      match c.client_source with
        None -> () | Some s -> f s)
  !outside_queue;
  List.iter (fun file ->
      Fifo.iter (fun (c,_) ->
          match c.client_source with
            None -> () | Some s -> f s
      ) file.file_clients;
      Array.iter (fun ss -> Queue.iter f ss) file.file_sources;
  ) (current_files ())

(* If the file is popular, and we cannot connect, just drop the source ! *)
let popular_file file =
  (Fifo.length file.file_clients) +
  (Queue.length file.file_sources.(new_sources_queue)) +
  (Queue.length file.file_sources.(old_sources1_queue)) 
  > 1000
  

let source_of_client c = 
  outside_queue := Intmap.remove (client_num c) !outside_queue;      
  
  if !verbose_sources then begin
      lprintf "source_of_client %d" (client_num c); lprint_newline ();
    end;
  
  match c.client_source with
    None -> 
(* This client is an indirect connection. Can't do anything with it. *)
      
      if !verbose_sources then begin
          lprintf "%d --> indirect" (client_num c); lprint_newline ();
        end;
      
      begin
        match c.client_indirect_address with
          None -> ()
        | Some addr ->
            let keep_source = ref false in
            List.iter (fun r ->
                match r.request_result with
                  File_chunk | File_found | File_upload ->
                    keep_source := true
                | _ -> ()
            ) c.client_files;
            
            if !keep_source then Fifo.put indirect_fifo (addr, last_time ())
      
      end;
      
      List.iter (fun r -> 
          remove_file_location r.request_file c) c.client_files;
      c.client_files <- [];
      raise Exit
  
  | Some s ->
      
      let ip, port = s.source_addr in
      if !verbose_sources then begin
          lprintf "Old source %s:%d score: %d" (Ip.to_string ip) port
            c.client_score; 
          lprint_newline ();
        end;
      (*
      lprintf "Client %d before purge:" (client_num c);
      List.iter (fun r ->
          lprint_char (
            match r.request_result with
            | File_chunk ->      'C'
            | File_upload ->     'U'
            | File_not_found ->  '-'
            | File_found ->      '+'
            | File_possible ->   '?'
            | File_expected ->   '!'
            | File_new_source -> 'n'
          )) c.client_files;      
      lprint_newline (); *)
      let (files, downloading) = purge_requests c.client_files in
      (*
      lprintf "Client %d after purge:" (client_num c);
      List.iter (fun r ->
          lprint_char (
            match r.request_result with
            | File_chunk ->      'C'
            | File_upload ->     'U'
            | File_not_found ->  '-'
            | File_found ->      '+'
            | File_possible ->   '?'
            | File_expected ->   '!'
            | File_new_source -> 'n'
          )) files;      
      lprint_newline ();  *)
      c.client_files <- files;
      try      
        let keep_client = ref false in
        List.iter (fun r ->
            try
              let file = r.request_file in
(*              let r = find_client_request c file in *)
              (*
              lprint_char (
                match r.request_result with
                | File_chunk ->      'C'
                | File_upload ->     'U'
                | File_not_found ->  '-'
                | File_found ->      '+'
                | File_possible ->   '?'
                | File_expected ->   '!'
                | File_new_source -> 'n'
              ); *)
              match r.request_result with
                File_possible | File_not_found -> ()
              | File_new_source | File_expected ->
                  let fifo = 
                    if c.client_score >= -10 then 
                      old_sources1_queue
                    else 
                    if popular_file file then begin
                        incr stats_remove_bad_sources_of_popular_file;
                        remove_file_location file c;
                        raise Exit
                      end else
                    if c.client_score >= -20 then
                      old_sources2_queue
                    else
                    if c.client_score >= -40 then
                      old_sources3_queue                    
                    else 
                      begin
(*                       c.client_files <- List2.removeq r c.client_files; *)
                        remove_file_location file c;
                        raise Exit
                      end
                  in
                  if not (List.memq file s.source_in_queues) then begin
                      Queue.put 
                        file.file_sources.(fifo) (last_time() , s);
                      s.source_in_queues <- file :: s.source_in_queues
                    end
              
              | File_found ->
                  if not (List.memq file s.source_in_queues) then begin
                      Queue.put 
                        file.file_sources.(good_sources_queue)
                      (last_time() , s);
                      s.source_in_queues <- file :: s.source_in_queues
                    end
                                  
              | File_chunk when c.client_rank > !!good_client_rank ->
                  if not (List.memq file s.source_in_queues) then begin
                      Queue.put 
                        file.file_sources.(good_sources_queue)
                      (last_time() , s);
                      s.source_in_queues <- file :: s.source_in_queues
                    end
                    
              | File_chunk ->
                  keep_client := true;
                  if !verbose_sources then begin
                      lprintf "%d --> kept (source)" (client_num c); lprint_newline ();
                    end;
                let level = rank_level c.client_rank in
                !stats_ranks.(level)  <- !stats_ranks.(level) + 1;
                  if not (List.memq file s.source_in_queues) then begin
                      Fifo.put file.file_clients (c, last_time ());
                      s.source_in_queues <- file :: s.source_in_queues
                    end

              | File_upload ->
                  keep_client := true;
                  if !verbose_sources then begin
                      lprintf "%d --> kept (uploader)" (client_num c); lprint_newline ();
                    end;
		  let level = rank_level c.client_rank in
        	  !stats_ranks.(level)  <- !stats_ranks.(level) + 1;
                  if not (List.memq file s.source_in_queues) then begin
                      Fifo.put file.file_clients (c, last_time ());
                      s.source_in_queues <- file :: s.source_in_queues
                    end
            
            with 
              _ -> (* lprint_char 'e' *) ()
        ) c.client_files;
  (*      lprint_newline (); *)
        if not !keep_client then begin
            let basic_score = c.client_score in
            List.iter (fun r -> remove_file_location r.request_file c) 
            c.client_files;
(*
            if !verbose_sources then begin
                lprintf "Set SourceLastConnection for source %d" 
                  s.source_num; 
                lprint_newline ();
              end; *)
            s.source_client <- SourceLastConnection (
              basic_score, last_time (), client_num c);
            s.source_files <- c.client_files;
            c.client_files <- [];            
          end;
        if s.source_in_queues = [] then begin
            incr stats_remove_useless_sources;
            raise Exit
          end;
        
      with _ ->          
          if !verbose_sources then  begin
            lprintf "%d --> removed (%d):" (client_num c) c.client_score; 
            if not c.client_connected then 
              lprintf "(ind) ";
            List.iter (fun r ->
                lprint_char (
                  match r.request_result with
                  | File_chunk ->      'C'
                  | File_upload ->     'U'
                  | File_not_found ->  '-'
                  | File_found ->      '+'
                  | File_possible ->   '?'
                  | File_expected ->   '!'
                  | File_new_source -> 'n'
                )) c.client_files;      
            
            lprint_newline ();
            
            end;
          H.remove sources s;
          s.source_files <- [];
          List.iter (fun r -> 
              remove_file_location r.request_file c) c.client_files;
          c.client_files <- []
          
let client_connected c =
  c.client_score <- 0;
  match c.client_source with None -> () | Some s ->
      s.source_age <- last_time ()

let clean_file_sources file nsources =
  
  for i = Array.length file.file_sources -1 downto 0 do
    try
      while !nsources > !!max_sources_per_file do
        let _,s = Queue.take file.file_sources.(i) in
        s.source_in_queues <- List2.removeq file s.source_in_queues;
        decr nsources;
        match s.source_in_queues, s.source_client with
          [] , SourceLastConnection _ -> H.remove sources s
        | _ -> ()
      done
    with _ -> ()
        
  done
      
let recompute_ready_sources f =

(* for each file, try to apply the max_sources_per_file option *)
  List.iter (fun file ->
      let nsources = ref (Fifo.length file.file_clients) in
      Array.iter (fun q -> nsources := !nsources + Queue.length q)
      file.file_sources;
            
      if !nsources > !!max_sources_per_file then 
        clean_file_sources file nsources

  ) (current_files ());
  
(* query all connected clients *)
  Intmap.iter (fun _ c ->
      match c.client_sock with
        None -> ()
      | Some sock ->  
          match client_state c with
            Connected _ | Connected_downloading ->
              List.iter (fun r ->
                  let file = r.request_file in
                  if file_state file = FileDownloading then
                    match r.request_result with
                      File_not_found | File_possible -> ()
                    | _ -> really_query_file c file r
              ) c.client_files
          | _ -> ()
  ) !outside_queue
  
  
(* Change a source structure into a client structure before attempting
  a connection. *)
let client_of_source reconnect_client s file basic_score client_num = 
  
  if !verbose_sources then begin
      lprintf "client_of_source %d" s.source_num; lprint_newline ();
    end;
  let (files, downloading) = purge_requests s.source_files in
  if !verbose_sources then begin
      lprintf "Source for %d files" (List.length files); 
      lprint_newline ();
    end;
  let (ip, port) = s.source_addr in
  let c = new_client_with_num (Known_location (ip,port))
    client_num in
  c.client_next_queue <- 0;
  
  c.client_brand <- s.source_brand;
  (match c.client_source with
      Some ss when s != ss -> 
        if !verbose_sources then begin
            lprintf "Client already has a source!"; lprint_newline ();
          end;
    |  _ -> ());
  c.client_source <- Some s;
  
  s.source_client <- SourceClient c;

(* This will be used after the connection to know where to put this client *)
  c.client_score <- basic_score - 10;
  
  c.client_files <- s.source_files;
  let new_source = ref false in
  let good_source = ref false in
  List.iter (fun r ->
      if r.request_result > File_not_found then begin
          add_file_location r.request_file c;
          match r.request_result with
          | File_new_source -> new_source := true
          | File_chunk | File_upload -> good_source := true
          | _ -> ()
        end;
  ) c.client_files;
  
  if !good_source then 
    incr stats_connect_good_sources
  else
  if !new_source then
    incr stats_connect_new_sources
  else
    incr stats_connect_old_sources;
  
  useful_client source_of_client reconnect_client c

let source_files = ref []

let check_source_from_file reconnect_client file =
  
  try
    let rec iter_client () =
      let (c, time) = Fifo.head file.file_clients in
      let wait_for = time + !!min_reask_delay - last_time ()  in
      if wait_for < 0 then
        let _ = Fifo.take file.file_clients in
        
        begin
          match c.client_source with
            None -> lprintf "ERROR: Client source can not be NOne\n"
          | Some s ->
              if not (List.memq file s.source_in_queues) then begin
                  lprintf "ERROR: client should be in file queue (1)\n";
                  match s.source_client with
                    SourceLastConnection _ -> 
                      lprintf "  ERROR: client source has last conn\n"
                  | SourceClient cc ->
                      if c !=cc then
                        lprintf "  ERROR: client source client is different\n"
                end;
              
              s.source_in_queues <- List2.removeq file s.source_in_queues
        end;
        
        incr stats_connect_good_clients;
        stats_register_files c.client_files;
        
        if !verbose_sources then begin
            lprintf "Source: Good Client of %s" (file_best_name file); 
            lprint_newline ();
          end;
        
        (useful_client source_of_client reconnect_client c) || (iter_client ())
      
      else begin (*
          lprintf "Client can not be connected: %d" wait_for;
          lprint_newline (); *)
          raise Fifo.Empty
        end
    in
    iter_client ()
  with Fifo.Empty ->
      let rec iter_source i =
(*        lprintf "iter_source %d" i; lprint_newline (); *)
        if i < nqueues then
          try
            
            let _, s = Queue.head file.file_sources.(i) in
(*
lprintf "Checking source from queue[%s]" queue_name.(i); lprint_newline (); 
  *)
            let ip, port = s.source_addr in
            if !verbose_sources then begin
                lprintf "One source %s:%d from queue[%s]" (Ip.to_string ip) port queue_name.(i); lprint_newline ();
                
              end;
            if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
(*                
lprintf "ERROR: Source invalidated"; lprint_newline ();
*)
                let _,s = Queue.take file.file_sources.(i) in
                                
                if not (List.memq file s.source_in_queues) then begin
                    lprintf "ERROR: client should be in file queue (2)";
                    lprint_newline ();
                  end;
                
                s.source_in_queues <- List2.removeq file s.source_in_queues;

                raise Not_found
              end else
            match s.source_client with
            | SourceLastConnection (basic_score, time, client_num) ->
                
                let wait_for = time + queue_period.(i) - last_time () in
                if wait_for < 0 then
(* This source is good, connect to it !!! *)
                  let _, s = Queue.take file.file_sources.(i) in
                  
                  
                  if not (List.memq file s.source_in_queues) then begin
                      lprintf "ERROR: client should be in file queue (3)";
                      lprint_newline ();
                    end;
                  
                  s.source_in_queues <- List2.removeq file s.source_in_queues;
                  
                  if !verbose_sources then begin
                      lprintf "Source could be connected (last %d)" 
                        (last_time () - time); lprint_newline ();
                    end;
                  
                  
                  if i = new_sources_queue then 
                    incr stats_connect_new_sources
                  else
                  if i = good_saved_sources_queue || i = old_saved_sources_queue
                  then
                    incr stats_connect_good_sources
                  else
                    incr stats_connect_old_sources;
                  
                  if !verbose_sources then  begin
                      lprintf "Source %d: queue[%s] of file %s (last conn %d)" client_num queue_name.(i)
                      (file_best_name file)  (last_time () - time)
                      ; lprint_newline (); 
                    end;
                  
                  if client_of_source reconnect_client s file basic_score client_num then
                    true
                  else raise Not_found
                else begin
(* Too early to connect to this source, move to the next queue  *)
                    if !verbose_sources then begin
                        lprintf "Too early for this source %d (last %d)" wait_for (last_time () - time); lprint_newline ();
                      end;
                    raise Fifo.Empty
                  end
            
            | SourceClient c -> 
(* This source is already connected, remove it immediatly, and retry *)
                let _, s = Queue.take file.file_sources.(i) in
                if not (List.memq file s.source_in_queues) then begin
                    lprintf "ERROR: client should be in file queue (4)";
                    lprint_newline ();
                  end;
                
                s.source_in_queues <- List2.removeq file s.source_in_queues;
                query_file c file;
                
                
                raise Not_found
            
          with
          | Fifo.Empty -> iter_source (i+1)
          | Not_found -> iter_source i
          | e -> 
              lprintf "Exception %s" (Printexc2.to_string e); 
              lprint_newline ();
              iter_source i
        else
          false
      in 
      iter_source 0

      
let print_sources buf =
  
  let noutside_queue = Intmap.length !outside_queue in
  Printf.bprintf buf "  Outside of queues: %d sources\n" noutside_queue;
  Printf.bprintf buf "  Indirect Connections: %d \n" !indirect_connections;
  Printf.bprintf buf "  Total Connections: %d \n" (nb_sockets ());

  Printf.bprintf buf "\n  Indirect Sources waiting: %d\n" (Fifo.length indirect_fifo); 
  let positive_sources = ref 0 in
  let negative_sources = ref 0 in
  let nchunks = ref 0 in
  let nupload = ref 0 in
  let nfound = ref 0 in
  let nnotfound = ref 0 in
  let scores = Array.create 10 0 in
  iter (fun s ->
      let score = match s.source_client with 
          SourceClient c -> 
            s.source_files <- c.client_files; c.client_score
        | SourceLastConnection (score,_,_) -> score
      in
      let score = - score in
      let score = 
        if score < 0 then 0 else 
        if score > 99 then 99 else score in
      scores.(score/10) <- scores.(score/10) +1;
      
      List.iter (fun r ->
          match r.request_result with
            File_not_found -> incr nnotfound
          | File_found -> incr nfound
          | File_chunk -> incr nchunks
          | File_upload -> incr nupload
          | _ -> ()
      ) s.source_files;
      
  );
  
  Printf.bprintf buf "Scores: ";
  for i = 0 to 9 do
    Printf.bprintf buf "%d " scores.(i)
  done;
  Printf.bprintf buf "\n";
  Printf.bprintf buf "NotFound/Found/Chunk/Upload: %d/%d/%d/%d\n\n"
    !nnotfound !nfound !nchunks !nupload;
  Printf.bprintf buf "Ranks: ";
  for i = 0 to 9 do
    Printf.bprintf buf " %d[%d]" !stats_ranks.(i) !stats_saved_ranks.(i)
  done;
  Printf.bprintf buf "\n";
  
  Printf.bprintf buf "Removed Sources (on %d): useless %d/old %d/too old %d/popular %d\n"
    !stats_sources
    !stats_remove_useless_sources !stats_remove_old_sources
    !stats_remove_too_old_sources
    !stats_remove_bad_sources_of_popular_file
  ;
  
  
  Printf.bprintf buf "  Connected last %d seconds[previous 10 minutes]: %d[%d]\n"
    ((last_time () - start_time) mod 600)
  (!stats_connect_good_clients + !stats_connect_good_sources + !stats_connect_old_sources + !stats_connect_new_sources)
  (!stats_saved_connect_good_clients + !stats_saved_connect_good_sources + !stats_saved_connect_old_sources + !stats_saved_connect_new_sources)
  
  ;
  Printf.bprintf buf "     Good clients: %d[%d]\n" 
    !stats_connect_good_clients
    !stats_saved_connect_good_clients;
  Printf.bprintf buf "     Saved sources: %d[%d]\n" 
    !stats_connect_good_sources
    !stats_saved_connect_good_sources;
  Printf.bprintf buf "     New sources: %d/%d[%d/%d]\n" 
    !stats_connect_new_sources !stats_new_sources
    !stats_saved_connect_new_sources !stats_saved_new_sources;
  Printf.bprintf buf "     Old sources: %d[%d]\n" 
    !stats_connect_old_sources
    !stats_saved_connect_old_sources;

  let nsources = ref 0 in
  let per_queue = Array.create (nqueues+1) 0 in
  Printf.bprintf buf "\nBy files:\n";
  List.iter (fun file ->

      if file_state file = FileDownloading then begin
          Printf.bprintf buf "  %-60s:\n" (file_best_name file);

          let nclients = Fifo.length file.file_clients in
          nsources := !nsources + nclients;
          per_queue.(0) <- per_queue.(0) + nclients;
          Printf.bprintf buf "     Queue[Clients]: %d clients (next %s)\n"
            nclients
          (try
              let (c,time) = Fifo.head file.file_clients in
              let wait = time + !!min_reask_delay - last_time () in
              if wait > 0 then Printf.sprintf "%d seconds" wait else
                "READY"
            with _ -> "none");
          
          Array.iteri (fun i q ->
              let queue_size = Queue.length q in
              per_queue.(i+1) <- per_queue.(i+1) + queue_size;
              nsources := !nsources + queue_size;
              Printf.bprintf buf "     Queue[%s] : %d sources (next %s)\n" 
                queue_name.(i) queue_size
              (try
                  let _,s = Queue.head q in
                  match s.source_client with
                    SourceLastConnection (_, last_conn, _) ->
                      let wait =  (last_conn + queue_period.(i)) - last_time ()
                      in
                      if wait > 0 then
                        Printf.sprintf "%d seconds (last ok %d, last try %d)"
                          wait (last_time () - s.source_age)
                        (last_time () - last_conn)
                      else
                        Printf.sprintf "READY (last ok %d, last try %d)"
                          (last_time () - s.source_age)
                        (last_time () - last_conn)
                      
                  | _ -> "connected"
                with _ -> "none"   
              )
              
              ;
          ) file.file_sources;
          
        end;
  
  ) (current_files ());

  Printf.bprintf buf "\nPer queue:\n";
  Printf.bprintf buf "   Queue[Clients]: %d clients\n" per_queue.(0);
  for i = 0 to nqueues - 1 do
    Printf.bprintf buf "   Queue[%s]: %d sources\n" 
      queue_name.(i)
    per_queue.(i+1);
  done;
  
  Printf.bprintf buf "\nTotal number of sources:%d\n" 
    (noutside_queue + !nsources)

let print_sources_html file buf =

  let nsources = ref 0 in
  let per_queue = Array.create (nqueues+1) 0 in

      if file_state file = FileDownloading then begin

          let nclients = Fifo.length file.file_clients in
          nsources := !nsources + nclients;
          per_queue.(0) <- per_queue.(0) + nclients;
          
              if nclients > 0 then begin
                  Printf.bprintf buf "\\<table width=\\\"100%%\\\" class=\\\"sources\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>
\\<td title=\\\"Queue (IP - Age|Overnet T/F|Score) (Mouseover=Name)\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Queue[Clients]: %d clients (next %s)\\</td\\>
\\<tr class=\\\"dl-2\\\"\\>\\<td class=\\\"dl-2\\\"\\>"
            nclients
          (try
              let (c,time) = Fifo.head file.file_clients in
              let wait = time + !!min_reask_delay - last_time () in
              if wait > 0 then Printf.sprintf "%d seconds" wait else
                "READY"
            with _ -> "none");

              let lasttime = (last_time()) in
              let counter = ref 0 in
              let dlclass = ref "" in
                Fifo.iter (fun (c,age) ->

                       let last = ((lasttime - age) / 60) in
                      if !counter mod 2 = 0 then dlclass := "dl-1" else dlclass := "dl-2";
                      incr counter;
                  Printf.bprintf buf "\\<div style=\\\"float: left;\\\"\\>\\<table width=\\\"130px\\\" class=\\\"src\\\" cellspacing=0 cellpadding=1 border=0\\>\\<tr\\>";
                  Printf.bprintf buf "
                  \\<td title=\\\"[%d] %s\\\" class=\\\"srctd %s al\\\"\\>%s\\</td\\>
                  \\<td class=\\\"srctd %s ar\\\"\\>%dm/%s/%d\\</td\\>
                  " (client_num c)
                  c.client_name
                  !dlclass 
                     
           (
                try 
                  match c.client_kind with 
                    Known_location (ip,port) -> Printf.sprintf "%s" (Ip.to_string ip)
                  | Indirect_location _ -> Printf.sprintf "Indirect"
                with _ -> ""
          ) 
                  !dlclass last 
                  (if c.client_brand = 1 then "T" else "F") 
                  c.client_score;

                  Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>";
                      
              ) file.file_clients;
              
                  Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>"
          end;

          
          Array.iteri (fun i q ->
              let queue_size = Queue.length q in
              per_queue.(i+1) <- per_queue.(i+1) + queue_size;
              nsources := !nsources + queue_size;

              if queue_size > 0 then begin

                  Printf.bprintf buf "\\<table width=\\\"100%%\\\" class=\\\"sources\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>
\\<td title=\\\"Queue (IP - Age|Overnet T/F|Score)\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Queue[%s]: %d sources (next %s)\\</td\\>
\\<tr class=\\\"dl-2\\\"\\>\\<td class=\\\"dl-2\\\"\\>"
                queue_name.(i) queue_size
              (try
                  let _,s = Queue.head q in
                  match s.source_client with
                    SourceLastConnection (_, last_conn, _) ->
                      let wait =  (last_conn + queue_period.(i)) - last_time ()
                      in
                      if wait > 0 then
                        Printf.sprintf "%d seconds (last ok %d, last try %d)"
                          wait (last_time () - s.source_age)
                        (last_time () - last_conn)
                      else
                        Printf.sprintf "READY (last ok %d, last try %d)"
                          (last_time () - s.source_age)
                        (last_time () - last_conn)
                      
                  | _ -> "connected"
                with _ -> "none"   
              );


              let lasttime = (last_time()) in
              let counter = ref 0 in
              let dlclass = ref "" in
              Queue.iter (fun ss ->

                      if !counter mod 2 = 0 then dlclass := "dl-1" else dlclass := "dl-2";
                      incr counter;
                 
                  let ip,port = ss.source_addr in
                  let last = ((lasttime - ss.source_age) / 60) in
                  Printf.bprintf buf "\\<div style=\\\"float: left;\\\"\\>\\<table width=\\\"130px\\\" class=\\\"src\\\" cellspacing=0 cellpadding=1 border=0\\>\\<tr\\>";
                  Printf.bprintf buf "
                  \\<td class=\\\"srctd %s al\\\"\\>%s\\</td\\>
                  \\<td class=\\\"srctd %s ar\\\"\\>%dm/%s/%d\\</td\\>
                  "
                  !dlclass (Ip.to_string ip) 
                  !dlclass last 
                  (if ss.source_brand = 1 then "T" else "F")
                  ss.source_score;
                  Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>";
                     
              ) q              ;
                  Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>"

              end
              
              ;
          ) file.file_sources;
          
        end
  
      
let check_sources reconnect_client = 

(*
  let buf = Buffer.create 100 in
  print_sources buf;
  lprintf "\n\nSTATS: %s" (Buffer.contents buf);
  lprint_newline ();
*)
  
  let uptime = last_time () - start_time in

  if uptime mod 60 = 0 then recompute_ready_sources ();
  
  if uptime mod 600 = 0 then
    begin
      
      stats_saved_connect_good_clients := !stats_connect_good_clients;
      stats_saved_connect_good_sources := !stats_connect_good_sources;
      stats_saved_connect_new_sources := !stats_connect_new_sources;
      stats_saved_connect_old_sources := !stats_connect_old_sources;
      stats_saved_new_sources := !stats_new_sources;
      stats_saved_files := !stats_files;
      stats_saved_ranks := !stats_ranks;
      
      stats_saved_files_size := 1;
      Intmap.iter (fun _ n -> 
          stats_saved_files_size := !stats_saved_files_size + !n)
      !stats_saved_files;
      
      stats_connect_good_clients := 0;
      stats_connect_new_sources := 0;
      stats_connect_good_sources := 0;
      stats_connect_old_sources := 0;
      stats_new_sources := 0;
      stats_files := Intmap.empty;
      stats_ranks := Array.create 10 0;
    
    end;
  
  (try
      while true do
        let (addr, time) = Fifo.head indirect_fifo in
        if time + !!min_reask_delay < last_time () then
          let ((id, ip, port), _) = Fifo.take indirect_fifo in
          
          query_indirect_client_from_server ip port id
          
        else raise Not_found
      done;
    with _ -> ());
  
  
  let rec iter_first n =
    if CommonGlobals.can_open_connection () && n > 0 then 
      match !source_files with
        [] -> 
          source_files := current_files ();
          iter_second n false
      | file :: tail ->
          source_files := tail;
          iter_first (if check_source_from_file reconnect_client file
              then n-1 else n)
  
  and iter_second n found = 
    if  CommonGlobals.can_open_connection () && n > 0 then 
      match !source_files with
        [] -> 
          if found then begin
              source_files := current_files ();
              iter_second n false
            end
      | file :: tail ->
          source_files := tail;
          let source_found = check_source_from_file reconnect_client file  in
          iter_second (if source_found then n-1 else n) (found || source_found)
          
  in
  iter_first !!max_clients_per_second
  
let reschedule_sources f = ()
  (*
  lprintf "reschedule_sources on sources not implemented"; lprint_newline ()
*)  
  
let init () =
  queue_period.(good_saved_sources_queue) <-  !!min_reask_delay;
  queue_period.(old_sources1_queue) <-  !!min_reask_delay;
  queue_period.(old_sources2_queue) <-  !!min_reask_delay;

end
