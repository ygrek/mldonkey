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

open Queues
open Printf2
open Md4
open Options
open BasicSocket

open TcpBufferedSocket
open CommonFile
open CommonOptions
open CommonTypes

(* BUGS:

* From mlnet.log: not all sources in [new_sources] are ready. More
sources for a file than total sources in [sources_by_uid]. Probably the
same source entered several times inside the same queue. After a connection
failure, we should not only change the request_time but also the
  request_score in case it is initial_new_source_score.
  
*)
  
(* TODO: 

* Limit total number of indirect connections
* Implement need_new_sources
* The concept of 'source' should replace the concept of 'client' for the
  interface. The old version of source management used to keep good clients
  so that useful information about them would not be lost.
  
*)
  
(* A source might be in the following states:

(1) Not Connected: the source appears in the queues between
    'new_sources_queue' and 'old_sources_queue2'.

(2) Connecting: 'source_last_attempt' is not zero. The source appears in the
    queue 'connecting_sources_queue' for all files.

(3) Connected: The source appears either in the queue
  'connected_sources_queue' or 'busy_sources_queue' for every files.
  If 'r.request_time' is 0, it means we can query the source whenever we
  want, but we are already too busy, so the source is in
  'busy_sources_queue'. Otherwise, we are not ready to query, and the source
  has to be in 'connected_sources_queue'.

*)
  

type request_result = 
| File_possible   (* we asked, but didn't know *)
| File_not_found  (* we asked, the file is not there *)
| File_expected   (* we asked, because it was announced *)
| File_new_source (* we never asked, but we should *)
| File_found      (* the file was found *)
| File_chunk      (* the file has chunks we want *)
| File_upload     (* we uploaded from this client *)
| File_unknown    (* We don't know anything *)

let initial_new_source_score = 10 (* before the first request *)
let new_source_score = 1 (* after the first request *)
let not_found_score = -5
let possible_score = -3
let expected_score = 0
let found_score = 3
let chunk_score = 5
let upload_score = 7

let outside_queue = -1
let new_sources_queue = 0
let good_sources_queue = 1
let ready_saved_sources_queue = 2
let waiting_saved_sources_queue = 3
let old_sources1_queue = 4
let old_sources2_queue = 5
let old_sources3_queue = 6
let connected_sources_queue = 7
let connecting_sources_queue = 8
let busy_sources_queue = 9

let queue_name = [|
    "new_sources";
    "good_sources";
    "ready_saved_sources";
    "waiting_saved_sources";
    "old_sources1";
    "old_sources2";
    "old_sources3";
    "connected_sources";
    "connecting_sources";
    "busy_sources";
  |]


let nqueues = Array.length queue_name

let queue_period = Array.create nqueues 600
  
let _ =
  queue_period.(new_sources_queue) <- 0;
  queue_period.(connected_sources_queue) <- 0;
  queue_period.(connecting_sources_queue) <- 0;
  queue_period.(busy_sources_queue) <- 0
  
module Make(M: 


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Argument                              *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
    
    sig
      
      val module_name : string
      
      type source_uid              
      val dummy_source_uid : source_uid
      val source_uid_to_value: source_uid -> Options.option_value
      val value_to_source_uid: Options.option_value -> source_uid
      
      type source_brand
      val dummy_source_brand : source_brand
      val source_brand_to_value: source_brand -> Options.option_value
      val value_to_source_brand: Options.option_value -> source_brand
      
      val direct_source : source_uid -> bool    
    end) = 
  (struct


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Body                                  *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(*************************************************************************)
(*                                                                       *)
(*                         Types                                         *)
(*                                                                       *)
(*************************************************************************)
      
      type source = {
          source_uid : M.source_uid;
          mutable source_files : file_request list;

(* the 'source_score' increases with failures in connections *)
          mutable source_score : int;

(* the 'source_num' that should be used to create the client corresponding to
  this source *)
          mutable source_num : int;

(* the 'source_age' is the time of the last successful connection *)
          mutable source_age : int;

(* the 'source_connecting' indicates that this source is currently in the
  process of being connected. *)
          mutable source_last_attempt : int;
          mutable source_sock : tcp_connection;
          
          mutable source_brand : M.source_brand;
        }
      
      and file_request = {
          request_file : file_sources_manager;
          mutable request_queue : int;
          mutable request_time : int;
          mutable request_score : int;
        }
      
      and file_sources_manager = {
          manager_uid : string;
          mutable manager_sources : source Queues.Queue.t array;
          mutable manager_active_sources : int;
          mutable manager_all_sources : int;
          mutable manager_file : (unit -> file);
          mutable manager_brothers : file_sources_manager list;
        }
      
      and functions = {
          mutable function_connect: (M.source_uid -> unit);
          mutable function_query: (M.source_uid -> string -> unit);
          
          mutable function_string_to_manager: (string -> file_sources_manager);
          
          mutable function_max_connections_per_second : (unit -> int);
          mutable function_max_sources_per_file : (unit -> int);
          
          mutable function_add_location : 
          (M.source_uid -> string -> unit);
          mutable function_remove_location : 
          (M.source_uid -> string -> unit);
        }

(*************************************************************************)
(*                                                                       *)
(*                         Modules                                       *)
(*                                                                       *)
(*************************************************************************)
      
      module HS = Weak2.Make(struct
            type t = source
            let hash s = Hashtbl.hash s.source_uid
            
            let equal x y = x.source_uid = y.source_uid
          end)
      
      module H = Weak2.Make(struct
            type t = source
            let hash s = Hashtbl.hash s.source_num
            
            let equal x y = x.source_num = y.source_num
          end)
      
      module SourcesQueueCreate = Queues.Make(struct 
            type t = source 
            let compare s1 s2 = compare s1.source_uid s2.source_uid
          end)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)
      
      let dummy_source = {
          source_uid = M.dummy_source_uid;
          source_files = [];
          
          source_num = 0;
          source_score = 0;
          source_age = 0;
          source_last_attempt = 0;
          source_sock = NoConnection;
          
          source_brand = M.dummy_source_brand;
        }
      
      let last_refill = ref 0
      
      let not_implemented s _ = 
        failwith (Printf.sprintf "CommonSources.%s not implemneted" s)
      
      let functions = {
          function_connect = not_implemented "function_connect";
          function_query = not_implemented "function_query";
          function_string_to_manager = not_implemented 
            "function_string_to_manager";
          
          function_max_connections_per_second = (fun _ ->
              !!max_connections_per_second);
          function_max_sources_per_file = (fun _ -> 10);
          
          function_add_location = not_implemented "function_add_location";
          function_remove_location = not_implemented "function_remove_location";
        
        }
      
      let indirect_connections = ref 0

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)
      
      let sources_by_uid = HS.create 13557      
      let sources_by_num = H.create 13557      
      
      let file_sources_managers = ref []
      
      let connecting_sources = Fifo.create ()
      
      let next_direct_sources = Fifo.create ()
      let next_indirect_sources = ref []
      
      
      let active_queue q = 
        q >= connected_sources_queue && q <= busy_sources_queue

(*************************************************************************)
(*                                                                       *)
(*                         print_source                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let print_source buf s =
        Printf.bprintf buf "Source %d:\n" s.source_num;
        Printf.bprintf buf "   score: %d\n" s.source_score;
        if s.source_age <> 0 then
          Printf.bprintf buf "   age: %d\n" s.source_age;
        if s.source_last_attempt <> 0 then
          Printf.bprintf buf "   last_attemps: %d" s.source_last_attempt;
        List.iter (fun r ->
            Printf.bprintf buf "     File %s\n" (file_best_name (r.request_file.manager_file ()));
            Printf.bprintf buf "       Score: %d\n" r.request_score;
            if r.request_time <> 0 then
              Printf.bprintf buf "       Time: %d\n" r.request_time;
        ) s.source_files

(*************************************************************************)
(*                                                                       *)
(*                         print                                         *)
(*                                                                       *)
(*************************************************************************)
      
      let print buf =
        
        Printf.bprintf buf "Statistics on sources: time %d\n" (last_time ());
        
        Printf.bprintf buf "File Sources Managers table:\n";
        let nsources_per_queue = Array.create nqueues 0 in
        let nready_per_queue = Array.create nqueues 0 in
        let nindirect_per_queue = Array.create nqueues 0 in
        List.iter (fun m ->
            Printf.bprintf buf "Manager: %d/%d sources for %s\n" 
              m.manager_active_sources m.manager_all_sources
              (file_best_name (m.manager_file ()))
            ;
            
            for i = 0 to nqueues -1 do
              let q = m.manager_sources.(i) in
              Printf.bprintf buf "   Queue[%s]: %d entries" 
                queue_name.(i) (Queue.length q);
              let nindirect = ref 0 in
              let nready = ref 0 in
              let nsources = ref 0 in
              Queue.iter (fun (time, s) ->
                  incr nsources;
                  if not (M.direct_source s.source_uid) then incr nindirect;
                  if time + !!min_reask_delay < last_time () then
                    incr nready
                  else
                  if i = new_sources_queue then begin
                      Printf.bprintf buf "ERROR: Source is not ready in new_sources_queue !\n";
                      print_source buf s
                    end
              ) q;
              Printf.bprintf buf " (ready: %d/%d)" !nready !nsources ;
              if !nindirect > 0 then
                Printf.bprintf buf  " (indirect %d)" !nindirect;
              nsources_per_queue.(i) <- nsources_per_queue.(i) + !nsources;
              nready_per_queue.(i) <- nready_per_queue.(i) + !nready;
              nindirect_per_queue.(i) <- nindirect_per_queue.(i) + !nindirect;
              Printf.bprintf buf "\n";
            done
        
        ) !file_sources_managers;
        
        Printf.bprintf buf  "\nFor all managers:\n";
        for i = 0 to nqueues - 1 do
          Printf.bprintf buf "   Queue[%s]: %d entries"
            queue_name.(i) 
          nsources_per_queue.(i) ;
          
          if nsources_per_queue.(i) > 0 then
            Printf.bprintf buf " (%d ready)" nready_per_queue.(i);
          
          if nindirect_per_queue.(i) > 0 then
            Printf.bprintf buf " (%d indirect)" nindirect_per_queue.(i);
          
          Printf.bprintf buf "\n";
        
        done;
        
        let nsources = ref 0 in
        HS.iter (fun _ -> incr nsources) sources_by_uid;
        Printf.bprintf buf "Sources by UID table: %d entries\n" !nsources;
        
        let nconnected = ref 0 in
        Fifo.iter (fun (_,s) ->
            if s.source_last_attempt = 0 then incr nconnected;
        ) connecting_sources;
        Printf.bprintf buf "Connecting Sources: %d entries" 
          (Fifo.length connecting_sources);
        if !nconnected > 0 then Printf.bprintf buf " (connected: %d)" !nconnected;
        Printf.bprintf buf "\n";
        
        Printf.bprintf buf "Next Direct Sources: %d entries\n" 
          (Fifo.length next_direct_sources);
        
        Printf.bprintf buf "Next Indirect Sources: %d entries\n"
          (List.length !next_indirect_sources)

(*************************************************************************)
(*                                                                       *)
(*                         set_score                                     *)
(*                                                                       *)
(*************************************************************************)
      
      let set_score s r score =
        let old_score = r.request_score in
        r.request_score <- score


(*************************************************************************)
(*                                                                       *)
(*                         reschedule_source_for_file                    *)
(*                                                                       *)
(*************************************************************************)
      
      let reschedule_source_for_file saved s r =
        if r.request_queue = outside_queue then
          if r.request_score > possible_score then
            let queue = 
              if s.source_last_attempt <> 0 then
                connecting_sources_queue
              else
              match s.source_sock with
              | (NoConnection | ConnectionWaiting _)  ->
(* State (1) *)

(* Two things matter: the global score and the local score *)
                  if s.source_score < 3 then
                    if r.request_score > expected_score then
                      if saved then
                        if 
                          r.request_time + !!min_reask_delay < last_time () 
                        then 
                          ready_saved_sources_queue
                        else waiting_saved_sources_queue
                      else
                      if r.request_score = initial_new_source_score then
                        new_sources_queue
                      else
                        good_sources_queue 
                    else
                      old_sources1_queue
                  else
                  if s.source_score < 10 then old_sources1_queue else 
                  if s.source_score < 20 then old_sources2_queue else
                    old_sources3_queue

(* State (3) *)
              | Connection _ -> 
                  if r.request_time = 0 then
                    busy_sources_queue
                  else
                    connected_sources_queue
            in
            let m = r.request_file in
            if !verbose_sources > 1 then
              lprintf "Put source %d in queue %s\n" s.source_num
                queue_name.(queue);
            Queue.put m.manager_sources.(queue) (r.request_time, s);
            if active_queue queue then              
              m.manager_active_sources <- m.manager_active_sources + 1;
            m.manager_all_sources <- m.manager_all_sources + 1;
            r.request_queue <- queue

(*************************************************************************)
(*                                                                       *)
(*                         iter_all_sources                              *)
(*                                                                       *)
(*************************************************************************)
      
      let iter_all_sources f m =
        Array.iter (fun q ->
            Queue.iter (fun (_,s) -> f s)  q
        ) m.manager_sources

(*************************************************************************)
(*                                                                       *)
(*                         iter_active_sources                           *)
(*                                                                       *)
(*************************************************************************)
      
      let iter_active_sources f m =
        for i = connected_sources_queue to busy_sources_queue do
          let q = m.manager_sources.(i) in
          Queue.iter (fun (_,s) -> f s)  q
        done



(*************************************************************************)
(*                                                                       *)
(*                         set_source_brand                              *)
(*                                                                       *)
(*************************************************************************)
      
      let set_source_brand s brand = 
        s.source_brand <- brand

(*************************************************************************)
(*                                                                       *)
(*                         source_brand                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let source_brand s = s.source_brand

(*************************************************************************)
(*                                                                       *)
(*                         remove_from_queue                             *)
(*                                                                       *)
(*************************************************************************)
      
      let remove_from_queue s r = 
        if r.request_queue <> outside_queue then begin
            if !verbose_sources > 1 then
              lprintf " ** Remove source %d from queue %s\n" s.source_num
                queue_name.(r.request_queue);
            
            let m = r.request_file in
            if active_queue r.request_queue then
              m.manager_active_sources <- m.manager_active_sources - 1;
            Queue.remove r.request_file.manager_sources.(r.request_queue)
            (r.request_time, s);
            r.request_queue <- outside_queue;
            m.manager_all_sources <- m.manager_all_sources - 1
          end

(*************************************************************************)
(*                                                                       *)
(*                         source_connecting                             *)
(*                                                                       *)
(*************************************************************************)

(* From state (1) to state (2) *)
      let source_connecting s = 
        s.source_last_attempt <- last_time ();
        Fifo.put connecting_sources (s.source_last_attempt, s);
        List.iter (fun r ->
            if r.request_queue <> outside_queue then begin
                remove_from_queue s r;
                reschedule_source_for_file false s r;
              end
        ) s.source_files


(*************************************************************************)
(*                                                                       *)
(*                         source_query                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let source_query s r =
        remove_from_queue s r;
        r.request_time <- 0; (* The source is ready for this request *)
        reschedule_source_for_file false s r; (* put it in busy_sources_queue *)
        (try functions.function_query 
              s.source_uid r.request_file.manager_uid with
            e ->
              lprintf "Exception %s in functions.function_query\n"
                (Printexc2.to_string e))

(*************************************************************************)
(*                                                                       *)
(*                         source_connected                              *)
(*                                                                       *)
(*************************************************************************)

(* From state (2) to state (3) *)
      let source_connected s = 
        s.source_score <- 0;
        s.source_age <- last_time ();
        s.source_last_attempt <- 0;
        List.iter (fun r ->
(*            lprintf "SOURCE> request: "; *)
            if r.request_queue <> outside_queue then begin
(*                lprintf "score %d/%d last query %s\n"
                  r.request_score possible_score
                  (if r.request_time = 0 then "never" else
                    Printf.sprintf "%d secs"
                      (last_time () - r.request_time));                *)
                remove_from_queue s r;
                if r.request_score > possible_score &&
                  r.request_time + !!min_reask_delay < last_time () then
                  source_query s r;
                (try 
                    let m = r.request_file in
                    functions.function_add_location s.source_uid 
                      m.manager_uid with _ -> ());
                reschedule_source_for_file false s r
              end (* else
              lprintf "outside queue\n" *)
        ) s.source_files

(*************************************************************************)
(*                                                                       *)
(*                         source_disconnected                           *)
(*                                                                       *)
(*************************************************************************)

(* From states (1) or (2) to state (3) *)
      let source_disconnected s =
        (match s.source_sock with
            NoConnection -> ()
          | ConnectionWaiting token ->
              cancel_token token;
              s.source_sock <- NoConnection
          | Connection sock ->
              close sock Closed_for_timeout
        );
        let connecting =  s.source_last_attempt <> 0 in
        s.source_last_attempt <- 0;
        List.iter (fun r ->
            if r.request_queue <> outside_queue then begin
                remove_from_queue s r;
                if connecting then begin
                    r.request_time <- last_time ();
                    if r.request_score = initial_new_source_score then
                      set_score s r new_source_score
                  end else begin
                    if r.request_time = 0 then
                      r.request_time <- last_time () - 600;
                    (try
                        let m = r.request_file in
                        functions.function_remove_location s.source_uid
                          m.manager_uid with _ -> ())
                  end;
                reschedule_source_for_file false s r;
              end;
        ) s.source_files

(*************************************************************************)
(*                                                                       *)
(*                         connect_source                                *)
(*                                                                       *)
(*************************************************************************)
      
      let connect_source s =
        if !verbose_sources > 1 then
          lprintf "CommonSources.connect_source\n";
        s.source_score <- s.source_score + 1;
        functions.function_connect s.source_uid

(*************************************************************************)
(*                                                                       *)
(*                         create_queues                                 *)
(*                                                                       *)
(*************************************************************************)
      
      let create_queues () =
        let queues = [|              
(* New sources *)
(* We should change this to 'oldest_last' to improve Queue.remove *)
            SourcesQueueCreate.oldest_last (); (* instead of lifo *)
(* Good sources *)
(* We should change this to 'oldest_first' to improve Queue.remove *)
            SourcesQueueCreate.oldest_first (); (* instead of fifo *)
(* Ready saved sources *)
            SourcesQueueCreate.oldest_last ();
(* Waiting saved sources *)
            SourcesQueueCreate.oldest_first ();
(* Old sources *)
(* We should change this to 'oldest_first' to improve Queue.remove *)
            SourcesQueueCreate.oldest_first (); (* instead of fifo *)
            SourcesQueueCreate.oldest_first (); (* instead of fifo *)
            SourcesQueueCreate.oldest_first (); (* instead of fifo *)
(* Connected Sources *)
            SourcesQueueCreate.oldest_first ();
(* Connecting Sources *)
            SourcesQueueCreate.oldest_first ();
(* Busy Sources *)
            SourcesQueueCreate.oldest_first ();
          |] in
        if Array.length queues <> Array.length queue_name then begin
            lprintf "Falal error in CommonSources.create_queues\n";
            exit 2;
          end;
        queues

(*************************************************************************)
(*                                                                       *)
(*                         create_file_sources_manager                   *)
(*                                                                       *)
(*************************************************************************)
      
      let create_file_sources_manager file_uid =
        let m = {
            manager_uid = file_uid;
            manager_file = not_implemented "manager_file";
            manager_all_sources = 0;
            manager_active_sources = 0;
            manager_sources = create_queues ();
            manager_brothers = [];
          } in
        file_sources_managers := m :: !file_sources_managers;
        m

(*************************************************************************)
(*                                                                       *)
(*                         remove_file_sources_manager                   *)
(*                                                                       *)
(*************************************************************************)
      
      let remove_file_sources_manager m =
        
        iter_all_sources (fun s -> 
            s.source_files <- List.filter (fun r ->
                r.request_file != m
            ) s.source_files;
        ) m;
        
        m.manager_sources <- create_queues ();
        
        let brothers = List.filter (fun m' -> m' <> m) m.manager_brothers in
        List.iter (fun m -> m.manager_brothers <- brothers) brothers;
        
        file_sources_managers := List2.removeq m !file_sources_managers


(*************************************************************************)
(*                                                                       *)
(*                         find_source_by_uid                            *)
(*                                                                       *)
(*************************************************************************)
      
      let find_source_by_uid uid = 
        try
          let finder =  { dummy_source with source_uid = uid } in
          let s = HS.find sources_by_uid finder in
          s
        
        with _ ->
            if !verbose_sources > 1 then
              lprintf "Creating new source\n";
            let n = CommonClient.book_client_num () in
            let s = { dummy_source with
                source_uid = uid;
                source_age = 0;
                source_num = n;
                source_files = [];
              }  in
            HS.add sources_by_uid s;
            H.add sources_by_num s;
            s

(*************************************************************************)
(*                                                                       *)
(*                         find_source_by_num                            *)
(*                                                                       *)
(*************************************************************************)
      
      let find_source_by_num num = 
        let finder =  { dummy_source with source_num = num } in
        let s = H.find sources_by_num finder in
        s

(*************************************************************************)
(*                                                                       *)
(*                         find_request                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let rec iter_has_request rs file =
        match rs with
          [] -> raise Not_found
        | r :: tail ->
            if r.request_file == file then r else
              iter_has_request tail file
      
      let find_request s file = 
        iter_has_request s.source_files file


(*************************************************************************)
(*                                                                       *)
(*                         find_request_result                           *)
(*                                                                       *)
(*************************************************************************)
      
      let find_request_result s file =
        let score =  (find_request s file).request_score in
        if score = initial_new_source_score then File_new_source else
        if score = not_found_score then File_not_found else
        if score < not_found_score then File_possible else
        if score <= new_source_score then File_expected else
        if score <= found_score then File_found else
        if score <= chunk_score then File_chunk else
        if score <= upload_score then File_upload else
          assert false

(*************************************************************************)
(*                                                                       *)
(*                         add_request                                   *)
(*                                                                       *)
(*************************************************************************)
      
      let check_time time =
        if time = 0 then
          last_time () - 650
        else
          time (* changed 2.5.24 *)
      
      let add_request s file time =
        let r =
          try
            let r = find_request s file in
            remove_from_queue s r;
            set_score s r (if r.request_score = initial_new_source_score then
                new_source_score
              else
                r.request_score - 1);
            r.request_time <- check_time time;
            r
          with Not_found ->
              let r = {
                  request_file = file;
                  request_time = check_time time;
                  request_score = possible_score;
                  request_queue = outside_queue;
                } in
              s.source_files <- r :: s.source_files;
              r
        in
        reschedule_source_for_file false s r;
        r

(*************************************************************************)
(*                                                                       *)
(*                         set_request_score                            *)
(*                                                                       *)
(*************************************************************************)
      
      let rec set_request_score s file score =
        try
          let r = find_request s file in
          if not (
(* If a request has been done in the last half-hour, and the source is
  announced as new, just forget it. *)
              score = initial_new_source_score &&
              r.request_time + 1800 > last_time ()
            ) then
            let score = 
              if score = initial_new_source_score then new_source_score
              else score
            in
            if r.request_queue < connected_sources_queue then
              remove_from_queue s r;
            set_score s r score;
            reschedule_source_for_file false s r;
        with Not_found ->
            let r = {
                request_file = file;
                request_time = check_time 0;
                request_score = possible_score;
                request_queue = outside_queue;
              } in
            set_score s r score;
            s.source_files <- r :: s.source_files;
            reschedule_source_for_file false s r

(*************************************************************************)
(*                                                                       *)
(*                         set_request_result                            *)
(*                                                                       *)
(*************************************************************************)
      
      let set_request_result s file result =
        set_request_score s file (match result with
            File_not_found -> not_found_score
          | File_found -> 
(* Advertise the files associated with this file that this source is
probably interesting. Since we are already connected, it means
we will probably query for the other file almost immediatly. *)
              List.iter (fun m ->
                  set_request_score s m initial_new_source_score
              ) file.manager_brothers;
              found_score
          | File_chunk -> chunk_score
          | File_upload -> upload_score
          | File_new_source -> initial_new_source_score
          | File_possible -> possible_score + 1
          | _ -> assert false)

(*************************************************************************)
(*                                                                       *)
(*                         source_to_value                               *)
(*                                                                       *)
(*************************************************************************)
      
      let source_to_value s assocs =
        let requests = ref [] in
        List.iter (fun r ->
            if r.request_score > possible_score then
              
              requests := 
              (SmallList
                  [once_value (string_to_value r.request_file.manager_uid);
                  int_to_value r.request_score;
                  int_to_value r.request_time]
              ) :: 
              !requests
        ) s.source_files;
        if !requests = [] then raise Exit;
        (
          ("sscore", int_to_value s.source_score ) ::
          ("addr", M.source_uid_to_value s.source_uid ) ::
          ("brand", M.source_brand_to_value s.source_brand ) ::
          ("files", smalllist_to_value "file list" 
              (fun s -> s)
            !requests) ::
          ("age", int_to_value s.source_age ) ::
          assocs
        )


(*************************************************************************)
(*                                                                       *)
(*                         query_file                                    *)
(*                                                                       *)
(*************************************************************************)
      
      let query_file s file =
        if file_state (file.manager_file ()) = FileDownloading then
          let r = find_request s file in
          if r.request_time + !!min_reask_delay <= last_time () then 
            if r.request_score > not_found_score ||
              last_time () - r.request_time > 3600 then 
              source_query s r


(*************************************************************************)
(*                                                                       *)
(*                         set_brothers                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let set_brothers files =
        let brothers = ref [] in
        let rec add_brother m =
          if not (List.memq m !brothers) then begin
              brothers := m :: !brothers;
              List.iter add_brother m.manager_brothers
            end
        in
        List.iter add_brother files;
        
        List.iter (fun m ->
            m.manager_brothers <- !brothers
        ) !brothers

(*************************************************************************)
(*                                                                       *)
(*                         get_brothers                                  *)
(*                                                                       *)
(*************************************************************************)
      
      let get_brothers file =
        List.map (fun m -> m.manager_uid) file.manager_brothers

(*************************************************************************)
(*                                                                       *)
(*                         add_saved_source_request                      *)
(*                                                                       *)
(*************************************************************************)
      
      let add_saved_source_request s uid score time =
        if !verbose_sources > 1 then
          lprintf "  Request %s %d %d\n" uid score time;
        let file = 
          try
            functions.function_string_to_manager uid 
          with e ->
              if !verbose then begin
                  lprintf "CommonSources: add_saved_source_request -> %s not found\n" uid;
                end;
              raise e 
        in
        let r = add_request s file time in
        set_score s r score;
        reschedule_source_for_file true s r;
        if !verbose_sources > 1 then
          lprintf "Put saved source %d in queue %s\n" s.source_num
            queue_name.(r.request_queue)

(*************************************************************************)
(*                                                                       *)
(*                         value_to_source                               *)
(*                                                                       *)
(*************************************************************************)
      
      let value_to_source assocs =
(*        lprintf "(1) value_to_source\n"; *)
        let get_value name conv = conv (List.assoc name assocs) in    
        
        let addr = get_value "addr" M.value_to_source_uid in
        let files = get_value "files" 
            (value_to_list (fun s -> s)) in
        
        let last_conn = 
          try get_value "age" value_to_int with _ -> 0
        in
        
        let score = try get_value "sscore" value_to_int with _ -> 0 in
        let brand = try get_value "brand" M.value_to_source_brand with _ -> 
              M.dummy_source_brand in
        
        if !verbose_sources > 1 then
          lprintf "New source from value\n";
        let s = find_source_by_uid addr in
        s.source_score <- score;
        s.source_age <- last_conn;
        s.source_brand <- brand;

(*        lprintf "(2) value_to_source \n"; *)
        
        let rec iter v =
          match v with
            OnceValue v -> iter v
          | List [uid; score; time] | SmallList [uid; score; time] ->
              (try
                  let uid = value_to_string uid in
                  let score = value_to_int score in
                  let time = value_to_int time in
(*                  lprintf "(3) value_to_source \n"; *)
                  
                  add_saved_source_request s uid score time
                
                with e -> 
                    if !verbose_sources > 1 then begin
                        lprintf "CommonSources.value_to_source: exception %s in iter request\n"
                          (Printexc2.to_string e);
                      end
              )
          | (StringValue _) as uid ->
              (try
                  let uid = value_to_string uid in
(*                  lprintf "(4) value_to_source \n"; *)
                  
                  let score = 0 in
                  let time = 0 in
                  add_saved_source_request s uid score time
                
                with e -> 
                    if !verbose_sources > 1 then begin
                        lprintf "CommonSources.value_to_source: exception %s in iter request\n"
                          (Printexc2.to_string e);
                      end
              )
          | _ -> assert false
        
        in
(*        lprintf "(5) value_to_source \n"; *)
        
        List.iter iter files;
(*        lprintf "(6) value_to_source \n"; *)
        
        raise SideEffectOption

(*************************************************************************)
(*                                                                       *)
(*                         refill_sources                                *)
(*                                                                       *)
(*************************************************************************)
      
      let refill_sources () =

(* Wait for 9 seconds before refilling, since we put at least 10 seconds
  of clients in the previous bucket. *)
        if !last_refill + 8 < last_time () then
          
          try
            last_refill := last_time ();
            if !verbose_sources > 0 then begin
                lprintf "CommonSources.refill_sources BEFORE:\n";
                let buf = Buffer.create 100 in
                print buf;
                lprintf "%s\n\n" (Buffer.contents buf);
              end;
            
            let rec iter nsources todo_files done_files =
              if nsources > 0 then
                match todo_files with
                  (m, queue) :: tail ->
                    iter_sources nsources tail done_files m queue
                | [] ->
                    match done_files with
                      [] -> nsources
                    | _ -> iter nsources done_files []
              else 0
            
            and iter_sources nsources todo_files done_files m queue =
              if queue = connected_sources_queue then
                iter nsources todo_files done_files
              else
              let q = m.manager_sources.(queue) in
              if Queue.length q > 0 then
                let (request_time, s) = Queue.head q in
                if request_time + queue_period.(queue) < last_time () then begin
                    if !verbose_sources > 1 then
                      lprintf "Sources: take source from Queue[%s] for %s\n"
                        queue_name.(queue) 
                      (file_best_name (m.manager_file ()));
                    source_connecting s;
                    if M.direct_source s.source_uid then begin
                        Fifo.put next_direct_sources s;
                        iter (nsources-1) todo_files ((m, queue) :: done_files)
                      end else begin
                        next_indirect_sources := s :: !next_indirect_sources;
                        iter_sources nsources todo_files done_files m queue
                      end
                  end else begin 
                    if !verbose_sources > 1 then
                      lprintf "Source of queue %s is not ready for %s\n"
                        queue_name.(queue) (file_best_name (m.manager_file ()));
                    iter_sources nsources todo_files done_files m (queue+1)
                  end
              else begin
                  if !verbose_sources > 1 then
                    lprintf "Queue %s is empty for %s\n"
                      queue_name.(queue) (file_best_name (m.manager_file ()));
                  iter_sources nsources todo_files done_files m (queue+1)
                end
            
            in

(* Fill only with sources from files being downloaded *)
            
            let nfiles = ref 0 in
            let files = ref [] in
            List.iter (fun m ->
                match file_state (m.manager_file ()) with
                  FileDownloading ->
                    let priority = file_priority (m.manager_file ()) in
                    
                    (try
                        let files = List.assoc priority !files in
                        files := (m, new_sources_queue) :: !files
                      with _ ->
                          files := (priority, ref [m, new_sources_queue]) :: !files
                    );
                    
                    incr nfiles
                | _ -> ()
            ) !file_sources_managers;
            let files = List.sort (fun (p1,_) (p2,_) -> compare p2 p1) !files in
            let nsources = maxi (!nfiles*3) 
              (functions.function_max_connections_per_second () * 10) in

(* Fill by starting with the files with either priority. Sources for files 
with lower priority might not be connected at all for long period, but
we don't care since we decided to decrease their priority ! *)
            
            let rec iter_priorities nsources files =
              if nsources > 0 then
                match files with
                  [] -> ()
                | (prio, files) :: tail ->
                    if !verbose_sources > 1 then 
                      lprintf "Adding sources for priority %d (%d max)\n" prio
                        nsources;
                    let nsources = iter nsources !files [] in
                    iter_priorities nsources tail
            in
            iter_priorities nsources files;
            if !verbose_sources > 0 then begin
                lprintf "CommonSources.refill_sources AFTER:\n";
                let buf = Buffer.create 100 in
                print buf;
                lprintf "%s\n\n" (Buffer.contents buf);
              end;
          
          with e -> 
              lprintf "Exception %s in refill_sources\n"
                (Printexc2.to_string e)

(*************************************************************************)
(*                                                                       *)
(*                         clean_sources                                 *)
(*                                                                       *)
(*************************************************************************)
      
      let clean_sources () =
(* Maybe this should be dependant on the file (priority, state,...) ? *)
        let max_sources_per_file = functions.function_max_sources_per_file () in
        List.iter (fun m ->
            let nsources = ref 0 in
            iter_all_sources (fun _ -> incr nsources) m;
            
            if !nsources > max_sources_per_file then
              let rec iter nsources q queue =
                if nsources > 0 then
                  if Queue.length q > 0 then
                    begin
                      let _, s = Queue.take q in
                      m.manager_all_sources <- m.manager_all_sources - 1;
                      if active_queue queue then
                        m.manager_active_sources <- m.manager_active_sources - 1;                      
                      List.iter (fun r ->
                          if r.request_file == m then begin
                              r.request_queue <- outside_queue;
                              set_score s r not_found_score
                            end
                      ) s.source_files;
                      iter (nsources-1) q queue
                    end
                  else
                  if queue > new_sources_queue then
                    iter nsources m.manager_sources.(queue-1) (queue-1)
              in
              iter (!nsources - max_sources_per_file)
              (m.manager_sources.(old_sources3_queue))
              old_sources3_queue
        ) !file_sources_managers

(*************************************************************************)
(*                                                                       *)
(*                         connect_sources                               *)
(*                                                                       *)
(*************************************************************************)
      
      let connect_sources connection_manager = 
        
        if !verbose_sources > 1 then
          lprintf "connect_sources\n";
(* After 2 minutes, consider that connections attempted should be revoked. *)
        
        if !verbose_sources > 1 then
          lprintf "   revoke connecting sources...\n";
        let rec iter () =
          if not (Fifo.empty connecting_sources) then
            let (time, s) = Fifo.head connecting_sources in
            if time <> s.source_last_attempt then begin
                ignore (Fifo.take connecting_sources);
                iter ()
              end else
            if time + 120 < last_time () then begin
                ignore (Fifo.take connecting_sources);
                if s.source_last_attempt <> 0 then source_disconnected s;
                iter ()
              end
        in
        iter ();

(* First, require !!max_connections_per_second sources to connect to us.
The probability is very high they won't be able to connect to us. *)
        
        if !verbose_sources > 1 then
          lprintf "   connect indirect sources...\n";
        let (first_sources, last_sources) = 
          List2.cut !!max_connections_per_second !next_indirect_sources in
        next_indirect_sources := last_sources;
        List.iter (fun s -> 
            ignore (connect_source s)) first_sources;

(* Second, for every file being downloaded, query sources that are already
connected if needed *)
        if !verbose_sources > 1 then
          lprintf "   query connected sources...\n";
        List.iter (fun m ->
            match file_state (m.manager_file ()) with
              FileDownloading ->
                let q = m.manager_sources.(connected_sources_queue) in
                let rec iter () =
                  if Queue.length q > 0 then
                    let (time, s) = Queue.head q in
                    if time + !!min_reask_delay < last_time () then begin
                        
                        let r = find_request s m in
                        source_query s r;
(* After this step, the source is either in 'busy_sources_queue', if for some
reason, the request could not be sent, or in 'connected_sources_queue' at
the tail if the request could be sent. This seems thus safe. *)
                        iter ()
                      end
                in
                iter ()
            | _ -> ()
        ) !file_sources_managers;
        
        if !verbose_sources > 1 then
          lprintf "   connect to sources...\n";
(* Finally, connect to available sources *)
        try
          let max_sources = functions.function_max_connections_per_second () in
          if !verbose_sources > 1 then
            lprintf "max_sources: %d\n" max_sources;
          let rec iter nsources refilled =
            if nsources > 0 && can_open_connection connection_manager then
              if Fifo.length next_direct_sources > 0 then
                let s = Fifo.take next_direct_sources in
                connect_source s;
                let nsources = match s.source_sock with
                    NoConnection -> 
                      if !verbose_sources > 1 then
                        lprintf "not connected\n"; nsources
                  | _ -> nsources-1 
                in
                iter nsources refilled
              else
              if not refilled then begin
                  refill_sources ();
                  iter nsources true
                end
          in
          iter max_sources false;
          if !verbose_sources > 1 then
            lprintf "   done\n";
        with Exit -> ()



(*************************************************************************)
(*                                                                       *)
(*                         attach_sources_to_file                        *)
(*                                                                       *)
(*************************************************************************)
      
      let value_to_module f v =
        match v with
          Module list -> f list
        | _ -> failwith "Option should be a module"
      
      let option = define_option_class "Source"
          (fun v -> 
(*            lprintf "(n) source !!\n"; *)
            value_to_module value_to_source v)
        (fun s -> Module (source_to_value s []))
      
      let file_sources_option = ref None
      
      let attach_sources_to_file section =
(*        lprintf "attach_sources_to_file\n"; *)
        let sources = match !file_sources_option with
            None -> 
(*              lprintf "attaching sources this time\n"; *)
              let sources = define_option section 
                  ["sources"] ""  (listiter_option option) []
              in
(*              lprintf "done\n"; *)
              file_sources_option := Some sources;
              sources
          | Some sources ->  sources
        in
        sources =:= [];
        HS.iter (fun s -> sources =:= s :: !!sources) sources_by_uid;
        
        (fun _ -> sources =:= [])


(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
      
      let _ = 
        Heap.add_memstat M.module_name (fun level buf ->
            
            let nsources_per_queue = Array.create nqueues 0 in
            let nready_per_queue = Array.create nqueues 0 in
            List.iter (fun m ->
                for i = 0 to nqueues -1 do
                  let q = m.manager_sources.(i) in
                  let nready = ref 0 in
                  let nsources = ref 0 in
                  Queue.iter (fun (time, s) ->
                      incr nsources;
                      if time + !!min_reask_delay < last_time () then
                        incr nready
                      else
                      if i = new_sources_queue then begin
                          Printf.bprintf buf "ERROR: Source is not ready in new_sources_queue !\n";
                          print_source buf s
                        end
                  ) q;
                  nsources_per_queue.(i) <- nsources_per_queue.(i) + !nsources;
                  nready_per_queue.(i) <- nready_per_queue.(i) + !nready;
                done            
            ) !file_sources_managers;
            
            Printf.bprintf buf  "\nFor all managers:\n";
            for i = 0 to nqueues - 1 do
              Printf.bprintf buf "   Queue[%s]: %d entries (%d ready)\n" 
                queue_name.(i) nsources_per_queue.(i) nready_per_queue.(i);
            
            done;
            
            let nsources = ref 0 in
            HS.iter (fun _ -> incr nsources) sources_by_uid;
            Printf.bprintf buf "Sources by UID table: %d entries\n" !nsources;
            
            let nconnected = ref 0 in
            Fifo.iter (fun (_,s) ->
                if s.source_last_attempt = 0 then incr nconnected;
            ) connecting_sources;
            Printf.bprintf buf "Connecting Sources: %d entries" 
              (Fifo.length connecting_sources);
            if !nconnected > 0 then Printf.bprintf buf " (connected: %d)" !nconnected;
            Printf.bprintf buf "\n";
            
            Printf.bprintf buf "Next Direct Sources: %d entries\n" 
              (Fifo.length next_direct_sources);
            
            Printf.bprintf buf "Next Indirect Sources: %d entries\n"
              (List.length !next_indirect_sources)
        )
    
    end)
  
  