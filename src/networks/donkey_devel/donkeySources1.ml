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

open Queues
open Printf2
open Options
open CommonOptions
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals
open DonkeySourcesMisc


let client_connected c =
  c.client_score <- 0;
  match c.client_source with None -> () | Some s ->
      s.source_age <- last_time ()

let recompute_ready_sources () =
  lprintf "recompute_ready_sources not implemented"; lprint_newline ()
  
let immediate_connect_queue = 0
let good_clients_queue = 1
    
(* The queues after this one are implemented using 'sources' and not
'clients' *)
let last_clients_queue = good_clients_queue
    
let new_sources_queue = 2
let good_old_sources_queue = 3
let concurrent_sources_queue = 4
let old_sources_queue = 5
let bad_sources_queue1 = 6
let bad_sources_queue2 = 7
let bad_sources_queue3 = 8
let bad_sources_queue4 = 9
let bad_sources_queue = 10



  
  
let nqueues = bad_sources_queue + 1
let sources_queues = Array.init nqueues (fun _ -> SourcesQueueCreate.fifo ())
let clients_queues = Array.init nqueues (fun _ -> SourcesQueueCreate.fifo ())
let sources_periods = Array.create nqueues 600
let sources_slots = Array.create nqueues 1
let sources_name = Array.create nqueues "sources"
  
let one_hour = 3600

let _ =
  
  sources_queues.(new_sources_queue) <- SourcesQueueCreate.lifo ();
  sources_queues.(good_old_sources_queue) <- SourcesQueueCreate.oldest_first ();  
  sources_queues.(old_sources_queue) <- SourcesQueueCreate.oldest_last ();  
  
  
  sources_periods.(immediate_connect_queue) <- 0;
  sources_periods.(good_clients_queue) <- 720;
  sources_periods.(new_sources_queue) <- 0;
  sources_periods.(concurrent_sources_queue) <- one_hour;
  sources_periods.(good_old_sources_queue) <- 720;
  sources_periods.(old_sources_queue) <- 720;
  sources_periods.(bad_sources_queue1) <- 720;
  sources_periods.(bad_sources_queue2) <- 1200;
  sources_periods.(bad_sources_queue3) <- one_hour;
  sources_periods.(bad_sources_queue4) <- 2 * one_hour;
  sources_periods.(bad_sources_queue) <- 12 * one_hour;
  
(* total is 31, with 5 clients_per_second, immediat connect is 6 seconds... *)
  sources_slots.(immediate_connect_queue) <- 30;
  sources_slots.(good_clients_queue) <- 100;
  sources_slots.(new_sources_queue) <- 100;
  sources_slots.(concurrent_sources_queue) <- 50;
  sources_slots.(good_old_sources_queue) <- 50;
  sources_slots.(old_sources_queue) <- 40;
  sources_slots.(bad_sources_queue1) <- 30;
  sources_slots.(bad_sources_queue2) <- 30;
  sources_slots.(bad_sources_queue3) <- 30;
  sources_slots.(bad_sources_queue4) <- 30;
  sources_slots.(bad_sources_queue) <- 10;
  
  sources_name.(immediate_connect_queue) <- "Immediat clients";
  sources_name.(good_clients_queue) <- "Good clients";
  sources_name.(new_sources_queue) <- "New sources";
  sources_name.(concurrent_sources_queue) <- "Concurrent Sources";
  sources_name.(good_old_sources_queue) <- "Good Old sources";
  sources_name.(old_sources_queue) <- "Old sources";
  sources_name.(bad_sources_queue1) <- "Bad sources 1";
  sources_name.(bad_sources_queue2) <- "Bad sources 2";
  sources_name.(bad_sources_queue3) <- "Bad sources 3";
  sources_name.(bad_sources_queue4) <- "Bad sources 4";
  sources_name.(bad_sources_queue) <- "Bad sources"
  
let reschedule_source s file =
(* This already known source has been announced has a source for a new file *)
  match s.source_client with
  | SourceClient c -> add_file_location file c; s
  
  | SourceLastConnection (queue, time, client_num) ->
      if sources_periods.(queue) > 601 || queue > old_sources_queue then
(* This source will not be connected early enough *)
(* TODO: if it is a popular file, don't schedule it too early *)
        let new_queue = 
          if time + sources_periods.(new_sources_queue) <= last_time () then
            new_sources_queue else old_sources_queue
        in
        let ss = { s with 
            source_client = SourceLastConnection (
              new_queue, time, client_num);
          } in
        lprintf "reschedule_source and invalidate old"; lprint_newline ();
        s.source_files <- []; (* Invalidate the old one *)
        H.remove sources s;
        H.add sources ss;
        Queue.put sources_queues.(new_queue) (time,s);
        ss
      else
(* The source is in a queue good for it *)
        s
        
        
let add_source_request = add_source_request

let queue_new_source new_queue last_conn addr file =
  let ip, port = addr in
  if !verbose_sources then begin
      lprintf "NEW SOURCE %s:%d" (Ip.to_string ip) port; lprint_newline ();
    end;
  try
    let finder =  { dummy_source with source_addr = addr } in
    let s = H.find sources finder in
    if not (has_source_request s file) then 
      let s = reschedule_source s file in
      s.source_files <- {
        request_file = file;
        request_result = File_new_source;
        request_time = 0;
      } :: s.source_files;
      s
    else
      s
  
  with _ ->
      
      let s = { dummy_source with
          source_num = (incr source_counter;!source_counter);
          source_age = last_time ();
          source_addr = addr;
          source_client = SourceLastConnection (
            new_queue, last_conn, 
            CommonClient.book_client_num ());
          source_files = [{
              request_file = file;
              request_result = File_new_source;
              request_time = 0;
            }];
        }  in
      H.add sources s;
      if !verbose_sources then begin
          lprintf "Source added"; lprint_newline ();
        end;
      Queue.put sources_queues.(new_queue) (last_conn, s);
      s

let new_source addr file = 
  queue_new_source new_sources_queue 0 addr file

let old_source _ last_conn addr file =   
  
  let new_queue = 
    if last_time () - last_conn < 720 then 
      good_old_sources_queue
    else old_sources_queue
  in
  
  queue_new_source new_queue last_conn addr file
      
let iter f =
  Array.iter (fun fifo ->
      Queue.iter (fun c ->
          match c.client_source with
            None -> () | Some s -> f s) fifo
  ) clients_queues;
  Array.iter (fun fifo ->
      Queue.iter f fifo
  ) sources_queues;
  Intmap.iter (fun _ c ->
      match c.client_source with
        None -> () | Some s -> f s)
  !outside_queue
  
  
  
let source_of_client c =
  
  outside_queue := Intmap.remove (client_num c) !outside_queue;
  
  if !verbose_sources then begin
      lprintf "source_of_client"; lprint_newline ();
    end;
(*
  if client_type c <> NormalClient then begin
      if !verbose_sources then begin
          lprintf "--> friends"; lprint_newline ();
        end;
      SourcesQueue.put clients_queues.(good_clients_queue) 
      (c, last_time ())
    end else *)
  match c.client_source with
    None -> 
      if !verbose_sources then begin
          lprintf "--> indirect"; lprint_newline ();
        end;
      raise Exit

(* This client is an indirect connection. Can't do anything with it. *)
  | Some s ->
      
      let ip, port = s.source_addr in
      if !verbose_sources then begin
          lprintf "Old source %s:%d" (Ip.to_string ip) port; lprint_newline ();
        end;
      let (files, downloading) = purge_requests c.client_files in
      c.client_files <- files;
      try
        let new_queue = 
          if client_type c <> NormalClient then good_clients_queue
          else
          if files = [] then begin
              H.remove sources s;
              raise Exit
            end else
          if keep_client c then good_clients_queue  else
          if List.exists (fun r ->
                r.request_result > File_new_source 
            ) c.client_files then concurrent_sources_queue
          else 
            c.client_next_queue
        in
        if new_queue <= last_clients_queue then begin
            if !verbose_sources then begin
                lprintf "--> client queue %d" new_queue; lprint_newline ();
              end;
            Queue.put clients_queues.(new_queue) (last_time (),c)
          end else
          begin
            if !verbose_sources then begin
                lprintf "--> source queue %d" new_queue; lprint_newline ();
              end;
            List.iter (fun r -> 
                remove_file_location r.request_file c) c.client_files;
            
            let last_conn = last_time () in
            s.source_client <- SourceLastConnection (
              new_queue, last_conn, client_num c);
            s.source_files <- c.client_files;
            Queue.put sources_queues.(new_queue) (last_conn, s)
          end
      with _ ->
          if !verbose_sources then begin
              lprintf "--> removed"; lprint_newline ();
            end;
          List.iter (fun r -> remove_file_location 
              r.request_file c) c.client_files
        
let reschedule_sources file =
  Array.iter (fun queue ->
      Queue.iter (fun s ->
          if has_source_request s file then
            ignore (reschedule_source s file)
      ) queue
  ) sources_queues
  
(* This will be configurable later... how many clients can we ask
in 10 minutes ? *)
let need_new_sources file =
  Queue.length sources_queues.(new_sources_queue) +
    Queue.length clients_queues.(good_clients_queue) < 
    600 * !!max_clients_per_second
  
  
  
let slots_counter = ref 0
    
  
(* Change a source structure into a client structure before attempting
  a connection. *)
let client_of_source reconnect_client s index client_num = 
  if !verbose_sources then begin
      lprintf "client_of_source"; lprint_newline ();
    end;
  let (files, downloading) = purge_requests s.source_files in
  if !verbose_sources then begin
      lprintf "Source for %d files" (List.length files); lprint_newline ();
    end;
  (files <> []) &&
  (if downloading then
      let (ip, port) = s.source_addr in
      let c = DonkeyGlobals.new_client_with_num (Known_location (ip,port))
        client_num in
      c.client_overnet <- s.source_overnet;
      if s.source_overnet then begin
          c.client_brand <- Brand_overnet;
        end;
      
      c.client_next_queue <- (
(* If the connection fails:
- if the client is a good client, try a new attempt shortly (old_sources_queue)
- if the client has never failed before, progressively delay connections
  (bad_sources_queue1)
- if the client is already too far, just let it where it is (bad_sources_queue)
*)
        if index < good_old_sources_queue then good_old_sources_queue else
        if index < bad_sources_queue1 then bad_sources_queue1 else
        if index = bad_sources_queue then bad_sources_queue else
          index+1
      );
      (match c.client_source with
          Some ss when s != ss -> 
            if !verbose_sources then begin
                lprintf "Client already has a source!"; lprint_newline ();
              end;
        |  _ -> ());
      c.client_source <- Some s;
      s.source_client <- SourceClient c;

(* This will be used after the connection to know where to put this client *)
      c.client_score <- 0;
      
      List.iter (fun r ->
          if r.request_result > File_not_found then begin
              add_file_location r.request_file c;
              match r.request_result with
              | File_new_source -> (* new_source := true *) ()
              | File_chunk | File_upload -> (* good_source := true *) ()
              | _ -> ()
            end;
      ) c.client_files;
      
      useful_client source_of_client reconnect_client c
    else
      false
  )
  
(* This function is called every second *)
let check_sources1 reconnect_client =

(* We should first check if we have some download bandwidth available 
and if we have not exceeded the max number of clients   *)

(* Find the total number of slots. Should be only computed once... *)
  let nsources = ref 0 in  
  let nslots = ref 0 in
  for i = 0 to nqueues - 1 do 
    if !verbose_sources then begin
        lprintf "queue[%s]: %d sources" sources_name.(i)
        (let n = if i <= last_clients_queue then
              Queue.length clients_queues.(i)
            else 
              Queue.length sources_queues.(i) in
          nsources := !nsources + n; n);
        lprint_newline ();
      end;
    nslots := !nslots + sources_slots.(i);
  done;
  let nslots = !nslots in
  if !verbose_sources then begin
      lprintf "nslots: %d nsources:%d" nslots !nsources; lprint_newline ();
      end;
  
  
(* Find which queue we have to start with *)
  let slot = !slots_counter mod nslots in    
  let rec iter_slots slot index =
    if sources_slots.(index) <= slot then
      iter_slots (slot - sources_slots.(index)) (index+1)
    else
      (index, slot)
  in
  let index, slot = iter_slots slot 0 in
  if !verbose_sources then begin
      lprintf "index=%d slot=%d" index slot; lprint_newline ();
    end;
  
  let next_slot nclients index slot =
(* The connection has been sent, move to next slot *)
    let slot = slot + 1 in
    incr slots_counter;
    if slot >= sources_slots.(index) then
      let index = index + 1 in
      (nclients-1, index, 0)
    else
      (nclients-1, index, slot)
  in

(* test this slot *)  
  let rec iter nclients index slot =
    if !verbose_sources then begin
        lprintf "iter nclients:%d index:%d slot:%d (slots_counter: %d)" nclients index slot !slots_counter;
        lprint_newline ();
      end;
    if CommonGlobals.can_open_connection () && index < nqueues then begin
        if nclients > 0 then begin
            
            let (nclients, index, slot) =
              try
                
                if index <= last_clients_queue then
                  
                  let (time, c) = Queue.head clients_queues.(index) in
                  if time + sources_periods.(index) <= last_time () then
                    let _ = Queue.take clients_queues.(index) in
                    
                    if useful_client source_of_client reconnect_client c then
                      next_slot nclients index slot
                    else (nclients, index, slot)
                  
                  else raise Not_found
                else
                
                let _, s = Queue.head sources_queues.(index) in
                let ip, port = s.source_addr in
                if !verbose_sources then begin
                    lprintf "One source %s:%d from queue[%s]" (Ip.to_string ip) port sources_name.(index); lprint_newline ();
                  end;
                if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
                    if !verbose_sources then begin
                        lprintf "Source invalidated"; lprint_newline ();
                      end;
                    let s = Queue.take sources_queues.(index) in
                    (nclients, index, slot)
                  end else
                match s.source_client with
                | SourceLastConnection (queue, time, client_num) ->
                    
                    if time + sources_periods.(index) <= last_time () then
(* This source is good, connect to it !!! *)
                      let _, s = Queue.take sources_queues.(index) in
                      if !verbose_sources then begin
                          lprintf "Source could be connected %d" time; lprint_newline ();
                        end;
                      
                      if client_of_source reconnect_client s queue client_num then
                        
                        next_slot nclients index slot
                      
                      else
(* For some reason, the connection has been aborted, retry *)
                        (nclients, index, slot)
                    
                    else begin
(* Too early to connect to this source, move to the next queue  *)
                        if !verbose_sources then begin
                            lprintf "Too early for this source %d" time; lprint_newline ();
                          end;
                        raise Not_found       
                      end
                
                | _ -> 
(* This source is already connected, remove it immediatly, and retry *)
                    let s = Queue.take sources_queues.(index) in
                    (nclients, index, slot)
              with _ -> 
(* for some reason, we didn't find a good client in this queue, go to the
next one *)
                  slots_counter := !slots_counter + sources_slots.(index) - slot;
                  let index = index+1 in
                  (nclients, index, 0)
            in
            iter nclients index slot
          
          end      
      end    
  in
  
  iter !!max_clients_per_second index slot

  
let check_sources2 reconnect_client =

(* Find the total number of slots. Should be only computed once... *)
  let nsources = ref 0 in  
  let nslots = ref 0 in
  for i = 0 to nqueues - 1 do 
    if !verbose_sources then begin
        lprintf "queue[%s]: %d sources" sources_name.(i)
        (let n = if i <= last_clients_queue then
              Queue.length clients_queues.(i)
            else 
              Queue.length sources_queues.(i) in
          nsources := !nsources + n; n);
        lprint_newline ();
      end;
    nslots := !nslots + sources_slots.(i);
  done;
  let nslots = !nslots in
  if !verbose_sources then begin
      lprintf "nslots: %d nsources:%d" nslots !nsources; lprint_newline ();
    end;
    
  let rec iter respect_timers nclients index = 
    if  CommonGlobals.can_open_connection () &&  nclients > 0 then
      if index < nqueues then
        let (nclients, index) = 
          if !verbose_sources then begin
              lprintf "Testing queue %s (%d possible connections)"
                sources_name.(index) nclients
              ; lprint_newline ();
            end;
          try
            if index <= last_clients_queue then
              
              let (time, c) = Queue.head clients_queues.(index) in
              if time + sources_periods.(index) <= last_time () then
                let _ = Queue.take clients_queues.(index) in
                
                if useful_client source_of_client reconnect_client c then
                  (nclients-1, index)
                else (nclients, index)
              
              else raise Not_found
            else
            
            let _, s = Queue.head sources_queues.(index) in
            let ip, port = s.source_addr in
            if !verbose_sources then begin
                lprintf "One source %s:%d from queue[%s]" (Ip.to_string ip) port sources_name.(index); lprint_newline ();
              end;
            if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
                if !verbose_sources then begin
                    lprintf "Source invalidated"; lprint_newline ();
                  end;
                let s = Queue.take sources_queues.(index) in
                (nclients, index)
              end else
            match s.source_client with
            | SourceLastConnection (queue, time, client_num) ->
                
                if time + 
                    (if respect_timers then
                      sources_periods.(index)
                    else mini 720 sources_periods.(index) ) <= last_time () then
(* This source is good, connect to it !!! *)
                  let _, s = Queue.take sources_queues.(index) in
                  if !verbose_sources then begin
                      lprintf "Source could be connected %d" time; lprint_newline ();
                    end;
                  
                  if client_of_source reconnect_client s queue client_num then
                    
                    (nclients-1, index)
                  
                  else
(* For some reason, the connection has been aborted, retry *)
                    (nclients, index)
                
                else begin
(* Too early to connect to this source, move to the next queue  *)
                    if !verbose_sources then begin
                        lprintf "Too early for this source %d" time; lprint_newline ();
                      end;
                    raise Not_found       
                  end
            
            | _ -> 
(* This source is already connected, remove it immediatly, and retry *)
                let s = Queue.take sources_queues.(index) in
                (nclients, index)
          with _ -> 
              lprintf "Exception: go to next queue"; lprint_newline ();
              (nclients, index+1)
        in
        iter respect_timers nclients index
      else
      if respect_timers then 
        iter false nclients index
  in
  iter true !!max_clients_per_second immediate_connect_queue

    
let print_sources buf =
  let nsources = ref 0 in  
  for i = 0 to nqueues - 1 do 
    Printf.bprintf buf "Queue[%s]: %d sources\n" sources_name.(i)
    (let n = if i <= last_clients_queue then
          Queue.length clients_queues.(i)
        else 
          Queue.length sources_queues.(i) in
      nsources := !nsources + n;
      n)
  done;
  
  let n = Intmap.length !outside_queue in
  Printf.bprintf buf "  Outside of queues: %d sources\n" n;
  nsources := !nsources + n;
  
  Printf.bprintf buf "\nTotal number of sources:%d\n" !nsources

let check_sources = check_sources2


let init () = ()
  