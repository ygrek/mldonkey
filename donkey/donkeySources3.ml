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

open Md4
open Options
open CommonOptions
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals
open DonkeySourcesMisc
  
let add_new_source new_score source_age addr = 
  let ip, port = addr in
  if !verbose_sources then begin
      Printf.printf "queue_new_source %s:%d" (Ip.to_string ip) port; 
      print_newline ();
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
          Printf.printf "Source %d added" s.source_num; print_newline ();
        end;
      s


let new_sources_queue = 0
let good_saved_sources_queue = 1
let old_saved_sources_queue = 2
let old_sources1_queue = 3
let old_sources2_queue = 4
let old_sources3_queue = 5
let old_sources4_queue = 6
let nqueues = 7
  
  
let queue_name = [|
    "new_sources";
    "good_saved_sources";
    "old_saved_sources";
    "old_sources1";
    "old_sources2";
    "old_sources3";
    "old_sources4";
  |]
  
let queue_period = [|
    0;
    720;
    0;
    720;
    720;
    1200;
    2400
  |]
  
let need_new_sources file = 
  Fifo.length file.file_clients + 
  SourcesQueue.length file.file_sources.(new_sources_queue) < 200

let add_source_request s file time result =
  try
    let r = find_source_request s file in
    r.request_result <- result;
    r.request_time <- time;
  with _ ->
      add_request s file time result;
      SourcesQueue.put file.file_sources.(match result with
        File_new_source -> new_sources_queue
        | _ -> 
            if last_time () - time < !!min_reask_delay then
              good_saved_sources_queue
            else 
              old_saved_sources_queue) (time, s)

let old_source old_source_score source_age addr file = 
  (*
  Printf.printf "New source connected %d seconds ago" (last_time () - source_age);
  print_newline ();
*)
  
  let s = add_new_source old_source_score source_age addr in
  add_source_request s file source_age File_expected;
  s

let new_source  addr file = 
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
      Array.iter (fun ss -> SourcesQueue.iter f ss) file.file_sources;
  ) !current_files


let source_of_client c = 
  outside_queue := Intmap.remove (client_num c) !outside_queue;      
  
  if !verbose_sources then begin
      Printf.printf "source_of_client %d" (client_num c); print_newline ();
    end;
  
  match c.client_source with
    None -> 
(* This client is an indirect connection. Can't do anything with it. *)
      
      if !verbose_sources then begin
          Printf.printf "%d --> indirect" (client_num c); print_newline ();
        end;
      List.iter (fun r -> 
          remove_file_location r.request_file c) c.client_files;
      c.client_files <- [];
      c.client_from_queues <- [];      
      raise Exit
  
  | Some s ->
      
      let ip, port = s.source_addr in
      if !verbose_sources then begin
          Printf.printf "Old source %s:%d score: %d" (Ip.to_string ip) port
            c.client_score; 
          print_newline ();
        end;
      let (files, downloading) = purge_requests c.client_files in
      c.client_files <- files;
      try      
        let keep_client = ref false in
        let useful_source = ref false in
        List.iter (fun file ->
            try
              let r = find_client_request c file in
              match r.request_result with
                File_possible | File_not_found -> ()
              | File_new_source | File_expected ->
                  let fifo = 
                    if c.client_score >= -10 then 
                      old_sources1_queue
                    else 
                    if c.client_score >= -20 then
                      old_sources2_queue
                    else
                    if c.client_score >= -40 then
                      old_sources3_queue                    
                    else begin
                        c.client_files <- List2.removeq r c.client_files;
                        remove_file_location file c;
                        raise Exit
                      end
                  in
                  useful_source := true;
                  SourcesQueue.put file.file_sources.(fifo) (last_time() , s)
              
              | File_chunk | File_found ->
                  keep_client := true;
                  useful_source := true;
                  if !verbose_sources then begin
                      Printf.printf "%d --> kept (source)" (client_num c); print_newline ();
                    end;
                  
                  Fifo.put file.file_clients (c, last_time ())
              | File_upload ->
                  keep_client := true;
                  useful_source := true;
                  if !verbose_sources then begin
                      Printf.printf "%d --> kept (uploader)" (client_num c); print_newline ();
                    end;
                  
                  Fifo.put file.file_clients (c, last_time ())
            
            with Exit -> ()
        ) c.client_from_queues;
        c.client_from_queues <- [];
        if not !keep_client then begin
            let basic_score = c.client_score in
            List.iter (fun r -> remove_file_location r.request_file c) 
            c.client_files;
(*
            if !verbose_sources then begin
                Printf.printf "Set SourceLastConnection for source %d" 
                  s.source_num; 
                print_newline ();
              end; *)
            s.source_client <- SourceLastConnection (
              basic_score, last_time (), client_num c);
            s.source_files <- c.client_files;
            c.client_files <- [];            
          end;
        if not !useful_source then begin
            incr stats_remove_useless_sources;
            raise Exit
          end;
        
      with _ ->          
          if !verbose_sources then begin
              Printf.printf "%d --> removed" (client_num c); print_newline ();
            end;
          H.remove sources s;
          s.source_files <- [];
          List.iter (fun r -> 
              remove_file_location r.request_file c) c.client_files;
          c.client_files <- [];
          c.client_from_queues <- []
                  
let client_connected c =
  c.client_score <- 0;
  match c.client_source with None -> () | Some s ->
      s.source_age <- last_time ()
      
let recompute_ready_sources f =
  Printf.printf "recompute_ready_sources on sources not implemented"; print_newline ()

(* Change a source structure into a client structure before attempting
  a connection. *)
let client_of_source reconnect_client s file basic_score client_num = 
  
  if !verbose_sources then begin
      Printf.printf "client_of_source %d" s.source_num; print_newline ();
    end;
  let (files, downloading) = purge_requests s.source_files in
  if !verbose_sources then begin
      Printf.printf "Source for %d files" (List.length files); 
      print_newline ();
    end;
  let (ip, port) = s.source_addr in
  let c = DonkeyGlobals.new_client_with_num (Known_location (ip,port))
    client_num in
  c.client_next_queue <- 0;
  c.client_from_queues <- file :: c.client_from_queues;
  
  c.client_overnet <- s.source_overnet;
  if s.source_overnet then begin
      c.client_brand <- Brand_overnet;
    end;
  (match c.client_source with
      Some ss when s != ss -> 
        if !verbose_sources then begin
            Printf.printf "Client already has a source!"; print_newline ();
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
        
        incr stats_connect_good_clients;
        stats_register_files c.client_files;

        if !verbose_sources then begin
            Printf.printf "Source: Good Client of %s" (file_best_name file); 
            print_newline ();
          end;

        (useful_client source_of_client reconnect_client c) || (iter_client ())
      
      else begin (*
          Printf.printf "Client can not be connected: %d" wait_for;
          print_newline (); *)
          raise Fifo.Empty
        end
    in
    iter_client ()
  with Fifo.Empty ->
      let rec iter_source i =
(*        Printf.printf "iter_source %d" i; print_newline (); *)
        if i < nqueues then
          try
            
            let _, s = SourcesQueue.head file.file_sources.(i) in
(*
Printf.printf "Checking source from queue[%s]" queue_name.(i); print_newline (); 
  *)
            let ip, port = s.source_addr in
            if !verbose_sources then begin
                Printf.printf "One source %s:%d from queue[%s]" (Ip.to_string ip) port queue_name.(i); print_newline ();
              end;
            if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
                if !verbose_sources then begin
                    Printf.printf "Source invalidated"; print_newline ();
                  end;
                let s = SourcesQueue.take file.file_sources.(i) in
                raise Not_found
              end else
            match s.source_client with
            | SourceLastConnection (basic_score, time, client_num) ->
                
                let wait_for = time + queue_period.(i) - last_time () in
                if wait_for < 0 then
(* This source is good, connect to it !!! *)
                  let _, s = SourcesQueue.take file.file_sources.(i) in
                  if !verbose_sources then begin
                      Printf.printf "Source could be connected (last %d)" 
                        (last_time () - time); print_newline ();
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
                    Printf.printf "Source %d: queue[%s] of file %s (last conn %d)" client_num queue_name.(i)
                      (file_best_name file)  (last_time () - time)
                      ; print_newline (); 
                    end;
                  
                  if client_of_source reconnect_client s file basic_score client_num then
                    true
                  else raise Not_found
                else begin
(* Too early to connect to this source, move to the next queue  *)
                    if !verbose_sources then begin
                        Printf.printf "Too early for this source %d (last %d)" wait_for (last_time () - time); print_newline ();
                      end;
                    raise Fifo.Empty
                  end
                  
            | SourceClient c -> 
(* This source is already connected, remove it immediatly, and retry *)
                let s = SourcesQueue.take file.file_sources.(i) in
                c.client_from_queues <- file :: c.client_from_queues;
                raise Not_found
            
          with
          | Fifo.Empty -> iter_source (i+1)
          | Not_found -> iter_source i
          | e -> 
              Printf.printf "Exception %s" (Printexc2.to_string e); 
              print_newline ();
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

  
  let positive_sources = ref 0 in
  let negative_sources = ref 0 in
  let nchunks = ref 0 in
  let nupload = ref 0 in
  let nfound = ref 0 in
  let nnotfound = ref 0 in
  iter (fun s ->
      (match s.source_client with 
          SourceClient c -> s.source_files <- c.client_files;
        | _ -> ());
      
      List.iter (fun r ->
          match r.request_result with
            File_not_found -> incr nnotfound
          | File_found -> incr nfound
          | File_chunk -> incr nchunks
          | File_upload -> incr nupload
          | _ -> ()
      ) s.source_files;
      
      if s.source_score >= 0 then incr positive_sources else
        incr negative_sources);
  
  Printf.bprintf buf "Positive/Negative: %d/%d\n" !positive_sources
    !negative_sources; 
  Printf.bprintf buf "NotFound/Found/Chunk/Upload: %d/%d/%d/%d\n\n"
    !nnotfound !nfound !nchunks !nupload;
  Printf.bprintf buf "Ranks: ";
  for i = 0 to 9 do
    Printf.bprintf buf " %d[%d]" !stats_ranks.(i) !stats_saved_ranks.(i)
  done;
  Printf.bprintf buf "\n";
  
  Printf.bprintf buf "Removed Sources (on %d): useless %d/old %d/too old %d\n"
    !stats_sources
    !stats_remove_useless_sources !stats_remove_old_sources
    !stats_remove_too_old_sources;
  
  
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
  Printf.bprintf buf "By files:\n";
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
              let queue_size = SourcesQueue.length q in
              per_queue.(i+1) <- per_queue.(i+1) + queue_size;
              nsources := !nsources + queue_size;
              Printf.bprintf buf "     Queue[%s] : %d sources (next %s)\n" 
                queue_name.(i) queue_size
              (try
                  let _,s = SourcesQueue.head q in
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
  
  ) !current_files;

  Printf.bprintf buf "\nPer queue:\n";
  Printf.bprintf buf "   Queue[Clients]: %d clients\n" per_queue.(0);
  for i = 0 to nqueues - 1 do
    Printf.bprintf buf "   Queue[%s]: %d sources\n" 
      queue_name.(i)
    per_queue.(i+1);
  done;
  
  Printf.bprintf buf "\nTotal number of sources:%d\n" 
    (noutside_queue + !nsources)

      
let check_sources reconnect_client = 

  (*
  let buf = Buffer.create 100 in
  print_sources buf;
  Printf.printf "\n\nSTATS: %s" (Buffer.contents buf);
  print_newline ();
*)
  
  let uptime = last_time () - start_time in
  
  if uptime mod 60 = 0 then
    recompute_ready_sources ();
  
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
  
  let rec iter_first n =
    if CommonGlobals.can_open_connection () && n > 0 then 
      match !source_files with
        [] -> 
          source_files := !current_files;
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
              source_files := !current_files;
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
  Printf.printf "reschedule_sources on sources not implemented"; print_newline ()
*)  
  
let init () =
  queue_period.(good_saved_sources_queue) <-  !!min_reask_delay;
  queue_period.(old_sources1_queue) <-  !!min_reask_delay;
  queue_period.(old_sources2_queue) <-  !!min_reask_delay;
