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
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals
open DonkeySourcesMisc

(* Here the semantics of the fields in the source structure are different:
source_client = SourceLastConnection (basic_score, last_attempt, client_num)
source_score = / basic_score when source_client = SourceClient
               \ current_score
*)    


module SourcesSet = Set.Make (
    struct
      type t = source
      let compare s1 s2 = 
        if s1.source_addr = s2.source_addr then begin
            (if !verbose_sources then
                if s1.source_num <> s2.source_num then begin
                    lprintf "same addr for different sources!"; 
                    lprint_newline ();
                    exit 2
                  end);
            0 end else
        let result = 
          if s1.source_score = s2.source_score then
            compare s1.source_addr s2.source_addr
          else
            s1.source_score - s2.source_score
        in
        if !verbose_sources && result = 0 then begin
            lprintf "Two different sources are compared equally"; 
            lprint_newline ();
            exit 2
          end;
        result
    end
  )

let old_source_score = 0
let new_source_score = 100
let friend_source_score = 1000

let compare_sources s1 s2 =
  if s1.source_addr = s2.source_addr then 0 else
    s1.source_score - s2.source_score

let clients_queue = Fifo.create ()
let ready_sources = ref SourcesSet.empty

let queues = [| (-20, 10); (-40, 30); (-1000, 60) |]
let waiting_sources = Array.init (Array.length queues) (fun _ ->
      Fifo.create  ())

let add_source_request = add_source_request

let rec find_source_queue score pos len =
  if pos = len then raise SourceTooOld;
  let (min_score, period) = queues.(pos) in
  if min_score <= score then pos else 
    find_source_queue score (pos+1) len

let source_queue s =
  match s.source_client with 
  | SourceClient _ -> raise Not_found
  | SourceLastConnection (basic_score,_,_) ->
      find_source_queue basic_score 0 (Array.length queues)

exception UselessSource

let score s = 
  begin
    if !verbose_sources then begin
        lprintf "score %d" s.source_num; lprint_newline ();
      end;
    match s.source_client with
    | SourceClient _ -> assert false          
    | SourceLastConnection (basic_score, last_attempt, client_num) ->
        s.source_score <- basic_score 
          + (last_time () - last_attempt) / 60
        ;
        if !verbose_sources then begin
            lprintf "  initial score: %d" s.source_score; lprint_newline ();
          end;
        let useful_source = ref false in
        List.iter (fun r ->
            
            let popularity = 1 + try !
                  (Intmap.find (file_num r.request_file)
                  !stats_saved_files) * 100
                  / !stats_saved_files_size
              with _ -> 0 in
            s.source_score <- s.source_score + 
              (match file_state r.request_file  with
                FileDownloading -> useful_source := true; 2
              | FilePaused | FileAborted _ -> useful_source := true; 1
              | _ -> 0) *
            (maxi 1 (file_priority r.request_file + 10)) *
            (match r.request_result with
                File_not_found | File_possible -> 0
              | File_expected -> 1
              | File_found -> 10
              | File_chunk -> 20
              | File_upload -> 30
              | File_new_source -> 15
            ) / popularity;
            if !verbose_sources then begin
                lprintf "  request %s result %s" (Md4.Md4.to_string r.request_file.file_md4) (string_of_result r.request_result);
                lprint_newline ();
              end;
        ) s.source_files;
        if not !useful_source then raise UselessSource;
        let (ip,port) = s.source_addr in
        if !verbose_sources then begin
            lprintf "Score for %d(%s:%d) = %d/%d" s.source_num (Ip.to_string ip) port 
              s.source_score basic_score;
            lprint_newline ();
          end;
  end

let reschedule_source s file =
  if !verbose_sources then begin
      lprintf "reschedule_source %d" s.source_num; lprint_newline ();
    end;
  if SourcesSet.mem s !ready_sources then begin
      ready_sources := SourcesSet.remove s !ready_sources ;
      score s;
      ready_sources := SourcesSet.add s !ready_sources;
    end 

let client_connected c =
  c.client_score <- 0;
  match c.client_source with None -> () | Some s ->
      s.source_age <- last_time ()

let queue_new_source new_score source_age addr file =
  let ip, port = addr in
  if !verbose_sources then begin
      lprintf "queue_new_source %s:%d" (Ip.to_string ip) port; 
      lprint_newline ();
    end;
  try
    let finder =  { dummy_source with source_addr = addr } in
    let s = H.find sources finder in
    
    incr stats_new_sources;
    
    if not (has_source_request s file) then begin
        s.source_files <- {
          request_file = file;
          request_time = 0;
          request_result = File_new_source;
        } :: s.source_files;
        reschedule_source s file;
        s
      end
    else
      s
  
  with _ ->
      let s = { dummy_source with
          source_num = (incr source_counter;!source_counter);
          source_addr = addr;
          source_age = source_age;
          source_client = SourceLastConnection (
            new_score, last_time (), CommonClient.book_client_num ());
          source_files = [{
              request_file = file;
              request_time = 0;
              request_result = File_new_source;
            }]
        }  in
      H.add sources s;
      incr stats_sources;
      if !verbose_sources then begin
          lprintf "Source %d added" s.source_num; lprint_newline ();
        end;
      score s;
      ready_sources := SourcesSet.add s !ready_sources;
      s

let old_source old_source_score source_age addr file = 
  queue_new_source (old_source_score-50) source_age addr file

let new_source addr file = 
  queue_new_source new_source_score (last_time()) addr file

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
      List.iter (fun r -> 
          remove_file_location r.request_file c) c.client_files;
      c.client_files <- [];
      
      raise Exit
  
  | Some s ->
      
      let ip, port = s.source_addr in
      if !verbose_sources then begin
          lprintf "Old source %s:%d" (Ip.to_string ip) port; 
          lprint_newline ();
        end;
      let (files, downloading) = purge_requests c.client_files in
      c.client_files <- files;
      try
        
        if keep_client c then begin
            if !verbose_sources then begin
                lprintf "%d --> kept" (client_num c); lprint_newline ();
              end;
            
            Fifo.put clients_queue (c, last_time ())
          
          end else
        if not (List.exists (fun r -> r.request_result > File_not_found)
            files) then begin
            H.remove sources s;
            incr stats_remove_useless_sources;
            raise Exit
          end else
        let basic_score = c.client_score in
        if !verbose_sources then begin
            lprintf "%d --> new score %d" (client_num c) basic_score; lprint_newline ();
          end;
        List.iter (fun r -> remove_file_location r.request_file c) 
        c.client_files;
        if !verbose_sources then begin
            lprintf "Set SourceLastConnection for source %d" 
              s.source_num; 
            lprint_newline ();
          end;
        s.source_client <- SourceLastConnection (
          basic_score, last_time (), client_num c);
        s.source_files <- c.client_files;
        c.client_files <- [];
        try
          Fifo.put waiting_sources.(source_queue s) s;
          score s
        with SourceTooOld ->
            if !verbose_sources then begin
                lprintf "Removed old source %d" s.source_num;
                lprint_newline ();
              end;
            H.remove sources s;
            incr stats_remove_old_sources;
      
      with _ ->
          if !verbose_sources then begin
              lprintf "%d --> removed" (client_num c); lprint_newline ();
            end;
          List.iter (fun r -> 
              remove_file_location r.request_file c) c.client_files;
          c.client_files <- []

let reschedule_sources files = 
  if !verbose_sources then begin      
      lprintf "reschedule_sources file not implemented";
      lprint_newline () 
    end

(* Change a source structure into a client structure before attempting
  a connection. *)
let client_of_source reconnect_client s basic_score client_num = 
  if !verbose_sources then begin
      lprintf "client_of_source %d" s.source_num; lprint_newline ();
    end;
  let (files, downloading) = purge_requests s.source_files in
  if !verbose_sources then begin
      lprintf "Source for %d files" (List.length files); 
      lprint_newline ();
    end;
  (if downloading then
      let (ip, port) = s.source_addr in
      let c = DonkeyGlobals.new_client_with_num (Known_location (ip,port))
        client_num in
      c.client_next_queue <- 0;
      
      c.client_overnet <- s.source_overnet;
      if s.source_overnet then begin
          c.client_brand <- Brand_overnet;
        end;
      (match c.client_source with
          Some ss when s != ss -> 
            if !verbose_sources then begin
                lprintf "Client already has a source!"; lprint_newline ();
              end;
        |  _ -> ());
      c.client_source <- Some s;
      if !verbose_sources then begin
          lprintf "set SourceClient for source %d" s.source_num;
          lprint_newline ();
        end;
      
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
    else
    try (* put back the sources in the waiting_queues. Not a good idea,
        we should have another table for them, to wait until the file
        is resumed, or another file can be downloaded from them. *)
      Fifo.put waiting_sources.(source_queue s) s;
      false
    with SourceTooOld -> false
  )

let recompute_ready_sources () =
  if !verbose_sources then begin
      lprintf "recompute_ready_sources"; lprint_newline ();
    end;
  let t1 = Unix.gettimeofday () in

(* Very simple *)
  let previous_sources = !ready_sources in
  let list = ref [] in
  let rec iter i =
    let s = try Fifo.head waiting_sources.(i) with _ -> raise Not_found in
    match s.source_client with
      SourceLastConnection (_,time,_) ->
        if time + 720 < last_time () then
          let s = Fifo.take waiting_sources.(i) in
          list := s :: !list;
          iter i
    | SourceClient c -> 
        if !verbose_sources then begin
            lprintf "ERROR: CLIENT %d" (client_num c); lprint_newline ();
            assert false
          end
  in
  for i = 0 to Array.length queues - 1 do
    (try iter i   with Not_found -> ());
  done;
  ready_sources := SourcesSet.empty;
  let add_source s =
    if s.source_age + !!max_sources_age * half_day < last_time () then begin
        if !verbose_sources then begin
            lprintf " --> drop source too old"; lprint_newline ();
          end;
        H.remove sources s;
        incr stats_remove_too_old_sources;
      end else begin
        try
          score s; 
          ready_sources := SourcesSet.add s !ready_sources
        with UselessSource ->
            H.remove sources s;
            incr stats_remove_useless_sources
      end
  in        
  SourcesSet.iter add_source previous_sources;
  List.iter add_source !list;
  let t2 = Unix.gettimeofday () in
  if !verbose_sources then begin
      lprintf "Delay for Sources: %2.2f" (t2 -. t1); lprint_newline ();
    end


let check_sources reconnect_client = 
  
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
  
  let rec iter_clients nclients = 
    if CommonGlobals.can_open_connection () && nclients > 0 then begin
        try
          let (c, time) = Fifo.head clients_queue in
          if time + 720 <= last_time () then
            let _ = Fifo.take clients_queue in
            
            incr stats_connect_good_clients;
            stats_register_files c.client_files;
            
            if useful_client source_of_client reconnect_client c then
              iter_clients (nclients-1)
            else iter_clients nclients
          
          else raise Fifo.Empty
        
        with Fifo.Empty ->
            iter_sources nclients
      end
  
  and iter_sources nclients = 
    if !verbose_sources then begin
        lprintf "iter_sources %d" nclients; lprint_newline ();
      end;
    
    if CommonGlobals.can_open_connection () && nclients > 0 then begin
        let s = SourcesSet.max_elt !ready_sources in
        ready_sources := SourcesSet.remove s !ready_sources;
        
        
        let ip, port = s.source_addr in
        if !verbose_sources then begin
            lprintf "One source %d[%s:%d] from ready_sources" 
              s.source_num
              (Ip.to_string ip) port; 
            lprint_newline ();
          end;
        
        
        if !verbose_sources then begin
            if SourcesSet.mem s !ready_sources then begin
                lprintf "Source %d is still in ready_sources after remove" s.source_num; lprint_newline ();
              end;
          end;
        let ss = SourcesSet.max_elt !ready_sources in
        if !verbose_sources then begin
            lprintf "next max = %d" s.source_num; lprint_newline ();
          end;                
        match s.source_client with
        | SourceLastConnection (basic_score, time, client_num) ->
            s.source_score <- basic_score;
            stats_register_files s.source_files;
            if client_of_source reconnect_client s basic_score client_num
            then
              iter_sources (nclients-1)
            else
              iter_sources nclients                  
        
        | SourceClient c -> 
            if !verbose_sources then begin                    
                lprintf "ERROR: CLIENT %d" (client_num c); 
                lprint_newline ();
                assert false
              end
      
      end
  in
  try
    iter_clients !!max_clients_per_second
  with Not_found -> ()
  | e ->
      if !verbose_sources then begin
          lprintf "Exception %s in check_sources" (Printexc2.to_string e);
          lprint_newline ()
        end

let need_new_sources file = 
  Fifo.length clients_queue < 720

let iter f =
  Intmap.iter (fun _ c ->
      match c.client_source with
        None -> () | Some s -> f s)
  !outside_queue;
  Fifo.iter (fun (c,_) ->
      match c.client_source with
        None -> () | Some s -> f s
  ) clients_queue;
  SourcesSet.iter f !ready_sources;
  Array.iter (fun fifo -> Fifo.iter f fifo) waiting_sources

let print_sources buf =
  let ngood_clients = Fifo.length clients_queue in  
  Printf.bprintf buf "Queue[Good Clients]: %d sources\n" 
    ngood_clients;
  let nready_sources = SourcesSet.cardinal !ready_sources in
  Printf.bprintf buf "Queue[Ready Sources]: %d sources\n" 
    nready_sources;
  
  let total_nwaiting_sources = ref 0 in
  for i = 0 to Array.length queues - 1 do
    let nwaiting_sources = Fifo.length waiting_sources.(i) in
    total_nwaiting_sources := !total_nwaiting_sources + nwaiting_sources;
    Printf.bprintf buf "Queue[Waiting Sources %d]: %d sources\n" 
      i nwaiting_sources;
  done;
  
  let noutside_queue = Intmap.length !outside_queue in
  Printf.bprintf buf "  Outside of queues: %d sources\n" noutside_queue;
  
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
  Printf.bprintf buf "     Good sources: %d[%d]\n" 
    !stats_connect_good_sources
    !stats_saved_connect_good_sources;
  Printf.bprintf buf "     New sources: %d/%d[%d/%d]\n" 
    !stats_connect_new_sources !stats_new_sources
    !stats_saved_connect_new_sources !stats_saved_new_sources;
  Printf.bprintf buf "     Old sources: %d[%d]\n" 
    !stats_connect_old_sources
    !stats_saved_connect_old_sources;
  
  Printf.bprintf buf "By files:\n";
  let stats_connect_old_file = ref 0 in
  Intmap.iter (fun file_num n ->
      try
        let file = CommonFile.file_find file_num in
        let old_n = try !(Intmap.find file_num !stats_saved_files) with 
            _ -> 0
        in
        Printf.bprintf buf "  %-60s  %d[%d]\n" 
          (CommonFile.file_best_name file) !n old_n
      with _ -> incr stats_connect_old_file
  ) !stats_files;
  Printf.bprintf buf " Old files:  %d\n" !stats_connect_old_file;
  
  Printf.bprintf buf "\nTotal number of sources:%d\n" 
    (noutside_queue + ngood_clients + nready_sources + 
      !total_nwaiting_sources)
  
let init () = ()