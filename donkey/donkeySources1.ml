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

open Options
open CommonOptions
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals

let verbose_sources = verbose_src_manager
let source_counter = ref 0
        
  
let outside_queue = ref (Intmap.empty: client Intmap.t)
      
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
    
let has_source_request s file =
  try
    ignore (iter_has_request s.source_files file); true
  with _ -> false
  
let find_source_request s file = iter_has_request s.source_files file

let add_source_request s file time result =
  try
    let r = find_source_request s file in
    r.request_result <- result;
    r.request_time <- time;
  with _ ->
      let r = {
          request_file = file;
          request_time = time;
          request_result = result;
        } in
      s.source_files <- r :: s.source_files

let set_request_result c file rs =
  List.iter (fun r ->
      if r.request_file == file then
        r.request_result <- rs
  ) c.client_files
  
let query_file sock c file =
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
    if r.request_time + !!min_reask_delay < last_time () then 
      match r.request_result with
        File_not_found when last_time () - r.request_time > 3600 -> ()
(* one hour ago, it did not have the file.... *)
      | _ ->
          DonkeyProtoCom.direct_client_send sock (
            let module M = DonkeyProtoClient in
            M.QueryFileReq file.file_md4);      
          DonkeyProtoCom.direct_client_send sock (
            let module M = DonkeyProtoClient in
            M.QueryChunksReq file.file_md4);      
          
          r.request_time <- last_time ();
          match r.request_result with
            File_possible -> ()
          | _ -> r.request_result <- File_expected

let add_file_location file c =
  if not (Intmap.mem (client_num c) file.file_locations) then begin
      file.file_locations <- Intmap.add (client_num c) c file.file_locations;
      CommonFile.file_add_source (CommonFile.as_file file.file_file) 
      (CommonClient.as_client c.client_client);
      match c.client_sock, client_state c with
        Some sock, (Connected_busy | Connected_idle | Connected_queued) ->
          query_file sock c file
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
      
module SourcesQueue = struct
    type 'a t = {
        head : (unit -> 'a);
        put : ('a -> unit);
        length : (unit -> int);
        take : (unit -> 'a);
        iter : ( ('a -> unit) -> unit);
        put_back : ('a -> unit);
      }
      
    let head t = t.head ()
    let put t x = t.put x
    let iter f t = t.iter f
    let length t = t.length ()
    let take t = t.take ()
    let put_back t e = t.put_back e
      
    let create_fifo () = 
      let t = Fifo.create () in
      {
        head = (fun _ -> Fifo.head t);
        put = (fun x -> Fifo.put t x);
        length = (fun _ -> Fifo.length t);
        take = (fun _ -> Fifo.take t);
        iter = (fun f -> Fifo.iter f t);
        put_back = (fun e -> Fifo.put_back_ele t e);
      }
      
    let create_lifo () = 
      let t = ref [] in
      {
        head = (fun _ -> match !t with 
              [] -> raise Not_found | x :: _ -> x);
        put = (fun x -> t := x :: !t);
        length = (fun _ -> List.length !t);
        take = (fun _ -> match !t with 
              [] -> raise Not_found | x :: tail -> 
                t:=tail; x);        
        iter = (fun f -> List.iter f !t);
        put_back = (fun e -> t := e :: !t);
        }      
      
    module SourcesSet = Set.Make (
        struct
          type t = source
          let compare s1 s2 = 
            if s1.source_addr = s2.source_addr then begin
                0 end else
            match s1.source_client, s2.source_client with
            | SourceClient _, SourceClient _ -> 
                compare  s1.source_addr s2.source_addr
            | SourceClient _, _ -> -1
            | _, SourceClient _ -> 1
            | SourceLastConnection (_, t1, _),SourceLastConnection (_, t2,_)
              ->
                let x = compare t1 t2 in
                if x = 0 then compare s1.source_addr s2.source_addr else x
        end
      )

    let create_oldest_first () = 
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.min_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.min_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

    let create_oldest_last () = 
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.max_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.max_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

    let create_max_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.max_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.max_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }

    let create_min_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.min_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.min_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }

  end
  

(* Connect to a client. Test before if the connection is really useful. *)
let useful_client source_of_client reconnect_client c = 
  if !verbose_sources then begin
      Printf.printf "Testing source"; print_newline ();
    end;
  let (files, downloading) = purge_requests c.client_files in
  c.client_files <- files;
  try
    if downloading || 
      (client_type c <> NormalClient &&
        c.client_next_view_files < last_time ()) then
      (
        if !verbose_sources then begin
            Printf.printf "Connect to source"; print_newline ();
            (match c.client_kind with Indirect_location _ -> 
                  Printf.printf "Indirect localtion ?"; print_newline ();
              | _ -> ());
          end;
        reconnect_client c; 
        if client_state c = NotConnected then begin
            if !verbose_sources then begin
                Printf.printf "Connection to source failed"; print_newline ();
              end;
            source_of_client c; false
          end else begin
            outside_queue := Intmap.add  (client_num c) c !outside_queue;
            true
          end)
    else raise Not_found
  with _ ->
      source_of_client c;
      false
  
  
      
(**************************************************************


         SPECIFIC TO THIS SOURCE MANAGEMENT SYSTEM


***************************************************************)

(* Here, we have different queues, and we are trying them in order, 
taking as many sources as possible from the first before using
the next one. Maybe a bit unfair... *)

      (*
module SourceManagement1 = struct  
  
  
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
let sources_queues = Array.init nqueues (fun _ -> SourcesQueue.create_fifo ())
let clients_queues = Array.init nqueues (fun _ -> SourcesQueue.create_fifo ())
let sources_periods = Array.create nqueues 600
let sources_slots = Array.create nqueues 1
let sources_name = Array.create nqueues "sources"
  
let one_hour = 3600

let _ =
  
  sources_queues.(new_sources_queue) <- SourcesQueue.create_lifo ();
  sources_queues.(good_old_sources_queue) <- SourcesQueue.create_oldest_first ();  
  sources_queues.(old_sources_queue) <- SourcesQueue.create_oldest_last ();  
  
  
  sources_periods.(immediate_connect_queue) <- 0;
  sources_periods.(good_clients_queue) <- 600;
  sources_periods.(new_sources_queue) <- 0;
  sources_periods.(concurrent_sources_queue) <- one_hour;
  sources_periods.(good_old_sources_queue) <- 600;
  sources_periods.(old_sources_queue) <- 600;
  sources_periods.(bad_sources_queue1) <- 600;
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
        Printf.printf "reschedule_source and invalidate old"; print_newline ();
        s.source_files <- []; (* Invalidate the old one *)
        H.remove sources s;
        H.add sources ss;
        SourcesQueue.put sources_queues.(new_queue) s;
        ss
      else
(* The source is in a queue good for it *)
        s

let queue_new_source new_queue last_conn addr file =
  let ip, port = addr in
  if !verbose_sources then begin
      Printf.printf "NEW SOURCE %s:%d" (Ip.to_string ip) port; print_newline ();
    end;
  try
    let finder =  { dummy_source with source_addr = addr } in
    let s = H.find sources finder in
    if not (List.mem_assq file s.source_files) then 
      let s = reschedule_source s file in
      s.source_files <- (file, 0) :: s.source_files;
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
          source_files = [file, 0];
        }  in
      H.add sources s;
      if !verbose_sources then begin
          Printf.printf "Source added"; print_newline ();
        end;
      SourcesQueue.put sources_queues.(new_queue) s;
      s

let new_source addr file = 
  queue_new_source new_sources_queue 0 addr file

let old_source last_conn addr file =   
  
  let new_queue = 
    if last_time () - last_conn < 600 then 
      good_old_sources_queue
    else old_sources_queue
  in
  
  queue_new_source new_queue last_conn addr file
      
let iter f =
  Array.iter (fun fifo ->
      SourcesQueue.iter (fun (c,_) ->
          match c.client_source with
            None -> () | Some s -> f s) fifo
  ) clients_queues;
  Array.iter (fun fifo ->
      SourcesQueue.iter f fifo
  ) sources_queues;
  Intmap.iter (fun _ c ->
      match c.client_source with
        None -> () | Some s -> f s)
  !outside_queue
  
  
  
let source_of_client c =
  
  outside_queue := Intmap.remove (client_num c) !outside_queue;
  
  if !verbose_sources then begin
      Printf.printf "source_of_client"; print_newline ();
    end;
(*
  if client_type c <> NormalClient then begin
      if !verbose_sources then begin
          Printf.printf "--> friends"; print_newline ();
        end;
      SourcesQueue.put clients_queues.(good_clients_queue) 
      (c, last_time ())
    end else *)
  match c.client_source with
    None -> 
      if !verbose_sources then begin
          Printf.printf "--> indirect"; print_newline ();
        end;
      raise Exit

(* This client is an indirect connection. Can't do anything with it. *)
  | Some s ->
      
      let ip, port = s.source_addr in
      if !verbose_sources then begin
          Printf.printf "Old source %s:%d" (Ip.to_string ip) port; print_newline ();
        end;
      let (files, downloading) = purge_client_files c.client_files in
      c.client_files <- files;
      try
        let new_queue = 
          if client_type c <> NormalClient then good_clients_queue
          else
          if files = [] then begin
              H.remove sources s;
              raise Exit
            end else
          match c.client_score with
          | Client_not_connected -> c.client_next_queue
          | Client_has_priority_file
          | Client_has_file -> concurrent_sources_queue
          | Client_has_priority_chunk
          | Client_has_priority_upload
          | Client_has_chunk
          | Client_has_upload ->  good_clients_queue 
(*          | Client_has_ranked num ->
              if num > 1000 then begin
                  Printf.printf "Client ranked too far"; print_newline ();
                  concurrent_sources_queue
              end else good_clients_queue *)
        in
        
        if new_queue <= last_clients_queue then begin
            if !verbose_sources then begin
                Printf.printf "--> client queue %d" new_queue; print_newline ();
              end;
          SourcesQueue.put clients_queues.(new_queue) (c, last_time ())          
          end else
          begin
            if !verbose_sources then begin
                Printf.printf "--> source queue %d" new_queue; print_newline ();
              end;
            List.iter (fun file -> remove_file_location file c) c.client_files;
            
            s.source_client <- SourceLastConnection (
              new_queue, last_time (), client_num c);
            s.source_files <- List.map (fun file -> 
                file, try !(List.assq file c.client_requests) with _ -> 0) 
            c.client_files;
            SourcesQueue.put sources_queues.(new_queue) s
          end
      with _ ->
          if !verbose_sources then begin
              Printf.printf "--> removed"; print_newline ();
            end;
          List.iter (fun file -> remove_file_location file c) c.client_files
        
let reschedule_sources file =
  Array.iter (fun queue ->
      SourcesQueue.iter (fun s ->
          if List.mem_assq file s.source_files then
            ignore (reschedule_source s file)
      ) queue
  ) sources_queues
  
(* This will be configurable later... how many clients can we ask
in 10 minutes ? *)
let need_new_sources () =
  SourcesQueue.length sources_queues.(new_sources_queue) +
    SourcesQueue.length clients_queues.(good_clients_queue) < 
    600 * !!max_clients_per_second
  
  
  
let slots_counter = ref 0
    
  
(* Change a source structure into a client structure before attempting
  a connection. *)
let client_of_source reconnect_client s index client_num = 
  if !verbose_sources then begin
      Printf.printf "client_of_source"; print_newline ();
    end;
  let (files, downloading) = purge_source_files s.source_files in
  if !verbose_sources then begin
      Printf.printf "Source for %d files" (List.length files); print_newline ();
    end;
  (files <> []) &&
  (if downloading then
      let (ip, port) = s.source_addr in
      let c = DonkeyGlobals.new_client_with_num (Known_location (ip,port))
        client_num in
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
                Printf.printf "Client already has a source!"; print_newline ();
              end;
        |  _ -> ());
      c.client_source <- Some s;
      s.source_client <- SourceClient c;

(* This will be used after the connection to know where to put this client *)
      c.client_score <- Client_not_connected;
      
      List.iter (fun (file, time) ->
          c.client_requests <- (file, ref time) :: c.client_requests;
          add_file_location file c) files;
      
      useful_client source_of_client reconnect_client c
    else
      (List.iter (fun (file, _)  ->
            if !verbose_sources then begin
                Printf.printf "Adding paused source"; print_newline ();
              end;
            Fifo.put file.file_paused_sources (s, index)
        ) files; false)
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
        Printf.printf "queue[%s]: %d sources" sources_name.(i)
        (let n = if i <= last_clients_queue then
              SourcesQueue.length clients_queues.(i)
            else 
              SourcesQueue.length sources_queues.(i) in
          nsources := !nsources + n; n);
        print_newline ();
      end;
    nslots := !nslots + sources_slots.(i);
  done;
  let nslots = !nslots in
  if !verbose_sources then begin
      Printf.printf "nslots: %d nsources:%d" nslots !nsources; print_newline ();
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
      Printf.printf "index=%d slot=%d" index slot; print_newline ();
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
        Printf.printf "iter nclients:%d index:%d slot:%d (slots_counter: %d)" nclients index slot !slots_counter;
        print_newline ();
      end;
    if CommonGlobals.can_open_connection () && index < nqueues then begin
        if nclients > 0 then begin
            
            let (nclients, index, slot) =
              try
                
                if index <= last_clients_queue then
                  
                  let (c, time) = SourcesQueue.head clients_queues.(index) in
                  if time + sources_periods.(index) <= last_time () then
                    let _ = SourcesQueue.take clients_queues.(index) in
                    
                    if useful_client source_of_client reconnect_client c then
                      next_slot nclients index slot
                    else (nclients, index, slot)
                  
                  else raise Not_found
                else
                
                let s = SourcesQueue.head sources_queues.(index) in
                let ip, port = s.source_addr in
                if !verbose_sources then begin
                    Printf.printf "One source %s:%d from queue[%s]" (Ip.to_string ip) port sources_name.(index); print_newline ();
                  end;
                if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
                    if !verbose_sources then begin
                        Printf.printf "Source invalidated"; print_newline ();
                      end;
                    let s = SourcesQueue.take sources_queues.(index) in
                    (nclients, index, slot)
                  end else
                match s.source_client with
                | SourceLastConnection (queue, time, client_num) ->
                    
                    if time + sources_periods.(index) <= last_time () then
(* This source is good, connect to it !!! *)
                      let s = SourcesQueue.take sources_queues.(index) in
                      if !verbose_sources then begin
                          Printf.printf "Source could be connected %d" time; print_newline ();
                        end;
                      
                      if client_of_source reconnect_client s queue client_num then
                        
                        next_slot nclients index slot
                      
                      else
(* For some reason, the connection has been aborted, retry *)
                        (nclients, index, slot)
                    
                    else begin
(* Too early to connect to this source, move to the next queue  *)
                        if !verbose_sources then begin
                            Printf.printf "Too early for this source %d" time; print_newline ();
                          end;
                        raise Not_found       
                      end
                
                | _ -> 
(* This source is already connected, remove it immediatly, and retry *)
                    let s = SourcesQueue.take sources_queues.(index) in
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
        Printf.printf "queue[%s]: %d sources" sources_name.(i)
        (let n = if i <= last_clients_queue then
              SourcesQueue.length clients_queues.(i)
            else 
              SourcesQueue.length sources_queues.(i) in
          nsources := !nsources + n; n);
        print_newline ();
      end;
    nslots := !nslots + sources_slots.(i);
  done;
  let nslots = !nslots in
  if !verbose_sources then begin
      Printf.printf "nslots: %d nsources:%d" nslots !nsources; print_newline ();
    end;
  
  
  let rec iter respect_timers nclients index = 
    if  CommonGlobals.can_open_connection () &&  nclients > 0 then
      if index < nqueues then
        let (nclients, index) = 
          if !verbose_sources then begin
              Printf.printf "Testing queue %s (%d possible connections)"
                sources_name.(index) nclients
              ; print_newline ();
            end;
          try
            if index <= last_clients_queue then
              
              let (c, time) = SourcesQueue.head clients_queues.(index) in
              if time + sources_periods.(index) <= last_time () then
                let _ = SourcesQueue.take clients_queues.(index) in
                
                if useful_client source_of_client reconnect_client c then
                  (nclients-1, index)
                else (nclients, index)
              
              else raise Not_found
            else
            
            let s = SourcesQueue.head sources_queues.(index) in
            let ip, port = s.source_addr in
            if !verbose_sources then begin
                Printf.printf "One source %s:%d from queue[%s]" (Ip.to_string ip) port sources_name.(index); print_newline ();
              end;
            if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
                if !verbose_sources then begin
                    Printf.printf "Source invalidated"; print_newline ();
                  end;
                let s = SourcesQueue.take sources_queues.(index) in
                (nclients, index)
              end else
            match s.source_client with
            | SourceLastConnection (queue, time, client_num) ->
                
                if time + 
                    (if respect_timers then
                      sources_periods.(index)
                    else mini 600 sources_periods.(index) ) <= last_time () then
(* This source is good, connect to it !!! *)
                  let s = SourcesQueue.take sources_queues.(index) in
                  if !verbose_sources then begin
                      Printf.printf "Source could be connected %d" time; print_newline ();
                    end;
                  
                  if client_of_source reconnect_client s queue client_num then
                    
                    (nclients-1, index)
                  
                  else
(* For some reason, the connection has been aborted, retry *)
                    (nclients, index)
                
                else begin
(* Too early to connect to this source, move to the next queue  *)
                    if !verbose_sources then begin
                        Printf.printf "Too early for this source %d" time; print_newline ();
                      end;
                    raise Not_found       
                  end
            
            | _ -> 
(* This source is already connected, remove it immediatly, and retry *)
                let s = SourcesQueue.take sources_queues.(index) in
                (nclients, index)
          with _ -> 
              Printf.printf "Exception: go to next queue"; print_newline ();
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
          SourcesQueue.length clients_queues.(i)
        else 
          SourcesQueue.length sources_queues.(i) in
      nsources := !nsources + n;
      n)
  done;
  
  let n = Intmap.length !outside_queue in
  Printf.bprintf buf "  Outside of queues: %d sources\n" n;
  nsources := !nsources + n;
  
  Printf.bprintf buf "\nTotal number of sources:%d\n" !nsources

let check_sources = check_sources2

end
*)


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

module SourceManagement2 = struct

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
                        Printf.printf "same addr for different sources!"; 
                        print_newline ();
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
                Printf.printf "Two different sources are compared equally"; 
                print_newline ();
                exit 2
              end;
            result
        end
      )
    
    let old_source_score = 0
    let new_source_score = 30
    let friend_source_score = 1000
    
    let compare_sources s1 s2 =
      if s1.source_addr = s2.source_addr then 0 else
        s1.source_score - s2.source_score
    
    let clients_queue = Fifo.create ()
    let ready_sources = ref SourcesSet.empty
      
    let queues = [| (-20, 10); (-40, 30); (-1000, 60) |]
    let waiting_sources = Array.init (Array.length queues) (fun _ ->
          Fifo.create  ())

    exception SourceTooOld
      
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
          
    let score s = 
      begin
        if !verbose_sources then begin
            Printf.printf "score %d" s.source_num; print_newline ();
          end;
        match s.source_client with
        | SourceClient _ -> assert false          
        | SourceLastConnection (basic_score, last_attempt, client_num) ->
            s.source_score <- basic_score 
              + (last_time () - last_attempt) / 60
            ;
            if !verbose_sources then begin
                Printf.printf "  initial score: %d" s.source_score; print_newline ();
              end;
            List.iter (fun r ->
                
                s.source_score <- s.source_score + 
                  (file_priority r.request_file + 1) *
                (match r.request_result with
                    File_not_found | File_possible -> 0
                  | File_expected -> 1
                  | File_found -> 10
                  | File_chunk -> 20
                  | File_upload -> 30
                  | File_new_source -> 15
                );
                if !verbose_sources then begin
                    Printf.printf "  request %s result %s" (Md4.Md4.to_string r.request_file.file_md4) (string_of_result r.request_result);
                    print_newline ();
                  end;
            ) s.source_files;
            let (ip,port) = s.source_addr in
            if !verbose_sources then begin
                Printf.printf "Score for %d(%s:%d) = %d/%d" s.source_num (Ip.to_string ip) port 
                  s.source_score basic_score;
                print_newline ();
              end;
      end
    
    let reschedule_source s file =
      if !verbose_sources then begin
          Printf.printf "reschedule_source %d" s.source_num; print_newline ();
        end;
      if SourcesSet.mem s !ready_sources then begin
          ready_sources := SourcesSet.remove s !ready_sources ;
          score s;
          ready_sources := SourcesSet.add s !ready_sources;
        end 
    
    let queue_new_source new_score source_age addr file =
      let ip, port = addr in
      if !verbose_sources then begin
          Printf.printf "queue_new_source %s:%d" (Ip.to_string ip) port; 
          print_newline ();
        end;
      try
        let finder =  { dummy_source with source_addr = addr } in
        let s = H.find sources finder in
        
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
          if !verbose_sources then begin
              Printf.printf "Source %d added" s.source_num; print_newline ();
            end;
          score s;
          ready_sources := SourcesSet.add s !ready_sources;
          s
    
    let old_source source_age addr file = 
      queue_new_source old_source_score source_age addr file
    
    let new_source addr file = 
      queue_new_source new_source_score (last_time()) addr file
    
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
          raise Exit
      
      | Some s ->
          
          let ip, port = s.source_addr in
          if !verbose_sources then begin
              Printf.printf "Old source %s:%d" (Ip.to_string ip) port; 
              print_newline ();
            end;
          let (files, downloading) = purge_requests c.client_files in
          c.client_files <- files;
          try
            let keep_client =
              client_type c <> NormalClient ||
              
              (List.exists (fun r -> r.request_result >= File_chunk)
                c.client_files)
            in
            
            if keep_client then begin
                if !verbose_sources then begin
                    Printf.printf "%d --> kept" (client_num c); print_newline ();
                  end;
                
                Fifo.put clients_queue (c, last_time ())
              
              end else
            if not (List.exists (fun r -> r.request_result > File_not_found)
                files) then begin
                H.remove sources s;
                raise Exit
              end else
            let basic_score = c.client_score in
            if !verbose_sources then begin
                Printf.printf "%d --> new score %d" (client_num c) basic_score; print_newline ();
              end;
            List.iter (fun r -> remove_file_location r.request_file c) c.client_files;
            if !verbose_sources then begin
                Printf.printf "Set SourceLastConnection for source %d" 
                  s.source_num; 
                print_newline ();
                end;
            s.source_client <- SourceLastConnection (
              basic_score, last_time (), client_num c);
            s.source_files <- c.client_files;
            try
              Fifo.put waiting_sources.(source_queue s) s;
              score s
            with SourceTooOld ->
                if !verbose_sources then begin
                    Printf.printf "Removed old source %d" s.source_num;
                    print_newline ();
                  end;
                H.remove sources s
          
          with _ ->
              if !verbose_sources then begin
                  Printf.printf "%d --> removed" (client_num c); print_newline ();
                end;
              List.iter (fun r -> 
                  remove_file_location r.request_file c) c.client_files
    
    let reschedule_sources files = 
      if !verbose_sources then begin      
          Printf.printf "reschedule_sources file not implemented";
          print_newline () 
        end
        
(* Change a source structure into a client structure before attempting
  a connection. *)
    let client_of_source reconnect_client s basic_score client_num = 
      if !verbose_sources then begin
          Printf.printf "client_of_source %d" s.source_num; print_newline ();
        end;
      let (files, downloading) = purge_requests s.source_files in
      if !verbose_sources then begin
          Printf.printf "Source for %d files" (List.length files); 
          print_newline ();
        end;
      (if downloading then
          let (ip, port) = s.source_addr in
          let c = DonkeyGlobals.new_client_with_num (Known_location (ip,port))
            client_num in
          c.client_next_queue <- 0;
          (match c.client_source with
              Some ss when s != ss -> 
                if !verbose_sources then begin
                    Printf.printf "Client already has a source!"; print_newline ();
                  end;
            |  _ -> ());
          c.client_source <- Some s;
          if !verbose_sources then begin
              Printf.printf "set SourceClient for source %d" s.source_num;
              print_newline ();
            end;
          
          s.source_client <- SourceClient c;

(* This will be used after the connection to know where to put this client *)
          c.client_score <- basic_score - 10;
          
          c.client_files <- s.source_files;
          List.iter (fun r ->
              if r.request_result > File_not_found then
              add_file_location r.request_file c
          ) c.client_files;
          
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
          Printf.printf "recompute_ready_sources"; print_newline ();
        end;
      let t1 = Unix.gettimeofday () in
      
(* Very simple *)
      let previous_sources = !ready_sources in
      let list = ref [] in
      let rec iter i =
        let s = try Fifo.head waiting_sources.(i) with _ -> raise Not_found in
        match s.source_client with
          SourceLastConnection (_,time,_) ->
            if time + 600 < last_time () then
              let s = Fifo.take waiting_sources.(i) in
              list := s :: !list;
              iter i
        | SourceClient c -> 
            if !verbose_sources then begin
                Printf.printf "ERROR: CLIENT %d" (client_num c); print_newline ();
                assert false
              end
      in
      for i = 0 to Array.length queues - 1 do
        (try iter i   with Not_found -> ());
      done;
      ready_sources := SourcesSet.empty;
      SourcesSet.iter (fun s ->
          score s; 
          ready_sources := SourcesSet.add s !ready_sources) previous_sources;
      List.iter (fun s ->
          score s; 
          ready_sources := SourcesSet.add s !ready_sources) !list;
      let t2 = Unix.gettimeofday () in
      if !verbose_sources then begin
          Printf.printf "Delay for Sources: %2.2f" (t2 -. t1); print_newline ();
        end

      
    let check_sources_counter = ref 0
      
    let check_sources reconnect_client = 

      incr check_sources_counter;
      if !check_sources_counter mod 60 = 0 then
        recompute_ready_sources ();
      
      let rec iter_clients nclients = 
        if CommonGlobals.can_open_connection () && nclients > 0 then begin
            try
              let (c, time) = Fifo.head clients_queue in
              if time + 600 <= last_time () then
                let _ = Fifo.take clients_queue in
                
                if useful_client source_of_client reconnect_client c then
                  iter_clients (nclients-1)
                else iter_clients nclients
              
              else raise Fifo.Empty
            
            with Fifo.Empty ->
                iter_sources nclients
          end
      
      and iter_sources nclients = 
        if !verbose_sources then begin
            Printf.printf "iter_sources %d" nclients; print_newline ();
          end;
        
        if CommonGlobals.can_open_connection () && nclients > 0 then begin
            let s = SourcesSet.max_elt !ready_sources in
            ready_sources := SourcesSet.remove s !ready_sources;
            
                        
            let ip, port = s.source_addr in
            if !verbose_sources then begin
                Printf.printf "One source %d[%s:%d] from ready_sources" 
                  s.source_num
                  (Ip.to_string ip) port; 
                print_newline ();
              end;

            
            if !verbose_sources then begin
                if SourcesSet.mem s !ready_sources then begin
                    Printf.printf "Source %d is still in ready_sources after remove" s.source_num; print_newline ();
                    end;
                end;
            let ss = SourcesSet.max_elt !ready_sources in
            if !verbose_sources then begin
                Printf.printf "next max = %d" s.source_num; print_newline ();
              end;                
            match s.source_client with
            | SourceLastConnection (basic_score, time, client_num) ->
                s.source_score <- basic_score;
                if client_of_source reconnect_client s basic_score client_num
                then
                  iter_sources (nclients-1)
                else
                  iter_sources nclients                  
                  
            | SourceClient c -> 
                if !verbose_sources then begin                    
                    Printf.printf "ERROR: CLIENT %d" (client_num c); 
                    print_newline ();
                    assert false
                  end
                    
          end
      in
      try
        iter_clients !!max_clients_per_second
      with Not_found -> ()
      | e ->
          if !verbose_sources then begin
              Printf.printf "Exception %s in check_sources" (Printexc2.to_string e);
              print_newline ()
            end
          
    let need_new_sources _ = 
      Fifo.length clients_queue < !!max_clients_per_second * 600
      
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
      Printf.bprintf buf "NotFound/Found/Chunk/Upload: %d/%d/%d/%d\n"
        !nnotfound !nfound !nchunks !nupload;
      
      let noutside_queue = Intmap.length !outside_queue in
      Printf.bprintf buf "  Outside of queues: %d sources\n" noutside_queue;
      
      Printf.bprintf buf "\nTotal number of sources:%d\n" 
        (noutside_queue + ngood_clients + nready_sources + 
        !total_nwaiting_sources)
      
  end


module S = SourceManagement2
  