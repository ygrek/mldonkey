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

open Options
open CommonOptions
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals

let verbose_sources = ref false
  
let immediate_connect_queue = 0
let good_clients_queue = 1
  
(* The queues after this one are implemented using 'sources' and not
'clients' *)
let last_clients_queue = good_clients_queue
  
let concurrent_sources_queue = 2
let old_sources_queue = 3
let new_sources_queue = 4
let bad_sources_queue1 = 6
let bad_sources_queue2 = 7
let bad_sources_queue3 = 8
let bad_sources_queue4 = 9
let bad_sources_queue = 10
  
let nqueues = bad_sources_queue + 1
let sources_queues = Array.init nqueues (fun _ -> Fifo.create ())
let clients_queues = Array.init nqueues (fun _ -> Fifo.create ())
let sources_periods = Array.create nqueues 600.
let sources_slots = Array.create nqueues 1
let sources_name = Array.create nqueues "sources"
  
let one_hour = 3600.

let _ =
  sources_periods.(immediate_connect_queue) <- 0.;
  sources_periods.(good_clients_queue) <- 600.;
  sources_periods.(new_sources_queue) <- 600.;
  sources_periods.(concurrent_sources_queue) <- one_hour;
  sources_periods.(old_sources_queue) <- 600.;
  sources_periods.(bad_sources_queue1) <- 600.;
  sources_periods.(bad_sources_queue2) <- 1200.;
  sources_periods.(bad_sources_queue3) <- one_hour;
  sources_periods.(bad_sources_queue4) <- 2. *. one_hour;
  sources_periods.(bad_sources_queue) <- 12. *. one_hour;
  
(* total is 31, with 5 clients_per_second, immediat connect is 6 seconds... *)
  sources_slots.(immediate_connect_queue) <- 30;
  sources_slots.(good_clients_queue) <- 100;
  sources_slots.(new_sources_queue) <- 100;
  sources_slots.(concurrent_sources_queue) <- 50;
  sources_slots.(old_sources_queue) <- 20;
  sources_slots.(bad_sources_queue1) <- 30;
  sources_slots.(bad_sources_queue2) <- 30;
  sources_slots.(bad_sources_queue3) <- 30;
  sources_slots.(bad_sources_queue4) <- 30;
  sources_slots.(bad_sources_queue) <- 10;
  
  sources_name.(immediate_connect_queue) <- "Immediat clients";
  sources_name.(good_clients_queue) <- "Good clients";
  sources_name.(new_sources_queue) <- "New sources";
  sources_name.(concurrent_sources_queue) <- "Concurrent Sources";
  sources_name.(old_sources_queue) <- "Old sources";
  sources_name.(bad_sources_queue1) <- "Bad sources 1";
  sources_name.(bad_sources_queue2) <- "Bad sources 2";
  sources_name.(bad_sources_queue3) <- "Bad sources 3";
  sources_name.(bad_sources_queue4) <- "Bad sources 4";
  sources_name.(bad_sources_queue) <- "Bad sources"
      
module H = Weak2.Make(struct
      type t = source
      let hash s = Hashtbl.hash s.source_addr
      
      let equal x y = x.source_addr = y.source_addr
    end)

let sources = H.create 13557
  
let dummy_source = {
    source_addr = (Ip.null, 0);
    source_client = SourceLastConnection (0, 0.0);
    source_files = [];
    source_overnet = false;
  }

let query_file sock c file =
  if file_state file = FileDownloading then
    let time = 
      try
        List.assq file c.client_requests 
      with _ ->
          let time = ref 0.0 in
          c.client_requests <- (file, time) :: c.client_requests;
          time
    in
    if !time +. !!min_reask_delay < last_time () then begin
        DonkeyProtoCom.direct_client_send sock (
          let module M = DonkeyProtoClient in
          let module C = M.QueryFile in
          M.QueryFileReq file.file_md4);         
        time := last_time ()
      end
      
let add_file_location file c =
  if not (Intmap.mem (client_num c) file.file_locations) then begin
      file.file_locations <- Intmap.add (client_num c) c file.file_locations;
      CommonFile.file_add_source (CommonFile.as_file file.file_file) 
      (CommonClient.as_client c.client_client);
      c.client_files <- file :: c.client_files;
      match c.client_sock, client_state c with
        Some sock, (Connected_busy | Connected_idle | Connected_queued) ->
          query_file sock c file
      | _ -> ()
    end
    
let remove_file_location file c = 
  file.file_locations <- Intmap.remove (client_num c) file.file_locations
  
let reschedule_source s file =
(* This already known source has been announced has a source for a new file *)
  match s.source_client with
  | SourceClient c -> add_file_location file c; s
      
  | SourceLastConnection (queue, time) ->
      if sources_periods.(queue) > 601. || queue > new_sources_queue then
(* This source will not be connected early enough *)
        (* TODO: if it is a popular file, don't schedule it too early *)
        let ss = { s with 
            source_files = s.source_files;
            source_client = SourceLastConnection (new_sources_queue, time);
          } in
        s.source_files <- []; (* Invalidate the old one *)
        H.remove sources s;
        H.add sources ss;
        Fifo.put sources_queues.(new_sources_queue) s;
        ss
      else
(* The source is in a queue good for it *)
        s
        
let new_source addr file =
  let ip, port = addr in
  if !verbose_sources then begin
      Printf.printf "NEW SOURCE %s:%d" (Ip.to_string ip) port; print_newline ();
    end;
  try
    let finder =  { dummy_source with source_addr = addr } in
    let s = H.find sources finder in
    if not (List.mem_assoc file s.source_files) then 
      let s = reschedule_source s file in
      s.source_files <- (file, 0.0) :: s.source_files;
      s
    else
      s
      
  with _ ->
      let s = {
          source_addr = addr;
          source_client = SourceLastConnection (new_sources_queue, 0.0);
          source_files = [file, 0.0];
          source_overnet = false;
        }  in
      H.add sources s;
      if !verbose_sources then begin
          Printf.printf "Source added"; print_newline ();
        end;
      Fifo.put sources_queues.(new_sources_queue) s;
      s

let iter f =
  Array.iter (fun fifo ->
      Fifo.iter f fifo
  ) sources_queues

      
let purge_source_files files =
  let rec iter downloading files all_files =
    match files with
      [] -> all_files, downloading
    | ((file, last_req) as head) :: tail ->
        match file_state file with
        | FileDownloading -> iter true tail (head :: all_files)
        | FilePaused -> iter downloading tail (head :: all_files)
        | FileNew
        | FileShared
        | FileCancelled
        | FileDownloaded -> iter downloading tail all_files
  in
  iter false files []
      
let purge_client_files files =
  let rec iter downloading files all_files =
    match files with
      [] -> all_files, downloading
    | file :: tail ->
        match file_state file with
        | FileDownloading -> iter true tail (file :: all_files)
        | FilePaused -> iter downloading tail (file :: all_files)
        | FileNew
        | FileShared
        | FileCancelled
        | FileDownloaded -> iter downloading tail all_files
  in
  iter false files []
  
  
let source_of_client c =
  if !verbose_sources then begin
      Printf.printf "source_of_client"; print_newline ();
    end;
  if client_type c <> NormalClient then begin
      if !verbose_sources then begin
          Printf.printf "--> friends"; print_newline ();
        end;
      Fifo.put clients_queues.(good_clients_queue) (c, last_time ())
    end else
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
          if files = [] then raise Exit else
          match c.client_score with
          | Client_not_connected -> c.client_next_queue
          | Client_has_file -> concurrent_sources_queue
          | Client_has_new_chunk
          | Client_has_upload ->  good_clients_queue 
        in

        
        if new_queue <= last_clients_queue then begin
            if !verbose_sources then begin
                Printf.printf "--> client queue %d" new_queue; print_newline ();
              end;
          Fifo.put clients_queues.(new_queue) (c, last_time ())          
          end else
          begin
            if !verbose_sources then begin
                Printf.printf "--> source queue %d" new_queue; print_newline ();
              end;
            List.iter (fun file -> remove_file_location file c) c.client_files;
            
            s.source_client <- SourceLastConnection (new_queue, last_time ());
            s.source_files <- List.map (fun file -> 
                file, try !(List.assq file c.client_requests) with _ -> 0.0) 
            c.client_files;
            Fifo.put sources_queues.(new_queue) s
          end
      with _ ->
          if !verbose_sources then begin
              Printf.printf "--> removed"; print_newline ();
            end;
          List.iter (fun file -> remove_file_location file c) c.client_files
        
let reschedule_sources file =
  Array.iter (fun queue ->
      Fifo.iter (fun s ->
          if List.mem_assoc file s.source_files then
            ignore (reschedule_source s file)
      ) queue
  ) sources_queues

let source_has_upload c = 
  if c.client_score <> Client_has_upload then
    c.client_score <- Client_has_upload
    
let source_has_new_chunk c = 
  match c.client_score with
    Client_has_upload | Client_has_new_chunk -> ()
  | _ -> c.client_score <- Client_has_new_chunk
      
let source_has_file c = 
  if c.client_score = Client_not_connected then
    c.client_score <- Client_has_file
    