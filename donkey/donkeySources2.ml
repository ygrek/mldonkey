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

open DonkeyGlobals
open CommonTypes
open Options
open DonkeyOptions
open CommonOptions
open BasicSocket
open DonkeyTypes
open DonkeySources1

let slots_counter = ref 0
    
(* Connect to a client. Test before if the connection is really useful. *)
let useful_client c = 
  if !verbose_sources then begin
      Printf.printf "Testing source"; print_newline ();
    end;
  let (files, downloading) = purge_client_files c.client_files in
  c.client_files <- files;
  try
    if downloading || 
      (client_type c <> NormalClient &&
        c.client_next_view_files < last_time ()) then
      (
        if !verbose_sources then begin
            Printf.printf "Connect to source"; print_newline ();
          end;
        DonkeyClient.reconnect_client c; true)
    else raise Not_found
  with _ ->
      if client_type c <> NormalClient then
        Fifo.put clients_queues.(good_clients_queue) (c, last_time() +. 600.)
      else
      if files <> [] then  source_of_client c;
      false
  
(* Change a source structure into a client structure before attempting
  a connection. *)
let client_of_source s index = 
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
      let c = DonkeyGlobals.new_client (Known_location (ip,port)) in
      c.client_next_queue <- (
(* If the connection fails:
- if the client is a good client, try a new attempt shortly (old_sources_queue)
- if the client has never failed before, progressively delay connections
  (bad_sources_queue1)
- if the client is already too far, just let it where it is (bad_sources_queue)
*)
        if index < new_sources_queue then old_sources_queue else
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
      
      useful_client c
    else
      (List.iter (fun (file, _)  ->
            if !verbose_sources then begin
                Printf.printf "Adding paused source"; print_newline ();
              end;
            Fifo.put file.file_paused_sources (s, index)
        ) files; false)
  )
  
  
(* This function is called every second *)
let check_sources _ =

(* We should first check if we have some download bandwidth available 
and if we have not exceeded the max number of clients   *)

(* Find the total number of slots. Should be only computed once... *)
  let nslots = ref 0 in
  for i = 0 to nqueues - 1 do 
    if !verbose_sources then begin
        Printf.printf "queue[%s]: %d sources" sources_name.(i)
        (if i <= last_clients_queue then
            Fifo.length clients_queues.(i)
          else 
            Fifo.length sources_queues.(i));
        print_newline ();
      end;
    nslots := !nslots + sources_slots.(i);
  done;
  let nslots = !nslots in
  if !verbose_sources then begin
      Printf.printf "nslots: %d" nslots; print_newline ();
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
    if index < nqueues then begin
        if nclients > 0 then begin
            
            let (nclients, index, slot) =
              try
                
                if index <= last_clients_queue then
                  
                  let (c, time) = Fifo.head clients_queues.(index) in
                  if time +. sources_periods.(index) <= last_time () then
                    let _ = Fifo.take clients_queues.(index) in
                    
                    if useful_client c then
                      next_slot nclients index slot
                    else (nclients, index, slot)
                  
                  else raise Not_found
                else
                
                let s = Fifo.head sources_queues.(index) in
                let ip, port = s.source_addr in
                if !verbose_sources then begin
                    Printf.printf "One source %s:%d from queue[%s]" (Ip.to_string ip) port sources_name.(index); print_newline ();
                  end;
                if s.source_files = [] then begin
(* For some reason, this source has been invalidated *)
                    if !verbose_sources then begin
                        Printf.printf "Source invalidated"; print_newline ();
                      end;
                    let s = Fifo.take sources_queues.(index) in
                    (nclients, index, slot)
                  end else
                match s.source_client with
                | SourceLastConnection (queue, time) ->
                    
                    if time +. sources_periods.(index) <= last_time () then
(* This source is good, connect to it !!! *)
                      let s = Fifo.take sources_queues.(index) in
                      if !verbose_sources then begin
                          Printf.printf "Source could be connected"; print_newline ();
                        end;
                      
                      if client_of_source s queue then
                        
                        next_slot nclients index slot
                      
                      else
(* For some reason, the connection has been aborted, retry *)
                        (nclients, index, slot)
                    
                    else begin
(* Too early to connect to this source, move to the next queue  *)
                        if !verbose_sources then begin
                            Printf.printf "Too early for this source"; print_newline ();
                          end;
                        raise Not_found       
                      end
                
                | _ -> 
(* This source is already connected, remove it immediatly, and retry *)
                    let s = Fifo.take sources_queues.(index) in
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
