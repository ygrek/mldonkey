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

(* This is supposed to become a generic module to manage hosts lists, ie
list of potential servers. We need to add:
  1) Limit the number of known hosts
  2) Allow to save/load hosts from config file, potentially with server info.
*)

open Queues
open Printf2
open Md4
open BasicSocket
open Options
open TcpBufferedSocket
  
type ('server,'host_kind,'request,'ip) host = {
    host_num : int;
    mutable host_server : 'server option;
    host_addr : 'ip;
    host_port : int;
(* the last time we have indirectly heard about this host *)
    mutable host_age : int;
(* the last time this host has talked to us *)
    mutable host_connected : int;
    
(* the set of requests to perform on this host, and the last time they have 
  been done *)
    mutable host_requests : ('request * int) list; 
    mutable host_kind : 'host_kind;
    
    mutable host_queues : ('server,'host_kind,'request,'ip) host Queue.t list;
  }

module Make(M: sig 
      
      type server
      type host_kind
      type request
      type ip
        
      val requests : (request *
          
          (int (* repeat request delay *) * 
(* returns the queue in which the host should be put *)
            ( host_kind -> (server, host_kind, request,ip) host Queues.Queue.t list)
            )
        ) list
    
      val default_requests : host_kind -> (request * int) list
    end) = struct
    
    open M

(* Hosts are first injected in workflow. The workflow ensures that any
host object is inspected every two minutes. *)
    let (workflow : (server, host_kind, request,ip) host Queues.Queue.t) = 
      Queues.workflow (fun time -> time + 120 > last_time ())
    
    let host_queue_add q h time =
      if not (List.memq q h.host_queues) then begin
          Queue.put q (time, h);
          h.host_queues <- q :: h.host_queues
        end
    
    let host_queue_take q =
      let (time,h) = Queue.take q in
      if List.memq q h.host_queues then begin
          h.host_queues <- List2.removeq q h.host_queues 
        end;
      h
    
    let hosts_by_key = Hashtbl.create 103
    
    let hosts_counter = ref 0
    
    let new_host ip port host_kind = 
      let key = (ip,port) in
      try
        let h = Hashtbl.find hosts_by_key key in
        h.host_age <- last_time ();
        h
      with _ ->
          incr hosts_counter;
          let host = {
              host_num = !hosts_counter;
              host_server = None;
              host_addr = ip;
              host_port = port;
              
              host_age = last_time ();
              host_requests = default_requests host_kind;
              host_connected = 0;
              
              host_kind = host_kind;
              host_queues = [];
            } in
          Hashtbl.add hosts_by_key key host;
          host_queue_add workflow host 0;
          host
    
    let rec set_request_rec list r tail =
      match list with
        [] -> (r, last_time ()) :: tail
      | (rr,_) :: rem when rr = r ->
          (r, last_time ()) :: (tail @ rem)
      | rr :: rem ->
          set_request_rec rem r (rr :: tail)
    
    let set_request h r =
      h.host_requests <- set_request_rec h.host_requests r []
    
    
    let manage_host h =
      try
        let current_time = last_time () in
(*    lprintf "host queue before %d\n" (List.length h.host_queues);  *)

(*    lprintf "host queue after %d\n" (List.length h.host_queues); *)
(* Don't do anything with hosts older than one hour and not responding *)
        if max h.host_connected h.host_age > last_time () - 3600 then begin
            host_queue_add workflow h current_time;
(* From here, we must dispatch to the different queues *)
            List.iter (fun (request, last) ->
                try
                  let (delay,f) = List.assoc request requests in
                  if last + delay < current_time then 
                    List.iter (fun queue ->
                        host_queue_add queue  h current_time)
                    (f h.host_kind)
                with _ -> ()
            ) h.host_requests;
            
            (*
            match h.host_kind with
            | Ultrapeer | IndexServer ->        
                if h.host_udp_request + 600 < last_time () then begin
(*              lprintf "waiting_udp_queue\n"; *)
                    H.
                  end;
                if h.host_tcp_request + 600 < last_time () then begin
                    H.host_queue_add  ultrapeers_waiting_queue h current_time;
                  end
            | Peer ->
                if h.host_udp_request + 600 < last_time () then begin
(*              lprintf "g01_waiting_udp_queue\n"; *)
                    H.host_queue_add waiting_udp_queue h current_time;
                  end;
                if h.host_tcp_request + 600 < last_time () then
H.host_queue_add peers_waiting_queue h current_time;
  *)
          end    else 
        if max h.host_connected h.host_age > last_time () - 3 * 3600
            || h.host_queues <> []
          then begin
            host_queue_add workflow h current_time;      
          end else
(* This host is too old, remove it *)
          ()
      
      with e ->
          lprintf "Exception %s in manage_host\n" (Printexc2.to_string e)
    
    let manage_hosts () = 
      let rec iter () =
        let h = host_queue_take workflow in
        manage_host h;
        iter ()
      in
      (try iter () with _ -> ());
      
      
  end