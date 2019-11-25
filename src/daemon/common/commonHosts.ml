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
open BasicSocket
open Options
open CommonOptions

type host_kind = Peer | Ultrapeer | IndexServer

type ('server,'request,'ip) host = {
    host_num : int;
    mutable host_server : 'server option;
    mutable host_on_remove : unit -> unit;
    host_addr : 'ip;
    host_port : int;
    (* the last time we have indirectly heard about this host *)
    mutable host_obsolete : int;

    (* the set of requests to perform on this host, and the last time they have
       been done *)
    mutable host_requests : ('request * int) list;
    mutable host_kind : host_kind;

    mutable host_queues : ('server, 'request, 'ip) host Queue.t list;
  }

module Make(M: sig

      type server
      type request
      type ip

      val requests : (request *

          (int (* repeat request delay *) *
(* returns the queue into which the host should be put *)
            ( host_kind -> (server, request,ip) host Queues.Queue.t list)
          )
        ) list

      val default_requests : host_kind -> (request * int) list

      val max_ultrapeers : int Options.option_record
      val max_peers : int Options.option_record

    end) = struct

    open M

    (* Hosts are first injected in workflow. The workflow ensures that any
       host object is inspected every two minutes. *)
    let (workflow : (server, request,ip) host Queues.Queue.t) =
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

    let _ =
     Heap.add_memstat "CommonHosts" (fun level buf ->
      Printf.bprintf buf "  hosts_by_key: %d\n" (Hashtbl.length hosts_by_key);
     )


    let indexservers_counter = ref 0
    let ultrapeers_counter = ref 0
    let peers_counter = ref 0

    (* The number of new hosts that have been rejected *)
    let indexservers_pressure = ref 0
    let ultrapeers_pressure = ref 0
    let peers_pressure = ref 0

    let host_num = ref 0

    let counter n =
      match n with
      | Ultrapeer -> ultrapeers_counter
      | IndexServer -> indexservers_counter
      | _ -> peers_counter

    let pressure n =
      match n with
      | Ultrapeer -> ultrapeers_pressure
      | IndexServer -> indexservers_pressure
      | _ -> peers_pressure

    let max_hosts n =
      match n with
      | Ultrapeer -> !!max_ultrapeers
      | IndexServer -> max_int
      | _ -> !!max_peers

    let new_host ip port host_kind =
      let key = (ip,port) in
      try
        let h = Hashtbl.find hosts_by_key key in
        h.host_obsolete <- 0;
        h
      with _ ->
          incr host_num;
          let host = {
              host_num = !(counter host_kind);
              host_server = None;
              host_on_remove = (fun _ -> ());
              host_addr = ip;
              host_port = port;
              host_obsolete = 0;
              host_requests = default_requests host_kind;

              host_kind = host_kind;
              host_queues = [];
            } in
          if !(counter host_kind) < max_hosts host_kind then begin
              incr (counter host_kind);
              Hashtbl.add hosts_by_key key host;
              host_queue_add workflow host 0;
              host
            end else begin
              (* Be careful, we don't remember this host, so don't expect it to appear in the
                 workflow...
                 Why not remember it? Either the system should know that it
                 should drop the host or we should take care of that later.
                 We now add it and clean the pressure in manage_hosts. *)
              incr (pressure host_kind);
              incr (counter host_kind);
              Hashtbl.add hosts_by_key key host;
              host_queue_add workflow host 0;
              host
            end

    let rec set_request_rec list r tail =
      match list with
        [] -> (r, last_time ()) :: tail
      | (rr,_) :: rem when rr = r ->
          (r, last_time ()) :: (tail @ rem)
      | rr :: rem ->
          set_request_rec rem r (rr :: tail)

    let set_request h r =
      h.host_requests <- set_request_rec h.host_requests r []

    let under_pressure kind =
      ! (pressure kind) <> 0 ||
      !(counter kind) * 110 / 100 > (max_hosts kind)

    let under_much_pressure kind =
      !(counter kind) > 2*(max_hosts kind)

    (* TODO: we should try to be more clever. We should take care of the
       "pressure", i.e. the new hosts that we discover. If we don't discover
       new hosts, we should keep the old ones. If we discover new hosts, we should
       remove the old ones. *)
    let manage_host h =
      try
        let current_time = last_time () in
        (* Don't do anything with not responding hosts...
           but then, why do we keep then if we cannot remove them ? *)
        if ( not (under_pressure h.host_kind) || h.host_obsolete = 0 )
           && not (under_much_pressure h.host_kind)
          then begin
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

          end
        else
        if h.host_queues <> [] then begin
            host_queue_add workflow h current_time;
            if !verbose then
              lprintf_nl "[cHo] not removed host %d  %s server %d"
                (h.host_obsolete - last_time ())
                (match h.host_server with | None -> "none" | Some _ -> "some" )
                (Queue.length workflow);
          end else begin
            (* This host is too old, remove it *)
            h.host_on_remove ();
            if !verbose then
              lprintf_nl "[cHo] removed host %d  %s server %d"
                (h.host_obsolete - last_time ())
                (match h.host_server with | None -> "none" | Some _ -> "some" )
                (Queue.length workflow);
            decr (counter h.host_kind);
            decr (pressure h.host_kind);
            Hashtbl.remove hosts_by_key  (h.host_addr, h.host_port)
          end

      with e ->
          lprintf_nl "[cHo] Exception %s in manage_host" (Printexc2.to_string e)

    let manage_hosts () =
      let rec iter () =
        let h = host_queue_take workflow in
        manage_host h;
        iter ()
      in
      (try iter () with _ -> ())

    let try_connect h =
      if h.host_obsolete = 0 then
        (* This host will become obsolete if it doesn't reply to us in the
           next minute *)
        h.host_obsolete <- last_time () + 60

    let connected h =
      h.host_obsolete <- 0

  end
