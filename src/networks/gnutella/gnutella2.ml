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

open Queues
open CommonSwarming
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

open Gnutella2Proto
  
(* For Guess hosts ... to be done *)      
let extend_query f = 
  let send h =
    try
      match h.host_server with
        None -> ()
      | Some s ->
          match s.server_query_key with 
            | UdpQueryKey _ ->
                f s
            | _ -> ()
    with _ -> ()
  in
  Queue.iter send g2_active_udp_queue
  
let send_query uid words =
  let f s =
    server_send_query uid words s.server_sock s in
  List.iter f !g2_connected_servers;
  extend_query f

let recover_file file =
  let f s = server_recover_file file s.server_sock s in
  List.iter f !g2_connected_servers;
  extend_query f
         
let send_pings () =
  List.iter (fun s ->
      server_send_ping s.server_sock s;
(*      lprintf "Sending ping\n"; *)
      match s.server_query_key with
      | NoUdpSupport -> 
(*          lprintf "NoUdpSupport\n"; *)
          host_send_qkr s.server_host
      | _ -> 
(*          lprintf "Udp Support present\n"; *)
          ()
  ) !g2_connected_servers
  (*
  Queue.iter (fun h ->
      match h.host_server with
      | None -> () | Some s ->
          match s.server_query_key with
          | NoUdpSupport ->
              lprintf "recent: NoUdpSupport\n";
              server_send_qkr s 
          | _ -> 
              lprintf "recent: Already there\n";
  ) g2_active_udp_queue
*)
  
let udp_handler ip port buf =
  try
    Gnutella2Handler.udp_packet_handler ip port
      (parse_udp_packet ip port buf)
  with AckPacket | FragmentedPacket -> ()
      
      
let rec find_ultrapeer queue =
  let (next,h) = Queue.head queue in
  if next > last_time () then begin
(*      lprintf "not ready: %d s\n" (next - last_time ()); *)
      raise Not_found;
    end;
  ignore (host_queue_take queue);    
  try
    h,
    
    match h.host_kind with
      1 -> if not !!g1_enabled then raise Not_found; false
    | 2 -> if not !!g2_enabled then raise Not_found; true
    | _ -> 
        not !!g1_enabled
with _ -> find_ultrapeer queue
      
let try_connect_ultrapeer connect =
  let (h, with_accept) = 
    try
      find_ultrapeer g2_ultrapeers_waiting_queue
    with _ ->
(*        lprintf "not in g2_ultrapeers_waiting_queue\n"; *)
        try
          find_ultrapeer g0_ultrapeers_waiting_queue
        with _ ->
(*            lprintf "not in g0_ultrapeers_waiting_queue\n";  *)
            Gnutella2Redirector.connect ();
            find_ultrapeer g2_peers_waiting_queue
  in
(*  lprintf "contacting..\n"; *)
  connect g2_nservers true false h []
  
let connect_servers connect =
(*
  lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers; 
*)
  (if !!g2_max_ultrapeers > List.length !g2_connected_servers then
      try
        let to_connect = 3 * (!!g2_max_ultrapeers - !g2_nservers) in
        for i = 1 to to_connect do
(*          lprintf "try_connect_ultrapeer...\n"; *)
          try_connect_ultrapeer connect
        done
      with _ -> ());
  (try 
      for i = 0 to 3 do
        let h = host_queue_take g2_waiting_udp_queue in
(*        lprintf "g2_waiting_udp_queue\n"; *)
        if (
            match h.host_server with
              None -> true
            | Some s ->
                match s.server_query_key with
                  UdpQueryKey _ -> false
                | _ -> true
          ) then begin
(*            lprintf "host_send_qkr...\n"; *)
            h.host_udp_request <- last_time ();
            host_send_qkr h
          end
      done
    with _ -> ())
    
