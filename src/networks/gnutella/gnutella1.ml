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

open Gnutella1Proto
  
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
  Queue.iter send g1_active_udp_queue
  
let send_query uid words =
  let f s =
    server_send_query uid words s.server_sock s in
  List.iter f !g1_connected_servers;
  extend_query f

  
let recover_file file =
  let f s = server_recover_file file file.file_id s.server_sock s in
  List.iter f !g1_connected_servers;
  extend_query f


let recover_files () =
  List.iter (fun file ->
      recover_file file 
  ) !current_files;
  ()
         
let send_pings () =
  List.iter (fun s ->
      server_send_ping s;
      lprintf "Sending ping\n";
      match s.server_query_key with
      | NoUdpSupport -> 
          lprintf "NoUdpSupport\n";
          server_send_qkr s           
      | _ -> 
          lprintf "Udp Support present\n";
          ()
  ) !g1_connected_servers;
  Queue.iter (fun h ->
      match h.host_server with
      | None -> () | Some s -> ()
  ) g1_active_udp_queue

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
      find_ultrapeer g1_ultrapeers_waiting_queue
    with _ ->
(*        lprintf "not in waiting\n"; *)
        try
          find_ultrapeer g0_ultrapeers_waiting_queue
        with _ ->
(*                lprintf "not in old\n"; *)
            Gnutella1Redirector.connect ();
            find_ultrapeer g1_peers_waiting_queue
  in
  connect g1_nservers false false h []
  
let connect_servers connect =
  (*
  lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers; 
*)
  if !!g1_max_ultrapeers > List.length !g1_connected_servers then
    let to_connect = 3 * (!!g1_max_ultrapeers - !g1_nservers) in
    for i = 1 to to_connect do
      try_connect_ultrapeer connect
    done    
