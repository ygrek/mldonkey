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

(*
   This code is translated mainly from Markus Kern work on giFT-Fasttrack 
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
open FasttrackTypes
open FasttrackGlobals
open FasttrackOptions
open FasttrackProtocol
open FasttrackComplexOptions

open FasttrackProto
  
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
  Queue.iter send active_udp_queue
  
let send_query ss =
  let f s =
    server_send_query s ss in
  List.iter f !connected_servers;
  extend_query f

let recover_file file =
  let f s = server_send_query s file.file_search in
  List.iter f !connected_servers;
  extend_query f
         
let send_pings () =
  (*
  List.iter (fun s ->
      server_send_ping s;
(*      lprintf "Sending ping\n"; *)
      (*
      match s.server_query_key with
      | NoUdpSupport -> 
(*          lprintf "NoUdpSupport\n"; *)
          host_send_qkr s.server_host
      | _ -> 
(*          lprintf "Udp Support present\n"; *)
          () *)
  ) !connected_servers
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
  ) active_udp_queue
*)
*)
  ()
  
  (*
let udp_handler ip port buf =
  FasttrackHandler.udp_packet_handler ip port
  (FasttrackProto.parse_udp_packet  ip  port buf)
*)      
      
let rec find_ultrapeer queue =
  let (next,h) = Queue.head queue in
  try
    if next > last_time () then begin
(*        lprintf "not ready: %d s\n" (next - last_time ());  *)
        raise Not_found;
      end;
    ignore (host_queue_take queue);
    h
  with _ -> find_ultrapeer queue
      
let try_connect_ultrapeer connect =
  let h = 
    try
      find_ultrapeer ultrapeers_waiting_queue
    with _ ->
(*        lprintf "not in ultrapeers_waiting_queue\n";  *)
        try
          find_ultrapeer g0_ultrapeers_waiting_queue
        with _ ->
(*            lprintf "not in g0_ultrapeers_waiting_queue\n";   *)
            let (h : host) = 
              new_host (Ip.addr_of_string "fm2.imesh.com") 1214 IndexServer in
            find_ultrapeer peers_waiting_queue
  in
(*  lprintf "contacting..\n";  *)
  connect h
  
let connect_servers connect =
(*  lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers;  *)
  (if !!max_ultrapeers > List.length !connected_servers then
      try
        let to_connect = 3 * (!!max_ultrapeers - !nservers) in
        for i = 1 to to_connect do
(*          lprintf "try_connect_ultrapeer...\n";  *)
          try_connect_ultrapeer connect
        done
      with _ -> ())
    
