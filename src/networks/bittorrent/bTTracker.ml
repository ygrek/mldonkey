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

open Int64ops
open Md4
open Options
open CommonOptions
open Printf2

open BasicSocket
  
open CommonGlobals
  
open BTOptions
open BTTypes
open BTGlobals
open Bencode

  
open Gettext  
let _s x = _s "BTTracker" x
let _b x = _b "BTTracker" x  

(* 

We could have a a-la-edonkey tracker: it would connect back to incoming
  client, and check whether they are accessible from the outside world,
and also check which chunks they have before sending them sources, so
that we can filter out immediatly sources that are not interesting for
them.
*)
  
(*

torrents/: for BitTorrent
  downloads/: .torrent files of current downloads
  tracked/: .torrent files of tracked downloads
    * If the file appears in incoming/, it is automatically seeded.
  seeded/:
     * If the file appears in incoming/, it is automatically seeded.

  *)

  
open Http_server

type tracker_peer = {
    peer_id : Sha1.t;
    mutable peer_ip : Ip.t;
    mutable peer_port : int;
    mutable peer_active : int;
  }
  
type tracker = {
    tracker_id : Sha1.t;
    mutable tracker_table : (Sha1.t, tracker_peer) Hashtbl.t;
    mutable tracker_peers : tracker_peer Fifo.t;
    mutable tracker_message_content : string;
    mutable tracker_message_time : int;
  }
    
let tracker_sock = ref None  
let tracked_files = Hashtbl.create 13
let ntracked_files = ref 0
  
let tracker_port = define_option bittorrent_section ["tracker_port"]
  "The port to bind the tracker to"
    int_option 6881
  
let max_tracked_files = define_option bittorrent_section ["max_tracked_files"]
  "The maximal number of tracked files (to prevend saturation attack)"
    int_option 100
  
let max_tracker_reply = define_option bittorrent_section ["max_tracker_reply"]
  "The maximal number of peers returned by the tracker"
    int_option 20

  
  
let int64_of_string v =
  try
    Int64.of_string v
  with e ->
      lprintf "Exception %s in int64_of_string [%s]\n" 
        (Printexc2.to_string e) v;
      raise e
  
let int_of_string v =
  try
    int_of_string v
  with e ->
      lprintf "Exception %s in int_of_string [%s]\n" 
        (Printexc2.to_string e) v;
      raise e


let void_message = Bencode.encode (
    Dictionary [
      String "interval", Int (Int64.of_int 600);
      String "peers", List []
    ])

let reply_has_tracker r info_hash peer_id peer_port peer_event =

  lprintf "tracker contacted for %s\n" (Sha1.to_string info_hash);
  let tracker = try
      Hashtbl.find tracked_files info_hash 
    with Not_found ->
        lprintf "Need new tracker\n";
        if !ntracked_files < !!max_tracked_files then
          let tracker = {
              tracker_id = info_hash;
              tracker_table = Hashtbl.create 13;
              tracker_peers = Fifo.create ();
              tracker_message_time = 0;
              tracker_message_content = "";
            } in
          incr ntracked_files;
          Hashtbl.add tracked_files info_hash tracker;
          tracker
        else 
          failwith "Too many tracked files"
  in
  
  let peer = 
    try 
      let peer = 
        Hashtbl.find tracker.tracker_table peer_id
      in
      peer.peer_ip <- fst (TcpBufferedSocket.peer_addr r.sock);
      peer.peer_port <- peer_port;
      peer.peer_active <- last_time ();
      peer
    with _ -> 
        let peer = 
          { 
            peer_id = peer_id;
            peer_ip = fst (TcpBufferedSocket.peer_addr r.sock);
            peer_port = peer_port;
            peer_active = last_time ();
          } in
        lprintf "adding new peer\n";
        Hashtbl.add tracker.tracker_table peer_id peer;
        Fifo.put tracker.tracker_peers peer;
        peer
  in
  let message =
    match peer_event with
      "completed" -> void_message
(* Reply with clients that could not connect to this tracker otherwise *)
    | "stopped" -> void_message
(* Don't return anything *)
    | _ ->
(* Return the 20 best peers. In fact, we should only return peers if
  this peer is behind a firewall. *)
        
        if tracker.tracker_message_time < last_time () then
          
          let list = ref [] in
          lprintf "Tracker collecting peers:\n";
          (try
              for i = 1 to !!max_tracker_reply do
                let peer = Fifo.take tracker.tracker_peers in
                lprintf "   %s:%d\n" (Ip.to_string peer.peer_ip)peer.peer_port;
                list := peer :: !list
              done
            with _ -> ());
                    
          lprintf "Tracker sending %d peers\n" (List.length !list);
          List.iter (fun p ->
              lprintf "Tracker send: %s:%d\n" 
                (Ip.to_string p.peer_ip) p.peer_port;
              Fifo.put tracker.tracker_peers p
          ) !list;

(* reply by sending [head] *)
          
          let message = 
            Dictionary [
              String "interval", Int (Int64.of_int 600);
              String "peers", List 
                (List.map (fun p ->
                    Dictionary [
                      String "peer id", String 
                        (Sha1.direct_to_string p.peer_id);
                      String "ip", String (Ip.to_string p.peer_ip);
                      String "port", 
                      Int (Int64.of_int p.peer_port);
                    ]
                ) !list)
            ]
          in
          let m = Bencode.encode message in
          
(* We cache the reply for one minute if we sent enough replies. *)
          if List.length !list = !!max_tracker_reply then begin
              tracker.tracker_message_time <- last_time () + 60;
              tracker.tracker_message_content <- m;
            end;
          m
        else 
          tracker.tracker_message_content
  in
  
  r.reply_content <-  message
  
let http_handler t r =
  try
    add_reply_header r "Server" "MLdonkey";
    add_reply_header r "Connection" "close";
    add_reply_header r "Content-Type" "application/x-bittorrent";
    
    match r.get_url.Url.short_file with
      "/tracker" ->
        
        let args = r.get_url.Url.args in
        let info_hash = ref Sha1.null in
        let peer_id = ref Sha1.null in
        let port = ref 0 in
        let uploaded = ref zero in
        let downloaded = ref zero in
        let left = ref zero in
        let event = ref "" in
        List.iter (fun (name, arg) ->
            match name with
            | "info_hash" -> info_hash := Sha1.direct_of_string arg
            | "peer_id" -> peer_id := Sha1.direct_of_string arg
            | "port" -> port := int_of_string arg
            | "uploaded" -> uploaded := int64_of_string arg
            | "downloaded" -> downloaded := int64_of_string arg
            | "left" -> left  := int64_of_string arg
            | "event" -> event := arg
            | _ -> lprintf "BTTracker: Unexpected [%s=%s]\n" name arg
        ) args;
        
        lprintf "Connection received by tracker: \n";
        lprintf "    info_hash: %s\n" (Sha1.to_string !info_hash);
        lprintf "    event: %s\n" !event;
        
        reply_has_tracker r !info_hash !peer_id !port !event          
    
    | filename ->

        lprintf "Request for .torrent [%s]\n" filename;
        if (Filename2.last_extension filename <> ".torrent") then
          failwith "Incorrect filename 1";
        for i = 1 to String.length filename - 1 do
          let c = filename.[i] in
          if c = '/' || c = '\\' then failwith "Incorrect filename 2"
        done;
        if filename.[1] = ':' then failwith "Incorrect filename 3";

(* Try to find the .torrent file, normally in torrents/, but maybe
in sub-directories in former versions. *)
        
        let filename = 
          let file_name = Filename.concat old_torrents_directory filename in
(*          lprintf " xx [%s]/[%s]\n" file_name filename; *)
          if Sys.file_exists file_name then file_name else
          let file_name = Filename.concat downloads_directory filename in
(*          lprintf " xx [%s]/[%s]\n" file_name filename; *)
          if Sys.file_exists file_name then file_name else
          let file_name = Filename.concat tracked_directory filename in
(*          lprintf " xx [%s]/[%s]\n" file_name filename; *)
          if Sys.file_exists file_name then file_name else
          let file_name = Filename.concat seeded_directory filename in 
(*          lprintf " xx [%s]/[%s]\n" file_name filename; *)
          if Sys.file_exists file_name then file_name else 
            failwith 
              (Printf.sprintf "Tracker HTTPD: torrent [%s] not found" filename)
        in
        r.reply_content <- File.to_string filename
  
  with e ->
      lprintf "BTTracker: for request [%s] exception %s\n" 
        (Url.to_string r.get_url) (Printexc2.to_string e);
      
      r.reply_head <- "404 Not Found"
      
(* We came back to the "universal" tracker, i.e. a tracker for all files
with only a limitation on the number. So, this function is not useful anymore.
  
let scan_tracked_directory _ = 
  let filenames = Unix2.list_directory tracked_directory in
  
  let old_tracked_files = !current_tracked_files in
  current_tracked_files := Hashtbl.create 13;
  List.iter (fun filename ->
      let filename = Filename.concat tracked_directory filename in
      try
        let s = File.to_string filename in
        let (info_hash : Sha1.t), torrent = decode_torrent s in
        let tracker = 
          try
            Hashtbl.find old_tracked_files info_hash
          with Not_found ->
              lprintf "New tracked file %s\n" filename;
              {
                tracker_table = Hashtbl.create 13;
                tracker_peers = Fifo.create ();
              }
        in
        Hashtbl.add !current_tracked_files info_hash tracker
      with e ->
          lprintf "Cannot track file %s\n" filename
  ) filenames
    *)

let start_tracker () = 
  if !!tracker_port <> 0 then
    let config = {
        bind_addr = Unix.inet_addr_any ;
        port = !!tracker_port;
        requests = [];
        addrs = [ Ip.of_string "255.255.255.255" ];
        base_ref = "";
        default = http_handler;      
      } in
    let sock = TcpServerSocket.create "BT tracker"
        (Ip.to_inet_addr !!client_bind_addr) 
      !!tracker_port (Http_server.handler config) in
    tracker_sock := Some sock;
    ()
    
let stop_tracker () =
  match !tracker_sock with
    None -> ()
  | Some sock ->
(* We should also close all the sockets opened for HTTP connections, no ? *)
      TcpServerSocket.close sock Closed_by_user;
      tracker_sock := None

(* Every 600 seconds *)
let clean_tracker_timer () =
  let time_threshold = last_time () - 3600 in
  let trackers = ref [] in
  
  lprintf "clean_tracker_timer\n";
  Hashtbl.iter (fun _ tracker ->
      let list = ref [] in
      let old_peers = ref [] in
      Hashtbl.iter (fun _ peer -> 
          if peer.peer_active < time_threshold then
            old_peers := peer :: !old_peers
          else
          if Ip.valid peer.peer_ip && ip_reachable peer.peer_ip then
            list := peer :: !list) 
      tracker.tracker_table;
      List.iter (fun p ->
          Hashtbl.remove tracker.tracker_table p.peer_id
      ) !old_peers;
      Fifo.clear tracker.tracker_peers;
      if !list <> [] then begin
          List.iter (fun p -> Fifo.put tracker.tracker_peers p) !list;
          trackers := tracker :: !trackers;
        end;
  ) tracked_files;

  Hashtbl.clear tracked_files;
  List.iter (fun t ->
      Hashtbl.add tracked_files t.tracker_id t
  ) !trackers
  
let _ =
  add_infinite_timer 600. clean_tracker_timer
  
