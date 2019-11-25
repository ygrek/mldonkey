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

open BTOptions
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

torrents/: .torrent files
  downloads/: current downloads
  seeded/: currently seeding
  incoming/: monitored for new torrents to start downloading
  old/: no content available

  *)

(* prints a new logline with date, module and starts newline *)
let log_prefix = "[bTTrack]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

open Http_server

type tracker_peer = {
    peer_id : Sha1.t;
    mutable peer_ip : Ip.t;
    mutable peer_port : int;
    mutable peer_active : int;
    mutable peer_key : string;
  }

type tracker = {
    tracker_id : Sha1.t;
    mutable tracker_table : (Sha1.t, tracker_peer) Hashtbl.t;
    mutable tracker_peers : tracker_peer Fifo.t;
    mutable tracker_message_content : string;
    mutable tracker_message_time : int;
    mutable tracker_downloaded : int;
    mutable tracker_complete : int;
    mutable tracker_incomplete : int;
    mutable tracker_last : int;
  }

type local_torrents_files = {
    file_id : Sha1.t;
  }

let tracker_sock = ref None
(* let tracker_udp_sock = ref None *)
let tracked_files = Hashtbl.create 13
let ntracked_files = ref 0

let tracker_port = define_option bittorrent_section ["tracker_port"]
  ~restart: true
  "The port to bind the tracker to (0 to disable)"
    port_option 6881

let max_tracked_files = define_option bittorrent_section ["max_tracked_files"]
  "The maximum number of tracked files (to prevent saturation attack)"
    int_option 100

let max_tracker_reply = define_option bittorrent_section ["max_tracker_reply"]
  "The maximum number of peers returned by the tracker"
    int_option 20

let tracker_force_local_torrents = define_option bittorrent_section ["tracker_force_local_torrents"]
  "The tracker will track only torrents available locally"
    bool_option true

let tracker_use_key = define_option bittorrent_section ["tracker_use_key"]
  "The tracker will check the client key to update ip if changed"
    bool_option true

let default_tracker = define_option bittorrent_section ["default_tracker"]
    "Default tracker for creating torrents (leave empty for builtin tracker)"
    string_option ""

let default_comment = define_option bittorrent_section ["default_comment"]
    "Default comment for creating torrents"
    string_option ""


let int64_of_string v =
  try
    Int64.of_string v
  with e ->
      lprintf_nl "Exception %s in int64_of_string [%s]"
        (Printexc2.to_string e) v;
      raise e

let int_of_string v =
  try
    int_of_string v
  with e ->
      lprintf_nl "Exception %s in int_of_string [%s]"
        (Printexc2.to_string e) v;
      raise e


let void_message = Bencode.encode (
    Dictionary [
      "interval", Int 600L;
      "peers", List []
    ])

let new_tracker info_hash =
  if !ntracked_files < !!max_tracked_files then
    let tracker = {
        tracker_id = info_hash;
        tracker_table = Hashtbl.create 13;
        tracker_peers = Fifo.create ();
        tracker_message_time = 0;
        tracker_message_content = "";
        tracker_downloaded = 0;
        tracker_complete = 0;
        tracker_incomplete = 0;
        tracker_last = (int_of_float (Unix.gettimeofday ()));
      } in
    incr ntracked_files;
    if !verbose_msg_servers then
      lprintf_nl "Start tracking torrent [%s]" (Sha1.to_hexa info_hash);
    Hashtbl.add tracked_files info_hash tracker;
    if not (List.mem info_hash !!tracked_files_list) then
      tracked_files_list =:= info_hash :: !!tracked_files_list;
    tracker
  else
    failwith (Printf.sprintf "[BT] Too many tracked files (%d)" !ntracked_files)

let is_tracker_running () = !tracker_sock <> None

let check_tracker () =
  if not (is_tracker_running ()) then
    failwith "Tracker is not running (either BT-tracker_port is 0 or stopped)"

let tracker_url suffix =
  check_tracker ();
  Printf.sprintf "http://%s:%d/%s"
    (Ip.to_string (CommonOptions.client_ip None))
    !!tracker_port
    (Url.encode suffix)

let track_torrent filename info_hash =
  check_tracker ();
  if (Hashtbl.mem tracked_files info_hash) then
    (if !verbose_msg_servers then lprintf_nl "Torrent [%s] is already tracked" (Sha1.to_hexa info_hash))
  else
    ignore (new_tracker info_hash);
  tracker_url filename

let get_default_tracker () =
  match !!default_tracker with
  | "" -> tracker_url "announce"
  | s -> s

let reply_has_tracker r info_hash peer_id peer_ip peer_port peer_key peer_left peer_event numwant no_peer_id  =

  if !verbose_msg_servers then
    lprintf_nl "Tracker contacted for [%s]" (Sha1.to_hexa info_hash);
  let tracker = try
      Hashtbl.find tracked_files info_hash
    with Not_found ->
        if !!tracker_force_local_torrents then begin
            lprintf_nl "Tracker rejected announce request for torrent [%s]" (Sha1.to_hexa info_hash);
            failwith "Unknown torrent"
          end;
        lprintf_nl "[BT] Need new tracker";
        new_tracker info_hash
  in

  let peer =
    try
      let peer =
        Hashtbl.find tracker.tracker_table peer_id
      in
      if !!tracker_use_key && peer.peer_ip != peer_ip && peer.peer_key != peer_key then
        failwith "Invalid key";
      if (not !!tracker_use_key || (peer.peer_key = peer_key && peer.peer_ip != peer_ip)) then
        peer.peer_ip <- peer_ip;
      if peer.peer_port != peer_port then
        peer.peer_port <- peer_port;
      peer.peer_active <- last_time ();
      peer
    with _ ->
        let peer =
          {
            peer_id = peer_id;
            peer_ip = peer_ip;
            peer_port = peer_port;
            peer_key = peer_key;
            peer_active = last_time ();
          } in
        if peer_left > 0 then
          tracker.tracker_incomplete <- tracker.tracker_incomplete + 1
        else
          tracker.tracker_complete <- tracker.tracker_complete + 1;
        if !verbose_msg_servers then
          lprintf_nl "Tracker adding new peer [%s]" (Sha1.to_string peer_id);
        Hashtbl.add tracker.tracker_table peer_id peer;
        Fifo.put tracker.tracker_peers peer;
        peer
  in
  let message =
    match peer_event with
      "completed" ->
        tracker.tracker_incomplete <- tracker.tracker_incomplete - 1;
        tracker.tracker_complete <- tracker.tracker_complete + 1;
        void_message
    (* Reply with clients that could not connect to this tracker otherwise *)
    | "stopped" ->
        Hashtbl.remove tracker.tracker_table peer_id;
        Fifo.remove tracker.tracker_peers peer;
        if peer_left > 0 then
          tracker.tracker_incomplete <- tracker.tracker_incomplete - 1
        else
          tracker.tracker_complete <- tracker.tracker_complete - 1;
        void_message
    (* Don't return anything *)
    | _ ->
    (* Return numwant peers if specified in the limit of max_tracker_reply.
       In fact, we should only return peers if this peer is behind a firewall.
     *)

        (* use cache *)
        if tracker.tracker_message_time >= last_time () then
          tracker.tracker_message_content
        else
          let list = ref [] in
          lprintf_nl "Tracker collecting peers:";
          (try
              let max_peer_replies =
                if numwant > 0 then
                  min numwant !!max_tracker_reply
                else
                  !!max_tracker_reply
              in
              for i = 1 to max_peer_replies do
                let peer = Fifo.take tracker.tracker_peers in
                if !verbose_msg_servers then
                  lprintf_nl "   %s:%d" (Ip.to_string peer.peer_ip)peer.peer_port;
                list := peer :: !list
              done
            with _ -> ());

          lprintf_nl "Tracker sending %d peers" (List.length !list);
          List.iter (fun p ->
              lprintf_nl "Tracker send: %s:%d"
                (Ip.to_string p.peer_ip) p.peer_port;
              Fifo.put tracker.tracker_peers p
          ) !list;

          let message =
            Dictionary [
              "interval", Int 600L;
              "min interval", Int 600L;
              "peers", List
                (List.map (fun p ->
                    if no_peer_id = 1 then
                      Dictionary [
                        "ip", String (Ip.to_string p.peer_ip);
                        "port", Int (Int64.of_int p.peer_port);
                      ]
                    else
                      Dictionary [
                        "peer id", String
                          (Sha1.direct_to_string p.peer_id);
                        "ip", String (Ip.to_string p.peer_ip);
                        "port", Int (Int64.of_int p.peer_port);
                      ]
                ) !list);
              "downloaded", Int (Int64.of_int tracker.tracker_downloaded);
              "complete", Int (Int64.of_int tracker.tracker_complete);
              "incomplete", Int (Int64.of_int tracker.tracker_incomplete);
              "last", Int (Int64.of_int ((int_of_float (Unix.gettimeofday ())) - tracker.tracker_last));
            ]
          in
          let m = Bencode.encode message in

(* We cache the reply for one minute if we sent enough replies. *)
          if List.length !list = !!max_tracker_reply then begin
              tracker.tracker_message_time <- last_time () + 60;
              tracker.tracker_message_content <- m;
            end;
          m
  in

  r.reply_content <- message

let http_handler t r =
  try
    add_reply_header r "Server" (Printf.sprintf "MLdonkey/%s" Autoconf.current_version);
    add_reply_header r "Connection" "close";

    match r.get_url.Url.short_file with
      "announce" ->
        let args = r.get_url.Url.args in
        let info_hash = ref Sha1.null in
        let peer_id = ref Sha1.null in
        let ip = ref Ip.null in
        let port = ref 0 in
        let uploaded = ref zero in
        let downloaded = ref zero in
        let left = ref zero in
        let event = ref "" in
        let compact = ref zero in
        let hide = ref 0 in
        let numwant = ref 0 in
        let no_peer_id = ref 0 in
        let key = ref "" in
        let natmapped = ref 0 in
        let localip = ref Ip.null in
        List.iter (fun (name, arg) ->
            match name with
            | "info_hash" -> info_hash := Sha1.direct_of_string arg
            | "peer_id" -> peer_id := Sha1.direct_of_string arg
            | "ip" -> ip := (Ip.of_string arg)
            | "port" -> port := int_of_string arg
            | "uploaded" -> uploaded := int64_of_string arg
            | "downloaded" -> downloaded := int64_of_string arg
            | "left" -> left := int64_of_string arg
            | "event" -> event := arg
            | "compact" -> compact := int64_of_string arg
            | "hide" -> hide := int_of_string arg
            | "numwant" -> numwant := int_of_string arg
            | "no_peer_id" -> no_peer_id := int_of_string arg
            | "key" -> key := arg
            | "natmapped" -> natmapped := int_of_string arg
            | "localip" -> localip := (Ip.of_string arg)
            | _ -> if !verbose_msg_servers then
                     lprintf_nl "[BT] Tracker: Unexpected [%s=%s]" name arg
        ) args;

        if !ip = Ip.null && !localip = Ip.null then
          (* if the ip was not specified by the peer, use detected ip *)
          ip := (fst (TcpBufferedSocket.peer_addr r.sock))
        else if !ip = Ip.null && !natmapped = 1 then
          (* use localip if available *)
          ip := !localip;
        if !verbose_msg_servers then begin
            lprintf_nl "Connection received by tracker from client %s:%d with key [%s]:"
              (Ip.to_string !ip) !port !key;
            lprintf_nl "    info_hash: %s" (Sha1.to_hexa !info_hash);
            lprintf_nl "    peer_id: %s" (Sha1.to_hexa !peer_id);
            lprintf_nl "    event: %s" !event;
            lprintf_nl "    numwant: %d" !numwant;
            lprintf_nl "    compact: %d" (Int64.to_int !compact);
            lprintf_nl "    downloaded: %d" (Int64.to_int !downloaded);
            lprintf_nl "    uploaded: %d" (Int64.to_int !uploaded)
          end;
        (* Check hash then send reply *)
        (try
        let message_errors =
          if ((List.length args) = 0) then
            failwith "[BT] Empty request"
          else if ((List.length args) < 4) then
            failwith "[BT] Incomplete request"
          else if !info_hash = Sha1.null then
            failwith "[BT] Invalid info_hash"
          else if !peer_id = Sha1.null then
            failwith "[BT] Invalid peer_id"
          else if !port = 0 then
            failwith "[BT] Invalid port"
          else if !!tracker_use_key && !key = "" then
            failwith "[BT] Invalid client key"
          else ()
        in
        match message_errors with _ ->
              reply_has_tracker r !info_hash !peer_id !ip !port !key (Int64.to_int !left) !event !numwant !no_peer_id
        with e -> ())

    | "scrape" ->
        let files_tracked = ref [] in
        let log_tracked_files = ref "" in
        (* build the answer *)
        Hashtbl.iter (fun info_hash tracker ->
            files_tracked :=
                (Dictionary [
                   (Sha1.direct_to_string info_hash),
                     Dictionary [
                       "complete", Int (Int64.of_int tracker.tracker_complete);
                       "downloaded", Int (Int64.of_int tracker.tracker_downloaded);
                       "incomplete", Int (Int64.of_int tracker.tracker_incomplete);
                     ];
                 ]) :: !files_tracked;
            if !verbose_msg_servers then begin
                let next_file = (Printf.sprintf "[BT] f: %s d: %d c: %d i: %d\n"
                  (Sha1.to_hexa info_hash) tracker.tracker_downloaded tracker.tracker_complete tracker.tracker_incomplete) in
                log_tracked_files := !log_tracked_files ^ next_file
              end
        ) tracked_files;

        if !verbose_msg_servers then begin
            lprintf_nl "Scrape request received by tracker";
            lprintf_nl "Sending scrape list:";
            lprintf_nl "f: (file hash) d: (downloaded) c: (complete) i: (incomplete)";
            lprint_string !log_tracked_files;
          end;
        let message = Dictionary [ "files", List !files_tracked ] in
        let m = Bencode.encode message in
        r.reply_content <- m

    | "favicon.ico" ->
        if !verbose_msg_servers then
            lprintf_nl "favicon.ico request received by tracker";
            add_reply_header r "Content-Type" "image/x-icon";
            r.reply_content <- File.to_string "favicon.ico"

    | filename ->
        if !verbose_msg_servers then
          lprintf_nl "Tracker received a request for .torrent: [%s]" filename;
        let filename = Url.decode filename in
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
          let file_name = Filename.concat torrents_directory filename in
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
          let file_name = Filename.concat old_directory filename in
(*          lprintf " xx [%s]/[%s]\n" file_name filename; *)
          if Sys.file_exists file_name then file_name else
            failwith
              (Printf.sprintf "Tracker HTTPD: torrent [%s] not found" filename)
        in
        add_reply_header r "Content-Type" "application/x-bittorrent";
        r.reply_content <- File.to_string filename

  with e ->
      if !verbose_msg_servers then
        lprintf_nl "for request [%s] exception %s"
          (Url.to_string r.get_url) (Printexc2.to_string e);
      match e with
        Not_found ->
          r.reply_head <- "404 Not Found"
      | _ ->
          let message =
            Dictionary [
              "failure reason", String (Printexc2.to_string e);
            ]
          in
          let m = Bencode.encode message in
          r.reply_content <- m

(* Working zone *)


(*

let message_to_string t =
  let buf = Buffer.create 100 in
  begin
    match t with
    | _ ->
        Buffer.add_string buf "unknown\n"
  end;
  Buffer.contents buf

let ip_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) ->
      Ip.of_inet_addr inet
  | _ -> assert false

let port_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) -> port
  | _ -> assert false

let udp_handler sock event =
(*  lprintf "G2: udp_handler called\n"; *)
  match event with
    UdpSocket.READ_DONE ->
(*      lprintf "G2: udp read_packets...\n"; *)
      UdpSocket.read_packets sock (fun p ->
          try
(*            lprintf "G2: udp one packet...\n"; *)
            let pbuf = p.UdpSocket.udp_content in
            let len = String.length pbuf in
            let (ip,port) = match p.UdpSocket.udp_addr with
              | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
              | _ -> raise Not_found
            in
(*            lprintf "G2: calling udp_client_handler %s:%d\n"
              (Ip.to_string ip) port; *)
            let buf = p.UdpSocket.udp_content in
            let len = String.length buf in
            ()
(*            BTProtocol.udp_client_handler ip port buf *)
          with e ->
              lprintf "Error %s in udp_handler\n"
                (Printexc2.to_string e);
      ) ;
  | _ -> ()

*)
(* *)

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

let start_tracker tracked =
  if !!tracker_port <> 0 then begin
      let config = {
          bind_addr = if !!force_client_ip then Ip.to_inet_addr !!set_client_ip else Unix.inet_addr_any ;
          port = !!tracker_port;
          requests = [];
          addrs = Ip_set.of_list [ Ip.RangeCIDR (Ip.null, 0) ];
          base_ref = "";
(* limit access to MLDonkey BT tracker based on IP blocklist *)
          use_ip_block_list = true;
          default = http_handler;
        } in
      let sock = TcpServerSocket.create "BT tracker"
          (Ip.to_inet_addr !!client_bind_addr)
        !!tracker_port (Http_server.handler config) in
      tracker_sock := Some sock;
      List.iter begin fun info_hash ->
        try
          ignore (new_tracker info_hash)
        with exn ->
          lprintf_nl "Cannot start tracking %s : %s" (Sha1.to_hexa info_hash) (Printexc2.to_string exn)
      end tracked
    end
(*
  if !!tracker_port <> 0 then begin
      let sock = UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
          !!tracker_port udp_handler in
      tracker_udp_sock := Some sock;
      UdpSocket.set_write_controler sock udp_write_controler;
    end
*)
let stop_tracker () =
  match !tracker_sock with
    None -> ()
  | Some sock ->
(* We should also close all the sockets opened for HTTP connections, no ? *)
      TcpServerSocket.close sock Closed_by_user;
      tracker_sock := None
(*
  match !tracker_udp_sock with
    None -> ()
  | Some sock ->
      UdpSocket.close sock Closed_by_user;
      tracker_udp_sock := None
 *)
(* Every 600 seconds, refresh the peers list *)
let clean_tracker_timer () =
  let time_threshold = last_time () - 3600 in

  if !verbose_msg_servers then 
    lprintf_nl "clean_tracker_timer - purging old peers";
  Hashtbl.iter (fun _ tracker ->
      let old_peers = ref [] in
      Hashtbl.iter (fun _ peer ->
          if peer.peer_active < time_threshold then
            old_peers := peer :: !old_peers
          (*else
          if Ip.usable peer.peer_ip then
            list := peer :: !list*))
      tracker.tracker_table;
      List.iter (fun p ->
          Hashtbl.remove tracker.tracker_table p.peer_id
      ) !old_peers;
      Fifo.clear tracker.tracker_peers;
      (* sync peers list *)
      Hashtbl.iter (fun _ peer -> Fifo.put tracker.tracker_peers peer) tracker.tracker_table;
  ) tracked_files

let () =
  add_infinite_timer 600. clean_tracker_timer


(*
UDP tracker protocol

Structures
Before announcing or scraping, you have to obtain a connection ID.

Choose a (random) transaction ID.
Fill the connect input structure.
Send the packet.
connect input Offset Size Name Value
0 64-bit integer connection_id 0x41727101980
8 32-bit integer action 0
12 32-bit integer transaction_id
16

Receive the packet.
Check whether the packet is at least 16 bytes.
Check whether the transaction ID is equal to the one you chose.
Check whether the action is connect.
Store the connection ID for future use.
connect output Offset Size Name Value
0 32-bit integer action 0
4 32-bit integer transaction_id
8 64-bit integer connection_id
16

Choose a (random) transaction ID.
Fill the announce input structure.
Send the packet.
announce input Offset Size Name Value
0 64-bit integer connection_id
8 32-bit integer action 1
12 32-bit integer transaction_id
16 20-byte string info_hash
36 20-byte string peer_id
56 64-bit integer downloaded
64 64-bit integer left
72 64-bit integer uploaded
80 32-bit integer event
84 32-bit integer IP address 0
88 32-bit integer key
92 32-bit integer num_want -1
96 16-bit integer port
98

Receive the packet.
Check whether the packet is at least 20 bytes.
Check whether the transaction ID is equal to the one you chose.
Check whether the action is announce.
Do not announce again until interval seconds have passed or an event has happened.
announce output Offset Size Name Value
0 32-bit integer action 1
4 32-bit integer transaction_id
8 32-bit integer interval
12 32-bit integer leechers
16 32-bit integer seeders
20 + 6 * n 32-bit integer IP address
24 + 6 * n 16-bit integer TCP port
20 + 6 * N

Up to about 74 torrents can be scraped at once. A full scrape can't be done with this protocol.

Choose a (random) transaction ID.
Fill the scrape input structure.
Send the packet.
scrape input Offset Size Name Value
0 64-bit integer connection_id
8 32-bit integer action 2
12 32-bit integer transaction_id
16 + 20 * n 20-byte string info_hash
16 + 20 * N

Receive the packet.
Check whether the packet is at least 8 bytes.
Check whether the transaction ID is equal to the one you chose.
Check whether the action is scrape.
scrape output Offset Size Name Value
0 32-bit integer action 2
4 32-bit integer transaction_id
8 + 12 * n 32-bit integer seeders
12 + 12 * n 32-bit integer completed
16 + 12 * n 32-bit integer leechers
8 + 12 * N

If the tracker encounters an error, it might send an error packet.

Receive the packet.
Check whether the packet is at least 8 bytes.
Check whether the transaction ID is equal to the one you chose.
error output Offset Size Name Value
0 32-bit integer action 3
4 32-bit integer transaction_id
8 string message

If the tracker requires authentication, an authentication structure has to be appended to every packet you send to the tracker. The hash is the first 8 bytes of sha1(input + username + sha1(password)). authenticate input Offset Size Name
0 8-byte zero-padded string username
8 8-byte string hash
16



--------------------------------------------------------------------------------

Actions
0: connect
1: announce
2: scrape
3: error

--------------------------------------------------------------------------------

Events
0: none
1: completed
2: started
3: stopped
*)
