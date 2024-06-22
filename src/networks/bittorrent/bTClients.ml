(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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


(** Functions used in client<->client communication 
    and also client<->tracker
*)

(** A peer (or client) is always a remote peer in this file.
  A Piece is a portion of the file associated with a hash (sha1).
  In mldonkey a piece is referred as a block inside the swarming system.
  A SubPiece is a portion of a piece (without hash) which can be
  sent/downloaded to/from a peer.
  In mldonkey a SubPiece is referred as a range inside the swarming system.
  @see  <http://wiki.theory.org/index.php/BitTorrentSpecification> wiki for some
  unofficial (but more detailed) specs.
*)

open Int64ops
open AnyEndian
open BigEndian
open Printf2
open Md4
open Options
open BasicSocket
open TcpBufferedSocket

open CommonShared
open CommonUploads
open CommonOptions
open CommonInteractive
open CommonClient
open CommonTypes
open CommonFile
open CommonSwarming
open CommonGlobals

open BTRate
open BTTypes
open BTProtocol
open BTOptions
open BTGlobals
open BTChooser
open BTStats
open TcpMessages

module VB = VerificationBitmap

let http_ok = "HTTP 200 OK"
let http11_ok = "HTTP/1.1 200 OK"


let next_uploaders = ref ([] : BTTypes.client list)
let current_uploaders = ref ([] : BTTypes.client list)

(** Check that client is valid and record it *)
let maybe_new_client file id ip port =
  let cc = Geoip.get_country_code_option ip in
  if id <> !!client_uid
     && ip != Ip.null
     && port <> 0
     && (match !Ip.banned (ip, cc) with
         | None -> true
         | Some reason ->
           if !verbose_connect then
             lprintf_file_nl (as_file file) "%s:%d blocked: %s" (Ip.to_string ip) port reason;
           false)
  then
    ignore (new_client file id (ip,port) cc);
  if !verbose_sources > 1 then
    lprintf_file_nl (as_file file) "Received %s:%d" (Ip.to_string ip) port


let resume_clients_hook = ref (fun _ -> assert false)

include struct

(* open modules locally *)
open BTUdpTracker
open UdpSocket

let string_of_event = function
  | READ_DONE -> "READ_DONE"
  | WRITE_DONE -> "WRITE_DONE"
  | CAN_REFILL -> "CAN_REFILL"
  | BASIC_EVENT e -> match e with
    | CLOSED reason -> "CLOSED " ^ (string_of_reason reason)
    | RTIMEOUT -> "RTIMEOUT"
    | WTIMEOUT -> "WTIMEOUT"
    | LTIMEOUT -> "LTIMEOUT"
    | CAN_READ -> "CAN_READ"
    | CAN_WRITE -> "CAN_WRITE"

(** talk to udp tracker and parse response
  except of parsing should perform everything that 
  talk_to_tracker's inner function does FIXME refactor both 

  Better create single global udp socket and use it for all 
  tracker requests and distinguish trackers by txn? FIXME?
  *)
let talk_to_udp_tracker host port args file t need_sources =
  let interact ip =
    let socket = create (Ip.to_inet_addr !!client_bind_addr) 0 (fun sock event ->
(*       lprintf_nl "udpt got event %s for %s" (string_of_event event) host; *)
      match event with
      | WRITE_DONE | CAN_REFILL -> ()
      | READ_DONE -> assert false (* set_reader prevents this *)
      | BASIC_EVENT x -> match x with
        | CLOSED _ -> ()
        | CAN_READ | CAN_WRITE -> assert false (* udpSocket implementation prevents this *)
        | LTIMEOUT | WTIMEOUT | RTIMEOUT -> close sock (Closed_for_error "udpt timeout"))
    in
    let set_reader f =
      set_reader socket begin fun _ -> 
        try f () with exn ->
          lprintf_nl ~exn "udpt interact with %s" host;
          close socket (Closed_for_exception exn)
      end
    in
    BasicSocket.set_wtimeout (sock socket) 60.;
    BasicSocket.set_rtimeout (sock socket) 60.;
    let txn = Random.int32 Int32.max_int in
(*     lprintf_nl "udpt txn %ld for %s" txn host; *)
    write socket false (Bytes.unsafe_of_string @@ connect_request txn) ip port;
    set_reader begin fun () ->
      let p = read socket in
      let conn = connect_response (Bytes.unsafe_to_string p.udp_content) txn in
(*       lprintf_nl "udpt connection_id %Ld for %s" conn host; *)
      let txn = Random.int32 Int32.max_int in
(*       lprintf_nl "udpt txn' %ld for host %s" txn host; *)
      let int s = Int64.of_string (List.assoc s args) in
      let req = announce_request conn txn
        ~info_hash:(List.assoc "info_hash" args) 
        ~peer_id:(List.assoc "peer_id" args)
        (int "downloaded",int "left",int "uploaded")
        (match try List.assoc "event" args with Not_found -> "" with
         | "completed" -> 1l
         | "started" -> 2l
         | "stopped" -> 3l
         | "" -> 0l
         | s -> lprintf_nl "udpt event %s? for %s" s host; 0l)
        ~ip:(if !!force_client_ip then (Int64.to_int32 (Ip.to_int64 !!set_client_ip)) else 0l)
        ~numwant:(if need_sources then try Int32.of_string (List.assoc "numwant" args) with _ -> -1l else 0l)
        (int_of_string (List.assoc "port" args))
      in
      write socket false (Bytes.unsafe_of_string req) ip port;
      set_reader (fun () ->
        let p = read socket in

        t.tracker_last_conn <- last_time ();
        file.file_tracker_connected <- true;
        t.tracker_interval <- 600;
        t.tracker_min_interval <- 600;
        if need_sources then t.tracker_last_clients_num <- 0;

        let (interval,clients) = announce_response (Bytes.unsafe_to_string p.udp_content) txn in
        if !verbose_msg_servers then
          lprintf_nl "udpt got interval %ld clients %d for host %s" interval (List.length clients) host;
        if interval > 0l then
        begin
          t.tracker_interval <- Int32.to_int interval;
          if t.tracker_min_interval > t.tracker_interval then
            t.tracker_min_interval <- t.tracker_interval
        end;
        if need_sources then
        List.iter (fun (ip',port) ->
          let ip = Ip.of_int64 (Int64.logand 0xFFFFFFFFL (Int64.of_int32 ip')) in 
(*           lprintf_nl "udpt got %s:%d" (Ip.to_string ip) port; *)
          t.tracker_last_clients_num <- t.tracker_last_clients_num + 1;
          maybe_new_client file Sha1.null ip port
        ) clients;
        close socket Closed_by_user;
        if !verbose_msg_servers then
          lprintf_nl "udpt interact done for %s" host;
        if need_sources then !resume_clients_hook file
        ) end
  in
  try
    if !verbose_msg_servers then
      lprintf_nl "udpt start with %s:%d" host port;
    Ip.async_ip host (fun ip ->
(*         lprintf_nl "udpt resolved %s to ip %s" host (Ip.to_string ip); *)
        if not (Ip.equal Ip.localhost ip) then
          try interact ip with exn -> lprintf_nl ~exn "udpt interact with %s" host
        else if !verbose_msg_servers then
          lprintf_nl "udpt ignoring tracker %s (resolves to localhost)" host)
      (fun () -> 
        if !verbose_msg_servers then
          lprintf_nl "udpt failed to resolve %s" host)
  with
  exn -> 
    lprintf_nl ~exn "udpt start"

end (* include *)

(**
  In this function we connect to a tracker.
  @param file The file concerned by the request
  @param url Url of the tracker to connect
  @param event Event (as a string) to send to the tracker :
   can be 'completed' if the file is complete, 'started' for the first
   connection to this tracker or 'stopped' for a clean stop of the file.
   Everything else will be ok for a second connection to the tracker.
   Be careful to the spelling of this event
  @param f The function used to parse the result of the connection.
   The function will get a file as an argument (@see talk_to_tracker 
   for an example)

  If we have less than !!ask_tracker_threshold sources
  and if we respect the file_tracker_interval then
  we really ask sources to the tracker
*)
let connect_trackers file event need_sources f =

  (* reset session statistics when sending 'started' event *)
  if event = "started" then
  begin
    file.file_session_uploaded <- Int64.zero;
    file.file_session_downloaded <- Int64.zero;
  end;

  let args,must_check_delay, left =

    match file.file_swarmer with
      None ->
        begin
          match event with
          | "started" -> [("event", "started")],true,zero
          | "stopped" -> [("event", "stopped")],false,zero
          | _ -> [],true,zero
        end

    | Some swarmer ->
        let local_downloaded = CommonSwarming.downloaded swarmer in
        let left = file_size file -- local_downloaded in
        match event with
        | "completed" -> [("event", "completed")],false,zero
        | "started" -> [("event", "started")],true,left
        | "stopped" -> [("event", "stopped")],false,left
        | _ -> [],true,left
  in

  let args = ("no_peer_id", "1") :: ("compact", "1") :: args in
  let args = 
    if not need_sources then
      ("numwant", "0") :: args
    else if !!numwant > -1 then
      ("numwant", string_of_int !!numwant) :: args 
    else 
      args
  in
  let args = if !!send_key then
      ("key", Sha1.to_hexa !!client_uid) :: args else args
  in
   let args = if !!force_client_ip then
      ("ip", Ip.to_string !!set_client_ip) :: args else args
  in
  let args =
    ("info_hash", Sha1.direct_to_string file.file_id) ::
    ("peer_id", Sha1.direct_to_string !!client_uid) ::
    ("port", string_of_int !!client_port) ::
    ("uploaded", Int64.to_string file.file_session_uploaded) ::
    ("downloaded", Int64.to_string file.file_session_downloaded) ::
    ("left", Int64.to_string left) ::
    args
  in

  let enabled_trackers =
    let enabled_trackers = List.filter (fun t -> tracker_is_enabled t) file.file_trackers in
    if enabled_trackers <> [] then enabled_trackers
    else begin
      (* if there is no tracker left, do something ? *)
      if !verbose_msg_servers then
        lprintf_nl "No trackers left for %s, reenabling all of them..." (file_best_name (as_file file));
      List.iter (fun t ->
        match t.tracker_status with
        (* only re-enable after normal error *)
        | Disabled _ -> t.tracker_status <- Enabled
        | _ -> ()) file.file_trackers;
      List.filter (fun t -> tracker_is_enabled t) file.file_trackers
    end in

  List.iter (fun t ->

      (* if we have too few sources we may ask the tracker before the interval *)
      if not must_check_delay
        || not file.file_tracker_connected
        || t.tracker_last_conn + t.tracker_interval < last_time()
        || ( file.file_clients_num < !!ask_tracker_threshold
            && (file_state file) == FileDownloading
            && (if t.tracker_min_interval > !!min_tracker_reask_interval then
                t.tracker_last_conn + t.tracker_min_interval < last_time()
              else
                t.tracker_last_conn + !!min_tracker_reask_interval < last_time() ))
      then
        begin
          (* if we already tried to connect but failed, disable tracker, but allow re-enabling *)
          (* FIXME t.tracker_last_conn < 1 only at first connect, so later failures will stay undetected! *)
          if file.file_tracker_connected && t.tracker_last_clients_num = 0 && t.tracker_last_conn < 1 then 
            begin
              if !verbose_msg_servers then
                lprintf_nl "Request error from tracker: disabling %s" (show_tracker_url t.tracker_url);
              t.tracker_status <- Disabled (intern "MLDonkey: Request error from tracker")
            end
          (* Send request to tracker *)
          else 
            let args = if String.length t.tracker_id > 0 then
                ("trackerid", t.tracker_id) :: args else args
            in
            let args = if String.length t.tracker_key > 0 then
                ("key", t.tracker_key) :: args else args
            in
            if !verbose_msg_servers then
              lprintf_nl "connect_trackers: connected:%s id:%s key:%s last_clients:%i last_conn-last_time:%i numwant:%s file: %s"
                (string_of_bool file.file_tracker_connected)
                t.tracker_id t.tracker_key t.tracker_last_clients_num
                (t.tracker_last_conn - last_time()) (try List.assoc "numwant" args with _ -> "_") file.file_name;

            match t.tracker_url with
            | `Http url ->
              let module H = Http_client in
              let r = {
                  H.basic_request with
                  H.req_url = Url.of_string ~args url;
                  H.req_proxy = !CommonOptions.http_proxy;
                  H.req_user_agent = get_user_agent ();
                  (* #4541 [egs]  supports redirect *)
                  H.req_max_retry = !!max_tracker_redirect;
                  H.req_filter_ip = (fun ip -> not (Ip.equal Ip.localhost ip));
                } in

              if !verbose_msg_servers then
                  lprintf_nl "Request sent to tracker %s for file: %s"
                    url file.file_name;
              H.wget r
                (fun fileres ->
                  t.tracker_last_conn <- last_time ();
                  file.file_tracker_connected <- true;
                  f t fileres)
            | `Other url -> assert false (* should have been disabled *)
            | `Udp (host,port) -> talk_to_udp_tracker host port args file t need_sources
        end

      else
        if !verbose_msg_servers then
          lprintf_nl "Request NOT sent to tracker %s - next request in %ds for file: %s"
            (show_tracker_url t.tracker_url) (t.tracker_interval - (last_time () - t.tracker_last_conn)) file.file_name
  ) enabled_trackers

let connect_trackers file event need_sources f =
  if !!use_trackers then connect_trackers file event need_sources f

let start_upload c =
  set_client_upload (as_client c) (as_file c.client_file);
  set_client_has_a_slot (as_client c) NormalSlot;
  Rate.update_no_change c.client_downloaded_rate;
  Rate.update_no_change c.client_upload_rate;
  c.client_last_optimist <- last_time();
  client_enter_upload_queue (as_client c);
  send_client c Unchoke

(** In this function we decide which peers will be
  uploaders. We send a choke message to current uploaders
  that are not in the next uploaders list. We send Unchoke
  for clients that are in next list (and not in current)
*)
let recompute_uploaders () =
  if !verbose_upload then lprintf_nl "recompute_uploaders";
  next_uploaders := choose_uploaders current_files;
  (*Send choke if a current_uploader is not in next_uploaders*)
  List.iter ( fun c -> if ((List.mem c !next_uploaders)==false) then
        begin
          set_client_has_a_slot (as_client c) NoSlot;
          (*we will let him finish his download and choke him on next_request*)
        end
  ) !current_uploaders;

  (*don't send Choke if new uploader is already an uploaders *)
  List.iter ( fun c ->
    if not (List.mem c !current_uploaders) then start_upload c
  ) !next_uploaders;
  current_uploaders := !next_uploaders


(****** Fabrice: why are clients which are disconnected removed ???
  These clients might still be useful to reconnect to, no ? *)


(** This function is called when a client is disconnected
  (be it by our side or its side).
  A client which disconnects (even only one time) is discarded.
  If it's an uploader which disconnects we recompute uploaders
  (see recompute_uploaders) immediately.
  @param c The client to disconnect
  @param reason The reason for the disconnection (see in BasicSocket.ml)
*)
let disconnect_client c reason =
  if !verbose_msg_clients then
    lprintf_nl "Client %d: disconnected: %s" (client_num c) (string_of_reason reason);
  begin
    match c.client_sock with
      NoConnection -> ()
    | ConnectionWaiting token ->
        cancel_token token;
        c.client_sock <- NoConnection
    | Connection sock  ->
        close sock reason;
        try
(*          List.iter (fun r -> CommonSwarming.free_range r) c.client_ranges; *)
          set_client_disconnected c reason;
          c.client_session_downloaded <- 0L;
          c.client_session_uploaded <- 0L;
          (try if c.client_good then count_seen c with _ -> ());
          (* this is not useful already done in the match
          (try close sock reason with _ -> ());   *)
(*---------not needed ?? VvvvvV---------------
          c.client_ranges <- [];
          c.client_block <- None;
          if not c.client_good then
            connection_failed c.client_connection_control;
          c.client_good <- false;
          c.client_sock <- NoConnection;
          c.client_chunks <- [];
          c.client_allowed_to_write <- zero;
          c.client_new_chunks <- [];
          c.client_interesting <- false;
          c.client_alrd_sent_interested <- false;
    -------------------^^^^^--------------------*)
          if (c.client_registered_bitfield) then
            begin
              match c.client_uploader with
                None -> ()
              | Some up ->
                  c.client_uploader <- None;
                  (* If the client registered a bitfield then
                     we must unregister him to update the swarmer
                     (Useful for availability)
                  *)
                  CommonSwarming.unregister_uploader up
(*        c.client_registered_bitfield <- false;
          for i = 0 to String.length c.client_bitmap - 1 do
            c.client_bitmap.[0] <- '0';
          done*)
            end;
          (* Don't test if a client have an upload slot because
             it don't have one (removed during earlier in
          set_client_disconnected c reason)
          *)
          if (List.mem c !current_uploaders) then
            begin
(*BTGlobals.remove_client*)
              remove_client c;
              recompute_uploaders ();
            end
          else  
            remove_client c;
        with _ -> ()
  end


(** Disconnect all clients of a file
  @param file The file to which we must disconnects all clients
*)
let disconnect_clients file =
  let must_keep = ref true in
    (match file_state file with
       | FilePaused | FileCancelled -> must_keep:=false
       | _-> ()
    );
  Hashtbl.iter (fun _ c ->
  if not ( !must_keep && (client_has_a_slot (as_client c) || c.client_interested)) then
    begin
      if !verbose_msg_clients then
        lprintf_file_nl (as_file file) "disconnect since download is finished";
      disconnect_client c Closed_by_user
    end
  ) file.file_clients


(** What to do when a file is finished
  @param file the finished file
*)
let download_finished file =
    if List.memq file !current_files then
      begin
        connect_trackers file "completed" false (fun _ _ -> 
          lprintf_file_nl (as_file file) "Tracker return: completed %s" file.file_name;
          ()); (*must be called before swarmer gets removed from file*)
        (*CommonComplexOptions.file_completed*)
        file_completed (as_file file);
        (* Remove the swarmer for this file as it is not useful anymore... *)
        CommonSwarming.remove_swarmer file.file_swarmer;
        file.file_swarmer <- None;
        (* At this point, the file state is FileDownloaded. We should not remove
           the file, because we continue to upload. *)
      end


(** Check if a file is finished or not.
  A file is finished if all blocks are verified.
  @param file The file to check status
*)
let check_finished swarmer file =
  if CommonSwarming.check_finished swarmer then
      download_finished file

let bits = [| 128; 64; 32;16;8;4;2;1 |]

(* Check/set bits in strings (bittorrent format) *)

let is_bit_set s n =
  (Char.code s.[n lsr 3]) land bits.(n land 7) <> 0

let set_bit s n =
  let i = n lsr 3 in
  s.[i] <- Char.unsafe_chr (Char.code (Bytes.get s i) lor bits.(n land 7))

(* Official client seems to use max_range_request 5 and max_range_len 2^14 *)
(* How much requests in the 'pipeline' *)
let max_range_requests = 5
(* How much bytes we can request in one Piece *)

let reserved () =
  let s = Bytes.make 8 '\x00' in
  s.[7] <- (match !bt_dht with None -> '\x00' | Some _ -> '\x01');
  s.[5] <- '\x10'; (* TODO bep9, bep10, notify clients about extended*)
  Bytes.unsafe_to_string s

(** handshake *)
let send_init client_uid file_id sock =
  let buf = Buffer.create 100 in
  buf_string8 buf  "BitTorrent protocol";
  Buffer.add_string buf (reserved ());
  Buffer.add_string buf (Sha1.direct_to_string file_id);
  Buffer.add_string buf (Sha1.direct_to_string client_uid);
  let s = Buffer.contents buf in
  write_string sock s

(** A wrapper to send Interested message to a client.
  (Send interested only if needed)
  @param c The client to send Interested
*)
let send_interested c =
  if c.client_interesting && (not c.client_alrd_sent_interested) then
    begin
      c.client_alrd_sent_interested <- true;
      send_client c Interested
    end


(** Send a Bitfield message to a client.
  @param c The client to send the Bitfield message
*)

let send_bitfield c =
  if not c.client_file.file_metadata_downloading then
  send_client c (BitField
      (
      match c.client_file.file_swarmer with
      | None ->
          (* This must be a seeded file... *)
          if !verbose_download then 
            lprintf_nl "Sending completed verified bitmap";
          let nchunks = Array.length c.client_file.file_chunks in
          let len = (nchunks+7)/8 in
          let s = Bytes.make len '\000' in
          for i = 0 to nchunks - 1 do
            set_bit s i
          done;
          Bytes.unsafe_to_string s
      | Some swarmer ->
          let bitmap = CommonSwarming.chunks_verified_bitmap swarmer in
          if !verbose_download then 
            lprintf_nl "Sending verified bitmap: [%s]" (VB.to_string bitmap);
          let len = (VB.length bitmap + 7)/8 in
          let s = Bytes.make len '\000' in
          VB.iteri (fun i c ->
            if c = VB.State_verified then set_bit s i) bitmap;
          Bytes.unsafe_to_string s
    ))

let counter = ref 0

let parse_reserved rbits c =
  let has_bit pos h = Char.code rbits.[pos] land h <> 0 in
  
  c.client_dht <- has_bit 7 0x01;
  c.client_cache_extension <- has_bit 7 0x02;
  c.client_fast_extension <- has_bit 7 0x04;

  c.client_utorrent_extension <- has_bit 5 0x10;

  c.client_azureus_messaging_protocol <- has_bit 0 0x80

let send_extended_handshake c file =
  let module B = Bencode in
  let msg = (B.encode (B.Dictionary [(* "e",B.Int 0L; *)
                                     "m", (B.Dictionary ["ut_metadata", B.Int 1L]);
                                     (* "metadata_size", B.Int (-1L) *)])) in begin
    send_client c (Extended (Int64.to_int 0L, msg));
  end

let send_extended_piece_request c piece file =
  let module B = Bencode in
  let msg = (B.encode (B.Dictionary ["msg_type", B.Int 0L; (* 0 is request subtype*)
                                     "piece", B.Int piece; ])) in begin
    send_client c (Extended (Int64.to_int c.client_ut_metadata_msg, msg));
  end

let show_client c =
  let (ip,port) = c.client_host in
  Printf.sprintf "%s:%d %S" (Ip.to_string ip) port (brand_to_string c.client_brand)

(** This function is called to parse the first message that
  a client send.
  @param counter client num
  @param cc Expected client (probably useless now that we don't save any client)
  @param init_sent A boolean to know if we sent this client the handshake message
  @param gconn Don't know
  @param sock The socket we use for this client
  @param proto Unused (required by tuple type?)
  @param file_id The file hash (sha1) of the file involved in this exchange
*)
(* removed: @param peer_id The hash (sha1) of the client. (Should be checked)
*)
let rec client_parse_header counter cc init_sent gconn sock
    (proto, rbits, file_id) =
  try
    set_lifetime sock 600.;
    if !verbose_msg_clients then
      lprintf_nl "client_parse_header %d" counter;

    let file = Hashtbl.find files_by_uid file_id in
    if !verbose_msg_clients then
      lprintf_file_nl (as_file file) "file found";
    let ccc, cc_country_code = !cc in
    let c =
      match ccc with
        None ->
          let c = new_client file Sha1.null (TcpBufferedSocket.peer_addr sock) cc_country_code in
          if !verbose_connect then lprintf_file_nl (as_file file) "Client %d: incoming connection" (client_num c);
          cc := (Some c), cc_country_code;
          c
      | Some c ->
          (* Does it happen that this c was already used to connect successfully?
             If yes then this must happen: *)
          c.client_received_peer_id <- false;
          if cc_country_code <> None && c.client_country_code = None then
            c.client_country_code <- cc_country_code;
          c
          (* client could have had Sha1.null as peer_id/uid *)
          (* this is to be done, later
          if c.client_uid <> peer_id then
          c.client_software <- (parse_software (Sha1.direct_to_string peer_id));
          c
           *)

(*          if c.client_uid <> peer_id then begin
              lprintf "Unexpected client by UID\n";
              let ccc = new_client file peer_id (TcpBufferedSocket.host sock) in
              lprintf "CLIENT %d: testing instead of %d\n"
                (client_num ccc) (client_num c);
              (match ccc.client_sock with
                  Connection _ ->
                    lprintf_nl "[BT]: This client is already connected";
                    close sock (Closed_for_error "Already connected");
                    remove_client ccc;
                    c
                | _ ->
                    lprintf_nl "[BT]: Client %d: recovered by UID" (client_num ccc);
                    remove_client c;
                    cc := Some ccc;
                    ccc)
            end else
            c       *)
    in

    if !verbose_msg_clients then
        lprintf_nl "Client %d: Connected from %s" (client_num c) (show_client c);

    parse_reserved rbits c;

    (match c.client_sock with
        NoConnection ->
          if !verbose_msg_clients then begin
              let (ip,port) = c.client_host in
              lprintf_nl "No connection to client (%s:%d)!!!" (Ip.to_string ip) port;
            end;
          c.client_sock <- Connection sock
      | ConnectionWaiting token ->
          cancel_token token;
          if !verbose_msg_clients then
            lprintf_nl "Waiting for connection to client !!!";
          c.client_sock <- Connection sock
      | Connection s when s != sock ->
          if !verbose_msg_clients then
            lprintf_nl "CLIENT %d: IMMEDIATE RECONNECTION" (client_num c);
          disconnect_client c (Closed_for_error "Reconnected");
          c.client_sock <- Connection sock;
      | Connection _  -> ()
    );

    set_client_state (c) (Connected (-1));
    if not init_sent then
      begin
        c.client_incoming <- true;
        send_init !!client_uid file_id sock;
        send_extended_handshake c file;
      end;
    connection_ok c.client_connection_control;
    if !verbose_msg_clients then
      lprintf_nl "file and client found";
(*    if not c.client_incoming then *)
    send_bitfield c; (* BitField is always the first message *)
    begin match c.client_dht, !bt_dht with
    | true, Some dht -> send_client c (DHT_Port dht.BT_DHT.M.dht_port)
    | _ -> ()
    end;
    c.client_blocks_sent <- file.file_blocks_downloaded;
(*
      TODO !!! : send interested if and only if we are interested
      -> we must recieve at least other peer bitfield.
      in common swarmer -> compare : partition -> partition -> bool
    *)

    set_rtimeout sock !!client_timeout;
    (* Once parsed succesfully we define the function client_to_client
       to be the function used when a message is read *)
    gconn.gconn_handler <- Reader (fun gconn sock ->
        bt_handler TcpMessages.parsing (client_to_client c) c sock
    );

    let b = TcpBufferedSocket.buf sock in
(* The receive buffer is normally not empty now, lets parse the rest, most likely PeerID *)
    if b.len <> 0 then
      ignore (bt_handler TcpMessages.parsing (client_to_client c) c sock);

(* Some newer clients send more opcodes in their handshake packet, lets parse them now.
   Using "while b.len <> 0 do ... done" is not possible here because libtorrent clients
   send unparsable five extra bytes after their PeerID which would result into a loop *)
    if b.len <> 0 then
      ignore (bt_handler TcpMessages.parsing (client_to_client c) c sock);
    ()
  with
      | Not_found ->
          let (ip,port) = (TcpBufferedSocket.peer_addr sock) in
          if !verbose_unexpected_messages then
            lprintf_nl "Client %s:%d requested a file that is not shared [%s]"
              (Ip.to_string ip) port (Sha1.to_hexa file_id)
      | exn ->
          lprintf_nl ~exn "client_parse_header";
          close sock (Closed_for_exception exn);
          raise exn


(** Update the bitmap of a client. Unclear if it is still useful.
  @param c The client which we want to update.
*)
and update_client_bitmap c =
  let file = c.client_file in
  
  let swarmer = match file.file_swarmer with
      None -> assert false
    | Some swarmer -> swarmer 
  in
  
  let up =
    match c.client_uploader with
      None ->
        let up = CommonSwarming.register_uploader swarmer (as_client c)
          (AvailableIntervals []) in
        c.client_uploader <- Some up;
        up
    | Some up ->
        up
  in

  let bitmap = match c.client_bitmap with
      None ->
        let len = CommonSwarming.partition_size swarmer in
        let bitmap = Bitv.create len false in
        c.client_bitmap <- Some bitmap;
        bitmap
    | Some bitmap -> bitmap
  in

  if c.client_new_chunks <> [] then begin
    let chunks = c.client_new_chunks in
    c.client_new_chunks <- [];
    List.iter (fun n -> Bitv.set bitmap n true) chunks;
    CommonSwarming.update_uploader_intervals up (AvailableBitv bitmap);
  end


(** In this function we decide which piece we must request from client.
  @param sock Socket of the client
  @param c The client
*)
and get_from_client sock (c: client) =
  let file = c.client_file in
  (* Check if there's not enough requests in the 'pipeline'
     and if a request can be send (not choked and file is downloading) *)
  if List.length c.client_ranges_sent < max_range_requests 
      && file_state file = FileDownloading 
      && (c.client_choked == false) 
    then
  (* num is the number of the piece, x and y are the position
     of the subpiece in the piece(!), r is a (CommonSwarmer) range *)

    let up = match c.client_uploader with
        None -> assert false
      | Some up -> up in
    let swarmer = CommonSwarming.uploader_swarmer up in

  try
  
    let num, x,y, r =

      if !verbose_msg_clients then
        lprintf_file_nl (as_file file) "CLIENT %d: Finding new range to send" (client_num c);

      if !verbose_swarming then begin
        lprintf_n "Current download:\n  Current chunks: ";

        try
          List.iter (fun (x,y) -> lprintf "%Ld-%Ld " x y) c.client_chunks
        with _ -> lprintf "No Chunks";
          
        lprint_newline ();
          
        lprintf_n "Current ranges: ";
          
        List.iter (fun (p1,p2, r) ->
          let (x,y) = CommonSwarming.range_range r in
          lprintf "%Ld-%Ld[%Ld-%Ld] " p1 p2 x y
        ) c.client_ranges_sent;

        match c.client_range_waiting with
        | None -> ()
        | Some (x,y,r) -> lprintf "Waiting %Ld-%Ld" x y;

        lprint_newline ();
          
        lprintf_n "Current blocks: ";
          
        match c.client_chunk with
        | None -> lprintf "none"
        | Some (chunk, blocks) -> List.iter (fun b -> 
            CommonSwarming.print_block b.up_block) blocks;

        lprint_newline ();
      
        lprintf_file_nl (as_file file) "Finding Range:";
      end;

      try

        (*We must find a block to request first, and then
          some range inside this block
        *)

        let rec iter () =

          match c.client_chunk with

          | None -> 

              if !verbose_swarming then lprintf_file_nl (as_file file) "No block";
              update_client_bitmap c;
              (try CommonSwarming.verify_one_chunk swarmer with _ -> ());
              (*Find a free block in the swarmer*)
              let chunk, blocks = CommonSwarming.find_blocks up in
              if !verbose_swarming then begin 
                lprintf_n "Blocks Found: "; List.iter (fun b ->
                  CommonSwarming.print_block b.up_block) blocks;
                lprint_newline ()
              end;
              c.client_chunk <- Some (chunk, blocks);

             (*We put the found block in client_block to
               request range in this block. (Useful for
               not searching each time a new block)
              *)

              iter ()

          | Some (chunk, blocks) ->

              if !verbose_swarming then begin
                lprintf_n "Current Blocks: "; List.iter (fun b ->
                  CommonSwarming.print_block b.up_block) blocks;
                lprint_newline ()
              end;

              try
                (*Given a block find a range inside*)
                let (x,y,r) =
                  match c.client_range_waiting with
                  | Some (x,y,r) ->
                        c.client_range_waiting <- None;
                        (x,y,r)
                  | None -> 
                        CommonSwarming.find_range up (min max_range_len file.file_piece_size)
                in

                let (x,y,r) =
                  
                  if y -- x > max_range_len then begin
                      c.client_range_waiting <- Some (x ++ max_range_len, y, r);
                      (x, x ++ max_range_len, r)
                  end else 
                      (x,y,r) 
                in
                
                  c.client_ranges_sent <- c.client_ranges_sent @ [x,y, r];
(*                CommonSwarming.alloc_range r; *)
                
(* naughty, naughty, was computing a block number instead of a chunk
   number. Only matters with merged downloads, and even then other
   clients didn't seem to care (?), so the bug remained hidden *)
                  if !verbose_swarming then 
                    lprintf_file_nl (as_file file) "Asking %d For Range %Ld-%Ld" chunk x y;
                      
                  chunk, x -- file.file_piece_size ** Int64.of_int chunk, y -- x, r

              with Not_found ->
  
                  (*If we don't find a range to request inside the block,
                    iter to choose another block*)
                  if !verbose_swarming then
                    lprintf_nl "Could not find range in current block";
(*                  c.client_blocks <- List2.removeq b c.client_blocks; *)

                  c.client_chunk <- None;

                  iter ()
        in

        iter ()

      with Not_found ->
          (*If we don't find a block to request we can check if the
            file is finished (if there's missing pieces we can't decide
            that the file is finished because we didn't found
            a block to ask)
          *)
          if !verbose_swarming then
            lprintf_nl "Unable to get a block !!";
          CommonSwarming.compute_bitmap swarmer;
          check_finished swarmer file;
          raise Not_found
    in

    send_client c (Request (num,x,y));

    if !verbose_msg_clients then
      lprintf_file_nl (as_file file) "CLIENT %d: Asking %s For Range %Ld-%Ld"
        (client_num c) (Sha1.to_string c.client_uid) x y

  with Not_found ->
        if not (CommonSwarming.check_finished swarmer) && !verbose_download then
          lprintf_file_nl (as_file file) "BTClient.get_from_client ERROR: can't find a block to download and file is not yet finished for file : %s..." file.file_name


(** In this function we match a message sent by a client
  and react according to this message.
  @param c The client which sent us a message
  @param sock The socket used for this client
  @param msg The message sent by the client
*)
and client_to_client c sock msg =
  if !verbose_msg_clients then begin
      let (ip,port) = (TcpBufferedSocket.peer_addr sock) in
      let (timeout, next) = get_rtimeout sock in
      lprintf_nl "CLIENT %d(%s:%d): (%d, %d,%d) Received %s"
        (client_num c) (Ip.to_string ip) port
      (last_time ())
      (int_of_float timeout)
      (int_of_float next)
      (TcpMessages.to_string msg);
  end;

  let file = c.client_file in

(* Sending the "Have" message was moved to bTGlobals so this is useless *)
(*  if c.client_blocks_sent != file.file_blocks_downloaded then begin
      let rec iter list =
        match list with
          [] -> ()
        | b :: tail when tail == c.client_blocks_sent ->
            c.client_blocks_sent <- list;
            let (num,_,_) = CommonSwarming.block_block b  in
            send_client c (Have (Int64.of_int num))
        | _ :: tail -> iter tail
      in
      iter file.file_blocks_downloaded
    end;*)

  try
    match msg with
    | Piece (num, offset, s, pos, len) ->
        (*A Piece message contains the data*)
        set_client_state c (Connected_downloading (file_num file));
        (*flag it as a good client *)
        c.client_good <- true;
        if file_state file = FileDownloading then begin
            let position = offset ++ file.file_piece_size *.. num in
            let up = match c.client_uploader with
                None -> assert false
              | Some up -> up in
            let swarmer = CommonSwarming.uploader_swarmer up in

            if !verbose_msg_clients then
              (match c.client_ranges_sent with
                  [] -> lprintf_file_nl (as_file file) "EMPTY Ranges !!!"
                | (p1,p2,r) :: _ ->
                    let (x,y) = CommonSwarming.range_range r in
                    lprintf_file_nl (as_file file) "Current range from %s : %Ld [%d] (asked %Ld-%Ld[%Ld-%Ld])"
                      (show_client c) position len
                      p1 p2 x y
              );

            let old_downloaded =
              CommonSwarming.downloaded swarmer in
(*            List.iter CommonSwarming.free_range c.client_ranges;       *)
            CommonSwarming.received up
              position s pos len;
(*            List.iter CommonSwarming.alloc_range c.client_ranges; *)
            let new_downloaded =
              CommonSwarming.downloaded swarmer in

            (*Update rate and amount of data received from client*)
            count_download c (new_downloaded -- old_downloaded);
            (* use len here with max_dr quickfix *)
            Rate.update c.client_downloaded_rate ~amount:len;
            (* count bytes downloaded from network for this file *)
            file.file_session_downloaded <- file.file_session_downloaded ++ (Int64.of_int len);
            if !verbose_msg_clients then
              (match c.client_ranges_sent with
                  [] -> lprintf_file_nl (as_file file) "EMPTY Ranges !!!"
                | (p1,p2,r) :: _ ->
                    let (x,y) = CommonSwarming.range_range r in
                    lprintf_file_nl (as_file file) "Received %Ld [%d] %Ld-%Ld[%Ld-%Ld] -> %Ld"
                      position len
                      p1 p2 x y
                      (new_downloaded -- old_downloaded)
              );

(* changed 2.5.28 should have been done before !
             if new_downloaded <> old_downloaded then
              add_file_downloaded (as_file file)
                (new_downloaded -- old_downloaded); *)
          end;
        begin
          match c.client_ranges_sent with
            [] -> ()
          | r :: tail ->
(*              CommonSwarming.free_range r; *)
              c.client_ranges_sent <- tail;
        end;
        get_from_client sock c;

        (* Check if the client is still interesting for us... *)
        check_if_interesting file c

    | PeerID p ->
      (* Disconnect if that is ourselves. *)
      c.client_uid <- Sha1.direct_of_string p;
      if not (c.client_uid = !!client_uid) then
        begin
          let brand, release = parse_software p in
          c.client_brand <- brand;
          c.client_release <- release;
          send_client c Choke;
          c.client_sent_choke <- true;
        end
      else
        disconnect_client c Closed_by_user


    | BitField p ->
        (*A bitfield is a summary of what a client have*)
      if !verbose_msg_clients then
        lprintf_file_nl (as_file file) "Bitfield message,  metadata state %B" c.client_file.file_metadata_downloading;
      if not c.client_file.file_metadata_downloading then
        begin
          match c.client_file.file_swarmer with
            None -> ()
          | Some swarmer ->
              c.client_new_chunks <- [];

              let npieces = CommonSwarming.partition_size swarmer in
              let nbits = String.length p * 8 in

              if nbits < npieces then begin
                lprintf_file_nl (as_file file) "Error: expected bitfield of atleast %d but got %d" npieces nbits;
                disconnect_client c (Closed_for_error "Wrong bitfield length")
              end else begin

                let bitmap = CommonSwarming.chunks_verified_bitmap swarmer in

                for i = 0 to npieces - 1 do
                  if is_bit_set p i then begin
                    c.client_new_chunks <- i :: c.client_new_chunks;
                    match VB.get bitmap i with
                    | VB.State_missing | VB.State_partial ->
                      c.client_interesting <- true
                    | VB.State_complete | VB.State_verified -> ()
                  end 
                done;

                update_client_bitmap c;
                c.client_registered_bitfield <- true;

                if c.client_interesting then
                  send_interested c;

                if !verbose_msg_clients then
                  lprintf_file_nl (as_file file) "New BitField Registered";

                (*  for i = 1 to max_range_requests - List.length c.client_ranges do
                      (try get_from_client sock c with _ -> ())
                    done
                *)

            end;
        end;
        (* Note: a bitfield must only be sent after the handshake and before everything else: NOT here *)

    | Have n ->
        (* A client can send a "Have" without sending a Bitfield *)
        if not c.client_file.file_metadata_downloading then
        begin
          match c.client_file.file_swarmer with
            None -> ()
          | Some swarmer ->
              let n = Int64.to_int n in
              let bitmap = CommonSwarming.chunks_verified_bitmap swarmer in
              (* lprintf_nl "verified: %c;" (VB.state_to_char (VB.get bitmap n)); *)
              (* if the peer has a chunk we don't, tell him we're interested and update his bitmap *)
              match VB.get bitmap n with
              | VB.State_missing | VB.State_partial ->
                  c.client_interesting <- true;
                  send_interested c;
                  c.client_new_chunks <- n :: c.client_new_chunks;
                  update_client_bitmap c;
              | VB.State_complete | VB.State_verified -> ()

(*        begin
          match c.client_bitmap, c.client_uploader with
            Some bitmap, Some up ->
              let swarmer = CommonSwarming.uploader_swarmer up in
              let n = Int64.to_int n in
              if bitmap.[n] <> '1' then

                let verified = CommonSwarming.verified_bitmap swarmer in
                if verified.[n] < '2' then begin
                    c.client_interesting <- true;
                    send_interested c;
                    c.client_new_chunks <- n :: c.client_new_chunks;
                    if c.client_block = None then begin
                        update_client_bitmap c;
(*   for i = 1 to max_range_requests -
                    List.length c.client_ranges do
                    (try get_from_client sock c with _ -> ())
                  done*)
                      end
                  end
          | None, Some _ -> lprintf_nl "no bitmap but client_uploader";
    | Some _ , None ->lprintf_nl "bitmap but no client_uploader";
    | None, None -> lprintf_nl "no bitmap no client_uploader";
        end
*)
        end

    | Interested ->
        c.client_interested <- true;

    | Choke ->
        begin
          set_client_state (c) (Connected (-1));
          (* remote peer will clear the list of range we sent *)
          begin
           match c.client_uploader with
              None ->
                (* Afaik this is no protocol violation and happens if the client
                   didn't send a client bitmap after the handshake. *)
                  if !verbose_msg_clients then lprintf_file_nl (as_file file) "%s : Choke send, but no client bitmap"
                    (show_client c)
            | Some up ->
                CommonSwarming.clear_uploader_intervals up
          end;
          c.client_ranges_sent <- [];
          c.client_range_waiting <- None;
          c.client_choked <- true;
        end

    | NotInterested ->
        c.client_interested <- false;

    | Unchoke ->
        begin
          c.client_choked <- false;
          (* remote peer cleared our request : re-request *)
          for i = 1 to max_range_requests -
            List.length c.client_ranges_sent do
            (try get_from_client sock c with _ -> ())
          done
        end

    | Request (n, pos, len) ->
        if len > max_request_len then begin
            close sock (Closed_for_error "Request longer than 1<<16");
            raise Exit
          end;

        if !CommonGlobals.has_upload = 0 then
          begin
            if client_has_a_slot (as_client c) then
              begin
(*                lprintf "Received request for upload\n"; *)
                (match c.client_upload_requests with
                  [] ->
                      CommonUploads.ready_for_upload (as_client c);
                  | _ -> ());
                c.client_upload_requests <- c.client_upload_requests @ [n,pos,len];
                let file = c.client_file in
                match file.file_shared with
                    None -> ()
                  | Some s ->
                      begin
                        s.impl_shared_requests <- s.impl_shared_requests + 1;
                        shared_must_update (as_shared s)
                      end
              end
            else
              begin
                send_client c Choke;
                c.client_sent_choke <- true;
                c.client_upload_requests <- [];
              end
          end;

    | Ping -> ()
        (* We don't 'generate' a Ping message on a Ping. *)

    | Cancel (n, pos, len) ->
        (* if we receive a cancel message from a peer, remove request *)
        if client_has_a_slot (as_client c) then
          c.client_upload_requests <- List2.remove_first (n, pos, len) c.client_upload_requests
        else
          if !verbose_msg_clients then
            lprintf_file_nl (as_file file) "Error: received cancel request but client has no slot"

    | Extended (extmsg, payload) ->
      (* extmsg: 0 handshake, N other message previously declared in handshake.
         atm ignore extended messages if were not currently in metadata state.
         TODO when were not in metadata state we should be friendly and answer metadata requests
      *)
      let module B = Bencode in
      if file.file_metadata_downloading then begin
        (* since we got at least one extended handshake from the peer, it should be okay to
           send a handshake back now. we need to send it so the remote client knows how
           to send us messages back.
           this should of course be moved but I dont know where yet.
           also we shouldnt send more than one handshake of course...
        *)
        if !verbose_msg_clients then
          lprintf_file_nl (as_file file) "Got extended msg: %d %s" extmsg (String.escaped payload);

        match extmsg with
            0x0 ->
            if !verbose_msg_clients then
              lprintf_file_nl (as_file file) "Got extended handshake";
            let dict = Bencode.decode payload in begin
              match dict with
                  B.Dictionary list ->
                    List.iter (fun (key,value) ->
                      match key, value with
                        | "metadata_size", B.Int n ->
                            if !verbose_msg_clients then
                              lprintf_file_nl (as_file file) "Got metadata size %Ld" n;
                            c.client_file.file_metadata_size <- n;
                        | "m", B.Dictionary  mdict ->
                          if !verbose_msg_clients then
                            lprintf_file_nl (as_file file) "Got meta dict";
                          List.iter (fun (key,value) ->
                            match key, value with
                                "ut_metadata", B.Int n ->
                                  if !verbose_msg_clients then
                                    lprintf_file_nl (as_file file) "ut_metadata is %Ld " n;
                                  c.client_ut_metadata_msg <- n;
                              | _ -> ();
                          ) mdict;

                        | _ -> () ;
                    ) list;
                    (* okay so now we know what to ask for, so ask for metadata now
                       since metadata can be larger than 16k which is the limit, the transfer needs to be chunked, so
                       it is not really right to make the query here. but its a start.
                       also im just asking for piece 0.
                       (we should also check that we actually got the metadata info before proceeding)
                    *)
                    send_extended_handshake c file;
                    send_extended_piece_request c c.client_file.file_metadata_piece file;
                  |_ -> () ;
            end;
          | 0x01 -> (* ut_metadata is 1 because we asked it to be 1 in the handshake
                       the msg_type is probably
                       1 for data,
                       but could be 0 for request(unlikely since we didnt advertise we had the meta)
                       2 for reject, also unlikely since peers shouldnt advertise if they dont have(but will need handling in the end)

                       {'msg_type': 1, 'piece': 0, 'total_size': 3425}
                       after the dict comes the actual piece
                    *)
            if !verbose_msg_clients then
              lprintf_file_nl (as_file file) "Got extended ut_metadata message";
            let msgtype = ref 0L in begin
              begin
                match B.decode payload with
                    B.Dictionary list ->
                      List.iter (fun (key,value) ->
                        match key, value with
                            "msg_type", B.Int n ->
                              if !verbose_msg_clients then
                                lprintf_file_nl (as_file file) "msg_type %Ld" n;
                              msgtype := n;
                          | "piece", B.Int n ->
                            if !verbose_msg_clients then
                              lprintf_file_nl (as_file file) "piece %Ld" n;
                            file.file_metadata_piece <- n;
                          | "total_size", B.Int n ->
                             if !verbose_msg_clients then
                              lprintf_file_nl (as_file file) "total_size %Ld" n; (* should always be the same as received in the initial handshake i suppose *)
                          |_ -> () ;
                      ) list;
                  |_ -> () ;
              end;
              match !msgtype with
                  1L ->
                    let last_piece_index = (Int64.div file.file_metadata_size 16384L) in
                    if !verbose_msg_clients then
                      lprintf_file_nl (as_file file) "handling metadata piece %Ld of %Ld"
                        file.file_metadata_piece
                        last_piece_index;
                        (* store the metadata piece in memory *)
                    file.file_metadata_chunks.(1 + (Int64.to_int file.file_metadata_piece)) <- payload;
                        (* possibly write metadata to disk *)
                    if file.file_metadata_piece >=
                      (Int64.div file.file_metadata_size 16384L) then begin
                        if !verbose_msg_clients then
                          lprintf_file_nl (as_file file) "this was the last piece";
                            (* here we should simply delete the current download, and wait for mld to pick up the new torrent file *)
                            (* the entire payload is currently in the array, TODO *)
                        let newtorrentfile = (Printf.sprintf "%s/BT-%s.torrent"
                                                      (Filename2.temp_dir_name ())
                                                      (Sha1.to_string file.file_id)) in
                        let fd = Unix32.create_rw  newtorrentfile  in
                        let fileindex = ref 0L in
                        begin
                          (* the ee is so we can use the same method to find the
                             start of the payload for the real payloads as well as the synthetic ones
                             *)
                          file.file_metadata_chunks.(0) <- "eed4:info";
                          file.file_metadata_chunks.(2 + Int64.to_int last_piece_index) <- "eee";
                          try
                            Array.iteri (fun index chunk ->
                            (* regexp ee is a fugly way to find the end of the 1st dict before the real payload *)
                              let metaindex = (2 + (Str.search_forward  (Str.regexp_string "ee") chunk 0 )) in
                              let chunklength = ((String.length chunk) - metaindex) in
                              Unix32.write fd !fileindex (Bytes.unsafe_of_string chunk)
                                metaindex
                                chunklength;
                              fileindex := Int64.add !fileindex  (Int64.of_int chunklength);
                              ();
                            ) file.file_metadata_chunks;
                          with e -> begin
                            (* TODO ignoring errors for now, the array isnt really set up right anyway yet *)
                            (*
                            lprintf_file_nl (as_file file) "Error %s saving metadata"
                              (Printexc2.to_string e)
                            *) ()
                          end;
                          (* Yay, now the new torrent is on disk! amazing! However, now we need to kill the dummy torrent
                             and restart it with the fresh real torrent *)

                          (* it seems we need to use the dynamic interface... *)
                          if !verbose then
                            lprintf_file_nl (as_file file) "cancelling metadata download ";
                          let owner = file.file_file.impl_file_owner in
                          let group = file.file_file.impl_file_group in begin
                            CommonInteractive.file_cancel (as_file file) owner ;
                            (* hack_op_file_cancel c.client_file;   *)
                            if !verbose then
                              lprintf_file_nl (as_file file) "starting download from metadata torrent %s" newtorrentfile  ;
                            ignore(CommonNetwork.network_parse_url BTGlobals.network newtorrentfile owner group);
                          end;
                          (try Sys.remove newtorrentfile with _ -> ())
                        end;

                      end
                    else begin
                          (* now ask for the next metadata piece, if any *)
                      let nextpiece = (Int64.succ file.file_metadata_piece) in begin
                        if !verbose_msg_clients then
                          lprintf_file_nl (as_file file) "asking for the next piece %Ld" nextpiece;
                        send_extended_piece_request c nextpiece file;
                      end;
                    end;
                |_ ->
                  if !verbose_msg_clients then
                    lprintf_file_nl (as_file file) "unmatched extended subtype" ;
            end;

          | _ ->
            if !verbose_msg_clients then
              lprintf_file_nl (as_file file) "Got extended other msg ";
      end;

    | DHT_Port port ->
        match !bt_dht with
        | None ->
          if !verbose_msg_clients then
            lprintf_file_nl (as_file file) "Received DHT PORT when DHT is disabled. From %s" (show_client c)
        | Some dht ->
          BT_DHT.M.ping dht (fst c.client_host, port) begin function
          | None ->
            if !verbose then
              lprintf_file_nl (as_file file) "Peer %s didn't reply to DHT ping on port %d" (show_client c) port
          | Some (id,addr) ->
            BT_DHT.update dht Kademlia.Good id addr
          end

  with e ->
      lprintf_file_nl (as_file file) "Error %s while handling MESSAGE: %s" (Printexc2.to_string e) (TcpMessages.to_string msg)


(** The function used to connect to a client.
The connection is not immediately initiated. It will
be put in a fifo and dequeued according to
!!max_connections_per_second. (@see commonGlobals.ml)
@param c The client we must connect
*)
let connect_client c =
  if can_open_connection connection_manager &&
    (let (ip,port) = c.client_host in
     match !Ip.banned (ip, c.client_country_code) with
       None -> true
     | Some reason ->
         if !verbose_connect then
           lprintf_nl "%s:%d (%s), blocked: %s"
             (Ip.to_string ip) port
             (fst (Geoip.get_country_code_name c.client_country_code))
             reason;
         false)
  then
  match c.client_sock with
    NoConnection ->

      let token =
        add_pending_connection connection_manager (fun token ->
            try
              if !verbose_msg_clients then
                lprintf_nl "CLIENT %d: connect_client" (client_num c);
              let (ip,port) = c.client_host in
              if !verbose_msg_clients then
                lprintf_nl "connecting %s:%d" (Ip.to_string ip) port;
              connection_try c.client_connection_control;
                begin
                  let sock = connect token "bittorrent download"
                      (Ip.to_inet_addr ip) port
                      (fun sock event ->
                        match event with
                          BASIC_EVENT LTIMEOUT ->
                            if !verbose_msg_clients then
                              lprintf_nl "CLIENT %d: LIFETIME" (client_num c);
                            close sock Closed_for_timeout
                        | BASIC_EVENT RTIMEOUT ->
                            if !verbose_msg_clients then
                              lprintf_nl "CLIENT %d: RTIMEOUT (%d)" (client_num c)
                              (last_time ())
                              ;
                            close sock Closed_for_timeout
                        | BASIC_EVENT (CLOSED r) ->
                            begin
                              match c.client_sock with
                              | Connection s when s == sock ->
                                  disconnect_client c r
                              | _ -> ()
                            end;
                        | _ -> ()
                    )
                  in
                  c.client_sock <- Connection sock;
                  set_lifetime sock 600.;
                  TcpBufferedSocket.set_read_controler sock download_control;
                  TcpBufferedSocket.set_write_controler sock upload_control;
                  TcpBufferedSocket.set_rtimeout sock 30.;
                  let file = c.client_file in

                  if !verbose_msg_clients then
                    lprintf_file_nl (as_file file) "READY TO DOWNLOAD FILE";

                  send_init !!client_uid file.file_id sock;
                  send_extended_handshake c file;

(* Fabrice: Initialize the client bitmap and uploader fields to <> None *)
                  update_client_bitmap c;
(*              (try get_from_client sock c with _ -> ());*)
                  incr counter;
                  (*We 'hook' the client_parse_header function to the socket
                    This function will then be called when the first message will
                    be parsed
                  *)
                  set_bt_sock sock !verbose_msg_clients
                    (BTHeader (client_parse_header !counter (ref ((Some c), c.client_country_code)) true))
                end
            with exn ->
                lprintf_nl ~exn "connecting to client";
                disconnect_client c (Closed_for_exception exn)
        );
      (*Since this is a pending connection put ConnectionWaiting
        in client_sock
      *)
      in
      c.client_sock <- ConnectionWaiting token
  | _ -> ()


(** The Listen function (very much like in C : TCP Socket Server).
Monitors client connection to us.
*)
let listen () =
  try
    let s = TcpServerSocket.create "bittorrent client server"
        (Ip.to_inet_addr !!client_bind_addr)
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s,
              Unix.ADDR_INET(from_ip, from_port)) ->
              (*Receiving an event TcpServerSocket.CONNECTION from
                the TcpServerSocket means that a new client try
                to connect to us
              *)
              let ip = (Ip.of_inet_addr from_ip) in
              let cc = Geoip.get_country_code_option ip in
              if !verbose_sources > 1 then lprintf_nl "CONNECTION RECEIVED FROM %s"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ;
              (*Reject this connection if we don't want
                to bypass the max_connection parameter
              *)
              if can_open_connection connection_manager &&
                (match !Ip.banned (ip, cc) with
                   None -> true
                 | Some reason ->
                     if !verbose_connect then
                       lprintf_nl "%s:%d (%s) blocked: %s"
                         (Ip.to_string ip) from_port
                         (fst (Geoip.get_country_code_name cc))
                         reason;
                     false)
              then
                begin
                  let token = create_token connection_manager in
                  let sock = TcpBufferedSocket.create token
                      "bittorrent client connection" s
                      (fun sock event ->
                        match event with
                          BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
                          (*monitor read and life timeout on client
                            sockets
                          *)
                            close sock Closed_for_timeout
                        | _ -> ()
                    )
                  in
                  TcpBufferedSocket.set_read_controler sock download_control;
                  TcpBufferedSocket.set_write_controler sock upload_control;

                  let c = ref (None, cc) in
                  TcpBufferedSocket.set_closer sock (fun _ r ->
                      match fst !c with
                        Some c ->  begin
                            match c.client_sock with
                            | Connection s when s == sock ->
                                disconnect_client c r
                            | _ -> ()
                          end
                      | None -> ()
                  );
                  set_rtimeout sock 30.;
                  incr counter;
                  (*Again : 'hook' client_parse_header to the socket*)
                  set_bt_sock sock !verbose_msg_clients
                    (BTHeader (client_parse_header !counter c false));
                end
              else
               (*don't forget to close the incoming sock if we can't
                 open a new connection
               *)
                Unix.close s
          | _ -> ()
      ) in
    listen_sock := Some s;
    ()
  with exn ->
      if !verbose_connect then
        lprintf_nl ~exn "init bittorrent server"


(** This function send keepalive messages to all connected clients
  (and update socket lifetime)
*)
let send_pings () =
  List.iter (fun file ->
      Hashtbl.iter (fun _ c ->
          match c.client_sock with
          | Connection sock ->
              send_client c Ping;
              set_lifetime sock 130.;
          | _ -> ()
      ) file.file_clients
  ) !current_files

open Bencode


(** Check each clients for a given file if they are connected.
 If they aren't, try to connect them
  *)
let resume_clients file =
  Hashtbl.iter (fun _ c ->
      try
        match c.client_sock with
        | Connection sock -> ()
            (*i think this one is not really usefull for debugging
              lprintf_nl "[BT]: RESUME: Client is already connected"; *)
        | _ ->
            (try
               (*test if we can connect client according to the its
                 connection_control.
                 Currently the delay between two try is 120 seconds.
               *)
               if connection_can_try c.client_connection_control then
                 connect_client c
               else
                 print_control c.client_connection_control
             with _ -> ())
      with exn ->
          if !verbose_connect then
            lprintf_file_nl ~exn (as_file file) "resume_clients"
  ) file.file_clients

let () =
  resume_clients_hook := resume_clients

(** Check if the value replied by the tracker is correct.
  @param key the name of the key
  @param n the value to check
  @param url Url of the tracker
  @param name the name of the file
*)
let chk_keyval key n url name =
  let int_n = (Int64.to_int n) in
  if !verbose_msg_clients then
    lprintf_nl "Reply from %s in file: %s has %s: %d" (show_tracker_url url) name key int_n;
  if int_n > -1 then
    int_n
  else begin
     lprintf_nl "Reply from %s in file: %s has an invalid %s value: %d" (show_tracker_url url) name key int_n;
     0
   end

let exn_catch f x = try `Ok (f x) with exn -> `Exn exn

(** In this function we interact with the tracker
  @param file The file for which we want some sources
  @param need_sources whether we need any sources
*)
let talk_to_tracker file need_sources =
  (* This is the function which will be called by the http client for parsing the response *)
  let f t filename =
    let tracker_url = show_tracker_url t.tracker_url in
    let tracker_failed reason =
      (* On failure, disable the tracker and count attempts (@see is_tracker_enabled) *)
      let num = match t.tracker_status with | Disabled_failure (i,_) -> i + 1 | _ -> 1 in
      t.tracker_status <- Disabled_failure (num, intern reason);
      lprintf_file_nl (as_file file) "Failure no. %d%s from Tracker %s for file: %s Reason: %s"
        num
        (if !!tracker_retries = 0 then "" else Printf.sprintf "/%d" !!tracker_retries)
        tracker_url file.file_name (Charset.Locale.to_utf8 reason)
    in
    match exn_catch File.to_string filename with
    | `Exn _ | `Ok "" -> tracker_failed "empty reply"
    | `Ok s ->
    match exn_catch Bencode.decode s with
    | `Exn exn -> tracker_failed (Printf.sprintf "wrong reply (%s)" (Printexc2.to_string exn))
    | `Ok (Dictionary list) ->
        t.tracker_interval <- 600;
        t.tracker_min_interval <- 600;
        if need_sources then t.tracker_last_clients_num <- 0;
        let chk_keyval key n = chk_keyval key n t.tracker_url file.file_name in
        if not (List.mem_assoc "failure reason" list) then
        begin
          begin match t.tracker_status with
          | Disabled_failure (i, _) ->
              lprintf_file_nl (as_file file) "Received good message from Tracker %s after %d bad attempts" 
                tracker_url i
          | _ -> () end;
          (* Received good message from tracker after failures, re-enable tracker *)
          t.tracker_status <- Enabled;
        end;
        List.iter (fun (key,value) ->
            match (key,value) with
            | "failure reason", String failure -> tracker_failed failure
            | "warning message", String warning ->
                lprintf_file_nl (as_file file) "Warning from Tracker %s in file: %s Reason: %s" 
                  tracker_url file.file_name warning
            | "interval", Int n ->
                t.tracker_interval <- chk_keyval key n;
                (* in case we don't receive "min interval" *)
                if t.tracker_min_interval > t.tracker_interval then
                  t.tracker_min_interval <- t.tracker_interval
            | "min interval", Int n ->
                t.tracker_min_interval <- chk_keyval key n;
                (* make sure "min interval" is always < or equal to "interval" *)
                if t.tracker_min_interval > t.tracker_interval then
                  t.tracker_min_interval <- t.tracker_interval
            | "downloaded", Int n ->
                t.tracker_torrent_downloaded <- chk_keyval key n
            | "complete", Int n
            | "done peers", Int n ->
                t.tracker_torrent_complete <- chk_keyval key n
            | "incomplete", Int n ->
                t.tracker_torrent_incomplete <- chk_keyval key n;
                (* if complete > 0 and we receive incomplete we probably won't receive num_peers so we simulate it below *)
                if t.tracker_torrent_complete > 0 then
                  t.tracker_torrent_total_clients_count <- (t.tracker_torrent_complete + t.tracker_torrent_incomplete);
            | "num peers", Int n ->
                t.tracker_torrent_total_clients_count <- chk_keyval key n;
                (* if complete > 0 and we receive num_peers we probably won't receive incomplete so we simulate it below *)
                if t.tracker_torrent_complete > 0 then
                  t.tracker_torrent_incomplete <- (t.tracker_torrent_total_clients_count - t.tracker_torrent_complete);
            | "last", Int n ->
                t.tracker_torrent_last_dl_req <- chk_keyval key n
            | "key", String n ->
                t.tracker_key <- n;
                if !verbose_msg_clients then
                  lprintf_file_nl (as_file file) "%s in file: %s has key: %s" tracker_url file.file_name n
            | "tracker id", String n ->
                t.tracker_id <- n;
                if !verbose_msg_clients then
                  lprintf_file_nl (as_file file) "%s in file: %s has tracker id %s" tracker_url file.file_name n

            | "peers", List list ->
                if need_sources then
                List.iter (fun v ->
                    match v with
                    | Dictionary list ->
                        let peer_id = ref Sha1.null in
                        let peer_ip = ref Ip.null in
                        let port = ref 0 in

                        List.iter (fun v ->
                            match v with
                              "peer id", String id ->
                                peer_id := Sha1.direct_of_string id;
                            | "ip", String ip ->
                                peer_ip := Ip.of_string ip
                            | "port", Int p ->
                                port := Int64.to_int p
                            | _ -> ()
                        ) list;

                        t.tracker_last_clients_num <- t.tracker_last_clients_num + 1;
                        maybe_new_client file !peer_id !peer_ip !port

                    | _ -> assert false
                ) list
            | "peers", String p ->
                let rec iter_comp s pos l =
                  if pos < l then
                    let ip = Ip.of_ints (get_uint8 s pos,get_uint8 s (pos+1),
                        get_uint8 s (pos+2),get_uint8 s (pos+3))
                    and port = get_int16 s (pos+4)
                    in
                    t.tracker_last_clients_num <- t.tracker_last_clients_num + 1;
                    maybe_new_client file Sha1.null ip port;

                    iter_comp s (pos+6) l
                in
                if need_sources then 
                  iter_comp p 0 (String.length p)
            | "private", Int n -> ()
              (* TODO: if set to 1, disable peer exchange *)
            | "peers6", _ -> ()
              (* TODO IPv6 support required *)
            | key, _ -> lprintf_file_nl (as_file file) "received unknown entry in answer from tracker: %s : %s" key (Bencode.print value)
        ) list;
       (*Now, that we have added new clients to a file, it's time
         to connect to them*)
        if !verbose_sources > 0 then
          lprintf_file_nl (as_file file) "talk_to_tracker: got %i source(s) for file %s"
            t.tracker_last_clients_num file.file_name;
        if need_sources then resume_clients file

    | _ -> tracker_failed "wrong reply (value)" 
  in
  let event =
    if file.file_tracker_connected then ""
    else "started"
  in
  connect_trackers file event need_sources f

let talk_to_dht file need_sources =
  match !bt_dht with
  | None -> ()
  | Some dht ->
    if !verbose then lprintf_file_nl (as_file file) "DHT announce";
    file.file_last_dht_announce <- last_time ();
    BT_DHT.query_peers dht file.file_id (fun (_,addr as node) token peers ->
      BT_DHT.M.announce dht addr !!client_port token file.file_id (fun _ -> ()) ~kerr:(fun () -> 
        if !verbose then lprintf_file_nl (as_file file) "DHT announce to %s failed" (BT_DHT.show_node node));
      if need_sources then
      begin
        List.iter (fun (ip,port) -> maybe_new_client file Sha1.null ip port) peers;
        resume_clients file
      end)

let talk_to_tracker file need_sources =
  if file.file_last_dht_announce + 14*60 < last_time () && not file.file_private then talk_to_dht file need_sources;
  talk_to_tracker file need_sources

(** Check to see if file is finished, if not
  try to get sources for it
*)
let recover_files () =
  if !verbose_share then
    lprintf_nl "recover_files";
  List.iter (fun file ->
      match file.file_swarmer with
        None -> ()
      | Some swarmer ->
          (try check_finished swarmer file with e -> ());
          match file_state file with
            FileDownloading ->
              if !verbose_share then
                lprintf_file_nl (as_file file) "recover downloading";
              (try talk_to_tracker file true with _ -> ())
          | FileShared ->
              if !verbose_share then
                lprintf_file_nl (as_file file) "recover shared";
              (try talk_to_tracker file false with _ -> ())
          | FilePaused -> () (*when we are paused we do nothing, not even logging this vvvv*)
          | FileQueued -> ()
          | s -> if !verbose then lprintf_file_nl (as_file file) "recover: Other state %s!!" (string_of_state s)
      ) !current_files

let upload_buffer = String.create 100000


(**
  Send a Piece message
  for one of the request of client
  @param sock The socket of the client
  @param c The client
*)
let rec iter_upload sock c =
  match c.client_upload_requests with
    [] -> ()
  | (num, pos, len) :: tail ->
      if len = zero then begin
          c.client_upload_requests <- tail;
          iter_upload sock c
        end else
      if c.client_allowed_to_write >= 0L then begin
        try
          c.client_upload_requests <- tail;

          let file = c.client_file in
          let offset = pos ++ file.file_piece_size *.. num in
          c.client_allowed_to_write <- c.client_allowed_to_write -- len;
          count_upload c len;
          let len = Int64.to_int len in
(*          lprintf "Unix32.read: offset %Ld len %d\n" offset len; *)
          Unix32.read (file_fd file) offset upload_buffer 0 len;
          (* update upload rate from len bytes *)
          Rate.update c.client_upload_rate  ~amount:len;
          Rate.update c.client_downloaded_rate;
          file.file_uploaded <- file.file_uploaded ++ (Int64.of_int len);
          file.file_session_uploaded <- file.file_session_uploaded ++ (Int64.of_int len);
          let _ =
            (* update stats *)
            count_filerequest c;
            match file.file_shared with
                None -> ()
              | Some s ->
                  begin
                    s.impl_shared_uploaded <- file.file_uploaded;
                    shared_must_update (as_shared s)
                  end
          in
(*          lprintf "sending piece\n"; *)
          send_client c (Piece (num, pos, Bytes.unsafe_to_string upload_buffer, 0, len));
          iter_upload sock c
        with exn -> 
          if !verbose then 
            lprintf_nl ~exn "iter_upload"
        end else
        begin
(*          lprintf "client is waiting for another piece\n"; *)
          ready_for_upload (as_client c)
        end


(**
  In this function we check if we can send bytes (according
  to bandwidth control), if we can, call iter_upload to
  send a Piece message
  @param c the client to which we can send some bytes
  @param allowed the amount of bytes we can send to client
*)
let client_can_upload c allowed =
(*  lprintf "allowed to upload %d\n" allowed;  *)
  do_if_connected  c.client_sock (fun sock ->
      match c.client_upload_requests with
        [] -> ()
      | _ :: tail ->
    let new_allowed_to_write =
      c.client_allowed_to_write ++ (Int64.of_int allowed) in
      if allowed > 0 && can_write_len sock
            (Int64.to_int new_allowed_to_write)
      then begin
        CommonUploads.consume_bandwidth allowed;
        c.client_allowed_to_write <- new_allowed_to_write;
      end;
          iter_upload sock c
  )

let file_resume file =
  List.iter (fun t ->
    match t.tracker_status with
    | Enabled | Disabled_mld _ -> ()
    | Disabled_failure _ | Disabled _ -> t.tracker_status <- Enabled
  ) file.file_trackers;
  (try talk_to_tracker file true with _ -> ())



(**
  Send info to tracker when stopping a file.
  @param file the file we want to stop
*)
let file_stop file =
    if file.file_tracker_connected then
    begin
      connect_trackers file "stopped" false (fun _ _ ->
          lprintf_file_nl (as_file file) "Tracker return: stopped %s" file.file_name;
          file.file_tracker_connected <- false)
    end

(*
  Create the 'hooks'
*)
let _ =
  client_ops.op_client_can_upload <- client_can_upload;
  file_ops.op_file_resume <- file_resume;
  file_ops.op_file_recover <- file_resume;
  file_ops.op_file_pause <- (fun file ->
      Hashtbl.iter (fun _ c ->
          match c.client_sock with
            Connection sock -> close sock Closed_by_user
          | _ -> ()
      ) file.file_clients;
       (*When a file is paused we consider it is stopped*)
      file_stop file
  );
  file_ops.op_file_queue <- file_ops.op_file_pause;
  client_ops.op_client_enter_upload_queue <- (fun c ->
      if !verbose_msg_clients then
        lprintf_nl "Client %d: client_enter_upload_queue" (client_num c);
      ready_for_upload (as_client c));
  network.op_network_connected_servers <- (fun _ -> []);
