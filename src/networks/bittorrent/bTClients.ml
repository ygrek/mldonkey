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
open Ip_set

open CommonShared
open CommonUploads
open CommonOptions
open CommonDownloads
open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open CommonSwarming
open CommonGlobals
open CommonDownloads  
  
open BTRate
open BTTypes
open BTProtocol
open BTOptions
open BTGlobals
open BTComplexOptions
open BTChooser
open TcpMessages
  
let http_ok = "HTTP 200 OK"
let http11_ok = "HTTP/1.1 200 OK"
  

let next_uploaders = ref ([] : BTTypes.client list)
let current_uploaders = ref ([] : BTTypes.client list)




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
   The function will get a file as an argument (@see 
   get_sources_from_tracker for an example)
*)
let connect_trackers file event f =
  
  let args,must_check_delay, downloaded, left = 
    
    match file.file_swarmer with
      None -> 
        begin 
          match event with
          | "started" -> [("event", "started")],true,zero,zero
          | "stopped" -> [("event", "stopped")],false,zero,zero
          | _ -> [],true, zero, zero
        end
    
    | Some swarmer ->
        let local_downloaded = Int64Swarmer.downloaded swarmer in
        let left = file_size file -- local_downloaded in
        match event with
          | "completed" -> [("event", "completed")],false,zero,zero
        | "started" -> [("event", "started")],true,local_downloaded, left
        | "stopped" -> [("event", "stopped")],false,local_downloaded, left
        | _ -> [],true,local_downloaded, left
  in
  
  let args = 
    ("info_hash", Sha1.direct_to_string file.file_id) ::
    ("peer_id", Sha1.direct_to_string !!client_uid) ::
    ("port", string_of_int !!client_port) ::
    ("uploaded", Int64.to_string file.file_uploaded) ::
    ("downloaded", Int64.to_string downloaded) ::
    ("left", Int64.to_string ((file_size file) -- downloaded)) ::
    ("compact","1") ::
    args
  in  
  let args = if !!numwant > -1 then
      ("numwant", string_of_int !!numwant) :: args else args in
  
  List.iter (fun t ->  
      if 
        (file.file_clients_num < !!ask_tracker_threshold )
        ||
        t.tracker_last_conn + 14400 < last_time()
        ||
        t.tracker_last_conn + t.tracker_interval < last_time()
      then
        begin
          if !verbose_msg_servers then 
            lprintf "get_sources_from_tracker: tracker_connected:%s last_clients:%i last_conn-last_time:%i file: %s\n"
              (string_of_bool file.file_tracker_connected) t.tracker_last_clients_num
              (t.tracker_last_conn - last_time()) file.file_name;
          t.tracker_last_conn <- (last_time() - (t.tracker_interval + 30));


        let module H = Http_client in
        let url = t.tracker_url in
        let r = {
            H.basic_request with
            H.req_url = Url.of_string ~args: args url;
            H.req_proxy = !CommonOptions.http_proxy;
            H.req_user_agent = 
            Printf.sprintf "MLdonkey/%s" Autoconf.current_version;
          } in
        
        lprintf "Request sent to tracker\n";
        H.wget r 
          (fun fileres -> 
            t.tracker_last_conn <- last_time ();
            file.file_tracker_connected <- true;
            f t fileres
        )
        end else
        lprintf "Request NOT sent to tracker - remaning: %d\n" 
        (t.tracker_interval - (last_time () - t.tracker_last_conn))
  ) file.file_trackers        

(** In this function we decide which peers will be 
  uploaders. We send a choke message to current uploaders
  that are not in the next uploaders list. We send Unchoke
  for clients that are in next list (and not in current) 
*)
let recompute_uploaders () =
  if !verbose_upload then lprintf "recompute_uploaders\n";
  next_uploaders := choose_uploaders current_files;
  (*Send choke if a current_uploader is not in next_uploaders*)      
  List.iter ( fun c -> if ((List.mem c !next_uploaders)==false) then
        begin
          set_client_has_a_slot (as_client c) false;
          (*we will let him finish his download and choke him on next_request*)
        end
  ) !current_uploaders;
  
  (*don't send Choke if new uploader is already an uploaders *)            
  List.iter ( fun c -> if ((List.mem c !current_uploaders)==false) then
        begin
          set_client_has_a_slot (as_client c) true;
          Rate.update_no_change c.client_downloaded_rate;
          Rate.update_no_change c.client_upload_rate;
          c.client_last_optimist<- last_time();
          client_enter_upload_queue (as_client c);
          send_client c Unchoke;
        end
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
    lprintf "CLIENT %d: disconnected: %s\n" (client_num c) (string_of_reason reason);
  begin
    match c.client_sock with
      NoConnection -> ()
    | ConnectionWaiting token ->
        cancel_token token;
        c.client_sock <- NoConnection
    | Connection sock  -> 
        close sock reason;
        try
(*          List.iter (fun r -> Int64Swarmer.free_range r) c.client_ranges; *)
          set_client_disconnected c reason;
          let file = c.client_file in
(* this is not useful already done in the match
          (try close sock reason with _ -> ());	  *)
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
                  Int64Swarmer.unregister_uploader up
(*	      c.client_registered_bitfield <- false;
          for i = 0 to String.length c.client_bitmap - 1 do
            c.client_bitmap.[0] <- '0';
			      done*)
            end;
(* Don't test if a client have an upload slot because
	  it don't have one (removed during earlier in
	    set_client_disconnected c reason) *)
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
        lprintf "disconnect since download is finished\n";
      disconnect_client c Closed_by_user
    end
  ) file.file_clients
          


 
(** What to do when a file is finished
  @param file the finished file
*)         
let download_finished file = 
  if List.memq file !current_files then begin      
      connect_trackers file "completed" (fun _ _ -> ()); (*must be called before swarmer gets removed from file*)
(*CommonComplexOptions.file_completed*)    
      file_completed (as_file file);
      
(* Remove the swarmer for this file as it is not useful anymore... *)
      file.file_swarmer <- None;
      
(* At this point, the file state is FileDownloaded. We should not remove
  the file. *)
(*
remove_file file;
disconnect_clients file;
*)
      
(* Why do we send a "stopped" event to the tracker ??? *)
      connect_trackers file "stopped" (fun _ _ -> ());
(* start sharing! - Copy file from torrents/download to torrent/seeded 
Hum, this shouldn't be useful anymore as we copy downloads and share files
  in torrents/.
  
      try
        match file.file_torrent_diskname with
          None -> ()
        | Some torrent_diskname ->
            let s = File.to_string torrent_diskname in  
            if String.length s = 0 then raise Not_found;
            let filename = Filename.concat seeded_directory 
                ((String2.replace file.file_name '/' "") ^ ".torrent") in
            File.from_string filename s
      with _ ->
        lprintf "Unable to start sharing %s\n" file.file_name
    end
*)
    end

      
(** Check if a file is finished or not.
  A file is finished if all blocks are verified.
  @param file The file to check status
*)
let check_finished swarmer file = 
  if Int64Swarmer.check_finished swarmer then
      download_finished file
      
    


    
let bits = [| 128; 64; 32;16;8;4;2;1 |]
(* Official client seems to use max_range_request 5 and max_range_len 2^14 *)
(* How much requests in the 'pipeline' *)
let max_range_requests = 5
(* How much bytes we can request in one Piece *)

    


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
  let bitmap =
    match c.client_file.file_swarmer with
      None -> 
(* This must be a seeded file... *)
        String.make (Array.length c.client_file.file_chunks) '3'
    | Some swarmer ->
        Int64Swarmer.verified_bitmap swarmer 
  in
  
  if !verbose then begin 
      lprintf "SENDING Verified bitmap: [%s]\n" bitmap;
      end; 
  send_client c (BitField 
      (        
      let nchunks = String.length bitmap in
      let len = (nchunks+7)/8 in
      let s = String.make len '\000' in
      for i = 0 to nchunks - 1 do
        let n = i lsr 3 in
        let j = i land 7 in
(* In the future, only accept bitmap.[n] > '2' when verification works *)
        if bitmap.[i] >= '2' then begin
            s.[n] <- char_of_int (int_of_char s.[n]
                lor bits.(j))
          end
      done;
      s
    ))
  

  



  
let counter = ref 0



(** This function is called to parse the first message that 
  a client send.
  @param counter Don't know what it is
  @param cc Expected client (probably useless now that we don't save any client)
  @param init_sent A boolean to know if we sent this client the handshake message
  @param gconn Don't know
  @param sock The socket we use for this client
  @param proto Not used???
  @param file_id The file hash (sha1) of the file involved in this exchange
  @param peer_id The hash (sha1) of the client. (Should be checked)
*)
let rec client_parse_header counter cc init_sent gconn sock 
    (proto, file_id, peer_id) = 
  try
    set_lifetime sock 600.;
    if !verbose_msg_clients then
      lprintf "client_parse_header %d\n" counter;
    
    let file = Hashtbl.find files_by_uid file_id in
    if !verbose_msg_clients then
      lprintf "file found\n";
    let c = 
      match !cc with 
        None ->
          let c = new_client file peer_id (TcpBufferedSocket.peer_addr sock) in
          lprintf "CLIENT %d: incoming CONNECTION\n" (client_num c);
          cc := Some c;
          c
      | Some c -> 
         (* client could have had Sha1.null as peer_id/uid *)
         if c.client_uid <> peer_id then 
          c.client_software <- (parse_software (Sha1.direct_to_string peer_id));
          c

(*          if c.client_uid <> peer_id then begin
              lprintf "Unexpected client by UID\n";
              let ccc = new_client file peer_id (TcpBufferedSocket.host sock) in
              lprintf "CLIENT %d: testing instead of %d\n"
                (client_num ccc) (client_num c);
              (match ccc.client_sock with 
                  Connection _ -> 
                    lprintf "This client is already connected\n";
                    close sock (Closed_for_error "Already connected"); 
                    remove_client ccc;
                    c
                | _ -> 
                    lprintf "CLIENT %d: recovered by UID\n" (client_num ccc);
                    remove_client c;
                    cc := Some ccc;
                    ccc)
            end else
            c       *)   
    in
    
    if !verbose_msg_clients then begin
        let (ip,port) = c.client_host in
        lprintf "CLIENT %d: Connected (%s:%d)\n"  (client_num c)
        (Ip.to_string ip) port
        ;
      end;
    
    (match c.client_sock with
        NoConnection ->
          if !verbose_msg_clients then
            lprintf "Client was not connected !!!\n";
          c.client_sock <- Connection sock
      | ConnectionWaiting token ->
          cancel_token token;
          if !verbose_msg_clients then
            lprintf "Client was not connected !!!\n";
          c.client_sock <- Connection sock
      | Connection s when s != sock -> 
          if !verbose_msg_clients then 
            lprintf "CLIENT %d: IMMEDIATE RECONNECTION\n" (client_num c);
          disconnect_client c (Closed_for_error "Reconnected");
          c.client_sock <- Connection sock;
      | Connection _  -> ()
    );
    
    set_client_state (c) (Connected (-1));
    if not init_sent then 
      begin
        c.client_incoming<-true;
        send_init !!client_uid file_id sock;
      end;
    connection_ok c.client_connection_control;
    if !verbose_msg_clients then
      lprintf "file and client found\n";
(*    if not c.client_incoming then *)
    send_bitfield c;
    c.client_blocks_sent <- file.file_blocks_downloaded;
(*
      TODO !!! : send interested if and only if we are interested 
      -> we must recieve at least other peer bitfield.
      in common swarmer -> compare : partition -> partition -> bool
    *)

(*    send_client c Unchoke;  *)
    
    set_rtimeout sock !!client_timeout;
(*Once parse succesfully we define the function
    client_to_client to be the function used when a message
    is read*)
    gconn.gconn_handler <- Reader (fun gconn sock ->
        bt_handler TcpMessages.parser (client_to_client c) c.client_software sock
    );
    
    ()
  with e ->
    begin
      match e with
        | Not_found ->
          lprintf "BT: requested file not shared [%s]\n" (Sha1.direct_to_string file_id)
        | _ ->
      lprintf "Exception %s in client_parse_header\n" (Printexc2.to_string e);
    end;
      close sock (Closed_for_exception e);
      raise e





(** Update the bitmap of a client. Unclear if it is still useful.
  @param c The client which we want to update.
*)
and update_client_bitmap c =
  let file = c.client_file in
  let swarmer = match file.file_swarmer with
      None -> assert false
    | Some swarmer -> swarmer in
  let up = 
    match c.client_uploader with
      None ->
        let up = Int64Swarmer.register_uploader swarmer (as_client c)
          (AvailableRanges []) in
        c.client_uploader <- Some up;
        up
    | Some up ->
        up
  in
  
  let bitmap = match c.client_bitmap with
      None -> 
        let len = Int64Swarmer.partition_size swarmer in
        let bitmap = String.make (len*8) '0' in
        c.client_bitmap <- Some bitmap;
        bitmap
    | Some bitmap -> bitmap
  in
  
  if c.client_new_chunks <> [] then
    let chunks = c.client_new_chunks in
    c.client_new_chunks <- [];
    let file = c.client_file in
    List.iter (fun n -> bitmap.[n] <- '1') chunks;

(* As we are lazy, we don't send this event...
        CommonEvent.add_event (File_update_availability
            (as_file file.file_file, as_client c, 
            String.copy bitmap));
*)
    
    Int64Swarmer.update_uploader up (AvailableCharBitmap bitmap)

(** In this function we decide which piece we must request from client.
  @param sock Socket of the client
  @param c The client
*)
and get_from_client sock (c: client) =
  let file = c.client_file in
(*Check if there's not enough requests in the 'pipeline'
    and if a request can be send (not choked and file is downloading) *)
  if List.length c.client_ranges_sent < max_range_requests && 
    file_state file = FileDownloading && (c.client_choked == false)  then 
(*num is the number of the piece, x and y are the position
of the subpiece in the piece(!), r is a (CommonSwarmer) range *)
    
    
    let up = match c.client_uploader with
        None -> assert false
      | Some up -> up in 
    let swarmer = Int64Swarmer.uploader_swarmer up in       
    
    let num, x,y, r = 
      if !verbose_msg_clients then begin
          lprintf "CLIENT %d: Finding new range to send\n" (client_num c);
        end;
      
      if !verbose_swarming then begin
          lprintf "Current download:\n  Current chunks: "; 
          List.iter (fun (x,y) -> lprintf "%Ld-%Ld " x y) c.client_chunks;
          lprintf "\n  Current ranges: ";
          List.iter (fun (p1,p2, r) ->
              let (x,y) = Int64Swarmer.range_range r 
              in
              lprintf "%Ld-%Ld[%Ld-%Ld] " p1 p2 x y) c.client_ranges_sent;
          (match c.client_range_waiting with
              None -> ()
            | Some (x,y,r) -> lprintf "Waiting %Ld-%Ld\n" x y);
          lprintf "\n  Current block: ";
          (match c.client_block with
              None -> lprintf "none\n"
            | Some b -> Int64Swarmer.print_block b);
          lprintf "\n\nFinding Range: \n";
        end;
      try
(*We must find a block to request first, and then 
	some range inside this block*)
        let rec iter () =
          match c.client_block with
            None -> 
              if !verbose_swarming then
                lprintf "No block\n";
              update_client_bitmap c;
              (try Int64Swarmer.verify_one_chunk swarmer with _ -> ());
(*Find a free block in the swarmer*)
              let b = Int64Swarmer.find_block up in
              if !verbose_swarming then begin 
                  lprintf "Block Found: "; Int64Swarmer.print_block b;
                end; 
              c.client_block <- Some b;
(*We put the found block in client_block to 
		request range in this block. (Useful for 
		not searching each time a new block)
	      *)	      
              iter ()
          | Some b ->
              if !verbose_swarming then begin
                  lprintf "Current Block: "; Int64Swarmer.print_block b;
                end;
              try
(*Given a block find a range inside*)
                let (x,y,r) = 
                  match c.client_range_waiting with
                    Some (x,y,r) -> 
                      c.client_range_waiting <- None;
                      (x,y,r)
                  | None -> Int64Swarmer.find_range up in
                
                let (x,y,r) =
                  if y -- x > max_range_len then begin
                      c.client_range_waiting <- Some (x ++ max_range_len, y, r);
                      (x, x ++ max_range_len, r)
                    end else (x,y,r) in
                c.client_ranges_sent <- c.client_ranges_sent @ [x,y, r];
(*                Int64Swarmer.alloc_range r; *)
                let num = Int64Swarmer.block_num swarmer b in
                if !verbose_swarming then
                  lprintf "Asking %d For Range %Ld-%Ld\n" num x y;
                
                num, x -- file.file_piece_size ** Int64.of_int num, y -- x, r
              with Not_found ->
(*If we don't find a range to request inside the block,
		iter to choose another block*)
                  if !verbose_swarming then 
                    lprintf "Could not find range in current block\n";
(*                  c.client_blocks <- List2.removeq b c.client_blocks; *)
                  c.client_block <- None;
                  iter ()
        in
        iter ()
      with Not_found -> 
(*If we don't find a block to request we can check if the
	  file is finished (if there's missing pieces we can't decide
	  that the file is finished because we didn't found 
	  a block to ask)*)
          
          if !verbose_swarming then
            lprintf "Unable to get a block !!\n";
          Int64Swarmer.compute_bitmap swarmer;
          check_finished swarmer file;
          raise Not_found
    in
    send_client c (Request (num,x,y));
    if !verbose_msg_clients then
      lprintf "CLIENT %d: Asking %s For Range %Ld-%Ld\n"
        (client_num c)
      (Sha1.to_string c.client_uid) 
      x y



(** In this function we match a message sent by a client
  and react according to this message.
  @param c The client which sent us a message
  @param sock The socket used for this client
  @param msg The message sent by the client
*)
and client_to_client c sock msg = 
  if !verbose_msg_clients then begin
      let (timeout, next) = get_rtimeout sock in
      lprintf "CLIENT %d: (%d, %d,%d) Received %s" 
        (client_num c)
      (last_time ())
      (int_of_float timeout)
      (int_of_float next)
      (TcpMessages.to_string msg);
    end;
  
  let file = c.client_file in
(*  if c.client_blocks_sent != file.file_blocks_downloaded then begin
      let rec iter list =
        match list with
          [] -> ()
        | b :: tail when tail == c.client_blocks_sent ->
            c.client_blocks_sent <- list;
            let (num,_,_) = Int64Swarmer.block_block b  in
            send_client c (Have (Int64.of_int num))
        | _ :: tail -> iter tail
      in
      iter file.file_blocks_downloaded
    end;*)
  
  try
    match msg with
      Piece (num, offset, s, pos, len) ->
(*A Piece message contains the data*)
        set_client_state c (Connected_downloading (file_num file));
(*?*)
        c.client_good <- true;
        if file_state file = FileDownloading then begin
            let position = offset ++ file.file_piece_size *.. num in
            
            let up = match c.client_uploader with
                None -> assert false
              | Some up -> up in 
            let swarmer = Int64Swarmer.uploader_swarmer up in       
            
            if !verbose_msg_clients then 
              (match c.client_ranges_sent with
                  [] -> lprintf "EMPTY Ranges !!!\n"
                | (p1,p2,r) :: _ -> 
                    let (x,y) = Int64Swarmer.range_range r in
                    lprintf "Current range %Ld [%d] (asked %Ld-%Ld[%Ld-%Ld])\n"
                      position len
                      p1 p2 x y 
              );
            
            let old_downloaded = 
              Int64Swarmer.downloaded swarmer in
(*            List.iter Int64Swarmer.free_range c.client_ranges;       *)
            Int64Swarmer.received up
              position s pos len;
(*            List.iter Int64Swarmer.alloc_range c.client_ranges; *)
            let new_downloaded = 
              Int64Swarmer.downloaded swarmer in

(*Update rate and ammount of data received from client*)
            c.client_downloaded <- c.client_downloaded ++ 
              (new_downloaded -- old_downloaded);
            Rate.update c.client_downloaded_rate  (float_of_int len);
            
            if !verbose_msg_clients then 
              (match c.client_ranges_sent with
                  [] -> lprintf "EMPTY Ranges !!!\n"
                | (p1,p2,r) :: _ -> 
                    let (x,y) = Int64Swarmer.range_range r in
                    lprintf "Received %Ld [%d] %Ld-%Ld[%Ld-%Ld] -> %Ld\n"
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
(*              Int64Swarmer.free_range r; *)
              c.client_ranges_sent <- tail;
        end;
        get_from_client sock c;
        if (List.length !current_uploaders < (!!max_uploaders_per_torrent-1)) &&
          (List.mem c (!current_uploaders)) == false && c.client_interested then
          begin
(*we are probably an optimistic uploaders for this client
             don't miss the oportunity if we can*)
            current_uploaders := c::(!current_uploaders);
            c.client_sent_choke <- false;
            set_client_has_a_slot (as_client c) true;
            client_enter_upload_queue (as_client c);
            send_client c Unchoke;
          end;

(* Check if the client is still interesting for us... *)
        check_if_interesting file c
    
    | PeerID p ->
      c.client_software <- (parse_software p);
      c.client_uid <- Sha1.direct_of_string p

    | BitField p ->
(*A bitfield is a summary of what a client have*)
        
        begin        
          match c.client_file.file_swarmer with
            None -> ()
          | Some swarmer -> 
              c.client_new_chunks <- [];
              let len = String.length p in
              let bitmap = String.make (len*8) '0' in
              for i = 0 to len - 1 do
                for j = 0 to 7 do
                  if (int_of_char p.[i]) land bits.(j) <> 0 then
                    begin
                      c.client_new_chunks <- i*8+j :: c.client_new_chunks;
                      bitmap.[i*8+j] <- '1';
                    end
                  else 
                    bitmap.[i*8+j] <- '0';	    
                done;
              done;

(*Update availability for GUI*)
              CommonEvent.add_event (File_update_availability
                  (as_file file, as_client c,  String.copy bitmap));
              update_client_bitmap c;
              
              let verified = Int64Swarmer.verified_bitmap swarmer in        
              let npieces = Int64Swarmer.partition_size swarmer in
              for i = 0 to npieces -1  do
                if bitmap.[i] = '1' && verified.[i] < '2' then
                  c.client_interesting <- true;
              done;
              
              if !verbose_msg_clients then 
                lprintf "BitField translated\n";
              if !verbose_msg_clients then 
                lprintf "Old BitField Unregistered\n";
              (match c.client_uploader with
                  None -> assert false
                | Some up ->
                    Int64Swarmer.update_uploader up 
                      (AvailableCharBitmap bitmap));
              c.client_registered_bitfield <- true;	  
              c.client_bitmap <- Some bitmap;
              send_interested c;
              if !verbose_msg_clients then 
                lprintf "New BitField Registered\n";
(*        for i = 1 to max_range_requests - List.length c.client_ranges do
          (try get_from_client sock c with _ -> ())
        done*)
        end;
        (*a bitfield must only be sent after the handshake and befor everything else: NOT here
        if c.client_incoming then send_bitfield c;*)
    
    | Have n ->
(*A client can send a Have without sending a Bitfield*)
        begin
          match c.client_file.file_swarmer with
            None -> ()
          | Some swarmer -> 
              let n = Int64.to_int n in
              let verified = Int64Swarmer.verified_bitmap swarmer in
              if verified.[n] < '2' then begin
                  c.client_interesting <- true;
                  send_interested c;  
                  c.client_new_chunks <- n :: c.client_new_chunks;
                  update_client_bitmap c;
                end;


(*        begin
          match c.client_bitmap, c.client_uploader with
            Some bitmap, Some up ->
              let swarmer = Int64Swarmer.uploader_swarmer up in
              let n = Int64.to_int n in
              if bitmap.[n] <> '1' then
                
                let verified = Int64Swarmer.verified_bitmap swarmer in
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
          | None, Some _ -> lprintf "no bitmap but client_uploader\n";
	  | Some _ , None ->lprintf "bitmap but no client_uploader\n";
	  | None, None -> lprintf "no bitmap no client_uploader\n";
        end
*)      
        end
        
    | Interested ->
        c.client_interested <- true;
    
    | Choke ->
        begin
          set_client_state (c) (Connected (-1));
(*remote peer will clear the list of range we sent*)
          begin
          match c.client_uploader with
              None ->
                lprintf "BT: Choke send, but no client bitmap\n"
            | Some up -> 
          Int64Swarmer.clear_uploader_ranges up
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
(*remote peer cleared our request : re-request*)
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
        
        if !CommonUploads.has_upload = 0 then
          begin
            if client_has_a_slot (as_client c) then
              begin
(*                lprintf "Received request for upload\n"; *)
                (match c.client_upload_requests with
                  [] ->
                      CommonUploads.ready_for_upload (as_client c);
                  | _ -> ());
                c.client_upload_requests <- 
                  c.client_upload_requests @ [n,pos,len];
              end
            else
              begin
                send_client c Choke;
                c.client_sent_choke <- true;
                c.client_upload_requests <- [];                 
              end
          end;
        
    
    | Ping -> ()
	(*We don't 'generate' a Ping message.*)
    
    | Cancel _ -> ()
  with e ->
      lprintf "Error %s while handling MESSAGE\n" (Printexc2.to_string e)
      


(** The function used to connect to a client.
The connection is not immediately initiated. It will
be put in a fifo and dequeud according to
!!max_connections_per_second. (@see commonGlobals.ml)
@param c The client we must connect
*)      
let connect_client c =
  if can_open_connection connection_manager then
  match c.client_sock with
    NoConnection ->
    
      let token =
        add_pending_connection connection_manager (fun token ->
            try
              if !verbose_msg_clients then begin
                  lprintf "CLIENT %d: connect_client\n" (client_num c);
                end;
              let (ip,port) = c.client_host in
              if !verbose_msg_clients then begin
                  lprintf "connecting %s:%d\n" (Ip.to_string ip) port; 
                end;
              connection_try c.client_connection_control;
                begin
                  let sock = connect token "bittorrent download" 
                      (Ip.to_inet_addr ip) port
                      (fun sock event ->
                        match event with
                          BASIC_EVENT LTIMEOUT ->
                            if !verbose_msg_clients then
                              lprintf "CLIENT %d: LIFETIME\n" (client_num c);
                            close sock Closed_for_timeout
                        | BASIC_EVENT RTIMEOUT ->
                            if !verbose_msg_clients then
                              lprintf "CLIENT %d: RTIMEOUT (%d)\n" (client_num c)
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
                  
                  if !verbose_msg_clients then begin
                      lprintf "READY TO DOWNLOAD FILE\n";
                    end;
                  
                  send_init !!client_uid file.file_id sock;
(* Fabrice: Initialize the client bitmap and uploader fields to <> None *)
                  update_client_bitmap c;
(*              (try get_from_client sock c with _ -> ());*)
                  incr counter;
(*We 'hook' the client_parse_header function to the socket
	      This function will then be called when the first message will
	      be parsed*)
                  set_bt_sock sock !verbose_msg_clients
                    (BTHeader (client_parse_header !counter (ref (Some c)) true))
                end
            with e ->
                lprintf "Exception %s while connecting to client\n" 
                  (Printexc2.to_string e);
                disconnect_client c (Closed_for_exception e)
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
		to connect to us*)
	      let ip = (Ip.of_inet_addr from_ip) in 
              if !verbose_sources > 1 then lprintf "CONNECTION RECEIVED FROM %s\n"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
(*Reject this connection if we don't want
		to bypass the max_connection parameter*)
              if can_open_connection connection_manager && 
		(match Ip_set.match_ip !Ip_set.bl ip with
		     None -> true
		   | Some br ->
		       if !verbose_connect then
			 lprintf "%s:%d blocked: %s\n"
			   (Ip.to_string ip) from_port br.blocking_description;
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
			sockets*)
                            close sock Closed_for_timeout
                        | _ -> ()
                    )
                  in
                  TcpBufferedSocket.set_read_controler sock download_control;
                  TcpBufferedSocket.set_write_controler sock upload_control;
                  
                  let c = ref None in
                  TcpBufferedSocket.set_closer sock (fun _ r ->
                      match !c with
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
		open a new connection*)
                Unix.close s
          | _ -> ()
      ) in
    listen_sock := Some s;
    ()
  with e ->
      lprintf "Exception %s while init bittorrent server\n" 
        (Printexc2.to_string e)
      
  
  




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
            (*i think this one is not realy usefull for debugging
			lprintf "RESUME: Client is already connected\n"; *)
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
      with e -> ()
(* lprintf "Exception %s in resume_clients\n"   (Printexc2.to_string e) *)
  ) file.file_clients
  
    
    


(** In this function we initiate a connection to the file tracker
  to get sources.
  @param file The file for which we want some sources
  @param url Url of the tracker
  If we have less than !!ask_tracker_threshold sources 
  and if we respect the file_tracker_interval then
  we really ask sources to the tracker
*)
let get_sources_from_tracker file = 
  let f t filename = 
(*This is the function which will be called by the http client
for parsing the response*)
    let v = Bencode.decode (File.to_string filename) in
    
    t.tracker_interval <- 600;
    t.tracker_last_clients_num <- 0;
    match v with
      Dictionary list ->
        List.iter (fun (key,value) ->
            match (key, value) with
              String "interval", Int n -> 
                t.tracker_interval <- Int64.to_int n
            | String "peers", List list ->
                List.iter (fun v ->
                    match v with
                      Dictionary list ->
                        t.tracker_last_clients_num <- t.tracker_last_clients_num + 1;
                        
                        let peer_id = ref Sha1.null in
                        let peer_ip = ref Ip.null in
                        let port = ref 0 in
                        
                        List.iter (fun v ->
                            match v with
                              String "peer id", String id -> 
                                peer_id := Sha1.direct_of_string id;
                            | String "ip", String ip ->
                                peer_ip := Ip.of_string ip
                            | String "port", Int p ->
                                port := Int64.to_int p
                            | _ -> ()
                        ) list;
                        
                        if !peer_id != Sha1.null &&
                          !peer_id <> !!client_uid &&
                          !peer_ip != Ip.null &&
                          !port <> 0 && 
                          (match match_ip !Ip_set.bl !peer_ip with
                              None -> true
                            | Some br ->
                                if !verbose_connect then
                                  lprintf "%s:%d blocked: %s\n"
                                    (Ip.to_string !peer_ip) !port br.blocking_description;
                                false)
                        then
                          let c = new_client file !peer_id (!peer_ip,!port)
                          in
                          lprintf "Received %s:%d\n" (Ip.to_string !peer_ip)
                          !port;
                          ()
                    
                    
                    | _ -> assert false
                
                ) list
            | String "peers", String p ->
                let rec iter_comp s pos l =
                  if pos < l then
                    let ip = Ip.of_ints (get_uint8 s pos,get_uint8 s (pos+1),
                        get_uint8 s (pos+2),get_uint8 s (pos+3))
                    and port = get_int16 s (pos+4) 
                    in 
                    ignore( new_client file Sha1.null (ip,port));
                    t.tracker_last_clients_num <- t.tracker_last_clients_num + 1;
                    
                    iter_comp s (pos+6) l
                in
                iter_comp p 0 (String.length p)
            | _ -> ()
        ) list;
(*Now, that we have added new clients to a file, it's time
	to connect to them*)
        if !verbose_sources > 0 then 
          lprintf "get_sources_from_tracker: got %i sources for file %s\n" 
          t.tracker_last_clients_num file.file_name;
        resume_clients file
    
    | _ -> assert false    
  in
  let event = 
    if file.file_tracker_connected then ""
    else "started"
  in
  connect_trackers file event f
    
    
  

(** Check to see if file is finished, if not 
  try to get sources for it
*)  
let recover_files () =
  List.iter (fun file ->
      match file.file_swarmer with
        None -> ()
      | Some swarmer ->
          (try check_finished swarmer file with e -> ());
          match file_state file with
            FileDownloading ->
              (try get_sources_from_tracker file with _ -> ())
          | FileShared ->
              (try 
                  connect_trackers file "" (fun _ _ -> ()) with _ -> ())
          | FilePaused -> () (*when we are paused we do nothing, not even logging this vvvv*)
          | s -> lprintf "Other state %s!!\n" (string_of_state s)
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
      if c.client_allowed_to_write >= len then begin
          c.client_upload_requests <- tail;
          
          let file = c.client_file in
          let offset = pos ++ file.file_piece_size *.. num in
          c.client_allowed_to_write <- c.client_allowed_to_write -- len;
          c.client_uploaded <- c.client_uploaded ++ len;
          let len = Int64.to_int len in
(*          CommonUploads.consume_bandwidth (len/2); *)
(*          lprintf "Unix32.read: offset %Ld len %d\n" offset len; *)
          Unix32.read (file_fd file) offset upload_buffer 0 len;
(*update uploade rate from len bytes*)
          Rate.update c.client_upload_rate  (float_of_int len);
          file.file_uploaded <- file.file_uploaded ++ (Int64.of_int len);
(*          lprintf "sending piece\n"; *)
          send_client c (Piece (num, pos, upload_buffer, 0, len));
          iter_upload sock c
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
          CommonUploads.consume_bandwidth allowed;
          c.client_allowed_to_write <- 
            c.client_allowed_to_write ++ (Int64.of_int allowed);
          iter_upload sock c
  )



(** Probably useless now
*)
let file_resume file = 
(* useless with no saving of sources
  resume_clients file;
*)
  (try get_sources_from_tracker file  with _ -> ())





(**
  Send info to tracker when stopping a file.
  @param file the file we want to stop
*)
let file_stop file =
    if file.file_tracker_connected then 
    begin
      connect_trackers file "stopped" (fun _ _ -> 
          lprintf "BT-Tracker return: stopped %s\n" file.file_name;
          file.file_tracker_connected <- false)
    end
      
(**
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
  client_ops.op_client_enter_upload_queue <- (fun c ->
      if !verbose_msg_clients then
        lprintf "CLIENT %d: client_enter_upload_queue\n" (client_num c);
      ready_for_upload (as_client c));
  network.op_network_connected_servers <- (fun _ -> []);

  
