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


open AnyEndian
open CommonShared
open CommonUploads
open Printf2
open CommonOptions
open CommonDownloads
open Md4
open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonSwarming  
open BTTypes
open BTOptions
open BTGlobals
open BTComplexOptions

open BTProtocol
  
let http_ok = "HTTP 200 OK"
let http11_ok = "HTTP/1.1 200 OK"
  
let disconnect_client c reason =
  if !verbose_msg_clients then
    lprintf "CLIENT %d: disconnected\n" (client_num c);
  match c.client_sock with
    NoConnection | ConnectionWaiting | ConnectionAborted -> ()
  | Connection sock -> 
      close sock reason;
      try
        List.iter (fun r -> Int64Swarmer.free_range r) c.client_ranges;
        c.client_ranges <- [];
        c.client_block <- None;
        if not c.client_good then
          connection_failed c.client_connection_control;
        c.client_good <- false;
        set_client_disconnected c reason;
        (try close sock reason with _ -> ());
        c.client_sock <- NoConnection;
        let file = c.client_file in
        c.client_chunks <- [];
        c.client_allowed_to_write <- zero;
        c.client_new_chunks <- [];
        Int64Swarmer.unregister_uploader_bitmap 
          file.file_partition c.client_bitmap;
        for i = 0 to String.length c.client_bitmap - 1 do
          c.client_bitmap.[0] <- '0';
        done
      with _ -> ()

let disconnect_clients file = 
  Hashtbl.iter (fun _ c ->
      if !verbose_msg_clients then
        lprintf "disconnect since download is finished\n";
      disconnect_client c Closed_by_user
  ) file.file_clients
          
let download_finished file = 
  if List.memq file !current_files then begin      
      file_completed (as_file file.file_file);
      BTGlobals.remove_file file;
      old_files =:= (file.file_name, file_size file) :: !!old_files;
      disconnect_clients file
    end
    
let (++) = Int64.add
let (--) = Int64.sub
      

let check_finished file = 
  if file_state file <> FileDownloaded then begin
      let bitmap = Int64Swarmer.verified_bitmap file.file_partition in
      for i = 0 to String.length bitmap - 1 do
        if bitmap.[i] <> '3' then raise Not_found;
      done;  
      if (file_size file <> Int64Swarmer.downloaded file.file_swarmer)
      then
        lprintf "Downloaded size differs after complete verification\n";
      download_finished file
    end
    
let bits = [| 128; 64; 32;16;8;4;2;1 |]


let max_range_requests = 10
let max_range_len = 1 lsl 15

    
  
let counter = ref 0
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
          let c = new_client file peer_id (TcpBufferedSocket.host sock) in
          lprintf "CLIENT %d: incoming CONNECTION\n" (client_num c);
          cc := Some c;
          c
      | Some c ->
          if c.client_uid <> peer_id then begin
              lprintf "Unexpected client by UID\n";
              let ccc = new_client file peer_id (TcpBufferedSocket.host sock) in
              lprintf "CLIENT %d: testing instead of %d\n"
                (client_num ccc) (client_num c);
              (match ccc.client_sock with 
                  Connection _ -> 
                    lprintf "This client is already connected\n";
                    close sock (Closed_for_error "Already connected"); c
                | _ -> 
                    lprintf "CLIENT %d: recovered by UID\n" (client_num ccc);
                    cc := Some ccc;
                    ccc)
            end else
            c          
    in
    
    if !verbose_msg_clients then begin
        let (ip,port) = c.client_host in
        lprintf "CLIENT %d: Connected (%s:%d)\n"  (client_num c)
        (Ip.to_string ip) port
        ;
      end;
    
    (match c.client_sock with
        ConnectionWaiting | NoConnection | ConnectionAborted ->
          if !verbose_msg_clients then
            lprintf "Client was not connected !!!\n";
          c.client_sock <- Connection sock
      | Connection s when s != sock -> 
          if !verbose_msg_clients then 
            lprintf "CLIENT %d: IMMEDIATE RECONNECTION\n" (client_num c);
          disconnect_client c (Closed_for_error "Reconnected");
          c.client_sock <- Connection sock;
      | Connection _ -> ()
    );
    
    set_client_state (c) (Connected (-1));
    if not init_sent then send_init file c sock;
    connection_ok c.client_connection_control;
    if !verbose_msg_clients then
      lprintf "file and client found\n";
    let bitmap = Int64Swarmer.verified_bitmap file.file_partition in
    if bitmap <> "" then
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
        )); 
    c.client_blocks_sent <- file.file_blocks_downloaded;
    send_client c Interested;
(*    send_client c Unchoke;  *)
    
    set_rtimeout sock 300.;
    gconn.gconn_handler <- Reader (fun gconn sock ->
        bt_handler bt_parser (client_to_client c) sock
    );
    
    ()
  with e ->
      lprintf "Exception %s in client_parse_header\n" (Printexc2.to_string e);
      close sock (Closed_for_exception e);
      raise e

and update_client_bitmap c =
  if c.client_new_chunks <> [] then
    let chunks = c.client_new_chunks in
    c.client_new_chunks <- [];
    let file = c.client_file in
    Int64Swarmer.unregister_uploader_bitmap 
      file.file_partition c.client_bitmap;
    List.iter (fun n ->
        c.client_bitmap.[n] <- '1') chunks;
    let bs = 
      Int64Swarmer.register_uploader_bitmap file.file_partition 
        c.client_bitmap in
    c.client_blocks <- bs

and get_from_client sock (c: client) =
  let file = c.client_file in
  if List.length c.client_ranges < max_range_requests && 
    file_state file = FileDownloading then 
    let num, x,y, r = 
      if !verbose_msg_clients then begin
          lprintf "CLIENT %d: Finding new range to send\n" (client_num c);
        end;
      
      if !verbose_swarming then begin
          lprintf "Current download:\n  Current chunks: "; 
          List.iter (fun (x,y) -> lprintf "%Ld-%Ld " x y) c.client_chunks;
          lprintf "\n  Current ranges: ";
          List.iter (fun r ->
              let (x,y) = Int64Swarmer.range_range r 
              in
              lprintf "%Ld-%Ld " x y) c.client_ranges;
          lprintf "\n  Current block: ";
          (match c.client_block with
              None -> lprintf "none\n"
            | Some b -> Int64Swarmer.print_block b);
          lprintf "\n\nFinding Range: \n";
        end;
      try
        let rec iter () =
          match c.client_block with
            None -> 
              if !verbose_swarming then
                lprintf "No block\n";
              update_client_bitmap c;
              let b = Int64Swarmer.get_block c.client_blocks in
              if !verbose_swarming then begin 
                  lprintf "Block Found: "; Int64Swarmer.print_block b;
                end; 
              c.client_block <- Some b;
              iter ()
          | Some b ->
              if !verbose_swarming then begin
                  lprintf "Current Block: "; Int64Swarmer.print_block b;
                end;
              try
                let r = Int64Swarmer.find_range_bitmap b 
                    c.client_ranges 
                    (Int64.of_int max_range_len) in
                c.client_ranges <- c.client_ranges @ [r];
                Int64Swarmer.alloc_range r;
                let x,y = Int64Swarmer.range_range r in
                let num, b_begin, b_end = Int64Swarmer.block_block b in
                if !verbose_swarming then
                  lprintf "Asking %d For Range %Ld-%Ld\n" num x y;
                
                num, x -- b_begin, y -- x, r
              with Not_found ->
                  if !verbose_swarming then 
                    lprintf "Could not find range in current block\n";
                  c.client_blocks <- List2.removeq b c.client_blocks;
                  c.client_block <- None;
                  
                  
                  iter ()
        in
        iter ()
      with Not_found -> 
          if !verbose_swarming then
            lprintf "Unable to get a block !!\n";
          Int64Swarmer.compute_bitmap file.file_partition;
          check_finished file
          ;
          raise Not_found
    in
    send_client c (Request (num,x,y));
    if !verbose_msg_clients then
      lprintf "CLIENT %d: Asking %s For Range %Ld-%Ld\n"
        (client_num c)
      (Sha1.to_string c.client_uid) 
      x y

and client_to_client c sock msg = 
  if !verbose_msg_clients then begin
      let (timeout, next) = get_rtimeout sock in
      lprintf "CLIENT %d: (%d, %d,%d) Received " 
        (client_num c)
      (last_time ())
      (int_of_float timeout)
      (int_of_float next);
      bt_print msg;
    end;
  
  let file = c.client_file in
  if c.client_blocks_sent != file.file_blocks_downloaded then begin
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
    end;
  
  try
    match msg with
      Piece (num, offset, s, pos, len) ->
        let file = c.client_file in
        
        set_lifetime sock 600.;
        set_client_state c Connected_downloading;
        
        c.client_good <- true;
        if file_state file = FileDownloading then begin
            let position = offset ++ file.file_piece_size ** num in
            
            if !verbose_msg_clients then 
              (match c.client_ranges with
                  [] -> lprintf "EMPTY Ranges !!!\n"
                | r :: _ -> 
                    let (x,y) = Int64Swarmer.range_range r in
                    lprintf "Current range %Ld [%d] (%Ld-%Ld)\n"
                      position len
                      x y 
              );
            
            let old_downloaded = 
              Int64Swarmer.downloaded file.file_swarmer in
            List.iter Int64Swarmer.free_range c.client_ranges;      
            Int64Swarmer.received file.file_swarmer
              position s pos len;
            List.iter Int64Swarmer.alloc_range c.client_ranges;
            let new_downloaded = 
              Int64Swarmer.downloaded file.file_swarmer in
            
            c.client_downloaded <- c.client_downloaded ++ (Int64.of_int len);
            
            if !verbose_msg_clients then 
              (match c.client_ranges with
                  [] -> lprintf "EMPTY Ranges !!!\n"
                | r :: _ -> 
                    let (x,y) = Int64Swarmer.range_range r in
                    lprintf "Received %Ld [%d] (%Ld-%Ld) -> %Ld\n"
                      position len
                      x y 
                      (new_downloaded -- old_downloaded)
              );
            
            
            if new_downloaded <> old_downloaded then
              add_file_downloaded file.file_file 
                (new_downloaded -- old_downloaded);
          end;
        begin
          match c.client_ranges with
            [] -> ()
          | r :: tail ->
              Int64Swarmer.free_range r;
              c.client_ranges <- tail;
        end;
        get_from_client sock c
    
    | BitField p ->
        c.client_new_chunks <- [];
        let file = c.client_file in
        let npieces = Int64Swarmer.partition_size file.file_partition in
        let len = String.length p in
        let bitmap = String.make (len*8) '0' in
        for i = 0 to len - 1 do
          for j = 0 to 7 do
            if (int_of_char p.[i]) land bits.(j) <> 0 then
              bitmap.[i*8+j] <- '1'
            else 
              bitmap.[i*8+j] <- '0'
          done;
        done;
        if !verbose_msg_clients then 
          lprintf "BitField translated\n";
        Int64Swarmer.unregister_uploader_bitmap 
          file.file_partition c.client_bitmap;
        if !verbose_msg_clients then 
          lprintf "Old BitField Unregistered\n";
        let bs = 
          Int64Swarmer.register_uploader_bitmap file.file_partition bitmap in
        c.client_blocks <- bs;
        c.client_bitmap <- bitmap;
        if !verbose_msg_clients then 
          lprintf "New BitField Registered\n";
        for i = 1 to max_range_requests - List.length c.client_ranges do
          (try get_from_client sock c with _ -> ())
        done

    | Have n ->
        let n = Int64.to_int n in
        if c.client_bitmap.[n] <> '1' then
          let verified = Int64Swarmer.verified_bitmap file.file_partition in
          if verified.[n] <> '3' then begin
              c.client_new_chunks <- n :: c.client_new_chunks;
              if c.client_block = None then begin
                  update_client_bitmap c;
                  for i = 1 to max_range_requests - 
                    List.length c.client_ranges do
                    (try get_from_client sock c with _ -> ())
                  done
                end
            end
            
    | Interested ->
        c.client_interested <- true;
        send_client c Unchoke
    
    | Choke ->
        set_client_state c (Connected 0)
    | NotInterested -> ()

    | Unchoke ->
        List.iter (fun r ->
            let (x, y) = Int64Swarmer.range_range r in
            let num = Int64.to_int (x // file.file_piece_size) in
            let b_begin = file.file_piece_size **  num in
            send_client c (Request (num, x -- b_begin, y -- x))
        ) c.client_ranges
        
    | Request (n, pos, len) ->
        begin
          match c.client_upload_requests with
            [] ->
              if client_has_a_slot (as_client c) then
                CommonUploads.ready_for_upload (as_client c)
              else
              if c.client_downloaded > Int64.zero then
                CommonUploads.give_a_slot (as_client c)
              else
                CommonUploads.add_pending_slot (as_client c)
          | _ -> ()        
        end;
(*        lprintf "client is waiting for piece\n"; *)
        c.client_upload_requests <- 
          c.client_upload_requests @ [n,pos,len];
        
    | Ping -> ()
    
    | Cancel _ -> ()
  with e ->
      lprintf "Error %s while handling MESSAGE\n" (Printexc2.to_string e)
      
let connect_client c =
  if (match c.client_sock with
      | Connection sock -> 
          if closed sock then
            (
              lprintf "Sock is already closed\n";
              disconnect_client c Closed_by_user; true)
          else false
      | ConnectionWaiting -> false
      | ConnectionAborted ->
          c.client_sock <- ConnectionWaiting;
          false
      | NoConnection -> true
    ) then begin
      
      add_pending_connection (fun _ ->
          if c.client_sock = ConnectionAborted then
            c.client_sock <- NoConnection
          else
          if c.client_sock = ConnectionWaiting then
            try
              if !verbose_msg_clients then begin
                  lprintf "CLIENT %d: connect_client\n" (client_num c);
                end;
              let (ip,port) = c.client_host in
              if !verbose_msg_clients then begin
                  lprintf "connecting %s:%d\n" (Ip.to_string ip) port; 
                end;
              connection_try c.client_connection_control;
              let sock = connect "bittorrent download" 
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
              
              send_init file c sock;
              (try get_from_client sock c with _ -> ());
              incr counter;
              set_bt_sock sock !verbose_msg_clients
                (BTHeader (client_parse_header !counter (ref (Some c)) true))
            
            with e ->
                lprintf "Exception %s while connecting to client\n" 
                  (Printexc2.to_string e);
                disconnect_client c (Closed_for_exception e)
      );
      c.client_sock <- ConnectionWaiting;
    end
    
let listen () =
  try
    let s = TcpServerSocket.create "bittorrent client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              lprintf "CONNECTION RECEIVED FROM %s\n"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              
              
              let sock = TcpBufferedSocket.create
                  "bittorrent client connection" s 
                  (fun sock event -> 
                    match event with
                      BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
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
              set_bt_sock sock !verbose_msg_clients
                (BTHeader (client_parse_header !counter c false));
          | _ -> ()
      ) in
    listen_sock := Some s;
    ()
  with e ->
      lprintf "Exception %s while init bittorrent server\n" 
        (Printexc2.to_string e)

let get_file_from_source c file =
  if connection_can_try c.client_connection_control then begin
      connect_client c
    end else begin
      print_control c.client_connection_control
    end
  
  
let send_pings () =
  List.iter (fun file ->
      Hashtbl.iter (fun _ c ->
          match c.client_sock with
          | Connection sock -> 
              send_client c Ping
          | _ -> ()
      ) file.file_clients
  ) !current_files

open Bencode

let resume_clients file = 
  Hashtbl.iter (fun _ c ->
      try
        match c.client_sock with 
        | Connection sock -> 
            lprintf "RESUME: Client is already conencted\n";
            get_from_client sock c
        | _ ->
            (try get_file_from_source c file with _ -> ())
      with e -> ()
(* lprintf "Exception %s in resume_clients\n"   (Printexc2.to_string e) *)
  ) file.file_clients
  
let connect_tracker file url = 
  let f filename = 
    file.file_tracker_connected <- true;
    
    let v = Bencode.decode (File.to_string filename) in
    file.file_tracker_connected <- true;
    file.file_tracker_last_conn <- last_time ();
    let interval = ref 600 in
    match v with
      Dictionary list ->
        List.iter (fun (key,value) ->
            match (key, value) with
              String "interval", Int n -> 
                file.file_tracker_interval <- Int64.to_int n
            | String "peers", List list ->
                List.iter (fun v ->
                    match v with
                      Dictionary list ->
                        let peer_id = ref Sha1.null in
                        let peer_ip = ref Ip.null in
                        let port = ref 0 in
                        
                        List.iter (fun v ->
                            match v with
                              String "peer id", String id -> 
                                peer_id := Sha1.direct_of_string id
                            | String "ip", String ip ->
                                peer_ip := Ip.of_string ip
                            | String "port", Int p ->
                                port := Int64.to_int p
                            | _ -> ()
                        ) list;
                        
                        if !peer_id != Sha1.null &&
                          !peer_ip != Ip.null && !port <> 0 then
                          let c = new_client file !peer_id (!peer_ip,!port)
                          in 
                          ()
                    
                    
                    | _ -> assert false
                
                ) list
            | _ -> ()
        ) list;
        resume_clients file
    
    | _ -> assert false    
  
  in       
  let args = [
      ("info_hash", Sha1.direct_to_string file.file_id);
      ("peer_id", Sha1.direct_to_string !!client_uid) ;
(*      ("ip", Ip.to_string (client_ip None)) ; *)
      ("port", string_of_int !!client_port) ; 
      ("uploaded", "0" ) ;
      ("downloaded", "0" ) ;
      ("left", Int64.to_string ((file_size file) -- 
            (Int64Swarmer.downloaded file.file_swarmer)) ) ; 
      ("event", 
(*          "completed" *)
        if file.file_tracker_connected then "" else "started" ) ;
    ]
  in
  
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = Url.of_string ~args: args url;
      H.req_user_agent = 
      Printf.sprintf "MLdonkey %s" Autoconf.current_version;
    } in
  H.wget r f
  
let recover_files () =
  List.iter (fun file ->
      (try check_finished file with e -> ());
      if file_state file = FileDownloading then begin
          resume_clients file;
          if file.file_tracker_last_conn + file.file_tracker_interval 
              < last_time () then
            (try connect_tracker file file.file_tracker  with _ -> ())
        end
  ) !current_files

let upload_buffer = String.create 100000
  
let rec iter_upload sock c = 
  match c.client_upload_requests with
    [] -> ()
  | (num, pos, len) :: tail ->
      if c.client_allowed_to_write >= len then begin
          c.client_upload_requests <- tail;
          
          let file = c.client_file in
          let offset = pos ++ file.file_piece_size ** num in
          c.client_allowed_to_write <- c.client_allowed_to_write -- len;
          c.client_uploaded <- c.client_uploaded ++ len;
          let len = Int64.to_int len in
(*          CommonUploads.consume_bandwidth (len/2); *)
          Unix32.read (file_fd file) offset upload_buffer 0 len;
          
(*          lprintf "sending piece\n"; *)
          send_client c (Piece (num, pos, upload_buffer, 0, len));
          iter_upload sock c
        end else
        begin
(*          lprintf "client is waiting for another piece\n"; *)
          ready_for_upload (as_client c)
        end
              
let client_can_upload c allowed = 
(*  lprintf "allowed to upload %d\n" allowed; *)
  match c.client_sock with
    NoConnection | ConnectionWaiting | ConnectionAborted -> ()
  | Connection sock ->
      match c.client_upload_requests with
        [] -> ()
      | _ :: tail ->
          CommonUploads.consume_bandwidth allowed;
          c.client_allowed_to_write <- 
            c.client_allowed_to_write ++ (Int64.of_int allowed);
          iter_upload sock c

let file_resume file = 
  resume_clients file;
  (try connect_tracker file file.file_tracker  with _ -> ())

let _ =
  client_ops.op_client_can_upload <- client_can_upload;
  file_ops.op_file_resume <- file_resume;
  file_ops.op_file_recover <- file_resume;
  file_ops.op_file_pause <- (fun file -> 
      Hashtbl.iter (fun _ c ->
          match c.client_sock with
            Connection sock -> close sock Closed_by_user
          | _ -> ()
      ) file.file_clients
  );
  client_ops.op_client_enter_upload_queue <- (fun c ->
      if !verbose_msg_clients then
        lprintf "CLIENT %d: client_enter_upload_queue\n" (client_num c);
      ready_for_upload (as_client c));
  network.op_network_connected_servers <- (fun _ -> []);

  