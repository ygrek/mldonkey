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
(* The function handling the cooperation between two clients. Most used 
functions are defined in downloadOneFile.ml *)

open Printf2
open Md4
open Options
open BasicSocket
open TcpBufferedSocket

open GuiProto
open CommonRoom
open CommonShared
open CommonGlobals
open CommonFile
open CommonClient
open CommonComplexOptions
open CommonSwarming
open CommonResult
open CommonTypes
open CommonOptions 
open CommonDownloads.SharedDownload
open CommonUploads
  
open DonkeyMftp
open DonkeyProtoCom
open DonkeyOneFile
open DonkeyOptions
open DonkeyComplexOptions
open DonkeyGlobals
open DonkeyStats
open DonkeyTypes  
open DonkeyReliability


module Udp = DonkeyProtoUdp

(* Lifetime of a socket after sending interesting messages *)
let active_lifetime = 1200.
  
let is_banned c sock = 
  c.client_banned <- Hashtbl.mem banned_ips (peer_ip sock)
    

(* Supports Emule Extended Protocol *)
let supports_eep cb = 
  match cb with
    Brand_lmule | Brand_newemule | Brand_cdonkey | Brand_mldonkey3 | Brand_shareaza -> true
  | _ -> false

let ban_client c sock msg = 
  let module M = DonkeyProtoClient in
  
  if !verbose then begin
      lprintf "client %s(%s) %s, it has been banned\n" msg
        c.client_name (brand_to_string c.client_brand);
    end;
  
  let ip = peer_ip sock in
  count_banned c;
  c.client_banned <- true;
  Hashtbl.add banned_ips ip (last_time ());
                
  if !!send_warning_messages then
    direct_client_send c ( M.SayReq  (
        Printf.sprintf 
        "[AUTOMATED ERROR] Your client %s, it has been banned" msg))
  
let corruption_warning c =
  if !!send_warning_messages then
    let module M = DonkeyProtoClient in
    direct_client_send c (
      M.SayReq "[AUTOMATED WARNING] It has been detected that your client is sending corrupted data. Please double-check your hardware (disk, memory, cpu) and software (latest version ?)")

let request_for c file sock =
  if !!ban_queue_jumpers then
    try
      let record = Hashtbl.find old_requests (client_num c, file_num file) in
      if record.last_request + 540 > last_time () then begin
          record.nwarnings <- record.nwarnings+ 1;
          record.last_request <- last_time ();
          if record.nwarnings > 3 then raise Exit;
          let module M = DonkeyProtoClient in
          if record.nwarnings =3 then begin
              ban_client c sock "is connecting too fast";
              raise Exit;
            end;
          if !verbose then begin
              lprintf "uploader %s(%s) has been warned\n" 
                c.client_name (brand_to_string c.client_brand);
            end;
          if !!send_warning_messages then
            direct_client_send c ( M.SayReq  (
                "[AUTOMATED WARNING] Your client is connecting too fast, it will get banned"))
        end else
        record.last_request <- last_time ();
    with Not_found ->
        Hashtbl.add old_requests (client_num c, file_num file) 
        { last_request = last_time (); nwarnings = 0; }
        
let clean_requests () = (* to be called every hour *)
  Hashtbl.clear old_requests;
  let remove_ips = ref [] in
  Hashtbl.iter (fun ip time ->
      if time + 3600 * !!ban_period < last_time () then 
        remove_ips := ip :: !remove_ips
  ) banned_ips;
  List.iter (fun ip ->
      Hashtbl.remove banned_ips ip;
  ) !remove_ips

let _ =
  
  let client_enter_upload_queue c =
    match c.client_sock with
      None -> ()
    | Some sock ->

        lprintf "client_enter_upload_queue\n";
        set_rtimeout sock !!upload_timeout;
        direct_client_send c (
          let module M = DonkeyProtoClient in
          let module Q = M.AvailableSlot in
          M.AvailableSlotReq Q.t);
        
        if !verbose then begin
            lprintf "New uploader %s: brand %s\n" 
              c.client_name (brand_to_string c.client_brand);
          end;
        
        set_write_power sock (c.client_power);
        set_read_power sock (c.client_power)
  
  in
  client_ops.op_client_enter_upload_queue <- client_enter_upload_queue
   
let client_log_info c sock = 
  let buf = Buffer.create 100 in
  let date = BasicSocket.date_of_int (last_time ()) in
  Printf.bprintf buf "%-12s(%d):%d -> %-30s[%-14s %-20s] connected for %5d secs %-10s bw %5d/%-5d %-6s %2d/%-2d reqs " 
    (Date.simple date) 
  (nb_sockets ())
  (client_num c)
  (
    let s = c.client_name in
    let len = String.length s in 
    if len > 30 then String.sub s 0 30 else s)
  
  (brand_to_string c.client_brand)
  (match c.client_kind with Indirect_location _ -> "LowID"
    | Known_location (ip,port) -> Printf.sprintf "%s:%d"
          (Ip.to_string ip) port)
  (last_time () - c.client_connect_time)
  (if c.client_rank > 0 then
      Printf.sprintf "rank %d" c.client_rank
      else "")
  (nwritten sock) (nread sock)
  (if c.client_banned then "banned" else "")
  c.client_requests_received
    c.client_requests_sent
  ;
  
  List.iter (fun r ->
      Buffer.add_char buf (
        match r.request_result with
        | File_chunk ->      'C'
        | File_upload ->     'U'
        | File_not_found ->  '-'
        | File_found ->      '+'
        | File_possible ->   '?'
        | File_expected ->   '!'
        | File_new_source -> 'n'
      )) c.client_files;      
  Buffer.add_char buf '\n';
  Buffer.contents buf 
  
let log_client_info c sock =
  CommonEvent.add_event (Console_message_event (client_log_info c sock))
  
let disconnect_client c reason =
  match c.client_sock with
    None -> ()
  | Some sock ->
      (try
          if c.client_debug || !verbose_connect then begin
              lprintf "*** Client[%d]: disconnected for %s ***\n " (client_num c) (string_of_reason reason);
              lprintf "%s\n" (client_log_info c sock)
            end;
          if c.client_checked then count_seen c;
          if !!log_clients_on_console && c.client_name <> "" then 
            log_client_info c sock
            ;
          (try Hashtbl.remove connected_clients c.client_md4 with _ -> ());
          CommonUploads.remove_pending_slot (as_client c.client_client);
          connection_failed c.client_connection_control;
          TcpBufferedSocket.close sock reason;
          printf_string "-c"; 
          c.client_has_a_slot <- false;
          c.client_sock <- None;
          save_join_queue c;
          c.client_slot <- SlotNotAsked;
          set_client_disconnected c reason;
          DonkeyOneFile.clean_client_zones c;
          List.iter (fun (file, bitmap) ->
              Int64Swarmer.unregister_uploader_bitmap file.file_partition 
              bitmap) 
          c.client_file_queue;          
          c.client_file_queue <- [];  
          if c.client_upload != None then CommonUploads.refill_upload_slots ();
        
        with e -> lprintf "Exception %s in disconnect_client\n"
              (Printexc2.to_string e));
      DonkeySources.source_of_client c
  
let client_send_if_possible c sock msg =
  if can_write_len sock (!!client_buffer_size/2) then
    client_send c msg
  
let tag_udp_client = 203

let client_can_receive c =
  match c.client_brand with
    | Brand_mldonkey2 -> true
    | Brand_mldonkey3 -> true
    | _ -> false
      
let new_udp_client c group =
  match c.client_kind with
    Indirect_location _ -> ()
  | Known_location (ip, port) ->
      let uc = {
          udp_client_last_conn = last_time ();
          udp_client_ip = ip;
          udp_client_port = port;
          udp_client_can_receive = client_can_receive c
        }
      in
      let uc =
        try
          let uc = UdpClientWHashtbl.find udp_clients uc in
          uc.udp_client_last_conn <- last_time ();
          uc
        with _ ->
            Heap.set_tag uc tag_udp_client;
            UdpClientWHashtbl.add udp_clients uc;
            uc
      in          
      group.group <- UdpClientMap.add c.client_kind uc group.group

      
let udp_client_send uc t =
  if not ( is_black_address uc.udp_client_ip (uc.udp_client_port+4)) then
    begin
      DonkeyProtoCom.udp_send (get_udp_sock ())
      uc.udp_client_ip (uc.udp_client_port+4)
      t
    end
            
let client_udp_send ip port t =
  if not ( is_black_address ip (port+4)) then
    begin
      DonkeyProtoCom.udp_send (get_udp_sock ()) 
      ip (port+4)
      t
    end

let find_sources_in_groups c md4 =
  if !!propagate_sources &&
    (match c.client_brand with
        Brand_mldonkey1 | Brand_overnet -> false
      | _ -> true) then
    try
      let group = Hashtbl.find file_groups md4 in
      try
        let uc = UdpClientMap.find c.client_kind group.group in
        uc.udp_client_last_conn <- last_time ()
(* the client is already known *)
      with _ ->
(* a new client for this group *)
          if client_can_receive c then begin
              match c.client_sock with 
                None -> ()
              | Some sock ->
(* send the list of members of the group to the client *)
                  let list = ref [] in
                  UdpClientMap.iter (fun _ uc ->
                      match ip_reliability uc.udp_client_ip with
                        Reliability_reliable | Reliability_neutral ->
                          list := (uc.udp_client_ip, uc.udp_client_port, uc.udp_client_ip) :: !list
                      | Reliability_suspicious _ -> ()
                  ) group.group;
                  if !list <> [] then begin
                      if !verbose_src_prop then begin
                          lprintf "Send %d sources from file groups to mldonkey peer\n" (List.length !list); 
                        end;
                      let msg = 
                        let module Q = DonkeyProtoClient.Sources in
                        DonkeyProtoClient.SourcesReq {
                          Q.md4 = md4;
                          Q.sources = !list;
                        }
                      in
                      client_send_if_possible c sock msg 
                    end
            end;
          
          match c.client_kind with 
            Indirect_location _ -> ()
          | Known_location (ip, port) ->
(* send this client as a source for the file to
		     all mldonkey clients in the group. add client to group *)
              
              UdpClientMap.iter (fun _ uc ->
                  if uc.udp_client_can_receive then begin
                      if !verbose_src_prop then
                        (lprintf "Send new source to file groups UDP peers\n"; 
                          );
                      udp_client_send uc (
                        let module M = DonkeyProtoServer in
                        Udp.QueryLocationReplyUdpReq (
                          let module Q = M.QueryLocationReply in
                          {
                            Q.md4 = md4;
                            Q.locs = [{ Q.ip = ip; Q.port = port }];
                          }))
                    end
              ) group.group;
              new_udp_client c group
    with _ ->
        if Fifo.length DonkeyGlobals.file_groups_fifo >= max_file_groups then 
          Hashtbl.remove file_groups (Fifo.take file_groups_fifo);
        let group = { group = UdpClientMap.empty } in
        Hashtbl.add file_groups md4 group;
        Fifo.put DonkeyGlobals.file_groups_fifo md4;
        new_udp_client c group
        
let clean_groups () =
  let one_day_before = last_time () - Date.day_in_secs in
  Hashtbl.iter (fun file group ->
      let map = group.group in
      group.group <- UdpClientMap.empty;
      UdpClientMap.iter (fun v uc ->
          if uc.udp_client_last_conn > one_day_before then
            group.group <- UdpClientMap.add v uc group.group
      ) map
  ) file_groups
      
let client_wants_file c md4 =
  if md4 <> Md4.null && md4 <> Md4.one && md4 <> Md4.two then begin
      find_sources_in_groups c md4;
    end
        
  
let new_chunk up begin_pos end_pos =
  if begin_pos <> end_pos then
    let len_requested = Int64.to_int (Int64.sub end_pos begin_pos) in
    let len = Int64.to_int (Int64.sub end_pos begin_pos) in
    let pair = (begin_pos, end_pos) in
    (match up.up_chunks with
        [] ->
          up.up_pos <- begin_pos;
          up.up_end_chunk <- end_pos;
          up.up_chunks <- [pair];
      | chunks ->
          if not (List.mem pair chunks) then
            up.up_chunks <- chunks @ [pair])
  
let identify_client_brand c =
  if c.client_brand = Brand_unknown then
    let md4 = Md4.direct_to_string c.client_md4 in
    c.client_brand <- (
      if md4.[5] = Char.chr 14 && md4.[14] = Char.chr 111 then
	Brand_newemule
      else if md4.[5] = 'M' && md4.[14] = 'L' then
        Brand_mldonkey2
    else
      if c.client_overnet then Brand_overnet else Brand_edonkey)

let identify_emule_compatible c tags = 
  List.iter (fun tag -> 
      match tag.tag_name with
        "compatible" -> (match tag.tag_value with
              Uint64 i -> begin 
                  let intof64 = (Int64.to_int i) in
                  match intof64 with 
                    1 -> c.client_brand <- Brand_cdonkey
                  | 2 -> c.client_brand <- Brand_lmule
                  | 4 -> c.client_brand <- Brand_shareaza
                  | _ -> ()
                end
            | _ -> ())
      | _ -> ()
  ) tags
  
let query_files c sock =  
  let nall_queries = ref 0 in
  let nqueries = ref 0 in

(*  lprintf "Client %d:" (client_num c); lprint_newline (); *)
  let files = ref [] in
  (if c.client_files = [] then begin
(*        lprintf "  query all files"; lprint_newline (); *)
        files := !current_files 
      
      end else
      List.iter (fun r ->
          if r.request_result > File_not_found then begin
(*              lprintf "   query file %s" (file_best_name r.request_file); 
              lprint_newline (); *)
              files := r.request_file :: !files
            end;
      ) c.client_files);

(*
  if !files = [] then begin
      lprintf "   No queries to send !"; lprint_newline ();
    end;
*)
  
  List.iter (fun file ->
      incr nall_queries;
      DonkeySourcesMisc.query_file c file)
  !files;
  if !nqueries > 0 then
    c.client_last_filereqs <- last_time ();
  if !verbose then begin
      lprintf "sent %d/%d file queries\n" !nqueries !nall_queries;
    end
    
  
let client_has_chunks c file chunks =
  
  DonkeySourcesMisc.add_file_location file c;
  
  let len = Array.length  chunks in
  let client_map = String.make len '0' in
  let bitmap = Int64Swarmer.verified_bitmap file.file_partition in
  let client_has_new_chunk = ref false in
  for i = 0 to len - 1 do
    if chunks.(i) then begin
        client_map.[i] <- '1';
        if bitmap.[i] < '2' then 
          client_has_new_chunk := true
      end
  done;
  
  if !client_has_new_chunk then 
    try
      DonkeySourcesMisc.set_request_result c file File_chunk;
      let blocks = 
        Int64Swarmer.register_uploader_bitmap file.file_partition client_map
      in
      
      let last_seen =  Int64Swarmer.partition_age file.file_partition in
      if last_seen > file_last_seen file then
        begin
          set_file_last_seen file last_seen;
          file_must_update_downloaded file;
        end;
      
      CommonEvent.add_event (File_update_availability
          (as_file file, as_client c.client_client, client_map));
      
      
      c.client_file_queue <- c.client_file_queue @ [
        file, client_map ];
      start_download c
    with _ -> 
        if !verbose_download then begin
            lprintf "client_has_chunks: EXCEPTION\n"; 
          end


(*
let download_is_finished file = 
  lprintf "[DEBUG] file is finished...1\n";
  if List.memq file !current_files then begin
      lprintf "[DEBUG] file is finished...2\n";
      file_completed (as_file file);
      remove_file file;
    end        
      *)

(* Nice to see some emule devels here... It's always possible to 
crack a protocol, but let's try to make it as boring as possible... *)
    
external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"
let hash x = hash_param 10 100 x

type challenge_vals =
  Zero of int * challenge_vals
| One of challenge_vals
| Two of challenge_vals * string
| Three of challenge_vals array
| Four of string
| Five of float * challenge_vals
| Six of char * challenge_vals
| Seven of challenge_vals * string
  
let solve_challenge md4 =
  let md4 = Md4.direct_to_string md4 in  
  let rec iter n =
    if n < 0 then iter (-n) else
    if n = 0 then Four "I like mldonkey" else
    let x = n land 7 in
    let y = n lsr 3 in
    let v = iter y in
    match x with
    | 0 -> Zero (y, v)
    | 1 -> One v
    | 2 -> Two (v, "mldonkey")
    | 3 -> Three [| v |]
    | 4 -> Four (Marshal.to_string v [])
    | 5 -> Five (3.3, v)
    | 6 -> Six ('x', v)
    | _ -> Seven (v, Marshal.to_string v [])
  in
  let array = Array.init 4 (fun i -> 
        let n = LittleEndian.get_int md4 (4*i) in
        iter n) in
  Md4.string (Marshal.to_string array [])

  (*
let shared_of_file file =
  match file.file_multinet with
    | None	-> None
    | Some sh	-> Some (as_shared sh)
      *)

let init_client_connection c sock =
  let module M = DonkeyProtoClient in
  
  if supports_eep c.client_brand then begin
(*    lprintf "Emule Extended Protocol query"; lprint_newline ();*)
      let module E = M.EmuleClientInfo in
      emule_send sock (M.EmuleClientInfoReq {
          E.version = !!emule_protocol_version; 
          E.protversion = 0x1;
          E.tags = [
(*           int_tag "compression" 0; *)
            int_tag "udp_port" (!!port+4)
          ]
        })
    end;
  
  List.iter (fun m ->
      direct_client_send c (M.SayReq m)
  ) c.client_pending_messages;
  c.client_pending_messages <- [];
  ()  
      
let client_to_client challenge for_files c t sock = 
  try
    let module M = DonkeyProtoClient in
    
    if !verbose_msg_clients || c.client_debug then begin
        lprintf "Message from client[%d] %s(%s)" (client_num c)
        c.client_name (brand_to_string c.client_brand);
        (match c.client_kind with
            Indirect_location _ -> ()
          | Known_location (ip,port) ->
              lprintf " [%s:%d]" (Ip.to_string ip) port;
        );
        lprint_newline ();
        M.print t;
        lprint_newline ();
      end;
    
    match t with
      M.ConnectReplyReq t ->
        printf_string "******* [CCONN OK] ********"; 
        
        c.client_checked <- true;
        c.client_has_a_slot <- false;
        
        DonkeySources.client_connected c;  
        
        let module CR = M.ConnectReply in
        
        if t.CR.md4 = !!client_md4 ||
          t.CR.md4 = overnet_md4 then
          TcpBufferedSocket.close sock (Closed_for_error "Connected to myself");


(* Test if the client is already connected *)
        if Hashtbl.mem connected_clients t.CR.md4 then begin
(*          lprintf "Client is already connected"; lprint_newline (); *)
            close sock (Closed_for_error "Already connected");
            raise Exit
          end;
        
        c.client_tags <- t.CR.tags;
        List.iter (fun tag ->
            match tag with
              { tag_name = "name"; tag_value = String s } -> 
                set_client_name c s t.CR.md4
            | _ -> ()
        ) c.client_tags;
        
        Hashtbl.add connected_clients t.CR.md4 c;
        
        connection_ok c.client_connection_control;
        
        begin
          match t.CR.server_info with
            Some (ip, port) when !!update_server_list -> safe_add_server ip port
          | _ -> ()
        end;
        
        identify_client_brand c;
        
        init_client_connection c sock;
        
        set_client_state c (Connected (-1));      
        
        challenge.challenge_md4 <- Md4.random ();
        direct_client_send c (
          let module M = DonkeyProtoClient in
          M.QueryFileReq challenge.challenge_md4);
        challenge.challenge_solved <- solve_challenge challenge.challenge_md4;
        
        query_files c sock;
        client_must_update c;      
        if client_type c <> NormalClient then begin
            if last_time () > c.client_next_view_files then begin
(*
            lprintf "****************************************";
            lprint_newline ();
            lprintf "       ASK VIEW FILES         ";
lprint_newline ();
  *)
                direct_client_send c (
                  let module M = DonkeyProtoClient in
                  let module C = M.ViewFiles in
                  M.ViewFilesReq C.t);          
              end
          end;
        is_banned c sock
    
    | M.EmuleQueueRankingReq t 
    | M.QueueRankingReq t ->
        c.client_rank <- t;
        set_client_state c (Connected t);
(* REMOVE THIS !!!
      if t > 1000 then 
        ban_client c sock "has an infinite queue"; *)
    
    | M.EmuleClientInfoReq t ->      
(*      lprintf "Emule Extended Protocol asked"; lprint_newline (); *)
        let module CI = M.EmuleClientInfo in
        identify_emule_compatible c t.CI.tags;
        
        if supports_eep c.client_brand then  begin
            let module E = M.EmuleClientInfo in
            emule_send sock (M.EmuleClientInfoReplyReq {
                E.version = !!emule_protocol_version; 
                E.protversion = 0x1;
                E.tags = [
                  int_tag "compression" 0;
                  int_tag "udp_port" (!!port+4);
                  int_tag "source_exchange" 1;
                  int_tag "comments" 1;
(*                int_tag "compatible" 0; *)
                  int_tag "extended_request" 1;
                  int_tag "udp_version" 1;
                ]
              })
          end
    
    
    | M.EmuleRequestSourcesReq t ->
        let module E = M.EmuleRequestSourcesReply in

(*       lprintf "Emule requested sources"; lprint_newline (); *)
        let file = find_file t in
        let sources = ref [] in
        Intmap.iter (fun _ c ->
            match c.client_kind with
              Indirect_location _ -> () (* not yet supported *)
            | Known_location (ip, port) ->
                match c.client_source with
                  None -> ()
                | Some s ->
                    if s.source_age > last_time () - 600 &&
                      (match ip_reliability ip with
                          Reliability_reliable | Reliability_neutral -> true
                        | Reliability_suspicious _ -> false) &&
                      List.exists (fun r ->
                          match r.request_result with
                            File_not_found | File_possible | File_expected ->
                              false
                          | _ -> true
                      ) c.client_files then
                      sources := {
                        E.ip = ip;
                        E.port = port;
                        E.server_ip = Ip.null;
                        E.server_port = 0;
                      } :: !sources
        ) file.file_locations;
        if !sources <> [] then        
          direct_client_send c (
            M.EmuleRequestSourcesReplyReq {
              E.md4 = t;
              E.sources = Array.of_list !sources;
            })  
    
    
    | M.EmuleClientInfoReplyReq t -> 
        
        let module CI = M.EmuleClientInfo in
        
        identify_emule_compatible c t.CI.tags


(*   lprintf "Emule Extended Protocol activated"; lprint_newline (); *)
    
    
    | M.ViewFilesReplyReq t ->
        c.client_next_view_files <- last_time () + 3600 * 6;
(*
      lprintf "****************************************";
      lprint_newline ();
      lprintf "       VIEW FILES REPLY         ";
      lprint_newline ();
      *)
        let module Q = M.ViewFilesReply in
        begin
          try
            let list = ref [] in
            List.iter (fun f ->
                match result_of_file f.f_md4 f.f_tags with
                  None -> ()
                | Some r ->
                    let r = DonkeyIndexer.index_result_no_filter r in
                    client_new_file c r;
                    list := r :: !list
            ) t;
            c.client_all_files <- Some !list;
            client_must_update c
          
          with e ->
              lprintf "Exception in ViewFilesReply %s\n"
                (Printexc2.to_string e); 
        end;
    
    | M.AvailableSlotReq _ ->
        set_lifetime sock active_lifetime;
        set_rtimeout sock !!queued_timeout; 
(* how long should we wait for a block ? *)
        begin
          match c.client_block with
            None -> ()
          | Some b ->
              lprintf "[QUEUED WITH BLOCK]\n"; 
              DonkeyOneFile.clean_client_zones c;
        end;
        begin
          match c.client_file_queue with
            _ :: _ -> 
              restart_download c
          | [] ->
              if c.client_slot = SlotNotAsked then
                try
                  let files, _ = try
                      let v = Hashtbl.find join_queue_by_md4 c.client_md4 in
                      if c.client_debug then
                        lprintf "Recovered file queue by md4\n";
                      v
                    with _ ->
                        let id = client_id c in
                        let v = Hashtbl.find join_queue_by_id id in
                        if c.client_debug then
                          lprintf "Recovered file queue by md4\n";
                        v
                  in
                  let file = List.map (fun (file, chunks) ->
                        file, String.copy chunks) files in
                  List.iter (fun (file, chunks) ->
                      ignore (
                        Int64Swarmer.register_uploader_bitmap file.file_partition 
                          chunks)
                  ) files;
                  c.client_file_queue <- files @ c.client_file_queue;
                  DonkeyOneFile.restart_download c
                
                
                with _ -> ()
        end;
(* now, we can forget we have asked for a slot *)
        c.client_slot <- SlotReceived;
        DonkeyOneFile.find_client_block c
    
    | M.JoinQueueReq _ when not (!!ban_queue_jumpers && c.client_banned) ->
(*
(*
      if !!ban_queue_jumpers && c.client_banned then
        direct_client_send sock (M.EmuleQueueRankingReq 
          (900 + Random.int 100))
      else *)
      begin try
          
          begin
            match c.client_brand with
            | Brand_mldonkey3 -> 
                if Fifo.length upload_clients >= !!max_upload_slots then
                  Fifo.iter (fun c -> 
                      if c.client_sock <> None && 
                        c.client_brand = Brand_mldonkey3 then raise Exit)
                  upload_clients
            | _ ->
                if Fifo.length upload_clients >= !!max_upload_slots then
                  raise Exit;
          end;

(*	   *)
          set_lifetime sock one_day;
          add_pending_slot c
        
with _ -> *)
(*      set_rtimeout sock !!upload_timeout; *)
        CommonUploads.add_pending_slot (as_client c.client_client);
        if !verbose then begin
            lprintf "(uploader %s: brand %s, couldn't get a slot)\n" 
              c.client_name (brand_to_string c.client_brand);
          end;
(*      end *)
    
    | M.CloseSlotReq _ ->
        printf_string "[DOWN]";
        DonkeyOneFile.clean_client_zones c;
        c.client_slot <- SlotNotAsked;
(* OK, the slot is closed, but what should we do now ????? *)
        begin
          match c.client_file_queue with
            [] -> ()
          | _ -> 
              if !verbose_download then begin
                  lprintf "CloseSlotReq\n"; 
                end;
              DonkeyOneFile.start_download c;
              set_rtimeout sock !!queued_timeout;
        end
    
    | M.ReleaseSlotReq _ ->
        c.client_has_a_slot <- false;
        direct_client_send c (
          let module M = DonkeyProtoClient in
          let module Q = M.CloseSlot in
          M.CloseSlotReq Q.t);
        if c.client_file_queue = [] then
          set_rtimeout sock 120.;
        CommonUploads.find_pending_slot ()
    
    | M.QueryFileReplyReq t ->
        let module Q = M.QueryFileReply in
        
        begin
          try
            let file = find_file t.Q.md4 in
            let s = Printf.sprintf "[FOUND FILE(%s)]" (match c.client_kind with
                  Known_location _ -> "OUT" | _ -> "IN") in
            printf_string s;
            c.client_rating <- c.client_rating + 1;
            
            DonkeySourcesMisc.set_request_result c file File_found;
            
            new_filename file.file_multinet t.Q.name;
            
            if file_size file <= block_size then 
              client_has_chunks c file [| true |]
          
          with _ -> ()
        end  
    
    | M.QueryChunksReplyReq t ->
        
        let module Q = M.QueryChunksReply in      
        let file = find_file t.Q.md4 in
        
        DonkeySourcesMisc.set_request_result c file File_found;
        
        if file_state file = FileDownloading then begin
(* ask for the file description *)
            
            if file.file_md4s = [||] && file_size file > block_size then begin
                direct_client_send c (
                  let module M = DonkeyProtoClient in
                  let module C = M.QueryChunkMd4 in
                  M.QueryChunkMd4Req file.file_md4);
              end;                               

(* ask for more sources *)
            if supports_eep c.client_brand &&
              DonkeySources.need_new_sources file then begin

(*              lprintf "Emule query sources"; lprint_newline (); *)
                let module E = M.EmuleRequestSources in
                emule_send sock (M.EmuleRequestSourcesReq file.file_md4)
              end;
            
            let chunks = 
              if file_size file <= block_size then  [| true |]
              else
              if t.Q.chunks = [||] then
                Array.create (file_nchunks file) true
              else
              if Array.length t.Q.chunks <> (file_nchunks file) then begin
                  lprintf "BAD BAD BAD: number of chunks is different %d/%d for %s:%Ld on peer\n" (Array.length t.Q.chunks) (file_nchunks file) (Md4.to_string file.file_md4) (file_size file);
                  lprintf "Peer info: name=[%s] md4=[%s] overnet=[%s] brand=[%s]\n" 
                    c.client_name
                    (Md4.to_string c.client_md4)
                  (string_of_bool c.client_overnet)
                  (brand_to_string c.client_brand)
                  ;
                  Array.create (file_nchunks file) false
(* What should we do ?

1) Try to recover the correct size of the file: we can use 
ViewFilesReq on all clients having the file to test what is
the most widely used size for this file. Maybe create 
different instances of the file for each proposed size ?

*)
                
                end else 
                t.Q.chunks in
            client_has_chunks c file chunks
          end
    
    | M.QueryChunkMd4ReplyReq t ->
        begin
          let module Q = M.QueryChunkMd4Reply in
          let file = find_file t.Q.md4 in
          
          let module Q = M.QueryChunkMd4Reply in
          if !verbose then begin
              lprintf "MD4 FOR CHUNKS RECEIVED\n"; 
            end;
          
          if file.file_md4s <> [||] then begin
              lprintf "[WARNING] Discarding Chunks Md4: already here\n";
            end else
          if (file_nchunks file) = 1 then begin
              lprintf "[ERROR]: one chunk file without md4\n"; 
              file.file_md4s <- [|file.file_md4|]
            end else
          if t.Q.chunks = [||] then begin
              lprintf "[ERROR]: empty multiple chunks message\n";
            end
          else
          if Array.length t.Q.chunks <> (file_nchunks file) then begin
              lprintf "BAD BAD BAD (2): number of chunks is different %d/%d for %s:%Ld on peer\n" (Array.length t.Q.chunks) (file_nchunks file) (Md4.to_string file.file_md4) (file_size file);
(* What should we do ?

1) Try to recover the correct size of the file: we can use 
ViewFilesReq on all clients having the file to test what is
the most widely used size for this file. Maybe create 
different instances of the file for each proposed size ?

Maybe we should allow a degraded mode of download, where each client
is checked for the file.
  
*)
            
            end else begin
(* We should check the correctness of the Md4 array *)
              
              let md4s = Array.to_list (t.Q.chunks) in
              let md4 = DonkeyShare.md4_of_list md4s in
              if md4 <> file.file_md4 then begin
                  lprintf "[ERROR]: Bad list of MD4s, discarding\n"; 
                end else begin
                  file_md4s_to_register := file :: !file_md4s_to_register;
                  file.file_md4s <- Array.of_list md4s
                end
            
            
            end
(*      if file.file_exists then verify_chunks file *)
        end
    
    | M.BlocReq t -> 
        
        set_lifetime sock active_lifetime;
        let module Q = M.Bloc in
        let file = client_file c in
        
        if !!reliable_sources && 
          client_reliability c = Reliability_suspicious 0 then begin
            lprintf "Receiving data from unreliable client, disconnect\n";
            corruption_warning c;
            disconnect_client c (Closed_for_error "Unreliable Source");
            raise Not_found
          end;
        
        DonkeySourcesMisc.set_request_result c file File_upload;
        
        if file.file_md4 <> t.Q.md4 then begin
            lprintf "BLOC FOR BAD FILE %s/%s !!\n"
              (Md4.to_string t.Q.md4) (Md4.to_string file.file_md4); 
            raise Not_found
          end;
        
        c.client_rating <- c.client_rating + 10;
        (match file_state file with
          | FilePaused 
          | FileAborted _ 
          | FileCancelled ->   next_file c; raise Not_found
          | _ -> ());
        
        let begin_pos = t.Q.start_pos in
        let end_pos = t.Q.end_pos in
        
        set_client_state c Connected_downloading;
        let len = Int64.sub end_pos begin_pos in
        if Int64.to_int len <> t.Q.bloc_len then begin
            lprintf "%d: inconsistent packet sizes\n" (client_num c);
            raise Not_found
          end;
        count_download c file len;
        
        let file_swarmer = file.file_multinet.file_swarmer in
        let old_downloaded =  Int64Swarmer.downloaded file_swarmer in
        List.iter (fun (_,_,r) -> Int64Swarmer.free_range r)  c.client_ranges;
        
        Int64Swarmer.received file_swarmer
          begin_pos t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len;
        List.iter (fun (_,_,r) -> Int64Swarmer.alloc_range r) c.client_ranges;
        let new_downloaded = 
          Int64Swarmer.downloaded file_swarmer in
        
        if new_downloaded <> old_downloaded then begin
            add_file_downloaded file.file_multinet.file_file
              (new_downloaded -- old_downloaded);
            match c.client_block with
              None -> ()
            | Some b -> 
                Int64Swarmer.add_block_contributor b c.client_ip;
          end else
          lprintf "BlocReq: last block was already known\n";
(* Update client ranges *)
        begin
(* We must be careful: what happens if a block is viewed as corrupted, we may
have to reask the range, but it will not be seen as emptied ! Maybe we should
use block versions. *)
          
          let empty_range_removed = ref false in
          let rec remove_empty_ranges rs =
            match rs with
              [] -> []
            | ((_,_,r) as head) :: tail ->
                let (begin_pos, end_pos) = Int64Swarmer.range_range r in
                if begin_pos = end_pos then begin
                    empty_range_removed := true;
                    Int64Swarmer.free_range r;
                    remove_empty_ranges tail
                  end else
                  head :: remove_empty_ranges tail
          in
          
          c.client_ranges <- remove_empty_ranges c.client_ranges;
          
          if !empty_range_removed then 
            DonkeyOneFile.find_client_block c;
        
        end;
        
        if new_downloaded = file_size file then
          check_finished file.file_multinet;

(*
      begin
        match c.client_block with
          None -> 
            printf_string "NO BLOCK EXPECTED FROM CLIENT";
            raise Not_found
        | Some bb ->
            let str_begin = Int64.of_int t.Q.bloc_begin in
            
            if bb.block_present || begin_pos < bb.block_begin
                || begin_pos >= bb.block_end || end_pos > bb.block_end
              then 
              let chunk_num = Int64.to_int (Int64.div begin_pos block_size) 
              in
              lprintf "%d: Exceeding block boundaries" (client_num c);
              lprint_newline ();
              
              lprintf "%Ld-%Ld (%Ld-%Ld)" 
                (begin_pos) (end_pos)
              (bb.block_begin) (bb.block_end)
              ;
              lprint_newline ();
              
              List.iter (fun z ->
                  lprintf "zone: %Ld-%Ld"
                    (z.zone_begin) (z.zone_end)
              ) c.client_zones;

(* try to recover the corresponding block ... *)
              
              if bb.block_pos <> chunk_num then begin
                  lprintf "OLD BLOCK %d <> %d" bb.block_pos chunk_num;
                  lprint_newline ();
                end else
                (              
                  match file.file_chunks.(chunk_num) with
                    PresentTemp | PresentVerified -> 
                      lprintf "ALREADY PRESENT"; lprint_newline ();

(* Here, we should probably try to find a new block !! *)
                      DonkeyOneFile.clean_client_zones c;
                      DonkeyOneFile.find_client_block c                    
                  
                  | AbsentTemp | AbsentVerified ->
                      lprintf "ABSENT (not implemented)"; 
                      lprint_newline ();
(* We receive information for a block we have not asked !! *)
                  
                  | PartialTemp b | PartialVerified b ->
                      
                      if b != bb then begin
                          lprintf "BLOCK DISAGREEMENT"; lprint_newline ();
                        end else begin
                          lprintf "PARTIAL"; lprint_newline ();

(* try to find the corresponding zone *)
                          List.iter (fun z ->
                              if z.zone_begin >= begin_pos &&
                                end_pos > z.zone_begin then begin
                                  lprintf "BEGIN ZONE MATCHES"; 
                                  lprint_newline ();
                                end else
                              if z.zone_begin < begin_pos &&
                                begin_pos < z.zone_end &&
                                z.zone_end < end_pos then begin
                                  lprintf "END ZONE MATCHES";
                                  lprint_newline ();
                                end 
                          
                          ) b.block_zones
                        end
                );              
              raise Not_found
            else
            try
              begin
                if c.client_connected then
                  printf_string "#[OUT]"
                else
                  printf_string "#[IN]";
                
                try
                  if !!buffer_writes then 
                    Unix32.buffered_write (file_fd file) begin_pos
                      t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len
                  else
                    Unix32.write (file_fd file) begin_pos
                      t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len

(*
                let final_pos = Unix32.seek64 (file_fd file) 
                  begin_pos Unix.SEEK_SET in
                if final_pos <> begin_pos then begin
                    lprintf "BAD LSEEK %Ld/%Ld"
                      (final_pos)
                    (begin_pos); lprint_newline ();
                    raise Not_found
                  end;
                if c.client_connected then
                  printf_string "#[OUT]"
                else
                  printf_string "#[IN]";

(*            if !verbose then begin
                lprintf "{%d-%d = %Ld-%Ld}" (t.Q.bloc_begin)
                (t.Q.bloc_len) (begin_pos) 
                (end_pos);
                lprint_newline ();
              end; *)
                let fd = try
                    Unix32.force_fd (file_fd file) 
                  with e -> 
                      lprintf "In Unix32.force_fd"; lprint_newline ();
                      raise e
                in
                Unix2.really_write fd t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len;
*)
                with 
                | e ->
                    lprintf "Error %s while writing block. Pausing download\n"
                      (Printexc2.to_string e);
                    file_pause (as_file file.file_file);      
              end;
              (try
                  List.iter (update_zone file begin_pos end_pos) c.client_zones;
                with e ->
                    lprintf "Exception %s while updating zones\n"
                      (Printexc2.to_string e);
                    raise e
              );
              (try
                  find_client_zone c
                with 
                | e ->
                    lprintf "Exception %s while searching for find client zone\n"
                      (Printexc2.to_string e);
                    raise e)
            
            with
              End_of_file ->
                lprintf "END OF FILE WITH CLIENT %s" c.client_name;
                lprint_newline ();
            | e ->
                lprintf "Exception %s while searching for new chunk\n"
                  (Printexc2.to_string e)      
      end;      
*)

(* Upload requests *)
    | M.ViewFilesReq t when
      !CommonUploads.has_upload = 0 && !!allow_browse_share -> 
        let files = DonkeyShare.browsable_shared () in
        direct_client_send_files sock files
    
    | M.QueryFileReq t ->
        c.client_requests_received <- c.client_requests_received + 1;
        
        if  !CommonUploads.has_upload = 0 && 
          not (!!ban_queue_jumpers && c.client_banned) then
          
          
          let could_be_challenge = ref false in
          
          if challenge.challenge_solved = t then begin
(*      lprintf "Client replied to challenge !!"; lprint_newline (); *)
              c.client_brand <- Brand_mldonkey3;
              could_be_challenge := true;
            end;
          
          if not challenge.challenge_ok  && t <> challenge.challenge_md4 then begin
              could_be_challenge := true;
              DonkeyProtoCom.direct_client_send c (
                let module M = DonkeyProtoClient in
                M.QueryFileReq (solve_challenge t));
              challenge.challenge_ok <- true;
            end;
          
          (try if not !could_be_challenge then
                client_wants_file c t with _ -> ());
          
          
          if t = Md4.null && c.client_brand = Brand_edonkey then  begin
              c.client_brand <- Brand_mldonkey1;
              if Random.int 100 < 2 && !!send_warning_messages then
                direct_client_send c (
                  M.SayReq "[AUTOMATED WARNING] Please, Update Your MLdonkey client to version 2.01");
            end;
          begin 
            count_filerequest c;          
            try
              let shared = 
                try
                  let file = Hashtbl.find files_by_md4 t in
                  DonkeySourcesMisc.query_file c file;
                  request_for c file sock;
                  as_shared file.file_shared;
                with _ -> 
                    let shared = Hashtbl.find shared_by_md4 t in
                    as_shared shared.shared_impl
              in
              shared_add_request shared;
              set_client_upload (as_client c.client_client) (Some shared);
              let filename = shared_codedname shared in
              let filename = Filename.basename filename in
              let filename = if String2.starts_with filename "hidden."
                then String.sub filename 7 (String.length filename - 7) 
                else filename
              in
              direct_client_send c (
                let module Q = M.QueryFileReply in
                M.QueryFileReplyReq {
                  Q.md4 = t;
                  Q.name = filename
                });
            
            
            with _ -> 
                direct_client_send c (
                  M.NoSuchFileReq t)
          end
    
    | M.EmuleRequestSourcesReplyReq t ->
(*      lprintf "Emule sent sources"; lprint_newline (); *)
        let module Q = M.EmuleRequestSourcesReply in
        begin
          try
            let file = find_file t.Q.md4 in
(* Always accept sources when already received !
  
          if file.file_enough_sources then begin
              lprintf "** Dropped %d sources for %s **" (List.length t.Q.sources) (file_best_name file);
              lprint_newline ()
end else *)
            if !verbose_location then begin
                lprint_newline ();
                lprintf "Client: Received %d sources for %s" (Array.length t.Q.sources) (file_best_name file);
              end;
            Array.iter (fun s ->
                if Ip.valid s.Q.ip && 
                  ((not !!black_list) || Ip.reachable s.Q.ip) then
                  ignore (DonkeySources.new_source (s.Q.ip, s.Q.port) file)
                else
                  begin
                    let module C = Udp.QueryCallUdp in
                    DonkeyProtoCom.udp_send (get_udp_sock ())
                    s.Q.server_ip (s.Q.server_port+4)
                    (Udp.QueryCallUdpReq {
                        C.ip = client_ip None;
                        C.port = !client_port;
                        C.id = s.Q.ip;
                      })
                  
                  end
            ) t.Q.sources
          with _ -> ()
        end
    
    
    | M.SourcesReq t ->
        
        let module Q = M.Sources in
        begin
          try
            let file = find_file t.Q.md4 in
(* Always accept sources when already received !
  
          if file.file_enough_sources then begin
              lprintf "** Dropped %d sources for %s **" (List.length t.Q.sources) (file_best_name file);
              lprint_newline ()
            end else *)
            List.iter (fun (ip1, port, ip2) ->
                if Ip.valid ip1 && 
                  ((not !!black_list) || Ip.reachable ip1) then
                  ignore (DonkeySources.new_source (ip1, port) file)
            ) t.Q.sources
          with _ -> ()
        end
    
    | M.SayReq s ->
        
        let ad_opt =
          match c.client_kind with
            Known_location (ip, port) ->
              (
                match c.client_chat_port with
                  0 -> None
                | p ->Some (Ip.to_string ip, p)
              )
          |   Indirect_location _ -> None
        in
(* A VOIR : historique à gérer *)
(*      !say_hook c s *)
        private_message_from (as_client c.client_client)  s;
        
        let cip =
          ( 
            try
              
              match c.client_sock with
                Some sock -> Ip.to_string (peer_ip sock)
              | None -> (match c.client_kind with 
                      Known_location (ip,port) -> Ip.to_string ip
                    | Indirect_location _ -> "Indirect"
                  )
            
            with _ -> 
                
                try 
                  match c.client_kind with 
                    Known_location (ip,port) -> Ip.to_string ip
                  | Indirect_location _ -> "Indirect"
                with _ -> ""
          ) 
        in
        log_chat_message cip (client_num c) c.client_name s;
    
    | M.QueryChunkMd4Req t when !CommonUploads.has_upload = 0 -> 
        
        let md4s = 
          try
            let file = Hashtbl.find files_by_md4 t in
            file.file_md4s
          with _ -> 
              let sh = Hashtbl.find shared_by_md4 t in
              sh.shared_info.sh_md4s
        in
        direct_client_send c (
          let module Q = M.QueryChunkMd4Reply in
          M.QueryChunkMd4ReplyReq {
            Q.md4 = t;
            Q.chunks = md4s
          })
        
    
    | M.QueryChunksReq t ->
        c.client_requests_received <- c.client_requests_received + 1;
        
        if  !CommonUploads.has_upload = 0 && not 
            (!!ban_queue_jumpers && c.client_banned) then
          
          let chunks = 
            if Hashtbl.mem shared_by_md4 t then
              [||]
            else
            let file = find_file t in
            DonkeySourcesMisc.query_file c file;
            let bitmap = Int64Swarmer.verified_bitmap file.file_partition in
            String2.map (fun state ->
                state = '3' 
            ) bitmap
          in
          direct_client_send c (
            let module Q = M.QueryChunksReply in
            M.QueryChunksReplyReq {
              Q.md4 = t;
              Q.chunks = chunks;
            });
    
    | M.QueryBlocReq t when !CommonUploads.has_upload = 0 &&
      client_has_a_slot (as_client c.client_client) ->
        
        if !verbose then begin
            lprintf "uploader %s(%s) ask for block\n" c.client_name
              (brand_to_string c.client_brand); 
          end;
        
        set_lifetime sock active_lifetime;
        set_rtimeout sock !!upload_timeout;
        let module Q = M.QueryBloc in
        let shared, fd = find_shared  t.Q.md4 in
        
        let up, waiting = match c.client_upload with
            Some ({ up_md4 = md4 } as up) when md4 == t.Q.md4 ->  up, up.up_waiting
          | Some old_up ->
              {
                up_md4 = t.Q.md4;
                up_shared = shared;
                up_fd = fd;
                up_pos = Int64.zero;
                up_end_chunk = Int64.zero;
                up_chunks = [];
                up_waiting = old_up.up_waiting;
              }, old_up.up_waiting
          | _ ->
              {
                up_md4 = t.Q.md4;
                up_shared = shared;
                up_fd = fd;
                up_pos = Int64.zero;
                up_end_chunk = Int64.zero;
                up_chunks = [];
                up_waiting = false;
              }, false
        in
        new_chunk up t.Q.start_pos1 t.Q.end_pos1;
        new_chunk up t.Q.start_pos2 t.Q.end_pos2;
        new_chunk up t.Q.start_pos3 t.Q.end_pos3;
        c.client_upload <- Some up;
        set_client_upload (as_client c.client_client) (Some up.up_shared);
        if not waiting && !CommonUploads.has_upload = 0 then begin
(*            lprintf "ready_for_upload\n"; *)
            CommonUploads.ready_for_upload (as_client c.client_client);
            up.up_waiting <- true
          end
    
    | M.NoSuchFileReq t ->
        begin
          try
            let file = find_file t in
            DonkeySourcesMisc.set_request_result c file  File_not_found;
          with _ -> ()
        end
    
    | _ -> 
        if !verbose_unknown_messages then begin
            lprintf "Unused Client Message:\n"; 
            M.print t;
            lprint_newline () 
          end
  with e ->
      lprintf "Exception %s in client_to_client\n" (Printexc2.to_string e)
      
let client_handler c sock event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      disconnect_client c s;

  | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
      printf_string "[TO?]";
      close sock Closed_for_timeout;

      (*
      if c.client_name <> "" then begin
          lprintf "client %s(%s) disconnected: reason %s"
            c.client_name (brand_to_string c.client_brand) s;
          lprint_newline ();
        end
*)
      
  | _ -> ()

let client_handler2 c sock event = 
  (match event with
      BASIC_EVENT (CLOSED s) -> decr DonkeySourcesMisc.indirect_connections
    | _ -> ());
  match !c with
    Some c -> client_handler c sock event
  | None ->
      match event with
        BASIC_EVENT (CLOSED s) ->
          printf_string "-c";
      
      | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
          printf_string "[TO?]";
          close sock Closed_for_timeout
          
      | _ -> ()
      
let init_connection sock =
(*  ignore (setsock_iptos_throughput (fd (TcpBufferedSocket.sock sock))); *)
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  set_rtimeout sock !!client_timeout;
  
(* Fix a lifetime for the connection. If we are not able to connect and
query file within this delay, the connection is aborted. 
  
With 150 connections of 1 minute, it means we can at most make 
make 1500 connections/10 minutes.  *)
  
(*  set_lifetime sock 60.; *)
  ()
  
let init_client sock c =
  set_handler sock WRITE_DONE (fun s ->
      match c.client_upload with
        None -> ()
      | Some up ->
          if not up.up_waiting && !CommonUploads.has_upload = 0 then begin
              up.up_waiting <- true;
              CommonUploads.ready_for_upload (as_client c.client_client)
            end
  );
(*
  set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
      connection_delay c.client_connection_control;
      printf_string "-!C";
      close s "timeout"
  ); *)
  c.client_block <- None;
  c.client_blocks <- [];
  c.client_file_queue <- [];
  c.client_has_a_slot <- false;
  c.client_upload <- None;
  c.client_rank <- 0;
  c.client_requests_received <- 0;
  c.client_requests_sent <- 0;
  c.client_slot <- SlotNotAsked
        
let read_first_message overnet challenge m sock =
  let module M = DonkeyProtoClient in
    
  if !verbose_msg_clients then begin
      lprintf "Message from client connecting client\n";
      M.print m;
      lprint_newline ();
    end;

  match m with
  
  | M.ConnectReq t ->
      printf_string "******* [PCONN OK] ********";
      
      let module CR = M.Connect in
      
      
      if t.CR.md4 = !!client_md4 ||
        t.CR.md4 = overnet_md4 then begin
          TcpBufferedSocket.close sock (Closed_for_error "Connected to myself");
          raise End_of_file
        end;

(* Test if the client is already connected *)
      if Hashtbl.mem connected_clients t.CR.md4 then begin
(*          lprintf "Client is already connected"; lprint_newline (); *)
          close sock (Closed_for_error "already connected");
          raise Exit
        end;
      let name = ref "" in
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> name := s
          | _ ->  ()
      ) t.CR.tags;
      
      let kind, indirect = try
          match t.CR.server_info with
            Some (ip, port) -> 
              if not (Ip.valid t.CR.ip) then
                if Ip.valid ip then
                  Indirect_location (!name,t.CR.md4),
                  Some (t.CR.ip, ip, port)
                else
                  raise Not_found
              else
                Known_location (t.CR.ip, t.CR.port), None
          | None ->  raise Not_found
        with _ -> Indirect_location (!name,t.CR.md4), None in
      let c = new_client kind in
      
      if c.client_debug || !verbose_msg_clients then begin  
          lprintf "First Message\n";
          M.print m;
          lprint_newline ();
        end;
      
      Hashtbl.add connected_clients t.CR.md4 c;
      
      begin
        match c.client_sock with
          None -> 
            c.client_sock <- Some sock;
            c.client_ip <- peer_ip sock;
            c.client_connected <- false;
            init_client sock c;
            c.client_connect_time <- last_time ();
        
        | Some _ -> 
            close sock (Closed_for_error "already connected");
            raise Not_found
      end;
      
      begin
        match c.client_source, kind with
          None, Known_location (ip, port) ->
            let s = DonkeySourcesMisc.create_source 0 (last_time ()) (ip, port) in
            c.client_source <- Some s
        | _ -> 
            c.client_indirect_address <- indirect;
      end;
      
      c.client_checked <- true;
      
      set_write_power sock c.client_power;
      set_read_power sock c.client_power;
      
      set_client_name c !name t.CR.md4;
      connection_try c.client_connection_control;
      connection_ok c.client_connection_control;
      c.client_tags <- t.CR.tags;
      
      if  !!reliable_sources && 
        ip_reliability (peer_ip sock) = Reliability_suspicious 0 then begin
          set_client_state c BlackListedHost;
          raise Not_found
        end;
      
      begin
        match t.CR.server_info with
          Some (ip, port) when !!update_server_list -> 
            safe_add_server ip port
        | None -> 
            if overnet then begin
                lprintf "incoming Overnet client\n"; 
                c.client_overnet <- overnet;
              end
        | _ -> ()
      end;

(*      List.iter (fun s ->
		   match s.server_sock with
		       None -> ()
		     | Some _ -> if ?? = s.server_ip then
			 c.client_brand <- Brand_server
) (connected_servers ()) *)
      identify_client_brand c;
      
      direct_client_send c (
        let module M = DonkeyProtoClient in
        let module C = M.ConnectReply in
        if c.client_overnet then
          M.ConnectReplyReq {
            C.md4 = overnet_md4;
            C.ip = client_ip (Some sock);
            C.port = !overnet_client_port;
            C.tags = !overnet_connectreply_tags;
            C.server_info = Some (!overnet_server_ip, 
              !overnet_server_port);
            C.left_bytes = left_bytes;
          }
        else
          M.ConnectReplyReq {
            C.md4 = !!client_md4;
            C.ip = client_ip (Some sock);
            C.port = !client_port;
            C.tags = !client_tags;
            C.server_info = t.CR.server_info;
            C.left_bytes = left_bytes;
          }
      );

      init_client_connection c sock;
            
      set_client_state c (Connected (-1));      
      
      challenge.challenge_md4 <-  Md4.random ();
      direct_client_send c (
        let module M = DonkeyProtoClient in
        M.QueryFileReq challenge.challenge_md4);
      challenge.challenge_solved <- solve_challenge challenge.challenge_md4;
      
      query_files c sock; 
      
      if client_type c <> NormalClient then
        if last_time () > c.client_next_view_files then begin
(*
            lprintf "****************************************";
            lprint_newline ();
            lprintf "       ASK VIEW FILES         ";
            lprint_newline ();
*)
            
            direct_client_send c (
              let module M = DonkeyProtoClient in
              let module C = M.ViewFiles in
              M.ViewFilesReq C.t);          
          end;
        client_must_update c;
      is_banned c sock;
      Some c
  
  | M.NewUserIDReq _ ->
      M.print m; lprint_newline ();
      None
  
  | _ -> 
      lprintf "BAD MESSAGE FROM CONNECTING CLIENT\n"; 
      M.print m; lprint_newline ();
      close sock (Closed_for_error "bad connecting message");
      raise Not_found
      
      
let reconnect_client c =
  if can_open_connection () then begin
      match c.client_kind with
        Indirect_location _ -> ()
      | Known_location (ip, port) ->
          if client_state c <> BlackListedHost then
            if !!black_list && is_black_address ip port ||
              (!!reliable_sources && ip_reliability ip = Reliability_suspicious 0) then
              set_client_state c BlackListedHost
            else
            try
              set_client_state c Connecting;
              connection_try c.client_connection_control;
              
              if !verbose_connect then 
                lprintf "[VERBOSE] Connecting to edonkey client %s:%d\n"
                  (Ip.to_string ip) port;
              
              printf_string "?C";
              let sock = TcpBufferedSocket.connect "donkey to client" (
                  Ip.to_inet_addr ip) 
                port 
                  (client_handler c) (*client_msg_to_string*) in
              TcpBufferedSocket.set_write_power sock c.client_power;
              TcpBufferedSocket.set_read_power sock c.client_power;
              c.client_connect_time <- last_time ();
              init_connection sock;
              init_client sock c;
(* The lifetime of the client socket is now half an hour, and
can be increased by AvailableSlotReq, BlocReq, QueryBlocReq 
  messages *)
              set_lifetime sock active_lifetime;
              
              c.client_checked <- false;
              
              let challenge = {
                  challenge_md4 = Md4.null;
                  challenge_solved = Md4.null;
                  challenge_ok = false;
                } in
              set_reader sock (
                DonkeyProtoCom.cut_messages DonkeyProtoClient.parse
                  (client_to_client challenge files c));
              
              c.client_sock <- Some sock;
              c.client_ip <- ip;
              c.client_connected <- true;
              let server_ip, server_port = 
                try
                  let s = DonkeyGlobals.last_connected_server () in
                  s.server_ip, s.server_port
                with _ -> Ip.localhost, 4665
              in
              
              direct_client_send c (
                let module M = DonkeyProtoClient in
                let module C = M.Connect in
                if c.client_overnet then
                  M.ConnectReq {
                    C.md4 = overnet_md4;
                    C.ip = client_ip None;
                    C.port = !overnet_client_port;
                    C.tags = !overnet_connect_tags;
                    C.version = 16;
                    C.server_info = Some (!overnet_server_ip, 
                      !overnet_server_port);
                    C.left_bytes = left_bytes;
                  }
                else
                  M.ConnectReq {
                    C.md4 = !!client_md4;
                    C.ip = client_ip None;
                    C.port = !client_port;
                    C.tags = !client_tags;
                    C.version = 16;
                    C.server_info = Some (server_ip, server_port);
                    C.left_bytes = left_bytes;
                  }
              )
            
            with e -> 
                lprintf "Exception %s in client connection\n"
                  (Printexc2.to_string e);
                connection_failed c.client_connection_control;
                set_client_disconnected c (Closed_for_exception e)
    end  
    
let query_id s sock ip file =
  printf_string "[QUERY ID]";
  direct_server_send sock (
    let module M = DonkeyProtoServer in
    let module C = M.QueryID in
    M.QueryIDReq ip
  );
  Fifo.put s.server_id_requests file
  
let query_locations_reply s t =
  let module M = DonkeyProtoServer in
  let module Q = M.QueryLocationReply in
  
  connection_ok s.server_connection_control;
  
  try
    let file = find_file t.Q.md4 in
    let nlocs = List.length t.Q.locs in

    if !verbose_location then begin
        lprintf "Server: Received %d sources for %s\n" nlocs (file_best_name file);
      end;
        
    s.server_score <- s.server_score + 3;

    List.iter (fun l ->
        let ip = l.Q.ip in
        let port = l.Q.port in
        
        if Ip.valid ip then
          (if (not !!black_list) || Ip.reachable ip  then 
              ignore (DonkeySources.new_source (ip, port) file))
        else
        match s.server_sock with
          None ->
            let module Q = Udp.QueryCallUdp in
            udp_server_send s 
              (Udp.QueryCallUdpReq {
                Q.ip = client_ip None;
                Q.port = !client_port;
                Q.id = ip;
              })
        
        | Some sock ->
            printf_string "QUERY ID";
            query_id s sock ip (Some file)
    ) t.Q.locs
  
  with Not_found -> ()
      
let can_open_indirect_connection () =
  let ns = nb_sockets () in
  ns < MlUnix.max_sockets &&
  !DonkeySourcesMisc.indirect_connections < !!max_indirect_connections

let client_connection_handler overnet t event =
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !verbose_connect then
        lprintf "Connection from edonkey client %s:%d\n"
          (Unix.string_of_inet_addr from_ip) from_port;
      if can_open_indirect_connection () then
        begin
          accept_connection_bandwidth download_control upload_control;
          (try
              let c = ref None in
              incr DonkeySourcesMisc.indirect_connections;
              let sock = 
                TcpBufferedSocket.create "donkey client connection" s 
                  (client_handler2 c) 
(*client_msg_to_string*)
              in
              init_connection sock;
              
(* Normal connections have 20 minutes to live (AvailableSlot, QueryBloc
  and Bloc messages extend this lifetime), whereas exceeding connections
  have only 1 minute 30 seconds to live. *)
              set_lifetime sock (
                if can_open_connection () then
                  active_lifetime
                else 
                  90.
              );
              (try
                  
                  let challenge = {
                      challenge_md4 = Md4.null;
                      challenge_solved = Md4.null;
                      challenge_ok = false;
                    } in
                  set_reader sock 
                    (DonkeyProtoCom.client_handler2 c (read_first_message overnet challenge)
                    (client_to_client challenge []));
                
                with e -> lprintf "Exception %s in init_connection\n"
                      (Printexc2.to_string e);
                    );
            with e ->
                lprintf "Exception %s in client_connection_handler\n"
                  (Printexc2.to_string e);
                Unix.close s)
        end     
      
      else begin
          lprintf "***** CONNECTION PREVENTED by limitations *****\n";
          Unix.close s
        end;
  | _ -> 
      ()      
      
      
