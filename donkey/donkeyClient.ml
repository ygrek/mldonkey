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

(* Some things about Emule:
- If a client asks for a file more often than 590 seconds, ie 10 minutes,
  it gets a BAD REQUEST
- If a client has 2 BAD REQUESTS, it get banned !
*)

open Md4

open CommonRoom
open CommonShared
open CommonGlobals
open CommonFile
open CommonClient
open CommonComplexOptions
open GuiProto
open CommonResult
open CommonTypes
open Options
open BasicSocket
open DonkeyMftp
open DonkeyProtoCom
open TcpBufferedSocket
open DonkeyOneFile
open DonkeyOptions
open CommonOptions
open DonkeyComplexOptions
open DonkeyGlobals
open DonkeyStats
open DonkeyTypes  

type request_record = {
  mutable last_request : float;
  mutable nwarnings : int;
}

let banned_ips = Hashtbl.create 113
let old_requests = Hashtbl.create 13013

let is_banned c sock = c.client_banned <- Hashtbl.mem banned_ips (peer_ip sock)
    
let ban_client c sock = 
  let ip = peer_ip sock in
  count_banned c;
  c.client_banned <- true;
  Hashtbl.add banned_ips ip (last_time ())
  
let request_for c file sock =
  if !!ban_queue_jumpers then
    try
      let record = Hashtbl.find old_requests (client_num c, file_num file) in
      if record.last_request +. 540. > last_time () then begin
          record.nwarnings <- record.nwarnings+ 1;
          record.last_request <- last_time ();
          if record.nwarnings > 3 then raise Exit;
          let module M = DonkeyProtoClient in
          if record.nwarnings =3 then begin
              if !!verbose then begin
                  Printf.printf "uploader %s(%s) has been banned" 
                    c.client_name (brand_to_string c.client_brand);
                  print_newline ();
                end;
              
              ban_client c sock;
              if !!send_warning_messages then
                direct_client_send sock ( M.SayReq  (
                    "[ERROR] Your client is connecting too fast, it has been banned"));
              raise Exit;
            end;
          if !!verbose then begin
              Printf.printf "uploader %s(%s) has been warned" 
                c.client_name (brand_to_string c.client_brand);
              print_newline ();
            end;
          if !!send_warning_messages then
            direct_client_send sock ( M.SayReq  (
                "[WARNING] Your client is connecting too fast, it will get banned"))
        end else
        record.last_request <- last_time ();
    with Not_found ->
        Hashtbl.add old_requests (client_num c, file_num file) 
        { last_request = last_time (); nwarnings = 0; }
        
let clean_requests () = (* to be called every hour *)
  Hashtbl.clear old_requests;
  let remove_ips = ref [] in
  Hashtbl.iter (fun ip time ->
      if time +. 3600. *. !!ban_period < last_time () then 
        remove_ips := ip :: !remove_ips
  ) banned_ips;
  List.iter (fun ip ->
      Hashtbl.remove banned_ips ip;
  ) !remove_ips

  
let pending_slots_map = ref Intmap.empty
let pending_slots_fifo = Fifo.create ()
  
let add_pending_slot c =
  if not (Intmap.mem (client_num c) !pending_slots_map) then begin
      pending_slots_map := Intmap.add (client_num c) c !pending_slots_map;
      Fifo.put pending_slots_fifo (client_num c)
    end
  
let remove_pending_slot c =
  if Intmap.mem (client_num c) !pending_slots_map then
    pending_slots_map := Intmap.remove (client_num c) !pending_slots_map
    
let rec give_a_slot c = 
  remove_pending_slot c;
  match c.client_sock with
    None -> find_pending_slot ()
  | Some sock ->
      set_rtimeout sock !!upload_timeout;
      
      direct_client_send sock (
        let module M = DonkeyProtoClient in
        let module Q = M.AvailableSlot in
        M.AvailableSlotReq Q.t);
      c.client_has_a_slot <- true;
      
      if !!verbose then begin
          Printf.printf "New uploader %s: brand %s" 
            c.client_name (brand_to_string c.client_brand);
          print_newline ();
        end;
      
      set_write_power sock (c.client_power);
      set_read_power sock (c.client_power)
      
and find_pending_slot () =
  try
    let rec iter () =
      let cnum = Fifo.take pending_slots_fifo in
      try
        let c = Intmap.find cnum !pending_slots_map in
        give_a_slot c
      with _ -> iter ()
    in
    iter ()
  with _ -> ()
  
let disconnect_client c =
  match c.client_sock with
    None -> ()
  | Some sock ->
      remove_pending_slot c;
      connection_failed c.client_connection_control;
      TcpBufferedSocket.close sock "closed";
      printf_string "-c"; 
      c.client_has_a_slot <- false;
      c.client_chunks <- [||];
      c.client_sock <- None;
      set_client_state c NotConnected;
      let files = c.client_file_queue in
      List.iter (fun (file, chunks) -> 
          remove_client_chunks file chunks)  
      files;    
      c.client_file_queue <- [];
      if c.client_upload != None then find_pending_slot ();
      DonkeyOneFile.clean_client_zones c;
      DonkeySources1.source_of_client c
  
let client_send_if_possible sock msg =
  if can_write_len sock (!!client_buffer_size/2) then
    client_send sock msg
  
let verbose_group = ref false

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

let max_file_groups = 1000
let file_groups_fifo = Fifo.create ()

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
                      list := (uc.udp_client_ip, uc.udp_client_port, uc.udp_client_ip) :: !list
                  ) group.group;
                  if !list <> [] then begin
                      if !verbose_group then begin
                          Printf.printf "Send %d sources from file groups to mldonkey peer" (List.length !list); print_newline ();                  
                        end;
                      let msg = 
                        let module Q = DonkeyProtoClient.Sources in
                        DonkeyProtoClient.SourcesReq {
                          Q.md4 = md4;
                          Q.sources = !list;
                        }
                      in
                      client_send_if_possible sock msg 
                    end
            end;
          
          match c.client_kind with 
            Indirect_location _ -> ()
          | Known_location (ip, port) ->
(* send this client as a source for the file to
		     all mldonkey clients in the group. add client to group *)
              
              UdpClientMap.iter (fun _ uc ->
                  if uc.udp_client_can_receive then begin
                      if !verbose_group then
                        (Printf.printf "Send new source to file groups UDP peers"; 
                          print_newline ());
                      udp_client_send uc (
                        let module M = DonkeyProtoServer in
                        M.QueryLocationReplyUdpReq (
                          let module Q = M.QueryLocationReply in
                          {
                            Q.md4 = md4;
                            Q.locs = [{ Q.ip = ip; Q.port = port }];
                          }))
                    end
              ) group.group;
              new_udp_client c group
    with _ ->
        if Fifo.length file_groups_fifo >= max_file_groups then 
          Hashtbl.remove file_groups (Fifo.take file_groups_fifo);
        let group = { group = UdpClientMap.empty } in
        Hashtbl.add file_groups md4 group;
        Fifo.put file_groups_fifo md4;
        new_udp_client c group
        
let clean_groups () =
  let one_day_before = last_time () -. one_day in
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
    let len_requested = Int32.to_int (Int32.sub end_pos begin_pos) in
    let len = Int32.to_int (Int32.sub end_pos begin_pos) in
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
      if md4.[5] = Char.chr 13 && md4.[14] = Char.chr 110 then
        Brand_oldemule
      else if md4.[5] = Char.chr 14 && md4.[14] = Char.chr 111 then
	Brand_newemule
      else if md4.[5] = 'M' && md4.[14] = 'L' then
        Brand_mldonkey2
    else
      if c.client_overnet then Brand_overnet else Brand_edonkey)

let query_files c sock =  
  let nall_queries = ref 0 in
  let nqueries = ref 0 in
  List.iter (fun file ->
      incr nall_queries;
      DonkeySources1.query_file sock c file)
  (if c.client_files = [] then !current_files
(*
TODO: test if we can really send this request conforming to the reask delay
*)
    else c.client_files);
  if !nqueries > 0 then
    c.client_last_filereqs <- last_time ();
  if !!verbose then begin
      Printf.printf "sent %d/%d file queries" !nqueries !nall_queries;
      print_newline ()
    end
    
  
let client_has_chunks c file chunks =
  
  DonkeySources1.add_file_location file c;
  
  DonkeySources1.source_has_file c;  
  
  if file.file_chunks_age = [||] then
    file.file_chunks_age <- Array.create file.file_nchunks 0.0;
  let change_last_seen = ref false in
  let chunks_string = String.make file.file_nchunks '0' in
  for i = 0 to file.file_nchunks - 1 do
    if chunks.(i) then chunks_string.[i] <- '1';
    match file.file_chunks.(i) with
      PresentVerified | PresentTemp -> 
        file.file_chunks_age.(i) <- last_time ()
    | _ -> 
        if chunks.(i) then begin
            change_last_seen := true;
            file.file_chunks_age.(i) <- last_time ();
            DonkeySources1.source_has_new_chunk c;
      end;
  done;
  
  if !change_last_seen then begin
      try
        let last_seen =  Array2.min file.file_chunks_age in
        if last_seen > file.file_file.impl_file_last_seen then
          begin
            file.file_file.impl_file_last_seen <- last_seen;
            file_must_update_downloaded (as_file file.file_file)
          end
      with _ -> ()
    end;
  
  add_client_chunks file chunks;
  CommonEvent.add_event (File_update_availability
      (as_file file.file_file, as_client c.client_client, chunks_string));
  
  match c.client_file_queue with
    [] ->
      c.client_file_queue <- [file, chunks];
      start_download c
  
  | _ -> 
      c.client_file_queue <- c.client_file_queue @ [file, chunks]

      
(*      
let add_new_location sock file c =
  
  let module M = DonkeyProtoClient in
  
  if !!propagate_sources &&
    (match c.client_brand with
	 Brand_mldonkey1 | Brand_overnet -> false
       | _ -> true) &&
    last_time () -. connection_last_conn c.client_connection_control < 300.
  then begin
(* Send the known sources to the client. *)
      
      if client_can_receive c then begin
          match c.client_kind with 
            Indirect_location _ -> ()
          | Known_location(ip, port) ->

(* send at most 10 sources connected in the last quarter to this new location *)
              let sources = ref [] in
              let send_locations = ref 10 in
              let time = last_time () -. 900. in
              try
                Intmap.iter (fun _ cc ->
                    if cc.client_checked &&
                      connection_last_conn cc.client_connection_control > time
                    then
                      match cc.client_kind with
                      | Known_location (ip, port) -> 
                          sources := (ip, port, ip) :: !sources;
                          decr send_locations;
                          if !send_locations = 0 then
                            raise Exit;
                      | _ -> ()
                ) file.file_sources;
              with _ -> ();
                  
                  if !sources <> [] then
                    client_send_if_possible sock ( (
                        let module Q = M.Sources in
                        M.SourcesReq {
                          Q.md4 = file.file_md4;
                          Q.sources = !sources;
                        }));
        end;
      
      match c.client_kind with
            Indirect_location _ -> ()
          | Known_location (ip, port) ->
              try
(* send this location to at most 10 other locations, 
		 connected in the last quarter *)
                let counter = ref 10 in

(* send this location to  all connected locations *)
                let msg = 
                  let module Q = M.Sources in
                  M.SourcesReq {
                    Q.md4 = file.file_md4;
                    Q.sources = [(ip, port, ip)];
                  }
                in
                let time = last_time () -. 900. in
                Intmap.iter (fun _ uc ->
                    if uc.client_checked &&
                      (client_can_receive uc) &&
                      connection_last_conn uc.client_connection_control  
                        > time then
                      match uc.client_kind with
                        Known_location (src_ip, src_port) -> 
                          
                          client_udp_send src_ip src_port (
                            let module Q = DonkeyProtoServer.QueryLocationReply in
                            DonkeyProtoServer.QueryLocationReplyUdpReq {
                              Q.md4 = file.file_md4;
                              Q.locs = [{ 
                                  Q.ip = ip;
                                  Q.port = port;
                                }];
                            });
                          decr counter;
                          if !counter = 0 then raise Exit;
                          
                          | _ -> 
                          match uc.client_sock with 
                            None -> ()
                              | Some sock -> 
                              client_send_if_possible sock msg;
                              decr counter;
                              if !counter = 0 then raise Exit;
                
                ) file.file_sources;
              with _ -> ();
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
| Seven of challenge_vals * int

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
    | _ -> Seven (v, hash v)
  in
  let array = Array.init 4 (fun i -> 
        let n = LittleEndian.get_int md4 (4*i) in
        iter n) in
  let v = Marshal.to_string array [] in
  Md4.string v  
  
let client_to_client challenge for_files c t sock = 
  let module M = DonkeyProtoClient in
(*
  Printf.printf "Message from client %s(%s)"
  c.client_name (brand_to_string c.client_brand);
  print_newline ();
  M.print t;
  print_newline ();
*)  
  match t with
    M.ConnectReplyReq t ->
      printf_string "******* [CCONN OK] ********"; 
      
      c.client_checked <- true;
      c.client_has_a_slot <- false;
      
      let module CR = M.ConnectReply in
      
      if t.CR.md4 = !!client_md4 ||
         t.CR.md4 = overnet_md4 then
        TcpBufferedSocket.close sock "connected to myself";
      
      c.client_tags <- t.CR.tags;
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> 
              set_client_name c s t.CR.md4
          | _ -> ()
      ) c.client_tags;
      
      
      connection_ok c.client_connection_control;
      
      begin
        match t.CR.server_info with
          Some (ip, port) when !!update_server_list -> safe_add_server ip port
        | _ -> ()
      end;
      
      identify_client_brand c;
      
      if c.client_brand = Brand_newemule then  begin
(*    Printf.printf "Emule Extended Protocol query"; print_newline ();*)
          let module E = M.EmuleClientInfo in
          emule_send sock (M.EmuleClientInfoReq {
              E.version = 0x24; 
              E.protversion = 0x1;
              E.tags = [
(*           int_tag "compression" 0; *)
                int_tag "udp_port" (!!port+4)
              ]
            })
        end;
      
      set_client_state c Connected_idle;      
      
      let challenge_md4 =  Md4.random () in
      DonkeyProtoCom.direct_client_send sock (
        let module M = DonkeyProtoClient in
        let module C = M.QueryFile in
        M.QueryFileReq challenge_md4);
      challenge.challenge_md4 <- solve_challenge challenge_md4;
      
      query_files c sock;
      client_must_update c;      
      if client_type c <> NormalClient then begin
          if last_time () > c.client_next_view_files then begin
(*
            Printf.printf "****************************************";
            print_newline ();
            Printf.printf "       ASK VIEW FILES         ";
print_newline ();
  *)
              direct_client_send sock (
                let module M = DonkeyProtoClient in
                let module C = M.ViewFiles in
                M.ViewFilesReq C.t);          
            end
        end;
      is_banned c sock
  
  | M.EmuleClientInfoReq t ->      
(*      Printf.printf "Emule Extended Protocol asked"; print_newline (); *)
      if c.client_brand = Brand_newemule then  begin
          let module E = M.EmuleClientInfo in
          emule_send sock (M.EmuleClientInfoReplyReq {
              E.version = 0x24; 
              E.protversion = 0x1;
              E.tags = [
                int_tag "compression" 0;
                int_tag "udp_port" (!!port+4)
              ]
            })
        end
  
  
  | M.EmuleRequestSourcesReq t ->
(*       Printf.printf "Emule requested sources"; print_newline (); *)
      ()
  
  | M.EmuleClientInfoReplyReq t -> 
(*   Printf.printf "Emule Extended Protocol activated"; print_newline (); *)
      ()
  
  
  | M.ViewFilesReplyReq t ->
      c.client_next_view_files <- last_time () +. 3600. *. 6.;
(*
      Printf.printf "****************************************";
      print_newline ();
      Printf.printf "       VIEW FILES REPLY         ";
      print_newline ();
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
            Printf.printf "Exception in ViewFilesReply %s"
              (Printexc2.to_string e); print_newline ();
      end;
  
  | M.AvailableSlotReq _ ->
      set_rtimeout sock !!queued_timeout; 
(* how long should we wait for a block ? *)
      begin
        match c.client_block with
          None -> find_client_block c
        | Some b ->
            printf_string "[QUEUED WITH BLOCK]";      
      end
  
  | M.JoinQueueReq _ ->
      begin try
          
          begin
            match c.client_brand with
            | Brand_mldonkey2
            | Brand_mldonkey3 -> 
                if Fifo.length upload_clients >= !!max_upload_slots then
                  Fifo.iter (fun c -> 
                      if c.client_sock <> None && 
                        (c.client_brand = Brand_mldonkey2 ||
                          c.client_brand = Brand_mldonkey3) then raise Exit)
                  upload_clients
            | Brand_oldemule
            | Brand_newemule ->
                if Fifo.length upload_clients >= !!max_upload_slots then
                  raise Exit;
                let nemule = ref 0 in
                Fifo.iter (fun c -> 
                    if c.client_sock <> None && 
                      ( c.client_brand = Brand_oldemule || 
                        c.client_brand = Brand_newemule)
                    then incr nemule) upload_clients;
                if !nemule > (!!max_upload_slots * !!max_emule_slots) / 100
                then raise Exit
            | _ ->
                if Fifo.length upload_clients >= !!max_upload_slots then
                  raise Exit;
          end;
          
          give_a_slot c
        
        with _ ->
            add_pending_slot c;
            if !!verbose then begin
                Printf.printf "(uploader %s: brand %s, couldn't get a slot)" 
                  c.client_name (brand_to_string c.client_brand);
                print_newline ()
              end;
      end
  
  | M.CloseSlotReq _ ->
      printf_string "[DOWN]";
(* OK, the slot is closed, but what should we do now ????? *)
      begin
        match c.client_file_queue with
          [] -> ()
        | _ -> 
            direct_client_send sock (
              let module M = DonkeyProtoClient in
              let module Q = M.JoinQueue in
              M.JoinQueueReq Q.t);                        
            set_rtimeout sock !!queued_timeout;
            set_client_state c Connected_queued
      end
  
  | M.ReleaseSlotReq _ ->
      c.client_has_a_slot <- false;
      direct_client_send sock (
        let module M = DonkeyProtoClient in
        let module Q = M.CloseSlot in
        M.CloseSlotReq Q.t);
      if c.client_file_queue = [] then
        set_rtimeout sock 120.;
      find_pending_slot ()
  
  | M.QueryFileReplyReq t ->
      let module Q = M.QueryFileReply in
      
      begin
        try
          let file = find_file t.Q.md4 in
          if file_state file = FileDownloading then begin
              let s = Printf.sprintf "[FOUND FILE(%s)]" (match c.client_kind with
                    Known_location _ -> "OUT" | _ -> "IN") in
              printf_string s;
              c.client_rating <- c.client_rating + 1;

(* ask for more sources *)
              if c.client_brand = Brand_newemule &&
                DonkeySources1.need_new_sources () then begin
                  
(*              Printf.printf "Emule query sources"; print_newline (); *)
                  let module E = M.EmuleRequestSources in
                  emule_send sock (M.EmuleRequestSourcesReq file.file_md4)
                end;
              
              
              if not (List.mem t.Q.name file.file_filenames) then begin
                  file.file_filenames <- file.file_filenames @ [t.Q.name] ;
                  update_best_name file
                end;
              if file_size file > block_size then
                direct_client_send sock (
                  let module M = DonkeyProtoClient in
                  let module C = M.QueryChunks in
                  M.QueryChunksReq file.file_md4)
              else
                client_has_chunks c file [| true |]
            end
        with _ -> ()
      end  
  
  | M.QueryChunksReplyReq t ->
      
      let module Q = M.QueryChunksReply in      
      let file = find_file t.Q.md4 in
      
      let chunks = 
        if t.Q.chunks = [||] then
          Array.create file.file_nchunks true
        else
        if Array.length t.Q.chunks <> file.file_nchunks then begin
            Printf.printf "BAD BAD BAD: number of chunks is different %d/%d for %s:%ld on peer" (Array.length t.Q.chunks) file.file_nchunks (Md4.to_string file.file_md4) (file_size file);
            print_newline ();
            Array.create file.file_nchunks false
(* What should we do ?

1) Try to recover the correct size of the file: we can use 
ViewFilesReq on all clients having the file to test what is
the most widely used size for this file. Maybe create 
different instances of the file for each proposed size ?

*)
          
          end else 
          t.Q.chunks in
      
      client_has_chunks c file chunks
  
  | M.QueryChunkMd4ReplyReq t ->
      begin
        let module Q = M.QueryChunkMd4Reply in
        let file = find_file t.Q.md4 in
        
        let module Q = M.QueryChunkMd4Reply in
        if !!verbose then begin
            Printf.printf "MD4 FOR CHUNKS RECEIVED"; 
            print_newline ();
          end;
        
        if file.file_md4s <> [] then begin
            Printf.printf "[WARNING] Discarding Chunks Md4: already here";
            print_newline ();
          end else
        if file.file_nchunks = 1 then begin
            Printf.printf "[ERROR]: one chunk file without md4"; 
            print_newline ();
            file.file_md4s <- [file.file_md4]
          end else
        if t.Q.chunks = [||] then begin
            Printf.printf "[ERROR]: empty multiple chunks message";
            print_newline ();
          end
        else
        if Array.length t.Q.chunks <> file.file_nchunks then begin
            Printf.printf "BAD BAD BAD (2): number of chunks is different %d/%d for %s:%ld on peer" (Array.length t.Q.chunks) file.file_nchunks (Md4.to_string file.file_md4) (file_size file);
            print_newline ();
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
                Printf.printf "[ERROR]: Bad list of MD4s, discarding"; 
                print_newline ();
              end else
              file.file_md4s <- md4s
          
          
          end
(*      if file.file_exists then verify_chunks file *)
      end
  
  | M.BlocReq t ->
      let module Q = M.Bloc in
      let file = client_file c in
      
      DonkeySources1.source_has_upload c;
      
      if file.file_md4 <> t.Q.md4 then begin
          Printf.printf "BLOC FOR BAD FILE %s/%s !!"
            (Md4.to_string t.Q.md4) (Md4.to_string file.file_md4); 
          print_newline ();
          raise Not_found
        end;
      
      c.client_rating <- c.client_rating + 10;
      if file_state file = FilePaused then 
        (next_file c; raise Not_found);
      
      let begin_pos = t.Q.start_pos in
      let end_pos = t.Q.end_pos in
      
      set_client_state c Connected_busy;
      let len = Int32.sub end_pos begin_pos in
      count_download c file (Int64.of_int32 len);
      begin
        match c.client_block with
          None -> 
            printf_string "NO BLOCK EXPECTED FROM CLIENT";
            raise Not_found
        | Some b ->
            let str_begin = Int32.of_int t.Q.bloc_begin in
            
            if begin_pos < b.block_begin
                || begin_pos >= b.block_end || end_pos > b.block_end
            then 
              let chunk_num = Int32.to_int (Int32.div begin_pos block_size) 
              in
              Printf.printf "%d: Exceeding block boundaries" (client_num c);
              print_newline ();
              
              Printf.printf "%ld-%ld (%ld-%ld)" 
                (begin_pos) (end_pos)
              (b.block_begin) (b.block_end)
              ;
              print_newline ();
              
              List.iter (fun z ->
                  Printf.printf "zone: %ld-%ld"
                    (z.zone_begin) (z.zone_end)
              ) c.client_zones;

(* try to recover the corresponding block ... *)
              
              (match file.file_chunks.(chunk_num) with
                  PresentTemp | PresentVerified -> 
                    Printf.printf "ALREADY PRESENT"; print_newline ();
                | AbsentTemp | AbsentVerified ->
                    Printf.printf "ABSENT (not implemented)"; 
                    print_newline ();
                | PartialTemp b | PartialVerified b ->
                    Printf.printf "PARTIAL"; print_newline ();

(* try to find the corresponding zone *)
                    List.iter (fun z ->
                        if z.zone_begin >= begin_pos &&
                          end_pos > z.zone_begin then begin
                            Printf.printf "BEGIN ZONE MATCHES"; 
                            print_newline ();
                          end else
                        if z.zone_begin < begin_pos &&
                          begin_pos < z.zone_end &&
                          z.zone_end < end_pos then begin
                            Printf.printf "END ZONE MATCHES";
                            print_newline ();
                          end 
                    
                    ) b.block_zones
              );              
              raise Not_found
            else
            
            let final_pos = Unix32.seek32 (file_fd file) 
              begin_pos Unix.SEEK_SET in
            if final_pos <> begin_pos then begin
                Printf.printf "BAD LSEEK %ld/%ld"
                  (final_pos)
                (begin_pos); print_newline ();
                raise Not_found
              end;
            if c.client_connected then
              printf_string "#[OUT]"
            else
              printf_string "#[IN]";

(*            if !!verbose then begin
                Printf.printf "{%d-%d = %ld-%ld}" (t.Q.bloc_begin)
                (t.Q.bloc_len) (begin_pos) 
                (end_pos);
                print_newline ();
              end; *)
            try
              let fd = try
                  Unix32.force_fd (file_fd file) 
                with e -> 
                    Printf.printf "In Unix32.force_fd"; print_newline ();
                    raise e
              in
              Unix2.really_write fd t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len;
              List.iter (update_zone file begin_pos end_pos) c.client_zones;
              find_client_zone c;                    
            with
              End_of_file ->
                Printf.printf "WITH CLIENT %s" c.client_name;
                print_newline ();
            | e ->
                Printf.printf "Error %s while writing block. Pausing download" (Printexc2.to_string e);
                print_newline ();
                file_pause (as_file file.file_file);
      
      end;


(* Upload requests *)
  | M.ViewFilesReq t when !has_upload = 0 -> 
      let files = DonkeyShare.all_shared () in
      let published_files = ref [] in
      List.iter (fun f ->
          let filename = file_best_name f in
          if not (String2.starts_with filename "hidden.") then
            published_files := f :: !published_files
      ) files;
(*
       Printf.printf "ASK VIEW FILES"; print_newline ();
       *)
      direct_client_send_files sock !published_files
  
  | M.QueryFileReq t when !has_upload = 0 && 
    not (!!ban_queue_jumpers && c.client_banned) ->
      
      let could_be_challenge = ref false in
      
      if challenge.challenge_md4 = t then begin
(*      Printf.printf "Client replied to challenge !!"; print_newline (); *)
          c.client_brand <- Brand_mldonkey3;
          could_be_challenge := true;
        end;
      
      if not challenge.challenge_ok  then begin
          could_be_challenge := true;
          DonkeyProtoCom.direct_client_send sock (
            let module M = DonkeyProtoClient in
            let module C = M.QueryFile in
            M.QueryFileReq (solve_challenge t));
          challenge.challenge_ok <- true;
        end;
      
      (try if not !could_be_challenge then
            client_wants_file c t with _ -> ());
      if t = Md4.null && c.client_brand = Brand_edonkey then  begin
          c.client_brand <- Brand_mldonkey1;
          if Random.int 100 < 2 then
            direct_client_send sock (
              M.SayReq "[WARNING] Please, Update Your MLdonkey client to version 2.01");
        end;
      
      if not c.client_already_counted then begin
          count_seen c;
          c.client_already_counted <- true
        end;
      begin try 	
          count_filerequest c;
          let file = find_file t in
          (match file.file_shared with
              None -> ()
            | Some impl ->
                shared_must_update_downloaded (as_shared impl);
                impl.impl_shared_requests <- impl.impl_shared_requests + 1);
          request_for c file sock;
          direct_client_send sock (
            let module Q = M.QueryFileReply in
            let filename = file_best_name file in
            let published_filename = if String.length filename < 7 ||
                String.sub filename 0 7 <> "hidden." then filename
              else String.sub filename 7 (String.length filename - 7) in
            M.QueryFileReplyReq {
              Q.md4 = file.file_md4;
              Q.name = published_filename
            });
          
          DonkeySources1.add_file_location file c
        with _ -> () end
  
  
  | M.EmuleRequestSourcesReplyReq t ->
(*      Printf.printf "Emule sent sources"; print_newline (); *)
      let module Q = M.EmuleRequestSourcesReply in
      begin
        try
          let file = find_file t.Q.md4 in
(* Always accept sources when already received !
  
          if file.file_enough_sources then begin
              Printf.printf "** Dropped %d sources for %s **" (List.length t.Q.sources) (file_best_name file);
              print_newline ()
            end else *)
          Array.iter (fun s ->
              if Ip.valid s.Q.ip && Ip.reachable s.Q.ip then
                ignore (DonkeySources1.new_source (s.Q.ip, s.Q.port) file)
              else
                begin
                  let module C = DonkeyProtoServer.QueryCallUdp in
                  DonkeyProtoCom.udp_send (get_udp_sock ())
                  s.Q.server_ip (s.Q.server_port+4)
                    (DonkeyProtoServer.QueryCallUdpReq {
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
              Printf.printf "** Dropped %d sources for %s **" (List.length t.Q.sources) (file_best_name file);
              print_newline ()
            end else *)
          List.iter (fun (ip1, port, ip2) ->
              if Ip.valid ip1 && Ip.reachable ip1 then
                ignore (DonkeySources1.new_source (ip1, port) file)
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
      |	Indirect_location _ -> None
      in
      (* A VOIR : historique à gérer *) 
(*      !say_hook c s *)
      private_message_from (as_client c.client_client)  s
      
  | M.QueryChunkMd4Req t when !has_upload = 0 -> 

      let file = find_file t in
      begin
        match file.file_md4s with
          [] -> () (* should not happen *)
        | md4s ->
            direct_client_send sock (
              let module Q = M.QueryChunkMd4Reply in
              M.QueryChunkMd4ReplyReq {
                Q.md4 = file.file_md4;
                Q.chunks = Array.of_list md4s
              })
      
      end
  
  | M.QueryChunksReq t when !has_upload = 0 && not 
      (!!ban_queue_jumpers && c.client_banned) ->

      let file = find_file t in
      direct_client_send sock (
        let module Q = M.QueryChunksReply in
        M.QueryChunksReplyReq {
          Q.md4 = file.file_md4;
          Q.chunks = Array.map (fun state ->
              match state with
                PresentVerified -> true 
              | _ -> false
          ) file.file_chunks;
        })
  
  | M.QueryBlocReq t when !has_upload = 0 && c.client_has_a_slot ->
      if !!verbose then begin
          Printf.printf "uploader %s(%s) ask for block" c.client_name
            (brand_to_string c.client_brand); print_newline ();
        end;
  
      set_rtimeout sock !!upload_timeout;
      let module Q = M.QueryBloc in
      let file = find_file  t.Q.md4 in

      let up, waiting = match c.client_upload with
          Some ({ up_file = f } as up) when f == file ->  up, up.up_waiting
        | Some old_up ->
            {
              up_file = file;
              up_pos = Int32.zero;
              up_end_chunk = Int32.zero;
              up_chunks = [];
              up_waiting = old_up.up_waiting;
            }, old_up.up_waiting
        | _ ->
            {
              up_file = file;
              up_pos = Int32.zero;
              up_end_chunk = Int32.zero;
              up_chunks = [];
              up_waiting = false;
            }, false
      in
      new_chunk up t.Q.start_pos1 t.Q.end_pos1;
      new_chunk up t.Q.start_pos2 t.Q.end_pos2;
      new_chunk up t.Q.start_pos3 t.Q.end_pos3;
      c.client_upload <- Some up;
      if not waiting && !has_upload = 0 then begin
          Fifo.put upload_clients c;
          up.up_waiting <- true
        end

  | _ -> ()
      
let client_handler c sock event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      disconnect_client c;

      (*
      if c.client_name <> "" then begin
          Printf.printf "client %s(%s) disconnected: reason %s"
            c.client_name (brand_to_string c.client_brand) s;
          print_newline ();
        end
*)
      
  | _ -> ()

let client_handler2 c sock event = 
  match !c with
    Some c -> client_handler c sock event
  | None ->
      match event with
        BASIC_EVENT (CLOSED s) ->
          printf_string "-c";
      | _ -> ()
      
let init_connection sock =
(*  ignore (setsock_iptos_throughput (fd (TcpBufferedSocket.sock sock))); *)
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  set_rtimeout sock !!client_timeout;
  set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
      printf_string "[TO?]";
      close s "timeout"
  )

let init_client sock c =
  set_handler sock WRITE_DONE (fun s ->
      match c.client_upload with
        None -> ()
      | Some up ->
          if not up.up_waiting && !has_upload = 0 then begin
              up.up_waiting <- true;
              Fifo.put upload_clients c
            end
  );
  set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
      connection_delay c.client_connection_control;
      printf_string "-!C";
      close s "timeout"
  );
  c.client_block <- None;
  c.client_zones <- [];
  c.client_file_queue <- [];
  c.client_has_a_slot <- false;
  c.client_upload <- None

  
        
let read_first_message overnet m sock =
  let module M = DonkeyProtoClient in
  match m with

  | M.ConnectReq t ->
      printf_string "******* [PCONN OK] ********";
      
      let module CR = M.Connect in

      
      if t.CR.md4 = !!client_md4 ||
         t.CR.md4 = overnet_md4 then begin
          TcpBufferedSocket.close sock "connected to myself";
          raise End_of_file
        end;
      
      let name = ref "" in
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> name := s
          | _ ->  ()
      ) t.CR.tags;

      let kind = Indirect_location (!name,t.CR.md4) in
      let c = new_client kind in
      
      begin
        match c.client_sock with
          None -> 
            c.client_sock <- Some sock;
            c.client_connected <- false;
            init_client sock c
        | Some _ -> 
            close sock "already connected"; raise Not_found
      end;

      
      set_write_power sock c.client_power;
      set_read_power sock c.client_power;

      set_client_name c !name t.CR.md4;
      connection_try c.client_connection_control;
      connection_ok c.client_connection_control;
      c.client_tags <- t.CR.tags;
      
      begin
        match t.CR.server_info with
          Some (ip, port) when !!update_server_list -> 
            safe_add_server ip port
        | None -> 
            if overnet then begin
                Printf.printf "incoming Overnet client"; print_newline ();
                c.client_overnet <- overnet;            
              end
        | _ -> ()
      end;

      (*      
      Printf.printf "Message from client %s(%s)"
      c.client_name (brand_to_string c.client_brand);
      print_newline ();
      M.print m;
      print_newline ();
*)
      
(*      List.iter (fun s ->
		   match s.server_sock with
		       None -> ()
		     | Some _ -> if ?? = s.server_ip then
			 c.client_brand <- Brand_server
) (connected_servers ()) *)
      identify_client_brand c;

      direct_client_send sock (
        let module M = DonkeyProtoClient in
        let module C = M.ConnectReply in
	if c.client_overnet then
          M.ConnectReplyReq {
            C.md4 = overnet_md4;
            C.ip = client_ip (Some sock);
            C.port = !overnet_client_port;
            C.tags = !overnet_client_tags;
            C.server_info = None;
          }
	else
          M.ConnectReplyReq {
            C.md4 = !!client_md4;
            C.ip = client_ip (Some sock);
            C.port = !client_port;
            C.tags = !client_tags;
            C.server_info = t.CR.server_info;
          }
      );

            
      if c.client_brand = Brand_newemule then  begin
(*          Printf.printf "Emule Extended Protocol query"; print_newline (); *)
          let module E = M.EmuleClientInfo in
          emule_send sock (M.EmuleClientInfoReq {
              E.version = 0x24; 
              E.protversion = 0x1;
              E.tags = [
(*                int_tag "compression" 0; *)
                int_tag "udp_port" (!!port+4)
              ]
            })
        end;

      
      set_client_state c Connected_idle;      
      query_files c sock; 

      if client_type c <> NormalClient then
        if last_time () > c.client_next_view_files then begin
            (*
            Printf.printf "****************************************";
            print_newline ();
            Printf.printf "       ASK VIEW FILES         ";
            print_newline ();
*)
            
            direct_client_send sock (
              let module M = DonkeyProtoClient in
              let module C = M.ViewFiles in
              M.ViewFilesReq C.t);          
            end;
      client_must_update c;
      is_banned c sock;
      Some c
      
  | M.NewUserIDReq _ ->
      M.print m; print_newline ();
      None
      
  | _ -> 
      Printf.printf "BAD MESSAGE FROM CONNECTING CLIENT"; print_newline ();
      M.print m; print_newline ();
      close sock "bad connecting message";
      raise Not_found

  
let reconnect_client c =
(*  Printf.printf "CLIENT: reconnect_client"; print_newline (); *)
  if can_open_connection () then
    match c.client_kind with
      Indirect_location _ -> ()
    | Known_location (ip, port) ->
(*        Printf.printf "CLIENT: black listed ?"; print_newline ();  *)
        if client_state c <> BlackListedHost then
          if is_black_address ip port then
            set_client_state c BlackListedHost
          else
          try
(*            Printf.printf "CLIENT: connecting"; print_newline (); *)
            set_client_state c Connecting;
            connection_try c.client_connection_control;

            
            c.client_score <- Client_not_connected;
            
            printf_string "?C";
            let sock = TcpBufferedSocket.connect "donkey to client" (
                Ip.to_inet_addr ip) 
              port 
                (client_handler c) (*client_msg_to_string*) in
            TcpBufferedSocket.set_write_power sock c.client_power;
            TcpBufferedSocket.set_read_power sock c.client_power;
            init_connection sock;
            init_client sock c;
            
            let challenge = {
                challenge_md4 = Md4.null;
                challenge_ok = false;
              } in
            set_reader sock (DonkeyProtoCom.cut_messages DonkeyProtoClient.parse
                (client_to_client challenge files c));
            
            c.client_sock <- Some sock;
            c.client_connected <- true;
            let server_ip, server_port = 
              try
                let s = DonkeyGlobals.last_connected_server () in
                s.server_ip, s.server_port
              with _ -> Ip.localhost, 4665
            in
            
            direct_client_send sock (
              let module M = DonkeyProtoClient in
              let module C = M.Connect in
	      if c.client_overnet then
                M.ConnectReq {
                  C.md4 = overnet_md4;
                  C.ip = client_ip None;
                  C.port = !overnet_client_port;
                  C.tags = !overnet_client_tags;
                  C.version = 16;
                  C.server_info = None;
                }
	      else
                M.ConnectReq {
                  C.md4 = !!client_md4;
                  C.ip = client_ip None;
                  C.port = !client_port;
                  C.tags = !client_tags;
                  C.version = 16;
                  C.server_info = Some (server_ip, server_port);
                }
          )

        with e -> 
            Printf.printf "Exception %s in client connection"
              (Printexc2.to_string e);
            print_newline ();
            connection_failed c.client_connection_control;
            set_client_state c NotConnected

let query_id s sock ip file =
  printf_string "[QUERY ID]";
  direct_server_send sock (
    let module M = DonkeyProtoServer in
    let module C = M.QueryID in
    M.QueryIDReq ip
  );
  Fifo.put s.server_id_requests file

let udp_server_send s t =
  DonkeyProtoCom.udp_send (get_udp_sock ())
  s.server_ip (s.server_port+4)
  t
  
let query_locations_reply s t =
  let module M = DonkeyProtoServer in
  let module Q = M.QueryLocationReply in
  
  connection_ok s.server_connection_control;
  
  try
    let file = find_file t.Q.md4 in
    let nlocs = List.length t.Q.locs in
    s.server_score <- s.server_score + 3;

(* Is this a joke ? ok, when we have enough sources, don't ask for more.
But if we receive them, take them !
  
    if file.file_enough_sources then begin
        Printf.printf "** Dropped %d sources for %s **" nlocs
          (file_best_name file);
        print_newline ()
      end else 
*)
    List.iter (fun l ->
        let ip = l.Q.ip in
        let port = l.Q.port in
        
        if Ip.valid ip  && Ip.reachable ip  then
          ignore (DonkeySources1.new_source (ip, port) file)
        else
        match s.server_sock with
          None ->
            let module Q = M.QueryCallUdp in
            udp_server_send s 
              (M.QueryCallUdpReq {
                Q.ip = client_ip None;
                Q.port = !client_port;
                Q.id = ip;
              })
        
        | Some sock ->
            printf_string "QUERY ID";
            query_id s sock ip (Some file)
    ) t.Q.locs
  
  with Not_found -> ()
      
let client_connection_handler overnet t event =
  printf_string "[REMOTE CONN]";
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

(*
      if can_open_connection () then
begin
*)
      (try
          let c = ref None in
          let sock = 
            TcpBufferedSocket.create "donkey client connection" s 
              (client_handler2 c) 
(*client_msg_to_string*)
          in
          init_connection sock;
          
          (try
              
              let challenge = {
                  challenge_md4 = Md4.null;
                  challenge_ok = false;
                } in
              set_reader sock 
                (DonkeyProtoCom.client_handler2 c (read_first_message overnet)
                  (client_to_client challenge []));
            
            with e -> Printf.printf "Exception %s in init_connection"
                  (Printexc2.to_string e);
                print_newline ());
        with e ->
            Printf.printf "Exception %s in client_connection_handler"
              (Printexc2.to_string e);
            print_newline ();
            Unix.close s)
(*
end     

      else begin
          Printf.printf "***** CONNECTION PREVENTED by limitations *****";
          print_newline ();
          Unix.close s
end;
  *)
  | _ -> 
      ()      
      
(* This function should be called every second *)
      
let all_sources = Fifo.create ()
let nsources = ref 0
  
let check_sources () =
  
(* first of all, do we have new available slots in the sources *)
  let nmax_sources = 
    (int_of_float !!min_reask_delay) * !!max_clients_per_second in
  ()
  
  