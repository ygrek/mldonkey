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

open CommonSources
open CommonDownloads  
open CommonRoom
open CommonShared
open CommonGlobals
open CommonFile
open CommonClient
open CommonComplexOptions
  
open GuiTypes
open GuiProto
open CommonResult
open CommonTypes
open Options
open BasicSocket
open DonkeyMftp
open DonkeyProtoCom
open TcpBufferedSocket
open DonkeyOptions
open CommonOptions
open DonkeyComplexOptions
open DonkeyGlobals
open DonkeyStats
open DonkeyTypes  
open DonkeyReliability
open DonkeyThieves


module Udp = DonkeyProtoUdp

(* Lifetime of a socket after sending interesting messages *)
let active_lifetime = 1200.
  
let is_banned c sock = 
  c.client_banned <- Hashtbl.mem banned_ips (peer_ip sock)


(* Supports Emule Extended Protocol *)
let supports_eep cb = 
  match cb with
    Brand_lmule | Brand_newemule | Brand_cdonkey | 
    Brand_mldonkey3 | Brand_shareaza | Brand_amule | Brand_lphant -> true
  | _ -> false

let ban_client c sock msg = 
    let ip = peer_ip sock in
  if not (Hashtbl.mem banned_ips ip) then
    let module M = DonkeyProtoClient in
    
    if !verbose then begin
        lprintf "client %s(%s) %s, it has been banned\n" msg
          c.client_name (brand_to_string c.client_brand);
      end;
    
    count_banned c;
    c.client_banned <- true;
    Hashtbl.add banned_ips ip (last_time ());
    
    if !!send_warning_messages then
      client_send c ( M.SayReq  (
          Printf.sprintf 
            "[AUTOMATED ERROR] Your client %s has been banned" msg))
  
let corruption_warning c =
  if !!send_warning_messages then
    let module M = DonkeyProtoClient in
    client_send c (
      M.SayReq "
[AUTOMATED WARNING] It has been detected that your client
is sending corrupted data. Please double-check your hardware
(disk, memory, cpu) and software (latest version ?)")
    
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
            client_send c ( M.SayReq  (
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
    do_if_connected  c.client_source.DonkeySources.source_sock (fun sock ->
        set_rtimeout sock !!upload_timeout;
        client_send c (
          let module M = DonkeyProtoClient in
          let module Q = M.AvailableSlot in
          M.AvailableSlotReq Q.t);
        
        if !verbose then begin
            lprintf "New uploader %s: brand %s\n" 
              c.client_name (brand_to_string c.client_brand);
          end;
        
    )  
  in
  client_ops.op_client_enter_upload_queue <- client_enter_upload_queue
   
let string_of_client c =
  Printf.sprintf "client[%d] %s(%s) %s" (client_num c)
  c.client_name (brand_to_string c.client_brand)
  (match c.client_kind with
      Indirect_address _ | Invalid_address _ -> ""
    | Direct_address (ip,port) ->
        Printf.sprintf  " [%s:%d]" (Ip.to_string ip) port;
  )

let log_client_info c sock = 
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
  (match c.client_kind with Indirect_address _ | Invalid_address _ -> "LowID"
    | Direct_address (ip,port) -> Printf.sprintf "%s:%d"
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
      Printf.bprintf  buf "(%d)"  r.DonkeySources.request_score;
  ) c.client_source.DonkeySources.source_files;      
  Buffer.add_char buf '\n';
  let m = Buffer.contents buf in
  CommonEvent.add_event (Console_message_event m)
  
let disconnect_client c reason =
  match c.client_source.DonkeySources.source_sock with
    NoConnection -> ()
  | ConnectionWaiting token ->
      cancel_token token;
      c.client_source.DonkeySources.source_sock <- NoConnection
  | Connection sock ->
      (try
          c.client_comp <- None;
          if c.client_debug then begin
            lprintf "Client[%d]: disconnected" (client_num c);
           CommonGlobals.print_localtime ();
          end;
          (try if c.client_checked then count_seen c with _ -> ());
          (try if !!log_clients_on_console && c.client_name <> "" then 
                log_client_info c sock with _ -> ());
          
          (*
          if c.client_connect_time > 0 then begin
              lprintf "Disconnected %s\n" (string_of_client c);
              lprintf "Client connected for %d seconds, %d requests issued\n"
                (last_time () - c.client_connect_time)
              c.client_requests_sent
            end;
*)
          
          c.client_connect_time <- 0;
          (try Hashtbl.remove connected_clients c.client_md4 with _ -> ());
          (try CommonUploads.remove_pending_slot (as_client c) with _ -> ());
          set_client_has_a_slot (as_client c) false;
(*          connection_failed c.client_connection_control; *)
          (try TcpBufferedSocket.close sock reason with _ -> ());

(* Remove the Connected and NoLimit tags *)
          set_client_type c (client_type c
              land (lnot (client_initialized_tag lor client_nolimit_tag)));
(*          c.client_chunks <- [||];*)
          c.client_source.DonkeySources.source_sock <- NoConnection;
          save_join_queue c;
          c.client_slot <- SlotNotAsked;
          let files = c.client_file_queue in
          
          (try DonkeyOneFile.clean_current_download c with _ -> ());

          List.iter (fun (file, chunks, up) -> 
              try Int64Swarmer.unregister_uploader up with _ -> ()
          )
          files;    
          c.client_file_queue <- [];  
          if c.client_upload != None then CommonUploads.refill_upload_slots ();
        
        with e -> lprintf "Exception %s in disconnect_client\n"
              (Printexc2.to_string e));
(*      lprintf "Client %d to source:" (client_num c);
      List.iter (fun r ->
                lprint_char (
                  match r.request_result with
                  | File_chunk ->      'C'
                  | File_upload ->     'U'
                  | File_not_found ->  '-'
                  | File_found ->      '+'
                  | File_possible ->   '?'
                  | File_expected ->   '!'
                  | File_new_source -> 'n'
                )) c.client_files;      
      lprintf "\n"; *)
      set_client_disconnected c reason;
      DonkeySources.source_disconnected c.client_source
  
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
    Indirect_address _ | Invalid_address _ -> ()
  | Direct_address (ip, port) ->
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
              do_if_connected c.client_source.DonkeySources.source_sock (fun sock ->
(* send the list of members of the group to the client *)
                  let list = ref [] in
                  UdpClientMap.iter (fun _ uc ->
                      match ip_reliability uc.udp_client_ip with
                        Reliability_reliable | Reliability_neutral ->
                          list := (uc.udp_client_ip, uc.udp_client_port, uc.udp_client_ip) :: !list
                      | Reliability_suspicious _ -> ()
                  ) group.group;
                  if !list <> [] then begin
                      if !verbose_sources then begin
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
              )
            end;
          
          match c.client_kind with 
            Indirect_address _ | Invalid_address _ -> ()
          | Direct_address (ip, port) ->
(* send this client as a source for the file to
		     all mldonkey clients in the group. add client to group *)
              
              UdpClientMap.iter (fun _ uc ->
                  if uc.udp_client_can_receive then begin
                      if !verbose_sources then
                        lprintf "Send new source to file groups UDP peers\n"; 
                      udp_client_send uc (
                        Udp.QueryLocationReplyUdpReq (
                          let module Q = DonkeyProtoServer.QueryLocationReply in
                          [{
                            Q.md4 = md4;
                            Q.locs = [{ Q.ip = ip; Q.port = port }];
                          }]))
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
     (* if List.exists (fun s -> Ip.equal c.client_ip s.server_ip) (connected_servers ()) then
       Brand_server
      else *) if md4.[5] = Char.chr 14 && md4.[14] = Char.chr 111 then
       Brand_newemule
      else if md4.[5] = 'M' && md4.[14] = 'L' then
        Brand_mldonkey2
    else
      if DonkeySources.source_brand c.client_source then
        Brand_overnet else Brand_edonkey)

let mod_array =
  [|
    ("extasy", Brand_mod_extasy);
    ("hunter", Brand_mod_hunter);
    ("mortimer", Brand_mod_mortimer);
    ("sivka", Brand_mod_sivka);
    ("plus", Brand_mod_plus);
    ("lsd", Brand_mod_lsd);
    ("maella", Brand_mod_maella);
    ("pille", Brand_mod_pille);
    ("morphkad", Brand_mod_morphkad);
    ("ef-mod", Brand_mod_efmod);
    ("xtreme", Brand_mod_xtreme);
    ("bionic", Brand_mod_bionic);
    ("pawcio", Brand_mod_pawcio);
    ("gammaoh", Brand_mod_gammaoh);
    ("zzul", Brand_mod_zzul);
    ("black hand", Brand_mod_blackhand);
    ("lovelace", Brand_mod_lovelace);
    ("morphnext", Brand_mod_morphnext);
    ("fincan", Brand_mod_fincan);
    ("ewombat", Brand_mod_ewombat);
    ("mortillo", Brand_mod_mortillo);
    ("emulespa\241a", Brand_mod_emulespana);
    ("blackrat", Brand_mod_blackrat);
    ("enkeydev", Brand_mod_enkeydev);
    ("gnaddelwarz", Brand_mod_gnaddelwarz);
    ("phoenix-kad", Brand_mod_phoenixkad);
    ("phoenix", Brand_mod_phoenix);
    ("koizo", Brand_mod_koizo);
    ("ed2kfiles", Brand_mod_ed2kfiles);
    ("athlazan", Brand_mod_athlazan);
    ("goldi cryptum", Brand_mod_goldicryptum);
    ("cryptum", Brand_mod_cryptum);
    ("lamerzchoice", Brand_mod_lamerzchoice);
    ("notdead", Brand_mod_notdead);
    ("peace", Brand_mod_peace);
    ("eastshare", Brand_mod_eastshare);
    ("[mfck]", Brand_mod_mfck);
    ("echanblard", Brand_mod_echanblard);
    ("sp4rk", Brand_mod_sp4rk);
    ("bloodymad", Brand_mod_bloodymad);
    ("roman2k", Brand_mod_roman2k);
    ("elfenwombat", Brand_mod_elfenwombat);
    ("o\178", Brand_mod_o2);
    ("sf-iom", Brand_mod_sfiom);
    ("magic-elseve", Brand_mod_magic_elseve);
    ("schlumpmule", Brand_mod_schlumpmule);
    ("noamson", Brand_mod_noamson);
    ("stormit", Brand_mod_stormit);
    ("omax", Brand_mod_omax);
    ("spiders", Brand_mod_spiders);
    ("ib\233rica", Brand_mod_iberica);
    ("stonehenge", Brand_mod_stonehenge);
    ("mison", Brand_mod_mison);
    ("xlillo", Brand_mod_xlillo);
    ("imperator", Brand_mod_imperator);
    ("raziboom", Brand_mod_raziboom);
    ("khaos", Brand_mod_khaos);
    ("hardmule", Brand_mod_hardmule);
    ("sc", Brand_mod_sc);
    ("cy4n1d", Brand_mod_cy4n1d);
    ("dmx", Brand_mod_dmx);
    ("ketamine", Brand_mod_ketamine);
    ("blackmule", Brand_mod_blackmule);
    ("morphxt", Brand_mod_morphxt);
    ("ngdonkey", Brand_mod_ngdonkey);
    ("morph", Brand_mod_morph);
    ("dm", Brand_mod_dm);
    ("lc", Brand_mod_lc);
    ("lh", Brand_mod_lh);
    ("ice", Brand_mod_ice);
    ("cyrex", Brand_mod_cyrex)
  |]
  
let to_lowercase s = String.lowercase s

let string_of_tags_list tags =
  let s = ref "" in
  List.iter (fun tag ->
    let st = to_lowercase (CommonTypes.string_of_tag tag.tag_value) in
    let str = tag.tag_name ^ " : " ^ st ^ " ; " in
    s := !s ^ str
  ) tags;
  !s 

let identify_client_mod_brand c tags =

  if c.client_mod_brand = Brand_mod_unknown then begin
      List.iter (fun tag ->
        let s = to_lowercase (CommonTypes.string_of_tag tag.tag_value) in 
          match tag.tag_name with
           "mod_version" ->
               begin
               let rec iter i len =
                 let sub = fst mod_array.(i) in
                     if  (String2.subcontains s sub) then
                        c.client_mod_brand <- snd mod_array.(i)
                     else iter (i+1) len
               in
               iter 0 (Array.length mod_array)
               end

           | _ -> ()

   ) tags;
   if String2.subcontains c.client_name "@PowerMule" then begin
     c.client_mod_brand <- Brand_mod_powermule
   end
  end

let update_client_from_tags c tags =
  List.iter (fun tag ->
      match tag.tag_name with
      | "name" -> ()
      | "version" -> ()
      | "emule_udpports" -> ()
      | "emule_miscoptions1" ->
          for_int64_tag tag (fun i ->
              DonkeyProtoClient.update_emule_proto_from_miscoptions1 
              c.client_emule_proto i)
      | "emule_version" ->
          for_int_tag tag (fun i ->
              c.client_emule_proto.emule_version <- i)
      | _ -> ()
  ) tags
    
let update_emule_proto_from_tags c tags = 
  List.iter (fun tag -> 
      match tag.tag_name with
        "compatableclient" -> 
          for_int_tag tag (fun i ->
              match i with 
                1 -> c.client_brand <- Brand_cdonkey
              | 2 -> c.client_brand <- Brand_lmule
              | 3 -> c.client_brand <- Brand_amule
              | 4 -> c.client_brand <- Brand_shareaza
              | 10 -> c.client_brand <- Brand_mldonkey3
              | 20 -> c.client_brand <- Brand_lphant
              | _ -> ()
          )          
      | "compression" ->
          for_int_tag tag (fun i -> 
              c.client_emule_proto.emule_compression <- i)
      | "udpver" ->
          for_int_tag tag (fun i -> 
              c.client_emule_proto.emule_udpver <- i)          
      | "udpport" -> ()
      | "sourceexchange" ->
          for_int_tag tag (fun i -> 
              c.client_emule_proto.emule_sourceexchange <- i)          
      | "comments" ->
          for_int_tag tag (fun i -> 
              c.client_emule_proto.emule_comments <- i)          
      | "extendedrequest" ->
          for_int_tag tag (fun i -> 
              c.client_emule_proto.emule_extendedrequest <- i)          
      | "features" ->
          for_int_tag tag (fun i -> 
              c.client_emule_proto.emule_secident <- i land 0x3)          
      
      | s -> 
          if !verbose_msg_clients then
            lprintf "Unknown Emule tag: [%s]\n" (String.escaped s)
  ) tags

let query_id ip port id =
  let client_ip = client_ip None in

(* TODO: check if we are connected to this server. If yes, issue a 
  query_id instead of a UDP packet *)
  if ip_reachable client_ip then
    let module Q = DonkeyProtoUdp.QueryCallUdp in
(*    lprintf "Ask connection from indirect client\n"; *)
    
    let s = new_server ip port 0 in
    match s.server_sock with 
      NoConnection | ConnectionWaiting _ ->
        
        DonkeyProtoCom.udp_send (get_udp_sock ())
        ip (port+4)
        (DonkeyProtoUdp.QueryCallUdpReq {
            Q.ip = client_ip;
            Q.port = !client_port;
            Q.id = id;
          })
    | Connection sock ->
        printf_string "[QUERY ID]";
        server_send sock (
          let module M = DonkeyProtoServer in
          let module C = M.QueryID in
          M.QueryIDReq id
        );
(*                   Fifo.put s.server_id_requests file *)
        ()
        
        
let query_files c sock =  
  
  let s = c.client_source in
  if s.DonkeySources.source_files = [] then begin
(*      lprintf "Unknown Incoming clients\n"; *)
      List.iter (fun file ->
          if file_state file = FileDownloading then begin
              DonkeySources.set_request_result s file.file_sources
                File_possible
            end
      ) !current_files;
    end (* else
    lprintf "Client has %d requests to emit\n" 
      (List.length s.DonkeySources.source_files); *)
    
  (*
  let nall_queries = ref 0 in
  let nqueries = ref 0 in

(*  lprintf "Client %d:\n" (client_num c);  *)
  let files = ref [] in
  (if c.client_source.DonkeySources.source_files = [] then begin
(*        lprintf "  query all files\n";  *)
        files := !current_files 
      
      end else
      List.iter (fun r ->
          if r.request_score > File_not_found then begin
(*              lprintf "   query file %s\n" (file_best_name r.request_file); 
               *)
              files := r.request_file :: !files
            end;
      ) c.client_source.source_files);

(*
  if !files = [] then begin
      lprintf "   No queries to send !\n"; 
    end;
*)
  
  List.iter (fun file ->
      incr nall_queries;
      DonkeySources.query_file c file)
  !files;
  if !nqueries > 0 then
    c.client_last_filereqs <- last_time ();
  if !verbose then begin
      lprintf "sent %d/%d file queries\n" !nqueries !nall_queries;
    end
*)


(* Nice to see some emule devels here... It's always possible to 
crack a protocol, but let's try to make it as boring as possible... *)
    
    external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"
let hash x = hash_param 10 100 x

let shared_of_file file =
  match file.file_shared with
    | None	-> None
    | Some sh	-> Some (as_shared sh)

let query_view_files c =
  if client_browsed_tag land client_type c <> 0 then begin
      if last_time () > c.client_next_view_files then begin
(*
lprintf "****************************************\n";
lprintf "       ASK VIEW FILES         \n";
  *)
          c.client_next_view_files <- last_time () + 3600 * 6;
          client_send c (
            let module M = DonkeyProtoClient in
            let module C = M.ViewFiles in
            M.ViewFilesReq C.t);          
        end
    end

let client_has_file c file =
  let module M = DonkeyProtoClient in
  
  if file_state file = FileDownloading then begin
      
(* ask for more sources *)
      if M.sourceexchange c.client_emule_proto > 0 then        
        let module E = M.EmuleRequestSources in
        client_send c (M.EmuleRequestSourcesReq file.file_md4)
    end
    
let received_client_bitmap c md4 chunks =
  
  let module M = DonkeyProtoClient in
  let file = find_file md4 in
  
  if !verbose_msg_clients then begin
      lprintf "Compared to:\n";
      match file.file_swarmer with
        None -> ()
      | Some swarmer ->
          lprintf "   %s\n" (Int64Swarmer.verified_bitmap swarmer);
    end;
  
  DonkeyNeighbours.new_neighbour c file;          
  DonkeySources.set_request_result c.client_source file.file_sources File_found;
  
  client_has_file c file;
  
  let chunks = 
    if file_size file <= block_size then  [| true |]
    else
    if chunks = [||] then
      Array.create file.file_nchunks true
    else
    if Array.length chunks <> file.file_nchunks then begin
        lprintf "BAD BAD BAD: number of chunks is different %d/%d for %s:%Ld on peer\n" (Array.length chunks) file.file_nchunks (Md4.to_string file.file_md4) (file_size file);
        lprintf "Peer info: name=[%s] md4=[%s] overnet=[%s] brand=[%s]\n" 
          c.client_name
          (Md4.to_string c.client_md4)
        (string_of_bool (DonkeySources.source_brand c.client_source))
        (brand_to_string c.client_brand)
        ;
        Array.create file.file_nchunks false
(* What should we do ?

1) Try to recover the correct size of the file: we can use 
ViewFilesReq on all clients having the file to test what is
the most widely used size for this file. Maybe create 
different instances of the file for each proposed size ?

*)
      
      end else 
      chunks in
  DonkeyOneFile.add_client_chunks c file chunks;
  if file_state file = FileDownloading then begin
      DonkeyOneFile.request_slot c
    end
    
let init_client_connection c sock =
  let module M = DonkeyProtoClient in
  
  if supports_eep c.client_brand then begin
(*    lprintf "Emule Extended Protocol query\n"; *)
      let module E = M.EmuleClientInfo in
      client_send c (M.EmuleClientInfoReq emule_info)
    end

let send_pending_messages c sock =
  let module M = DonkeyProtoClient in
  
  List.iter (fun m ->
      client_send c (M.SayReq m)
  ) c.client_pending_messages;
  c.client_pending_messages <- []
  
let  init_client_after_first_message sock c = 
  
(* Add the Connected tag and when needed the NoLimit tag *)
  let t = client_type c lor client_initialized_tag in
  let t = try
      if Ip.matches (peer_ip sock) !!nolimit_ips then t lor client_nolimit_tag
      else t
    with _ -> t in
  set_client_type c t;
  ()
  

let finish_client_handshake c sock =  
  c.client_connect_time <- last_time ();
  send_pending_messages c sock;
  set_client_state c (Connected (-1));      
  query_files c sock;
  DonkeySources.source_connected c.client_source;  
  query_view_files c;
  client_must_update c;
  is_banned c sock
  
let client_to_client for_files c t sock = 
  let module M = DonkeyProtoClient in
  
  if !verbose_msg_clients || c.client_debug then begin
      lprintf "Message from %s\n" (string_of_client c);
      M.print t;
      lprintf "\n"
    end;
  
  match t with
    M.ConnectReplyReq t ->
      printf_string "******* [CCONN OK] ********"; 

      if DonkeySources.source_brand c.client_source then
        !activity.activity_client_overnet_successful_connections <-
          !activity.activity_client_overnet_successful_connections +1 
      else
        !activity.activity_client_edonkey_successful_connections <-
          !activity.activity_client_edonkey_successful_connections +1 ;
      
      c.client_ip <- peer_ip sock;
      init_client_after_first_message sock c;
      
      c.client_checked <- true;
      set_client_has_a_slot (as_client c) false;
      
      let module CR = M.Connect in
      
      if t.CR.md4 = !!client_md4 ||
        t.CR.md4 = overnet_md4 then
        TcpBufferedSocket.close sock (Closed_for_error "Connected to myself");
      
      if not (register_client_hash (peer_ip sock) t.CR.md4) then
        if !!ban_identity_thieves then
          ban_client c sock "is probably using stolen client hashes";

(* Test if the client is already connected *)
        if Hashtbl.mem connected_clients t.CR.md4 then begin
(*          lprintf "Client is already connected\n";  *)
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

(*      connection_ok c.client_connection_control; *)
      
      begin
        match t.CR.server_info with
          Some (ip, port) when !!update_server_list -> safe_add_server ip port
        | _ -> ()
      end;
      
      update_client_from_tags c t.CR.tags;
      identify_client_brand c;
      
      init_client_connection c sock;

      if not (register_client_hash (peer_ip sock) t.CR.md4) then
        if !!ban_identity_thieves then
          ban_client c sock "is probably using stolen client hashes";
        
      finish_client_handshake c sock
  
  | M.EmuleQueueRankingReq t 
  | M.QueueRankReq t ->
      c.client_rank <- t;
      set_client_state c (Connected t);
(* REMOVE THIS !!!
      if t > 1000 then 
        ban_client c sock "has an infinite queue"; *)
  
  | M.EmuleClientInfoReq t ->      
      
      c.client_ip <- peer_ip sock;
(*      lprintf "Emule Extended Protocol asked\n";  *)
      let module CI = M.EmuleClientInfo in
      update_emule_proto_from_tags c t.CI.tags;
      if !!emule_mods_count then
        identify_client_mod_brand c t.CI.tags;
      
      
      if supports_eep c.client_brand then  begin
          let module E = M.EmuleClientInfo in
          client_send c (M.EmuleClientInfoReplyReq emule_info)
        end
  
  | M.EmuleClientInfoReplyReq t -> 
      
      let module CI = M.EmuleClientInfo in
      
      update_emule_proto_from_tags c t.CI.tags;
      
      if !verbose_msg_clienttags then begin
          lprintf "Message from client[%d] %s(%s)%s\n" (client_num c)
          c.client_name (brand_to_string c.client_brand)
          (match c.client_kind with
              Indirect_address _ | Invalid_address _ -> ""
            | Direct_address (ip,port) ->
                Printf.sprintf  " [%s:%d]" (Ip.to_string ip) port;
          );
          let xx = string_of_tags_list t.CI.tags in
          lprintf "tags: %s\n" xx;
        end

(*   lprintf "Emule Extended Protocol activated\n"; *)
  
  
  | M.EmuleRequestSourcesReq t ->
      let module E = M.EmuleRequestSourcesReply in

(*       lprintf "Emule requested sources\n";  *)
      let file = find_file t in
      let sources = ref [] in
      DonkeySources.iter_all_sources (fun s ->
          match s.DonkeySources.source_uid with
            Indirect_address _ | Invalid_address _ -> () (* not yet supported *)
          | Direct_address (ip, port) ->
              if s.DonkeySources.source_age > last_time () - 600 &&
                (match ip_reliability ip with
                    Reliability_reliable | Reliability_neutral -> true
                  | Reliability_suspicious _ -> false) &&
                List.exists (fun r ->
                    r.DonkeySources.request_score > expected_score
                ) s.DonkeySources.source_files then
                sources := {
                  E.src_ip = ip;
                  E.src_port = port;
                  E.src_server_ip = Ip.null;
                  E.src_server_port = 0;
                  E.src_md4 = Md4.null;
                } :: !sources
      ) file.file_sources;
      if !sources <> [] then        
        client_send c (
          M.EmuleRequestSourcesReplyReq {
            E.md4 = t;
            E.sources = Array.of_list !sources;
          })  
  
  
  
  | M.ViewFilesReplyReq t ->
(*
      lprintf "****************************************\n";
      lprintf "       VIEW FILES REPLY         \n";
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
(*      begin
        match c.client_block with
          None -> ()
        | Some b ->
            lprintf "[QUEUED WITH BLOCK]\n"; 
            DonkeyOneFile.clean_client_zones c;
end; *)
      begin
        match c.client_download with
        | Some (file,up) -> 
            lprintf "DonkeyClient: Clear download\n";
            Int64Swarmer.clear_uploader_ranges up;
            c.client_download <- None
        | None ->
            match c.client_file_queue with
              _ :: _ -> ()
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
                    List.iter (fun (file, chunks) ->
                        let chunks = Array.copy chunks in
                        DonkeyOneFile.add_client_chunks c file chunks) files;
(*                DonkeyOneFile.restart_download c *)
                  with _ -> 
                      lprintf "AvailableSlot received, but not file to download !!\n";
(* TODO: ask for the files now *)
      end;
(* now, we can forget we have asked for a slot *)
      c.client_slot <- SlotReceived;
      DonkeyOneFile.get_from_client c
  
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
                      if c.client_source.source_sock <> None && 
                        c.client_brand = Brand_mldonkey3 then raise Exit)
                  upload_clients
            | _ ->
                if Fifo.length upload_clients >= !!max_upload_slots then
                  raise Exit;
          end;

(*	  set_rtimeout sock !!upload_timeout; *)
          set_lifetime sock one_day;
          add_pending_slot c
        
with _ -> *)
      
      let cc = as_client c in

(* If the client is in the nolimit_ips list, he doesn't need a slot, so put
it immediatly in the upload queue... but what will happen in the queue
since the client upload should not be taken into account ! 

What we need: put the upload and download engines inside the bandwidth
controler, and use two bandwidth controlers, one for limited sockets, the
other one for unlimited sockets.  *)

(* NOT IMPLEMENTED YET
      if is_nolimit cc then begin
          set_client_has_a_slot cc true;
          client_enter_upload_queue cc
        end else *)
      CommonUploads.add_pending_slot (as_client c);
      if !verbose then begin
          lprintf "(uploader %s: brand %s, couldn't get a slot)\n" 
            c.client_name (brand_to_string c.client_brand);
        end;
(*      end *)
  
  | M.CloseSlotReq _ ->
      set_client_state c (Connected 0);
      printf_string "[DOWN]";
      begin
        match c.client_download with
          None -> ()
        | Some (file,up) ->
            lprintf "Slot closed during download\n";
            Int64Swarmer.clear_uploader_ranges up
      end;
(*      DonkeyOneFile.clean_current_download c; *)
      c.client_slot <- SlotNotAsked;
(* OK, the slot is closed, but what should we do now ????? *)
      begin
        match c.client_file_queue with
          [] -> ()
        | _ -> 
            if !verbose_download then begin
                lprintf "CloseSlotReq\n"; 
              end;
            DonkeyOneFile.request_slot c;
            set_rtimeout sock !!queued_timeout;
      end
  
  | M.ReleaseSlotReq _ ->
      set_client_has_a_slot (as_client c) false;
      client_send c (
        let module M = DonkeyProtoClient in
        let module Q = M.CloseSlot in
        M.CloseSlotReq Q.t);
      if c.client_file_queue = [] then
        set_rtimeout sock 120.;
      CommonUploads.refill_upload_slots ()
  
  | M.QueryFileReplyReq t ->
      let module Q = M.QueryFileReply in
      
      begin
        try
          let file = find_file t.Q.md4 in
          let s = Printf.sprintf "[FOUND FILE(%s)]" (match c.client_kind with
                Direct_address _ -> "OUT" | _ -> "IN") in
          printf_string s;
          c.client_rating <- c.client_rating + 1;
          
          DonkeyNeighbours.new_neighbour c file;          
          
          DonkeySources.set_request_result c.client_source 
            file.file_sources File_found;
          
          begin
            let ips = 
              try
                List.assoc t.Q.name file.file_filenames
              with Not_found ->
                  let ips = noips() in
                  file.file_filenames <- file.file_filenames @ [t.Q.name, ips];
                  update_best_name file;
                  ips
            in
            if not (List.mem c.client_ip ips.ips) then begin
                ips.ips <- c.client_ip :: ips.ips;
                ips.nips <- 1 + ips.nips;
              end
          end;
          
          if file_size file <= block_size then begin
              client_has_file c file;
              DonkeyOneFile.add_client_chunks c file [| true |];
              DonkeyOneFile.request_slot c
            end else begin
              
              let know_file_chunks = ref false in
              List.iter (fun (f,_,_) ->
                  if f == file then know_file_chunks := true
              ) c.client_file_queue;
                
              if not !know_file_chunks then
                DonkeyProtoCom.client_send c (
                  let module M = DonkeyProtoClient in
                  M.QueryChunksReq file.file_md4);
              
              if file.file_md4s = [||] then begin
                  client_send c (
                    let module M = DonkeyProtoClient in
                    let module C = M.QueryChunkMd4 in
                    M.QueryChunkMd4Req file.file_md4);
                end;
            end
        with _ -> ()
      end  
  
  | M.QueryChunksReplyReq t ->
      let module Q = M.QueryChunksReply in      
      
      received_client_bitmap c t.Q.md4 t.Q.chunks
        
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
        if file.file_nchunks = 1 then begin
            lprintf "[ERROR]: one chunk file without md4\n"; 
            file.file_md4s <- [|file.file_md4|]
          end else
        if t.Q.chunks = [||] then begin
            lprintf "[ERROR]: empty multiple chunks message\n";
          end
        else
        if Array.length t.Q.chunks <> file.file_nchunks then begin
            lprintf "BAD BAD BAD (2): number of chunks is different %d/%d for %s:%Ld on peer\n" (Array.length t.Q.chunks) file.file_nchunks (Md4.to_string file.file_md4) (file_size file);
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
            
            let md4s = t.Q.chunks in
            let md4 = DonkeyShare.md4_of_array md4s in
            if md4 <> file.file_md4 then begin
                lprintf "[ERROR]: Bad list of MD4s, discarding\n"; 
              end else begin
                file_md4s_to_register := file :: !file_md4s_to_register;
                file.file_md4s <- md4s
              end
          
          
          end
(*      if file.file_exists then verify_chunks file *)
      end
  
  
  | M.EmuleCompressedPart (md4, statpos, newsize, bloc) ->
      
      set_lifetime sock active_lifetime;      
      if !!reliable_sources && 
        client_reliability c = Reliability_suspicious 0 then begin
          lprintf "Receiving data from unreliable client, disconnect\n";
          corruption_warning c;
          disconnect_client c (Closed_for_error "Unreliable Source");
          raise Not_found
        end;
      
      let comp = match c.client_comp with
          None ->
            let comp = {
                comp_md4 = md4;
                comp_pos = statpos;
                comp_total = Int64.to_int newsize;
                comp_len = 0;
                comp_blocs = [];
              } in
            c.client_comp <- Some comp;
            comp
        | Some comp -> comp
      in
      comp.comp_blocs <- bloc :: comp.comp_blocs;
      comp.comp_len <- comp.comp_len + String.length bloc;

(*            lprintf "Comp bloc: %d/%d\n" comp.comp_len comp.comp_total; *)
      if comp.comp_len = comp.comp_total then begin
          lprintf "Complete Compressed bloc received !!!!!!\n";
          
          let s = String.create comp.comp_len in
          let rec iter list =
            match list with
              [] -> 0
            | b :: tail ->
                let pos = iter tail in
                let len = String.length b in
                String.blit b 0 s pos len;
                pos + len
          in
          let pos = iter comp.comp_blocs in
          assert (pos = comp.comp_len);
          if Autoconf.has_zlib then
            let s = Autoconf.zlib__uncompress_string2 s in
            lprintf "Decompressed: %d/%d\n" (String.length s) comp.comp_len;
            
            DonkeyOneFile.block_received c comp.comp_md4
              comp.comp_pos s 0 (String.length s)
          else
            lprintf "ERROR: No Zlib to uncompress packet";
          
          c.client_comp <- None;
        end else
      if comp.comp_len > comp.comp_total then begin
          lprintf "ERROR: more data than compressed!!!\n";
          c.client_comp <- None;
        end
  
  | M.BlocReq t ->
      
      set_lifetime sock active_lifetime;      
      if !!reliable_sources && 
        client_reliability c = Reliability_suspicious 0 then begin
          lprintf "Receiving data from unreliable client, disconnect\n";
          corruption_warning c;
          disconnect_client c (Closed_for_error "Unreliable Source");
          raise Not_found
        end;
      
      let module M = DonkeyProtoClient in  
      let module Q = M.Bloc in
      
      let begin_pos = t.Q.start_pos in
      let end_pos = t.Q.end_pos in
      let len = Int64.sub end_pos begin_pos in
      if Int64.to_int len <> t.Q.bloc_len then begin
          lprintf "%d: inconsistent packet sizes\n" (client_num c);
          raise Not_found
        end;
      
      DonkeyOneFile.block_received c t.Q.md4
        t.Q.start_pos t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len
  
(* Upload requests *)
  | M.ViewFilesReq t when !CommonUploads.has_upload = 0 && 
    (match !!allow_browse_share with
        1 -> client_friend_tag land client_type c <> 0
      | 2 -> true
      | _ -> false) -> 
      let files = DonkeyShare.all_shared () in
      let published_files = ref [] in
      List.iter (fun f ->
          let filename = file_best_name f in
          if not (String2.starts_with filename "hidden.") then
            published_files := f :: !published_files
      ) files;
(*
       lprintf "ASK VIEW FILES\n"; 
       *)
      client_send_files sock !published_files
  
  | M.QueryFileReq t ->
      let md4 = t.M.QueryFile.md4 in
      let emule_extension = t.M.QueryFile.emule_extension in
      c.client_requests_received <- c.client_requests_received + 1;
      
      if  !CommonUploads.has_upload = 0 && 
        not (!!ban_queue_jumpers && c.client_banned) then
        
        (try client_wants_file c md4 with _ -> ());
      
      
      if md4 = Md4.null && c.client_brand = Brand_edonkey then  begin
          c.client_brand <- Brand_mldonkey1;
          if Random.int 100 < 2 && !!send_warning_messages then
            client_send c (
              M.SayReq "[AUTOMATED WARNING] Please, Update Your MLdonkey client to version 2.5-4+2");
        end;
      
      begin try 	
          count_filerequest c;
          let file = find_file md4 in
          (match file.file_shared with
              None -> raise Not_found
            | Some impl ->
                shared_must_update_downloaded (as_shared impl);
                impl.impl_shared_requests <- impl.impl_shared_requests + 1);
          request_for c file sock;
          set_client_upload (as_client c) (shared_of_file file);
          client_send c (
            let module Q = M.QueryFileReply in
            let filename = file_best_name file in
            let published_filename = if String.length filename < 7 ||
                String.sub filename 0 7 <> "hidden." then filename
              else String.sub filename 7 (String.length filename - 7) in
            M.QueryFileReplyReq {
              Q.md4 = file.file_md4;
              Q.name = published_filename
            });
          DonkeySources.query_file c.client_source file.file_sources
        
        with _ -> 
            client_send c (
              M.NoSuchFileReq md4)
      end;
      
      begin
        match t.M.QueryFile.emule_extension with
          None -> ()
        | Some (chunks, _) ->
            received_client_bitmap c md4 chunks
      end
  
  
  | M.EmuleRequestSourcesReplyReq t ->
(*      lprintf "Emule sent sources\n";  *)
      let module Q = M.EmuleRequestSourcesReply in
      begin
        try
          let file = find_file t.Q.md4 in
(* Always accept sources when already received !
  
          if file.file_enough_sources then begin
              lprintf "** Dropped %d sources for %s **\n" (List.length t.Q.sources) (file_best_name file);
              
end else *)
          if !verbose_location then begin
              lprintf "Client: Received %d sources for %s\n" (Array.length t.Q.sources) (file_best_name file);
            end;
          Array.iter (fun s ->
              try
              let addr = 
                  if Ip.valid s.Q.src_ip then
                    if ip_reachable s.Q.src_ip then
                      Direct_address (s.Q.src_ip, s.Q.src_port)
                    else raise Not_found
                  else begin
(*                      lprintf "RIA: Received indirect address\n"; *)
                    Indirect_address (s.Q.src_server_ip, s.Q.src_server_port,
                        id_of_ip s.Q.src_ip)
                    end
                in
                let s = DonkeySources.find_source_by_uid addr in
                DonkeySources.set_request_result s file.file_sources
                  File_new_source
              with Not_found -> ()
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
              lprintf "** Dropped %d sources for %s **\n" (List.length t.Q.sources) (file_best_name file);
              
            end else *)
          List.iter (fun (ip1, port, ip2) ->
              if Ip.valid ip1 && ip_reachable ip1 then
                let s = DonkeySources.find_source_by_uid (Direct_address (ip1, port))
                in
                DonkeySources.set_request_result s file.file_sources
                  File_new_source
          ) t.Q.sources
        with _ -> ()
      end
  
  | M.SayReq s when (!is_not_spam) s ->
(* A VOIR : historique à gérer *)
(*      !say_hook c s *)
      private_message_from (as_client c)  s;
      
      let cip =
        ( 
          try
            
            match c.client_source.DonkeySources.source_sock with
              Connection sock -> Ip.to_string (peer_ip sock)
            | _ -> (match c.client_kind with 
                    Direct_address (ip,port) -> Ip.to_string ip
                  | Indirect_address _ | Invalid_address _ -> "Indirect"
                )
          
          with _ -> 
              
              try 
                match c.client_kind with 
                  Direct_address (ip,port) -> Ip.to_string ip
                | Indirect_address _ | Invalid_address _ -> "Indirect"
              with _ -> ""
        ) 
      in
      log_chat_message cip (client_num c) c.client_name s;
  
  | M.QueryChunkMd4Req t when !CommonUploads.has_upload = 0 -> 
      
      let file = find_file t in
      begin
        match file.file_md4s with
          [||] -> () (* should not happen *)
        | md4s ->
            client_send c (
              let module Q = M.QueryChunkMd4Reply in
              M.QueryChunkMd4ReplyReq {
                Q.md4 = file.file_md4;
                Q.chunks = md4s
              })
      
      end
  
  | M.QueryChunksReq t ->
      c.client_requests_received <- c.client_requests_received + 1;
      
      if  !CommonUploads.has_upload = 0 && not 
          (!!ban_queue_jumpers && c.client_banned) then
        begin
          try
            let file = find_file t in
            match file.file_swarmer with
              None -> failwith "QueryChunksReq: no swarmer for file"
            | Some swarmer ->
                let bitmap = Int64Swarmer.verified_bitmap swarmer in
                let chunks = 
                  Array.init (String.length bitmap) 
                  (fun i -> bitmap.[i] = '3')
                in
                client_send c (
                  let module Q = M.QueryChunksReply in
                  M.QueryChunksReplyReq {
                    Q.md4 = file.file_md4;
                    Q.chunks = chunks;
                  });
                DonkeySources.query_file c.client_source file.file_sources
          with 
          | _ -> ()
        end
        
  | M.QueryBlocReq t when !CommonUploads.has_upload = 0 &&
    client_has_a_slot (as_client c) ->
      
      if !verbose then begin
          lprintf "uploader %s(%s) ask for block\n" c.client_name
            (brand_to_string c.client_brand); 
        end;

      let module Q = M.QueryBloc in
      let file = find_file  t.Q.md4 in
      let prio = (file_priority file) in
      let client_upload_lifetime = ref ((maxi 0 !!upload_lifetime) * 60) in
      begin

        if !!dynamic_upload_lifetime
           && c.client_uploaded > c.client_downloaded
           && c.client_uploaded > Int64.mul (Int64.of_int !!dynamic_upload_threshold) zone_size
        then
          client_upload_lifetime :=
              Int64.to_int 
                (Int64.div
                  (Int64.mul 
                    (Int64.of_int !client_upload_lifetime)
                    c.client_downloaded)
                  c.client_uploaded);
        if last_time() > c.client_connect_time + 
          !client_upload_lifetime + 5 * prio then
          begin
            
(* And what happens if we were downloading from this client also ? *)
            
          disconnect_client c (Closed_for_error "Upload lifetime expired");
          raise Not_found
        end;

      set_lifetime sock active_lifetime;
      set_rtimeout sock !!upload_timeout;
      
      let up, waiting = match c.client_upload with
          Some ({ up_file = f } as up) when f == file ->  up, up.up_waiting
        | Some old_up ->
            {
              up_file = file;
              up_pos = Int64.zero;
              up_end_chunk = Int64.zero;
              up_chunks = [];
              up_waiting = old_up.up_waiting;
            }, old_up.up_waiting
        | _ ->
            {
              up_file = file;
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
      set_client_upload (as_client c) (shared_of_file file);
      if not waiting && !CommonUploads.has_upload = 0 then begin
          CommonUploads.ready_for_upload (as_client c);
          up.up_waiting <- true
        end
    end

  | M.NoSuchFileReq t ->
      begin
        try
          let file = find_file t in
          DonkeySources.set_request_result c.client_source 
            file.file_sources File_not_found;
        with _ -> ()
      end
        
  | _ -> 
      if !verbose_unknown_messages then begin
          lprintf "Unused Client Message:\n"; 
          M.print t;
          lprintf "\n"
        end
      
let client_handler c sock event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      disconnect_client c s;

  | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
      printf_string "[TO?]";
      close sock Closed_for_timeout;

      (*
      if c.client_name <> "" then begin
          lprintf "client %s(%s) disconnected: reason %s\n"
            c.client_name (brand_to_string c.client_brand) s;
        end
*)
      
  | _ -> ()

let client_handler2 c sock event = 
  (match event with
      BASIC_EVENT (CLOSED s) -> decr DonkeySources.indirect_connections
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
      
let init_connection sock ip =
(*  ignore (setsock_iptos_throughput (fd (TcpBufferedSocket.sock sock))); *)
  
  let nolimit = try
      Ip.matches ip !!nolimit_ips
    with _ -> false in
  if not nolimit then begin
      TcpBufferedSocket.set_read_controler sock download_control;
      TcpBufferedSocket.set_write_controler sock upload_control;
    end;
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
              CommonUploads.ready_for_upload (as_client c)
            end
  );
(*
  set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
      connection_delay c.client_connection_control;
      printf_string "-!C";
      close s "timeout"
  ); *)
(*  c.client_block <- None; *)
(*  c.client_zones <- []; *)
  c.client_file_queue <- [];
  set_client_has_a_slot (as_client c) false;
  c.client_upload <- None;
  c.client_rank <- 0;
  c.client_requests_received <- 0;
  c.client_requests_sent <- 0;
  c.client_slot <- SlotNotAsked
        
let read_first_message overnet m sock =
  let module M = DonkeyProtoClient in
  
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
(*          lprintf "Client is already connected\n";  *)
          close sock (Closed_for_error "already connected");
          raise Exit
        end;
      let name = ref "" in
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> name := s
          | _ ->  ()
      ) t.CR.tags;
      
      let kind = if Ip.valid t.CR.ip && Ip.reachable t.CR.ip then
          Direct_address (t.CR.ip, t.CR.port)
        else
        if not (Ip.valid t.CR.ip) then
          match t.CR.server_info with
            None ->  Invalid_address  (!name, Md4.to_string t.CR.md4)
          | Some (ip,port) ->
              if Ip.valid ip && Ip.reachable ip then begin
(*                  lprintf "RIA: Received indirect address\n"; *)
                  Indirect_address (ip, port, id_of_ip t.CR.ip)
              end else Invalid_address  (!name, Md4.to_string t.CR.md4)
        else
          Invalid_address  (!name, Md4.to_string t.CR.md4)
      in
      
      let c = new_client kind in
      
      
      
      if c.client_debug || !verbose_msg_clients then begin  
          lprintf "First Message\n";
          M.print m;
          lprintf "\n";
        end;
      
      Hashtbl.add connected_clients t.CR.md4 c;
      
      update_client_from_tags c t.CR.tags;
      
      begin
        match c.client_source.DonkeySources.source_sock with
        | NoConnection -> 
            c.client_source.DonkeySources.source_sock <- Connection sock;
            c.client_ip <- peer_ip sock;
            c.client_connected <- false;
            init_client sock c;
            init_client_after_first_message sock c
        
        | ConnectionWaiting token -> 
            cancel_token token;
            c.client_source.DonkeySources.source_sock <- Connection sock;
            c.client_ip <- peer_ip sock;
            c.client_connected <- false;
            init_client sock c;
            init_client_after_first_message sock c
        
        | _ -> 
            close sock (Closed_for_error "already connected");
            raise Not_found
      end;

(*
      begin
        match c.client_source, kind with
          None, Direct_address (ip, port) ->
            let s = DonkeySources.create_source 0 (last_time ()) (ip, port) in
            c.client_source <- Some s;
            c.client_files := !(s.source_files);
        | _ -> 
            c.client_indirect_address <- indirect;
      end;
*)
      
      c.client_checked <- true;
      
      set_client_name c !name t.CR.md4;
(*      connection_try c.client_connection_control;
      connection_ok c.client_connection_control; *)
      c.client_tags <- t.CR.tags;
      
      if not (register_client_hash (peer_ip sock) t.CR.md4) &&
        !!ban_identity_thieves then
        ban_client c sock "is probably using stolen client hashes";
      
      List.iter (fun ban ->
          if String2.subcontains c.client_name ban then begin
              client_send c (
                M.SayReq "[AUTOMATED WARNING] Sorry, you have not understood P2P");
              set_client_state c BlackListedHost;
              lprintf "Client[%d]: banned (%s)\n" (client_num c) ban;
              raise Not_found
            end)
      ["Mison"]; (* People who don't understand P2P themselves please leave this list alone *)
      
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
                DonkeySources.set_source_brand c.client_source overnet;
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
      init_client_connection c sock;
      
      client_send c (
        let module M = DonkeyProtoClient in
        let module C = M.Connect in
        if DonkeySources.source_brand c.client_source then
          M.ConnectReplyReq {
            C.md4 = overnet_md4;
            C.ip = client_ip (Some sock);
            C.port = !overnet_client_port;
            C.tags = !overnet_connectreply_tags;
            C.server_info = Some (!overnet_server_ip, !overnet_server_port);
            C.left_bytes = left_bytes;
            C.version = -1;
          }
        else
          begin
(*	 if (c.client_brand == Brand_server) then begin
          M.ConnectReplyReq {
            C.md4 = !!server_client_md4;
            C.ip = client_ip (Some sock);
            C.port = !client_port;
            C.tags = !client_tags;
            C.server_info = t.CR.server_info;
            C.left_bytes = left_bytes; 
          }
	 end
	 else
	 begin *)
            M.ConnectReplyReq {
              C.md4 = !!client_md4;
              C.ip = client_ip (Some sock);
              C.port = !client_port;
              C.tags = !client_to_client_tags;
              C.server_info = t.CR.server_info;
              C.left_bytes = left_bytes; 
              C.version = -1;
            }
          end;
      );
      
      
      
      
      if DonkeySources.source_brand c.client_source then
        !activity.activity_client_overnet_indirect_connections <-
          !activity.activity_client_overnet_indirect_connections +1 
      else
        !activity.activity_client_edonkey_indirect_connections <-
          !activity.activity_client_edonkey_indirect_connections +1 ;
      
      if not (register_client_hash (peer_ip sock) t.CR.md4) then
        (if !!ban_identity_thieves then
            ban_client c sock "is probably using stolen client hashes");
      
      finish_client_handshake c sock;
      Some c
      
  | M.NewUserIDReq _ ->
      M.print m; lprintf "\n";
      None
  
  | _ -> 
      lprintf "BAD MESSAGE FROM CONNECTING CLIENT\n"; 
      M.print m; lprintf "\n";
      close sock (Closed_for_error "bad connecting message");
      raise Not_found
      
      
let reconnect_client c =
  if can_open_connection connection_manager then 
    match c.client_kind with
      Indirect_address _ | Invalid_address _ -> ()
    | Direct_address (ip, port) ->
        if client_state c <> BlackListedHost then
          if !!black_list && is_black_address ip port ||
            (!!reliable_sources && ip_reliability ip = Reliability_suspicious 0) then
            set_client_state c BlackListedHost
          else
          match c.client_source.DonkeySources.source_sock with
            ConnectionWaiting _ | Connection _ -> 
(* Already connected ! *)
              () 
          | NoConnection ->
              let token =
                add_pending_connection connection_manager (fun token ->
                    try
                      set_client_state c Connecting;
(*                  connection_try c.client_connection_control; *)
                      
                      printf_string "?C";
                      let sock = TcpBufferedSocket.connect token "donkey to client" 
                          (Ip.to_inet_addr ip)
                        port 
                          (client_handler c) (*client_msg_to_string*) in
                      
                      
                      if DonkeySources.source_brand c.client_source then
                        !activity.activity_client_overnet_connections <-
                          !activity.activity_client_overnet_connections +1 
                      else
                        !activity.activity_client_edonkey_connections <-
                          !activity.activity_client_edonkey_connections +1 ;
                                            
                      init_connection sock ip;
                      init_client sock c;
(* The lifetime of the client socket is now half an hour, and
can be increased by AvailableSlotReq, BlocReq, QueryBlocReq 
  messages *)
                      set_lifetime sock active_lifetime;
                      
                      c.client_checked <- false;
                      
                      set_reader sock (
                        DonkeyProtoCom.cut_messages 
                          (DonkeyProtoClient.parse c.client_emule_proto)
                        (client_to_client files c));
                      
                      c.client_source.DonkeySources.source_sock <- Connection sock;
                      c.client_ip <- ip;
                      c.client_connected <- true;
                      let server_ip, server_port = 
                        try
                          let s = DonkeyGlobals.last_connected_server () in
                          s.server_ip, s.server_port
                        with _ -> Ip.localhost, 4665
                      in
                      
                      client_send c (
                        let module M = DonkeyProtoClient in
                        let module C = M.Connect in
                        if DonkeySources.source_brand c.client_source then
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
                            C.tags = !client_to_client_tags;
                            C.version = 16;
                            C.server_info = Some (server_ip, server_port);
                            C.left_bytes = left_bytes;
                          }
                      )
                      
                with e -> 
                    lprintf "Exception %s in client connection\n"
                      (Printexc2.to_string e);
(*                    connection_failed c.client_connection_control; *)
                    set_client_disconnected c (Closed_for_exception e);
                    DonkeySources.source_disconnected c.client_source
            )
          in
          c.client_source.DonkeySources.source_sock <- ConnectionWaiting token
          
  
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
        try
          let addr = 
            if Ip.valid ip then
              (if ip_reachable ip  then 
                  Direct_address (ip, port)
                else raise Not_found)
            else begin
(*              lprintf "RIA: Received indirect address\n"; *)
                Indirect_address (s.server_ip, s.server_port, id_of_ip ip)
              end
          in
          
(* TODO: verify that new sources are queried as soon as possible. Maybe we
should check how many new sources this client has, and query a connection
immediatly if they are too many.

                if Ip.valid cid then
                  match s.server_sock with
                  | Connection sock ->
                      printf_string "QUERY ID";
                      query_id s sock ip (Some file)
                      
                  | _ ->
            DonkeySources.ask_indirect_connection_by_udp s.server_ip s.server_port ip 
            let module Q = Udp.QueryCallUdp in
            udp_server_send s 
              (Udp.QueryCallUdpReq {
                Q.ip = client_ip None;
                Q.port = !client_port;
                Q.id = ip;
              })
        *)
          
          let s = DonkeySources.find_source_by_uid addr in
          DonkeySources.set_request_result s file.file_sources File_new_source
        with Not_found -> () (* Black listed *)
    ) t.Q.locs
    
  with Not_found -> ()
      
let can_open_indirect_connection () =
  let ns = nb_sockets () in
  ns < MlUnix.max_sockets &&
  !DonkeySources.indirect_connections < !!max_indirect_connections

let client_connection_handler overnet t event =
  printf_string "[REMOTE CONN]";
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if can_open_indirect_connection () then
        begin
          (try
              let c = ref None in
              incr DonkeySources.indirect_connections;
(*              lprintf "INDIRECT CONNECTION.........\n"; *)
              let token = create_token connection_manager in
              let sock = 
                TcpBufferedSocket.create token "donkey client connection" s 
                  (client_handler2 c) 
(*client_msg_to_string*)
              in
              init_connection sock (Ip.of_inet_addr from_ip);
              accept_connection_bandwidth sock;
              
(* Normal connections have 20 minutes to live (AvailableSlot, QueryBloc
  and Bloc messages extend this lifetime), whereas exceeding connections
  have only 1 minute 30 seconds to live. *)
              set_lifetime sock (
                if can_open_connection connection_manager then
                  active_lifetime
                else 
                  90.
              );
              (try
                  
                  set_reader sock 
                    (DonkeyProtoCom.client_handler2 c (read_first_message overnet)
                    (client_to_client []));
                
                with e -> lprintf "Exception %s in init_connection\n"
                      (Printexc2.to_string e);
                    );
            with e ->
                lprintf "Exception %s in client_connection_handler\n"
                  (Printexc2.to_string e);
                Unix.close s)
        end     
      
      else begin
          (* lprintf "***** CONNECTION PREVENTED by limitations *****\n"; *)
          Unix.close s
        end;
  | _ -> 
      ()      

      
(*************************************************************************)
(*                                                                       *)
(*                       Stubs for CommonSources                         *)
(*                                                                       *)
(*************************************************************************)

let _ =
  DonkeySources.functions.DonkeySources.function_query <-
    (fun s_uid file_uid ->
      try
        let c = find_client_by_key s_uid in
        let file = find_file (Md4.of_string file_uid) in
        c.client_requests_sent <- c.client_requests_sent + 1;
        let module M = DonkeyProtoClient in
        
        let emule_extension = 
          let extendedrequest = M.extendedrequest c.client_emule_proto in
          if extendedrequest > 0 then
            match file.file_swarmer with
              None -> None
            | Some swarmer ->
                let bitmap = Int64Swarmer.verified_bitmap swarmer in
                let chunks = 
                  Array.init (String.length bitmap) 
                  (fun i -> bitmap.[i] = '3')
                in
                let ncompletesources = if extendedrequest > 1 then
                    0 else -1 in
                Some (chunks, ncompletesources)
          else
            None
        in
        DonkeyProtoCom.client_send c (
          M.QueryFileReq {
            M.QueryFile.md4 = file.file_md4;

(* TODO build the extension if needed *)
            M.QueryFile.emule_extension = emule_extension;
          });
        ignore (DonkeySources.add_request c.client_source 
            file.file_sources (last_time ()))        
      with e -> 
          lprintf "DonkeyClient.query_source: exception %s\n"
            (Printexc2.to_string e)
  );
  
  DonkeySources.functions.DonkeySources.function_connect <-
    (fun s_uid ->
      try
        match s_uid with
          Direct_address _ ->
            let c = new_client s_uid in        
            reconnect_client c
        | Invalid_address _ -> ()
        | Indirect_address (ip, port, id) ->

            query_id ip port id

                  
      with e -> 
          lprintf "DonkeyClient.connect_source: exception %s\n"
            (Printexc2.to_string e)
  );
  
  DonkeySources.functions.DonkeySources.function_max_connections_per_second <-
    (fun () -> !!max_clients_per_second);
  
  DonkeySources.functions.DonkeySources.function_max_sources_per_file <-
    (fun () -> !!max_sources_per_file);
  
  DonkeySources.functions.DonkeySources.function_string_to_manager <-
    (fun file_uid ->
      let file = find_file (Md4.of_string file_uid) in
      file.file_sources
  );

  
(* TODO: verify that the client is destroyed very early. We should also find
a way to keep the client allocated after the first successful connection,
for a given time. For example, we could put successful clients in
a FIFO from where they are removed after 30 minutes. What about using
  file.file_clients for this purpose !! *)
  DonkeySources.functions.DonkeySources.function_add_location <- (fun
      s_uid file_uid ->
      try
        let c = new_client s_uid in
        let file = find_file (Md4.of_string file_uid) in
        
        CommonFile.file_add_source (CommonFile.as_file file.file_file) 
        (CommonClient.as_client c.client_client);
      
      with e -> 
          lprintf "DonkeyClient.add_location: exception %s\n"
            (Printexc2.to_string e)  
  );
  
  DonkeySources.functions.DonkeySources.function_remove_location <- (fun
      s_uid file_uid ->
      try
        let c = new_client s_uid in
        let file = find_file (Md4.of_string file_uid) in
        CommonFile.file_remove_source (CommonFile.as_file file.file_file)
        (CommonClient.as_client c.client_client);
        
      with e -> 
          lprintf "DonkeyClient.remove_location: exception %s\n"
            (Printexc2.to_string e)
  )
  
  