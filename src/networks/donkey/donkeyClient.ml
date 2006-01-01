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

open Int64ops
open Printf2
open Md4
open Ip_set

open CommonSources
open CommonDownloads  
open CommonRoom
open CommonShared
open CommonGlobals
open CommonFile
open CommonClient
open CommonComplexOptions
open CommonSwarming
  
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

(* prints a new logline with date, module and starts newline *)
let lprintf_nl () =
  lprintf "%s[EDK] "
    (log_time ()); lprintf_nl2

(* prints a new logline with date, module and does not start newline *)
let lprintf_n () =
  lprintf "%s[EDK] "
    (log_time ()); lprintf

(* Lifetime of a socket after sending interesting messages *)
let active_lifetime = 1200.


(*************************************************************************)
(*              adding a source to the source-management                 *)
(*************************************************************************)
let add_source file ip port serverIP serverPort =
    (* man, we are receiving sources from some clients even when we release *)
    if (file_state file) = FileDownloading then
        try
            let uid = 
                if low_id ip then
                  (* indirect address *)
                  begin
                    try
                        (* without server, we can't request a callback *)
                        let s = Hashtbl.find servers_by_key serverIP in
                        if serverPort = s.server_port then
                            Indirect_address ( serverIP, serverPort, id_of_ip ip )
                        else
                            raise Not_found
                    with _ ->
                        raise Not_found
                  end
                else
                  (* direct adsdess *)
                  if Ip.usable ip then
                    if not ( is_black_address ip port ) then
                        if not ( Hashtbl.mem banned_ips ip) then
                            Direct_address ( ip, port )
                        else
                            raise Not_found
                    else
                        raise Not_found
                  else
                    raise Not_found
            in
            let s = DonkeySources.find_source_by_uid uid in
            DonkeySources.set_request_result s file.file_sources File_new_source;
        with Not_found -> ()

let is_banned c sock = 
  c.client_banned <- Hashtbl.mem banned_ips (fst (peer_addr sock))


(* Supports Emule Extended Protocol *)
let supports_eep cb = 
  match cb with
    Brand_lmule | Brand_newemule | Brand_cdonkey |
    Brand_emuleplus | Brand_hydranode | Brand_mldonkey3 |
    Brand_shareaza | Brand_amule | Brand_lphant -> true
  | _ -> false

let ban_client c sock msg = 
    let ip = fst (peer_addr sock) in
  if not (Hashtbl.mem banned_ips ip) then
    let module M = DonkeyProtoClient in
    
    if !verbose then
        lprintf_nl () "banned: %s %s" msg (full_client_identifier c);
    
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
          let old_time = last_time () - record.last_request in
          record.nwarnings <- record.nwarnings + 1;
          record.last_request <- last_time ();
          if record.nwarnings > 3 then raise Exit;
          let module M = DonkeyProtoClient in
          if record.nwarnings = 3 then begin
              ban_client c sock "is connecting too fast";
              raise Exit;
            end;
          if !verbose then
              lprintf_nl () "warning no. %d, connecting too fast (last connect %d sec. ago): %s"
	        record.nwarnings old_time (full_client_identifier c);
          if !!send_warning_messages then
            client_send c ( M.SayReq  (
                "[AUTOMATED WARNING] Your client is connecting too fast, it will get banned"))
        end else
        record.last_request <- last_time ();
    with
      Not_found ->
        Hashtbl.add old_requests (client_num c, file_num file) 
        { last_request = last_time (); nwarnings = 0; }
      | Exit -> ()
        
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
        c.client_connect_time <- last_time ();
        client_send c (
          let module M = DonkeyProtoClient in
          let module Q = M.AvailableSlot in
          M.AvailableSlotReq Q.t);
        
        if !verbose then
            lprintf_nl () "New uploader %s"
              (full_client_identifier c);
        
    )  
  in
  client_ops.op_client_enter_upload_queue <- client_enter_upload_queue
   

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
	  let log_print cc =
            lprintf_nl () "Client[%d] %s disconnected, connected %s%s%s"
	      (client_num cc)
	      (full_client_identifier cc)
	      (Date.time_to_string (last_time () - cc.client_connect_time) "verbose")
	      (if cc.client_uploaded > 0L then
		Printf.sprintf ", send %s" (size_of_int64 cc.client_uploaded) else "")
	      (if cc.client_downloaded > 0L then
		Printf.sprintf ", rec %s" (size_of_int64 cc.client_downloaded) else "")
	  in
	  if c.client_debug ||
	    (!verbose && (c.client_uploaded > 0L || c.client_downloaded > 0L)) then
	    log_print c;

          c.client_comp <- None;
          (try if c.client_checked then count_seen c with _ -> ());
          (try if !!log_clients_on_console && c.client_name <> "" then 
                log_client_info c sock with _ -> ());
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
        
        with e -> lprintf_nl () "Exception %s in disconnect_client"
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
                      if !verbose_sources > 2 then
                          lprintf_nl () "Send %d sources from file groups to mldonkey peer" (List.length !list); 
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
(* send this client as a source for the file to all mldonkey clients in the group. add client to group *)
              
              UdpClientMap.iter (fun _ uc ->
                  if uc.udp_client_can_receive then begin
                      if !verbose_sources > 2 then
                        lprintf_nl () "Send new source to file groups UDP peers";
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
    ("efmod", Brand_mod_efmod);
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
    ("ib\233ricaxt", Brand_mod_ibericaxt);
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
    ("emule.de", Brand_mod_emulede);
    ("aldo", Brand_mod_aldo);
    ("dm", Brand_mod_dm);
    ("lc", Brand_mod_lc);
    ("lh", Brand_mod_lh);
    ("l!onetwork", Brand_mod_lh);
    ("lionetwork", Brand_mod_lh);
    ("hawkstar", Brand_mod_hawkstar);
    ("neo mule", Brand_mod_neomule);
    ("cyrex", Brand_mod_cyrex);
    ("zx", Brand_mod_zx);
    ("ackronic", Brand_mod_ackronic);
    ("rappis", Brand_mod_rappis);
    ("overdose", Brand_mod_overdose);
    ("hebmule", Brand_mod_hebmule);
    ("senfei", Brand_mod_senfei);
    ("spoofmod", Brand_mod_spoofmod);
    ("fusspilz", Brand_mod_fusspilz);
    ("rocket", Brand_mod_rocket);
    ("warezfaw", Brand_mod_warezfaw);
    ("emusicmule", Brand_mod_emusicmule);
    ("aideadsl", Brand_mod_aideadsl);
    ("a i d e a d s l", Brand_mod_aideadsl);
    ("epo", Brand_mod_epo);
    ("kalitsch", Brand_mod_kalitsch);
    ("raynz", Brand_mod_raynz);
    ("serverclient", Brand_mod_serverclient);
    ("bl4ckbird", Brand_mod_bl4ckbird);
    ("bl4ckf0x", Brand_mod_bl4ckf0x);
    ("candy-mule", Brand_mod_candymule);
    ("rt", Brand_mod_rt);
    ("ice", Brand_mod_ice);
    ("air-ionix", Brand_mod_airionix);
    ("ionix", Brand_mod_ionix);
    ("tornado", Brand_mod_tornado);
    ("anti-faker", Brand_mod_antifaker);
    ("netf", Brand_mod_netf);
    ("nextemf", Brand_mod_nextemf);
    ("proemule", Brand_mod_proemule);
    ("szemule", Brand_mod_szemule);
    ("darkmule", Brand_mod_darkmule);
    ("miragemod", Brand_mod_miragemod);
    ("nextevolution", Brand_mod_nextevolution);
    ("pootzgrila", Brand_mod_pootzgrila);
    ("freeangel", Brand_mod_freeangel);
    ("enos", Brand_mod_enos);
    ("webys", Brand_mod_webys)
  |]
  
let to_lowercase s = String.lowercase s

let string_of_tags_list tags =
  let s = ref "" in
  List.iter (fun tag ->
    let st = to_lowercase (string_of_tag_value tag.tag_value) in
    let str = (escaped_string_of_field tag) ^ " : " ^ st ^ " ; " in
    s := !s ^ str
  ) tags;
  !s 

let identify_client_brand_mod c tags =
  if c.client_brand_mod = Brand_mod_unknown then begin
      List.iter (fun tag ->
        let s = to_lowercase (string_of_tag_value tag.tag_value) in 
          match tag.tag_name with
           Field_UNKNOWN "mod_version" ->
               begin
               let rec iter i len =
                if i < len then
                 let sub = fst mod_array.(i) in
                     if  (String2.subcontains s sub) then
                        c.client_brand_mod <- snd mod_array.(i)
                     else iter (i+1) len
               in
               iter 0 (Array.length mod_array)
               end
           | _ -> ()

   ) tags;
   if String2.subcontains c.client_name "@PowerMule" then begin
     c.client_brand_mod <- Brand_mod_powermule
   end
  end

let update_emule_release c =
  let client_version = c.client_emule_proto.emule_version land 0x00ffffff in
  let brand = c.client_brand in

  let maj = (client_version lsr 17) land 0x7f in
  let min =  (client_version lsr 10) land 0x7f in
  let up = (client_version lsr 7) land 0x07 in

  c.client_emule_proto.emule_release <- (
    if maj = 0 && min = 0 && up = 0 then 
      "" 
    else if brand = Brand_newemule || brand = Brand_emuleplus then
      Printf.sprintf "%d.%d%c" maj min (Char.chr ((int_of_char 'a') + up))
    else 
      Printf.sprintf "%d.%d.%d" maj min up 
  )

let parse_compatible_client num old_brand =
    match num with
      0 -> old_brand
    | 1 -> Brand_cdonkey
    | 2 -> Brand_lmule
    | 3 -> Brand_amule
    | 4
    | 40 -> Brand_shareaza
    | 5 -> Brand_emuleplus
    | 6 -> Brand_hydranode
    | 10 -> Brand_mldonkey3
    | 20 -> Brand_lphant
    | _ -> lprintf_nl () "unknown compatibleclient %d (please report to dev team)" num; Brand_unknown

let parse_mod_version s c =
  let rec iter i len =
    if i < len then
      let sub = fst mod_array.(i) in
      if (String2.subcontains s sub) then
         c.client_brand_mod <- snd mod_array.(i)
      else iter (i+1) len
  in
   iter 0 (Array.length mod_array)

let update_client_from_tags c tags =
  List.iter (fun tag ->
      match tag.tag_name with
      | Field_UNKNOWN "name" -> ()
      | Field_UNKNOWN "version" -> ()
      | Field_UNKNOWN "emule_udpports" -> 
          for_two_int16_tag tag (fun ed2k_port kad_port ->
(* Kademlia: we should use this client to bootstrap Kademlia *)
            if kad_port <> 0 && !!enable_kademlia then
              DonkeyProtoKademlia.Kademlia.bootstrap 
                c.client_ip kad_port
          )
      | Field_UNKNOWN "emule_miscoptions1" ->
          for_int64_tag tag (fun i ->
            DonkeyProtoClient.update_emule_proto_from_miscoptions1
            c.client_emule_proto i
          )
      | Field_UNKNOWN "emule_version" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_version <- i;
            let compatibleclient = (i lsr 24) in
            c.client_brand <- parse_compatible_client compatibleclient c.client_brand;
            update_emule_release c;
              
            if c.client_brand = Brand_unknown then
              lprintf_nl () "[emule_version] Brand_unknown %s" (full_client_identifier c);
          )
      | Field_UNKNOWN "mod_version" ->
          let s = to_lowercase (string_of_tag_value tag.tag_value) in 
          parse_mod_version s c
      | _ -> ()

  ) tags
    
let update_emule_proto_from_tags c tags =
  List.iter (fun tag ->
      match tag.tag_name with
        Field_UNKNOWN "compatibleclient" ->
          for_int_tag tag (fun i ->
            c.client_brand <- parse_compatible_client i c.client_brand;
            if c.client_brand = Brand_unknown then
              lprintf_nl () "[compatibleclient] Brand_unknown %s" (full_client_identifier c);
          )
      | Field_UNKNOWN "compression" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_compression <- i
          )
      | Field_UNKNOWN "udpver" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_udpver <- i
          )
      | Field_UNKNOWN "udpport" -> ()
      | Field_UNKNOWN "sourceexchange" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_sourceexchange <- i
          ) 
      | Field_UNKNOWN "comments" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_comments <- i
          )
      | Field_UNKNOWN "extendedrequest" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_extendedrequest <- i
          )
      | Field_UNKNOWN "features" ->
          for_int_tag tag (fun i ->
            c.client_emule_proto.emule_secident <- i land 0x3
          )
      | Field_UNKNOWN "mod_version" ->
          let s = to_lowercase (string_of_tag_value tag.tag_value) in 
          parse_mod_version s c;
      | _ -> 
          if !verbose_msg_clients then
            lprintf_nl () "Unknown Emule tag: [%s]" (escaped_string_of_field tag)
  ) tags

let fight_disguised_mods c =
   if (c.client_brand = Brand_mldonkey2 || c.client_brand = Brand_mldonkey3)
     && (c.client_brand_mod = Brand_mod_morphxt || c.client_brand_mod = Brand_mod_ionix) then
       c.client_brand <- Brand_newemule;
   if c.client_emule_proto.emule_release <> "" && c.client_brand = Brand_mldonkey2 then
      c.client_brand <- Brand_newemule;
   if c.client_brand = Brand_edonkey && c.client_brand_mod = Brand_mod_plus then
      c.client_brand <- Brand_emuleplus;
   if c.client_brand = Brand_emuleplus && c.client_brand_mod = Brand_mod_plus then
      c.client_brand_mod <- Brand_mod_unknown

let rec query_id ip port id =
  let client_ip = client_ip None in

(* TODO: check if we are connected to this server. If yes, issue a 
  query_id instead of a UDP packet *)
  if Ip.reachable client_ip then
    let module Q = DonkeyProtoUdp.QueryCallUdp in
(*    lprintf "Ask connection from indirect client\n"; *)

    try
      let s = DonkeyGlobals.find_server ip port in
    match s.server_sock with 
      NoConnection | ConnectionWaiting _ -> ()

(* OK, this fixes the problem with Lugdunum servers, but there should be
another better way, since this functionnality is still useful... 

  DonkeyProtoCom.udp_send (get_udp_sock ())
        ip (port+4)
        (DonkeyProtoUdp.QueryCallUdpReq {
            Q.ip = client_ip;
            Q.port = !!donkey_port;
            Q.id = id;
          }) *)
    | Connection sock ->
        server_send sock (
          let module M = DonkeyProtoServer in
          let module C = M.QueryID in
          M.QueryIDReq id
        );
        ()
    with _ ->
      if !!update_server_list_client then
        begin
          ignore(check_add_server ip port);
          query_id ip port id
        end


external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
let hash x = hash_param 10 100 x

let shared_of_file file =
  match file.file_shared with
    | None  -> None
    | Some sh -> Some (as_shared sh)

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

(* client is valid if it's not us or if it's not yet connected *)
let is_valid_client md4 =
  md4 <> !!client_md4 &&
  md4 <> overnet_md4 &&
  not (Hashtbl.mem connected_clients md4)

(*Do what's need to be done when client has a file we want:
  - register it in sources
  - do *not* ask for sources, we can't be sure, the client is still downloading the file!
*)
let client_has_file c file =
  DonkeyNeighbours.new_neighbour c file;
  DonkeySources.set_request_result c.client_source file.file_sources File_found

(*
  Do what's need to be done when client asked for a file we want:
  - register it in sources
  - ask for sources if necessary 
  - do not ask sources from mldonkey-clients, they are supposed to automatically send sources after an QueryFileReq
*)
let client_queried_file c file =
    client_has_file c file;
    let module M = DonkeyProtoClient in
    if file_state file = FileDownloading
       && M.sourceexchange c.client_emule_proto > 0
       && DonkeySources.need_new_sources file.file_sources
       && not (client_can_receive c)
      then
        (* ask for more sources *)
        begin
          if !verbose_location then
             lprintf_nl () "donkeyClient: Requesting sources from client %s that queried file %s"
                 (full_client_identifier c) (file_best_name file);
          let module E = M.EmuleRequestSources in
          client_send c (M.EmuleRequestSourcesReq file.file_md4)
        end

(*Do what's need to be done when client has file chunks we want:
  - register it in sources
  - ask for sources if necessary  Edit: errr, where is this done?
*)
let client_is_useful c file chunks = 
  DonkeySources.set_request_result c.client_source file.file_sources File_chunk;
  DonkeyOneFile.add_client_chunks c file chunks;
  if file_state file = FileDownloading then
    DonkeyOneFile.request_slot c

(* added in 2.5.25
Check if the bitmap returned by a client contains a chunk that has not
  yet been downloaded.
  *)
let is_useful_client file chunks =
  match file.file_swarmer with
    None -> false
  | Some swarmer ->
      let bitmap = Int64Swarmer.verified_bitmap swarmer in
      let rec iter bitmap chunks i len =
        if i = len then false else
        if Bitv.get chunks i && bitmap.[i] < '2' then true else
          iter bitmap chunks (i+1) len
      in
      iter bitmap chunks 0 (String.length bitmap)
    
let received_client_bitmap c file chunks =
  
  let module M = DonkeyProtoClient in
  
  if !verbose_msg_clients then begin
      lprintf_nl () "Compared to:";
      match file.file_swarmer with
        None -> ()
      | Some swarmer ->
          lprintf_nl () "   %s" (Int64Swarmer.verified_bitmap swarmer);
    end;
  
  let chunks = 
    if file_size file <= block_size then Bitv.create 1 true
    else
      if Bitv.length chunks = 0 then
        Bitv.create file.file_nchunks true
    else
    if Bitv.length chunks <> file.file_nchunks then begin
        if !verbose then
	    lprintf_nl () "number of chunks is different %d/%d for %s(%s), size %Ld on %s"
              (Bitv.length chunks)
              file.file_nchunks 
	      (file_best_name file)
              (Md4.to_string file.file_md4) 
              (file_size file)
	      (full_client_identifier c);
        Bitv.create file.file_nchunks false
(* What should we do ?

1) Try to recover the correct size of the file: we can use 
ViewFilesReq on all clients having the file to test what is
the most widely used size for this file. Maybe create 
different instances of the file for each proposed size ?
  
*)
      
      end else 
      chunks in
    if is_useful_client file chunks then client_is_useful c file chunks


let send_pending_messages c sock =
  let module M = DonkeyProtoClient in
  
  List.iter (fun m ->
      client_send c (M.SayReq m)
  ) c.client_pending_messages;
  c.client_pending_messages <- []
  
let init_client_after_first_message sock c = 
  (* we read something on socket so ip is now know for socket *)
  c.client_ip <- peer_ip sock;
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
  (* query_files c sock;  see comment at implementation*)
  DonkeySources.source_connected c.client_source;  
  query_view_files c;
  client_must_update c;
  c.client_checked <- true;
  is_banned c sock


(* reverse ip bytes? *)
let int64_of_rip ip =
  Ip.to_int64 (Ip.rev ip)

let get_high_id_int64 () = 
  let result = ref Int64.zero in
  List.iter (fun s ->
    if !result = Int64.zero then
      (match s.server_cid with
        None -> ()
      | Some i -> if not (low_id i) then 
                    result := int64_of_rip i;
    )
  ) (connected_servers());
  !result

(* If we know our own IP (donkey high id), use type 20 and our ip
   If we do not know our IP (could be NAT'd), use type 10 and their ip *)
let get_ip_and_type sock =
  let ip = ref (get_high_id_int64 ()) in
  let ip_type = ref (if !ip == Int64.zero then 0 else 20) in
  
  if (!ip_type == 0) then begin
    match sock with 
    Connection s ->
            ip_type := 10; 
            ip := int64_of_rip (peer_ip s);
    | _ -> ()
  end;
  (!ip,!ip_type)

let has_pubkey c =
  match c.client_public_key with
   None -> false
   |  _ -> true

let get_pubkey c =
  match c.client_public_key with
   None -> ""
   | Some s -> s

let send_signature c = 
  if has_pubkey c then 
  begin

    let ip = ref Int64.zero in
    let ip_type = ref 0 in
    (* check low id? *)
    if (c.client_emule_proto.emule_secident == 2) then begin (* Use v1 as default, except if only v2 is supported (same as emule) *)
      let (x,y) = get_ip_and_type c.client_source.DonkeySources.source_sock in
      ip := x;
      ip_type := y;
    end;

    let pubkey = get_pubkey c in
    let signature = Unix32.create_signature pubkey (String.length pubkey) c.client_req_challenge !ip_type !ip in
    
    if !verbose_msg_clients then begin
      lprintf_nl () "%s [send_signature] [sigLen: %d] [keyLen: %d] [reqChall: %Ld] [ipType: %d] [ip: %Ld]" (full_client_identifier c) (String.length signature) (String.length pubkey) c.client_req_challenge !ip_type !ip;
    end;
  
    let module M = DonkeyProtoClient in
    let module E = M.EmuleSignatureReq in
    client_send c (M.EmuleSignatureReq {
           E.signature = signature;
           E.ip_type = !ip_type;
    });
  end
    else
      if !verbose_msg_clients then begin
        lprintf_nl () "%s [send_signature] Can't send without a key" (full_client_identifier c)
      end

let verify_ident c =
  let challenge = Random.int64 (Int64.of_int32 Int32.max_int) in
  let state, state_string = if has_pubkey c then (1,"SIGNEEDED") else (2,"KEYANDSIGNEEDED") in
  c.client_sent_challenge <- challenge;

  if !verbose_msg_clients then begin
    lprintf_nl () "%s [verify_ident] [state: %d (%s)] [sentChall: %Ld]" (full_client_identifier c) state state_string challenge;
  end;

  let module M = DonkeyProtoClient in
  let module E = M.EmuleSecIdentStateReq in
  client_send c (M.EmuleSecIdentStateReq {
    E.state = state;
    E.challenge = challenge;
  })

let send_public_key c =
  if !verbose_msg_clients then begin
    lprintf_nl () "%s [send_public_key] [keyLen: %d]" (full_client_identifier c) (String.length !client_public_key);
  end;

  let module M = DonkeyProtoClient in
  client_send c (M.EmulePublicKeyReq !client_public_key)

let get_server_ip_port () =
  match !DonkeyGlobals.master_server with
    | None ->
       Ip.null, 0
    | Some s ->
       let port =
         match s.server_realport with
           None -> (*lprintf "%d\n" s.server_port;*) s.server_port
           | Some p -> (*lprintf "%d\n" p;*) p
       in
         s.server_ip, port

let process_mule_info c t =
  update_emule_proto_from_tags c t;
  update_emule_release c;
  if !!enable_sui
      && (c.client_md4 <> Md4.null) 
      && (c.client_sent_challenge == Int64.zero) 
      && (c.client_emule_proto.emule_secident > 0) 
  then begin
    if !verbose_msg_clients then begin
      lprintf_nl () "%s [process_mule_info] [verify_ident]" (full_client_identifier c);
    end;
    verify_ident c
  end


let incr_activity_successful_connections c =
  if DonkeySources.source_brand c.client_source then
    !activity.activity_client_overnet_successful_connections <-
      !activity.activity_client_overnet_successful_connections +1 
  else
    !activity.activity_client_edonkey_successful_connections <-
      !activity.activity_client_edonkey_successful_connections +1

let incr_activity_indirect_connections c =
  if DonkeySources.source_brand c.client_source then
    !activity.activity_client_overnet_indirect_connections <-
      !activity.activity_client_overnet_indirect_connections +1 
  else
    !activity.activity_client_edonkey_indirect_connections <-
      !activity.activity_client_edonkey_indirect_connections +1

let incr_activity_connections c = 
  if DonkeySources.source_brand c.client_source then
    !activity.activity_client_overnet_connections <-
      !activity.activity_client_overnet_connections +1 
  else
    !activity.activity_client_edonkey_connections <-
      !activity.activity_client_edonkey_connections +1

let check_stolen_hash c sock md4 =
  if not (register_client_hash (peer_ip sock) md4) then
    if !!ban_identity_thieves then
      ban_client c sock "is probably using stolen client hashes"

let client_to_client for_files c t sock = 
  let module M = DonkeyProtoClient in
  
  if !verbose_msg_clients || c.client_debug then begin
      lprintf_nl () "Message from %s" (string_of_client c);
      M.print t;
    end;
  
  match t with
    M.ConnectReplyReq t ->
      if !verbose_msg_clients then begin
        lprintf_nl () "[HELLOANSWER] %s" (full_client_identifier c); 
      end;
    
      incr_activity_successful_connections c;
      
      init_client_after_first_message sock c;
      
      set_client_has_a_slot (as_client c) false;
      
      let module CR = M.Connect in

      if not (is_valid_client t.CR.md4) then
        begin
          TcpBufferedSocket.close sock (Closed_for_error "Reply of Invalid Client");
          raise Exit
        end;
      
      if (is_black_address t.CR.ip t.CR.port) then raise Exit;
      
      check_stolen_hash c sock t.CR.md4; 

      c.client_tags <- t.CR.tags;

      List.iter (fun tag ->
          match tag with
            { tag_name = Field_UNKNOWN "name"; tag_value = String s } -> 
              set_client_name c s t.CR.md4
          | _ -> ()
      ) c.client_tags;

      identify_client_brand c;
      update_client_from_tags c t.CR.tags;
      fight_disguised_mods c;
      update_emule_release c;
      Hashtbl.add connected_clients t.CR.md4 c;

(*      connection_ok c.client_connection_control; *)

      if c.client_debug || !verbose_msg_clients || !verbose_msg_clienttags then begin  
        M.Connect.print t;
      end;

      begin
        match t.CR.server_info with
          Some (ip, port) -> if !!update_server_list_client then safe_add_server ip port
        | _ -> ()
      end;
      
      check_stolen_hash c sock t.CR.md4;

      finish_client_handshake c sock;
      (* We initiated the connection so we know which files to ask *)
      DonkeySources.query_files c.client_source
  
  | M.EmuleQueueRankingReq rank
  | M.QueueRankReq rank ->
      c.client_rank <- rank;
      set_client_state c (Connected rank);
      if rank > !!good_client_rank then
        List.iter (fun (file, _, _) ->
            let s = c.client_source  in
            let m = file.file_sources in
            match DonkeySources.find_request_result s m with
              File_chunk -> 
                DonkeySources.set_request_result s m  File_found;  
            | _ -> ()
        ) c.client_file_queue
  
  | M.EmuleClientInfoReq t ->      
      
      c.client_ip <- peer_ip sock;
(*      lprintf "Emule Extended Protocol asked\n";  *)
      let module CI = M.EmuleClientInfo in
      process_mule_info c t.CI.tags;
      if !!emule_mods_count then
        identify_client_brand_mod c t.CI.tags;
      
      (* TODO : remove this comment 
          ERR! i think the peer support eep if it send an emule client info
          PLUS this message is sent _before_ we received an md4 so 
          client_brand is unknown here.
      if supports_eep c.client_brand then  begin
      *)
          let module E = M.EmuleClientInfo in
          client_send c (M.EmuleClientInfoReplyReq emule_info)
  
  | M.EmuleClientInfoReplyReq t -> 
      
      let module CI = M.EmuleClientInfo in
      
      process_mule_info c t.CI.tags;
      
      if !verbose_msg_clienttags then
          lprintf_nl () "Message from client[%d] %s %s  tags: %s"
            (client_num c)
            (match c.client_kind with
              Indirect_address _ | Invalid_address _ -> ""
              | Direct_address (ip,port) ->
                  Printf.sprintf " [%s:%d]" (Ip.to_string ip) port;
             )
            (full_client_identifier c)
            (string_of_tags_list t.CI.tags)

(*   lprintf "Emule Extended Protocol activated\n"; *)
  
  
  | M.EmuleRequestSourcesReq t ->
      let module E = M.EmuleRequestSourcesReply in

(*       lprintf "Emule requested sources\n";  *)
      let file = find_file t in
      let sources = ref [] in
      DonkeySources.iter_qualified_sources (fun s ->
          match s.DonkeySources.source_uid with
            Indirect_address _ | Invalid_address _ -> () (* not yet supported *)
          | Direct_address (ip, port) ->
              if s.DonkeySources.source_age > last_time () - 600 &&
                (match ip_reliability ip with
                    Reliability_reliable | Reliability_neutral -> true
                  | Reliability_suspicious _ -> false) &&
                List.exists (fun r ->
                    r.DonkeySources.request_score >= CommonSources.possible_score
                ) s.DonkeySources.source_files then
                sources := {
                  E.src_ip = ip;
                  E.src_port = port;
                  E.src_server_ip = Ip.null;
                  E.src_server_port = 0;
(* this is not very good, but what can we do ? we don't keep sources UIDs *)
                  E.src_md4 = Md4.null;
                } :: !sources
      ) file.file_sources;
      if !sources <> [] then
        begin
          if !verbose_location then
            lprintf_nl () "donkeyClient: EmuleRequestSourcesReq: Sending %d Sources to %s for file %s"
              (List.length !sources) (full_client_identifier c) (file_best_name file);
          client_send c (
            M.EmuleRequestSourcesReplyReq {
              E.md4 = t;
              E.sources = Array.of_list !sources;
            })
        end

  | M.ViewFilesReplyReq t ->
(*
      lprintf "****************************************\n";
      lprintf "       VIEW FILES REPLY         \n";
      *)
      let module Q = M.ViewFilesReply in
      begin
        if !verbose_msg_clients then
            lprintf_nl () "Received ViewFilesReply";
        try
          let list = ref [] in
          List.iter (fun f ->
              match result_of_file f.f_md4 f.f_tags with
                None -> ()
              | Some r ->
(* TODO                   let r = DonkeyIndexer.index_result_no_filter r in *)
                  client_new_file c r;
                  list := r :: !list
          ) t;
          c.client_all_files <- Some !list;
          client_must_update c
        
        with e ->
            lprintf_nl () "Exception in ViewFilesReply %s"
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
            if !verbose_download then
                lprintf_nl () "Clear download";
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
                          lprintf_nl () "Recovered file queue by md4";
                        v
                      with _ ->
                          let id = client_id c in
                          let v = Hashtbl.find join_queue_by_id id in
                          if c.client_debug then
                            lprintf_nl () "Recovered file queue by md4";
                          v
                    in
                    List.iter (fun (file, chunks) ->
                        let chunks = Bitv.copy chunks in
                        DonkeyOneFile.add_client_chunks c file chunks) files;
(*                DonkeyOneFile.restart_download c *)
                  with _ -> 
                      if c.client_debug then
                        lprintf_nl () "AvailableSlot received, but not file to download!";
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

(*    set_rtimeout sock !!upload_timeout; *)
          set_lifetime sock one_day;
          add_pending_slot c
        
with _ -> *)
      
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
      if !verbose_upload then
          lprintf_nl () "donkeyClient: uploader couldn't get a slot: %s"
            (full_client_identifier c);
(*      end *)
  
  | M.CloseSlotReq _ ->
      set_client_state c (Connected 0);
      begin
        match c.client_download with
          None -> ()
        | Some (file,up) ->
            if !verbose_download then
                lprintf_nl () "Slot closed during download";
            Int64Swarmer.clear_uploader_ranges up
      end;
(*      DonkeyOneFile.clean_current_download c; *)
      c.client_slot <- SlotNotAsked;
(* OK, the slot is closed, but what should we do now ????? *)
      begin
        match c.client_file_queue with
          [] -> ()
        | _ -> 
            if !verbose_download then
                lprintf_nl () "CloseSlotReq"; 
            DonkeyOneFile.request_slot c;
            set_rtimeout sock !!queued_timeout;
      end
  
  | M.ReleaseSlotReq _ ->
      DonkeyOneFile.remove_client_slot c;
      if c.client_file_queue = [] then set_rtimeout sock 120.;
      CommonUploads.refill_upload_slots ()
  
  | M.QueryFileReplyReq t ->
      let module Q = M.QueryFileReply in
      
      begin
        try
          let file = find_file t.Q.md4 in
          c.client_rating <- c.client_rating + 1;
          
          client_has_file c file;
          
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
              client_is_useful c file (Bitv.create 1 true)
            end else begin
              
              if file.file_computed_md4s = [||] then begin
                  client_send c (
                    let module M = DonkeyProtoClient in
                    let module C = M.QueryChunkMd4 in
                    M.QueryChunkMd4Req file.file_md4);
                end
            end
        with _ -> ()
      end  
  
  | M.QueryChunksReplyReq t ->
      let module Q = M.QueryChunksReply in
      begin
          try
            let file = find_file t.Q.md4 in
              received_client_bitmap c file t.Q.chunks
          with e ->
	    client_send c (M.NoSuchFileReq t.Q.md4);
	    if !verbose then lprintf_nl ()
	      "QueryChunksReply: Client (%s) asked for file_md4 %s, Exception %s"
	      (full_client_identifier c)
	      (Md4.to_string t.Q.md4)
	      (Printexc2.to_string e)
      end
  
  | M.QueryChunkMd4ReplyReq t ->
      begin
        let module Q = M.QueryChunkMd4Reply in
        let file = find_file t.Q.md4 in
        
        let module Q = M.QueryChunkMd4Reply in
        if !verbose then
            lprintf_nl () "Received chunks md4 for %s from %s"
                (file_best_name file) (full_client_identifier c);
        
        if file.file_computed_md4s = [||] then begin
        if file.file_nchunks = 1 then begin
            lprintf_nl () "[ERROR] file %s has only one chunk, ignoring QueryChunkMd4ReplyReq"
	      (file_best_name file);
            file.file_computed_md4s <- [|file.file_md4|];
            match file.file_swarmer with
              None -> ()
            | Some swarmer ->
                Int64Swarmer.set_verifier swarmer 
                  (Verification [| Ed2k file.file_md4 |])
          end else
        if t.Q.chunks = [||] then
            lprintf_nl () "[ERROR] received empty chunks md4 message for %s from %s"
	      (file_best_name file) (full_client_identifier c)
        else
        if Array.length t.Q.chunks <> file.file_nchunks then begin
            if !verbose then
	      lprintf_nl () "[ERROR] number of chunks does not match, received md4s %d/should be %d, for %s(%s):%Ld bytes from %s"
	        (Array.length t.Q.chunks)
		file.file_nchunks
		(file_best_name file)
		(Md4.to_string file.file_md4)
		(file_size file)
		(full_client_identifier c)
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
                lprintf_nl () "[ERROR] Chunks md4s do not match file_md4 for %s(%s) from %s"
		  (file_best_name file) (Md4.to_string file.file_md4) (full_client_identifier c);
              end else begin
                file.file_computed_md4s <- md4s;
                match file.file_swarmer with
                  None -> ()
                | Some swarmer ->
                    Int64Swarmer.set_verifier swarmer 
                      (Verification (Array.map (fun m -> Ed2k m) md4s))

              end
            end
          end
(*      if file.file_exists then verify_chunks file *)
      end
  
  
  | M.EmuleCompressedPart (md4, statpos, newsize, bloc) ->
      
      set_lifetime sock active_lifetime;      
      if !!reliable_sources && 
        client_reliability c = Reliability_suspicious 0 then begin
          lprintf_nl () "Receiving data from unreliable client, disconnect";
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
          if !verbose_download then
            lprintf_nl () "Complete compressed block received!";
          
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
            let s = Autoconf.zlib__uncompress_string2 s in
            if !verbose_download then
        lprintf_nl () "Decompressed: %d/%d" (String.length s) comp.comp_len;
            
            DonkeyOneFile.block_received c comp.comp_md4
              comp.comp_pos s 0 (String.length s);
          
          c.client_comp <- None;
        end else
      if comp.comp_len > comp.comp_total then begin
          if !verbose_unknown_messages then
            lprintf_nl () "eMule compressed data, ignoring, more data (%d) than compressed (%d) from %s for %s"
	      comp.comp_len comp.comp_total (full_client_identifier c) (Md4.to_string comp.comp_md4);
          c.client_comp <- None;
        end
  
  | M.BlocReq t ->
      
      set_lifetime sock active_lifetime;      
      if !!reliable_sources && 
        client_reliability c = Reliability_suspicious 0 then begin
          lprintf_nl () "Receiving data from unreliable client, disconnect";
          corruption_warning c;
          disconnect_client c (Closed_for_error "Unreliable Source");
          raise Not_found
        end;
      
      let module M = DonkeyProtoClient in  
      let module Q = M.Bloc in
      
      let begin_pos = t.Q.start_pos in
      let end_pos = t.Q.end_pos in
      let len = end_pos -- begin_pos in
      if Int64.to_int len <> t.Q.bloc_len then begin
          lprintf_nl () "%d: inconsistent packet sizes" (client_num c);
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
      if !verbose_msg_clients then
          lprintf_nl () "Sending %d Files in ViewFilesReqReply" (List.length !published_files);
      client_send_files sock !published_files
  
  | M.QueryFileReq t ->
      let md4 = t.M.QueryFile.md4 in
      c.client_requests_received <- c.client_requests_received + 1;
      
      if  !CommonUploads.has_upload = 0 && 
        not (!!ban_queue_jumpers && c.client_banned) then
        
        (try client_wants_file c md4 with _ -> ());
      
      if md4 = Md4.null && c.client_brand = Brand_edonkey then
          c.client_brand <- Brand_mldonkey1;
      if c.client_brand = Brand_mldonkey1 || c.client_brand = Brand_mldonkey2 then begin
          if !verbose then
              lprintf_nl () "donkeyClient:QueryFileReq: Client %s is really old mldonkey1/2 and queried file %s"
                  (full_client_identifier c) (Md4.to_string md4);
          if Random.int 100 < 3 && !!send_warning_messages then
            client_send c (
                M.SayReq "[AUTOMATED WARNING] Please, update your MLdonkey client to at least version 2.7.0!");
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
          client_queried_file c file;
          (* Here's the correct place to check for emule_extension *)
          begin
            match t.M.QueryFile.emule_extension with
                None -> ()
              | Some (chunks, _) ->
                  received_client_bitmap c file chunks
          end;
    if file_state file = FileDownloading then
      DonkeySources.query_files c.client_source
        
        with Not_found -> 
            client_send c (M.NoSuchFileReq md4);
            if !verbose_unexpected_messages then
              lprintf_nl () "donkeyClient: QueryFileReq: Client %s queried unpublished file %s"
	        (full_client_identifier c) (Md4.to_string md4)
        | e -> 
            lprintf_nl () "Exception %s in QueryFileReq"
              (Printexc.to_string e)
      end

  | M.EmuleSignatureReq t ->
      if !!enable_sui then 
      begin
      let module Q = M.EmuleSignatureReq in

      if !verbose_msg_clients then begin
        let lipType,lipTypeString = 
          (match t.Q.ip_type with
           10 -> (10, "IpLocal")
          | 20 -> (20, "IpRemote")
          | e -> (e, "Unknown")) in
        let lkeyString = if (has_pubkey c) then "" else "[NO KEY!!]" in
        lprintf_nl () "%s [ESigReq] [sentChall: %Ld] [ipType: %d (%s)] %s" (full_client_identifier c) c.client_sent_challenge lipType lipTypeString lkeyString;
      end;

      let ip_type = ref 0 in
      let id = ref Int64.zero in
    
      if (c.client_emule_proto.emule_secident > 1 && t.Q.ip_type <> 0) then 
      begin
        ip_type := t.Q.ip_type;
        if (!ip_type == 20) (* || isLowid *) then
            id := int64_of_rip (peer_ip sock)
        else 
          begin
            id := get_high_id_int64 ();  
            if !id = Int64.zero then begin
                id := int64_of_rip (my_ip sock);
                if !verbose_msg_clients then begin
                  lprintf_nl () "%s [ESigReq] Warning: Local IP unknown (signature might fail)" (full_client_identifier c);
                end;
            end;
          end;
      end;

      let pubKey = get_pubkey c in
      
      if !verbose_msg_clients then begin
        lprintf_nl () "%s [ESigReq] [verify_signature] [keyLen: %d] [sigLen: %d] [sentChall: %Ld] [ipType %d] [ip: %Ld]" (full_client_identifier c) (String.length pubKey) (String.length t.Q.signature) c.client_sent_challenge !ip_type !id;
      end;

      let verified = Unix32.verify_signature pubKey (String.length pubKey) t.Q.signature (String.length t.Q.signature) c.client_sent_challenge !ip_type !id in
      c.client_sui_verified <- Some verified;
      c.client_sent_challenge <- Int64.zero;
      client_must_update c;

      if !verbose_msg_clients then begin
        lprintf_nl () "%s [ESigReq] [verify_signature: %s]" (full_client_identifier c) (if verified then "passed" else "failed");
      end;

      end else
      if !verbose_msg_clients then begin
        lprintf_nl () "%s [ESigReq] [DISABLED]" (full_client_identifier c) ;
      end

  | M.EmulePublicKeyReq t ->
      if !!enable_sui then 
      begin
      let module Q = M.EmulePublicKeyReq in
        (match c.client_public_key with 
        Some s -> if s <> t then 
                  begin
                   if !verbose_msg_clients then begin
                     lprintf_nl () "%s [EPubKeyReq] [Key is different!]" (full_client_identifier c);
                   end;
                   c.client_public_key <- None; 
                  end 
                    else 
                      if !verbose_msg_clients then begin
                        lprintf_nl () "%s [EPubKeyReq] [Key matches]" (full_client_identifier c);
                      end;
        | _ -> 
          c.client_public_key <- Some t;
          if !verbose_msg_clients then begin
            lprintf_nl () "%s [EPubKeyReq] [New Key] [keyLen: %d] [reqChall: %Ld]" (full_client_identifier c) (String.length t) c.client_req_challenge;
          end;
  
          if (c.client_req_challenge <> Int64.zero) then send_signature c;
        );
      end
       else
      if !verbose_msg_clients then begin
        lprintf_nl () "%s [EPubKeyReq] [DISABLED]" (full_client_identifier c) ;
      end

  | M.EmuleSecIdentStateReq t ->
      if !!enable_sui then 
      begin
      let module Q = M.EmuleSecIdentStateReq in

        if !verbose_msg_clients then begin
          let lstate,lstateString = 
            (match t.Q.state with 
              1 -> (1,"SIGNNEEDED") 
            | 2 -> (2,"KEYANDSIGNNEEDED") 
            | e -> (e,"UNKNOWN")) in
          lprintf_nl () "%s [ESecIdentStateReq] [type: %d (%s)] [reqChall: %Ld] [sendChall: %Ld] [hasKey: %b]" 
            (full_client_identifier c) lstate lstateString t.Q.challenge c.client_sent_challenge (has_pubkey c);
        end;

        c.client_req_challenge <- t.Q.challenge;
        if (not (has_pubkey c)) && (c.client_sent_challenge = Int64.zero) 
          then verify_ident c;
        if (t.Q.state == 2)
          then send_public_key c;
        if (has_pubkey c)
          then send_signature c;

      end else
      if !verbose_msg_clients then begin
        lprintf_nl () "%s [ESecIdentStateReq] [DISABLED]" (full_client_identifier c) ;
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
            if !verbose_location then
              lprintf_nl () "donkeyClient: EmuleRequestSourcesReply: Received %d sources from %s for %s"
                    (Array.length t.Q.sources) (full_client_identifier c) (file_best_name file);
            
            Array.iter (fun s ->
              add_source file s.Q.src_ip s.Q.src_port s.Q.src_server_ip s.Q.src_server_port
            ) t.Q.sources;
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
            if !verbose_location then
              lprintf_nl () "donkeyClient: SourcesReq: Received %d sources from %s for %s"
                    (List.length t.Q.sources) (full_client_identifier c) (file_best_name file);
            List.iter (fun (ip1, port, ip2) ->
                add_source file ip1 port Ip.null 0
            ) t.Q.sources;
        with _ -> ()
      end
  
  | M.SayReq s when (!is_not_spam) s ->
(* FIXME: add logging *)
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
        match file.file_computed_md4s with
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

      (* All clients query chunks during download! This is legitimate!
        !CommonUploads.has_upload = 0 && *)
      (* banned is banned, do we need to check ban_queue_jumpers
         here? besides that ... we shouldn't be connected with
         a banned client! Waste of resources! Or? *)
      if not (!!ban_queue_jumpers && c.client_banned) then
        begin
          try
            let file = find_file t in
            let chunks =
              match file.file_swarmer with
                None ->
                    (* file was found, if we have no swarmer, we have
                       the file complete and share it! it's save to
                       asume that we have all chunks! *)
                    Bitv.create file.file_nchunks true
              | Some swarmer ->
                  let bitmap = Int64Swarmer.verified_bitmap swarmer in
                  Bitv.init (String.length bitmap) 
                      (fun i -> bitmap.[i] = '3')
                  (* This is not very smart, as we might get banned for this request.
                     TODO We should probably check if we don't know already this source...

                     NONSENSE! We don't need to query_file! A peer requesting
                     chunks will always have (part of) that file!
                     We would just have to add it as source ... but I think it was already done!

                  DonkeySources.query_file c.client_source file.file_sources;
                  chunks
                  *)
            in
            client_send c (
              let module Q = M.QueryChunksReply in
              M.QueryChunksReplyReq {
                Q.md4 = file.file_md4;
                Q.chunks = chunks;
              });
          with
            | _ ->
                if !verbose_unexpected_messages then
                  lprintf_nl () "donkeyClient:QueryChunksReq: chunks of unpublished file %s queried from %s"
                    (Md4.to_string t) (full_client_identifier c);
                client_send c ( M.NoSuchFileReq t );
        end
  
  | M.QueryBlocReq t when !CommonUploads.has_upload = 0 &&
    client_has_a_slot (as_client c) ->
      
      if !verbose_upload then
          lprintf_nl () "donkeyClient: uploader %s ask for block" (full_client_identifier c);
      
      let module Q = M.QueryBloc in
      let file = find_file  t.Q.md4 in
      let prio = (file_priority file) in
      let client_upload_lifetime = ref ((maxi 0 !!upload_lifetime) * 60) in
      begin
        
        if !!dynamic_upload_lifetime
            && c.client_uploaded > c.client_downloaded
            && c.client_uploaded > Int64.of_int !!dynamic_upload_threshold ** zone_size
        then
          client_upload_lifetime :=
          Int64.to_int 
            (Int64.of_int !client_upload_lifetime 
              ** c.client_downloaded // c.client_uploaded);
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
      end;
      if !verbose_upload then lprintf_nl () "QueryBloc treated"
      
  | M.NoSuchFileReq t ->
      begin
        try
          let file = find_file t in
          if !verbose_location then
            lprintf_nl () "donkeyClient: NoSuchFileReq: from %s for file %s"
                (full_client_identifier c) (file_best_name file);
          DonkeySources.set_request_result c.client_source 
            file.file_sources File_not_found;
        with _ -> ()
      end
        
  | _ -> 
      if !verbose_unknown_messages then begin
          lprintf_nl () "Unused client message %s:" (full_client_identifier c);
          M.print t;
        end
      
let client_handler c sock event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      disconnect_client c s;

  | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
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
        BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
          close sock Closed_for_timeout
          
      | _ -> ()
      
let init_connection sock ip =
  TcpBufferedSocket.setsock_iptos_throughput sock;
  
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
    
  if !verbose_msg_clients then begin
      lprintf_nl () "Message from incoming client";
      M.print m;
    end;

  match m with
  
  | M.ConnectReq t ->
      if !verbose_msg_clients then begin
        lprintf_nl () "[HELLO] %s" (Ip.to_string (peer_ip sock));
      end;
      
      let module CR = M.Connect in
      
      if not (is_valid_client t.CR.md4 ) then
        begin
          TcpBufferedSocket.close sock (Closed_for_error "Connect of Invalid Client");
          raise Exit
        end;

      if (is_black_address t.CR.ip t.CR.port) then raise Exit;

      let name = ref "" in
      List.iter (fun tag ->
          match tag with
            { tag_name = Field_UNKNOWN "name"; tag_value = String s } -> name := s
          | _ ->  ()
      ) t.CR.tags;

      let kind =
        if low_id t.CR.ip then
            match t.CR.server_info with
            | None ->
                Invalid_address (!name, Md4.to_string t.CR.md4)
            | Some (ip,port) ->
                if Ip.usable ip then
                    Indirect_address (ip, port, id_of_ip t.CR.ip)
                else
                    Invalid_address (!name, Md4.to_string t.CR.md4)
        else
        if Ip.usable t.CR.ip then
            Direct_address (t.CR.ip, t.CR.port)
        else
            Invalid_address  (!name, Md4.to_string t.CR.md4)
      in
      
      let c = new_client kind in
      
      if c.client_debug || !verbose_msg_clients || !verbose_msg_clienttags then begin  
        M.print m;
      end;
      
      Hashtbl.add connected_clients t.CR.md4 c;

      set_client_name c !name t.CR.md4;
      c.client_tags <- t.CR.tags;
      identify_client_brand c;
      update_client_from_tags c t.CR.tags;
      fight_disguised_mods c;
      update_emule_release c;
      begin
        match c.client_source.DonkeySources.source_sock with
        | NoConnection -> 
            c.client_source.DonkeySources.source_sock <- Connection sock;
            c.client_connected <- true;
            init_client sock c;
            init_client_after_first_message sock c
        
        | ConnectionWaiting token -> 
            cancel_token token;
            c.client_source.DonkeySources.source_sock <- Connection sock;
            c.client_connected <- true;
            init_client sock c;
            init_client_after_first_message sock c
        
        | _ -> 
            close sock (Closed_for_error "already connected");
            c.client_connected <- false;
            raise Not_found
      end;

      check_stolen_hash c sock t.CR.md4;
      
      if !!reliable_sources && 
        ip_reliability (peer_ip sock) = Reliability_suspicious 0 then begin
          set_client_state c BlackListedHost;
          raise Not_found
        end;
      
      begin
        match t.CR.server_info with
          Some (ip, port) ->  if !!update_server_list_client then safe_add_server ip port
        | None -> 
            if overnet then begin
                lprintf_nl () "incoming Overnet client";
                DonkeySources.set_source_brand c.client_source overnet;
              end
      end;

      if supports_eep c.client_brand then
        begin
          (* lprintf "Emule Extended Protocol query\n"; *)
          let module M = DonkeyProtoClient in
          let module E = M.EmuleClientInfo in
          client_send c (M.EmuleClientInfoReq emule_info)
        end;
      
      client_send c (
        let module M = DonkeyProtoClient in
        let module C = M.Connect in
        if DonkeySources.source_brand c.client_source then
          M.ConnectReplyReq {
            C.md4 = overnet_md4;
            C.ip = client_ip (Some sock);
            C.port = !!overnet_port;
            C.tags = !overnet_connectreply_tags;
            C.server_info = Some (!overnet_server_ip, !overnet_server_port);
            C.left_bytes = left_bytes;
            C.hash_len = 16;
          }
        else
          begin
            M.ConnectReplyReq {
              C.md4 = !!client_md4;
              C.ip = client_ip (Some sock);
              C.port = !!donkey_port;
              C.tags = !client_to_client_tags;
              C.server_info = Some (get_server_ip_port ());
              C.left_bytes = left_bytes; 
              C.hash_len = 16;
            }
          end;
      );
      
      incr_activity_indirect_connections c;
      
      check_stolen_hash c sock t.CR.md4;
      
      finish_client_handshake c sock;
      Some c
      
  | M.NewUserIDReq _ ->
      lprintf_nl () "NewUserIDReq: "; M.print m; 
      None
  
  | _ -> 
      if !verbose_unknown_messages then
        begin
          lprintf_nl () "BAD MESSAGE FROM CONNECTING CLIENT with ip:%s port:%i overnet:%b"
            (Ip.to_string (peer_ip sock)) (peer_port sock) overnet;
          M.print m; lprint_newline ();
        end;
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
                      
                      let sock = TcpBufferedSocket.connect token "donkey to client" 
                          (Ip.to_inet_addr ip)
                        port 
                          (client_handler c) (*client_msg_to_string*) in
                      
                      
                      incr_activity_connections c;
                                            
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
                      let server_ip, server_port, server_cid = 
                        try
                          let s = DonkeyGlobals.last_connected_master () in
                          match s.server_cid with
                            None -> s.server_ip, s.server_port, Ip.any
                            | Some cid -> s.server_ip, s.server_port, cid
                        with _ -> Ip.localhost, 4665, Ip.any
                      in
                      let send_this_id =
                        if not (!!force_high_id || !!force_client_high_id)
                            && low_id server_cid
                            && Ip.any != server_cid
                            then
                          server_cid
                        else
                          client_ip None
                      in
                      client_send c (
                        let module M = DonkeyProtoClient in
                        let module C = M.Connect in
                        if DonkeySources.source_brand c.client_source then
                          M.ConnectReq {
                            C.md4 = overnet_md4;
                            C.ip = client_ip None;
                            C.port = !!overnet_port;
                            C.tags = !overnet_connect_tags;
                            C.hash_len = 16;
                            C.server_info = Some (!overnet_server_ip, 
                              !overnet_server_port);
                            C.left_bytes = left_bytes;
                          }
                        else
                          M.ConnectReq {
                            C.md4 = !!client_md4;
                            C.ip = send_this_id;
                            C.port = !!donkey_port;
                            C.tags = !client_to_client_tags;
                            C.hash_len = 16;
                            C.server_info = Some (server_ip, server_port);
                            C.left_bytes = left_bytes;
                          }
                      )
                      
                with
      Unix.Unix_error (Unix.ENETUNREACH,_,_) ->
        if !verbose then lprintf_nl () "Network unreachable for IP %s:%d"
          (Ip.to_string ip) port;
                    set_client_disconnected c (Closed_connect_failed);
                    DonkeySources.source_disconnected c.client_source
    | e -> 
                    lprintf_nl () "Exception %s in client connection to IP %s:%d"
                      (Printexc2.to_string e) (Ip.to_string ip) port;
(*                    connection_failed c.client_connection_control; *)
                    set_client_disconnected c (Closed_for_exception e);
                    DonkeySources.source_disconnected c.client_source
            )
          in
          c.client_source.DonkeySources.source_sock <- ConnectionWaiting token
          
  
let query_locations_reply s t =
  let module M = DonkeyProtoServer in
  let module Q = M.QueryLocationReply in
  
  try
    let file = find_file t.Q.md4 in
    let nlocs = List.length t.Q.locs in
    
    if !verbose_location then
        lprintf_nl () "Received %d sources from server %s:%s for %s"
     nlocs (Ip.to_string s.server_ip) (string_of_int s.server_port) (file_best_name file);
    
    s.server_score <- s.server_score + 3;

    (* TODO: verify that new sources are queried as soon as
       possible. Maybe we should check how many new sources
       this client has, and query a connection immediatly if
       they are too many. No need to care about in this
       place ... make need_new_sources based on ready
       sources, then the next refill_file will query them,
       that's soon enough!
     *)

    List.iter (fun l ->
        add_source file l.Q.ip l.Q.port s.server_ip s.server_port
    ) t.Q.locs;
  with Not_found -> ()
      
let client_connection_handler overnet t event =
(*  lprintf "[REMOTE CONN]\n"; *)
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = (Ip.of_inet_addr from_ip) in
      if !DonkeySources.indirect_connections <
         !!max_opened_connections * !!max_indirect_connections / 100 &&
        (match Ip_set.match_ip !Ip_set.bl from_ip with
                   None -> true
                 | Some br ->
                     if !verbose_connect then
                       lprintf "DKOV: %s:%d blocked: %s\n"
                         (Ip.to_string from_ip) from_port br.blocking_description;
                     false) then
        begin
(*          lprintf "+++++++++++++++++++++++++++++++++++++++++++++++\n"; *)
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
              init_connection sock from_ip;
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
                
                with e -> lprintf_nl () "Exception %s in init_connection"
                      (Printexc2.to_string e);
                    );
            with e ->
                lprintf_nl () "Exception %s in client_connection_handler"
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
                  Bitv.init (String.length bitmap) 
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
            let know_file_chunks = List.exists (fun (f,_,_) -> f == file) c.client_file_queue in
            if not know_file_chunks then
              DonkeyProtoCom.client_send c (
                let module M = DonkeyProtoClient in
                  M.QueryChunksReq file.file_md4);
        ignore (DonkeySources.add_request c.client_source 
            file.file_sources (last_time ()))        
      with e -> 
        if !verbose then
          lprintf_nl () "query_source: exception %s" (Printexc2.to_string e)
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

       if low_id ip && Ip.reachable ip then
              query_id ip port id; 
                  
      with e -> 
       if !verbose then begin
         lprintf_nl () "connect_source: exception %s" (Printexc2.to_string e);
       end
  );
  
  
  DonkeySources.functions.DonkeySources.function_max_connections_per_second <-
    (fun () -> !!max_connections_per_second);
  
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
        if !verbose then begin
          lprintf_nl () "add_location: exception %s" (Printexc2.to_string e);
        end
  );
  
  DonkeySources.functions.DonkeySources.function_remove_location <- (fun
      s_uid file_uid ->
      try
        let c = new_client s_uid in
        let file = find_file (Md4.of_string file_uid) in
        CommonFile.file_remove_source (CommonFile.as_file file.file_file)
        (CommonClient.as_client c.client_client);
        
      with e -> 
        if !verbose then
          lprintf_nl () "remove_location for file_md4 %s: exception %s"
	    file_uid (Printexc2.to_string e)
  )
