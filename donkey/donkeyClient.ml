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

open CommonGlobals
open CommonFile
open CommonClient
open CommonComplexOptions
open Gui_proto
open CommonResult
open CommonTypes
open Options
open BasicSocket
open DonkeyMftp
open Mftp_comm
open TcpBufferedSocket
open DonkeyTypes  
open DonkeyOneFile
open DonkeyOptions
open CommonOptions
open DonkeyComplexOptions
open DonkeyGlobals

let client_send_if_possible sock msg =
  if can_write_len sock (!!client_buffer_size/2) then
    client_send sock msg
  
let verbose_group = ref false

let tag_udp_client = 203
  
let new_udp_client c group =
  match c.client_kind with
    Indirect_location _ -> ()
  | Known_location (ip, port) ->
      let uc =
        try
          let uc = Hashtbl.find udp_clients c.client_kind in
          uc.udp_client_last_conn <- last_time ();
          uc
        with _ ->
            let uc = {
                udp_client_last_conn = last_time ();
                udp_client_ip = ip;
                udp_client_port = port;
                udp_client_is_mldonkey = c.client_is_mldonkey >= 2;
              }
            in
            Heap.set_tag uc tag_udp_client;
            Hashtbl.add udp_clients c.client_kind uc;
            uc
      in          
      group.group <- UdpClientMap.add c.client_kind uc group.group

      
let udp_sock () =
  match !udp_sock with
    None -> failwith "No UDP socket"
  | Some sock -> sock
      
let udp_client_send uc t =
  Mftp_comm.udp_send_if_possible (udp_sock ()) upload_control 
    (Unix.ADDR_INET (Ip.to_inet_addr uc.udp_client_ip,uc.udp_client_port+4))
  t

let client_udp_send ip port t =
  Mftp_comm.udp_send_if_possible (udp_sock ()) upload_control
  (Unix.ADDR_INET (Ip.to_inet_addr ip,port+4))
  t

let find_sources_in_groups c md4 =
  if !!propagate_sources then
    try
      let group = Hashtbl.find file_groups md4 in
      try
        let uc = UdpClientMap.find c.client_kind group.group in
        uc.udp_client_last_conn <- last_time ()
(* the client is already known *)
      with _ -> 
(* a new client for this group *)
          if c.client_is_mldonkey >= 2 then begin
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
                      let msg = client_msg (
                          let module Q = Mftp_client.Sources in
                          Mftp_client.SourcesReq {
                            Q.md4 = md4;
                            Q.sources = !list;
                          }
                        ) in
                    client_send_if_possible sock msg 
                  end
          end else
          begin
            match c.client_kind with 
              Indirect_location _ -> ()
            | Known_location (ip, port) ->
                UdpClientMap.iter (fun _ uc ->
                    if !verbose_group then
                      (Printf.printf "Send source from file groups to UDP peer";
                        print_newline ());
                    client_udp_send ip port (
                      let module Q = Mftp_server.QueryLocationReply in
                      Mftp_server.QueryLocationReplyUdpReq {
                        Q.md4 = md4;
                        Q.locs = [{ 
                            Q.ip = uc.udp_client_ip;
                            Q.port = uc.udp_client_port;
                          }];
                      })
                ) group.group;                  
          end
          ;
        match c.client_kind with 
          Indirect_location _ -> ()
        | Known_location (ip, port) ->
(* send this client as a source for the file to
all mldonkey clients in the group. add client to group *)
            
            UdpClientMap.iter (fun _ uc ->
                if !verbose_group then
                  (Printf.printf "Send new source to file groups UDP peers"; 
                    print_newline ());
                udp_client_send uc (
                  let module M = Mftp_server in
                  M.QueryLocationReplyUdpReq (
                    let module Q = M.QueryLocationReply in
                    {
                      Q.md4 = md4;
                      Q.locs = [{ Q.ip = ip; Q.port = port }];
                    }))
            ) group.group;
            new_udp_client c group
  with _ ->
      let group = { group = UdpClientMap.empty } in
      Hashtbl.add file_groups md4 group;
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
  if md4 <> Md4.null && md4 <> Md4.one then begin
      find_sources_in_groups c md4;
    end
        
  
let new_chunk up begin_pos end_pos =
  if begin_pos <> end_pos then
    let pair = (begin_pos, end_pos) in
    match up.up_chunks with
      [] ->
        up.up_pos <- begin_pos;
        up.up_end_chunk <- end_pos;
        up.up_chunks <- [pair]
    | chunks ->
        if not (List.mem pair chunks) then
          up.up_chunks <- chunks @ [pair]
  
let rec really_write fd s pos len =
  if len = 0 then begin
      Printf.printf "really_write 0 BYTES !!!!!!!!!"; print_newline ();
      raise End_of_file
    end else
  let nwrite = Unix.write fd s pos len in
  if nwrite = 0 then raise End_of_file else
  if nwrite < len then 
    really_write fd s (pos + nwrite) (len - nwrite)
  

let identify_as_mldonkey c sock =
  direct_client_send sock (
    let module M = Mftp_client in
    let module C = M.QueryFile in
    M.QueryFileReq Md4.null);
  direct_client_send sock (
    let module M = Mftp_client in
    let module C = M.QueryFile in
    M.QueryFileReq Md4.one)
  
  
let query_files c sock for_files =  
  List.iter (fun file ->
      let msg = client_msg (
          let module M = Mftp_client in
          let module C = M.QueryFile in
          M.QueryFileReq file.file_md4)
      in
      client_send sock msg;
  ) for_files;
  List.iter (fun file ->
      direct_client_send sock (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq file.file_md4);          
  ) !current_files;
  ()

  
let client_has_chunks c file chunks =
  
  if not (Intmap.mem (client_num c) file.file_sources) then begin
      new_source file c;
      file.file_new_locations <- true
    end;
  
  if file.file_chunks_age = [||] then
    file.file_chunks_age <- Array.create file.file_nchunks 0.0;
  for i = 0 to file.file_nchunks - 1 do
    if chunks.(i) then file.file_chunks_age.(i) <- last_time ()
  done;
  
  add_client_chunks file chunks;
  
  match c.client_file_queue with
    [] ->
      c.client_file_queue <- [file, chunks];
      start_download c
  
  | _ -> 
      c.client_file_queue <- c.client_file_queue @ [file, chunks]

let add_new_location sock file c =
  
  let is_new = ref false in
  let module M = Mftp_client in
  if not (Intmap.mem (client_num c) file.file_sources) then begin
      new_source file c; 
      is_new := true;
      file.file_new_locations <- true;
    
    end;  
  
  if last_time () -.               
    connection_last_conn c.client_connection_control < 300.
      && !!propagate_sources then
    let send_locations = ref 10 in

(* Send the known sources to the client. Use UDP for normal clients, TCP
for mldonkey clients .*)
    begin
      match c.client_kind with
        Known_location (ip, port) ->
          (try
(* send this location to at most 10 other locations, 
  connected in the last quarter *)
              let counter = ref 10 in

(* send this location to  all connected locations *)
              let msg = client_msg (
                  let module Q = M.Sources in
                  M.SourcesReq {
                    Q.md4 = file.file_md4;
                    Q.sources = [(ip, port, ip)];
                  })                    
              in
              let time = last_time () -. 900. in
              Intmap.iter (fun _ cc ->
                  if cc.client_checked &&
                    connection_last_conn c.client_connection_control  
                      > time then begin
                      match cc.client_kind with
                        Known_location (src_ip, src_port) -> 
                          decr counter;
                          if !counter = 0 then raise Exit;
                          
                          client_udp_send src_ip src_port (
                            let module Q = Mftp_server.QueryLocationReply in
                            Mftp_server.QueryLocationReplyUdpReq {
                              Q.md4 = file.file_md4;
                              Q.locs = [{ 
                                  Q.ip = ip;
                                  Q.port = port;
                                }];
                            });
                      | _ -> 
                          if cc.client_is_mldonkey > 1 then
                            match cc.client_sock with 
                              None -> ()
                            | Some sock -> client_send_if_possible sock msg
                                
                  end
            ) file.file_sources;
          with _ -> ());

      | _ -> ()
    end;
  begin
    match c.client_kind with
      Indirect_location _ when c.client_is_mldonkey < 2 -> ()
    | _ ->
        
(* send at most 10 sources connected in the last quarter to this new location *)
        let sources = ref [] in
        let time = last_time () -. 900. in
        (try (* simply send the last found location *)
            Intmap.iter (fun _ c ->
                if c.client_checked &&
                  connection_last_conn c.client_connection_control > time
                then
                  match c.client_kind with
                    Known_location (ip, port) -> 
                      sources := (ip, port, ip) :: !sources;
                      decr send_locations;
                      if !send_locations = 0 then
                        raise Not_found
                  | _ -> ()
            ) file.file_sources;
          with _ -> ());
        
        if !sources <> [] then
          if c.client_is_mldonkey = 2 then begin
              client_send_if_possible sock (client_msg (
                let module Q = M.Sources in
                M.SourcesReq {
                  Q.md4 = file.file_md4;
                  Q.sources = !sources;
                }))
            end else begin
              match c.client_kind with
                Indirect_location _ -> ()
              | Known_location (ip, port) ->
                  List.iter (fun (src_ip, src_port, _ ) ->
                      client_udp_send ip port (
                        let module Q = Mftp_server.QueryLocationReply in
                        Mftp_server.QueryLocationReplyUdpReq {
                          Q.md4 = file.file_md4;
                          Q.locs = [{ 
                              Q.ip = src_ip;
                              Q.port = src_port;
                            }];
                        });                    
                  ) !sources;
            end;
  end                  
  

      
let read_first_message t sock =
  let module M = Mftp_client in
  match t with

  | M.ConnectReq t ->
      printf_string "******* [PCONN OK] ********";
      
      let module CR = M.Connect in
      
      if t.CR.md4 = !!client_md4 then begin
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
        | Some _ -> 
            close sock "already connected"; raise Not_found
      end;

      set_client_name c !name t.CR.md4;
      connection_ok c.client_connection_control;
      c.client_tags <- t.CR.tags;
      
      if Ip.valid t.CR.ip_server then
        ignore (add_server t.CR.ip_server t.CR.port_server);
      
      direct_client_send sock (
        let module M = Mftp_client in
        let module C = M.ConnectReply in
        M.ConnectReplyReq {
          C.md4 = !!client_md4;
          C.ip = !!client_ip;
          C.port = !client_port;
          C.tags = !client_tags;
          C.ip_server = t.CR.ip_server;
          C.port_server = t.CR.port_server;
        }
      );
      
      set_client_state c Connected_idle;      
      identify_as_mldonkey c sock;
      query_files c sock [];
      if client_is_friend c then
        if last_time () > c.client_next_view_files then begin
            Printf.printf "****************************************";
            print_newline ();
            Printf.printf "       ASK VIEW FILES         ";
            print_newline ();

            direct_client_send sock (
              let module M = Mftp_client in
              let module C = M.ViewFiles in
              M.ViewFilesReq C.t);          
            end;
      client_must_update c;
      Some c
      
  | M.NewUserIDReq _ ->
      M.print t; print_newline ();
      None
      
  | _ -> 
      Printf.printf "BAD MESSAGE FROM CONNECTING CLIENT"; print_newline ();
      M.print t; print_newline ();
      close sock "bad connecting message";
      raise Not_found
      
let client_to_client for_files c t sock = 
  if !ip_verified < 10 then verify_ip sock;
  let module M = Mftp_client in
  match t with
    M.ConnectReplyReq t ->
      printf_string "******* [CCONN OK] ********"; 
      
      c.client_checked <- true;
      
      let module CR = M.ConnectReply in
      
      if t.CR.md4 = !!client_md4 then
        TcpBufferedSocket.close sock "connected to myself";

      c.client_tags <- t.CR.tags;
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> 
              set_client_name c s t.CR.md4
          | _ -> ()
      ) c.client_tags;
      
      connection_ok c.client_connection_control;
      
      if Ip.valid t.CR.ip_server then
        ignore (add_server t.CR.ip_server t.CR.port_server);
      
      set_client_state c Connected_idle;
      
      identify_as_mldonkey c sock;
      query_files c sock for_files;
      client_must_update c;      
      if client_is_friend c then begin
          if last_time () > c.client_next_view_files then begin
            Printf.printf "****************************************";
            print_newline ();
            Printf.printf "       ASK VIEW FILES         ";
            print_newline ();
              direct_client_send sock (
                let module M = Mftp_client in
                let module C = M.ViewFiles in
                M.ViewFilesReq C.t);          
            end
        end
  
  | M.ViewFilesReplyReq t ->
      Printf.printf "****************************************";
      print_newline ();
      Printf.printf "       VIEW FILES REPLY         ";
      print_newline ();

      let module Q = M.ViewFilesReply in
      begin
        try
          c.client_all_files <- Some (List.map (fun f ->
                let tags =  f.f_tags in
                let md4 = f.f_md4 in
                let rec r = { 
                    result_num = 0;
                    result_network = network.network_num;
                    result_md4 = md4;
                    result_names = [];
                    result_size = Int32.zero;
                    result_tags = [];
                    result_type = "";
                    result_format = "";
                    result_comment = None;
                    result_done = false;
                  } in
                List.iter (fun tag ->
                    match tag with
                      { tag_name = "filename"; tag_value = String s } ->
                        r.result_names <- s :: r.result_names
                    | { tag_name = "size"; tag_value = Uint32 v } ->
                        r.result_size <- v;
                    | { tag_name = "format"; tag_value = String s } ->
                        r.result_tags <- tag :: r.result_tags;
                        r.result_format <- s
                    | { tag_name = "type"; tag_value = String s } ->
                        r.result_tags <- tag :: r.result_tags;
                        r.result_type <- s
                    | _ ->
                        r.result_tags <- tag :: r.result_tags
                ) tags;
                let r = DonkeyIndexer.index_result_no_filter r in
                client_new_file c r;
                r
            ) t);
          client_must_update c
        
        with e ->
            Printf.printf "Exception in ViewFilesReply %s"
              (Printexc.to_string e); print_newline ();
      end;
    
  | M.AvailableSlotReq _ ->
      printf_string "[QUEUED]";
      set_rtimeout sock !!queued_timeout; 
(* how long should we wait for a block ? *)
      begin
        match c.client_block with
          None -> find_client_block c
        | Some b ->
            printf_string "[QUEUED WITH BLOCK]";      
      end
(*
  | M.QueueReq t ->
      
      printf_char 'Q';
      List.iter (fun ip ->
          let c = new_client (Known_location (ip, 4662)) in
          if not (List.memq c !interesting_clients) then
            interesting_clients := c :: !interesting_clients
      ) t;
      
      set_client_state c Connected_queued;
      client_send sock (
        let module M = Mftp_client in
        M.QueueReq t);              
*)  
  
  | M.JoinQueueReq _ ->
      if Fifo.length upload_clients < !!max_upload_slots then begin
          set_rtimeout sock !!upload_timeout;
          
          direct_client_send sock (
            let module M = Mftp_client in
            let module Q = M.AvailableSlot in
            M.AvailableSlotReq Q.t);
        end
  
  | M.CloseSlotReq _ ->
      printf_string "[DOWN]"
  
  | M.ReleaseSlotReq _ ->
      direct_client_send sock (
        let module M = Mftp_client in
        let module Q = M.CloseSlot in
        M.CloseSlotReq Q.t);
  
  | M.QueryFileReplyReq t ->
      let module Q = M.QueryFileReply in
      
      begin
        try
          let file = find_file t.Q.md4 in
          
          printf_string "[FOUND FILE]";
          if not (List.mem t.Q.name file.file_filenames) then 
            file.file_filenames <- file.file_filenames @ [t.Q.name] ;
          if file.file_size > block_size then
            direct_client_send sock (
              let module M = Mftp_client in
              let module C = M.QueryChunks in
              M.QueryChunksReq file.file_md4)
          else
            client_has_chunks c file [| true |]
        with _ -> ()
      end  
  
  | M.QueryChunksReplyReq t ->
      
      let module Q = M.QueryChunksReply in      
      let file = find_file t.Q.md4 in
      let chunks = 
        if t.Q.chunks = [||] then
          Array.create file.file_nchunks true
        else
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
        if t.Q.chunks = [||] then
          file.file_md4s <- [file.file_md4]
        else
          file.file_md4s <- Array.to_list (t.Q.chunks);

(*      if file.file_exists then verify_chunks file *)
      end
  
  | M.BlocReq t ->
      let module Q = M.Bloc in
      let file = client_file c in
      
      if file.file_md4 <> t.Q.md4 then begin
          Printf.printf "BLOC FOR BAD FILE %s/%s !!"
            (Md4.to_string t.Q.md4) (Md4.to_string file.file_md4); 
          print_newline ();
          raise Not_found
        end;
      
      if file_state file = FilePaused then 
        (next_file c; raise Not_found);
      
      let begin_pos = t.Q.start_pos in
      let end_pos = t.Q.end_pos in
      
      set_client_state c Connected_busy;
      let len = Int32.sub end_pos begin_pos in
      download_counter := !download_counter + Int32.to_int len;
      c.client_rating <- Int32.add c.client_rating len;      
      begin
        match c.client_block with
          None -> 
            Printf.printf "NO BLOCK EXPECTED FROM CLIENT"; print_newline ();
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
              
              Printf.printf "%s-%s (%s-%s)" 
                (Int32.to_string begin_pos) (Int32.to_string end_pos)
              (Int32.to_string b.block_begin) (Int32.to_string b.block_end)
              ;
              print_newline ();
              
              List.iter (fun z ->
                  Printf.printf "zone: %s-%s"
                    (Int32.to_string z.zone_begin) (Int32.to_string z.zone_end)
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
            
            let final_pos = Unix32.seek32 file.file_fd 
                begin_pos Unix.SEEK_SET in
            if final_pos <> begin_pos then begin
                Printf.printf "BAD LSEEK %s/%s"
                  (Int32.to_string final_pos)
                (Int32.to_string begin_pos); print_newline ();
                raise Not_found
              end;
            printf_char '#';
(*            if !!verbose then begin
                Printf.printf "{%d-%d = %s-%s}" (t.Q.bloc_begin)
                (t.Q.bloc_len) (Int32.to_string begin_pos) 
                (Int32.to_string end_pos);
                print_newline ();
              end; *)
            try
              let fd = try
                  Unix32.force_fd file.file_fd 
                with e -> 
                    Printf.printf "In Unix32.force_fd"; print_newline ();
                    raise e
              in
              really_write fd t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len;
              List.iter (update_zone file begin_pos end_pos) c.client_zones;
              find_client_zone c;                    
            with
              End_of_file ->
                Printf.printf "WITH CLIENT %s" c.client_name;
                print_newline ();
            | e ->
                Printf.printf "Error %s while writing block. Pausing download" (Printexc.to_string e);
                print_newline ();
                file_pause (as_file file.file_file);
                info_change_file file
      
      end;


(* Upload requests *)
  | M.ViewFilesReq t when !has_upload = 0 -> 
      let files = DonkeyShare.all_shared () in
      
      direct_client_send sock (
        let module Q = M.ViewFilesReply in
        M.ViewFilesReplyReq (DonkeyShare.make_tagged files))
  
  
  | M.QueryFileReq t when !has_upload = 0 -> 
      
      (try client_wants_file c t with _ -> ());
      if t = Md4.null && c.client_is_mldonkey = 0 then 
        c.client_is_mldonkey <- 1;
      if t = Md4.one && c.client_is_mldonkey = 1 then  begin
          printf_string "[MLDONKEY]";
          c.client_is_mldonkey <- 2;
        end;
      
      begin try
          let file = find_file t in
          file.file_upload_requests <- file.file_upload_requests + 1;
          direct_client_send sock (
            let module Q = M.QueryFileReply in
            M.QueryFileReplyReq {
              Q.md4 = file.file_md4;
              Q.name = match file.file_filenames with
                [] -> file.file_hardname
              | name :: _ -> name
            });

          add_new_location sock file c
                            
        with _ -> () end    
      
  | M.SourcesReq t ->
      
      let module Q = M.Sources in
      begin
        try
          let file = find_file t.Q.md4 in
          List.iter (fun (ip1, port, ip2) ->
              if Ip.valid ip1 then
                let c = new_client (Known_location (ip1, port)) in
                if not (Intmap.mem (client_num c) file.file_sources) then
                  new_source file c;
                clients_list := (c, [file]) :: !clients_list;
                incr clients_list_len;
          ) t.Q.sources
        with _ -> ()
      end

  | M.SayReq s ->
      
      let ad_opt = 
      match c.client_kind with
	Known_location (ip, port) -> 
	  (
	   match c.client_chat_port with
	     None -> None
	   | Some p ->Some (Ip.to_string ip, p)
	  )
      |	Indirect_location _ -> None
      in
      CommonChat.send_text c.client_name ad_opt s
      (* A VOIR : historique à gérer *) (*!say_hook c s*)
      
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
  
  | M.QueryChunksReq t when !has_upload = 0 ->

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
  
  | M.QueryBlocReq t when !has_upload = 0 -> 

      set_rtimeout sock !!upload_timeout;
      let module Q = M.QueryBloc in
      let file = find_file  t.Q.md4 in

      let up = match c.client_upload with
          Some ({ up_file = f } as up) when f == file ->  up
        | _ ->
            {
              up_file = file;
              up_pos = Int32.zero;
              up_end_chunk = Int32.zero;
              up_chunks = [];
              up_waiting = false;
            }
      in
      new_chunk up t.Q.start_pos1 t.Q.end_pos1;
      new_chunk up t.Q.start_pos2 t.Q.end_pos2;
      new_chunk up t.Q.start_pos3 t.Q.end_pos3;
      c.client_upload <- Some up;
      if not up.up_waiting && !has_upload = 0 then begin
          Fifo.put upload_clients c;
          up.up_waiting <- true
        end

  | _ -> ()

let client_handler c sock event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      connection_failed c.client_connection_control;
      disconnected_from_client c s
  
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
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  set_rtimeout sock !!client_timeout

let init_client sock c file =
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
      printf_string "-!C";
      connection_delay c.client_connection_control;
      close s "timeout"
  );
  c.client_block <- None;
  c.client_zones <- [];
  c.client_file_queue <- []
  
let reconnect_client cid files c =
  if can_open_connection () then
    match c.client_kind with
      Indirect_location _ -> ()
    | Known_location (ip, port) ->
        try
          set_client_state c Connecting;
          connection_try c.client_connection_control;
          
          printf_string "?C";
          let sock = TcpBufferedSocket.connect "donkey to client" (
              Ip.to_inet_addr ip) 
            port 
              (client_handler c) (*client_msg_to_string*) in
          
          init_connection sock;
          init_client sock c files;
          
          set_reader sock (Mftp_comm.cut_messages Mftp_client.parse
            (client_to_client files c));
          
          c.client_sock <- Some sock;
          let server_ip, server_port = 
            try
              let s = DonkeyServers.last_connected_server () in
              s.server_ip, s.server_port
            with _ -> Ip.localhost, 4665
          in
          direct_client_send sock (
            let module M = Mftp_client in
            let module C = M.Connect in
            M.ConnectReq {
              C.md4 = !!client_md4; (* we want a different id each conn *)
              C.ip = cid;
              C.port = !client_port;
              C.tags = !client_tags;
              C.version = 16;
              C.ip_server = server_ip;
              C.port_server = server_port;
            }
          )
        
        with e -> 
            Printf.printf "Exception %s in client connection"
              (Printexc.to_string e);
            print_newline ();
            connection_failed c.client_connection_control;
            set_client_state c NotConnected
            
let connect_client cid files c = 
  match c.client_sock with
    None -> 
      if connection_can_try c.client_connection_control then 
        reconnect_client cid files c
  | Some sock ->
      match client_state c with
        Connected_idle -> 
          if can_fill sock then
            query_files c sock files
      | _ -> ()
        
let query_id_reply s t =
  let module M = Mftp_server in
  let module Q = M.QueryIDReply in
  let c = new_client (Known_location (t.Q.ip, t.Q.port)) in
  connect_client s [] c
      
let query_id s sock ip =
  printf_string "[QUERY ID]";
  direct_server_send sock (
    let module M = Mftp_server in
    let module C = M.QueryID in
    M.QueryIDReq ip
  )

let udp_server_send s t =
  Mftp_comm.udp_send_if_possible (udp_sock ()) upload_control
  (Unix.ADDR_INET (Ip.to_inet_addr s.server_ip,s.server_port+4))
  t
  
let query_locations_reply s t =
  let module M = Mftp_server in
  let module Q = M.QueryLocationReply in
  
  let nlocs = List.length t.Q.locs in
  s.server_score <- s.server_score + 3;
  
  List.iter (fun l ->
      let ip = l.Q.ip in
      let port = l.Q.port in
      
      if Ip.valid ip then
        let c = new_client (Known_location (ip, port)) in
        connect_client s.server_cid [] c
      else
      match s.server_sock with
        None ->
          let module Q = M.QueryCallUdp in
          udp_server_send s 
            (M.QueryCallUdpReq {
              Q.ip = !!client_ip;
              Q.port = !client_port;
              Q.id = ip;
            })

      | Some sock ->
          query_id s sock ip
  ) t.Q.locs
      
let query_locations file s sock =
  printf_string "[QUERY LOC]";
  direct_server_send sock (
    let module M = Mftp_server in
    let module C = M.QueryLocation in
    M.QueryLocationReq file.file_md4
  )
  
let client_connection_handler t event =
  printf_string "[REMOTE CONN]";
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      
      begin
        try
          let c = ref None in
          let sock = 
            TcpBufferedSocket.create "donkey client connection" s (client_handler2 c) 
(*client_msg_to_string*)
          in
          (try
              set_reader sock 
                (Mftp_comm.client_handler2 c read_first_message
                (client_to_client []));
              
            with e -> Printf.printf "Exception %s in init_connection"
                  (Printexc.to_string e);
                print_newline ());
        with e ->
            Printf.printf "Exception %s in client_connection_handler"
              (Printexc.to_string e);
            print_newline ();
            Unix.close s
      end      
  | _ -> 
      ()      
      
