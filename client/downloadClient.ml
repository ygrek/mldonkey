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

(*
 Quand un client se connecte, on mute les champs:
   client_state
   on met client_handler comme handler du socket
   on appelle init_connection c sock
   on modifie client_sock  
*)

open Options
open BasicSocket
open Mftp
open Mftp_comm
open TcpClientSocket
open DownloadTypes  
open DownloadOneFile
open DownloadOptions
open DownloadComplexOptions
open DownloadGlobals
open Gui_types
  
let file_groups = Hashtbl.create 1023
let udp_clients = Hashtbl.create 1023
  
let new_udp_client c group =
  match c.client_kind with
    Indirect_location -> ()
  | Known_location (ip, port) ->
      let uc =
        try
          Hashtbl.find udp_clients c.client_kind
        with _ ->
            let uc = {
                udp_client_ip = ip;
                udp_client_port = port;
                udp_client_is_mldonkey = c.client_is_mldonkey >= 2;
              }
            in
            Hashtbl.add udp_clients c.client_kind uc;
            uc
      in          
      group.group <- UdpClientMap.add c.client_kind uc group.group

      
let udp_client_send uc t =
  Mftp_comm.udp_send (match !udp_sock with
      None -> failwith "No UDP socket"
    | Some sock -> sock)  
  (Unix.ADDR_INET (Ip.to_inet_addr uc.udp_client_ip,uc.udp_client_port+4))
  t

let client_wants_file c md4 =
  if md4 <> Md4.null && md4 <> Md4.one then 
    try
      let group = Hashtbl.find file_groups md4 in
      try
        ignore (UdpClientMap.find c.client_kind group.group);
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
                      Printf.printf "Found sources in file groups"; print_newline ();                  
                      client_send sock (
                        let module Q = Mftp_client.Sources in
                        Mftp_client.SourcesReq {
                          Q.md4 = md4;
                          Q.sources = !list;
                        }
                      )
                    end
            end;
          match c.client_kind with 
            Indirect_location -> ()
          | Known_location (ip, port) ->
(* send this client as a source for the file to
all mldonkey clients in the group. add client to group *)
              
              UdpClientMap.iter (fun _ uc ->
                  if uc.udp_client_is_mldonkey then
                    udp_client_send uc (
                      let module M = Mftp_server in
                      M.FileGroupInfoUdpReq (
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
  let nwrite = Unix.write fd s pos len in
  if nwrite = 0 then raise End_of_file else
  if nwrite < len then 
    really_write fd s (pos + nwrite) (len - nwrite)
  

let identify_as_mldonkey c sock =
  client_send sock (
    let module M = Mftp_client in
    let module C = M.QueryFile in
    M.QueryFileReq Md4.null);
  client_send sock (
    let module M = Mftp_client in
    let module C = M.QueryFile in
    M.QueryFileReq Md4.one)
  
  
let query_files c sock for_files =  
(*  Printf.printf "QUERY FILES ........."; print_newline (); *)
  List.iter (fun file ->
      client_send sock (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq file.file_md4);
  ) for_files;
  List.iter (fun file ->
      client_send sock (
        let module M = Mftp_client in
        let module C = M.QueryFile in
        M.QueryFileReq file.file_md4);          
  ) !!files;
(*  Printf.printf ".... DONE"; print_newline *)
  ()
  
let real_client c = 
  match c.client_alias with
    None -> c
  | Some c -> c
  
let client_to_client for_files c t sock = 
  let c = real_client c in
  let module M = Mftp_client in
(*  M.print t; print_newline (); *)
  match t with
    M.ConnectReplyReq t ->
      printf_string "[CCONN OK]";
      
      let module CR = M.ConnectReply in
      
      if t.CR.md4 = !!client_md4 then
        TcpClientSocket.close sock "connected to myself";
      
      c.client_md4 <- t.CR.md4;
      connection_ok c.client_connection_control;
      c.client_tags <- t.CR.tags;
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> 
              c.client_name <- s
          | _ -> ()
      ) c.client_tags;
      
      ignore (add_server t.CR.ip_server t.CR.port_server);
      
      set_client_state c Connected_idle;
      
      identify_as_mldonkey c sock;
      query_files c sock for_files;
      begin
        try
          Hashtbl.find indirect_friends (c.client_name, c.client_md4);
          c.client_is_friend <- Friend
        with _ -> ()
      end;
      !client_change_hook c;      
      begin
        match c.client_is_friend with
          Friend ->
(*            Printf.printf "******* Asking for files ********"; print_newline (); *)            
            if last_time () > c.client_next_view_files then begin
                client_send sock (
                  let module M = Mftp_client in
                  let module C = M.ViewFiles in
                  M.ViewFilesReq C.t);          
              end
        | _ -> ()
      end
  
  | M.ViewFilesReplyReq t ->
(*
      Printf.printf "******* ViewFilesReplyReq ******* %d" c.client_num; 
print_newline (); 
  *)
      let module Q = M.ViewFilesReply in
      begin
        try
          c.client_all_files <- Some (List.map (fun f ->
                let tags =  f.f_tags in
                let md4 = f.f_md4 in
                let r = { 
                    result_md4 = md4;
                    result_names = [];
                    result_size = Int32.zero;
                    result_tags = [];
                    result_type = "";
                    result_format = "";
                    result_filtered_out = 0;
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
                DownloadIndexer.index_result_no_filter r
            ) t);
          c.client_changed <- BigChange;
          !client_change_hook c
        
        with e ->
            Printf.printf "Exception in ViewFilesReply %s"
              (Printexc.to_string e); print_newline ();
      end;
  
  | M.ConnectReq t ->
      printf_string "[CCONN OK]";
      
      let module CR = M.Connect in
      
      let c =
        if c.client_kind <> Indirect_location then c else
        if Ip.valid t.CR.ip then
          let kind = Known_location (t.CR.ip, t.CR.port) in
          try
            let cc = Hashtbl.find clients_by_kind kind in
            match cc.client_sock with
            | Some _ -> c
            | None -> 
                if cc.client_md4 = Md4.null || 
                  t.CR.md4 = cc.client_md4 then begin
(*
                Printf.printf "Aliasing indirect connection to known client";
                print_newline ();
*)
                    
                    cc.client_sock <- c.client_sock;
                    c.client_alias <- Some cc;
                    c.client_kind <- kind;
                    Hashtbl.remove clients_by_num c.client_num;
                    Hashtbl.add clients_by_num c.client_num cc;
                    cc
                  end else c
          with _ -> 
(* unknown location *)
              c.client_kind <- kind;
              Hashtbl.add clients_by_kind kind c;
              if c.client_is_friend = Friend then
                known_friends =:= c :: !!known_friends;
              c
        else c
      in

(* what about being connected several times to the same client ? 
We should probably check that here ... *)  
      c.client_md4 <- t.CR.md4;
      
      if t.CR.md4 = !!client_md4 then begin
          TcpClientSocket.close sock "connected to myself";
          raise End_of_file
        end;
      begin
        if c.client_kind = Indirect_location then 
          try
            Hashtbl.find indirect_clients_by_md4 t.CR.md4;
            Printf.printf "We are already connected to this client... disconnect"; print_newline ();
(* WE ARE ALREADY CONNECTED TO THIS CLIENT !!! *)
            shutdown sock "already connected to client";
            raise End_of_file
          with Not_found -> 
              Hashtbl.add indirect_clients_by_md4 t.CR.md4 ()
      end;
      
      connection_ok c.client_connection_control;
      c.client_tags <- t.CR.tags;
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String s } -> 
              c.client_name <- s
          | _ -> ()
      ) c.client_tags;
      
      ignore (add_server t.CR.ip_server t.CR.port_server);
      
      client_send sock (
        let module M = Mftp_client in
        let module C = M.ConnectReply in
        M.ConnectReplyReq {
          C.md4 = !!client_md4;
          C.ip = !client_ip;
          C.port = !client_port;
          C.tags = !client_tags;
          C.ip_server = t.CR.ip_server;
          C.port_server = t.CR.port_server;
        }
      );
      
      c.client_state <- Connected_idle;
      
      identify_as_mldonkey c sock;
      query_files c sock for_files;
      
      begin
        try
          Hashtbl.find indirect_friends (c.client_name, c.client_md4);
          c.client_is_friend <- Friend
        with _ -> ()
      end;
      !client_change_hook c;      
      begin
        match c.client_is_friend with
          Friend ->
            if last_time () > c.client_next_view_files then begin
                client_send sock (
                  let module M = Mftp_client in
                  let module C = M.ViewFiles in
                  M.ViewFilesReq C.t);          
              end
        | _ -> ()
      end
  
  | M.AvailableSlotReq _ ->
      printf_string "[QUEUED]";
      set_rtimeout (TcpClientSocket.sock sock) infinite_timeout;
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
      
      client_send sock (
        let module M = Mftp_client in
        let module Q = M.AvailableSlot in
        M.AvailableSlotReq Q.t);              
  
  | M.CloseSlotReq _ ->
      printf_string "[DOWN]"
  
  | M.ReleaseSlotReq _ ->
      client_send sock (
        let module M = Mftp_client in
        let module Q = M.CloseSlot in
        M.CloseSlotReq Q.t);              

(*
  | M.OtherLocationsReq t ->
      List.iter (fun ip ->
          let c = new_client (Known_location (ip, 4662)) in
          if not (List.memq c !interesting_clients) then
            interesting_clients := c :: !interesting_clients
      ) t
*)
  
  | M.QueryFileReplyReq t ->
      let module Q = M.QueryFileReply in
      
      begin
        try
          let file = find_file t.Q.md4 in
          
          printf_string "[FOUND FILE]";
          if not (List.mem t.Q.name file.file_filenames) then 
            file.file_filenames <- file.file_filenames @ [t.Q.name] ;
          client_send sock (
            let module M = Mftp_client in
            let module C = M.QueryChunks in
            M.QueryChunksReq file.file_md4)
        with _ -> ()
      end  
  
  | M.QueryChunksReplyReq t ->
      
      let module Q = M.QueryChunksReply in      
      begin
        try
          let file = find_file t.Q.md4 in
          
          begin
            match c.client_kind with
              Known_location _ ->
                if not (List.memq c file.file_known_locations) then begin
                    file.file_changed <- BigChange;
                    file.file_known_locations <- c :: file.file_known_locations
                  end
            | _ ->
                if not (List.memq c file.file_indirect_locations) then
                  file.file_indirect_locations <- 
                    c :: file.file_indirect_locations
          end;
          
          
          let chunks =
            if t.Q.chunks = [||] then
              Array.create file.file_nchunks true
            else
              t.Q.chunks in
          
          add_client_chunks file chunks;
          
          match c.client_files with
            [] ->
              c.client_files <- [file, chunks];
              start_download c
          
          | _ -> 
              c.client_files <- c.client_files @ [file, chunks];
        with _ -> ()
      end;
  
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

      if file.file_state = FilePaused then begin
          next_file c
        end
      else
      
      let begin_pos = t.Q.start_pos in
      let end_pos = t.Q.end_pos in
      
      set_client_state c Connected_busy;
      let len = Int32.sub end_pos begin_pos in
      download_counter := !download_counter + Int32.to_int len;
      c.client_rating <- Int32.add c.client_rating len;      
      begin
        match c.client_block with
          None -> ()
        | Some b ->
            let str_begin = Int32.of_int t.Q.bloc_begin in
            
            if begin_pos < b.block_begin
                || begin_pos >= b.block_end || end_pos > b.block_end
            then begin
                Printf.printf "%d: Exceeding block boundaries" c.client_num;
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
                
                ( (* try to recover the corresponding block ... *)
                  
                  let chunk_num = Int32.to_int (Int32.div begin_pos block_size) 
                  in
                  match file.file_chunks.(chunk_num) with
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
              end
            else
              
              begin
                printf_char '#'; 
                
                ignore (Unix32.seek32 file.file_fd begin_pos Unix.SEEK_SET);
                
                begin
                  try
                    really_write (Unix32.force_fd file.file_fd)
                    t.Q.bloc_str t.Q.bloc_begin t.Q.bloc_len
                  with
                    e ->
                      Printf.printf "Error %s while writing block. Pausing download" (Printexc.to_string e);
                      print_newline ();
                      file.file_state <- FilePaused;
                      small_change_file file
                end;
(*
                let mmap_pos = Int32.mul (
                    Int32.div begin_pos page_size) page_size
                in
                
                let m = Mmap.mmap file.file_md4_name (file_fd file)
                  mmap_pos (Int32.sub end_pos mmap_pos) in
                
                let begin_pos = Int32.sub begin_pos mmap_pos in
                if 
                  (Int32.of_int (String.length t.Q.bloc_str)) <
                    Int32.add str_begin len then begin
                    Printf.printf "BLOC TO SHORT %d %s+%s"
                      (String.length t.Q.bloc_str) 
                    (Int32.to_string str_begin)
                    (Int32.to_string str_begin);
                    print_newline ();
                    raise Not_found
                  end;
                
                Mmap.blit_from_string t.Q.bloc_str str_begin
                  m begin_pos len;
Mmap.munmap m;
                *)
              end;
      
      end;
      
      begin
        
        List.iter (update_zone file begin_pos end_pos) c.client_zones;
        find_client_zone c;
        !client_change_hook c
      
      end


(* Upload requests *)
  | M.ViewFilesReq t when !has_upload = 0 -> 
      let files = DownloadServers.all_shared () in
(*      Printf.printf "VIEW %d FILES" (List.length files); print_newline (); *)
      
      client_send sock (
        let module Q = M.ViewFilesReply in
        M.ViewFilesReplyReq (DownloadServers.make_tagged files))
            
  
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
(*      Printf.printf "QUERY %s" file.file_name; print_newline (); *)
          file.file_upload_requests <- file.file_upload_requests + 1;
          client_send sock (
            let module Q = M.QueryFileReply in
            M.QueryFileReplyReq {
              Q.md4 = file.file_md4;
              Q.name = match file.file_filenames with
                [] -> file.file_hardname
              | name :: _ -> name
            });
          
          let send_locations = ref 10 in
          (match c.client_kind with
              Known_location (ip, port) ->
                if not (List.memq c file.file_known_locations) then begin
                    file.file_known_locations <- 
                      c :: file.file_known_locations;
                    send_locations := max_int;
                    file.file_changed <- BigChange;
                  end
            | Indirect_location -> 
                if not (List.memq c file.file_indirect_locations) then begin
                    file.file_indirect_locations <- 
                      c :: file.file_indirect_locations;
                    send_locations := max_int;
                    file.file_changed <- BigChange;
                  end
          );
          if c.client_is_mldonkey = 2 then 
            let sources = ref [] in
            begin
              try (* simply send the last found location *)
                List.iter (fun c ->
                    match c.client_kind with
                      Known_location (ip, port) -> 
                        sources := (ip, port, ip) :: !sources;
                        decr send_locations;
                        if !send_locations = 0 then
                          raise Not_found
                    | _ -> ()
                ) file.file_known_locations;                  
              with _ -> ()
            end;
            
            if !sources <> [] then
              client_send sock (
                let module Q = M.Sources in
                M.SourcesReq {
                  Q.md4 = file.file_md4;
                  Q.sources = !sources;
                })
        with _ -> () end    

  | M.SourcesReq t ->
      
      let module Q = M.Sources in
      begin
        try
          let file = find_file t.Q.md4 in
          List.iter (fun (ip1, port, ip2) ->
              if Ip.valid ip1 then
                let c = new_client (Known_location (ip1, port)) in
                if not (List.memq c file.file_known_locations) then
                  file.file_known_locations <- c :: file.file_known_locations;
                if not (List.memq c !interesting_clients) then
                  interesting_clients := c :: !interesting_clients
          ) t.Q.sources
        with _ -> ()
      end

  | M.SayReq s ->
      
      !say_hook (Some c) s
      
  | M.QueryChunkMd4Req t when !has_upload = 0 -> 

      let file = find_file t in
      begin
        match file.file_md4s with
          [] -> () (* should not happen *)
        | md4s ->
            client_send sock (
              let module Q = M.QueryChunkMd4Reply in
              M.QueryChunkMd4ReplyReq {
                Q.md4 = file.file_md4;
                Q.chunks = Array.of_list md4s
              })
      
      end
  
  | M.QueryChunksReq t when !has_upload = 0 ->

(*      Printf.printf "QueryChunksReq"; print_newline (); *)
      let file = find_file t in
      client_send sock (
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

      set_rtimeout (TcpClientSocket.sock sock) infinite_timeout;
      let module Q = M.QueryBloc in
      let file = find_file  t.Q.md4 in
(* Printf.printf "QUERY BLOCS %s" file.file_hardname; print_newline ();*)

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
      if can_write sock && not up.up_waiting && !has_upload = 0 then begin
          Fifo.put upload_clients c;
          up.up_waiting <- true
        end

  | _ -> ()

let client_handler c sock event = 
  let c = real_client c in
  match event with
    BASIC_EVENT (CLOSED s) ->
      printf_string "-c";
      connection_failed c.client_connection_control;
      disconnected_from_client c s
  
  | _ -> ()
      
let init_connection c  sock files =
  set_handler sock WRITE_DONE (fun s ->
      let c = real_client c in
      match c.client_upload with
        None -> ()
      | Some up ->
          if not up.up_waiting && !has_upload = 0 then begin
              up.up_waiting <- true;
              Fifo.put upload_clients c
            end
  );
  set_rtimeout (TcpClientSocket.sock sock) !!client_timeout;
  set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
      printf_string "-!C";
      connection_delay c.client_connection_control;
      close s "timeout"
  );
  set_reader sock (Mftp_comm.client_handler (client_to_client files c));
  c.client_block <- None;
  c.client_zones <- [];
  c.client_files <- [];
  !client_change_hook c
      
let reconnect_client cid files c =
  match c.client_kind with
    Indirect_location  -> ()
  | Known_location (ip, port) ->
      
      try
        set_client_state c Connecting;
        connection_try c.client_connection_control;

        printf_string "?C";
        let sock = TcpClientSocket.connect (
            Ip.to_inet_addr ip) 
          port 
            (client_handler c) in
        init_connection c sock files;
        c.client_sock <- Some sock;
        let s = DownloadServers.last_connected_server () in
        
        client_send sock (
          let module M = Mftp_client in
          let module C = M.Connect in
          M.ConnectReq {
            C.md4 = !!client_md4; (* we want a different id each conn *)
            C.ip = cid;
            C.port = !client_port;
            C.tags = !client_tags;
            C.version = 16;
            C.ip_server = s.server_ip;
            C.port_server = s.server_port;
          }
        )
      
      with _ -> 
          connection_failed c.client_connection_control;
          c.client_state <- NotConnected
          
let connect_client cid files c = 
  match c.client_sock with
    None -> 
      if connection_can_try c.client_connection_control then 
        reconnect_client cid files c
  | Some sock ->
      match c.client_state with
        Connected_idle -> 
          query_files c sock files
      | _ -> ()
        
let query_id_reply s t =
  let module M = Mftp_server in
  let module Q = M.QueryIDReply in
  let c = new_client (Known_location (t.Q.ip, t.Q.port)) in
(*  Printf.printf "QueryIDReply: Connect client"; print_newline (); *)
  connect_client s [] c
      
let query_id s sock ip =
  printf_string "[QUERY ID]";
  server_send sock (
    let module M = Mftp_server in
    let module C = M.QueryID in
    M.QueryIDReq ip
  )

let udp_server_send s t =
  Mftp_comm.udp_send (match !udp_sock with
      None -> failwith "No UDP socket"
    | Some sock -> sock)  
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
              Q.ip = !client_ip;
              Q.port = !client_port;
              Q.id = ip;
            })

      | Some sock ->
          query_id s sock ip
  ) t.Q.locs
      
let query_locations file s sock =
  printf_string "[QUERY LOC]";
  server_send sock (
    let module M = Mftp_server in
    let module C = M.QueryLocation in
    M.QueryLocationReq file.file_md4
  )
  
let client_connection_handler t event =
  printf_string "[REMOTE CONN]";
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      
      let c = new_client Indirect_location in
      c.client_state <- Connected_initiating;
      let sock = TcpClientSocket.create s (client_handler c) in
      init_connection c sock [];      
      c.client_sock <- Some sock;
      
      let sc = TcpClientSocket.sock sock in
  
      if closed sc then begin
          Printf.printf "SOCK CLOSED %s" (error sock);  
          print_newline ();
        end;      
      
  | _ -> 
        ()      

