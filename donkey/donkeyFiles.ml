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

open CommonShared
open CommonServer
open CommonComplexOptions
open GuiProto
open CommonClient
open CommonUser
open CommonSearch
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open DonkeyMftp
open DonkeyOneFile
open DonkeyProtoCom
open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions
open DonkeyOptions
open CommonOptions
open DonkeyClient  
open CommonGlobals

let search_found search md4 tags = 
  let file_name = ref "" in
  let file_size = ref Int32.zero in
  let availability = ref 0 in
  let new_tags = ref [] in
  List.iter (fun tag ->
      match tag with
        { tag_name = "filename"; tag_value = String s } -> file_name := s
      | { tag_name = "size"; tag_value = Uint32 v } -> file_size := v
      | { tag_name = "availability"; tag_value = (Uint32 v| Fint32 v) } ->
          availability := Int32.to_int v;  new_tags := tag :: !new_tags
      | _ -> new_tags := tag :: !new_tags
  ) tags;
  try
    let rs = DonkeyIndexer.find_result md4 in
(*    Printf.printf "search_add_result"; print_newline (); *)
    search_add_result search rs.result_result; (* ADD AVAILABILITY *)
(*    Printf.printf "search_add_result DONE"; print_newline (); *)
    let doc = rs.result_index in
    let result = Store.get store doc in
(*    old_avail := !old_avail + !availability; *)
    if not (List.mem !file_name result.result_names) then begin
        DonkeyIndexer.add_name result !file_name;
        result.result_names <- !file_name :: result.result_names
      end
  with _ ->
      let new_result = { 
          result_num = 0;
          result_network = network.network_num;
          result_md4 = md4;
          result_names = [!file_name];
          result_size = !file_size;
          result_format = "";
          result_type = "";
          result_tags = List.rev !new_tags;
          result_comment = "";
          result_done = false;
        } in
      List.iter (fun tag ->
          match tag with
            { tag_name = "format"; tag_value = String s } ->
              new_result.result_format <- s
          | { tag_name = "type"; tag_value = String s } ->
              new_result.result_type <- s
          | _ -> ()
      ) new_result.result_tags;

(*      Printf.printf "new reply"; print_newline ();*)
      try
        let rs = DonkeyIndexer.index_result new_result in      
        let doc = rs.result_index in
(*        Printf.printf "search_add_result"; print_newline (); *)
        search_add_result search rs.result_result;
(*        Printf.printf "search_add_result DONE"; print_newline (); *)
        let result = Store.get store doc in
        ()
      with _ ->  (* the file was probably filtered *)
          ()
          
let search_handler search t =
  let s = search.search_search in
  let waiting = s.search_waiting - 1 in
  s.search_waiting <- waiting;
  List.iter (fun f ->
      search_found s f.f_md4 f.f_tags
  ) t;
  search.search_handler (Waiting s.search_waiting)
    
let udp_query_locations file s =
  let module M = DonkeyProtoServer in
  udp_server_send s (M.QueryLocationUdpReq file.file_md4)

let rec find_search_rec num list =
  match list with
    [] -> raise Not_found
  | s :: tail ->
      if s.search_search.search_num = num then s else 
        find_search_rec num tail
        
let find_search num = find_search_rec num !local_searches

let make_xs ss =
  let servers, left = List2.cut !!max_xs_packets ss.search_xs_servers in
  ss.search_xs_servers <- left;
  
  List.iter (fun s ->
      match s.server_sock with
      | Some sock -> ()
      | None ->
          let module M = DonkeyProtoServer in
          let module Q = M.Query in
          udp_server_send s (M.QueryUdpReq ss.search_search.search_query);
  ) servers

let fill_clients_list _ =
(* should we refill the queue ? *)
  if !!max_clients_per_second * 300 > !clients_list_len then begin
      List.iter (fun file -> 
          if file_state file = FileDownloading then 
            let files = [file] in
            Intmap.iter (fun _ c ->
                match c.client_kind with
                  Known_location _ ->
                    clients_list := (c, files) :: !clients_list
                | _ -> ()
            )
            file.file_sources;
      ) !current_files;
      (*
      List.iter (fun c ->
          clients_list := (c, []) :: !clients_list
      ) !!known_friends;
*)      
      clients_list_len := List.length !clients_list;
    end  
  
let add_interesting_client c files =
  clients_list := (c, files) :: !clients_list;
  incr clients_list_len
    
let rec connect_several_clients n =
(*  Printf.printf "connect_several_clients %d" n; print_newline (); *)
  if n > 0 && can_open_connection () then
    match !clients_list with
      [] -> ()
    | (c, files) :: tail ->
(*        Printf.printf "TRY TO CONNECT"; print_newline (); *)

        clients_list := tail;
        decr clients_list_len;
       
        match c.client_sock with
          None -> 
            if connection_can_try c.client_connection_control then begin
(*                Printf.printf "CAN CONNECT"; print_newline (); *)
                (try connect_client (client_ip None) files c with _ -> ());
                connect_several_clients (n-1)
              end
        | Some sock ->
            match client_state c with
              Connected_idle -> 
                (try query_files c sock files with _ -> ());
                connect_several_clients (n-1)
            | _ -> 
                connect_several_clients n

                
let remove_old_clients () =
  let min_last_conn =  last_time () -. 
    float_of_int !!max_sources_age *. one_day in
  List.iter (fun file ->
      let locs = file.file_sources in
      let nlocs = file.file_nlocations in
      file.file_sources <- Intmap.empty;
      file.file_nlocations <- 0;
      Intmap.iter (fun _ c ->
          if connection_last_conn c.client_connection_control >= min_last_conn 
            then
            new_source file c
      ) locs;
      if file.file_nlocations < !!min_left_sources then begin
          file.file_sources <- locs;
          file.file_nlocations <- nlocs
        end
  ) !current_files
  
let check_clients _ =
(*  Printf.printf "check_clients"; print_newline (); *)
  (* how many clients we try to connect per second ? *)
  let n = !!max_clients_per_second in
  connect_several_clients n
        
let force_check_locations () =
  try
    List.iter (fun file -> 
        if file_state file = FileDownloading then begin      
(*(* USELESS NOW *)
            Intmap.iter (fun _ c ->
                try connect_client !!client_ip [file] c with _ -> ()) 
            file.file_known_locations;
*)            
            
            (*
            List.iter (fun s ->
                match s.server_sock with
                  None -> () (* assert false !!! *)
                | Some sock ->
                    (try query_location file sock with _ -> ())
            ) (connected_servers());
*)
            
            let list = ref !udp_servers_list in
            for i = 1 to !!max_udp_sends do
              match !udp_servers_list with
                [] -> ()
              | s :: tail ->
                  list := tail;
                  if s.server_next_udp <= last_time () then
                    match s.server_sock with
                      None -> udp_query_locations file s
                    | _ -> ()
            done;
            
          end
    ) !current_files;

    for i = 1 to !!max_udp_sends do
      match !udp_servers_list with
        [] -> 
          udp_servers_list := Hashtbl2.to_list servers_by_key
      | s :: tail ->
          s.server_next_udp <- last_time () +. !!min_retry_delay;
          udp_servers_list := tail
    done;

    
    if !last_xs >= 0 then begin
        try
          let ss = find_search !last_xs in
          make_xs ss
        with _ -> ()
      end;

    (*
(* USELESS NOW *)
    List.iter (fun c -> 
        try connect_client !!client_ip [] c with _ -> ()) !interesting_clients;
    interesting_clients := [];
*)

    (*
(* USELESS NOW *)
    List.iter (fun c ->
        try connect_client !!client_ip [] c with _ -> ()
    ) !!known_friends;
*)
    
  with e ->
      Printf.printf "force_check_locations: %s" (Printexc.to_string e);
      print_newline ()
      
let new_friend c =  
  friend_add c

let browse_client c =
  match c.client_sock, client_state c with
  | None, NotConnected ->
      connection_must_try c.client_connection_control;
      connect_client (client_ip None) [] c
  | None, _ -> 
      add_interesting_client c []
  | Some sock, (
      Connected_initiating 
    | Connected_busy
    | Connected_queued
    | Connected_idle)
    ->
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
  | _ -> ()

let add_user_friend s u = 
  let kind = 
    if Ip.valid u.user_ip then
      Known_location (u.user_ip, u.user_port)
    else begin
        begin
          match s.server_sock, server_state s with 
            Some sock, (Connected_idle|Connected_busy) ->
              query_id s sock u.user_ip;
          | _ -> ()
        end;
        Indirect_location (u.user_name, u.user_md4)
      end
  in
  let c = new_client kind in
  c.client_tags <- u.user_tags;
  set_client_name c u.user_name u.user_md4;
  new_friend c

  
let udp_from_server p =
  match p.UdpSocket.addr with
  | Unix.ADDR_INET(ip, port) ->
      let ip = Ip.of_inet_addr ip in
      if Ip.valid ip then
        let s = add_server ip (port-4) in
(* set last_conn, but add a 2 minutes offset to prevent staying connected
to this server *)
        connection_set_last_conn s.server_connection_control (
          last_time () -. 121.);
        s.server_score <- s.server_score + 3;
        s
      else raise Not_found
  | _ -> raise Not_found

let udp_client_handler t p =
  let module M = DonkeyProtoServer in
  match t with
    M.QueryLocationReplyUdpReq t ->
(*      Printf.printf "Received location by UDP"; print_newline (); *)
      query_locations_reply (udp_from_server p) t
  | M.QueryReplyUdpReq t ->
(*      Printf.printf "Received file by UDP"; print_newline (); *)
      if !last_xs >= 0 then
        let ss = find_search !last_xs in
        Hashtbl.add udp_servers_replies t.f_md4 (udp_from_server p);
        search_handler ss [t]
        
(* Not useful anymore. Use standard server UDP packets.

  
  | M.FileGroupInfoUdpReq t ->
(*      Printf.printf "Received location by File Group"; print_newline (); *)
      let module M = DonkeyProtoServer in
      let module Q = M.QueryLocationReply in
      let md4 = t.Q.md4 in
      begin try
          let file = find_file md4 in
          List.iter (fun l ->
              let ip = l.Q.ip in
              let port = l.Q.port in
              
              let c = new_client (Known_location (ip, port)) in
              if not (Intmap.mem (client_num c) file.file_sources) then begin
                  Printf.printf "New location by File Group !!"; print_newline ();
                  new_source file c;
                end;
              connect_client !!client_ip [file] c
          ) t.Q.locs
        with _ -> ()
      end;
      List.iter (fun l ->
          let ip = l.Q.ip in
          let port = l.Q.port in
          let c = new_client (Known_location (ip, port)) in          
client_wants_file c md4) t.Q.locs

*)
        
  | _ -> ()

open Unix
  
let remaining_bandwidth = ref 0

let msg_block_size_int = 10000
let msg_block_size = Int32.of_int msg_block_size_int
let upload_buffer = String.create msg_block_size_int

let rec really_read fd s pos len =
  let nread = Unix.read fd s pos len in
  if nread = 0 then raise End_of_file else
  if nread < len then
    really_read fd s (pos + nread) (len - nread)
  
let send_small_block sock file begin_pos len = 
  let len_int = Int32.to_int len in
  remaining_bandwidth := !remaining_bandwidth - len_int / 1000;
  try
(*
  Printf.printf "send_small_block %s %s"
(Int32.to_string begin_pos) (Int32.to_string len);
print_newline ();
*)
    
    
    let msg = client_msg 
      (
        let module M = DonkeyProtoClient in
        let module B = M.Bloc in
        M.BlocReq {  
          B.md4 = file.file_md4;
          B.start_pos = begin_pos;
          B.end_pos = Int32.add begin_pos len;
          B.bloc_str = "";
          B.bloc_begin = 0;
          B.bloc_len = 0; 
        }
      ) in
    let s = client_msg_to_string msg in
    let slen = String.length s in
    let upload_buffer = String.create (slen + len_int) in
    String.blit s 0 upload_buffer 0 slen;
    DonkeyProtoCom.new_string msg upload_buffer;
    
    let fd = file_fd file in
    ignore (Unix32.seek32 fd begin_pos Unix.SEEK_SET);
    really_read (Unix32.force_fd fd) upload_buffer slen len_int;
(*    Printf.printf "slen %d len_int %d final %d" slen len_int (String.length upload_buffer); 
print_newline (); *)
    let uploaded = Int64.of_int len_int in
    upload_counter := Int64.add !upload_counter uploaded;
    (match file.file_shared with None -> ()
      | Some impl ->
          shared_must_update_downloaded (as_shared impl);
          impl.impl_shared_uploaded <- 
            Int64.add impl.impl_shared_uploaded uploaded);
    (*  Printf.printf "sending"; print_newline (); *)
    printf_char 'U';
    
    write_string sock upload_buffer
  with e -> 
      Printf.printf "Exception %s in send_small_block" (Printexc.to_string e);
      print_newline () 
  

let max_msg_size = 15000
 
let rec send_client_block c sock per_client =
  if per_client > 0 then
    match c.client_upload with
    | Some ({ up_chunks = _ :: chunks } as up)  ->
        if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
            c.client_upload <- None;
          end else
        let max_len = Int32.sub up.up_end_chunk up.up_pos in
        if max_len <= msg_block_size then
(* last block from chunk *)
          begin
            send_small_block  sock up.up_file up.up_pos max_len;
            up.up_chunks <- chunks;
            match chunks with
              [] -> 
                c.client_upload <- None
            | (begin_pos, end_pos) :: _ ->
                up.up_pos <- begin_pos;
                up.up_end_chunk <- end_pos;
                send_client_block c sock (per_client-1)                
          end
        else
(* small block from chunk *)
          begin
            send_small_block sock up.up_file up.up_pos msg_block_size;
            up.up_pos <- Int32.add up.up_pos msg_block_size;
            if can_write_len sock max_msg_size then
              send_client_block c sock (per_client-1)
          end
    | _ -> 
        ()
  
let rec send_client_block_partial c sock per_client =
  let msg_block_size = Int32.of_int (per_client * 1000) in
  match c.client_upload with
  | Some ({ up_chunks = _ :: chunks } as up)  ->
      if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
          c.client_upload <- None;
        end else
      let max_len = Int32.sub up.up_end_chunk up.up_pos in
      if max_len <= msg_block_size then
(* last block from chunk *)
        begin
          send_small_block  sock up.up_file up.up_pos max_len;
          up.up_chunks <- chunks;
          match chunks with
            [] -> 
              c.client_upload <- None
          | (begin_pos, end_pos) :: _ ->
              up.up_pos <- begin_pos;
              up.up_end_chunk <- end_pos;
        end
      else
(* small block from chunk *)
        begin
          send_small_block sock up.up_file up.up_pos msg_block_size;
          up.up_pos <- Int32.add up.up_pos msg_block_size;
        end
  | _ -> 
      ()
      
  (* timer started every 1/10 seconds *)
  
let reset_upload_timer _ =
  remaining_bandwidth := 
  (if !!max_hard_upload_rate = 0 then 10000
    else !!max_hard_upload_rate)

let rec next_upload n =
(*  Printf.printf "upload for %d" n; print_newline (); *)
  if n > 0 && !remaining_bandwidth > 0 then begin
      upload_to_one_client ();
      next_upload (n-1)
    end

and upload_to_one_client () =
  if !remaining_bandwidth < 10 then begin
      let c = Fifo.take upload_clients in
      match c.client_sock with
      | Some sock ->
          if can_write_len sock max_msg_size then 
            send_client_block_partial c sock !remaining_bandwidth;
          (match c.client_upload with
              None -> ()
            | Some up ->
                if !has_upload = 0 then Fifo.put upload_clients c
          )
      | _ -> ()              
    end else
  let per_client = 
    let len = Fifo.length upload_clients in
    if len * 10 < !remaining_bandwidth then
      mini 5 (max ((!remaining_bandwidth + 9)/ 10 / len ) 1) 
    else 1 in
  let c = Fifo.take upload_clients in
  match c.client_sock with
  | Some sock ->
      if can_write_len sock max_msg_size then 
        send_client_block c sock per_client;
      (match c.client_upload with
          None -> ()
        | Some up ->
            if !has_upload = 0 then  Fifo.put upload_clients c
      )
  | _ -> ()
      

let rec next_uploads () =
  let len = Fifo.length upload_clients in
(*  Printf.printf "uploads for %d" len; print_newline (); *)
  let old = !remaining_bandwidth in
  next_upload len;
  if !remaining_bandwidth < old then next_uploads ()
  
  (* timer started every 1/10 seconds *)
let upload_timer () =
  (try download_engine () with e -> 
        Printf.printf "Exception %s in download_engine" 
          (Printexc.to_string e); print_newline (););
  try
(*    Printf.printf "upload ?"; print_newline (); *)
    next_uploads ()
  with e -> 
      Printf.printf "exc %s in upload" (Printexc.to_string e);
      print_newline () 

let upload_credit_timer _ =
  if !has_upload = 0 then 
    (if !upload_credit < 300 then incr upload_credit)
  else
    decr has_upload
