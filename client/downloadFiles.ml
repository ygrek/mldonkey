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
open DownloadServers
open Options
open BasicSocket
open TcpClientSocket
open Mftp
open DownloadOneFile
open Mftp_comm
open DownloadTypes
open DownloadGlobals
open DownloadComplexOptions
open DownloadOptions
open DownloadClient  
open Gui_types
  
let make_query search =
  let search = search.search_query in
  let module M = Mftp_server in
  let module Q = M.Query in
    (
    let args = List.map (fun s -> Q.HasWord s) search.search_words in
    
    let args = match search.search_minsize with
        None -> args
      | Some v ->
          ( Q.HasMinVal ("size", v) :: args)
    in
    
    let args = match search.search_maxsize with
        None -> args
      | Some v ->
          ( Q.HasMaxVal ("size", v) :: args)
    in

    let args = match search.search_min_bitrate with
        None -> args
      | Some v -> 
          ( Q.HasMinVal ("bitrate", v) :: args)
    in
    
    let args = match search.search_artist with
        None -> args
      | Some v ->
          (Q.HasField ("Artist", v) :: args)
    in
    
    let args = match search.search_title with
        None -> args
      | Some v ->
          (Q.HasField ("Title", v) :: args)
    in
    
    let args = match search.search_album with
        None -> args
      | Some v ->
          (Q.HasField ("Album", v) :: args)
    in
    
    let args = match search.search_avail with
        None -> args
      | Some v ->
          ( Q.HasMaxVal ("availability", v) :: args)
    in
    
    let args = match search.search_media with
        None -> args
      | Some v ->
          ( Q.HasField ("type", v) :: args)
    in
    
    let args = match search.search_format with
        None -> args
      | Some v ->
          ( Q.HasField ("format", v) :: args)
    in
    
    let args = (List.map (fun (s1,s2) ->
            Q.HasField (s1, s2)) search.search_fields) @ args in
    
    let rec iter_and q1 q2 args =
      match args with
        [] -> Q.And (q1, q2)
      | q3 :: tail ->
          Q.And (iter_and q2 q3 tail, q1)
    in
    let q = match args with
        [] -> failwith "Empty search"
      | q1 :: q2 :: tail ->
          iter_and q1 q2 tail
      | [q] -> q
    in
    q)

  (*
let add_availability r v = 
  List.iter (fun tag ->
        match tag with
      | { tag_name = "availability"; tag_value = (Uint32 _ | Fint32 _) } ->
          tag.tag_value <- Uint32 (Int32.of_int v)
      | _ ->  ()
  ) r.result_tags
    *)

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
    let result, old_avail = Hashtbl.find search.search_files md4
    in
    old_avail := !old_avail + !availability;
    if not (List.mem !file_name result.result_names) then begin
        DownloadIndexer.add_name result !file_name;
        result.result_names <- !file_name :: result.result_names
      end
  with _ ->
      let new_result = { 
          result_md4 = md4;
          result_names = [!file_name];
          result_size = !file_size;
          result_format = "";
          result_type = "";
          result_tags = List.rev !new_tags;
          result_comment = None;
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
        let result =  DownloadIndexer.index_result new_result in      
        Hashtbl.add search.search_files md4 (result, availability);
        search.search_nresults <- search.search_nresults + 1;
        search.search_handler (Result result);
      with _ ->  (* the file was probably filtered *)
          ()
      
let search_handler search t =
  search.search_waiting <- search.search_waiting - 1;
  List.iter (fun f ->
      search_found search f.f_md4 f.f_tags
  ) t;
  search.search_handler (Waiting search.search_waiting)
  
    
let force_save_options () =  
  List.iter DownloadOneFile.update_options !!files;
  Gc.compact ();
  Options.save_with_help downloads_ini;
  Options.save_with_help files_ini;
  Options.save_with_help friends_ini;
  if !servers_ini_changed then begin
      printf_string "[SAVE OPTIONS]\n";
      DownloadServers.update_options ();
      Options.save_with_help servers_ini
    end
    
let udp_query_locations file s =
  let module M = Mftp_server in
  udp_server_send s (M.QueryLocationUdpReq file.file_md4)

let rec find_search_rec num list =
  match list with
    [] -> raise Not_found
  | s :: tail ->
      if s.search_num = num then s else 
        find_search_rec num tail
let find_search num = find_search_rec num !searches

let make_xs ss =
  let servers, left = List2.cut !!max_xs_packets ss.search_xs_servers in
  ss.search_xs_servers <- left;
  let query = make_query ss in
  
  List.iter (fun s ->
      match s.server_sock with
      | Some sock -> ()
      | None ->
          let module M = Mftp_server in
          let module Q = M.Query in
          udp_server_send s (M.QueryUdpReq query);
  ) servers
  
  
  
let force_check_locations () =
  try
    List.iter (fun file -> 
        if file.file_state = FileDownloading then begin      
            List.iter (fun c ->
                if allow_new_connection () then      
                  try connect_client !client_ip [file] c with _ -> ()) 
            file.file_known_locations;
            List.iter (fun s ->
                match s.server_sock with
                  None -> () (* assert false !!! *)
                | Some sock ->
                    (try query_locations file s sock with _ -> ())
            ) !connected_server_list;
            
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
    ) !!files;

    for i = 1 to !!max_udp_sends do
      match !udp_servers_list with
        [] -> udp_servers_list := !!known_servers
      | s :: tail ->
          s.server_next_udp <- last_time () +. !!medium_retry_delay;
          udp_servers_list := tail
    done;

    
    if !last_xs >= 0 then begin
        try
          let ss = find_search !last_xs in
          make_xs ss
        with _ -> ()
      end;
    
    List.iter (fun c -> 
        try connect_client !client_ip [] c with _ -> ()) !interesting_clients;
    interesting_clients := [];

    Printf.printf "try connecting friends"; print_newline ();
    List.iter (fun c ->
        if allow_new_connection () then
          try connect_client !client_ip [] c with _ -> ()
    ) !!known_friends;
    
  with e ->
      Printf.printf "force_check_locations: %s" (Printexc.to_string e);
      print_newline ()
      
let check_locations timer =
  reactivate_timer timer;
  force_check_locations ()  

let rec save_options timer =
  reactivate_timer timer;
  force_save_options ()

  
let install_hooks () =
  let old_hook = !server_is_connected_hook in
  server_is_connected_hook := (fun s sock ->
      old_hook s sock;
      connected_server_list := s :: !connected_server_list;
      List.iter (fun file ->
          if file.file_state = FileDownloading then
            query_locations file s sock    
      ) !!files);
  
  let old_hook = !server_is_disconnected_hook in
  server_is_disconnected_hook := (fun s ->
      try
        connected_server_list := List2.remove s !connected_server_list
      with _ -> 
          Printf.printf "Exception in List2.remove";
          print_newline ();
        );
  
  let old_hook = !received_from_server_hook in
  received_from_server_hook := (fun s sock t ->
      old_hook s sock t;
      let module M = Mftp_server in
      match t with
        M.QueryIDReplyReq t -> query_id_reply s.server_cid t
      | M.QueryReplyReq t ->
          let rec iter () =
            let query = try
                Fifo.take s.server_search_queries
              with _ -> failwith "No pending query"
            in
            try
              query s sock t
            with Already_done -> iter ()
          in
          iter ()          
      | M.QueryUsersReplyReq t ->
          let rec iter () =
            let query = try
                Fifo.take s.server_users_queries
              with _ -> failwith "No pending query"
            in
            try
              query s sock t
            with Already_done -> iter ()
          in
          iter ()          
      | M.QueryLocationReplyReq t -> query_locations_reply s t
      | M.QueryIDFailedReq t -> ()
      
      | _ -> ()
  )

let udp_from_server p =
  match p.UdpSocket.addr with
  | Unix.ADDR_INET(ip, port) ->
      let s = add_server (Ip.of_inet_addr ip) (port-4) in
      connection_set_last_conn s.server_connection_control (last_time ());
      s.server_score <- s.server_score + 3;
      s
  | _ -> raise Not_found

let udp_client_handler t p =
  let module M = Mftp_server in
  match t with
    M.QueryLocationReplyUdpReq t ->
(*      Printf.printf "Received location by UDP"; print_newline (); *)
      query_locations_reply (udp_from_server p) t
  | M.QueryReplyUdpReq t ->
(*      Printf.printf "Received file by UDP"; print_newline (); *)
      if !last_xs >= 0 then
        let ss = find_search !last_xs in
        search_handler ss [t]
  | M.FileGroupInfoUdpReq t ->
(*      Printf.printf "Received location by File Group"; print_newline (); *)
      let module M = Mftp_server in
      let module Q = M.QueryLocationReply in
      let md4 = t.Q.md4 in
      begin try
          let file = find_file md4 in
          List.iter (fun l ->
              let ip = l.Q.ip in
              let port = l.Q.port in
              
              let c = new_client (Known_location (ip, port)) in
              if not (List.memq c file.file_known_locations) then begin
                  Printf.printf "New location by File Group !!"; print_newline ();
                  file.file_known_locations <- c :: file.file_known_locations;
                end;
              connect_client !client_ip [file] c
          ) t.Q.locs
        with _ -> ()
      end;
      List.iter (fun l ->
          let ip = l.Q.ip in
          let port = l.Q.port in
          let c = new_client (Known_location (ip, port)) in          
          client_wants_file c md4) t.Q.locs
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
    let fd = file.file_fd in
    ignore (Unix32.seek32 fd begin_pos Unix.SEEK_SET);
    really_read (Unix32.force_fd fd) upload_buffer 0 len_int;
    incr upload_counter;
    file.file_upload_blocks <- file.file_upload_blocks + 1;
(*  Printf.printf "sending"; print_newline (); *)
    printf_char 'U';
    client_send sock (
      let module M = Mftp_client in
      let module B = M.Bloc in
      M.BlocReq {  
        B.md4 = file.file_md4;
        B.start_pos = begin_pos;
        B.end_pos = Int32.add begin_pos len;
        B.bloc_str = upload_buffer;
        B.bloc_begin = 0;
        B.bloc_len = len_int; 
      }
    )
  with e -> 
      Printf.printf "Exception %s in send_small_block" (Printexc.to_string e);
      print_newline () 
  
 
let rec send_client_block c sock per_client =
  if per_client > 0 then
    match c.client_upload with
    | Some ({ up_chunks = _ :: chunks } as up)  ->
        if not up.up_file.file_shared then begin
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
            send_client_block c sock (per_client-1)
          end
    | _ -> 
        ()
  
let rec send_client_block_partial c sock per_client =
  let msg_block_size = Int32.of_int (per_client * 1000) in
  match c.client_upload with
  | Some ({ up_chunks = _ :: chunks } as up)  ->
      if not up.up_file.file_shared then begin
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
  
let reset_upload_timer timer =
  reactivate_timer timer;
  download_counter := 0;
  remaining_bandwidth := !!max_upload_rate
  
  (* timer started every 1/10 seconds *)
let upload_timer timer =
  reactivate_timer timer;
  (try download_engine () with e -> 
        Printf.printf "Exception %s in download_engine" 
          (Printexc.to_string e); print_newline (););
  try
    while !remaining_bandwidth > 0 && not (Fifo.empty upload_clients) do
      if !remaining_bandwidth < 10 then begin
          let c = Fifo.take upload_clients in
          match c.client_sock with
          | Some sock ->
              send_client_block_partial c sock !remaining_bandwidth;
              (match c.client_upload with
                  None -> ()
                | Some up ->
                    if can_write sock && !has_upload = 0 then begin
                        up.up_waiting <- true;
                        Fifo.put upload_clients c
                      end else 
                      up.up_waiting <- false
              )
          | _ -> ()              
        end else
      let per_client = 
        let len = Fifo.length upload_clients in
        if len * 10 < !remaining_bandwidth then
          min 5 (max ((!remaining_bandwidth + 9)/ 10 / len ) 1) 
        else 1 in
      let c = Fifo.take upload_clients in
      match c.client_sock with
      | Some sock ->
          send_client_block c sock per_client;
          (match c.client_upload with
              None -> ()
            | Some up ->
                if can_write sock && !has_upload = 0 then begin
                    up.up_waiting <- true;
                    Fifo.put upload_clients c
                  end else 
                  up.up_waiting <- false
          )
      | _ -> ()
    done
  with e -> 
      Printf.printf "exc %s in upload" (Printexc.to_string e);
      print_newline () 

let upload_credit_timer timer =
  reactivate_timer timer;
  if !has_upload = 0 then 
    (if !upload_credit < 300 then incr upload_credit)
  else
    decr has_upload

let _ =
  DownloadGlobals.do_at_exit force_save_options