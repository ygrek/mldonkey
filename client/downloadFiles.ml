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
open TcpBufferedSocket
open Mftp
open DownloadOneFile
open Mftp_comm
open DownloadTypes
open DownloadGlobals
open DownloadComplexOptions
open DownloadOptions
open DownloadClient  
open Gui_types

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
    let doc, old_avail = Hashtbl.find search.search_files md4 in
    let result = Store.get DownloadIndexer.store doc in
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
        let doc = DownloadIndexer.index_result new_result in      
        Hashtbl.add search.search_files md4 (doc, availability);
        search.search_nresults <- search.search_nresults + 1;
        let result = Store.get DownloadIndexer.store doc in
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
(*  Printf.printf "Saving downloads.ini ..."; flush stdout; *)
  Options.save_with_help downloads_ini;
(*
Printf.printf "done"; print_newline ();
Printf.printf "Saving searches.ini ..."; flush stdout;
*)
  Options.save_with_help searches_ini;
(*
Printf.printf "done"; print_newline ();
Printf.printf "Saving files.ini ..."; flush stdout;
*)
  Options.save_with_help files_ini;
(*
Printf.printf "done"; print_newline ();
Printf.printf "Saving friends.ini ..."; flush stdout;
*)
  Options.save_with_help friends_ini;
(*  Printf.printf "done"; print_newline (); *)
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
  
  List.iter (fun s ->
      match s.server_sock with
      | Some sock -> ()
      | None ->
          let module M = Mftp_server in
          let module Q = M.Query in
          udp_server_send s (M.QueryUdpReq ss.search_query);
  ) servers

let fill_clients_list _ =
(* should we refill the queue ? *)
  if !!max_clients_per_second * 900 > !clients_list_len then begin
      List.iter (fun file -> 
          if file.file_state = FileDownloading then 
            let files = [file] in
            Intmap.iter (fun _ c ->
                clients_list := (c, files) :: !clients_list)
            file.file_known_locations;
      ) !!files;
      List.iter (fun c ->
          clients_list := (c, []) :: !clients_list
      ) !!known_friends;
      clients_list_len := List.length !clients_list;
      remaining_time_for_clients := 60 * 15
    end  
  
let rec connect_several_clients n =
  if n > 0 && can_open_connection () then
    match !clients_list with
      [] -> ()
    | (c, files) :: tail ->
        clients_list := tail;
        decr clients_list_len;
       
        match c.client_sock with
          None -> 
            if connection_can_try c.client_connection_control then begin
                (try connect_client !!client_ip files c with _ -> ());
                connect_several_clients (n-1)
              end
        | Some sock ->
            match c.client_state with
              Connected_idle -> 
                (try query_files c sock files with _ -> ());
                connect_several_clients (n-1)
            | _ -> 
                connect_several_clients n

                
let remove_old_clients () =
  let day = 3600. *. 24. in
  let min_last_conn =  last_time () -. 
    float_of_int !!max_sources_age *. day in
  List.iter (fun file ->
      let locs = file.file_known_locations in
      file.file_known_locations <- Intmap.empty;
      Intmap.iter (fun _ c ->
          if connection_last_conn c.client_connection_control < min_last_conn then
            begin
              set_client_state c Removed
            end
          else 
            file.file_known_locations <- Intmap.add c.client_num c
              file.file_known_locations) locs
  ) !!files

let check_clients _ =
  (* how many clients we try to connect per second ? *)
  let n = !!max_clients_per_second in
  connect_several_clients n
        
let force_check_locations () =
  try
    List.iter (fun file -> 
        if file.file_state = FileDownloading then begin      
(*(* USELESS NOW *)
            Intmap.iter (fun _ c ->
                try connect_client !!client_ip [file] c with _ -> ()) 
            file.file_known_locations;
*)            
            
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
              if not (Intmap.mem c.client_num file.file_known_locations) then begin
                  Printf.printf "New location by File Group !!"; print_newline ();
                  file.file_known_locations <- Intmap.add c.client_num c 
                    file.file_known_locations;
                end;
              if not (List.memq file c.client_files) then
                c.client_files <- file :: c.client_files;
              connect_client !!client_ip [file] c
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
    
    
    let msg = client_msg 
      (
        let module M = Mftp_client in
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
    Mftp_comm.new_string msg upload_buffer;
    
    let fd = file.file_fd in
    ignore (Unix32.seek32 fd begin_pos Unix.SEEK_SET);
    really_read (Unix32.force_fd fd) upload_buffer slen len_int;
(*    Printf.printf "slen %d len_int %d final %d" slen len_int (String.length upload_buffer); 
    print_newline (); *)
    incr upload_counter;
    file.file_upload_blocks <- file.file_upload_blocks + 1;
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
            if can_write_len sock max_msg_size then
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
  
let reset_upload_timer _ =
  download_counter := 0;
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
let upload_timer timer =
  reactivate_timer timer;
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

let _ =
  DownloadGlobals.do_at_exit force_save_options
