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

open Md4

open CommonShared
open CommonServer
open CommonComplexOptions
open GuiProto
open CommonClient
open CommonFile
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
open DonkeyStats
          
let search_handler s t =
  let waiting = s.search_waiting - 1 in
  s.search_waiting <- waiting;
  List.iter (fun f ->
      search_found s f.f_md4 f.f_tags
  ) t
(*  search.search_handler (Waiting s.search_waiting) *)
    
let udp_query_locations file s =
  if !!verbose then begin
      Printf.printf "UDP: query location %s" (Ip.to_string s.server_ip);
      print_newline ();
    end;
  let module M = DonkeyProtoServer in
  udp_server_send s (M.QueryLocationUdpReq file.file_md4)

  (*
let rec find_search_rec num list =
  match list with
    [] -> raise Not_found
  | s :: tail ->
      if s.search_search.search_num = num then s else 
        find_search_rec num tail
        
let find_search num = find_search_rec num !local_searches
    *)

let cut_for_udp_send max_servers list =
  let min_last_conn = last_time () -. 8. *. 3600. in
  let rec iter list n left =
    if n = 0 then 
      left, list
    else
    match list with 
      [] -> left, []
    | s :: tail ->
        if connection_last_conn s.server_connection_control > min_last_conn
        then
          iter tail (n-1) (s :: left)
        else
          iter tail n left
  in
  iter list max_servers []

let make_xs ss =
  if ss.search_num <> !xs_last_search then begin
      xs_last_search := ss.search_num;
      xs_servers_list := Hashtbl2.to_list servers_by_key;
    end;
  
  let before, after = cut_for_udp_send !!max_xs_packets !xs_servers_list in
  xs_servers_list := after;
  List.iter (fun s ->
      match s.server_sock with
      | Some sock -> ()
      | None ->
          let module M = DonkeyProtoServer in
          let module Q = M.Query in
          udp_server_send s (M.QueryUdpReq ss.search_query);
  ) before;
  
  DonkeyOvernet.overnet_search ss

(* Every five minutes, we fill the clients_lists array with the clients 
that have to be connected in the next five minutes. *)
    
let fill_clients_list _ =

(* First of all, clients which are still there should be saved *)

  let keep_clients = 
    clients_lists.(0) @
    clients_lists.(1) @
    clients_lists.(2) @
    clients_lists.(3) @
    clients_lists.(4) 
  in
  clients_lists.(0) <- [];
  clients_lists.(1) <- [];
  clients_lists.(2) <- [];
  clients_lists.(3) <- [];
  clients_lists.(4) <- [];
  if !!verbose then begin
      Printf.printf "Kept clients: %d" (List.length keep_clients); 
      print_newline ();
    end;

(* Now, for each file, put the sources in the lists *)
  List.iter (fun file -> 
      if file_state file = FileDownloading then 
        Intmap.iter (fun _ c ->
            schedule_client c
        )
        file.file_sources;
  ) !current_files;
  clients_lists.(0) <- keep_clients @ clients_lists.(0);
  remaining_seconds := 61;
  Printf.printf "Next minute: %d" (List.length clients_lists.(0));  print_newline ();
  Printf.printf "2 minutes: %d" (List.length clients_lists.(1));  print_newline ();
  Printf.printf "3 minutes: %d" (List.length clients_lists.(2));  print_newline ();
  Printf.printf "4 minutes: %d" (List.length clients_lists.(3));  print_newline ();
  Printf.printf "5 minutes: %d" (List.length clients_lists.(4));  print_newline ();
  Printf.printf "waiting for breaks: %d" (List.length !new_clients_list);  print_newline ()

(* Every second, try to connect to some clients *)          
let check_clients _ =
  decr remaining_seconds;
(*  Printf.printf "check_clients %d" !remaining_seconds; print_newline (); *)
  if !remaining_seconds = 0 then begin
      remaining_seconds := 60;
      clients_lists.(0) <- clients_lists.(0) @ clients_lists.(1);
      clients_lists.(1) <- clients_lists.(2);
      clients_lists.(2) <- clients_lists.(3);
      clients_lists.(3) <- clients_lists.(4);
      clients_lists.(4) <- []
    end;
  let nnew_clients = List.length !new_clients_list in
  let nwaiting = List.length clients_lists.(0) +
    (nnew_clients / 10) +
    (if nnew_clients>0 then 1 else 0) in
  try
    let nwaiting = ref (min
          (nwaiting / !remaining_seconds + 1)
        !!max_clients_per_second
      ) in
(*    Printf.printf "nwaiting %d" !nwaiting; print_newline (); *)
    while !nwaiting > 0 do
      if not (can_open_connection ()) then raise Exit;
(*      Printf.printf "findind client"; print_newline ();*)
      let c =
        match clients_lists.(0) with
          c :: tail -> clients_lists.(0) <- tail;
(*            Printf.printf "client from list"; print_newline (); *)
            c
        | [] -> match !new_clients_list with
              c :: tail -> 
(*                Printf.printf "client from new clients"; print_newline (); *)
                new_clients_list := tail;
                c
            | [] -> 
(*                Printf.printf "no client"; print_newline (); *)
                raise Exit in
      c.client_on_list <- false;
      try
        if connection_can_try c.client_connection_control then begin
            match c.client_sock with
              None -> 
                reconnect_client c;
                if c.client_sock <> None then decr nwaiting
            | Some sock ->
                query_files c sock;
                connection_try c.client_connection_control;
                connection_ok c.client_connection_control;
                ()
          end else begin (*
            Printf.printf "Client connection too early"; print_newline ();
	    *)
          end
      with e ->
          Printf.printf "Exception %s in check_clients"
            (Printexc2.to_string e); print_newline ()
    done
  with Exit -> ()
      
      (*
let throttle_searches () =
  List.iter (fun file ->
    let locs = file.file_sources in
    let nNotConnected = ref 0 in
    let nConnecting = ref 0 in
    let nInitiating = ref 0 in
    let nBusy = ref 0 in
    let nIdle = ref 0 in
    let nQueued = ref 0 in
    let nNewHost = ref 0 in
    let nRemovedHost = ref 0 in
    if file.file_nlocations < !!max_sources_per_file then 
      update_file_enough_sources file false
    else begin
      Intmap.iter (fun _ c ->
	match c.client_client.impl_client_state with
	  NotConnected -> incr nNotConnected
	| Connecting -> incr nConnecting
	| Connected_initiating -> incr nInitiating
	| Connected_busy -> incr nBusy
	| Connected_idle -> incr nIdle
	| Connected_queued -> incr nQueued
	| NewHost -> incr nNewHost
	| RemovedHost -> incr nRemovedHost
      ) locs;
      Printf.printf "%s: NC:%d C:%d In:%d Bz:%d Id:%d Qu:%d Nw:%d Rm:%d" (Md4.to_string file.file_md4) !nNotConnected !nConnecting !nInitiating !nBusy !nIdle !nQueued !nNewHost !nRemovedHost;
      print_newline ();
      if !nNewHost > !!max_sources_per_file / 10 then begin
	Printf.printf "%d untested sources, throttling searches" !nNewHost;
	print_newline ();
	update_file_enough_sources file true
      end else
	update_file_enough_sources file false
    end
  ) !current_files
*)
  
(* We need to be smarter. This function should be called every 20 minutes for
example. It should be used to remove sources when there are more sources
than we want, and it should disable the 
update_file_enough_sources flag if there are more than more 10% sources
than we want.

We need some properties:
* Connected sources should not be removed.
* New sources (added in the last 20 minutes) should not be removed.
* Good sources (connected in last 30 minutes) should not be removed.
* Other sources should be sorted and removed if needed.
*)

let remove_old_clients () =
  let min_last_conn =  last_time () -. 
    float_of_int !!max_sources_age *. one_day in

(* Good sources: connected in the last 45 minutes *)
(* New sources:  fake-connection 25 minutes before added *)
  let good_last_conn = last_time () -. 45. *. 60. in

(* How many good sources is enough ? *)
  let good_threshold = !!max_sources_per_file * !!good_sources_threshold / 100 in
  List.iter (fun file ->
        
(* First, remove sources older than max_sources_age *)
      let old_sources = ref [] in
      let young_sources = ref [] in
      let nkept_sources = ref 0 in

      Intmap.iter (fun _ c ->
        match c.client_sock with
	    Some _ ->
		incr nkept_sources
          | None ->
	      if connection_last_conn c.client_connection_control < min_last_conn then
		old_sources := c :: !old_sources
	      else begin
		young_sources := c :: !young_sources;
		incr nkept_sources
	      end
      ) file.file_sources;
      if !nkept_sources >= !!min_left_sources then
	List.iter (fun c ->
	  remove_source file c
	) !old_sources;

(* Do it only if we have more sources than we want *)
(* Since the list should not grow once the max_sources_per_file limit
   is reached (see DonkeyGlobals.new_source), this can only happen 
   in exceptional cases (user lowering max_sources_per_file ?) *)
      if file.file_nlocations > !!max_sources_per_file then begin
	let must_remove = mini (List.length !young_sources) 
			    (file.file_nlocations - !!max_sources_per_file) in
        let sources = List.sort (fun c1 c2 ->
(* First criterium for sorting is last_connection *)
          let l1 = connection_last_conn c1.client_connection_control in
          let l2 = connection_last_conn c2.client_connection_control in
            if l1 = l2 then (* they're floats, it's never gonna happen! *)
              c1.client_rating - c2.client_rating
            else
              if l1 > l2 then 1 else -1
            ) !young_sources in
          
        let to_remove, kept = List2.cut must_remove sources in
        (match to_remove, kept with
           | c1 :: _ , c2 :: _ ->
               let l1 = connection_last_conn c1.client_connection_control in
               let l2 = connection_last_conn c2.client_connection_control in
                 Printf.printf "remove %d (%s:%d) keep %d (%s:%d)"
                  (client_num c1) (Date.to_string l1) c1.client_rating
                  (client_num c2)  (Date.to_string l2) c2.client_rating;
                print_newline ();
             | _ -> 
                Printf.printf "no to_remove or no kept";
                print_newline ());
        List.iter (fun c ->
          remove_source file c
        ) to_remove;
        
        Printf.printf "After clean: sources %d" file.file_nlocations; 
        print_newline ();
      end;

      let ngood_sources = ref 0 in
	Intmap.iter (fun _ c ->
	  match c.client_sock with
              Some _ -> incr ngood_sources
            | None -> 
		if connection_last_conn c.client_connection_control > good_last_conn then
		  incr ngood_sources
        ) file.file_sources;

	Printf.printf "%s: %d good sources (%.2f %%)" (file_best_name file) !ngood_sources (100. *. (float_of_int !ngood_sources) /. (float_of_int !!max_sources_per_file));
	print_newline ();

	if !ngood_sources < good_threshold then begin
	  if file.file_enough_sources then begin
	    Printf.printf "Not enough good sources, restarting active search";
	    print_newline ();
	    file.file_enough_sources <- false
	  end
	end else begin
	  if not file.file_enough_sources then begin
	    Printf.printf "Enough good sources, stopping active search";
	    print_newline ();
	    file.file_enough_sources <- true
	  end
	end
  ) !current_files
          
let force_check_locations () =
  try
    
    let before, after = cut_for_udp_send !!max_udp_sends !udp_servers_list in
    udp_servers_list := after;
    
    List.iter (fun file -> 
        if file_state file = FileDownloading then 
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
            List.iter (fun s  ->
              if 
                connection_last_conn s.server_connection_control +. 3600. *. 8. > last_time () &&
                s.server_next_udp <= last_time () then
                  match s.server_sock with
                  None -> 
                    
                    udp_query_locations file s
                  | _ -> ()
            ) before
    ) !current_files;

    List.iter (fun s ->
        s.server_next_udp <- last_time () +. !!min_reask_delay) before;
    if !udp_servers_list = [] then
          udp_servers_list := Hashtbl2.to_list servers_by_key;
    
    if !xs_last_search >= 0 then  begin
        try
          make_xs (search_find !xs_last_search)
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
      Printf.printf "force_check_locations: %s" (Printexc2.to_string e);
      print_newline ()
      
let new_friend c =  
  friend_add c

let browse_client c =
  match c.client_sock, client_state c with
  | None, NotConnected ->
      connect_as_soon_as_possible c
  | None, _ -> 
      connect_as_soon_as_possible c 
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
    if Ip.valid u.user_ip && Ip.reachable u.user_ip then
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
      if !!update_server_list then
        let s = check_add_server ip (port-4) in
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
(*      Printf.printf "Received location by UDP"; print_newline ();  *)
      query_locations_reply (udp_from_server p) t
  | M.QueryReplyUdpReq t ->
(*      Printf.printf "Received file by UDP"; print_newline ();  *)
      if !xs_last_search >= 0 then
        let ss = search_find !xs_last_search in
        Hashtbl.add udp_servers_replies t.f_md4 (udp_from_server p);
        search_handler ss [t]

  | M.PingServerReplyUdpReq _ ->
      ignore (udp_from_server p)
        
  | _ -> ()

let verbose_upload = false
      
let msg_block_size_int = 10000
let msg_block_size = Int32.of_int msg_block_size_int
let upload_buffer = String.create msg_block_size_int
let max_msg_size = 15000
  
module NewUpload = struct
    
    let remaining_bandwidth = ref 0    
    let total_bandwidth = ref 0    
    let complete_bandwidth = ref 0
    let counter = ref 1    
    let sent_bytes = Array.create 10 0

      
    let check_end_upload c sock = ()
      (*
      if c.client_bucket = 0 then
	direct_client_send sock (
	  let module M = DonkeyProtoClient in
	  let module Q = M.CloseSlot in
	    M.CloseSlotReq Q.t)
*)
      
    let rec send_small_block c sock file begin_pos len_int = 
(*      let len_int = Int32.to_int len in *)
      remaining_bandwidth := !remaining_bandwidth - len_int;
      try
        if !!verbose then begin
            Printf.printf "send_small_block(%s) %ld %d"
              (brand_to_string c.client_brand)
              (begin_pos) (len_int);
            print_newline ();
          end;
        
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.Bloc in
            M.BlocReq {  
              B.md4 = file.file_md4;
              B.start_pos = begin_pos;
              B.end_pos = Int32.add begin_pos (Int32.of_int len_int);
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
        Unix2.really_read (Unix32.force_fd fd) upload_buffer slen len_int;
        let uploaded = Int64.of_int len_int in
        count_upload c file uploaded;
        (match file.file_shared with None -> ()
          | Some impl ->
              shared_must_update_downloaded (as_shared impl);
              impl.impl_shared_uploaded <- 
                Int64.add impl.impl_shared_uploaded uploaded);
        if c.client_connected then
          printf_string "U[OUT]"
        else
          printf_string "U[IN]";
        
        write_string sock upload_buffer;
	check_end_upload c sock
      with e -> 
          Printf.printf "Exception %s in send_small_block" (Printexc2.to_string e);
          print_newline () 
    
    let rec send_client_block c sock per_client =
      if per_client > 0 && !remaining_bandwidth > 0 then
        match c.client_upload with
        | Some ({ up_chunks = _ :: chunks } as up)  ->
            if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
                c.client_upload <- None;
              end else
            let max_len = Int32.sub up.up_end_chunk up.up_pos in
            let max_len = Int32.to_int max_len in
            let msg_block_size_int = mini msg_block_size_int per_client in
            if max_len <= msg_block_size_int then
(* last block from chunk *)
              begin
                if verbose_upload then begin
                    Printf.printf "END OF CHUNK (%d) %ld" max_len up.up_end_chunk; 
                    print_newline ();
                  end;
                send_small_block c sock up.up_file up.up_pos max_len;
                up.up_chunks <- chunks;
                let per_client = per_client - max_len in
                match chunks with
                  [] -> 
                    if verbose_upload then begin
                        Printf.printf "NO CHUNKS"; print_newline ();
                      end;
                    c.client_upload <- None;
                | (begin_pos, end_pos) :: _ ->
                    up.up_pos <- begin_pos;
                    up.up_end_chunk <- end_pos;
                    send_client_block c sock per_client
              end
            else
(* small block from chunk *)
              begin
                send_small_block c sock up.up_file up.up_pos 
                  msg_block_size_int;
                up.up_pos <- Int32.add up.up_pos 
                  (Int32.of_int msg_block_size_int);
                let per_client = per_client-msg_block_size_int in
                if can_write_len sock max_msg_size then
                  send_client_block c sock per_client
              end
        | _ -> ()
    
    
    and upload_to_one_client () =
      if !remaining_bandwidth < 10000 then begin
          let c = Fifo.take upload_clients in
          match c.client_sock with
          | Some sock ->
              if can_write_len sock !remaining_bandwidth then 
                send_client_block c sock !remaining_bandwidth;
              (match c.client_upload with
                  None -> ()
                | Some up ->
                    if !has_upload = 0 then Fifo.put upload_clients c
              )
          | _ -> ()
        end else
      let per_client = 
        let len = Fifo.length upload_clients in
        if len * 10000 < !remaining_bandwidth then
(* Each client in the Fifo can receive 10000 bytes.
Divide the bandwidth between the clients
*)
          (!remaining_bandwidth / 10000 / len) * 10000
        else mini 10000 !remaining_bandwidth in
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
    
    let rec fifo_uploads n =
      if n>0 && !remaining_bandwidth > 0 then
        begin
          upload_to_one_client ();
          fifo_uploads (n-1)
        end
    
    let rec next_uploads () =
      let old_remaining_bandwidth = !remaining_bandwidth in
      let len = Fifo.length upload_clients in
      fifo_uploads len;
      if !remaining_bandwidth < old_remaining_bandwidth then
        next_uploads ()
    
    let next_uploads () =
      sent_bytes.(!counter-1) <- sent_bytes.(!counter-1) - !remaining_bandwidth;
      if verbose_upload then begin
          Printf.printf "Left %d" !remaining_bandwidth; print_newline ();
        end;
      complete_bandwidth := !complete_bandwidth + !remaining_bandwidth;
      incr counter;
      if !counter = 11 then begin
          counter := 1;
          total_bandwidth := 
          (if !!max_hard_upload_rate = 0 then 10000 * 1024
            else (!!max_hard_upload_rate - 1) * 1024 );
          complete_bandwidth := !total_bandwidth;
          if verbose_upload then begin
              Printf.printf "Init to %d" !total_bandwidth; print_newline ();
            end;
          remaining_bandwidth := 0          
        end;
      
      let last_sec = ref 0 in
      for i = 0 to 9 do
        last_sec := !last_sec + sent_bytes.(i)
      done;
      
      if verbose_upload then begin
          Printf.printf "last sec: %d/%d (left %d)" !last_sec !total_bandwidth
            (!total_bandwidth - !last_sec);
          print_newline ();
        end;
      
      remaining_bandwidth := mini (mini (mini 
            (maxi (!remaining_bandwidth + !total_bandwidth / 10) 10000) 
          !total_bandwidth) !complete_bandwidth) 
      (!total_bandwidth - !last_sec);
      complete_bandwidth := !complete_bandwidth - !remaining_bandwidth;
      if verbose_upload then begin
          Printf.printf "Remaining %d[%d]" !remaining_bandwidth !complete_bandwidth; print_newline ();
        end;
      sent_bytes.(!counter-1) <- !remaining_bandwidth;
      if !remaining_bandwidth > 0 then 
        next_uploads ()
      
      
    let reset_upload_timer () = ()
  end
      
module OldUpload = struct
    
    let remaining_bandwidth = ref 0
    
    let check_end_upload c sock = ()
      
      (*
      if c.client_bucket = 0 then
        direct_client_send sock (
          let module M = DonkeyProtoClient in
          let module Q = M.CloseSlot in
M.CloseSlotReq Q.t)
   *)
(* FUCK THE CLOSE SLOT, MLdonkey clients does nothing with this. Very
good. Emule clients probably retries to enter the queue, while we are
blocked. This fucking new mechanism simply prevents mldonkey clients
from downloading from othe mldonkey clients. Why should something that
works correctly always be complexified until it doesnot work anymore ??? *)
        
        
    let send_small_block c sock file begin_pos len = 
      let len_int = Int32.to_int len in
      remaining_bandwidth := !remaining_bandwidth - len_int / 1000;
      try

        Printf.printf "OLD send_small_block(%s) %s %s"
          (brand_to_string c.client_brand)
        (Int32.to_string begin_pos) (Int32.to_string len);
print_newline ();

        
        
        let msg =  
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
        Unix2.really_read (Unix32.force_fd fd) upload_buffer slen len_int;
(*    Printf.printf "slen %d len_int %d final %d" slen len_int (String.length upload_buffer); 
print_newline (); *)
        let uploaded = Int64.of_int len_int in
	count_upload c file uploaded;
        (match file.file_shared with None -> ()
          | Some impl ->
              shared_must_update_downloaded (as_shared impl);
              impl.impl_shared_uploaded <- 
                Int64.add impl.impl_shared_uploaded uploaded);
(*  Printf.printf "sending"; print_newline (); *)
        if c.client_connected then
          printf_string "U[OUT]"
        else
          printf_string "U[IN]";
        
        write_string sock upload_buffer;
	check_end_upload c sock
      with e -> 
          Printf.printf "Exception %s in send_small_block" (Printexc2.to_string e);
          print_newline () 
    
    
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
                send_small_block c sock up.up_file up.up_pos max_len;
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
                send_small_block c sock up.up_file up.up_pos msg_block_size;
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
              send_small_block c sock up.up_file up.up_pos max_len;
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
              send_small_block c sock up.up_file up.up_pos msg_block_size;
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
        
end



  (* timer started every 1/10 seconds *)
let upload_timer () =
  (try download_engine () with e -> 
        Printf.printf "Exception %s in download_engine" 
          (Printexc2.to_string e); print_newline (););
  try
(*    Printf.printf "upload ?"; print_newline (); *)
    if !!new_upload_system then
      NewUpload.next_uploads ()
    else
      OldUpload.next_uploads ()
  with e -> 
      Printf.printf "exc %s in upload" (Printexc2.to_string e);
      print_newline () 
      
let reset_upload_timer _ =
    if !!new_upload_system then
      NewUpload.reset_upload_timer ()
    else
      OldUpload.reset_upload_timer ()
  
  
let upload_credit_timer _ =
  if !has_upload = 0 then 
    (if !upload_credit < 300 then incr upload_credit)
  else
    decr has_upload
    
