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

open Printf2
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

module Udp = DonkeyProtoUdp 

let search_handler s t =
  let waiting = s.search_waiting - 1 in
  s.search_waiting <- waiting;
  List.iter (fun f ->
      search_found false s f.f_md4 f.f_tags
  ) t
(*  search.search_handler (Waiting s.search_waiting) *)
    
let udp_query_locations file s =
  if !verbose then begin
      lprintf "UDP: query location %s" (Ip.to_string s.server_ip);
      lprint_newline ();
    end;
  let module Udp = DonkeyProtoUdp in
  udp_server_send s (Udp.QueryLocationUdpReq file.file_md4)

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
  let min_last_conn = last_time () - 8 * 3600 in
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
          udp_server_send s (Udp.QueryUdpReq ss.search_query);
  ) before;
  
  DonkeyOvernet.overnet_search ss
          
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
                    (try DonkeyServers.query_location file sock with _ -> ())
            ) (connected_servers());
*)
          
            List.iter (fun s  ->
              if 
                connection_last_conn s.server_connection_control + 3600*8 > last_time () &&
                s.server_next_udp <= last_time () then
                  match s.server_sock with
                  None -> 
                    
                    udp_query_locations file s
                  | _ -> ()
            ) before
    ) !current_files;

    List.iter (fun s ->
        s.server_next_udp <- last_time () + !!min_reask_delay) before;
    if !udp_servers_list = [] then
          udp_servers_list := Hashtbl2.to_list servers_by_key;
    
    if !xs_last_search >= 0 then  begin
        try
          make_xs (search_find !xs_last_search)
        with _ -> ()
      end;

  with e ->
      lprintf "force_check_locations: %s" (Printexc2.to_string e);
      lprint_newline ()

let add_user_friend s u = 
  let kind = 
    if Ip.valid u.user_ip && Ip.reachable u.user_ip then
      Known_location (u.user_ip, u.user_port)
    else begin
        begin
          match s.server_sock, server_state s with 
            Some sock, (Connected _ |Connected_downloading) ->
              query_id s sock u.user_ip None;
          | _ -> ()
        end;
        Indirect_location (u.user_name, u.user_md4)
      end
  in
  let c = new_client kind  in
  c.client_tags <- u.user_tags;
  set_client_name c u.user_name u.user_md4;
  friend_add c

  
let udp_from_server p =
  match p.UdpSocket.addr with
  | Unix.ADDR_INET(ip, port) ->
      let ip = Ip.of_inet_addr ip in
      if !!update_server_list then
        let s = check_add_server ip (port-4) in
(* set last_conn, but add a 2 minutes offset to prevent staying connected
to this server *)
        connection_set_last_conn s.server_connection_control (
          last_time () - 121);
        s.server_score <- s.server_score + 3;
        s
      else raise Not_found
  | _ -> raise Not_found

let udp_client_handler t p =
  let module M = DonkeyProtoServer in
  match t with
    Udp.QueryLocationReplyUdpReq t ->
(*      lprintf "Received location by UDP"; lprint_newline ();  *)
      query_locations_reply (udp_from_server p) t
      
  | Udp.QueryReplyUdpReq t ->
(*      lprintf "Received file by UDP"; lprint_newline ();  *)
      if !xs_last_search >= 0 then
        let ss = search_find !xs_last_search in
        Hashtbl.add udp_servers_replies t.f_md4 (udp_from_server p);
        search_handler ss [t]

  | Udp.PingServerReplyUdpReq _ ->
      ignore (udp_from_server p)
        
  | _ -> 
      lprintf "Unexpected UDP message: \n%s\n"
        (DonkeyProtoUdp.print t)
      

let verbose_upload = false
      
let msg_block_size_int = 10000
let msg_block_size = Int64.of_int msg_block_size_int
let upload_buffer = String.create msg_block_size_int
let max_msg_size = 15000

(* For upload, it is clearly useless to completely fill a
socket. Since we will try to upload to this client again when the
Fifo queue has been scanned, we can wait for it to download what we
have already given. 

max_hard_upload_rate * 1024 * nseconds

where nseconds = Fifo.length upload_clients
  
  *)
  
module NewUpload = struct
    
    
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
      CommonUploads.consume_bandwidth len_int;
      try
        if !verbose then begin
            lprintf "send_small_block(%s-%s) %Ld %d"
              c.client_name (brand_to_string c.client_brand)
            (begin_pos) (len_int);
            lprint_newline ();
          end;
        
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.Bloc in
            M.BlocReq {  
              B.md4 = file.file_md4;
              B.start_pos = begin_pos;
              B.end_pos = Int64.add begin_pos (Int64.of_int len_int);
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

(*
        let fd = file_fd file in
        ignore (Unix32.seek64 fd begin_pos Unix.SEEK_SET);
Unix2.really_read (Unix32.force_fd fd) upload_buffer slen len_int;
*)
        Unix32.read (file_fd file) begin_pos upload_buffer slen len_int;
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
          lprintf "Exception %s in send_small_block" (Printexc2.to_string e);
          lprint_newline () 
    
    let rec send_client_block c sock per_client =
      if per_client > 0 && CommonUploads.remaining_bandwidth () > 0 then
        match c.client_upload with
        | Some ({ up_chunks = _ :: chunks } as up)  ->
            if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
                c.client_upload <- None;
              end else
            let max_len = Int64.sub up.up_end_chunk up.up_pos in
            let max_len = Int64.to_int max_len in
            let msg_block_size_int = mini msg_block_size_int per_client in
            if max_len <= msg_block_size_int then
(* last block from chunk *)
              begin
                if verbose_upload then begin
                    lprintf "END OF CHUNK (%d) %Ld" max_len up.up_end_chunk; 
                    lprint_newline ();
                  end;
                send_small_block c sock up.up_file up.up_pos max_len;
                up.up_chunks <- chunks;
                let per_client = per_client - max_len in
                match chunks with
                  [] -> 
                    if verbose_upload then begin
                        lprintf "NO CHUNKS"; lprint_newline ();
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
                up.up_pos <- Int64.add up.up_pos 
                  (Int64.of_int msg_block_size_int);
                let per_client = per_client-msg_block_size_int in
                if can_write_len sock max_msg_size then
                  send_client_block c sock per_client
              end
        | _ -> ()

(* 
    let upload_to_one_client () =
      if CommonUploads.remaining_bandwidth () < 10000 then begin
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
          if can_write_len sock max_msg_size             then
            send_client_block c sock per_client;
          (match c.client_upload with
              None -> ()
            | Some up ->
                if !has_upload = 0 then  Fifo.put upload_clients c
          )
      | _ -> ()
 *)   
    
    let upload_to_client c size = 
      match c.client_sock with
        None -> ()
      |     Some sock ->
          if CommonUploads.can_write_len sock (maxi max_msg_size size) then
            send_client_block c sock size;
          (match c.client_upload with
              None -> ()
            | Some up ->
                if !CommonUploads.has_upload = 0 then
                  CommonUploads.ready_for_upload (as_client c.client_client)
          )

    let _ =
      client_ops.op_client_can_upload <- upload_to_client
          
    (*
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
          lprintf "Left %d" !remaining_bandwidth; lprint_newline ();
        end;
      complete_bandwidth := !complete_bandwidth + !remaining_bandwidth;
      incr counter;
      if !counter = 11 then begin
          counter := 1;
          total_bandwidth := 
          (if !!max_hard_upload_rate = 0 then 10000 * 1024
            else (maxi (!!max_hard_upload_rate - 1) 1) * 1024 );
          complete_bandwidth := !total_bandwidth;
          if verbose_upload then begin
              lprintf "Init to %d" !total_bandwidth; lprint_newline ();
            end;
          remaining_bandwidth := 0          
        end;
      
      let last_sec = ref 0 in
      for i = 0 to 9 do
        last_sec := !last_sec + sent_bytes.(i)
      done;
      
      if verbose_upload then begin
          lprintf "last sec: %d/%d (left %d)" !last_sec !total_bandwidth
            (!total_bandwidth - !last_sec);
          lprint_newline ();
        end;
      
      remaining_bandwidth := mini (mini (mini 
            (maxi (!remaining_bandwidth + !total_bandwidth / 10) 10000) 
          !total_bandwidth) !complete_bandwidth) 
      (!total_bandwidth - !last_sec);
      complete_bandwidth := !complete_bandwidth - !remaining_bandwidth;
      if verbose_upload then begin
          lprintf "Remaining %d[%d]" !remaining_bandwidth !complete_bandwidth; lprint_newline ();
        end;
      sent_bytes.(!counter-1) <- !remaining_bandwidth;
      if !remaining_bandwidth > 0 then 
        next_uploads ()
      
    let reset_upload_timer () = ()
    *)
    
  end
      
      
      (*
module OldUpload = struct
    
    let remaining_bandwidth = ref 0
    
    let check_end_upload c sock = ()        
        
    let send_small_block c sock file begin_pos len = 
      let len_int = Int64.to_int len in
      remaining_bandwidth := !remaining_bandwidth - len_int / 1000;
      try

        lprintf "OLD send_small_block(%s) %s %s"
          (brand_to_string c.client_brand)
        (Int64.to_string begin_pos) (Int64.to_string len);
lprint_newline ();

        
        
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.Bloc in
            M.BlocReq {  
              B.md4 = file.file_md4;
              B.start_pos = begin_pos;
              B.end_pos = Int64.add begin_pos len;
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
        (*
        let fd = file_fd file in
        ignore (Unix32.seek64 fd begin_pos Unix.SEEK_SET);
Unix2.really_read (Unix32.force_fd fd) upload_buffer slen len_int;
*)
        Unix32.read (file_fd file) begin_pos upload_buffer slen len_int;
(*    lprintf "slen %d len_int %d final %d" slen len_int (String.length upload_buffer); 
lprint_newline (); *)
        let uploaded = Int64.of_int len_int in
	count_upload c file uploaded;
        (match file.file_shared with None -> ()
          | Some impl ->
              shared_must_update_downloaded (as_shared impl);
              impl.impl_shared_uploaded <- 
                Int64.add impl.impl_shared_uploaded uploaded);
(*  lprintf "sending"; lprint_newline (); *)
        if c.client_connected then
          printf_string "U[OUT]"
        else
          printf_string "U[IN]";
        
        write_string sock upload_buffer;
	check_end_upload c sock
      with e -> 
          lprintf "Exception %s in send_small_block" (Printexc2.to_string e);
          lprint_newline () 
    
    
    let rec send_client_block c sock per_client =
      if per_client > 0 then
        match c.client_upload with
        | Some ({ up_chunks = _ :: chunks } as up)  ->
            if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
                c.client_upload <- None;
              end else
            let max_len = Int64.sub up.up_end_chunk up.up_pos in
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
                up.up_pos <- Int64.add up.up_pos msg_block_size;
                if CommonUploads.can_write_len sock max_msg_size then
                  send_client_block c sock (per_client-1)
              end
        | _ -> 
            ()
    
    let rec send_client_block_partial c sock per_client =
      let msg_block_size = Int64.of_int (per_client * 1000) in
      match c.client_upload with
      | Some ({ up_chunks = _ :: chunks } as up)  ->
          if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
              c.client_upload <- None;
            end else
          let max_len = Int64.sub up.up_end_chunk up.up_pos in
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
              up.up_pos <- Int64.add up.up_pos msg_block_size;
            end
      | _ -> 
          ()

(* timer started every 1/10 seconds *)
    
    let reset_upload_timer _ =
      remaining_bandwidth := 
      (if !!max_hard_upload_rate = 0 then 10000
        else !!max_hard_upload_rate)
    
    let rec next_upload n =
(*  lprintf "upload for %d" n; lprint_newline (); *)
      if n > 0 && !remaining_bandwidth > 0 then begin
          upload_to_one_client ();
          next_upload (n-1)
        end
    
    and upload_to_one_client () =
      if !remaining_bandwidth < 10 then begin
          let c = Fifo.take upload_clients in
          match c.client_sock with
          | Some sock ->
              if CommonUploads.can_write_len sock max_msg_size then 
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
          if CommonUploads.can_write_len sock max_msg_size then 
            send_client_block c sock per_client;
          (match c.client_upload with
              None -> ()
            | Some up ->
                if !has_upload = 0 then  Fifo.put upload_clients c
          )
      | _ -> ()
    
    
    let rec next_uploads () =
      let len = Fifo.length upload_clients in
(*  lprintf "uploads for %d" len; lprint_newline (); *)
      let old = !remaining_bandwidth in
      next_upload len;
      if !remaining_bandwidth < old then next_uploads ()
        
end

*)

(*
  try
(*    lprintf "upload ?"; lprint_newline (); *)
    if !!new_upload_system then
      NewUpload.next_uploads ()
    else
      OldUpload.next_uploads ()
  with e -> 
      lprintf "exc %s in upload" (Printexc2.to_string e);
      lprint_newline () 
      
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
    
*)
