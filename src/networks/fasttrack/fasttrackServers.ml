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

open Options
open Queues
open Printf2
open Md4
open BasicSocket
open TcpBufferedSocket

open AnyEndian
  
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonSwarming
open CommonTypes
open CommonGlobals
  
open FasttrackTypes
open FasttrackGlobals
open FasttrackOptions
open FasttrackProtocol
open FasttrackComplexOptions
open FasttrackProto


let server_parse_after s gconn sock = 
  try
    let b = buf sock in
    let len = b.len in
    if len > 0 then
      match int_of_char b.buf.[b.pos] with
        0x50 ->
(*          lprintf "We have got a ping\n"; *)
          buf_used sock 1;
          server_send_pong s
      | 0x52 ->
(*          lprintf "We have got a pong\n"; *)
          buf_used sock 1          
      | 0x4b ->
(*          lprintf "We have got a real packet\n"; *)
          begin
            if len > 4 then
              match s.server_ciphers with
                None -> assert false
              | Some ciphers ->

(*                dump_sub b.buf b.pos b.len; *)
                  let xtype = Int64.to_int (Int64.rem ciphers.in_xinu int64_3) in
                  
                  let msg_type, size = 
                    match xtype with
                      0 ->
                        let msg_type = get_int8 b.buf (b.pos+1) in
(* zero *)
                        let len_hi = get_int8 b.buf (b.pos+3) in
                        let len_lo = get_int8 b.buf (b.pos+4) in
                        msg_type, (len_hi lsl 8) lor len_lo
                    | 1 ->
(* zero *)
                        let len_hi = get_int8 b.buf (b.pos+2) in
                        let msg_type = get_int8 b.buf (b.pos+3) in
                        let len_lo = get_int8 b.buf (b.pos+4) in
                        msg_type, (len_hi lsl 8) lor len_lo
                    | _ ->
(*zero*)        
                        let len_lo = get_int8 b.buf (b.pos+2) in
                        let len_hi = get_int8 b.buf (b.pos+3) in
                        let msg_type = get_int8 b.buf (b.pos+4) in
                        msg_type, (len_hi lsl 8) lor len_lo
                  in
(*                lprintf "Message to read: xtype %d type %d len %d\n"
                  xtype msg_type size; *)
                  
                  if len >= size + 5 then begin
                      
                      ciphers.in_xinu <- Int64.logxor ciphers.in_xinu  
                        (Int64.logand
                          (Int64.lognot (Int64.of_int (size + msg_type))) 
                        int64_ffffffff);
                      let m = String.sub b.buf (b.pos+5) size in
                      buf_used sock (size + 5);
                      FasttrackHandler.server_msg_handler sock s msg_type m
                    end (* else
                  lprintf "Waiting for remaining %d bytes\n"
                    (size+5 - len) 
                  else
                    lprintf "Packet too short\n" *)
          end
      | n ->
          lprintf "Packet not understood: %d\n" n;
          raise Exit
  with e -> 
      lprintf "Exception %s in server_parse_after\n"
        (Printexc2.to_string e);
      close sock (Closed_for_error "Reply not understood")
      
let greet_supernode s =
  let b = Buffer.create 100 in

(* Hope it is the right order: ntohl(sa.sin_addr.s_addr) *)
  LittleEndian.buf_ip b (client_ip s.server_sock);
  BigEndian.buf_int16 b 0; (* client_port *)

(* This next byte represents the user's advertised bandwidth, on
* a logarithmic scale.  0xd1 represents "infinity" (actually,
* 1680 kbps).  The value is approximately 14*log_2(x)+59, where
* x is the bandwidth in kbps. *)
  buf_int8 b 0x68;
(* 1 byte: dunno. *)
  buf_int8 b  0x00;
  
  Buffer.add_string b (client_name ()); (* no ending 0 *)
  let m = Buffer.contents b in
  server_send s 0x02 m;
  server_send_ping s
  
let server_parse_netname s gconn sock = 
  let b = TcpBufferedSocket.buf sock in
  let len = b.len in
  let start_pos = b.pos in
  let end_pos = start_pos + len in
  let buf = b.buf in
  let net = String.sub buf start_pos len in
(*  lprintf "net: [%s]\n" (String.escaped net); *)
  let rec iter pos =
    if pos < end_pos then 
      if buf.[pos] = '\000' then begin
          let netname = String.sub buf start_pos (pos-start_pos) in
(*          lprintf "netname: [%s]\n" (String.escaped netname); *)
          buf_used sock (pos-start_pos+1);
          match s.server_ciphers with
            None -> assert false
          | Some ciphers ->
              gconn.gconn_handler <- 
                CipherReader (ciphers.in_cipher, server_parse_after s);
              greet_supernode s
        end else
        iter (pos+1)
  in
  iter start_pos
  
let server_parse_cipher s gconn sock = 
  let b = TcpBufferedSocket.buf sock in
  if b.len >= 8 then
    match s.server_ciphers with
      None -> assert false
    | Some ciphers ->
        cipher_packet_get b.buf b.pos ciphers.in_cipher ciphers.out_cipher;
        buf_used sock 8;
        server_crypt_and_send s ciphers.out_cipher (network_name ^ "\000");
        gconn.gconn_handler <- CipherReader (ciphers.in_cipher, server_parse_netname s);
        lprintf "waiting for netname\n"
          
let out_cipher_seed = Int32.of_string "0x0fACB1238"

let connect_server h =  
  let s = match h.host_server with
      None -> 
        let s = new_server h.host_addr h.host_port in
        h.host_server <- Some s;
        s
    | Some s -> s
  in
  match s.server_sock with
    ConnectionWaiting -> ()
  | ConnectionAborted -> s.server_sock <- ConnectionWaiting
  | Connection _ -> ()
  | NoConnection -> 
      incr nservers;
      s.server_sock <- ConnectionWaiting;
      add_pending_connection (fun _ ->
          decr nservers;
          match s.server_sock with
            ConnectionAborted -> 
              s.server_sock <- NoConnection;
              free_ciphers s
          | Connection _ | NoConnection -> ()
          | ConnectionWaiting ->
              try
                let ip = Ip.ip_of_addr h.host_addr in
                if not (Ip.valid ip) then
                  failwith "Invalid IP for server\n";
                let port = s.server_host.host_port in
                if !verbose_msg_servers then begin
                    lprintf "CONNECT TO %s:%d\n" 
                    (Ip.string_of_addr h.host_addr) port;
                  end;  
                h.host_tcp_request <- last_time ();
                let sock = connect "gnutella to server"
                    (Ip.to_inet_addr ip) port
                    (fun sock event -> 
                      match event with
                        BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
(*                  lprintf "RTIMEOUT\n"; *)
                          disconnect_from_server nservers s Closed_for_timeout
                      | _ -> ()
                  ) in
                TcpBufferedSocket.set_read_controler sock download_control;
                TcpBufferedSocket.set_write_controler sock upload_control;
                
                set_server_state s Connecting;
                s.server_sock <- Connection sock;
                incr nservers;
                set_fasttrack_sock sock !verbose_msg_servers
                  (Reader (server_parse_cipher s)
                
                );
                set_closer sock (fun _ error -> 
(*            lprintf "CLOSER %s\n" error; *)
                    disconnect_from_server nservers s error);
                set_rtimeout sock !!server_connection_timeout;
                
                let in_cipher = create_cipher () in
                let out_cipher = create_cipher () in
                s.server_ciphers <- Some {
                  in_cipher = in_cipher;
                  out_cipher = out_cipher;
                  in_xinu = Int64.of_int 0x51;
                  out_xinu = Int64.of_int 0x51;
                };
                set_cipher out_cipher out_cipher_seed 0x29;
                
                let s = String.create 12 in
                cipher_packet_set out_cipher s 0;
                if !verbose_msg_servers then begin
                    lprintf "SENDING %s\n" (String.escaped s);
                    AnyEndian.dump s;
                  end;
                write_string sock s;
              with _ ->
                  disconnect_from_server nservers s Closed_connect_failed
      )

let get_file_from_source c file =
  if connection_can_try c.client_connection_control then begin
      connection_try c.client_connection_control;
      match c.client_user.user_kind with
        Indirect_location ("", uid) ->
          (*
          lprintf "++++++ ASKING FOR PUSH +++++++++\n";   

(* do as if connection failed. If it connects, connection will be set to OK *)
          connection_failed c.client_connection_control;

          let uri = (find_download file c.client_downloads).download_uri in
          List.iter (fun s ->
                FasttrackProto.server_send_push s uid uri
) !connected_servers;
*)
          lprintf "PUSH NOT IMPLEMENTED\n"
      | _ ->
          FasttrackClients.connect_client c
    end

    
let exit = Exit
  
let disconnect_server s r =
  match s.server_sock with
  | Connection sock -> close sock r
  | ConnectionWaiting -> 
      s.server_sock <- ConnectionAborted;
      free_ciphers s
      
  | _ -> ()
    
let download_file (r : result) =
  let file = new_file (Md4.random ()) 
    r.result_name r.result_size r.result_hash in
  lprintf "DOWNLOAD FILE %s\n" file.file_name; 
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun (user, index) ->
      let c = new_client user.user_kind in
      add_download file c index;
      get_file_from_source c file;
  ) r.result_sources;
  Fasttrack.recover_file file;
  ()

let recover_files () =
  List.iter (fun file ->
      Fasttrack.recover_file file 
  ) !current_files;
  ()
    
let ask_for_files () =
  List.iter (fun file ->
      List.iter (fun c ->
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  List.iter (fun s ->
      try
        let ss = Fifo.take s.server_searches in
        server_send_query s ss
      with _ -> ()
  ) !connected_servers;
  ()

  (*
let udp_handler p =
  let (ip,port) = match p.UdpSocket.addr with
    | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
    | _ -> raise Not_found
  in
  let buf = p.UdpSocket.content in
  let len = String.length buf in
  Fasttrack.udp_handler (Ip.addr_of_ip ip) port buf
    *)

let _ =
  server_ops.op_server_disconnect <- (fun s ->
    disconnect_server s Closed_by_user);
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s Closed_by_user
  )
  
let manage_host h =
  try
    let current_time = last_time () in
(*    lprintf "host queue before %d\n" (List.length h.host_queues);  *)

(*    lprintf "host queue after %d\n" (List.length h.host_queues); *)
(* Don't do anything with hosts older than one hour and not responding *)
    if max h.host_connected h.host_age > last_time () - 3600 then begin
        host_queue_add workflow h current_time;
(* From here, we must dispatch to the different queues *)
        match h.host_kind with
        | Ultrapeer | IndexServer ->        
            if h.host_udp_request + 600 < last_time () then begin
(*              lprintf "waiting_udp_queue\n"; *)
                host_queue_add waiting_udp_queue h current_time;
              end;
            if h.host_tcp_request + 600 < last_time () then begin
                host_queue_add  ultrapeers_waiting_queue h current_time;
              end
        | Peer ->
            if h.host_udp_request + 600 < last_time () then begin
(*              lprintf "g01_waiting_udp_queue\n"; *)
                host_queue_add waiting_udp_queue h current_time;
              end;
            if h.host_tcp_request + 600 < last_time () then
              host_queue_add peers_waiting_queue h current_time;
      end    else 
    if max h.host_connected h.host_age > last_time () - 3 * 3600 then begin
        host_queue_add workflow h current_time;      
      end else
(* This host is too old, remove it *)
      ()
          
  with e ->
      lprintf "Exception %s in manage_host\n" (Printexc2.to_string e)
      
let manage_hosts () = 
  let rec iter () =
    let h = host_queue_take workflow in
    manage_host h;
    iter ()
  in
  (try iter () with _ -> ());
  (try while true do FasttrackClients.connect_client
        (Fifo.take reconnect_clients) done
      with _ -> ())

        
module Pandora = struct
    
    type t = UDP | TCP
    
    type cnx = {
        ip1 : string;
        port1 : int;
        ip2 : string;
        port2 : int;
        packets_in : Buffer.t;
        packets_out : Buffer.t;
      }
    let connections = Hashtbl.create 13
    
    let parse_packet msg_type m =
      match msg_type with
        0x00 -> (* SessMsgNodeList *)
          lprintf "SessMsgNodeList\n";
          
          let n = String.length m / 8 in
          for i = 0 to n - 1 do
            let l_ip = LittleEndian.get_ip m (i*8) in
            let l_port = BigEndian.get_int16 m (i*8+4) in
            let unknown = BigEndian.get_int16 m (i*8+6) in
            
            lprintf "    LittleEndian Node %s:%d   %d\n" (Ip.to_string l_ip) 
            l_port unknown;
          
          done;
      
      | 0x06 -> (* SessMsgQuery *)
          lprintf "SessMsgQuery (len %d)\n" (String.length m);
          dump m
          
      | 0x07 -> (* SessMsgQueryReply *)
          lprintf "SessMsgQueryReply\n";

(* probably supernode address *)
          let s_ip = LittleEndian.get_ip m 0 in
          let s_port = BigEndian.get_int16 m 4 in
          
          let id = BigEndian.get_int16 m 6 in
          
          let nresults = BigEndian.get_int16 m 8 in
          lprintf "Results: %d for Search %d\n" nresults id;
          
          let len = String.length m in
          let rec iter pos n = 
            if n > 0 && pos + 32 < len then
              let user_ip = LittleEndian.get_ip m pos in
              let user_port = BigEndian.get_int16 m (pos+4) in
              let user_bandwidth = get_int8 m (pos+6) in
              let pos = pos + 7 in
              let user_name, user_netname, pos =
                if get_int8 m pos = 2 then
                  "unknown", "unknown", pos+1
                else
                let end_name = String.index_from m pos '\001' in
                let end_netname = String.index_from m end_name '\000' in
                String.sub m pos (end_name - pos),
                String.sub m (end_name+1) (end_netname - end_name -1),
                end_netname + 1
              in
              
              lprintf "   User %s@%s %s:%d\n" user_name user_netname
                (Ip.to_string user_ip) user_port;
              
              let result_hash = Md5Ext.direct_of_string (String.sub m pos 20) in
              let checksum, pos = get_dynint m (pos+20) in
              let result_size, pos = get_dynint m pos in
              let ntags, pos = get_dynint m pos in
              let ntags = Int64.to_int ntags in
              
              lprintf "   Result %s size: %Ld tags: %d\n" 
                (Md5Ext.to_string_case false result_hash) result_size ntags;
              lprintf "    %s\n" (Md5Ext.to_hexa result_hash);
              
              
              let rec iter_tags name pos n tags =
                if n > 0 && pos < len-2 then
                  let tag, pos = get_dynint m pos in
                  let tag = Int64.to_int tag in
                  let tag_len, pos = get_dynint m pos in
                  let tag_len = Int64.to_int tag_len in
                  let tagdata = String.sub m pos tag_len in
                  let name = if tag = 2 then tagdata else name in
                  iter_tags name (pos + tag_len) (n-1) 
                  ((tag, tagdata) :: tags)
                else
                  name, tags, pos
              in
              let result_name, tags, pos = iter_tags "Unknown" pos ntags [] in
              List.iter (fun (tag, tagdata) ->
                  lprintf "      Tag: %d --> %s\n" tag (String.escaped tagdata);
              ) tags;
(*
          let url = Printf.sprintf 
            "FastTrack://%s:%d/.hash=%s" (Ip.to_string user_ip)
            user_port (Md5Ext.to_string_case false result_hash) in *)
              let url = Printf.sprintf 
                  "/.hash=%s" (Md5Ext.to_string_case false result_hash) in 
              lprintf "URL = %s\n" url;
              iter pos (n-1)
          in
          iter 10 nresults
      
      | 0x08 -> (* SessMsgQueryEnd *)
          let s = BigEndian.get_int16 m 0 in
          lprintf "SessMsgQueryEnd for search %d\n" s;
(*
fst_searchlist_process_reply (FST_PLUGIN->searches, msg_type, msg_data);


*)
      
      | 0x09 -> (* SessMsgNetworkStats *)
          lprintf "SessMsgNetworkStats\n";

(*
unsigned int mantissa, exponent;

if(fst_packet_remaining(msg_data) < 12) // 97 bytes total now? was 60?
break;

FST_PLUGIN->stats->users = ntohl(fst_packet_get_uint32 (msg_data));	// number of users	
FST_PLUGIN->stats->files = ntohl(fst_packet_get_uint32 (msg_data));	// number of files

mantissa = ntohs(fst_packet_get_uint16 (msg_data));	// mantissa of size
exponent = ntohs(fst_packet_get_uint16 (msg_data));	// exponent of size

if (exponent >= 30)
FST_PLUGIN->stats->size = mantissa << (exponent-30);
else
FST_PLUGIN->stats->size = mantissa >> (30-exponent);	 

// what follows in the packet is the number of files and their size per media type (6 times)
// we do not currently care for those

// something else with a size of 37 byte follows, dunno what it is

FST_DBG_3 ("received network stats: %d users, %d files, %d GB", FST_PLUGIN->stats->users, FST_PLUGIN->stats->files, FST_PLUGIN->stats->size);
break;


  *)
      
      | 0x1d -> (* SessMsgNetworkName *)
          lprintf "SessMsgNetworkName: %s\n" m;
      
      | _ ->
          lprintf "   ******* Unknown message %d\n" msg_type;
          AnyEndian.dump m
          
      
      
    let rec parse_packets pos s ciphers = 
      let len = String.length s - pos in
      if len > 0 then
        match int_of_char s.[pos] with
          0x50 ->
            lprintf "We have got a ping\n"; 
            parse_packets (pos+1) s ciphers
        | 0x52 ->
            lprintf "We have got a pong\n"; 
            parse_packets (pos+1) s ciphers
        | 0x4b ->
(*            lprintf "We have got a real packet\n"; *)
            begin
              if len > 4 then
(*                dump_sub s pos b.len; *)
                let xtype = Int64.to_int (Int64.rem ciphers.in_xinu int64_3) in
                
                let msg_type, size = 
                  match xtype with
                    0 ->
                      let msg_type = get_int8 s (pos+1) in
(* zero *)
                      let len_hi = get_int8 s (pos+3) in
                      let len_lo = get_int8 s (pos+4) in
                      msg_type, (len_hi lsl 8) lor len_lo
                  | 1 ->
(* zero *)
                      let len_hi = get_int8 s (pos+2) in
                      let msg_type = get_int8 s (pos+3) in
                      let len_lo = get_int8 s (pos+4) in
                      msg_type, (len_hi lsl 8) lor len_lo
                  | _ ->
(*zero*)        
                      let len_lo = get_int8 s (pos+2) in
                      let len_hi = get_int8 s (pos+3) in
                      let msg_type = get_int8 s (pos+4) in
                      msg_type, (len_hi lsl 8) lor len_lo
                in
(*                lprintf "Message to read: xtype %d type %d len %d\n"
                  xtype msg_type size; *)
                
                if len >= size + 5 then begin
                    
                    ciphers.in_xinu <- Int64.logxor ciphers.in_xinu  
                      (Int64.logand
                        (Int64.lognot (Int64.of_int (size + msg_type))) 
                      int64_ffffffff);
                    let m = String.sub s (pos+5) size in
                    lprintf "\nMessage %d:\n" msg_type;
                    parse_packet msg_type m;
                    parse_packets (pos + size + 5) s ciphers;
                  end else
                  lprintf "Waiting for remaining %d bytes\n"
                    (size+5 - len) 
              else
                lprintf "Packet too short\n" 
          end
      | n ->
          lprintf "Packet not understood: %d\n" n;
          raise Exit

      
    let parse_netname start_pos s ciphers = 
      let len = String.length s in
      let rec iter pos =
        if pos < len then 
          if s.[pos] = '\000' then begin
              let netname = String.sub s start_pos (pos-start_pos) in
              lprintf "netname: [%s]\n" (String.escaped netname); 
              
              parse_packets (pos+1) s ciphers
            end else
            iter (pos+1)
        else failwith "End of name not found"
      in
      iter (start_pos)
      
    let parse s_out s_in =
      let ciphers = {
          in_cipher = create_cipher ();
          out_cipher = create_cipher ();
          in_xinu = Int64.of_int 0x51;
          out_xinu = Int64.of_int 0x51;         
        } in
      begin
        try
          let dummy_cipher = create_cipher () in
          cipher_packet_get s_out 4 ciphers.out_cipher dummy_cipher;
          cipher_free dummy_cipher;
          cipher_packet_get s_in 0 ciphers.in_cipher ciphers.out_cipher;
          (
            let len = String.length s_out in
            let start_pos = 12 in
            apply_cipher ciphers.out_cipher s_out start_pos (len-start_pos);
            (*
            lprintf "Ciphered: [%s]\n" (String.escaped 
(String.sub s_out start_pos (min (len - start_pos) 1000)));
  *)
          );
          (
            let len = String.length s_in in
            let start_pos = 8 in
            apply_cipher ciphers.in_cipher s_in start_pos (len-start_pos);
            (*
            lprintf "Ciphered: [%s]\n" (String.escaped 
(String.sub s_in start_pos (min (len - start_pos) 1000)));
   *)
          );
          
          lprintf "---------------------------------------------->\n";
          parse_netname 12 s_out { ciphers with
            in_xinu = ciphers.out_xinu; in_cipher = ciphers.out_cipher };
          lprintf "<----------------------------------------------\n";
          parse_netname 8 s_in ciphers;
          (*
              (*
dump_sub s (start_pos) (len - start_pos); 
  *)
              
            end
  *)
        with e ->
            lprintf "exception %s while parsing stream\n"
              (Printexc2.to_string e) 
      end;
      cipher_free ciphers.in_cipher;
      cipher_free ciphers.out_cipher
    
    let rec iter s pos =
      if s.[pos] = '\n' then
        if s.[pos+1] = '\n' then pos+2
        else
        if s.[pos+1] = '\r' then
          if s.[pos+2] = '\n' then pos+3
          else iter s (pos+1)
        else iter s (pos+1)
      else 
      if s.[pos] = '\r' then
        if s.[pos] = '\n' then
          if s.[pos+1] = '\n' then pos+2
          else
          if s.[pos+1] = '\r' then
            if s.[pos+2] = '\n' then pos+3
            else iter s (pos+1)
          else iter s (pos+1)
        else
          iter s (pos+1)
      else iter s (pos+1)
    
    let hescaped s =
      String2.replace_char s '\r' ' ';s
    
    let commit () =  
      Hashtbl.iter (fun _ cnx ->
          try
            lprintf "CONNECTION %s:%d --> %s:%d\n" 
              cnx.ip1 cnx.port1 cnx.ip2 cnx.port2;
            
            let s = Buffer.contents cnx.packets_out in
            let len = String.length s in
            
            if String2.starts_with s "GET" || 
              String2.starts_with s "POST" then begin
                (*
                lprintf "Http connect to\n";
                let h1 = iter s 0 in
                lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
                
                let s = Buffer.contents cnx.packets_in in
                if String2.starts_with s "HTTP" then begin
                    lprintf "Http connected from\n";
                    let h1 = iter s 0 in
                    lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
                  end 
                else
                  lprintf "bad HTTP reply\n"
*)
                ()
              end else begin
                parse 
                (Buffer.contents cnx.packets_out) 
                (Buffer.contents cnx.packets_in);
              end
          with           
          | e ->
              lprintf "Exception %s\n" (Printexc2.to_string e)
      ) connections
    
    let new_packet (kind:t) (number:int) ip1 port1 ip2 port2 data = 
      match kind with
        UDP -> 
          begin
            try
(*              lprintf "New packet:\n%s\n" (String.escaped data);           *)
              ()
            with e ->
(*                lprintf "Could not parse UDP packet:\n"; *)
                ()
          end
      | TCP -> 
          let out_packet = (ip1, port1, ip2, port2) in
          let in_packet = (ip2, port2, ip1, port1) in
          
          try
            let cnx =  Hashtbl.find connections out_packet in
            Buffer.add_string cnx.packets_out data; 
            ()
          with _ ->
              try
                let cnx =  Hashtbl.find connections in_packet in
                Buffer.add_string cnx.packets_in data 
              with _ ->
                let cnx = {
                    ip1 = ip1;
                    port1 = port1;
                    ip2 = ip2;
                    port2 = port2;
                    packets_out = Buffer.create 100;
                    packets_in = Buffer.create 100;
                  } in
                  Hashtbl.add connections out_packet cnx;
                  Buffer.add_string cnx.packets_out data
          
  end
  
  
  