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

open Gnutella2
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open LimewireTypes
open LimewireGlobals
open LimewireOptions
open LimewireProtocol
open LimewireComplexOptions
  
module DG = CommonGlobals
module DO = CommonOptions

let bloom_hash_magic = Int32.of_string  "0x4F1BBCDC"
let bloom_hash_magic_int64 =  Int32ops.int64_of_uint32 bloom_hash_magic

let bloom_hash_fast x bits =
  let xx = Int32ops.int64_of_uint32 x in
  let prod = Int64.mul xx bloom_hash_magic_int64 in
  let ret = Int64.shift_left prod  32 in     (* prod << 32 *)
  Int64.shift_right_logical ret (32 + (32 - bits))   (* ret >>> (32 + (32 - bits))  *)

let bloom_hash_full s pos len bits =
  let xor = ref Int32.zero in
  let j = ref 0 in
  for i = pos to len - 1 do
    let b = Int32.of_int (int_of_char (Char.lowercase s.[i])) in
    let b = Int32.shift_left b (!j * 8) in
    xor := Int32.logxor !xor b;
    j := (!j+1) mod 4;
  done;
  bloom_hash_fast !xor bits
  
let bloom_hash s bits = bloom_hash_full s 0 (String.length s) bits
  
let create_qrt_table words table_size =
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  let old_array = Array.create table_length infinity in
  let array = Array.create table_length infinity in
  List.iter (fun w ->
      let pos = bloom_hash w table_size in
      lprintf "Position %Ld\n" pos;
      array.(Int64.to_int pos) <- 1;
  ) words;
  let string_size = table_length/2 in
  let table = String.create  string_size in
  for i = 0 to string_size - 1 do
    table.[i] <- char_of_int (
      (
        ((array.(i*2) - old_array.(i*2)) land 15) lsl 4) + 
      ((array.(i*2+1) - old_array.(i*2+1)) land 15))
  done;
  table
(*  
   create_qrt_table ["qrp"] 3;;
- : string = "\000\000\000\160"
   create_qrt_table ["test"] 3;;


*)

module WordSet = Set.Make(struct
      type t = string
      let compare = compare
    end)
  
let new_shared_words = ref false
let all_shared_words = ref []
let cached_qrt_table = ref ""
                
let extension_list = [
    "mp3" ; "avi" ; "jpg" ; "jpeg" ; "txt" ; "mov" ; "mpg" 
]
      
let rec remove_short list list2 =
  match list with
    [] -> List.rev list2
  | s :: list -> 
      if List.mem s extension_list then 
        remove_short list (s :: list2) else 
      
      if String.length s < 5 then (* keywords should had list be 5 bytes *)
        remove_short list list2
      else
        remove_short list (s :: list2)

let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' '
  done;
  lprintf "STEM %s\n" s;
  remove_short (String2.split s ' ') []

let update_shared_words () = 
  all_shared_words := [];
  let module M = CommonUploads in
  let words = ref WordSet.empty in
  let register_words s = 
    let ws = stem s in
    List.iter (fun w ->
        words := WordSet.add w !words
    ) ws
  in
  let rec iter node =
    List.iter (fun sh ->
        lprintf "CODED name: %s\n" sh.M.shared_codedname;
        register_words sh.M.shared_codedname;
    ) node.M.shared_files;
    List.iter (fun (_,node) ->
        register_words node.M.shared_dirname;
        iter node
    ) node.M.shared_dirs;
  in
  iter M.shared_tree;
  WordSet.iter (fun s ->
      all_shared_words := s :: !all_shared_words
  ) !words;
  lprintf "SHARED WORDS: ";
  List.iter (fun s ->
      lprintf "%s " s
  ) !all_shared_words;
  lprint_newline ()
  
  
let send_qrt_sequence s =
  
  if !new_shared_words then begin
      update_shared_words ();
      new_shared_words := false;
    end;
  
  let table_size = 10 in
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  server_send_new s (QrtResetReq {
      QrtReset.table_length = table_length;
      QrtReset.infinity = infinity;
    });
  
  if !cached_qrt_table = "" then 
        cached_qrt_table := create_qrt_table !all_shared_words table_size;
  let table = !cached_qrt_table in
  
  let compressor, table =
    if Autoconf.has_zlib then
      1, Autoconf.zlib__compress_string table
    else
      0, table 
  in
  
  server_send_new s (QrtPatchReq {
      QrtPatch.seq_no = 1;
      QrtPatch.seq_size = 1;
      QrtPatch.compressor = compressor;
      QrtPatch.entry_bits = 4;
      QrtPatch.table = table;
    })
  
let send_query servers words xml_query =
  let module Q = Query in
  let t = QueryReq {
      Q.min_speed = 0;
      Q.keywords = words;
      Q.xml_query  = [xml_query] } in
  let p = new_packet t in
  if !verbose_msg_servers then begin
      lprintf "sending query for <%s>\n" words;
    end;
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock -> server_send sock p
  ) servers;
  p

  
let get_name_keywords file_name =
  match stem file_name with 
    [] | [_] -> 
      lprintf "Not enough keywords to recover %s\n" file_name;
      [file_name]
  | l -> l
      
let gen_query file servers =
  if file.file_uids = [] then
    let keywords = get_name_keywords file.file_name in
    let words = String2.unsplit keywords ' ' in
    ignore (send_query servers words "urn:")
  else
    List.iter (fun uid ->
        let module Q = Query in
        let xml_query = 
          (match uid with
              Bitprint (s,_,_) -> s
            | Sha1 (s,_) -> s
            | Md4 (s,_) -> s
            | Md5 (s,_) -> s
          ) in
        ignore (send_query servers "" xml_query)
    ) file.file_uids    

  
let recover_files () =
  List.iter (fun file ->
      gen_query file !connected_servers
  ) !current_files;
  ()
  
let recover_files_from_server s =
  if !verbose_msg_servers then begin
      lprintf "trying to recover files from server\n";
    end;
  List.iter (fun file ->
      gen_query file [s]
  ) !current_files;
  ()
  
  
let redirector_to_client p sock = 
(*  lprintf "redirector_to_client\n"; *)
  match p.pkt_payload with
    PongReq t ->
      let module P = Pong in
(*      lprintf "ADDING PEER %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      Fifo.put peers_queue (t.P.ip, t.P.port);
  | _ -> ()
      
let redirector_parse_header gconn sock header = 
(*  lprintf "redirector_parse_header\n";*)
  if String2.starts_with header gnutella_ok then begin
(*      lprintf "GOOD HEADER FROM REDIRECTOR:waiting for pongs";*)
      server_send_new sock (
        let module P = Ping in
        PingReq (P.ComplexPing {
          P.ip = DO.client_ip (Some sock);
          P.port = !!client_port;
          P.nfiles = Int64.zero;
          P.nkb = Int64.zero;
          P.s = "none:128:false";
          }));
      gconn.gconn_handler <- Reader (gnutella_handler
        parse redirector_to_client)
    end else begin
      if !verbose_msg_servers then begin
          lprintf "BAD HEADER FROM REDIRECTOR: \n";
          LittleEndian.dump header;
        end;
      close sock "bad header";
      redirector_connected := false;
      raise Not_found
    end
  
let connect_to_redirector () =
  match !redirectors_to_try with
    [] ->
      redirectors_to_try := !!redirectors
  | name :: tail ->
      redirectors_to_try := tail;
(*      lprintf "connect to redirector\n"; *)
      Ip.async_ip name (fun ip ->
          try
            let sock = connect  "limewire to redirector"
                (Ip.to_inet_addr ip) 6346
                (fun sock event -> 
                  match event with
                    BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
                      close sock "timeout";
                      redirector_connected := false;
(*                  lprintf "TIMEOUT FROM REDIRECTOR\n"*)
                  | _ -> ()
              ) in
            TcpBufferedSocket.set_read_controler sock download_control;
            TcpBufferedSocket.set_write_controler sock upload_control;
            
            
            redirector_connected := true;
            set_gnutella_sock sock !verbose_msg_servers
              (HttpHeader redirector_parse_header);
            set_closer sock (fun _ _ -> 
(*            lprintf "redirector disconnected\n"; *)
                redirector_connected := false);
            set_rtimeout sock 10.;
            set_lifetime sock 120.;
            write_string sock "GNUTELLA CONNECT/0.4\n\n";
          with e ->
              lprintf "Exception in connect_to_redirector: %s\n"
                (Printexc2.to_string e); 
              redirector_connected := false
      )
      
let disconnect_from_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
(*
  lprintf "DISCONNECT FROM SERVER %s:%d\n" 
        (Ip.to_string s.server_ip) s.server_port;
  *)
      close sock "timeout";
      s.server_sock <- None;
      set_server_state s (NotConnected (-1));
      decr nservers;
      s.server_need_qrt <- true;
      if List.memq s !connected_servers then begin
          connected_servers := List2.removeq s !connected_servers;
        end;
      server_remove s

let update_user t =
  let module Q = QueryReply in
  let user = new_user t.Q.guid (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  user.user_speed <- t.Q.speed;
  user

let update_client t =
  let module Q = QueryReply in
  let c = new_client t.Q.guid (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  
  c.client_user.user_speed <- t.Q.speed;
  c

let find_header header headers default =
  try
    List.assoc header headers
  with Not_found -> default

let add_uid r uid =
  if not (List.mem uid r.result_uids) then
    r.result_uids <- uid :: r.result_uids
      
let server_to_client s p sock =
  if !verbose_msg_servers then begin
      lprintf "server_to_client\n";
      print p;
    end;
  match p.pkt_payload with
  | PingReq t ->
      if p.pkt_hops <= 3 then
        server_send sock {
          p with 
          pkt_hops = 0;
          pkt_type = PONG;
          pkt_payload = (
            let module P = Pong in
            PongReq {
              P.ip = (DO.client_ip (Some sock));
              P.port = !!client_port;
              P.nfiles = 10;
              P.nkb = 10;
            });
        };
      if s.server_need_qrt then begin
          s.server_need_qrt <- false;
          send_qrt_sequence sock
        end
  
  
  | PongReq t ->
      
      let module P = Pong in
(*      lprintf "FROM %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      if p.pkt_uid = s.server_ping_last then begin
          s.server_nfiles_last <- s.server_nfiles_last + t.P.nfiles;
          s.server_nkb_last <- s.server_nkb_last + t.P.nkb
        end
  
  | QueryReq t ->
(*      lprintf "REPLY TO QUERY NOT IMPLEMENTED YET :(\n";*)
      lprintf "SEARCH RECEIVED\n";
      begin
        try
          let q = 
            let q = 
              match String2.split_simplify t.Query.keywords ' ' with
                [] -> raise Not_found
              | s :: tail ->
                  List.fold_left (fun q s ->
                      QAnd (q, (QHasWord s))
                  ) (QHasWord s) tail
            in
(*
            match t.Search.sizelimit with
            | NoLimit -> q
            | AtMost n -> 
                QAnd (QHasMaxVal (CommonUploads.filesize_field, n),q)
            | AtLeast n -> 
QAnd (QHasMinVal (CommonUploads.filesize_field, n),q)
*) 
            q
          in
          try
            let files = CommonUploads.query q in
            lprintf "%d replies found\n" (Array.length files); 

(* How many matches should we return ? Let's say 10. *)
            if files <> [||] then
              let module M = QueryReply in
              let module C = CommonUploads in
              let replies = ref [] in
              for i = 0 to mini (Array.length files - 1) 9 do
                let sh = files.(i) in
                replies := {
                  M.index = sh.C.shared_id;
                  M.size = sh.C.shared_size;
                  M.name = sh.C.shared_codedname;
                  M.info = [];
                } :: !replies
              done;
              let module P = QueryReply in
              let t = QueryReplyReq {
                  P.guid = !!client_uid;
                  P.ip = DO.client_ip None;
                  P.port = !!client_port;
                  P.speed = 1300; 
                  P.files = !replies; 
                  P.vendor = "MLDK"; 
                  P.speed_measured = None; 
                  P.busy = None; 
                  P.stable = None; 
                  P.xml_reply = ""; 
                  P.support_chat = false; 
                  P.dont_connect = None;
                } in
              let pp = { (new_packet t) with
                  pkt_hops = 0;
                  pkt_uid = p.pkt_uid;
                } in
              server_send sock pp
          
          with Not_found -> ()
        with Not_found ->
            lprintf "Query browse\n"   
      end

(* GUID + Index of file to be pushed + ip + port *)
  | PushReq t ->
      let uid = t.Push.guid in
      let index = t.Push.index in
      let ip = t.Push.ip in
      let port = t.Push.port in
      LimewireClients.push_connection uid index ip port
  
  | QueryReplyReq t ->
(*      lprintf "REPLY TO QUERY\n";*)
      let module Q = QueryReply in
      let s = 
        try 
          let s = Hashtbl.find searches_by_uid p.pkt_uid in
          let user = update_user t in
          Some (s, user) with
          _ -> None 
      in
(*          lprintf "ADDING RESULTS\n";*)
      List.iter (fun f ->
(*              lprintf "NEW RESULT %s\n" f.Q.name; *)
          
          let uids = ref [] in
          List.iter (fun s ->
              if s.[0] = '{' || s.[0] = '<' then begin
(* probably XML. print to remember that we should be able to use this
information. *)
                  lprintf "xml of result: %s\n" (String.escaped s);
                end else
                uids := (extract_uids s) @ !uids
          ) f.Q.info;
          
          (try
              let file = Hashtbl.find files_by_key (f.Q.name, f.Q.size) in
              lprintf "++++++++++++ RECOVER FILE %s +++++++++++\n" 
                file.file_name; 
              let c = update_client t in
              add_download file c (FileByIndex f.Q.index);              
            with _ -> ());
          
          List.iter (fun uid ->
              try
                let file = Hashtbl.find files_by_uid uid in
                lprintf "++++++++++++ RECOVER FILE %s +++++++++++\n" 
                  file.file_name; 
                
                if file_size file = Int64.zero then begin
                    lprintf "Recover correct file size\n";
                    file.file_file.impl_file_size <- f.Q.size;
                    file_must_update file;
                  end;
                
                let c = update_client t in
                add_download file c (FileByIndex f.Q.index);
              with _ -> ()
          ) !uids;
          
          match s with
            None -> ()
          | Some (s, user) ->
              let r = new_result f.Q.name f.Q.size !uids in
              add_source r user (FileByIndex f.Q.index);
          
              CommonInteractive.search_add_result s.search_search 
              r.result_result;
      ) t.Q.files;
  | _ -> ()

let gnutella_proto = "GNUTELLA/"
let gnutella_proto_len = String.length gnutella_proto
  
let server_parse_header s gconn sock header =
  if !verbose_msg_servers then  LittleEndian.dump_ascii header;  
  try
    if not (String2.starts_with header gnutella_proto) then 
      failwith "Reply is not in gnutella protocol";
    let space_pos = String.index header ' ' in
    let proto = String.sub header gnutella_proto_len (
        space_pos - gnutella_proto_len) in
    let code = String.sub header space_pos 3 in
    let lines = Http_client.split_header header in
    let headers =        
      match lines with
        [] -> []
      | _ :: headers ->
          Http_client.cut_headers headers 
    in
    
    let ultra_peer = ref false in
    let gnutella2 = ref false in
    let deflate = ref false in
    List.iter (fun (header, value) ->
        match (String.lowercase header) with
        
        | "user-agent" ->  s.server_agent <- value
        
        | "x-try-ultrapeers" ->
            List.iter (fun s ->
                try
                  let len = String.length s in
(*            lprintf "NEW ULTRAPEER %s\n" s; *)
                  let pos = String.index s ':' in
                  let ip = String.sub s 0 pos in
                  let port = String.sub s (pos+1) (len - pos - 1) in
                  let ip = Ip.of_string ip in
                  let port = int_of_string port in
(*            lprintf "ADDING UP %s:%d\n" (Ip.to_string ip) port;
*)
                  Fifo.put ultrapeers_queue (ip,port ) ;
                  while Fifo.length 
                      ultrapeers_queue > !!max_known_ultrapeers do
                    ignore (Fifo.take ultrapeers_queue)
                  done
                with _ -> ()
            ) (String2.split value ',')
        
        | "x-try" ->
            List.iter (fun s ->
                try
                  let len = String.length s in
(*            lprintf "NEW PEER %s\n" s; *)
                  let pos = String.index s ':' in
                  let ip = String.sub s 0 pos in
                  let port = String.sub s (pos+1) (len - pos - 1) in
                  let ip = Ip.of_string ip in
                  let port = int_of_string port in
(*            lprintf "ADDING PEER %s:%d\n" (Ip.to_string ip) port;
            *)
                  Fifo.put peers_queue (ip,port);
                  while Fifo.length peers_queue > !!max_known_peers do
                    ignore (Fifo.take peers_queue)
                  done
                
                with _ -> ()
            ) (String2.split value ',')    

        | "content-type" ->
            List.iter (fun s ->
                match s with 
                  "application/x-gnutella2" -> gnutella2 := true
                | _ -> ()
            ) (String2.split value ',')

        | "x-ultrapeer" ->
            if value = "True" then ultra_peer := true
        
        | "content-encoding" -> 
            if value = "deflate" then deflate := true
              
        | _ -> ()
    
    ) headers;
    
    if not !ultra_peer then failwith "Not an Ultrapeer";
    if code <> "200" then
      failwith  (Printf.sprintf "Bad return code [%s]" code);
    if proto < "0.6" then
      failwith (Printf.sprintf "Bad protocol [%s]" proto);
    if not (!gnutella2 || !!enable_gnutella1) then
      failwith "Protocol Gnutella2 not supported";
    
    set_rtimeout sock DG.half_day;

    set_server_state s (Connected (-1));
    server_must_update (as_server s.server_server);

    let msg = 
      let buf = Buffer.create 100 in
      Buffer.add_string buf "GNUTELLA/0.6 200 OK\r\n";
      Printf.bprintf  buf "Content-Type: %s\r\n"
        (if !gnutella2 then
          "application/x-gnutella2" else
          "application/x-gnutella-packets");
      Buffer.add_string buf "X-Ultrapeer: False\r\n";
(* Contribute: sending gnutella 1 or 2 ultrapeers to other peers
      Buffer.add_string "X-Try-Ultrapeers: ...\r\n"; *)
      if !deflate then
        Printf.bprintf buf "Content-Encoding: deflate\r\n";
      Buffer.add_string buf "\r\n";
      Buffer.contents buf
    in
    write_string sock msg;
    
    if !gnutella2 then begin
        s.server_gnutella2 <- true;
        g2_connected_servers := s :: !g2_connected_servers;
        g2_recover_files_from_server sock;
        gconn.gconn_handler <- Reader Gnutella2.g2_handler
      end else begin
        s.server_gnutella2 <- false;
        connected_servers := s :: !connected_servers;
        recover_files_from_server s;
        gconn.gconn_handler <- Reader
          (gnutella_handler parse (server_to_client s))
        
      end

  with
  | e -> 
(*
      lprintf "DISCONNECT WITH EXCEPTION %s\n" (Printexc2.to_string e);
  *)
      disconnect_from_server s
      
let send_pings () =
  let pl =
    let module P = Ping in
    PingReq P.SimplePing
  in
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock -> 
          let p  = { (new_packet pl) with pkt_ttl = 1; } in
          s.server_nfiles <- s.server_nfiles_last;
          s.server_nkb <- s.server_nkb_last;
          s.server_ping_last <- p.pkt_uid;
          s.server_nfiles_last <- 0;
          s.server_nkb_last <- 0;
          server_send sock p
  ) !connected_servers
      
let connect_server (ip,port) =
  if !verbose_msg_servers then begin
      lprintf "SHOULD CONNECT TO %s:%d\n" (Ip.to_string ip) port;
    end;
  let s = new_server ip port in
  match s.server_sock with
    Some _ -> ()
  | None -> 
      try
        let sock = connect "limewire to server"
            (Ip.to_inet_addr ip) port
            (fun sock event -> 
              match event with
                BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
(*                  lprintf "RTIMEOUT\n"; *)
                  disconnect_from_server s
              | _ -> ()
          ) in
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;
        
        set_server_state s Connecting;
        s.server_sock <- Some sock;
        incr nservers;
        set_gnutella_sock sock !verbose_msg_servers
          (HttpHeader (server_parse_header s)
        
        );
        set_closer sock (fun _ error -> 
(*            lprintf "CLOSER %s\n" error; *)
            disconnect_from_server s);
        set_rtimeout sock !!server_connection_timeout;
        
        let s = 
          let buf = Buffer.create 100 in
(* Start by Gnutella2 headers *)
          Buffer.add_string buf "GNUTELLA CONNECT/0.6\r\n";
          Printf.bprintf buf "Listen-IP: %s:%d\r\n"
            (Ip.to_string (client_ip (Some sock))) !!client_port;
          Printf.bprintf buf "Remote-IP: %s\r\n"
            (Ip.to_string s.server_ip);
          Printf.bprintf buf "User-Agent: %s\r\n" user_agent;
          Printf.bprintf buf "Accept: %sapplication/x-gnutella2\r\n"
            (if !!enable_gnutella1 then "application/x-gnutella-packets,"
            else "");
          Printf.bprintf buf "X-Ultrapeer: False\r\n";
(* Contribute:  Packet compression is not yet supported...
          Printf.bprintf buf "Accept-Encoding: deflate\r\n"; *)
(* Other Gnutella headers *)          
          Printf.bprintf buf "X-My-Address: %s:%d\r\n"
            (Ip.to_string (client_ip (Some sock))) !!client_port;
          Printf.bprintf buf "X-Query-Routing: 0.1\r\n";
          Printf.bprintf buf "GGEP: 0.5\r\n";
(* Finish with headers *)
          Buffer.add_string buf "\r\n";
          Buffer.contents buf
        in
(*
        lprintf "SENDING\n";
        AP.dump s;
  *)
        write_string sock s;
      with _ ->
          disconnect_from_server s
          
  
let try_connect_ultrapeer () =
(*  lprintf "try_connect_ultrapeer\n";*)
  let s = try
      Fifo.take ultrapeers_queue
    with _ ->
        try 
          Fifo.take peers_queue 
        with _ ->
            if not !redirector_connected then              
              connect_to_redirector ();
            raise Not_found
  in
  connect_server s;
  ()

let connect_servers () =
  (*
  lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers; 
  *)
  if !nservers < !!max_ultrapeers then begin
      for i = !nservers to !!max_ultrapeers - 1 do
        try_connect_ultrapeer ()
      done
    end

let get_file_from_source c file =
  if connection_can_try c.client_connection_control then begin
      connection_try c.client_connection_control;
      match c.client_user.user_kind with
        Indirect_location ("", uid) ->
          lprintf "++++++ ASKING FOR PUSH +++++++++\n";   

(* do as if connection failed. If it connects, connection will be set to OK *)
          connection_failed c.client_connection_control;
          let module P = Push in
          let t = PushReq {
              P.guid = uid;
              P.ip = DO.client_ip None;
              P.port = !!client_port;
              P.index = (match List.assq file c.client_downloads with
                  FileByIndex index -> index
                | _ -> assert false)
            } in
          let p = new_packet t in
          List.iter (fun s ->
              match s.server_sock with
                None -> ()
              | Some sock -> server_send sock p
          ) !connected_servers
      | _ ->
          LimewireClients.connect_client c
    end
    
let download_file (r : result) =
  let file = new_file (Md4.random ()) 
    r.result_name r.result_size r.result_uids in
  lprintf "DOWNLOAD FILE %s\n" file.file_name; 
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun (user, index) ->
      let c = new_client user.user_uid user.user_kind in
      add_download file c index;
      get_file_from_source c file;
  ) r.result_sources;
  ()

let ask_for_files () =
  List.iter (fun file ->
      List.iter (fun c ->
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  ()
    
let disconnect_server s =
      match s.server_sock with
        None -> ()
      | Some sock -> close sock "user disconnect"
    
let _ =
(*  server_ops.op_server_connect <- connect_server; *)
  server_ops.op_server_disconnect <- disconnect_server;
(*
(*  server_ops.op_server_query_users <- (fun s -> *)
      match s.server_sock with
        None -> ()
      | Some sock ->
          server_send sock (GetNickListReq)
  );
(*  server_ops.op_server_users <- (fun s -> *)
      List2.tail_map (fun u -> as_user u.user_user) s.server_users
);
  *)
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s;
      server_remove s
  )
  
  
