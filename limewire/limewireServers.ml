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

let update_shared_words () = 
  let module M = CommonUploads in
  let words = ref WordSet.empty in
  let register_words s = 
    let ws = String2.split_simplify s ' ' in
    List.iter (fun w ->
        words := WordSet.add s !words
    ) ws
  in
  let rec iter node =
    List.iter (fun sh ->
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
  ) !words
  
  
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
  
let send_query min_speed keywords xml_query =
  let module Q = Query in
  let words = String2.unsplit keywords ' ' in
  let t = QueryReq {
      Q.min_speed = 0;
      Q.keywords = words;
      Q.xml_query  = "" } in
  let p = new_packet t in
  if !verbose_msg_servers then begin
      lprintf "sending query for <%s>\n" words;
    end;
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock -> server_send sock p
  ) !connected_servers;
  p

        
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
        let c = s.[i] in
        match c with
          'a'..'z' | '0' .. '9' -> ()
        | _ -> s.[i] <- ' ';
      done;
      remove_short (String2.split s ' ') []
    
let get_name_keywords file_name =
  match stem file_name with 
    [] | [_] -> 
      lprintf "Not enough keywords to recover %s\n" file_name;
      [file_name]
  | l -> l
      
let recover_files () =
  List.iter (fun file ->
      let keywords = get_name_keywords file.file_name 
      in
      ignore (send_query 0 keywords "")
  ) !current_files;
  ()
  
let recover_files_from_server sock =
  if !verbose_msg_servers then begin
      lprintf "trying to recover files from server\n";
    end;
  List.iter (fun file ->
      if !verbose_msg_servers then begin
          lprintf "FOR FILE %s\n" file.file_name; 
        end;
      let keywords = get_name_keywords file.file_name in
      let words = String2.unsplit keywords ' ' in
      if !verbose_msg_servers then begin
          lprintf "sending query for <%s>\n" words; 
          end;
      let module Q = Query in
      let t = QueryReq {
          Q.min_speed = 0;
          Q.keywords = words;
          Q.xml_query  = "" } in
      let p = new_packet t in
      server_send sock p
  ) !current_files;
  ()
  
  
let redirector_to_client p sock = 
(*  lprintf "redirector_to_client"; lprint_newline (); *)
  match p.pkt_payload with
    PongReq t ->
      let module P = Pong in
(*      lprintf "ADDING PEER %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      Fifo.put peers_queue (t.P.ip, t.P.port);
  | _ -> ()
      
let redirector_parse_header sock header = 
(*  lprintf "redirector_parse_header"; lprint_newline ();*)
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
        }))
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
(*      lprintf "connect to redirector"; lprint_newline (); *)
      Ip.async_ip name (fun ip ->
          try
            let sock = connect  "limewire to redirector"
                (Ip.to_inet_addr ip) 6346
                (fun sock event -> 
                  match event with
                    BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
                      close sock "timeout";
                      redirector_connected := false;
(*                  lprintf "TIMEOUT FROM REDIRECTOR"; lprint_newline ()*)
                  | _ -> ()
              ) in
            TcpBufferedSocket.set_read_controler sock download_control;
            TcpBufferedSocket.set_write_controler sock upload_control;
            
            
            redirector_connected := true;
            set_reader sock (handler !verbose_msg_servers redirector_parse_header
                (gnutella_handler parse redirector_to_client)
            );
            set_closer sock (fun _ _ -> 
(*            lprintf "redirector disconnected"; lprint_newline (); *)
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
  lprintf "DISCONNECT FROM SERVER %s:%d" 
        (Ip.to_string s.server_ip) s.server_port;
lprint_newline ();
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

let add_peers headers =
  (try
      let up = List.assoc "x-try-ultrapeers" headers in
      List.iter (fun s ->
          try
            let len = String.length s in
(*            lprintf "NEW ULTRAPEER %s" s; lprint_newline ();*)
            let pos = String.index s ':' in
            let ip = String.sub s 0 pos in
            let port = String.sub s (pos+1) (len - pos - 1) in
            let ip = Ip.of_string ip in
            let port = int_of_string port in
(*            lprintf "ADDING UP %s:%d" (Ip.to_string ip) port;
            lprint_newline ();*)
            Fifo.put ultrapeers_queue (ip,port ) ;
            while Fifo.length ultrapeers_queue > !!max_known_ultrapeers do
              ignore (Fifo.take ultrapeers_queue)
            done
          
          with _ -> ()
      ) (String2.split up ',');    
    with e -> 
        lprintf "add_ulta_peers : %s\n" (Printexc2.to_string e);
        );
  (try
      let up = List.assoc "x-try" headers in
      List.iter (fun s ->
          try
            let len = String.length s in
(*            lprintf "NEW PEER %s" s; lprint_newline (); *)
            let pos = String.index s ':' in
            let ip = String.sub s 0 pos in
            let port = String.sub s (pos+1) (len - pos - 1) in
            let ip = Ip.of_string ip in
            let port = int_of_string port in
(*            lprintf "ADDING PEER %s:%d" (Ip.to_string ip) port;
            lprint_newline ();*)
            Fifo.put peers_queue (ip,port);
            while Fifo.length peers_queue > !!max_known_peers do
              ignore (Fifo.take peers_queue)
            done
          
          with _ -> ()
      ) (String2.split up ',')    
    with _ -> ())

(*
ascii: [ G N U T E L L A / 0 . 6   2 0 0   O K(13)(10) U s e r - A g e n t :   G n u c l e u s   1 . 8 . 2 . 0(13)(10) R e m o t e - I P :   2 1 2 . 1 9 8 . 2 3 5 . 1 2 3(13)(10) X - Q u e r y - R o u t i n g :   0 . 1(13)(10) X - U l t r a p e e r :   T r u e(13)(10) X - L e a f - M a x :   4 0 0(13)(10) U p t i m e :   0 D   0 3 H   3 0 M(13)(10)(13)]
*)



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
  
let server_parse_header s sock header =
  if !verbose_msg_servers then  LittleEndian.dump_ascii header;  
  try
    if String2.starts_with header gnutella_200_ok then begin
(*      lprintf "GOOD HEADER FROM ULTRAPEER";
      lprint_newline (); *)
        set_rtimeout sock DG.half_day;
(*        lprintf "SPLIT HEADER..."; lprint_newline ();*)
        let lines = Http_client.split_header header in
        match lines with
          [] -> raise Not_found        
        | _ :: headers ->
(*            lprintf "CUT HEADER"; lprint_newline ();*)
            let headers = Http_client.cut_headers headers in
            let agent =  find_header "user-agent" headers "Unknown" in
            lprintf "USER AGENT: %s\n" agent;
(*
if String2.starts_with agent "LimeWire" ||
String2.starts_with agent "Gnucleus" ||
String2.starts_with agent "BearShare"              
then
begin
*)
            
            s.server_agent <- agent;
            server_must_update (as_server s.server_server);
(*                lprintf "LIMEWIRE Detected"; lprint_newline ();*)
            add_peers headers;
            if (find_header "x-ultrapeer" headers "False") <> "True" then
              raise Not_found;
            connected_servers := s :: !connected_servers;

(*                lprintf "******** ULTRA PEER %s:%d  *******"
                  (Ip.to_string s.server_ip) s.server_port;
                lprint_newline (); *)
            write_string sock "GNUTELLA/0.6 200 OK\r\n\r\n";
            set_server_state s (Connected (-1));
            recover_files_from_server sock
      end else 
    if String2.starts_with header gnutella_503_shielded then begin
(*      lprintf "GOOD HEADER FROM SIMPLE PEER";
      lprint_newline ();*)
        let lines = Http_client.split_header header in
        match lines with
          [] -> raise Not_found        
        | _ :: headers ->
            let headers = Http_client.cut_headers headers in
            let agent = List.assoc "user-agent" headers in
            if String2.starts_with agent "LimeWire" ||
              String2.starts_with agent "Gnucleus" ||
              String2.starts_with agent "BearShare"              
            then
              begin
(*                lprintf "LIMEWIRE Detected\n";*)
                add_peers headers;                
                raise Not_found
              end
            else raise Not_found
      end else begin
(*      lprintf "BAD HEADER FROM SERVER: [%s]\n" header;  *)
        raise Not_found
      end
  with
  | Not_found -> 
(*      lprintf "DISCONNECTION\n";  *)
      disconnect_from_server s
  | e -> 
(*
      lprintf "DISCONNECT WITH EXCEPTION %s\n" (Printexc2.to_string e);
  *)
      disconnect_from_server s
      
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
                  M.info = "";
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
      
  | QueryReplyReq t ->
(*      lprintf "REPLY TO QUERY\n";*)
      let module Q = QueryReply in
      begin
        try
          let s = Hashtbl.find searches_by_uid p.pkt_uid in
          
          let user = update_user t in

(*          lprintf "ADDING RESULTS\n";*)
          List.iter (fun f ->
(*              lprintf "NEW RESULT %s" f.Q.name; lprint_newline ();*)
              let result = new_result f.Q.name f.Q.size in
              add_source result user f.Q.index;
              
              CommonInteractive.search_add_result s.search_search result.result_result;
          ) t.Q.files
        with Not_found ->            
            lprintf "NO SUCH SEARCH !!!!\n"; 
            List.iter (fun ff ->
                List.iter (fun file ->
                    if file.file_name = ff.Q.name && 
                      file_size file = ff.Q.size then 
                      begin
                        lprintf "++++++++++++++ RECOVER FILE %s +++++++++++++" file.file_name; lprint_newline (); 
                        let c = update_client t in
                        add_download file c ff.Q.index;
                        end
                ) !current_files;
            ) t.Q.files
      end
  | _ -> ()

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
      lprintf "SHOULD CONNECT TO %s:%d" (Ip.to_string ip) port;
      lprint_newline ();
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
        set_reader sock (handler !verbose_msg_servers (server_parse_header s)
          (gnutella_handler parse (server_to_client s))
        );
        set_closer sock (fun _ error -> 
(*            lprintf "CLOSER %s" error; lprint_newline ();*)
            disconnect_from_server s);
        set_rtimeout sock !!server_connection_timeout;
        let s = add_header_fields 
            "GNUTELLA CONNECT/0.6\r\n" sock 
            (Printf.sprintf "Remote-IP: %s\r\n\r\n"
              (Ip.to_string s.server_ip))
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
  lprintf "connect_servers %d %d" !nservers !!max_ultrapeers; 
lprint_newline ();
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
              P.index = List.assq file c.client_downloads;
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
  let file = new_file (Md4.random ()) r.result_name r.result_size in
  lprintf "DOWNLOAD FILE %s" file.file_name; lprint_newline ();
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun (user, index) ->
      lprintf "Source %s %d" (Md4.to_string user.user_uid)
      index; lprint_newline ();
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
  
  
