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
open OpenFTTypes
open OpenFTGlobals
open OpenFTOptions
open OpenFTProtocol
open OpenFTComplexOptions
  
module DG = CommonGlobals
module DO = CommonOptions

let disconnect_from_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
(*
  lprintf "DISCONNECT FROM SERVER %s:%d" 
        (Ip.to_string s.server_ip) s.server_port;
lprint_newline ();
  *)
      close sock "disconnect";
      s.server_sock <- None;
      set_server_state s NotConnected;
      decr nservers;
      connection_failed s.server_connection_control;
      connected_servers := List2.removeq s !connected_servers;
      if s.server_type = User_node then
        Hashtbl.remove servers_by_key (s.server_ip, s.server_port)

let ask_for_files _ = 
  lprintf "OpenFTServers.ask_for_files not implemented"; lprint_newline ()
  
let send_pings  _ = 
  lprintf "OpenFTServers.send_pings not implemented"; lprint_newline ()
  
let recover_files _ =   
  lprintf "recover_files"; lprint_newline ();
  let module Q = Search in
  Hashtbl.iter (fun _ file ->  
      if file_state file = FileDownloading then begin
          incr nsearches;
          let t = SearchReq {
              Q.id = !nsearches;
              Q.search_type = Q.Search_md5;
              Q.words = (String.lowercase (Md4.to_string file.file_md5)) ;
              Q.exclude = "";
              Q.realm = "";
              Q.size_min = Int64.zero;
              Q.size_max = Int64.zero;
              Q.kbps_min = Int64.zero;
              Q.kbps_max = Int64.zero;
            } in
          List.iter (fun s ->
              match s.server_sock with
                None -> ()
              | Some sock -> server_send sock t
          ) !connected_servers
        end
  ) files_by_md5

let recover_files_from_server sock =         
  lprintf "recover_files_from_server"; lprint_newline ();
  let module Q = Search in
  Hashtbl.iter (fun _ file ->  
      if file_state file = FileDownloading then begin
          incr nsearches;
          let t = SearchReq {
              Q.id = !nsearches;
              Q.search_type = Q.Search_md5;
              Q.words = (String.lowercase (Md4.to_string file.file_md5)) ;
              Q.exclude = "";
              Q.realm = "";
              Q.size_min = Int64.zero;
              Q.size_max = Int64.zero;
              Q.kbps_min = Int64.zero;
              Q.kbps_max = Int64.zero;
            } in
          server_send sock t
        end      
  ) files_by_md5
  


let send_query keywords = 
  let module Q = Search in
  let words = String2.unsplit keywords ' ' in
  incr nsearches;
  let t = SearchReq {
      Q.id = !nsearches;
      Q.search_type = Q.Search_filename;
      Q.words = words;
      Q.exclude = "";
      Q.realm = "";
      Q.size_min = Int64.zero;
      Q.size_max = Int64.zero;
      Q.kbps_min = Int64.zero;
      Q.kbps_max = Int64.zero;
    } in
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock -> server_send sock t
  ) !connected_servers;
  !nsearches
  
(*
let update_user t =
  let module Q = QueryReply in
  let user = new_user t.Q.guid (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  user.user_speed <- t.Q.speed;
  user
    *)

(* NOT IMPLEMENTED YET
  
let send_query min_speed keywords xml_query =
  let module Q = Query in
  let words = String2.unsplit keywords ' ' in
  let t = QueryReq {
      Q.min_speed = 0;
      Q.keywords = words;
      Q.xml_query  = "" } in
  let p = new_packet t in
  if !!verbose_servers > 0 then begin
      lprintf "sending query for <%s>" words; lprint_newline ();
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
      lprintf "Not enough keywords to recover %s" file_name;
      lprint_newline ();
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
  if !!verbose_servers > 0 then begin
      lprintf "trying to recover files from server"; lprint_newline ();
    end;
  List.iter (fun file ->
      if !!verbose_servers > 0 then begin
          lprintf "FOR FILE %s" file.file_name; lprint_newline ();
        end;
      let keywords = get_name_keywords file.file_name in
      let words = String2.unsplit keywords ' ' in
      if !!verbose_servers > 0 then begin
          lprintf "sending query for <%s>" words; lprint_newline ();
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
      if !!verbose_servers>10 then begin
          lprintf "BAD HEADER FROM REDIRECTOR: "; lprint_newline (); 
          LittleEndian.dump header;
        end;
      close sock "bad header";
      redirector_connected := false;
      raise Not_found
    end
  
let connect_to_redirector () =
  match !redirectors_to_try with
    [] ->
      redirectors_to_try := !redirectors_ips
  | ip :: tail ->
      redirectors_to_try := tail;
(*      lprintf "connect to redirector"; lprint_newline (); *)
      try
        let sock = connect  "openft to redirector"
            (Ip.to_inet_addr ip) 6346
            (fun sock event -> 
              match event with
                BASIC_EVENT RTIMEOUT -> 
                  close sock "timeout";
                  redirector_connected := false;
(*                  lprintf "TIMEOUT FROM REDIRECTOR"; lprint_newline ()*)
              | _ -> ()
          ) in
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;

        
        redirector_connected := true;
        set_reader sock (handler !!verbose_servers redirector_parse_header
            (gnutella_handler parse redirector_to_client)
        );
        set_closer sock (fun _ _ -> 
(*            lprintf "redirector disconnected"; lprint_newline (); *)
            redirector_connected := false);
        set_rtimeout sock 10.;
        set_lifetime (TcpBufferedSocket.sock sock) 120.;
        write_string sock "GNUTELLA CONNECT/0.4\n\n";
      with e ->
          lprintf "Exception in connect_to_redirector: %s"
            (Printexc2.to_string e); lprint_newline ();
          redirector_connected := false
          
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
        lprintf "add_ulta_peers : %s" (Printexc2.to_string e);
        lprint_newline () );
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
        Some true ->  Indirect_location ("", t.Q.guid, _, _)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  user.user_speed <- t.Q.speed;
  user

let update_client t =
  let module Q = QueryReply in
  let c = new_client t.Q.guid (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid, _, _)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  
  c.client_user.user_speed <- t.Q.speed;
  c
  
let server_parse_header s sock header =
  if !!verbose_servers> 10 then  LittleEndian.dump_ascii header;  
  try
  if String2.starts_with header gnutella_200_ok then begin
(*      lprintf "GOOD HEADER FROM ULTRAPEER";
      lprint_newline (); *)
        set_rtimeout sock Date.half_day_in_secs;
(*        lprintf "SPLIT HEADER..."; lprint_newline ();*)
      let lines = Http_client.split_header header in
      match lines with
          [] -> raise Not_found        
        | _ :: headers ->
(*            lprintf "CUT HEADER"; lprint_newline ();*)
            let headers = Http_client.cut_headers headers in
            let agent =  List.assoc "user-agent" headers in
(*            lprintf "USER AGENT: %s" agent; lprint_newline ();*)
            if String2.starts_with agent "LimeWire" ||
              String2.starts_with agent "Gnucleus" ||
              String2.starts_with agent "BearShare"              
              then
              begin
                s.server_agent <- agent;
(*                lprintf "LIMEWIRE Detected"; lprint_newline ();*)
                add_peers headers;
                if List.assoc "x-ultrapeer" headers <> "True" then begin
(*                    lprintf "NOT AN ULTRAPEER ???"; lprint_newline (); *)
                    raise Not_found;
                  end;

(*                lprintf "******** ULTRA PEER %s:%d  *******"
                  (Ip.to_string s.server_ip) s.server_port;
                lprint_newline (); *)
                write_string sock "GNUTELLA/0.6 200 OK\r\n\r\n";
                set_server_state s Connected_idle;
                connected_servers := s :: !connected_servers;
                recover_files_from_server sock
              end
            else raise Not_found
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
(*                lprintf "LIMEWIRE Detected"; lprint_newline ();*)
                add_peers headers;                
                raise Not_found
              end
            else raise Not_found
    end else begin
(*      lprintf "BAD HEADER FROM SERVER: [%s]" header; lprint_newline (); *)
      raise Not_found
    end
  with
  | Not_found -> 
(*      lprintf "DISCONNECTION"; lprint_newline (); *)
      disconnect_from_server s
  | e -> 
(*
      lprintf "DISCONNECT WITH EXCEPTION %s" (Printexc2.to_string e);
lprint_newline ();
  *)
      disconnect_from_server s

        *)

let get_file_from_source c file =
  if connection_can_try c.client_connection_control then begin
      connection_try c.client_connection_control;
      let u = c.client_user in
      let s = u.user_server in
      lprintf "******* DOWNLOAD FROM %s %d %d *******"
        (Ip.to_string s.server_ip) s.server_port s.server_http_port;
      lprint_newline ();
      if s.server_http_port <> 0 then
      (*
      match c.client_user.user_kind with
        Indirect_location ("", uid) ->
          lprintf "++++++ ASKING FOR PUSH +++++++++"; lprint_newline ();   

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
  *)
          OpenFTClients.connect_client c
    end
    
let download_file (r : result) =
  let file = new_file r.result_md5 r.result_name r.result_size in
  lprintf "DOWNLOAD FILE %s" file.file_name; lprint_newline ();
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun (user, index) ->
      let s = user.user_server in
      let c = new_client s.server_ip s.server_port s.server_http_port in
      add_download file c index;
      get_file_from_source c file;
  ) r.result_sources;
  ()

(* these two functions are also in dcGlobals.ml *)          
let exit_exn = Exit
let basename filename =
  let s =
    let len = String.length filename in
    try
      let pos = String.rindex_from filename (len-1) '\\' in
      String.sub filename (pos+1) (len-pos-1)
    with _ ->      
        try
          if len > 2 then
            let c1 = Char.lowercase filename.[0] in
            let c2 = filename.[1] in
            match c1,c2 with
              'a'..'z', ':' ->
                String.sub filename 2 (len -2 )
            | _ -> raise exit_exn
          else raise exit_exn
        with _ -> Filename.basename filename
  in
  String.lowercase s

  
let server_to_client s t sock =
 
  if !!verbose_servers> 200 then begin
      lprintf "From server:"; lprint_newline ();
      print t; 
    end;
  match t with
    VersionReq ->
      set_rtimeout sock 60.; 
      server_send sock (
        let module V = VersionReply in
        VersionReplyReq {
          V.major_num = 0;
          V.minor_num = 0;
          V.micro_num = 5;
        })
  
  | VersionReplyReq t ->
      let module V = VersionReply in
      s.server_version <- Printf.sprintf "%d.%d.%d" 
        t.V.major_num t.V.minor_num t.V.micro_num;
      set_server_state s Connected_idle;
      connection_ok s.server_connection_control;

  
  | NodeInfoReq -> 
      server_send sock (let module N = NodeInfoReply in
        NodeInfoReplyReq { N.ip = client_ip (Some sock); 
          N.port = !!port; 
          N.http_port = !!http_port;
        })
  
  | NodeInfoReplyReq t ->
      let module N = NodeInfoReply in
      s.server_http_port <- t.N.http_port;
      assert (s.server_port = t.N.port) 
(* we should already have this information, no ? *)
  
  | ClassReq -> 
      server_send sock (ClassReplyReq User_node)
  
  | ClassReplyReq t ->
      s.server_type <- t;
      begin
        match s.server_type with
          Search_node -> 
            server_send sock (ChildReq None);
            server_send sock (StatsReq Stats.Retrieve_info);
        | _ ->
(* don't stay connected more than one minute to a user node *)
            set_lifetime sock 60.
      end
  
  | NodeListReq -> 
      List.iter (fun s ->
          server_send sock (let module N = NodeListReply in
            (NodeListReplyReq (Some  {
                  N.ip = s.server_ip;
                  N.port = s.server_port;
                  N.node_type = s.server_type;
                })))
      ) !connected_servers;
      server_send sock (NodeListReplyReq None)
  
  | NodeListReplyReq None -> ()
  | NodeListReplyReq (Some t) -> 
      begin
        let module N = NodeListReply in
        if t.N.port <> 0 then
          let s = new_server t.N.ip t.N.port in
          match s.server_type with
            User_node ->  Fifo.put peers_queue  (t.N.ip, t.N.port)
          | _ ->  s.server_type <- t.N.node_type
      end
  
  | NodeCapReq -> 
      server_send sock (NodeCapReplyReq ["MD5-FULL"]) (* not "ZLIB" yet *)
  
  | NodeCapReplyReq t ->
      s.server_caps <- t
  
  | ChildReplyReq t ->
      if t then begin
          
          lprintf "************ CONNECTED AND CHILD *************";
          lprint_newline ();
          set_rtimeout sock 3600.; 
          server_send sock (ChildReq (Some true));
          connected_servers := s :: !connected_servers;
          recover_files_from_server sock
          
        end else
        disconnect_from_server s

  | PingReq -> server_send sock PingReplyReq

            
  | SearchReplyReq t ->
      lprintf "REPLY TO QUERY"; lprint_newline ();
      let module Q = SearchReply in
      begin
        try
          let ss = Hashtbl.find searches_by_uid t.Q.id in

          let ip = if t.Q.ip = Ip.null then s.server_ip else t.Q.ip in
          let user = new_user ip t.Q.port t.Q.http_port in

          lprintf "NEW RESULT %s" t.Q.filename; lprint_newline ();
          let result = new_result t.Q.md5 (basename t.Q.filename) t.Q.size in
          lprintf "ADDING SOURCE FOR RESULT NOT IMPLEMENTED"; 
          lprint_newline ();
          add_source result user t.Q.filename; 
              
          CommonInteractive.search_add_result ss.search_search result.result_result;
        with Not_found ->            
            lprintf "NO SUCH SEARCH !!!!"; lprint_newline (); 
            
            try
              let file = Hashtbl.find files_by_md5 t.Q.md5 in
              let ip = if t.Q.ip = Ip.null then s.server_ip else t.Q.ip in
              let user = new_user ip t.Q.port t.Q.http_port in
              
              let result = new_result t.Q.md5 (basename t.Q.filename) t.Q.size in
              lprintf "ADDING SOURCE FOR RESULT NOT IMPLEMENTED"; 
              lprint_newline ();
              add_source result user t.Q.filename; 

              let s = user.user_server in
              let c = new_client s.server_ip s.server_port s.server_http_port in
              add_download file c t.Q.filename;
              get_file_from_source c file;
              ()
            with _ ->
                lprintf "NO SUCH SEARCH for no file !!!!"; 
                lprint_newline (); 
      end

      
  | _ ->
      lprintf "UNUSED MESSAGE";
      print t
      
(*
(*
  lprintf "server_to_client"; lprint_newline ();
print p;
  *)
  match p.pkt_payload with
  | PingReq t ->
      if p.pkt_hops <= 3 then
        server_send sock {
          p with 
          pkt_hops = p.pkt_hops + 1;
          pkt_type = PONG;
          pkt_payload = (
            let module P = Pong in
            PongReq {
              P.ip = (DO.client_ip (Some sock));
              P.port = !!client_port;
              P.nfiles = 10;
              P.nkb = 10;
            });
        }
  | PongReq t ->
      
      let module P = Pong in
(*      lprintf "FROM %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      if p.pkt_uid = s.server_ping_last then begin
          s.server_nfiles_last <- s.server_nfiles_last + t.P.nfiles;
          s.server_nkb_last <- s.server_nkb_last + t.P.nkb
        end
  
  | QueryReq _ ->
(*      lprintf "REPLY TO QUERY NOT IMPLEMENTED YET :("; lprint_newline ();*)
      ()
      
  | QueryReplyReq t ->
(*      lprintf "REPLY TO QUERY"; lprint_newline ();*)
      let module Q = QueryReply in
      begin
        try
          let s = Hashtbl.find searches_by_uid p.pkt_uid in
          
          let user = update_user t in

(*          lprintf "ADDING RESULTS"; lprint_newline ();*)
          List.iter (fun f ->
(*              lprintf "NEW RESULT %s" f.Q.name; lprint_newline ();*)
              let result = new_result f.Q.name f.Q.size in
              add_source result user f.Q.index;
              
              search_add_result s.search_search result.result_result;
          ) t.Q.files
        with Not_found ->            
            lprintf "NO SUCH SEARCH !!!!"; lprint_newline (); 
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
    *)

let connect_server (ip,port) =
  if !!verbose_servers > 5 then begin
      lprintf "SHOULD CONNECT TO %s:%d" (Ip.to_string ip) port;
      lprint_newline ();
    end;
  let s = new_server ip port in
  match s.server_sock with
    Some _ -> ()
  | None -> 
      try
        let sock = connect "openft to server"
          (Ip.to_inet_addr ip) port
            (fun sock event -> 
              match event with
                BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
(*                  lprintf "RTIMEOUT"; lprint_newline (); *)
                  disconnect_from_server s
              | _ -> ()
          ) in
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;

        connection_try s.server_connection_control;
        set_server_state s Connecting;
        s.server_sock <- Some sock;
        incr nservers;
        set_reader sock (cut_messages OpenFTProtocol.parse 
            (server_to_client s)
        );
        set_closer sock (fun _ error -> 
(*            lprintf "CLOSER %s" error; lprint_newline ();*)
            disconnect_from_server s);
        set_rtimeout sock !!server_connection_timeout;
        server_send sock VersionReq;
        server_send sock ClassReq;
        server_send sock NodeInfoReq;
        server_send sock NodeListReq;
        server_send sock NodeCapReq
      with _ ->
          disconnect_from_server s
          
  
let try_connect_ultrapeer () =
 (*   lprintf "try_connect_ultrapeer"; lprint_newline (); *)
  let s = try
      Fifo.take ultrapeers_queue
    with _ ->
        try 
          Fifo.take peers_queue 
        with _ ->
            Hashtbl.iter (fun key s ->
                match s.server_type with
                  User_node ->  ()
                | _ ->  Fifo.put ultrapeers_queue key
            ) servers_by_key;
            raise Not_found
  in
  connect_server s;
  ()

let connect_servers () =
  if !nservers < !!max_ultrapeers then begin
      for i = !nservers to !!max_ultrapeers - 1 do
        try_connect_ultrapeer ()
      done
    end

  (*
  
let ask_for_files () =
  List.iter (fun file ->
      List.iter (fun c ->

          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  ()
  
  
  *)

    
let _ =
  server_ops.op_server_connect <- (fun s ->
      connect_server (s.server_ip, s.server_port)); 
  server_ops.op_server_disconnect <- disconnect_from_server;
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
      disconnect_from_server s;
      Hashtbl.remove servers_by_key (s.server_ip, s.server_port);
      server_remove (as_server s.server_server);
  );
    server_ops.op_server_sort <- (fun s ->
      connection_last_conn s.server_connection_control
  )
  
