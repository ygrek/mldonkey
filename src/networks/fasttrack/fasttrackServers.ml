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

open Int64ops
open Options
open Queues
open Printf2
open BasicSocket
open TcpBufferedSocket

open CommonHosts
open CommonOptions
open CommonServer
open CommonTypes
open CommonGlobals

open FasttrackNetwork
open FasttrackTypes
open FasttrackGlobals
open FasttrackOptions
open FasttrackProtocol
open FasttrackProto

let load_nodes_file filename = 
  let regexp = Str.regexp "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\) \\([0-9]+\\) .*$" in
  Unix2.tryopen_read filename (fun cin ->
    try
    
      while true do
        let line = input_line cin in
        try 
          if Str.string_match regexp line 0 then 
            let ip = Ip.addr_of_string (Str.matched_group 1 line) in
            let port = int_of_string (Str.matched_group 2 line) in
            try
              ignore (H.new_host ip port Ultrapeer)
            with Not_found -> ()
        with _ ->
          lprintf_nl "Syntax error in %s" filename;
      done
    
    with End_of_file -> ()
  )

let unpack_nodes_gzip filename url =
  let ext = String.lowercase (Filename2.extension filename) in
  let last_ext = String.lowercase (Filename2.last_extension filename) in
  let real_ext = if last_ext = ".zip" then last_ext else ext in
    match real_ext with
    | ".gzip" -> (
         try     
            Misc.archive_extract filename "gz"
         with e ->
          lprintf_nl "Exception %s while extracting from %s" (Printexc2.to_string e) url;
          raise Not_found
      )
    | _ -> filename

let _ =
    CommonWeb.add_web_kind "nodes.gzip" "List of fasttrack nodes"
    (fun url filename -> 
        lprintf_nl "nodes.gzip loaded from %s" url;
        let f = unpack_nodes_gzip filename url in
        load_nodes_file f;
        if f <> filename then Sys.remove f
    )    

let server_parse_after s gconn sock =
  try
    match s.server_ciphers with
      None -> assert false
    | Some ciphers ->
        let b = buf sock in
(*        if !verbose_msg_raw then
          lprintf "server_parse_after: %d bytes\n" b.len;  *)
       let rec iter () =
          let len = b.len in
          if len > 0 then
            let size = TcpMessages.packet_size ciphers b.buf b.pos b.len in
            match size with
              None -> ()
            | Some size ->
                if len >= size then
                  let msg = Bytes.sub b.buf b.pos size in
                  buf_used b size;
                  let addr, t = TcpMessages.parse ciphers (Bytes.to_string msg) in
                  FasttrackHandler.server_msg_handler sock s addr t;
                  iter ()
        in
        iter ()
  with e ->
      lprintf "Exception %s in server_parse_after\n"
        (Printexc2.to_string e);
      close sock (Closed_for_error "Reply not understood")

let server_connection_hook = ref (None : (server -> unit) option)

let greet_supernode s =
  (match !server_connection_hook with
      None ->
        server_send s TcpMessages.DirectPacket (
          TcpMessages.NodeInfoReq (
            client_ip s.server_sock,
            !!client_port,
            default_bandwidth,
            client_name ()))
    | Some f -> f s)
(*  ;  server_send_ping s *)

let server_parse_netname s gconn sock =
  let b = TcpBufferedSocket.buf sock in
  let len = b.len in
  let start_pos = b.pos in
  let end_pos = start_pos + len in
  let buf = b.buf in
  let net = Bytes.sub buf start_pos len in
  if !verbose_msg_raw then
    lprintf "net:[%s]\n" (Bytes.unsafe_to_string (Bytes.escaped net));
  let rec iter pos =
    if pos < end_pos then
      if (Bytes.get buf pos) = '\000' then begin
          let netname = Bytes.sub buf start_pos (pos-start_pos) in
          if !verbose_msg_raw then
            lprintf "netname: [%s]\n" (Bytes.unsafe_to_string (Bytes.escaped netname));
          buf_used b (pos-start_pos+1);
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
  H.connected s.server_host;
  let b = TcpBufferedSocket.buf sock in
  if b.len >= 8 then
    match s.server_ciphers with
      None -> assert false
    | Some ciphers ->
        if !verbose_msg_raw then
          lprintf "Cipher received from server\n";
        get_cipher_from_packet (Bytes.to_string b.buf) b.pos ciphers.in_cipher;
        init_cipher ciphers.in_cipher;

        xor_ciphers ciphers.out_cipher ciphers.in_cipher;
        init_cipher ciphers.out_cipher;

        buf_used b 8;
        server_crypt_and_send s ciphers.out_cipher (network_name ^ "\000");
        gconn.gconn_handler <- CipherReader (ciphers.in_cipher, server_parse_netname s);
        if !verbose_msg_raw then
          lprintf "waiting for netname\n"

let client_cipher_seed () =
 (* Int32.of_int (Random.int max_int) *)
 0x0fACB1238l

let connection_header_hook = ref None

let connect_server h =
  let s = match h.host_server with
      None ->
        let s = new_server h.host_addr h.host_port in
        h.host_server <- Some s;
        s
    | Some s -> s
  in
  match s.server_sock with
  | NoConnection ->
      incr nservers;
      let token =
        add_pending_connection connection_manager (fun token ->
            decr nservers;
            try
              let ip = Ip.ip_of_addr h.host_addr in
              if not (Ip.valid ip) then
                failwith "Invalid IP for server\n";
              let port = s.server_host.host_port in
              if !verbose_msg_servers then
                lprintf "CONNECT TO %s:%d\n"
                    (Ip.string_of_addr h.host_addr) port;
              H.set_request h Tcp_Connect;
              H.try_connect h;
(* Standard Kazaa clients send a ping first, and only connect if they
  receive a Supernode Pong. We send the ping only to get the latency. *)
              udp_send ip port true (
                let module M = UdpMessages in
                M.PingReq (169, "\128", "KaZaA"));

              let ip = Ip.to_inet_addr ip in
              let sock = connect token "fasttrack to server"
                  ip port
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
                in_xinu = 0x51L;
                out_xinu = 0x51L;
              };
              set_cipher out_cipher (client_cipher_seed ()) 0x29;

              let s = String.create 12 in

              (match !connection_header_hook with
                  None ->
                    s.[0] <- '\250';
                    s.[1] <- '\000';
                    s.[2] <- '\182';
                    s.[3] <- '\043';
                | Some f -> f s);

              cipher_packet_set out_cipher s 4;

              if !verbose_msg_raw then begin
                  lprintf "SENDING %s\n" (Bytes.unsafe_to_string (Bytes.escaped s));
                  AnyEndian.dump_bytes s;
                end;
              write sock s 0 (Bytes.length s);
            with _ ->
                disconnect_from_server nservers s Closed_connect_failed
        )
      in
      s.server_sock <- ConnectionWaiting token;
  | _ -> ()

let get_file_from_source c file =
  try
    if connection_can_try c.client_connection_control then begin
        connection_try c.client_connection_control;
        match c.client_user.user_kind with
          Indirect_location ("", uid, _, _) ->
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
            if not (List.memq file c.client_in_queues) then begin
                Queue.put file.file_clients_queue (1,c);
                c.client_in_queues <- file :: c.client_in_queues
              end
      end
  with e ->
      lprintf "get_file_from_source: exception %s\n" (Printexc2.to_string e)

let exit = Exit

let disconnect_server s r =
  match s.server_sock with
  | Connection sock -> close sock r
  | ConnectionWaiting token ->
      cancel_token token;
      s.server_sock <- NoConnection;
      free_ciphers s

  | _ -> ()

let really_recover_file file =
  List.iter (fun s ->
      List.iter (fun ss ->
          if not (Fifo.mem s.server_searches ss) then
            Fifo.put s.server_searches ss
      )  file.file_searches
  ) !connected_servers

let really_download_file (r : CommonTypes.result_info) user group =
  let rec iter uids =
    match uids with
      uid :: tail ->
        (match Uid.to_uid uid with
            Md5Ext hash -> hash,  Uid.to_file_string uid
          | _  -> iter tail)
    | [] -> raise IgnoreNetwork
  in
  let hash,file_temp = iter r.result_uids in

  let file = new_file file_temp (List.hd r.result_names)
    r.result_size [Uid.create (Md5Ext hash)] user group in
  if !verbose then
    lprintf "DOWNLOAD FILE %s\n" file.file_name;
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  begin
    let sources = Hashtbl.find result_sources r.result_num in
    List.iter (fun (user, _) ->
        let c = new_client user.user_kind in
        add_download file c ();
        get_file_from_source c file;
    ) !sources;
  end;
  file

let ask_for_files () = (* called every minute *)
  List.iter (fun file ->
      List.iter (fun c ->
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  let module M = TcpMessages in
  List.iter (fun s ->
      try
        let ss = Fifo.take s.server_searches in
        match ss.search_search with
          FileUidSearch (file,file_hash) ->
            if file_state file = FileDownloading then
              server_send s M.DirectPacket
                (M.SearchReq
                  (32, ss.search_uid, M.QueryLocationReq file_hash))
        | UserSearch (_, words, (realm, tags)) ->

            let realm =
              match realm with
                  "audio" -> 0x21
                | "video" -> 0x22
                | "image" -> 0x23
                | "text" -> 0x24
                | "application" -> 0x25
                | _ -> 0x3f
            in

            server_send s M.DirectPacket
              (M.SearchReq
                (32, ss.search_uid, M.QueryFilesReq
                  (words, realm, tags)))
      with _ -> ()
  ) !connected_servers;
  ()

let _ =
  server_ops.op_server_disconnect <- (fun s ->
    disconnect_server s Closed_by_user);
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s Closed_by_user
  )

let nranges file =
  Int64.to_int ((file_size file) // min_range_size) + 5

let manage_hosts () =
  H.manage_hosts ();
  List.iter (fun file ->
      if file_state file = FileDownloading then
        try
(* For each file, we allow only (nranges+5) simultaneous communications,
  to prevent too many clients from saturing the line for only one file. *)
          let max_nconnected_clients = nranges file in
          while file.file_nconnected_clients < max_nconnected_clients do
            let (_,c) = Queue.take file.file_clients_queue in
            c.client_in_queues <- List2.removeq file c.client_in_queues;
            FasttrackClients.connect_client c
          done
        with _ -> ()
  ) !current_files

let rec find_ultrapeer queue =
  let (next,h) = Queue.head queue in
  try
    if next > last_time () then begin
(*        lprintf "not ready: %d s\n" (next - last_time ());  *)
        raise Not_found;
      end;
    ignore (H.host_queue_take queue);
    h
  with _ -> find_ultrapeer queue

let try_connect_ultrapeer connect =
(*  lprintf "try_connect_ultrapeer....\n"; *)
  let h =
    try
      find_ultrapeer ultrapeers_waiting_queue
    with _ ->
(*        lprintf "not in ultrapeers_waiting_queue\n";   *)
       raise Not_found
  in
(*  lprintf "contacting..\n";  *)
  connect h

let connect_servers connect =
  (* lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers; *)
  (if !!max_ultrapeers > List.length !connected_servers then
      try
        let to_connect = 3 * (!!max_ultrapeers - !nservers) in
        for i = 1 to to_connect do
(*          lprintf "try_connect_ultrapeer...\n";  *)
          try_connect_ultrapeer connect
        done
      with _ -> ())

(* Looks like there is no ping to send in Fasttrack? *)
let send_pings () = ()

let send_query ss =
  let s_uid = ss.search_uid in
  let module M = TcpMessages in
  let t =
    match ss.search_search with
      UserSearch (_, words, (realm, tags)) ->
        let realm =
          match realm with
              "audio" -> 0x21
            | "video" -> 0x22
            | "image" -> 0x23
            | "text" -> 0x24
            | "application" -> 0x25
          | _ -> 0x3f
        in
        M.QueryFilesReq (words, realm, tags)
  | FileUidSearch (_, file_hash) ->
        M.QueryLocationReq file_hash
  in
  List.iter (fun s ->
      FasttrackProto.server_send s M.DirectPacket (
        M.SearchReq (32, s_uid, t))) !connected_servers

(*
TODO:

push request: we send a push to the server when we cannot connect to
a particular client. The client by connecting to us with a
"GIVE <push_id>\r\n" request, to which we can reply by a "GET ...." *)
