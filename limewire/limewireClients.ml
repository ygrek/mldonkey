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
open CommonUploads
open Printf2
open CommonOptions
open CommonDownloads
open Md4
open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
  
open LimewireTypes
open LimewireOptions
open LimewireGlobals
open LimewireComplexOptions

open LimewireProtocol

let http_ok = "HTTP 200 OK"
let http11_ok = "HTTP/1.1 200 OK"
  
module Download = CommonDownloads.Make(struct 
      
      type c = client
      type f = file
      
      let file file = as_file file.file_file
      let client client = as_client client.client_client
      let subdir_option = commit_in_subdir
            
      let client_disconnected d =
        let c = d.download_client in
        if !verbose_msg_clients then begin
            lprintf "Disconnected from source\n"; 
          end;
        connection_failed c.client_connection_control;
        set_client_disconnected c;
        c.client_sock <- None;
        c.client_file <- None
      
      let download_finished d = 
        let file = d.download_file in
        current_files := List2.removeq file !current_files;
        old_files =:= (file.file_name, file_size file) :: !!old_files;
        List.iter (fun c ->
            c.client_downloads <- List.remove_assoc file c.client_downloads
        ) file.file_clients
        
    end)

  
let disconnect_client c =
  match c.client_sock with
    None -> ()
  | Some sock -> 
      try
        if !verbose_msg_clients then begin
            lprintf "Disconnected from source\n"; 
          end;
        connection_failed c.client_connection_control;
        set_client_disconnected c;
        close sock "closed";
        c.client_sock <- None
      with _ -> ()
          
let is_http_ok header = 
  let pos = String.index header '\n' in
  match String2.split (String.sub header 0 pos) ' ' with
    http :: code :: ok :: _ -> 
      let code = int_of_string code in
      code >= 200 && code < 299 && 
      String2.starts_with (String.lowercase http) "http"
  | _ -> false
  
let client_parse_header c gconn sock header = 
  if !verbose_msg_clients then begin
      lprintf "CLIENT PARSE HEADER\n"; 
    end;
  try
    match c.client_file with 
      None -> close sock "no download !"; raise Exit
    | Some d ->
        connection_ok c.client_connection_control;
        set_client_state c Connected_initiating;    
        if !verbose_msg_clients then begin
            lprintf "HEADER FROM CLIENT:\n";
            LittleEndian.dump_ascii header; 
          end;
        if is_http_ok header then
          begin
            
            if !verbose_msg_clients then begin
                lprintf "GOOD HEADER FROM CONNECTED CLIENT\n";
              end;
            
            set_rtimeout sock 120.;
(*              lprintf "SPLIT HEADER...\n"; *)
            let lines = Http_client.split_header header in
(*              lprintf "REMOVE HEADLINE...\n"; *)
            match lines with
              [] -> raise Not_found        
            | _ :: headers ->
(*                  lprintf "CUT HEADERS...\n"; *)
                let headers = Http_client.cut_headers headers in
(*                  lprintf "START POS...\n"; *)
                let start_pos = 
                  try
                    let range = List.assoc "content-range" headers in
                    try
                      let npos = (String.index range 'b')+6 in
                      let dash_pos = try String.index range '-' with _ -> -10 in
                      let slash_pos = try String.index range '/' with _ -> -20 in
                      let star_pos = try String.index range '*' with _ -> -30 in
                      if star_pos = slash_pos-1 then
                        Int64.zero (* "bytes */X" *)
                      else
                      let x = Int64.of_string (
                          String.sub range npos (dash_pos - npos) )
                      in
                      if slash_pos = star_pos - 1 then 
                        x (* "bytes x-y/*" *)
                      else
                      let len = String.length range in
                      let y = Int64.of_string (
                          String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
                      in
                      let z = Int64.of_string (
                          String.sub range (slash_pos+1) (len - slash_pos -1) )
                      in
                      if y = z then Int64.sub x Int64.one else x
                    with 
                    | e ->
                        lprintf "Exception %s for range [%s]\n" 
                          (Printexc2.to_string e) range;
                        raise e
                  with Not_found -> Int64.zero
                in                  
                if d.CommonDownloads.download_pos <> start_pos then  begin
                    lprintf "Asked %s Bad range %s for %s\n"
                      (Md4.to_string c.client_user.user_uid)
                    (Int64.to_string start_pos)
                    (Int64.to_string d.CommonDownloads.download_pos);
                    raise Exit
                  end;
                set_client_state c (Connected 0);
                gconn.gconn_handler <- Reader (fun h sock nread ->
                    Download.download_reader d sock nread)
          end else begin
            if !verbose_msg_clients then begin
                lprintf "BAD HEADER FROM CONNECTED CLIENT:\n";
                LittleEndian.dump header;
              end;        
            disconnect_client c
          end
  with e ->
      lprintf "Exception %s in client_parse_header\n" (Printexc2.to_string e);
      LittleEndian.dump header;      
      disconnect_client c;
      raise e

let client_to_client s p sock =
  match p.pkt_payload with
  | _ -> ()

let friend_parse_header c gconn sock header =
  try
    if String2.starts_with header gnutella_200_ok then begin
        set_rtimeout sock half_day;
        let lines = Http_client.split_header header in
        match lines with
          [] -> raise Not_found        
        | _ :: headers ->
            let headers = Http_client.cut_headers headers in
            let agent =  List.assoc "user-agent" headers in
            if String2.starts_with agent "LimeWire" ||
              String2.starts_with agent "Gnucleus" ||
              String2.starts_with agent "BearShare"              
            then
              begin
                (* add_peers headers; *)
                write_string sock "GNUTELLA/0.6 200 OK\r\n\r\n";
                if !verbose_msg_clients then begin
                    lprintf "********* READY TO BROWSE FILES *********\n";
                  end;
                gconn.gconn_handler <- Reader
                  (gnutella_handler parse (client_to_client c))
              end
            else raise Not_found
      end 
    else raise Not_found
  with e -> 
      lprintf "Exception %s in friend_parse_header\n" 
        (Printexc2.to_string e); 
      disconnect_client c
      
let get_from_client sock (c: client) (file : file) =
  if !verbose_msg_clients then begin
      lprintf "FINDING ON CLIENT\n";
    end;
  let index = List.assoc file c.client_downloads in
  if !verbose_msg_clients then begin
      lprintf "FILE FOUND, ASKING\n";
    end;
  
  let new_pos = file_downloaded file in
  let buf = Buffer.create 100 in
  Printf.bprintf buf "GET /get/%d/%s HTTP/1.0\r\n" index file.file_name;
  Printf.bprintf buf "User-Agent: %s\r\n" user_agent;
  Printf.bprintf buf "Range: bytes=%Ld-\r\n" new_pos;
  Printf.bprintf buf "\r\n";
  write_string sock (Buffer.contents buf);
  
  let d = Download.new_download sock 
      c
      file
      (mini
        (Int64.to_int (Int64.sub (file_size file) (file_downloaded file)))
      1000)
  in
  c.client_file <- Some d;
  lprintf "Asking %s For Range %Ld\n" (Md4.to_string c.client_user.user_uid) new_pos; 
  d
  
let connect_client c =
  match c.client_sock with
  | Some sock -> ()
  | None ->
      if !verbose_msg_clients then begin
          lprintf "connect_client\n";
        end;
      try
        match c.client_user.user_kind with
          Indirect_location _ -> ()
        | Known_location (ip, port) ->
            if !verbose_msg_clients then begin
                lprintf "connecting %s:%d\n" (Ip.to_string ip) port; 
              end;
            let sock = connect "limewire download" 
                (Ip.to_inet_addr ip) port
                (fun sock event ->
                  match event with
                    BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
                      disconnect_client c
                  | BASIC_EVENT (CLOSED _) ->
                      disconnect_client c
                  | _ -> ()
              )
            in
            match c.client_downloads with
              [] -> 
(* Here, we should probably browse the client or reply to
an upload request *)
                TcpBufferedSocket.set_read_controler sock download_control;
                TcpBufferedSocket.set_write_controler sock upload_control;
                
                set_client_state c Connecting;
                c.client_sock <- Some sock;
                TcpBufferedSocket.set_closer sock (fun _ _ ->
                    disconnect_client c
                );
                set_rtimeout sock 30.;
                if !verbose_msg_clients then begin
                    lprintf "NOTHING TO DOWNLOAD FROM CLIENT\n";
                  end;
                if client_type c = NormalClient then                
                  disconnect_client c;
                set_gnutella_sock sock !verbose_msg_clients
                  (HttpHeader (friend_parse_header c));
                let s = add_header_fields 
                    "GNUTELLA CONNECT/0.6\r\n" sock 
                    (Printf.sprintf "Remote-IP: %s\r\n\r\n" (Ip.to_string ip))
                in
(*
        lprintf "SENDING\n";
        AP.dump s;
  *)
                write_string sock s;
            
            
            | (file, _) :: _ ->
                if !verbose_msg_clients then begin
                    lprintf "READY TO DOWNLOAD FILE\n";
                  end;
                let d = get_from_client sock c file in
                set_gnutella_sock sock !verbose_msg_clients
                  (HttpHeader (client_parse_header c))
      
      with e ->
          lprintf "Exception %s while connecting to client\n" 
            (Printexc2.to_string e);
          disconnect_client c
          

(*
  
1022569854.519 24.102.10.39:3600 -> 212.198.235.45:51736 of len 82
ascii [ 
G I V   8 1 : 9 7 4 3 2 1 3 F B 4 8 6 2 3 D 0 F F D F A B B 3 8 0 E C 6 C 0 0 / P o l i c e   V i d e o   -   E v e r y   B r e a t h   Y o u   T a k e . m p g(10)(10)]

"GIV %d:%s/%s\n\n" file.file_number client.client_md4 file.file_name

*)

let find_file file_name file_size = 
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key key  
  with e ->
      lprintf "NO SUCH DOWNLOAD\n";
      raise e
      
let push_handler cc gconn sock header = 
  if !verbose_msg_clients then begin
      lprintf "PUSH HEADER: [%s]\n" (String.escaped header);
    end;
  try
    if String2.starts_with header "GIV" then begin
        if !verbose_msg_clients then begin    
            lprintf "PARSING GIV HEADER\n"; 
          end;
        let colon_pos = String.index header ':' in
        let slash_pos = String.index header '/' in
        let uid = Md4.of_string (String.sub header (colon_pos+1) 32) in
        let index = int_of_string (String.sub header 4 (colon_pos-4)) in
        if !verbose_msg_clients then begin
            lprintf "PARSED\n";
          end;
        let c = new_client uid (Indirect_location ("", uid)) in
        match c.client_sock with
          Some _ -> 
            if !verbose_msg_clients then begin
                lprintf "ALREADY CONNECTED\n"; 
              end;
            close sock "already connected";
            raise End_of_file
        | None ->
            if !verbose_msg_clients then begin
                lprintf "NEW CONNECTION\n";
              end;
            cc := Some c;
            c.client_sock <- Some sock;
            connection_ok c.client_connection_control;
            try
              if !verbose_msg_clients then begin
                  lprintf "FINDING FILE %d\n" index; 
                end;
              let file = List2.assoc_inv index c.client_downloads in
              if !verbose_msg_clients then begin
                  lprintf "FILE FOUND\n";
                end;
              let d = get_from_client sock c file in
              gconn.gconn_handler <- HttpHeader (client_parse_header c)
            with e ->
                lprintf "Exception %s during client connection\n"
                  (Printexc2.to_string e);
                disconnect_client c;
                raise End_of_file
      end
    else begin
        lprintf "parse_head\n";
        let r = Http_server.parse_head (header ^ "\n") in
        lprintf "Header parsed: %s ... %s\n"
          (r.Http_server.request) (r.Http_server.get_url.Url.file);
(* "/get/num/filename" *)
        assert (r.Http_server.request = "GET");
        let file = r.Http_server.get_url.Url.file in
        let get = String.lowercase (String.sub file 0 5) in
        assert (get = "/get/");
        let pos = String.index_from file 5 '/' in
        let num = String.sub file 5 (pos - 5) in
        let filename = String.sub file (pos+1) (String.length file - pos - 1) in
        lprintf "Download of file %s, filename = %s\n" num filename;
        let num = int_of_string num in
        lprintf "Download of file %d, filename = %s\n" num filename;
        
        let sh = CommonUploads.find_by_num num in

(*
BUG:
  * check whether a range is requested, and we need to
  * to send a correct reply
*)
        let no_range = Int64.zero, sh.shared_size in
        let chunk_pos, chunk_end = 
          try
            let range, _, _ = List.assoc "Range" r.Http_server.headers in
            match parse_range range with
              x, None, _ -> x, sh.shared_size
            | x, Some y, Some z ->
                if y = z then (* some vendor bug *)
                  Int64.sub x Int64.one, y
                else
                  x, Int64.add y Int64.one
            | x, Some y, None ->
                x, Int64.add y Int64.one
          with _ -> no_range
        in
        let uc = {
            uc_sock = sock;
            uc_file = sh;
(* BUG: parse the range header *)
            uc_chunk_pos = chunk_pos;
            uc_chunk_len = Int64.sub chunk_end chunk_pos;
            uc_chunk_end = chunk_end;
          } in
        let header_sent = ref false in
        
        let impl = sh.shared_impl in
        impl.impl_shared_requests <- impl.impl_shared_requests + 1;
        shared_must_update_downloaded (as_shared impl);
        
        let rec refill sock =
          lprintf "refill called\n";
          if not !header_sent then begin
(* BUG: send the header *)
              let buf = Buffer.create 100 in
              Printf.bprintf buf "HTTP 200 OK\r\n";
              Printf.bprintf buf "Server: %s\r\n" user_agent;
              Printf.bprintf buf "Content-type:application/binary\r\n";
              Printf.bprintf buf "Content-length: %Ld\r\n" uc.uc_chunk_len;
              if (chunk_pos, chunk_end) <> no_range then begin
                  Printf.bprintf buf "Accept-Ranges: bytes\r\n";
                  Printf.bprintf buf "Content-range: bytes=%Ld-%Ld/%Ld\r\n"
                    chunk_pos (Int64.sub uc.uc_chunk_end Int64.one)
                  sh.shared_size;
                end;
              Buffer.add_string buf "\r\n";
              let s = Buffer.contents buf in
              TcpBufferedSocket.write_string sock s;
              lprintf "Sending Header:\n";
              LittleEndian.dump s;
              lprint_newline ();
              header_sent:=true;
            end;
          let len = remaining_to_write sock in
          let can = maxi (8192 - len) 0 in
          let slen = sh.shared_size in
          let pos = uc.uc_chunk_pos in
          if pos < uc.uc_chunk_end && can > 0 then
            let rlen = 
              let rem = Int64.sub slen  pos in
              if rem > Int64.of_int can then can else Int64.to_int rem
            in
            let upload_buffer = String.create rlen in
            Unix32.read sh.shared_fd pos upload_buffer 0 rlen;
            TcpBufferedSocket.write sock upload_buffer 0 rlen;
            
            let impl = sh.shared_impl in
            impl.impl_shared_uploaded <- 
              Int64.add impl.impl_shared_uploaded (Int64.of_int rlen);
            shared_must_update_downloaded (as_shared impl);

            uc.uc_chunk_pos <- Int64.add uc.uc_chunk_pos (Int64.of_int rlen);
            if remaining_to_write sock = 0 then refill sock
        in
        gconn.gconn_refill <- refill :: gconn.gconn_refill;
        match gconn.gconn_refill with
          [ _ ] ->
            (* First refill handler, must be called immediatly *)
            refill sock
        | _ -> (* Already a refill handler, wait for it to finish its job *)
            ()
      end
  with e ->
      lprintf "Exception %s in push_handler\n" (Printexc2.to_string e);
      (match !cc with Some c -> disconnect_client c | _ -> ());
      raise e

      (*
let client_parse_header2 c sock header = 
    match !c with
    Some c ->
      client_parse_header c sock header
  | _ -> assert false
      

let client_reader2 c sock nread = 
  match !c with
    None -> assert false
  | Some c ->
      match c.client_file with
        None -> assert false
      | Some d ->
          Download.download_reader d sock nread
            *)

let listen () =
  try
    let sock = TcpServerSocket.create "limewire client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              lprintf "CONNECTION RECEIVED FROM %s FOR PUSH\n"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              
              
              let sock = TcpBufferedSocket.create
                  "limewire client connection" s 
                  (fun sock event -> 
                    match event with
                      BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> close sock "timeout"
                    | _ -> ()
                )
              in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;

              let c = ref None in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    Some c ->  disconnect_client c
                  | None -> ()
              );
              BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
              set_gnutella_sock sock !verbose_msg_clients
                (HttpHeader (push_handler c));
          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init limewire server\n" 
        (Printexc2.to_string e)
      
let push_connection guid index ip port =
  let sh = CommonUploads.find_by_num index in
  let sock = connect "limewire download" 
      (Ip.to_inet_addr ip) port
      (fun sock event -> 
        match event with
          BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> close sock "timeout"
        | _ -> ()
    )
  in
  lprintf "CONNECTION PUSHED TO %s\n" (Ip.to_string ip); 
  
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  
  let c = ref None in
  TcpBufferedSocket.set_closer sock (fun _ s ->
      match !c with
        Some c ->  disconnect_client c
      | None -> ()
  );
  BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
  set_gnutella_sock sock !verbose_msg_clients
    (HttpHeader (push_handler c));
  write_string sock 
    (Printf.sprintf "GIV %d:%s/%s\n\n" 
      index (Md4.to_string guid) sh.shared_codedname)
  