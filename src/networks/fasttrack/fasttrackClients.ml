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

(*
Two solutions:
1) Limit the number of connection attempts: if we have 10 ranges to download,
only allow 10 concurrent connections, so that we will download each
range only from one client
2) Don't ask for the range immediatly, wait for the connection to be
accepted (using the refill function of the socket ?), and check which range
to ask at that point.
*)

open Queues
open Printf2
open Md4
open Options

open BasicSocket
open TcpBufferedSocket
  
open CommonShared
open CommonUploads
open CommonOptions
open CommonDownloads
open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open CommonGlobals
open CommonSwarming  
  
open FasttrackTypes
open FasttrackOptions
open FasttrackGlobals
open FasttrackComplexOptions
open FasttrackProtocol

      (*
let max_range_size = Int64.of_int (256 * 1024)
  *)
let min_range_size = Int64.of_int (256 * 1024)

let range_size file =  min_range_size
  (*
  let range =  file_size file // (Int64.of_int 10) in
  max (min range max_range_size) min_range_size
*)
let max_queued_ranges = 1

let nranges file = 
  Int64.to_int (Int64.div (file_size file) 
    min_range_size) + 5
  
let disconnect_client c r =
  match c.client_sock with
  | Connection sock -> 
      (try
          if !verbose_msg_clients then begin
              lprintf "Disconnected from source\n"; 
            end;
          c.client_requests <- [];
          connection_failed c.client_connection_control;
          set_client_disconnected c r;
          close sock r;
          c.client_sock <- NoConnection;
          if c.client_reconnect then
            List.iter (fun d ->
                let file = d.download_file in
                if not (List.memq file c.client_in_queues) then begin
                    Queue.put file.file_clients_queue (0,c);
                    c.client_in_queues <- file :: c.client_in_queues
                  end;
            ) c.client_downloads;
          match c.client_connected_for with
            None -> ()
          | Some file -> 
              file.file_nconnected_clients <- file.file_nconnected_clients - 1;
(*
              lprintf "For file %s, %d/%d clients connected (disconnected from %d)\n"
                (file.file_name) file.file_nconnected_clients (nranges file)
(client_num (as_client c.client_client));
  *)
              c.client_connected_for <- None
      with e -> 
          lprintf "Exception %s in disconnect_client\n"
            (Printexc2.to_string e))
  | _ -> ()

let download_finished file = 
  if List.memq file !current_files then begin
      file_completed (as_file file.file_file);
      FasttrackGlobals.remove_file file;
      old_files =:= (file.file_name, file_size file) :: !!old_files;
      List.iter (fun c ->
          c.client_downloads <- remove_download file c.client_downloads
      ) file.file_clients
    end

let check_finished file =
  if file_state file <> FileDownloaded &&
    (file_size file = Int64Swarmer.downloaded file.file_swarmer) then
    download_finished file
   
let rec client_parse_header c gconn sock header = 
  if !verbose_msg_clients then begin
      lprintf "CLIENT PARSE HEADER\n"; 
    end;
  try
    set_lifetime sock 3600.;
    let d = 
      match c.client_requests with 
        [] -> failwith "No download request !!!"
      | d :: tail ->
          c.client_requests <- tail;
          d
    in
    connection_ok c.client_connection_control;
    set_client_state c Connected_initiating;    
    if !verbose_msg_clients then begin
        lprintf "HEADER FROM CLIENT:\n";
        AnyEndian.dump_ascii header; 
      end;
    let file = d.download_file in
    let size = file_size file in
    
    let endline_pos = String.index header '\n' in
    let http, code = 
      match String2.split (String.sub header 0 endline_pos
        ) ' ' with
      | http :: code :: ok :: _ -> 
          let code = int_of_string code in
          if not (String2.starts_with (String.lowercase http) "http") then
            failwith "Not in http protocol";
          http, code
      | _ -> failwith "Not a HTTP header line"
    in
    if !verbose_msg_clients then begin
        lprintf "GOOD HEADER FROM CONNECTED CLIENT\n";
      end;
    
    set_rtimeout sock 120.;
(*              lprintf "SPLIT HEADER...\n"; *)
    let lines = Http_client.split_header header in
(*              lprintf "REMOVE HEADLINE...\n"; *)
    let first_line, headers = match lines with
        [] -> raise Not_found        
      | line :: headers -> line, headers
    in
(*                  lprintf "CUT HEADERS...\n"; *)
    let headers = Http_client.cut_headers headers in
(*                  lprintf "START POS...\n"; *)
    
    
    if !verbose_unknown_messages then begin
        let unknown_header = ref false in
        List.iter (fun (header, _) ->
            unknown_header := !unknown_header || not (List.mem header FasttrackProto.known_download_headers)
        ) headers;
        if !unknown_header then begin
            lprintf "FT DEVEL: Download Header contains unknown fields\n";
            lprintf "    %s\n" first_line;
            List.iter (fun (header, (value,header2)) ->
                lprintf "    [%s] = [%s](%s)\n" header value header2;
            ) headers;
            lprintf "FT DEVEL: end of header\n";        
          end;
      end;
    
    
    if  code < 200 || code > 299 then
      failwith "Bad HTTP code";
    
    
    let start_pos, end_pos = 
      try
        let (range,_) = List.assoc "content-range" headers in
        try
          let npos = (String.index range 'b')+6 in
          let dash_pos = try String.index range '-' with _ -> -10 in
          let slash_pos = try String.index range '/' with _ -> -20 in
          let star_pos = try String.index range '*' with _ -> -30 in
          if star_pos = slash_pos-1 then
            Int64.zero, size (* "bytes */X" *)
          else
          let x = Int64.of_string (
              String.sub range npos (dash_pos - npos) )
          in
          let len = String.length range in
          let y = Int64.of_string (
              String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
          in
          if slash_pos = star_pos - 1 then 
            x,y ++ Int64.one (* "bytes x-y/*" *)
          else
          let z = Int64.of_string (
              String.sub range (slash_pos+1) (len - slash_pos -1) )
          in
          if y = z then x -- Int64.one, size else 
            x,y ++ Int64.one
        with 
        | e ->
            lprintf "Exception %s for range [%s]\n" 
              (Printexc2.to_string e) range;
            raise e
      with e -> 
          try
            if code <> 206 && code <> 200 then raise Not_found;
            let (len,_) = List.assoc "content-length" headers in
            let len = Int64.of_string len in
            lprintf "Specified length: %Ld\n" len;
            match d.download_ranges with
              [] -> raise Not_found
            | (start_pos,end_pos,r) :: _ -> 
                lprintf "WARNING: Assuming client is replying to range\n";
                if len <> end_pos -- start_pos then
                  begin
                    lprintf "\n\nERROR: bad computed range: %Ld-%Ld/%Ld \n%s\n"
                      start_pos end_pos len
                      (String.escaped header);
                    raise Not_found
                  end;
                (start_pos, end_pos)
          with _ -> 
(* A bit dangerous, no ??? *)
              lprintf "ERROR: Could not find/parse range header (exception %s), disconnect\nHEADER: %s\n" 
                (Printexc2.to_string e)
              (String.escaped header);
              disconnect_client c (Closed_for_error "Bad HTTP Range");
              raise Exit
    in 
    (try
        let (len,_) = List.assoc "content-length" headers in
        let len = Int64.of_string len in
        lprintf "Specified length: %Ld\n" len;
        if len <> end_pos -- start_pos then
          begin
            failwith "\n\nERROR: bad computed range: %Ld-%Ld/%Ld \n%s\n"
              start_pos end_pos len
              (String.escaped header);
          end
      with _ -> 
          lprintf "[WARNING]: no Content-Length field\n%s\n"
            (String.escaped header)
    );
    
    lprintf "Receiving range: %Ld-%Ld (len = %Ld)\n%s\n"    
      start_pos end_pos (end_pos -- start_pos)
    (String.escaped header)
    ;
    set_client_state c (Connected_downloading);
    let counter_pos = ref start_pos in
(* Send the next request *)
    for i = 1 to max_queued_ranges do
      if List.length d.download_ranges <= max_queued_ranges then
        (try get_from_client sock c with _ -> ());
    done; 
    gconn.gconn_handler <- Reader (fun gconn sock ->
        if file_state file <> FileDownloading then begin
            disconnect_client c Closed_by_user;
            raise Exit;
          end;
        
        let b = TcpBufferedSocket.buf sock in
        let to_read = min (end_pos -- !counter_pos) 
          (Int64.of_int b.len) in
(*
        lprintf "Reading: end_pos %Ld counter_pos %Ld len %d = to_read %Ld\n"
end_pos !counter_pos b.len to_read;
   *)
        let to_read_int = Int64.to_int to_read in
(*
  lprintf "CHUNK: %s\n" 
          (String.escaped (String.sub b.buf b.pos to_read_int)); *)
        let old_downloaded = 
          Int64Swarmer.downloaded file.file_swarmer in
        List.iter (fun (_,_,r) -> Int64Swarmer.free_range r) 
        d.download_ranges;

        begin
          try
            Int64Swarmer.received file.file_swarmer
              !counter_pos b.buf b.pos to_read_int;
          with e -> 
              lprintf "FT: Exception %s in Int64Swarmer.received\n"
                (Printexc2.to_string e)
        end;
          c.client_reconnect <- true;
          List.iter (fun (_,_,r) ->
              Int64Swarmer.alloc_range r) d.download_ranges;
        let new_downloaded = 
          Int64Swarmer.downloaded file.file_swarmer in
        
        (match d.download_ranges with
            [] -> lprintf "EMPTY Ranges !!!\n"
          | r :: _ -> 
(*
              let (x,y) = Int64Swarmer.range_range r in
              lprintf "Received %Ld [%Ld] (%Ld-%Ld) -> %Ld\n"
                !counter_pos to_read
                x y 
                (new_downloaded -- old_downloaded)
*)        
              ()
        );
        
        if new_downloaded = file_size file then
          download_finished file;
        if new_downloaded <> old_downloaded then
          add_file_downloaded file.file_file
            (new_downloaded -- old_downloaded);
(*
lprintf "READ %Ld\n" (new_downloaded -- old_downloaded);
lprintf "READ: buf_used %d\n" to_read_int;
  *)
        TcpBufferedSocket.buf_used b to_read_int;
        counter_pos := !counter_pos ++ to_read;
        if !counter_pos = end_pos then begin
            match d.download_ranges with
              [] -> assert false
            | (_,_,r) :: tail ->
(*
                lprintf "Ready for next chunk (version %s)\nHEADER:%s\n" http
                  (String.escaped header);
                *)
                Int64Swarmer.free_range r;
                d.download_ranges <- tail;
                (* If we have no more range to receive, disconnect *)
                if d.download_ranges = [] then raise Exit;
                gconn.gconn_handler <- HttpHeader (client_parse_header c);
          end)
  
  with e ->
      lprintf "Exception %s in client_parse_header\n" (Printexc2.to_string e);
      AnyEndian.dump header;      
      disconnect_client c (Closed_for_exception e);
      raise e

and get_from_client sock (c: client) =
  
  let rec iter downloads =
    match downloads with
      [] -> 
        if !verbose_msg_clients then
          lprintf "No other download to start\n";
        raise Not_found
    | d :: tail ->
        if file_state d.download_file <> FileDownloading then iter tail
        else begin
            if !verbose_msg_clients then begin
                lprintf "FINDING ON CLIENT\n";
              end;
            let file = d.download_file in
            if !verbose_msg_clients then begin
                lprintf "FILE FOUND, ASKING\n";
              end;
            
            if !verbose_swarming then begin
                lprintf "Current download:\n  Current chunks: "; 
                List.iter (fun (x,y) -> lprintf "%Ld-%Ld " x y) d.download_chunks;
                lprintf "\n  Current ranges: ";
                List.iter (fun (x,y,r) ->
(*              let (x,y) = Int64Swarmer.range_range r               in *)
                    lprintf "%Ld-%Ld " x y) d.download_ranges;
                lprintf "\n  Current blocks: ";
                List.iter (fun b -> Int64Swarmer.print_block b) d.download_blocks;
                lprintf "\n\nFinding Range: \n";
              end;
            let range = 
              try
                let rec iter () =
                  match d.download_block with
                    None -> 
                      if !verbose_swarming then
                        lprintf "No block\n";
                      let b = Int64Swarmer.get_block d.download_blocks in
                      if !verbose_swarming then begin
                          lprintf "Block Found: "; Int64Swarmer.print_block b;
                        end;
                      d.download_block <- Some b;
                      iter ()
                  | Some b ->
                      if !verbose_swarming then begin
                          lprintf "Current Block: "; Int64Swarmer.print_block b;
                        end;
                      try
                        let r = Int64Swarmer.find_range b 
                            d.download_chunks (List.map 
                              (fun (_,_,r) -> r)d.download_ranges)
                          (range_size file) in
                        let (x,y) = Int64Swarmer.range_range r in 
                        d.download_ranges <- d.download_ranges @ [x,y,r];
                        Int64Swarmer.alloc_range r;
                        Printf.sprintf "%Ld-%Ld" x (y -- Int64.one)
                      with Not_found ->
                          if !verbose_swarming then 
                            lprintf "Could not find range in current block\n";
                          d.download_blocks <- List2.removeq b d.download_blocks;
                          d.download_block <- None;
                          iter ()
                in
                iter ()
              with Not_found -> 
                  lprintf "Unable to get a block !!";
                  check_finished file;
                  raise Not_found
            in
            let buf = Buffer.create 100 in
            let url = Printf.sprintf 
               "/.hash=%s" (Md5Ext.to_hexa_case false file.file_hash) in 
 
             Printf.bprintf buf "GET %s HTTP/1.0\r\n" url;

            (*
            (match d.download_uri with
                FileByUrl url -> Printf.bprintf buf "GET %s HTTP/1.0\r\n" url
              | FileByIndex (index, name) -> 
                  Printf.bprintf buf "GET /get/%d/%s HTTP/1.1\r\n" index
                    name); *)
            (match c.client_host with
                None -> ()
              | Some (ip, port) ->
                  Printf.bprintf buf "Host: %s:%d\r\n" 
                    (Ip.to_string ip) port);
(*      Printf.bprintf buf "User-Agent: %s\r\n" user_agent; *)
            Printf.bprintf buf "X-Kazaa-Network: %s\r\n" network_name;
            Printf.bprintf buf "X-Kazaa-Username: %s\r\n" (client_name ());
(* fst_http_request_set_header (request, "Connection", "close"); *)
            Printf.bprintf buf "Range: bytes=%s\r\n" range;
            Printf.bprintf buf "\r\n";
            let s = Buffer.contents buf in
            if !verbose_msg_clients then
              lprintf "SENDING REQUEST: %s\n" (String.escaped s);
            write_string sock s;
            c.client_requests <- c.client_requests @ [d];
            if !verbose_msg_clients then 
              lprintf "Asking %s For Range %s\n" (Md4.to_string c.client_user.user_uid) 
              range
          end
  in
  iter c.client_downloads
  
let init_client sock =
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  ()
  
let connect_client c =
  match c.client_sock with
  | Connection sock | CompressedConnection (_,_,_,sock) -> ()
  | ConnectionWaiting -> ()
  | ConnectionAborted -> c.client_sock <- ConnectionWaiting;
  | NoConnection ->
      
(* Count this connection in the first file counter. Here, we assume
that the connection will not be aborted (otherwise, disconnect_client
  should clearly be called). *)
      (try List.iter (fun d ->
              let file = d.download_file in
              if file_state file = FileDownloading then
                begin
                  c.client_connected_for <- Some file;
                  file.file_nconnected_clients <- 
                    file.file_nconnected_clients + 1;
                  (*
                  lprintf "For file %s, %d/%d clients connected (connecting %d)\n"
                    (file.file_name)
                  file.file_nconnected_clients (nranges file) 
                    (client_num (as_client c.client_client)); *)
                  raise Exit;
                end
          ) c.client_downloads with _ -> ());
      
      add_pending_connection (fun _ ->
            match c.client_sock with
              ConnectionAborted -> c.client_sock <- NoConnection
            | Connection _ | NoConnection -> ()
            | _ ->
                if List.exists (fun d ->
                      let file = d.download_file in
                      file_state file = FileDownloading 
                  ) c.client_downloads 
                then
                  try
                    if !verbose_msg_clients then begin
                        lprintf "connect_client\n";
                      end;
                    match c.client_user.user_kind with
                      Indirect_location _ -> ()
                    | Known_location (ip, port) ->
                        if !verbose_msg_clients then begin
                            lprintf "connecting %s:%d\n" (Ip.to_string ip) port; 
                          end;
                        c.client_reconnect <- false;
                        let sock = connect "gnutella download" 
                            (Ip.to_inet_addr ip) port
                            (fun sock event ->
                              match event with
                                BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
                                  disconnect_client c Closed_for_timeout
                              | BASIC_EVENT (CLOSED s) ->
                                  disconnect_client c s

(* You can only use the CONNECTED signal if the socket is not yet controlled
  by the bandwidth manager... *)
                              
                              | CONNECTED ->
(*           lprintf "CONNECTED !!! Asking for range...\n"; *)
                                  init_client sock;                
                                  get_from_client sock c
                              | _ -> ()
                          )
                        in
                        c.client_host <- Some (ip, port);
                        set_client_state c Connecting;
                        c.client_sock <- Connection sock;
                        TcpBufferedSocket.set_closer sock (fun _ s ->
                            disconnect_client c s
                        );
                        set_rtimeout sock 30.;
                        if !verbose_msg_clients then begin
                            lprintf "READY TO DOWNLOAD FILE\n";
                          end;
                        set_fasttrack_sock sock !verbose_msg_clients
                          (HttpHeader (client_parse_header c))
                  
                  with e ->
                      lprintf "Exception %s while connecting to client\n" 
                        (Printexc2.to_string e);
                      disconnect_client c (Closed_for_exception e)
        );
        c.client_sock <- ConnectionWaiting
        
(*
let current_downloads = ref []

let push_handler cc gconn sock header = 
  if !verbose_msg_clients then begin
      lprintf "PUSH HEADER: [%s]\n" (String.escaped header);
    end;
  try
    let (ip, port) = TcpBufferedSocket.host sock in
    
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
        let c = try
            Hashtbl.find clients_by_uid (Indirect_location ("", uid)) 
          with _ ->
              try
                Hashtbl.find clients_by_uid (Known_location (ip,port))
              with _ ->
                  new_client  (Indirect_location ("", uid)) 
        in
        c.client_host <- Some (ip, port);
        match c.client_sock with
        | Connection _ -> 
            if !verbose_msg_clients then begin
                lprintf "ALREADY CONNECTED\n"; 
              end;
            close sock (Closed_for_error "already connected");
            raise End_of_file
        | _ ->
            if !verbose_msg_clients then begin
                lprintf "NEW CONNECTION\n";
              end;
            cc := Some c;
            c.client_sock <- Connection sock;
            connection_ok c.client_connection_control;
            try
              if !verbose_msg_clients then begin
                  lprintf "FINDING FILE %d\n" index; 
                end;
              let d = find_download_by_index index c.client_downloads in
              if !verbose_msg_clients then begin
                  lprintf "FILE FOUND\n";
                end;
              
              c.client_downloads <- d :: (List2.removeq d c.client_downloads);
              get_from_client sock c;
              gconn.gconn_handler <- HttpHeader (client_parse_header c)
            with e ->
                lprintf "Exception %s during client connection\n"
                  (Printexc2.to_string e);
                disconnect_client c (Closed_for_exception e);
                raise End_of_file
      end
    else begin
(*        lprintf "parse_head\n";    *)
        let r = Http_server.parse_head (header ^ "\n") in
        let url = r.Http_server.get_url in
        lprintf "Header parsed: %s ... %s\n"
          (r.Http_server.request) (url.Url.file);
(* "/get/num/filename" *)
        assert (r.Http_server.request = "GET");
        
(* First of all, can we accept this request ???? *)
        
        let rec iter list rem =
          match list with 
            [] ->
              if List.length rem >= !!max_available_slots then
                failwith "All Slots Used";
              sock :: rem
          | s :: tail ->
              if s == sock then list @ rem
              else
              if closed sock then
                iter tail rem
              else
                iter tail (s :: rem)
        in
        current_downloads := iter !current_downloads [];
        let file = url.Url.file in                    
        let sh = 
          if file = "/uri-res/N2R" then
            match url.Url.args with
              [(urn,_)] ->
                lprintf "Found /uri-res/N2R request\n";
                Hashtbl.find shareds_by_uid urn
                
            | _ -> failwith "Cannot parse /uri-res/N2R request"
          else
          let get = String.lowercase (String.sub file 0 5) in
          assert (get = "/get/");
          let pos = String.index_from file 5 '/' in
          let num = String.sub file 5 (pos - 5) in
          let filename = String.sub file (pos+1) (String.length file - pos - 1) in
          lprintf "Download of file %s, filename = %s\n" num filename;
          let num = int_of_string num in
          lprintf "Download of file %d, filename = %s\n" num filename;
          CommonUploads.find_by_num num
        in

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
                  x -- Int64.one, y
                else
                  x, y ++ Int64.one
            | x, Some y, None ->
                x, y ++ Int64.one
          with _ -> no_range
        in
        let uc = {
            uc_sock = sock;
            uc_file = sh;
(* BUG: parse the range header *)
            uc_chunk_pos = chunk_pos;
            uc_chunk_len = chunk_end -- chunk_pos;
            uc_chunk_end = chunk_end;
          } in
        let header_sent = ref false in
        
        let impl = sh.shared_impl in
        impl.impl_shared_requests <- impl.impl_shared_requests + 1;
        shared_must_update_downloaded (as_shared impl);
        
        let rec refill sock =
          lprintf "refill called\n";
          if uc.uc_chunk_pos = uc.uc_chunk_end then begin
              match gconn.gconn_refill with
                []  -> ()
              | [_] -> gconn.gconn_refill <- []
              | _ :: ((refill :: _ ) as tail) -> 
                  gconn.gconn_refill <- tail;
                  refill sock
            end else
          if not !header_sent then begin
(* BUG: send the header *)
              let buf = Buffer.create 100 in
              Printf.bprintf buf "HTTP/1.1 200 OK\r\n";
              Printf.bprintf buf "Server: %s\r\n" user_agent;
              Printf.bprintf buf "Content-type:application/binary\r\n";
              Printf.bprintf buf "Content-length: %Ld\r\n" uc.uc_chunk_len;
              if (chunk_pos, chunk_end) <> no_range then begin
                  Printf.bprintf buf "Accept-Ranges: bytes\r\n";
                  Printf.bprintf buf "Content-range: bytes=%Ld-%Ld/%Ld\r\n"
                    chunk_pos (uc.uc_chunk_end -- Int64.one)
                  sh.shared_size;
                end;
              Buffer.add_string buf "\r\n";
              let s = Buffer.contents buf in
              TcpBufferedSocket.write_string sock s;
              lprintf "Sending Header:\n";
              AnyEndian.dump s;
              lprint_newline ();
              header_sent:=true;
            end;
          let len = remaining_to_write sock in
          let can = maxi (8192 - len) 0 in
          let slen = sh.shared_size in
          let pos = uc.uc_chunk_pos in
          if pos < uc.uc_chunk_end && can > 0 then
            let rlen = 
              let rem = slen --  pos in
              if rem > Int64.of_int can then can else Int64.to_int rem
            in
            let upload_buffer = String.create rlen in
            Unix32.read sh.shared_fd pos upload_buffer 0 rlen;
            TcpBufferedSocket.write sock upload_buffer 0 rlen;
            
            let impl = sh.shared_impl in
            impl.impl_shared_uploaded <- 
              impl.impl_shared_uploaded ++ (Int64.of_int rlen);
            shared_must_update_downloaded (as_shared impl);

            uc.uc_chunk_pos <- uc.uc_chunk_pos ++ (Int64.of_int rlen);
            if remaining_to_write sock = 0 then refill sock
        in
        gconn.gconn_refill <- refill :: gconn.gconn_refill;
        match gconn.gconn_refill with
          refill :: tail ->
(* First refill handler, must be called immediatly *)
            refill sock
        | _ -> (* Already a refill handler, wait for it to finish its job *)
            ()
      end
  with e ->
      lprintf "Exception %s in push_handler: %s\n" (Printexc2.to_string e)
      (String.escaped header);
      (match !cc with Some c -> disconnect_client c (Closed_for_exception e)
| _ -> ());
      raise e

let listen () =
  try
    let sock = TcpServerSocket.create "gnutella client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              lprintf "CONNECTION RECEIVED FROM %s FOR PUSH\n"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              
              lprintf "*********** CONNECTION ***********\n";
              let sock = TcpBufferedSocket.create
                  "gnutella client connection" s 
                  (fun sock event -> 
                    match event with
                      BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
                        close sock Closed_for_timeout
                    | _ -> ()
                )
              in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;

              let c = ref None in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    Some c ->  disconnect_client c s
                  | None -> ()
              );
              BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
              set_fasttrack_sock sock !verbose_msg_clients
                (HttpHeader (push_handler c));
          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init gnutella server\n" 
        (Printexc2.to_string e)
*)    
