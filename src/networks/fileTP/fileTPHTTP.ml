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
open CommonDownloads  
  
open FileTPTypes
open FileTPOptions
open FileTPGlobals
open FileTPComplexOptions
open FileTPProtocol

open FileTPClients

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
let http_send_range_request c range sock d = 
  let url = d.download_url in
  
  let (x,y) = range in
  let range = Printf.sprintf "%Ld-%Ld" x (y -- Int64.one) in

  let buf = Buffer.create 100 in
  
  Printf.bprintf buf "GET %s HTTP/1.0\r\n" url.Url.full_file;

(*
            (match d.download_uri with
                FileByUrl url -> Printf.bprintf buf "GET %s HTTP/1.0\r\n" url
              | FileByIndex (index, name) -> 
                  Printf.bprintf buf "GET /get/%d/%s HTTP/1.1\r\n" index
                    name); *)
  Printf.bprintf buf "Host: %s\r\n" c.client_hostname;
  Printf.bprintf buf "User-Agent: %s\r\n" user_agent; 
  Printf.bprintf buf "Range: bytes=%s\r\n" range;
  Printf.bprintf buf "Connection: Keep-Alive\r\n";
  Printf.bprintf buf "\r\n";
  let s = Buffer.contents buf in
  if !verbose_msg_clients then
    lprintf "SENDING REQUEST to %s: %s\n" 
      c.client_hostname
      (String.escaped s);
  write_string sock s;
  c.client_requests <- c.client_requests @ [d]

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

  
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
            unknown_header := !unknown_header || not (List.mem header known_download_headers)
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
    set_client_state c (Connected_downloading (file_num file));
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
        let swarmer = match file.file_swarmer with
            None -> assert false | Some sw -> sw
        in
        let old_downloaded = 
          Int64Swarmer.downloaded swarmer in
(*        List.iter (fun (_,_,r) -> Int64Swarmer.free_range r)  
        d.download_ranges; *)
        
        begin
          try
            match d.download_uploader with
              None -> assert false
            | Some up ->
                Int64Swarmer.received up
                  !counter_pos b.buf b.pos to_read_int;
          with e -> 
              lprintf "FT: Exception %s in Int64Swarmer.received\n"
                (Printexc2.to_string e)
        end;
        c.client_reconnect <- true;
(*          List.iter (fun (_,_,r) ->
              Int64Swarmer.alloc_range r) d.download_ranges; *)
        let new_downloaded = 
          Int64Swarmer.downloaded swarmer in
        
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
(*                Int64Swarmer.free_range r; *)
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
      

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
        
let http_set_sock_handler c sock =
  set_fileTP_sock sock (HttpHeader (client_parse_header c))

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
let http_check_size url start_download_file =
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_request = H.HEAD;
      H.req_user_agent = user_agent;
    } in
  
  H.whead r (fun headers ->      
      lprintf "RECEIVED HEADERS\n";
      let content_length = ref None in
      List.iter (fun (name, content) ->
          if String.lowercase name = "content-length" then
            try          
              content_length := Some (Int64.of_string content)
            with _ -> 
                lprintf "bad content length [%s]\n" content;
      ) headers;
      match !content_length with
        None -> failwith "Unable to start download (HEAD failed)"
      | Some result_size ->
          start_download_file result_size)

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let http_connect token c f =
  let ip = Ip.from_name c.client_hostname in
  connect token "fileTP download" 
      (Ip.to_inet_addr ip) c.client_port
      (fun sock event ->
        match event with
          BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
            disconnect_client c Closed_for_timeout
        | BASIC_EVENT (CLOSED s) ->
            disconnect_client c s

(* You can only use the CONNECTED signal if the socket is not yet controlled
by the bandwidth manager... 2004/02/03: Normally, not true anymore, it should now work
  even in this case... *)
        
        | CONNECTED ->
          lprintf "CONNECTED !!! Asking for range...\n"; 
          f sock
        | _ -> ()
    )
    
  
let proto =
  {
    proto_send_range_request = http_send_range_request;
    proto_set_sock_handler = http_set_sock_handler;
    proto_check_size = http_check_size;
    proto_string = "http";
    proto_connect = http_connect;
  }
  
  