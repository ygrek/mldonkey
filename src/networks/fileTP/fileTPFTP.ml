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

open Int32ops
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
  
open FileTPTypes
open FileTPOptions
open FileTPGlobals
open FileTPComplexOptions
open FileTPProtocol

open FileTPClients

let range_reader c d counter_pos end_pos sock nread =
  if nread > 0 then
  let file = d.download_file in
  lprintf ".";
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
                lprintf "Ready for next chunk (version %s)\nHEADER:%s\n" ftp
                  (String.escaped header);
                *)
(*                Int64Swarmer.free_range r; *)
          d.download_ranges <- tail;
(* If we have no more range to receive, disconnect *)
          lprintf "\n ********** RANGE DOWNLOADED  ********** \n";
          close sock Closed_by_user

    end

let download_on_port c d (x,y) ip port =
  let token = create_token unlimited_connection_manager in
  let sock = TcpBufferedSocket.connect token "http client connecting"
      (Ip.to_inet_addr ip)
    port (fun _ e -> 
        ()
(*              lprintf "Event %s\n"
                (match e with
                  WRITE_DONE -> "WRITE_DONE"
                | CAN_REFILL -> "CAN_REFILL"
                | BUFFER_OVERFLOW -> "BUFFER_OVERFLOW"
                | READ_DONE n -> Printf.sprintf "READ_DONE %d" n
                | BASIC_EVENT e ->
                    match e with
                      (CLOSED s) -> Printf.sprintf "CLOSED %s" s
                    | RTIMEOUT -> "RTIMEOUT"
                    | LTIMEOUT -> "LTIMEOUT"
                    | WTIMEOUT -> "WTIMEOUT"
                    | CAN_READ -> "CAN_READ"
                    | CAN_WRITE -> "CAN_WRITE"
              ) *)
    )
  in
  let nread = ref false in
  TcpBufferedSocket.set_reader sock (range_reader c d (ref x) y);
  set_rtimeout sock 15.;
  TcpBufferedSocket.set_closer sock (fun _ _ -> ()
(*        lprintf "Connection closed nread:%b\n" !nread; *)
  )

let write_reqs sock reqs =
  
  let buf = Buffer.create 100 in
  List.iter (fun s -> Printf.bprintf buf "%s\r\n" s) reqs;
  let request = Buffer.contents buf in

(*
    let args = ref [] in
    let headers = ref [] in
    let ispost = ref false in
    let timeout = ref 300.0 in
    let proxy = ref None in
    List.iter (function   
      | Args l -> args := l@ !args
      | Headers l -> headers := l @ !headers;
      | Post -> ispost := true
      | Proxy (h,p) -> proxy := Some (h,p)
    ) get_args;
    let args = !args in
    let headers = !headers in
    let ispost = !ispost in
    let proxy = !proxy in
*)    
  
  TcpBufferedSocket.write_string sock request
  
let ftp_send_range_request c (x,y) sock d =  

  lprintf "FTP: Asking range %Ld-%Ld\n" x y ;
  
  let file = d.download_url in
  let reqs = [ 
      Printf.sprintf "CWD %s" (Filename.dirname file);
(* 250 *)
      "PASV";
(* 227 *)
      Printf.sprintf "REST %Ld" x;
(* 350 *)
      Printf.sprintf "RETR %s" (Filename.basename file);
(* 150 *)
    ]
  in

  
  write_reqs sock reqs;
  
  TcpBufferedSocket.set_reader sock (fun sock nread ->
      let b = TcpBufferedSocket.buf sock in
      let rec iter i =
        if i < b.len then
          if b.buf.[b.pos + i] = '\n' then begin
              let slen = if i > 0 && b.buf.[b.pos + i - 1] = '\r' then
                  i - 1
                else i in
              let line = String.sub b.buf b.pos slen in
              lprintf "FTP LINE [%s]\n" line;
              buf_used b (i+1);
              if slen > 4 && String.sub line 0 4 = "227 " then 
                try
                  let pos = String.index line '(' in
                  let line = String.sub line (pos+1) (slen - pos - 1) in
                  let pos = String.index line ')' in
                  let line = String.sub line 0 pos in
                  match 
                    List.map int_of_string (
                      String2.split_simplify line ',') with
                    [a0;a1;a2;a3;p0;p1] ->
                      let ip = Ip.of_string 
                          (Printf.sprintf "%d.%d.%d.%d" a0 a1 a2 a3) in
                      let port = p0 * 256 + p1 in
                      set_rtimeout sock 3600.;
                      download_on_port c d (x,y) ip port
                  | _ -> 
                      lprintf "FTP: cannot read ip address [%s]\n" line;
                      close sock Closed_by_user
                with e ->
                    lprintf "FTP: Error %s in reader\n"
                      (Printexc.to_string e);
                    close sock Closed_by_user
              else
              if slen > 4 && String.sub line 0 4 = "150 " then begin
                  lprintf "FTP: should initiate connection !\n"
                end
              else
              if slen > 4 && String.sub line 0 4 = "426 " then begin
                  lprintf "ASK ANOTHER CHUNK\n";
                  set_rtimeout sock 15.;
                  (try get_from_client sock c with _ -> ());
                end
              else
                iter 0
            end else
            iter (i+1)
      in
      iter 0
  );
  set_rtimeout sock 15.;
  TcpBufferedSocket.set_closer sock (fun _ _ -> 
    lprintf "\n+++++++++++ DISCONNECTED ++++++++++++++\n"  
      
  );
  ()  
  
  (*
  let url = d.download_url in
  
  let (x,y) = range in
  let range = Printf.sprintf "%Ld-%Ld" x (y -- Int64.one) in

  let buf = Buffer.create 100 in
  
  Printf.bprintf buf "GET %s HTTP/1.0\r\n" url;

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
*)
  
(*  
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
    let ftp, code = 
      match String2.split (String.sub header 0 endline_pos
        ) ' ' with
      | ftp :: code :: ok :: _ -> 
          let code = int_of_string code in
          if not (String2.starts_with (String.lowercase ftp) "ftp") then
            failwith "Not in ftp protocol";
          ftp, code
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
    gconn.gconn_handler <- Reader (  
  with e ->
      lprintf "Exception %s in client_parse_header\n" (Printexc2.to_string e);
      AnyEndian.dump header;      
      disconnect_client c (Closed_for_exception e);
      raise e
*)      
        
let ftp_set_sock_handler c sock = 
  
  write_reqs sock 
    [
(* 220 messages... *)
    "USER anonymous";
(* 331 *)
    "PASS -mldonkey@";
(* 230 *)
    "SYST";
(* 215 *)
    "PWD";
(* 257 "/" *)
    "TYPE I";
(* 200 *)    
  ]
 (*  set_fileTP_sock sock (HttpHeader (client_parse_header c)) *)
  
let ftp_check_size u url start_download_file = 

  let reqs = [ 
(* 220 messages... *)
      "USER anonymous";
(* 331 *)
      "PASS -mldonkey@";
(* 230 *)
      "SYST";
(* 215 *)
      "PWD";
(* 257 "/" *)
      "TYPE I";
(* 200 *)
      Printf.sprintf "CWD %s" (Filename.dirname u.Url.file);
(* 250 *)
      Printf.sprintf "SIZE %s" (Filename.basename u.Url.file);
(* 213 size *)
    ]
  in

  let buf = Buffer.create 100 in
  List.iter (fun s -> Printf.bprintf buf "%s\r\n" s) reqs;
  let request = Buffer.contents buf in
  
(*
    let args = ref [] in
    let headers = ref [] in
    let ispost = ref false in
    let timeout = ref 300.0 in
    let proxy = ref None in
    List.iter (function   
      | Args l -> args := l@ !args
      | Headers l -> headers := l @ !headers;
      | Post -> ispost := true
      | Proxy (h,p) -> proxy := Some (h,p)
    ) get_args;
    let args = !args in
    let headers = !headers in
    let ispost = !ispost in
    let proxy = !proxy in
*)    
  
  let server, port = u.Url.server, u.Url.port in
(*    lprintf "async_ip ...\n"; *)
  Ip.async_ip server (fun ip ->
(*        lprintf "IP done %s:%d\n" (Ip.to_string ip) port; *)
      let token = create_token unlimited_connection_manager in
      let sock = TcpBufferedSocket.connect token "http client connecting"
          (Ip.to_inet_addr ip)
        port (fun _ e -> 
            ()
(*              lprintf "Event %s\n"
                (match e with
                  WRITE_DONE -> "WRITE_DONE"
                | CAN_REFILL -> "CAN_REFILL"
                | BUFFER_OVERFLOW -> "BUFFER_OVERFLOW"
                | READ_DONE n -> Printf.sprintf "READ_DONE %d" n
                | BASIC_EVENT e ->
                    match e with
                      (CLOSED s) -> Printf.sprintf "CLOSED %s" s
                    | RTIMEOUT -> "RTIMEOUT"
                    | LTIMEOUT -> "LTIMEOUT"
                    | WTIMEOUT -> "WTIMEOUT"
                    | CAN_READ -> "CAN_READ"
                    | CAN_WRITE -> "CAN_WRITE"
              ) *)
        )
      in
      let nread = ref false in
      TcpBufferedSocket.write_string sock request;
      TcpBufferedSocket.set_reader sock (fun sock nread ->
          let b = TcpBufferedSocket.buf sock in
          let rec iter i =
            if i < b.len then
              if b.buf.[b.pos + i] = '\n' then begin
                  let slen = if i > 0 && b.buf.[b.pos + i - 1] = '\r' then
                      i - 1
                    else i in
                  let line = String.sub b.buf b.pos slen in
                  lprintf "FTP LINE [%s]\n" line;
                  buf_used b (i+1);
                  if slen > 4 && String.sub line 0 4 = "213 "
                  then begin
                      let result_size = 
                        Int64.of_string (String.sub line 4 (slen - 4))
                      in
                      lprintf "SIZE: [%Ld]\n" result_size;
                      start_download_file u url result_size;
                      close sock Closed_by_user
                    end else
                  iter 0
                end else
                iter (i+1)
          in
          iter 0
      );
      set_rtimeout sock 15.;
      TcpBufferedSocket.set_closer sock (fun _ _ -> ()
(*        lprintf "Connection closed nread:%b\n" !nread; *)
      )
  );
   
  ()
(*
tyminouch:~%  wget -c -d ftp://www-ftp.lip6.fr/pub/linux/distributions/mandrake-iso/i586/MandrakeLinux-10.0-beta2-CD1.i586.iso
DEBUG output created by Wget 1.8.2 on linux-gnu.

--22:44:24--  ftp://www-ftp.lip6.fr/pub/linux/distributions/mandrake-iso/i586/MandrakeLinux-10.0-beta2-CD1.i586.iso
           => `MandrakeLinux-10.0-beta2-CD1.i586.iso'
Resolving www-ftp.lip6.fr... done.
Caching www-ftp.lip6.fr => 195.83.118.1
Connecting to www-ftp.lip6.fr[195.83.118.1]:21... connected.
Created socket 5.
Releasing 0x807b070 (new refcount 1).
Logging in as anonymous ... 220-
220-        -- BIENVENUE SUR LE NOUVEAU SERVEUR FTP LIP6/JUSSIEU --
220-Utilisez le compte `anonymous' avec votre adresse e-mail comme mot de passe
220-    Merci de signaler les problèmes éventuels à ftpmaint@lip6.fr.
220-
220-    -- WELCOME ON THE NEW LIP6/JUSSIEU FTP SERVER --
220-    Please login as `anonymous' with your e-mail address as password
220-            Please report problems to ftpmaint@lip6.fr.
220-
220-
220 ftp.lip6.fr FTP server ready.

--> USER anonymous

331 Guest login ok, send your complete e-mail address as password.

--> PASS -wget@

230 Guest login ok, access restrictions apply.
Logged in!
==> SYST ... 
--> SYST

215 UNIX Type: L8
done.    ==> PWD ... 
--> PWD

257 "/" is current directory.
done.
==> TYPE I ... 
--> TYPE I

200 Type set to I.
done.  changing working directory
Prepended initial PWD to relative path:
  old: 'pub/linux/distributions/mandrake-iso/i586'
  new: '/pub/linux/distributions/mandrake-iso/i586'
==> CWD /pub/linux/distributions/mandrake-iso/i586 ... 
--> CWD /pub/linux/distributions/mandrake-iso/i586

250 CWD command successful.
done.
==> SIZE MandrakeLinux-10.0-beta2-CD1.i586.iso ... 
--> SIZE MandrakeLinux-10.0-beta2-CD1.i586.iso

213 681385984
done.
==> PASV ... 
--> PASV

227 Entering Passive Mode (195,83,118,1,88,253)
Created socket 7.
done.    ==> REST 339208 ... 
--> REST 339208

350 Restarting at 339208. Send STORE or RETRIEVE to initiate transfer.
done.    
==> RETR MandrakeLinux-10.0-beta2-CD1.i586.iso ... 
--> RETR MandrakeLinux-10.0-beta2-CD1.i586.iso

150 Opening BINARY mode data connection for MandrakeLinux-10.0-beta2-CD1.i586.iso (681385984 bytes).
done.
Length: 681,385,984 [681,046,776 to go]

*)
  
  
  (*
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = u;
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

*)
  
let proto =
  {
    proto_send_range_request = ftp_send_range_request;
    proto_set_sock_handler = ftp_set_sock_handler;
    proto_check_size = ftp_check_size;
    proto_string = "ftp"
  }
  
  