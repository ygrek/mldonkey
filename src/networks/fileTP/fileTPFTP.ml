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

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

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

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
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

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
        
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

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
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
  
  let server, port = u.Url.server, u.Url.port in
(*    lprintf "async_ip ...\n"; *)
  Ip.async_ip server (fun ip ->
(*        lprintf "IP done %s:%d\n" (Ip.to_string ip) port; *)
      let token = create_token unlimited_connection_manager in
      let sock = TcpBufferedSocket.connect token "http client connecting"
          (Ip.to_inet_addr ip)
        port (fun _ e -> 
            ()
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

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let ftp_connect token c f =
  let ip = Ip.from_name c.client_hostname in
  connect token "fileTP download" 
      (Ip.to_inet_addr ip) c.client_port
      (fun sock event ->
        match event with
          BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
            disconnect_client c Closed_for_timeout
        | BASIC_EVENT (CLOSED s) ->
            disconnect_client c s

        | CONNECTED ->
          lprintf "CONNECTED !!! Asking for range...\n"; 
          f sock
        | _ -> ()
    )

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
let proto =
  {
    proto_send_range_request = ftp_send_range_request;
    proto_set_sock_handler = ftp_set_sock_handler;
    proto_check_size = ftp_check_size;
    proto_string = "ftp";
    proto_connect = ftp_connect;
  }
  
  