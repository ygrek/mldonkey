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
open Printf2

open BasicSocket
open TcpBufferedSocket

open CommonOptions
open CommonClient
open CommonTypes

open FileTPTypes
open FileTPGlobals

open FileTPClients

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let default_user = "anonymous"
let default_pass = "-mldonkey@"

let get_user url = if url.Url.user = "" then default_user else url.Url.user
let get_pass url = if url.Url.passwd = "" then default_pass else url.Url.passwd

let reg530 = Str.regexp ".*not connect more.*\\|.*too many.*\\|.*overloaded.*\\|.*try \\(again \\|back \\)?later.*\\|.*is restricted to.*\\|.*maximum number.*\\|.*only.*session.*allowed.*\\|.*more connection.*"

let retry_530 s = 
  Str.string_match reg530 s 0  

let range_reader c d counter_pos end_pos sock nread =
  if nread > 0 then
    let file = d.download_file in
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

    begin
      try
        match d.download_uploader with
          None -> assert false
        | Some up ->

            let swarmer = CommonSwarming.uploader_swarmer up in

            let old_downloaded = CommonSwarming.downloaded swarmer in

            CommonSwarming.received up !counter_pos b.buf b.pos to_read_int;
            let new_downloaded = CommonSwarming.downloaded swarmer in

            c.client_total_downloaded <- c.client_total_downloaded ++ (new_downloaded -- old_downloaded);
            c.client_session_downloaded <- c.client_session_downloaded ++ (new_downloaded -- old_downloaded);
            client_must_update (as_client c);

            if new_downloaded = file_size file then
              download_finished file;

      with exn ->
        lprintf_nl ~exn "CommonSwarming.received"
  end;
  c.client_failed_attempts <- 0;
  c.client_reconnect <- true;
(*          List.iter (fun (_,_,r) ->
              CommonSwarming.alloc_range r) d.download_ranges; *)

  (match d.download_ranges with
      [] -> lprintf "EMPTY Ranges !!!\n"
    | r :: _ ->
        ()
  );
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
          if !verbose then lprintf_nl "RANGE DOWNLOADED";
          close sock Closed_by_user
    end

let download_on_port c d (x,y) ip port =
  if !verbose then 
    lprintf_nl "download_on_port: %Ld %Ld %s %d" x y (Ip.to_string ip) port;

  let file = d.download_file in
  set_client_state c (Connected_downloading (file_num file));

  let token = create_token unlimited_connection_manager in
  let sock = TcpBufferedSocket.connect token "filetp passive"
      (Ip.to_inet_addr ip) port (fun _ e -> ())
  in
  TcpBufferedSocket.set_reader sock (range_reader c d (ref x) y);
  init_client c sock; 
  set_rtimeout sock 15.;
  TcpBufferedSocket.set_closer sock (fun _ _ ->
        disconnect_client c Closed_by_user;
  )

let write_reqs sock reqs =

  let buf = Buffer.create 100 in
  List.iter (fun s -> 
    if !verbose then lprintf_nl "write_reqs: [%s]" s;
    Printf.bprintf buf "%s\r\n" s
  ) reqs;
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

let get_path_components s =
  match List.rev (List.map (Url.decode ~raw:true) (String2.split s '/')) with
  | file::path -> String.concat "/" (List.rev path), file
  | [] -> ("","") (* empty path *)

let ftp_send_range_request c (x,y) sock d =

  if !verbose then lprintf_nl "Asking range %Ld-%Ld" x y;
  let file = d.download_url.Url.full_file in
  TcpBufferedSocket.set_reader sock (fun sock nread ->
      let b = TcpBufferedSocket.buf sock in
      if !verbose then 
        AnyEndian.dump_hex (Bytes.to_string (Bytes.sub b.buf b.pos b.len));
      let rec iter i =
        if i < b.len then
          if (Bytes.get b.buf (b.pos + i)) = '\n' then begin
              let slen = if i > 0 && (Bytes.get b.buf (b.pos + i - 1)) = '\r' 
                then i - 1
                else i 
              in
              let line = Bytes.sub b.buf b.pos slen in
              let line = Bytes.to_string line in
              if !verbose then lprintf_nl "SRR LINE [%s]" line;
              buf_used b (i+1);
              if slen > 3 then begin
                match (String.sub line 0 4) with
                | "220-" -> iter 0
                | "220 " ->
                  let reqs = [Printf.sprintf "USER %s" (get_user d.download_url)] in
                  write_reqs sock reqs;
                | "331 " -> 
                  let reqs = [Printf.sprintf "PASS %s" (get_pass d.download_url)] in
                  write_reqs sock reqs;
                | "230 " ->
                  let reqs = ["SYST"] in
                  write_reqs sock reqs;
                | "215 " ->
                  let sys = String.sub line 4 (slen - 4) in
                  c.client_software <- sys;
                  if c.client_failed_attempts < 15 then
                    c.client_reconnect <- true;
                  client_must_update (as_client c);
                  let reqs = ["PWD"] in
                  write_reqs sock reqs;
                | "250 " -> 
                  let reqs = ["PASV"] in
                  write_reqs sock reqs;
                | "257 " ->
                  let reqs = ["TYPE I"] in
                  write_reqs sock reqs;
                | "200 " -> 
                  let reqs =
                    match get_path_components file with
                    | ("",_) -> (* no CWD needed *) ["PASV"]
                    | (dir,_) -> [Printf.sprintf "CWD %s" dir]
                  in
                  (* FIXME should really issue several CWDs (one for each directory)
                     TODO implement state for protocol *)
                  write_reqs sock reqs;
                | "227 " ->
                  (try
                  let pos = String.index line '(' in
                  let line = String.sub line (pos+1) (slen - pos - 1) in
                  let pos = String.index line ')' in
                  let line = String.sub line 0 pos in
                    (match List.map int_of_string (String2.split_simplify line ',') with
                    | [a0;a1;a2;a3;p0;p1] ->
                      let ip = Ip.of_string (Printf.sprintf "%d.%d.%d.%d" a0 a1 a2 a3) in
                      let port = (p0 lsl 8) lor p1 in
                      set_rtimeout sock 3600.;
                      download_on_port c d (x,y) ip port;
                      let reqs = [Printf.sprintf "REST %Ld" x] in
                      write_reqs sock reqs; 
                  | _ ->
                      lprintf_nl "Cannot read ip address [%s]\n" line;
                      close sock Closed_by_user)
                with exn ->
                    lprintf_nl ~exn "reader";
                    close sock Closed_by_user)
                | "350 " ->
                    let reqs = [Printf.sprintf "RETR %s" (snd (get_path_components file))] in
                    write_reqs sock reqs; 
                | "150 " ->
                    if !verbose then begin
                      lprintf_nl "should initiate connection!";
                      lprintf_nl "%s" line;
                    end;
                | "426 " ->
                    if !verbose then lprintf_nl "ASK ANOTHER CHUNK";
                  set_rtimeout sock 15.;
                  (try get_from_client sock c with _ -> ());
                | "530 " ->
                    let reason = String.sub line 4 (slen - 4) in
                    if not (retry_530 reason) then begin
                      pause_for_cause d.download_file "530" (CommonUserDb.admin_user ());
                    end else begin
                      c.client_reconnect <- true;
                    end;
                    disconnect_client c Closed_by_user;
                | "550 " ->
                    pause_for_cause d.download_file "550" (CommonUserDb.admin_user ());
                    disconnect_client c Closed_by_user;
                | _ -> 
                    if !verbose then lprintf_nl "Unexpected line %s" line;
                    iter 0;
                  end else iter 0 
            end else
            iter (i+1)
      in
      iter 0
  );
  set_rtimeout sock 30.;
  TcpBufferedSocket.set_closer sock (fun _ _ ->
    if !verbose then lprintf_nl "DISCONNECTED";
  );
  ()


  (*

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

*)

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let ftp_set_sock_handler c sock =
  if !verbose then begin
    let ip = 
      try 
        Ip.to_string (peer_ip sock)
      with _ -> "Unknown"
    in
    lprintf_nl "ftp_set_sock_handler %s" ip;
  end 
(*
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
*)
 (*  set_fileTP_sock sock (HttpHeader (client_parse_header c)) *)

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let ftp_check_size file url start_download_file =

(*
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
      Printf.sprintf "CWD %s" (Filename.dirname url.Url.full_file);
(* 250 *)
      Printf.sprintf "SIZE %s" (Filename.basename url.Url.full_file);
(* 213 size *)
    ]
  in

*)

  let (dirname,basename) = get_path_components url.Url.full_file in
  let server, port = url.Url.server, url.Url.port in
(*    lprintf "async_ip ...\n"; *)
  Ip.async_ip server (fun ip ->
(*        lprintf "IP done %s:%d\n" (Ip.to_string ip) port; *)
      let token = create_token unlimited_connection_manager in
      let sock = TcpBufferedSocket.connect token "ftp client check size"
          (Ip.to_inet_addr ip) port (fun _ e -> ())
      in
(*      write_reqs sock reqs; *)
      TcpBufferedSocket.set_reader sock (fun sock nread ->
          let b = TcpBufferedSocket.buf sock in
          if !verbose then
             AnyEndian.dump_hex (String.sub (Bytes.to_string b.buf) b.pos b.len);
          let rec iter i =
            if i < b.len then
              if (Bytes.get b.buf (b.pos + i)) = '\n' then begin
                  let slen = if i > 0 && (Bytes.get b.buf (b.pos + i - 1)) = '\r' 
                    then i - 1
                    else i 
                  in

                  let line = String.sub (Bytes.to_string b.buf) b.pos slen in
                  if !verbose then lprintf_nl "CS LINE [%s]" line;
                  buf_used b (i+1);
                  if slen > 3 then begin
                    match (String.sub line 0 4) with
                    | "220-" -> iter 0
                    | "220 " ->
                      let reqs = [Printf.sprintf "USER %s" (get_user url)] in
                      write_reqs sock reqs;
                    | "331 " -> 
                      let reqs = [Printf.sprintf "PASS %s" (get_pass url)] in
                      write_reqs sock reqs;
                    | "230 " ->
                      let reqs = ["SYST"] in
                      write_reqs sock reqs;
                    | "215 " ->
                      let reqs = ["PWD"] in
                      write_reqs sock reqs;
                    | "257 " ->
                      let reqs = ["TYPE I"] in
                      write_reqs sock reqs;
                    | "200 " ->
                      let reqs =
                        match dirname with
                        | "" -> (* no CWD needed *) [Printf.sprintf "SIZE %s" basename]
                        | dir -> [Printf.sprintf "CWD %s" dir]
                      in
                      write_reqs sock reqs;
                    | "250 " -> 
                      let reqs = [Printf.sprintf "SIZE %s" basename] in
                      write_reqs sock reqs;
                    | "213 " ->
                      let result_size =
                        Int64.of_string (String.sub line 4 (slen - 4))
                      in
                      if !verbose then lprintf_nl "FOUND SIZE %s: [%Ld]" basename result_size;
                      close sock Closed_by_user;  (* Disconnect so connect_client connects *)
                      start_download_file result_size;
                    | "530 " ->
                      let reason = String.sub line 4 (slen - 4) in
                      if not (retry_530 reason) then begin
                        pause_for_cause file "530" (CommonUserDb.admin_user ());
                      end;
                      close sock Closed_by_user;
                    | "550 " ->
                      pause_for_cause file "550" (CommonUserDb.admin_user ());
                      close sock Closed_by_user;
                    | _ -> 
                      if !verbose then lprintf_nl "Unexpected line %s" line;
                      iter 0;
                  end else iter 0 
              end
            else
                iter (i+1)
          in
          iter 0
      );
      set_rtimeout sock 30.;
      TcpBufferedSocket.set_closer sock (fun _ _ -> ()
(*        lprintf "Connection closed nread:%b\n" !nread; *)
      )
  ) (fun _ -> ());
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
            if (c.client_failed_attempts < 15) then
              c.client_reconnect <- true;
            c.client_failed_attempts <- c.client_failed_attempts + 1;
            disconnect_client c Closed_for_timeout
        | BASIC_EVENT (CLOSED s) ->
            disconnect_client c s
        | CONNECTED ->
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
