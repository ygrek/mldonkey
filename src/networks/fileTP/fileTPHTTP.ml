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

open BasicSocket
open TcpBufferedSocket

open CommonOptions
open CommonClient
open CommonTypes
open CommonGlobals

open FileTPTypes
open FileTPGlobals
open FileTPProtocol

open FileTPClients

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let http_send_range_request c range sock d =
  let url = d.download_url in

  let real_url =
    (Str.global_replace (Str.regexp " ") "%20" url.Url.full_file) in

  let (x,y) = range in
  let range = Printf.sprintf "%Ld-%Ld" x (Int64.pred y) in

  let buf = Buffer.create 100 in

  Printf.bprintf buf "GET %s HTTP/1.0\r\n" real_url;

(*
            (match d.download_uri with
                FileByUrl url -> Printf.bprintf buf "GET %s HTTP/1.0\r\n" url
              | FileByIndex (index, name) ->
                  Printf.bprintf buf "GET /get/%d/%s HTTP/1.1\r\n" index
                    name); *)
  Printf.bprintf buf "Host: %s\r\n" c.client_hostname;
  Printf.bprintf buf "User-Agent: %s\r\n" (get_user_agent ());
  Printf.bprintf buf "Referer: %s\r\n" c.client_referer;
  Printf.bprintf buf "Range: bytes=%s\r\n" range;
  Printf.bprintf buf "Connection: Keep-Alive\r\n";
  if url.Url.user <> "" then begin
    let userpass = Printf.sprintf "%s:%s" url.Url.user url.Url.passwd in
    let encoded = Base64.encode_to_string userpass in
    Printf.bprintf buf "Authorization: Basic %s\r\n" encoded
  end;
  Printf.bprintf buf "\r\n";
  let s = Buffer.contents buf in
  if !verbose_msg_clients then
    lprintf_nl "SENDING REQUEST to %s: %s"
      c.client_hostname
      (String.escaped s);
  write_string sock s;
  c.client_requests <- c.client_requests @ [d]

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)


let parse_line header = 
  let endline_pos = String.index header '\n' in
  let http, code =
    match String2.split (String.sub header 0 endline_pos) ' ' with
    | http :: code :: ok :: _ ->
      let code = int_of_string code in
      if not (String2.starts_with (String.lowercase http) "http") 
        then failwith "Not in http protocol"; 
      http, code
    | _ -> 
      failwith "Not a HTTP header line"
    in
  http, code



let rec client_parse_header c gconn sock header =
  if !verbose_msg_clients then
    lprintf_nl "CLIENT PARSE HEADER";
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
        lprintf_nl "HEADER FROM CLIENT:";
        AnyEndian.dump_ascii header;
      end;
    let file = d.download_file in
    let size = file_size file in

    let http, code = parse_line header in
    if !verbose_msg_clients then
      lprintf_nl "GOOD HEADER FROM CONNECTED CLIENT\n";

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
            lprintf_n "Download Header contains unknown fields";
            lprintf_nl "    %s" first_line;
            List.iter (fun (header, (value,header2)) ->
                lprintf_nl "    [%s] = [%s](%s)" header value header2;
            ) headers;
            lprintf_nl "end of header";
          end;
      end;

    if code < 200 || code > 299 then begin
      pause_for_cause file (Printf.sprintf "%d" code) (CommonUserDb.admin_user ());
      failwith "Bad HTTP code";
    end;

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
            x, Int64.succ y (* "bytes x-y/*" *)
          else
          let z = Int64.of_string (
              String.sub range (slash_pos+1) (len - slash_pos -1) )
          in
          if y = z then Int64.pred x, size else
            x, Int64.succ y
        with
        | e ->
            lprintf_nl "Exception %s for range [%s]"
              (Printexc2.to_string e) range;
            raise e
      with e ->
          try
            if code <> 206 && code <> 200 then raise Not_found;
            let (len,_) = List.assoc "content-length" headers in
            let len = Int64.of_string len in
            if !verbose then lprintf_nl "Specified length: %Ld" len;
            match d.download_ranges with
              [] -> raise Not_found
            | (start_pos,end_pos,r) :: _ ->
                lprintf_nl "WARNING: Assuming client is replying to range";
                if len <> end_pos -- start_pos then
                  begin
                    lprintf_nl "ERROR: bad computed range: %Ld-%Ld/%Ld \n%s"
                      start_pos end_pos len
                      (String.escaped header);
                    raise Not_found
                  end;
                (start_pos, end_pos)
          with _ ->
(* A bit dangerous, no ??? *)
              if !verbose_unknown_messages then
                lprintf_nl "ERROR: Could not find/parse range header (exception %s), disconnect\nHEADER: %s"
                    (Printexc2.to_string e)
                    (String.escaped header);
              disconnect_client c (Closed_for_error "Bad HTTP Range");
              raise Exit
    in
    (try
        let (len,_) = List.assoc "content-length" headers in
        let len = Int64.of_string len in
        if len <> end_pos -- start_pos then
          begin
            failwith (Printf.sprintf "ERROR: bad computed range: %Ld-%Ld/%Ld \n%s\n"
              start_pos end_pos len (String.escaped header))
          end
      with _ ->
          lprintf_nl "[WARNING]: no Content-Length field\n%s\n"
            (String.escaped header)
    );

    (try
        let (server,_) = List.assoc "server" headers in
        c.client_software <- server;
        client_must_update (as_client c);
      with _ -> ()
    );

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

        let to_read_int = Int64.to_int to_read in

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

          with e ->
              lprintf_nl "Exception %s in CommonSwarming.received"
                (Printexc2.to_string e)
        end;

        c.client_reconnect <- true;
(*          List.iter (fun (_,_,r) ->
              CommonSwarming.alloc_range r) d.download_ranges; *)

        (match d.download_ranges with
            [] -> lprintf_nl "EMPTY Ranges!"
          | r :: _ ->
(*
              let (x,y) = CommonSwarming.range_range r in
              if !verbose then lprintf_nl "Received %Ld [%Ld] (%Ld-%Ld) -> %Ld"
                !counter_pos to_read
                x y
                (new_downloaded -- old_downloaded)
*)
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
(*
                lprintf "Ready for next chunk (version %s)\nHEADER:%s\n" http
                  (String.escaped header);
                *)
(*                CommonSwarming.free_range r; *)
                d.download_ranges <- tail;
(* If we have no more range to receive, disconnect *)
                if d.download_ranges = [] then
                  raise Exit;
                gconn.gconn_handler <- HttpHeader (client_parse_header c);
          end)

  with e ->
      if !verbose_unknown_messages then
        begin
          lprintf_nl "Exception %s in client_parse_header" (Printexc2.to_string e);
          AnyEndian.dump header
        end;
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

let http_check_size file url start_download_file =
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_request = H.HEAD;
      H.req_user_agent = get_user_agent ();
    } in

  H.whead2 r (fun headers ->
      let content_length = ref None in
      List.iter (fun (name, content) ->
          if String.lowercase name = "content-length" then
        try content_length := Some (Int64.of_string content)
        with _ -> lprintf_nl "bad content length [%s]" content;
      ) headers;
    (match !content_length with
        None -> failwith "Unable to start download (HEAD failed)"
    | Some result_size -> start_download_file result_size);
  )
  (fun e -> pause_for_cause file (H.show_error e) (CommonUserDb.admin_user ()))

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
