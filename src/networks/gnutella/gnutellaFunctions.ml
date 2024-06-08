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


open Options
open Printf2

open BasicSocket
open LittleEndian
open TcpBufferedSocket

open GnutellaNetwork
open GnutellaTypes
open GnutellaOptions

open CommonOptions

type log_message = 
  LogReceive of Ip.t * int * string  
  
let log =
  let log_file = ref None in
  fun t ->
    match t with
      LogReceive(ip, port, s) ->
        
        if !!incoming_data_log <> "" then
          let oc = match !log_file with
              None -> 
                let o = open_out !!incoming_data_log in
                log_file := Some o;
                o
            | Some o -> o
          in
          let s = 
            let b = Buffer.create 100 in
            LittleEndian.buf_ip b ip;
            buf_int16 b port;
            buf_int b (last_time ());
            buf_int b (String.length s);
            Buffer.add_string b s;
            Buffer.contents b
          in
          output_string oc s;
          flush oc
          
let find_header_end buf pos len =
  let end_pos = pos + len in
  let begin_pos =  pos in
  let rec iter i n_read =
    if i < end_pos then
      if buf.[i] = '\r' then
        iter (i+1) n_read
      else
      if buf.[i] = '\n' then
        if n_read then i
        else
          iter (i+1) true
      else
        iter (i+1) false
    else 0 (* 0 means not found... *)
  in
  iter begin_pos false  

let default_handler_hook = ref (None : 
    (gconn -> TcpBufferedSocket.t -> unit) option)
  
let default_handler gconn sock =
  match !default_handler_hook with
    None ->
      let b = buf sock in
      if !verbose then
        lprintf "HttpReader: Handler not found for [%s]\n"
          (Bytes.unsafe_to_string (Bytes.escaped (Bytes.sub b.buf b.pos b.len)));
      close sock (Closed_for_error "not recognized");
      failwith "Reply is not in the correct protocol"
  | Some f -> f gconn sock

let lprint_http_header line headers =
  lprintf "     %s\n" line;
  List.iter (fun (header, (value,_)) ->
      lprintf "   %s = %s\n" header value;
  ) headers;
  lprintf "\n\n"
  
let handlers info gconn =
  let rec iter_read sock nread =
    let b = TcpBufferedSocket.buf sock in 
    if monitored sock || !verbose_msg_raw then
      lprintf "iter_read %s :%d/%d\n%s\n" (Ip.to_string (peer_ip sock)) nread b.len
        (Bytes.unsafe_to_string (Bytes.escaped (Bytes.sub b.buf b.pos b.len)));
    if b.len > 0 then
      match gconn.gconn_handler with
      | HttpReader (n, hs, default) ->
          if b.len >= n then
            let head = Bytes.sub b.buf b.pos n in
            let head = Bytes.to_string head in
            (try                 
                let rec iter hs =
                  match hs with
                    []  -> 
                      let len = b.len in
                      (try 
                          default gconn sock 
                        with e ->
                            if !verbose then
                              lprintf "HttpReader: default handler raised %s\n"
                                (Printexc2.to_string e);
                            close sock (Closed_for_exception e));
                      if b.len > 0 && b.len < len then 
                        let nused = len - b.len in
                        let old_bytes = len - nread in
                        let new_nused = nused - old_bytes in
                        let nread = nread - new_nused in
                        if monitored sock || !verbose_msg_raw
                        then lprintf "   iter...\n";
                        iter_read sock nread
                  
                  | (proto, h) :: tail ->
                      if String2.starts_with head proto then begin
                          let i = find_header_end (Bytes.to_string b.buf) b.pos b.len in
                          if i <> 0 then
                            let header = Bytes.to_string (Bytes.sub b.buf b.pos (i - b.pos)) in
                            let first_line, headers =        
                              match  Http_client.split_header header  with
                                [] -> "", []
                              | line :: headers ->
                                  let headers = Http_client.cut_headers headers in
                                  if !verbose then begin
                                      lprintf "RECEIVED HEADER:\n";
                                      lprint_http_header line headers;
                                    end; 
                                  line, headers          
                            in
                            (try
                                h gconn sock (first_line, headers);
                              with e ->
                                if !verbose then
                                  lprintf "HttpReader: handler raised %s\n"
                                    (Printexc2.to_string e);
                                  close sock (Closed_for_exception e));
                            if not (TcpBufferedSocket.closed sock) then begin
                                let nused = i - b.pos + 1 in
(*                        lprintf "HEADER: buf_used %d\n" nused; *)
                                buf_used b nused;
                                if monitored sock  || !verbose_msg_raw then
                                  lprintf "   iter...\n";
                                if b.len > 0 then
                                  iter_read sock 0;
                              end
                        end
                      else iter tail
                in
(*                lprintf "HttpReader: finding handler\n";*)
                
                iter hs
              
              with
              | e -> 
                  lprintf "Exception %s in HttpReader\n"
                    (Printexc2.to_string e);
                  close sock 
                    (BasicSocket.Closed_for_exception e));
      
      | Reader h -> 
          let len = b.len in
          (try
              h gconn sock;
            with e ->
                if !verbose then
                  lprintf "Reader: handler raised %s\n"
                    (Printexc2.to_string e);
                close sock (Closed_for_exception e));
          if b.len > 0 && b.len < len then 
            let nused = len - b.len in
            let old_bytes = len - nread in
            let new_nused = nused - old_bytes in
            let nread = nread - new_nused in
            if monitored sock || !verbose_msg_raw then lprintf "   iter...\n";
            iter_read sock nread            
      
      | CipherReader (cipher, h) ->
          if monitored sock || !verbose_msg_raw then
            lprintf "CipherReader %d: [%s]\n" nread
              (Bytes.unsafe_to_string (Bytes.escaped (Bytes.sub b.buf b.pos b.len)));
          if nread > 0 then begin
(*              AnyEndian.dump_sub b.buf (b.pos + b.len - nread) nread; *)
              apply_cipher cipher b.buf (b.pos + b.len - nread) nread;
              
              log (LogReceive(peer_ip sock, peer_port sock,
                  String.sub  (Bytes.to_string b.buf) (b.pos + b.len - nread) nread));
              
              if monitored sock || !verbose_msg_raw then
                lprintf "   deciphered: [%s]\n"
                  (Bytes.unsafe_to_string (Bytes.escaped (Bytes.sub b.buf b.pos b.len)));
            end;
          let len = b.len in
          (try
              h gconn sock;
            with e ->
                lprintf "CipherReader: handler raised %s\n"
                  (Printexc2.to_string e);
                close sock (Closed_for_exception e));
          
          if monitored sock then
            lprintf "RESULT: b.len %d len %d\n" b.len len;
          if b.len > 0 && b.len < len then begin
              if monitored sock || !verbose_msg_raw then lprintf "   iter...\n";

(* Keep nread = 0 because we don't want to decipher it again !!! *)
              iter_read sock 0
            end
            
  in
  iter_read
