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

open CommonSwarming
open Printf2
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
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

open Gnutella1

let redirector_to_client p sock = 
(*  lprintf "redirector_to_client\n"; *)
  match p.pkt_payload with
    PongReq t ->
      let module P = Pong in
(*      lprintf "ADDING PEER %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      Fifo.put peers_queue (t.P.ip, t.P.port);
  | _ -> ()

let redirector_parse_header gconn sock header = 
(*  lprintf "redirector_parse_header\n";*)
  if String2.starts_with header gnutella_ok then begin
(*      lprintf "GOOD HEADER FROM REDIRECTOR:waiting for pongs";*)
      sock_send_new sock (
        let module P = Ping in
        PingReq (P.ComplexPing {
            P.ip = DO.client_ip (Some sock);
            P.port = !!client_port;
            P.nfiles = Int64.zero;
            P.nkb = Int64.zero;
            P.s = "none:128:false";
          }));
      gconn.gconn_handler <- Reader (gnutella_handler 
          parse redirector_to_client)
    end else begin
      if !verbose_msg_servers then begin
          lprintf "BAD HEADER FROM REDIRECTOR: \n";
          AnyEndian.dump header;
        end;
      close sock "bad header";
      redirector_connected := false;
      raise Not_found
    end

let connect () =  
  match !redirectors_to_try with
    [] ->
      redirectors_to_try := !!redirectors
  | name :: tail ->
      redirectors_to_try := tail;
(*      lprintf "connect to redirector\n"; *)
      Ip.async_ip name (fun ip ->
          try
            let sock = connect  "limewire to redirector"
                (Ip.to_inet_addr ip) 6346
                (fun sock event -> 
                  match event with
                    BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
                      close sock "timeout";
                      redirector_connected := false;
(*                  lprintf "TIMEOUT FROM REDIRECTOR\n"*)
                  | _ -> ()
              ) in
            TcpBufferedSocket.set_read_controler sock download_control;
            TcpBufferedSocket.set_write_controler sock upload_control;
            
            redirector_connected := true;
            set_gnutella_sock sock !verbose_msg_servers
              (HttpHeader redirector_parse_header);
            set_closer sock (fun _ _ -> 
(*            lprintf "redirector disconnected\n"; *)
                redirector_connected := false);
            set_rtimeout sock 10.;
            set_lifetime sock 120.;
            write_string sock "GNUTELLA CONNECT/0.4\n\n";
          with e ->
              lprintf "Exception in connect_to_redirector: %s\n"
                (Printexc2.to_string e); 
              redirector_connected := false
      )
        
