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
open Options
open Md4
open AnyEndian
open LittleEndian
open TcpBufferedSocket
open Xml_types
  
open CommonHosts
open CommonTypes
open CommonOptions
open CommonGlobals

open GnutellaNetwork
open GnutellaGlobals
open GnutellaTypes
open GnutellaOptions
open GnutellaProtocol
open GnutellaProto

type t = UDP | TCP

type cnx = {
    ip1 : string;
    port1 : int;
    ip2 : string;
    port2 : int;
    packets : Buffer.t;
  }
let connections = Hashtbl.create 13

let rec piter s pos = 
  let len = String.length s in
  if String.length s - pos >= 23 then
    let msg_len = get_int s (pos+19) in
    if len >= 23 + msg_len then
      let pkt_uid = get_md4 s pos in
      let pkt_type = match get_uint8 s (pos+16) with
          0 -> PING
        | 1 -> PONG
        | 2 -> BYE
        | 48 -> QRP
        | 49 -> VENDOR
        | 64 -> PUSH
        | 128 -> QUERY
        | 129 -> QUERY_REPLY
        | n -> UNKNOWN n
      in
      let pkt_ttl = get_uint8 s  (pos+17) in
      let pkt_hops = get_uint8 s  (pos+18) in
      let data = String.sub s (pos+23) msg_len in
      let pkt = {
          pkt_uid = pkt_uid;
          pkt_type = pkt_type;
          pkt_ttl = pkt_ttl;
          pkt_hops = pkt_hops;
          pkt_payload = data;
        } in
      begin
        try
          let pkt = parse pkt in
          print pkt;
          if pkt_type = QUERY then
            dump (String.sub s pos (msg_len + 23))
        with e ->
            lprintf "Exception %s\n" (Printexc2.to_string e)
      end;
      piter s (pos + msg_len + 23)
(*
        else 
          lprintf "Remaining %d bytes: %s\n"
            (len - pos) (String.sub s pos (len-pos))
      else 
        lprintf "Remaining %d bytes: %s\n"
          (len - pos) (String.sub s pos (len-pos))
*)

let rec iter s pos =
  if s.[pos] = '\n' then
    if s.[pos+1] = '\n' then pos+2
    else
    if s.[pos+1] = '\r' then
      if s.[pos+2] = '\n' then pos+3
      else iter s (pos+1)
    else iter s (pos+1)
  else 
  if s.[pos] = '\r' then
    if s.[pos] = '\n' then
      if s.[pos+1] = '\n' then pos+2
      else
      if s.[pos+1] = '\r' then
        if s.[pos+2] = '\n' then pos+3
        else iter s (pos+1)
      else iter s (pos+1)
    else
      iter s (pos+1)
  else iter s (pos+1)

let hescaped s =
  String2.replace_char s '\r' ' ';s

let commit () =  
  Hashtbl.iter (fun _ cnx ->
      let s = Buffer.contents cnx.packets in
      let len = String.length s in
      try
        if String2.starts_with s "GNUTELLA CONNECT" then begin
            let h1 = iter s 0 in
            let h2 = iter s h1 in
            lprintf "\n-----------------------------------------------\n";
            lprintf "\nCONNECTION %s:%d -> %s:%d: %d bytes\n" 
              cnx.ip1 cnx.port1 cnx.ip2 cnx.port2 len;
            lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
            lprintf "Header 2: \n%s\n" (hescaped (String.sub s h1 
                  (h2-h1)));              
            piter s h2
          end else 
        if String2.starts_with s "GNUTELLA" then begin
            let h1 = iter s 0 in
            lprintf "\n-----------------------------------------------\n";
            lprintf "\nCONNECTION %s:%d -> %s:%d: %d bytes\n" 
              cnx.ip1 cnx.port1 cnx.ip2 cnx.port2 len;
            
            lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
            piter s h1
          end else ()
(*
            if String2.starts_with s "GET" then begin
                lprintf "Http connect to\n";
                let h1 = iter s 0 in
                lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
              end else 
            if String2.starts_with s "HTTP" then begin
                lprintf "Http connected from\n";
                let h1 = iter s 0 in
                lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
              end else 
              lprintf "Starting %s\n" (String.escaped
(String.sub s 0 (min len 20)))
  *)
      with 
      
      | e ->
(*
              lprintf "Exception %s in\n%s\n" (Printexc2.to_string e)
              (String.escaped s)
*) ()            
  ) connections;
  lprintf "\n\n#use \"limewire.ml\";;\n\n"



let new_packet (kind:t) (number:int) ip1 port1 ip2 port2 data = 
  match kind with
    UDP -> 
      begin
        try
(*              lprintf "New packet:\n%s\n" (String.escaped data);          *)
          piter data 0;
        with e ->
(*                lprintf "Could not parse UDP packet:\n"; *)
            ()
      end
  | TCP ->  
      let cnx = 
        try
          Hashtbl.find connections number
        with _ ->
            let cnx = {
                ip1 = ip1;
                port1 = port1;
                ip2 = ip2;
                port2 = port2;
                packets = Buffer.create 100;
              } in
            Hashtbl.add connections number cnx;
            cnx
      in
      Buffer.add_string cnx.packets data
      
      
