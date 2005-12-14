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
open AnyEndian
open Printf2
open Options
open Md4
open TcpBufferedSocket

open Xml_types
open CommonGlobals
open CommonTypes
open CommonOptions
open CommonHosts

open G2Network
open G2Options
open G2Types
open G2Protocol
open G2Globals
open G2Proto
  
type t = UDP | TCP

type cnx = {
    ip1 : string;
    port1 : int;
    ip2 : string;
    port2 : int;
    mutable buf : string list;
  }
let connections = Hashtbl.create 13

let new_packet t (n : int) ip1 port1 ip2 port2 s =
  match t with
    TCP -> 
      let cnx = 
        try
          Hashtbl.find connections n
        with _ ->
            let cnx = {
                ip1 = ip1;
                port1 = port1;
                ip2 = ip2;
                port2 = port2;
                buf = [];
              } in
            Hashtbl.add connections n  cnx;
            cnx
      in
      cnx.buf <- cnx.buf @ [ s ]
  
  | UDP -> try
        let p = parse_udp_packet (Ip.of_string ip1) port1 s in
        lprintf "\nPACKET %s:%d -> %s:%d\n%s\n\n" ip1 port1 ip2 port2
          (Print.print p)
      with e ->
(* lprintf "Could not parse: %s\n" (Printexc2.to_string e) *) ()

let hescaped s =
  String2.replace_char s '\r' ' ';s

let rec iter s pos =
  if pos < String.length s then
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
  else pos

let parse_string s =
(*      lprintf "Parse string:\n %s\n" (String.escaped s); *)
  let rec iter pos =
(*          lprintf "iter %d\n" pos; *)
    if pos < String.length s then
      try
        let p, decoded, pos = parse s pos in

(*
          lprintf "decoded:\n";
          dump decoded;
*)
        
        (try
            let encoded = g2_encode p in
(*
              lprintf "encoded:\n";
dump encoded;
  *)
            let pp, _, _ = parse encoded 0 in
            
            if encoded <> decoded then begin
                lprintf "ENCODER / DECODER ERROR:\n";
                lprintf "CORRECT:\n";
                dump decoded;
                lprintf "INCORRECT:\n";
                dump encoded;
                lprintf "______________________\n";
              end;
            assert (pp = p)
          with e ->
              lprintf "Exception %s in Encoding\n" 
                (Printexc2.to_string e));
        
        lprintf "Packet: \n%s\n" (Print.print p);
        iter pos
      with Not_found -> 
          String.sub s pos (String.length s - pos)
    else ""
  in
  iter 0

let piter s1 deflate h msgs = 
  let len = String.length s1 in
  try
    if len > 0 then
      if deflate then
        let z = Zlib.inflate_init true in
        let _ =  
          let s2 = String.make 100000 '\000' in
          let f = Zlib.Z_SYNC_FLUSH in
          let (_,used_in, used_out) = Zlib.inflate z s1 0 len s2 0 100000 f in
          lprintf "decompressed %d/%d\n" used_out len;
          String.sub s2 0 used_out
        in
        begin
(* First of all, deflate in one pass *)
          try
(*                lprintf "PARSE ONE BEGIN...\n%s\n" (String.escaped s1); *)
            let z = Zlib.inflate_init true in
            let s =  
              let s2 = String.make 1000000 '\000' in
              let f = Zlib.Z_SYNC_FLUSH in
              let len = String.length s1 in
              let (_,used_in, used_out) = Zlib.inflate z s1 0 len s2
                  0 1000000 f in
              String.sub s2 0 used_out
            in
            ignore (parse_string s);
(*                lprintf "...PARSE ONE END\n"; *)
          with e ->
              lprintf "Exception %s in deflate1\n" (Printexc2.to_string e)
        end;
        let z = Zlib.inflate_init true in
        let rec iter list offset rem buf =
          match list with
            [] -> ()
          | m :: tail ->
              let len = String.length m in
              if len <= offset then iter tail (offset - len) rem buf else
              let m = if offset > 0 then String.sub m offset (len - offset) else m in
              let rem = rem ^ m in
              let len = String.length rem in
              let s2 = String.make 100000 '\000' in
              let f = Zlib.Z_SYNC_FLUSH in
(*                  lprintf "deflating %d bytes\n" len; *)
              let (_,used_in, used_out) = Zlib.inflate z rem 0 len s2 0 100000 f in
(*                  lprintf "decompressed %d/%d[%d]\n" used_out len used_in; *)
              let m = buf ^ (String.sub s2 0 used_out) in
              
              let buf =
                try
                  parse_string m
                with
                  e ->
                    lprintf "Exception %s while parse_string\n"
                      (Printexc2.to_string e);
                    ""
              in
              
              let rem = 
                if used_in < len then String.sub rem used_in len else "" in
              iter tail 0 rem buf
        in
        iter msgs h "" ""
      else
        ignore (parse_string s1)
  with e ->
      lprintf "Exception %s while deflating \n O\nCUT:%s\n"
        (Printexc2.to_string e) (String.escaped s1)

let commit () =  
  Hashtbl.iter (fun _ cnx ->
      let buf = Buffer.create 1000 in
      List.iter (fun b ->
          Buffer.add_string buf b;
      ) cnx.buf;
      let s = Buffer.contents buf in
      let len = String.length s in
      lprintf "\n-----------------------------------------------\n";
      lprintf "\nCONNECTION %s:%d -> %s:%d: %d bytes\n" 
        cnx.ip1 cnx.port1 cnx.ip2 cnx.port2 len;
      begin
        try
          if String2.starts_with s "GNUTELLA CONNECT" then begin
              let h1 = iter s 0 in
              let h2 = iter s h1 in
              let s1 = (String.sub s h2 (len-h2)) in
              let s2 = (String.sub s h1 (h2-h1)) in
              lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
              lprintf "Header 2: \n%s\n" (hescaped s2);
              let deflate = try ignore (String2.search_from s2 0 "deflate");
                  lprintf "deflate\n"; true with _ -> false in
              piter s1 deflate h2 cnx.buf
            end else 
          if String2.starts_with s "GNUTELLA" then begin
              let h1 = iter s 0 in
              let s1 = (String.sub s h1 (len-h1)) in
              let s2 = (String.sub s 0 h1) in
              lprintf "Header 1: \n%s\n" (hescaped s2);
              let deflate = try ignore (String2.search_from s2 0 "deflate");
                  lprintf "deflate\n"; true with _ -> false in
              piter s1 deflate h1 cnx.buf;
            end else 
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
            lprintf "Starting [%s]\n" (String.escaped
                (String.sub s 0 (min len 100)))
        with 
        | e ->
            lprintf "Exception %s in\n%s\n" (Printexc2.to_string e)
            (String.escaped s);
            ()            
      end;
      lprintf "\nEND OF CONNECTION\n---------------------------------------\n";
  ) connections;
  
  