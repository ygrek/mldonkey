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
SENDING REQUEST: GET FastTrack://62.175.4.76:2798/.hash=9c1e0c03f1a38ba838feaf4b8ac0d560b43bc148 HTTP/1.1\013\nHost: 62.175.4.76:2798\013\nUser-Agent: MLDonkey 2.4-1\013\nRange: bytes=0-262143\013\n\013\n
Asking 00000000000000000000000000000000 For Range 0-262143
Disconnected from source
CLIENT PARSE HEADER
HEADER FROM CLIENT:
ascii: [
HTTP/1.0 501 Not Implemented
X-Kazaa-Username: rcb(13)
X-Kazaa-Network: KaZaA(13)
X-Kazaa-IP: 168.226.112.135:1959(13)
X-Kazaa-SupernodeIP: 200.75.229.212:1214(13)
]


ascii:[
HTTP/1.0 503 Service Unavailable
Retry-After: 284(13)
X-Kazaa-Username: johnl(13)
X-Kazaa-Network: KaZaA(13)
X-Kazaa-IP: 62.251.115.29:1457(13)
X-Kazaa-SupernodeIP: 195.169.211.25:3534(13)
]

ascii:[
HTTP/1.1 206 Partial Content(13)
Content-Range: bytes 3145728-3407871/6128453(13)
Content-Length: 262144(13)
Accept-Ranges: bytes(13)
Date: Thu, 15 May 2003 22:28:38 GMT(13)
Server: KazaaClient Nov  3 2002 20:29:03(13)
Connection: close(13)
Last-Modified: Sat, 22 Feb 2003 19:58:52 GMT(13)
X-Kazaa-Username: defaultuser(13)
X-Kazaa-Network: KaZaA(13)
X-Kazaa-IP: 212.8.74.24:1214(13)
X-Kazaa-SupernodeIP: 193.204.34.214:2093(13)
X-KazaaTag: 4=A solas con mi corazon(13)
X-KazaaTag: 6=Rosa(13)
X-KazaaTag: 8=Rosa(13)
X-KazaaTag: 14=Pop(13)
X-KazaaTag: 1=2002(13)
X-KazaaTag: 26=http://www.elitemp3.net(13)
X-KazaaTag: 10=es(13)
X-KazaaTag: 12=1(186) album - 29-04-2002(13)
X-KazaaTag: 5=386(13)
X-KazaaTag: 21=128(13)
X-KazaaTag: 3==qyWzRb1Qvnk4mtaBytIM1iHQuK8=(13)
Content-Type: audio/mpeg(13)
(13)]

HTTP/1.1206 Partial Content(13)
Content-Range: bytes 0-262143/3937679(13)
Content-Length: 262144(13)
Accept-Ranges: bytes(13)
Date: Thu, 15 May 2003 22:18:12 GMT(13)
Server: KazaaClient Nov  3 2002 20:29:03(13)
Connection: close(13)
Last-Modified: Mon, 05 May 2003 04:14:57 GMT(13)
X-Kazaa-Username: shaz2003(13)
X-Kazaa-Network: KaZaA(13)
X-Kazaa-IP: 81.103.29.119:3641(13)
X-Kazaa-SupernodeIP: 131.111.202.241:2674(13)
X-KazaaTag: 5=246(13)
X-KazaaTag: 21=128(13)
X-KazaaTag: 4=Fighter(13)
X-KazaaTag: 6=Christina Aguliera(13)
X-KazaaTag: 8=Stripped(13)
X-KazaaTag: 14=Other(13)
X-KazaaTag: 1=2002(13)
X-KazaaTag: 26=© christinas_eyedol 2002(13)
X-KazaaTag: 12=album version, stripped, fighter, real, christina, aguilera(13)
X-KazaaTag: 10=en(13)
X-KazaaTag: 18=Video Clip(13)
X-KazaaTag: 28=div3(13)
X-KazaaTag: 17=24(13)
X-KazaaTag: 9=241229701(13)
X-KazaaTag: 24=http://www.MusicInter.com(13)
X-KazaaTag: 3==kd8c6QgrXm0wvCYl5Uo0Aa9C7qg=(13)
Content-Type: audio/mpeg(13)
\n

  
*)

open Options
open Queues
open Printf2
open Md4
open BasicSocket
open TcpBufferedSocket

open AnyEndian
  
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonSwarming
open CommonTypes
open CommonGlobals
  
open FasttrackTypes
open FasttrackGlobals
open FasttrackOptions
open FasttrackProtocol
open FasttrackComplexOptions
open FasttrackProto

type t = UDP | TCP

type cnx = {
    ip1 : string;
    port1 : int;
    ip2 : string;
    port2 : int;
    packets_in : Buffer.t;
    packets_out : Buffer.t;
  }
let connections = Hashtbl.create 13

let parse_packet msg_type m =
  match msg_type with
    0x00 -> (* SessMsgNodeList *)
      lprintf "SessMsgNodeList\n";
      
      let n = String.length m / 8 in
      for i = 0 to n - 1 do
        let l_ip = LittleEndian.get_ip m (i*8) in
        let l_port = BigEndian.get_int16 m (i*8+4) in
        let unknown = BigEndian.get_int16 m (i*8+6) in
        
        lprintf "    LittleEndian Node %s:%d   %d\n" (Ip.to_string l_ip) 
        l_port unknown;
      
      done;
  
  | 0x06 -> (* SessMsgQuery *)
      lprintf "SessMsgQuery (len %d)\n" (String.length m);
      dump m
  
  | 0x07 -> (* SessMsgQueryReply *)
      lprintf "SessMsgQueryReply\n";

(* probably supernode address *)
      let s_ip = LittleEndian.get_ip m 0 in
      let s_port = BigEndian.get_int16 m 4 in
      
      let id = BigEndian.get_int16 m 6 in
      
      let nresults = BigEndian.get_int16 m 8 in
      lprintf "Results: %d for Search %d\n" nresults id;
      
      let len = String.length m in
      let rec iter pos n = 
        if n > 0 && pos + 32 < len then
          let user_ip = LittleEndian.get_ip m pos in
          let user_port = BigEndian.get_int16 m (pos+4) in
          let user_bandwidth = get_uint8 m (pos+6) in
          let pos = pos + 7 in
          let user_name, user_netname, pos =
            if get_uint8 m pos = 2 then
              "unknown", "unknown", pos+1
            else
            let end_name = String.index_from m pos '\001' in
            let end_netname = String.index_from m end_name '\000' in
            String.sub m pos (end_name - pos),
            String.sub m (end_name+1) (end_netname - end_name -1),
            end_netname + 1
          in
          
          lprintf "   User %s@%s %s:%d\n" user_name user_netname
            (Ip.to_string user_ip) user_port;
          
          let result_hash = Md5Ext.direct_of_string (String.sub m pos 20) in
          let checksum, pos = get_dynint m (pos+20) in
          let result_size, pos = get_dynint m pos in
          let ntags, pos = get_dynint m pos in
          let ntags = Int64.to_int ntags in
          
          lprintf "   Result %s size: %Ld tags: %d\n" 
            (Md5Ext.to_string_case false result_hash) result_size ntags;
          lprintf "    %s\n" (Md5Ext.to_hexa result_hash);
          
          
          let rec iter_tags name pos n tags =
            if n > 0 && pos < len-2 then
              let tag, pos = get_dynint m pos in
              let tag = Int64.to_int tag in
              let tag_len, pos = get_dynint m pos in
              let tag_len = Int64.to_int tag_len in
              let tagdata = String.sub m pos tag_len in
              let name = if tag = 2 then tagdata else name in
              iter_tags name (pos + tag_len) (n-1) 
              ((tag, tagdata) :: tags)
            else
              name, tags, pos
          in
          let result_name, tags, pos = iter_tags "Unknown" pos ntags [] in
          List.iter (fun (tag, tagdata) ->
              lprintf "      Tag: %d --> %s\n" tag (String.escaped tagdata);
          ) tags;
(*
          let url = Printf.sprintf 
            "FastTrack://%s:%d/.hash=%s" (Ip.to_string user_ip)
            user_port (Md5Ext.to_string_case false result_hash) in *)
          let url = Printf.sprintf 
              "/.hash=%s" (Md5Ext.to_string_case false result_hash) in 
          lprintf "URL = %s\n" url;
          iter pos (n-1)
      in
      iter 10 nresults
  
  | 0x08 -> (* SessMsgQueryEnd *)
      let s = BigEndian.get_int16 m 0 in
      lprintf "SessMsgQueryEnd for search %d\n" s;
(*
fst_searchlist_process_reply (FST_PLUGIN->searches, msg_type, msg_data);


*)
  
  | 0x09 -> (* SessMsgNetworkStats *)
      lprintf "SessMsgNetworkStats\n";

(*
unsigned int mantissa, exponent;

if(fst_packet_remaining(msg_data) < 12) // 97 bytes total now? was 60?
break;

FST_PLUGIN->stats->users = ntohl(fst_packet_get_uint32 (msg_data));	// number of users	
FST_PLUGIN->stats->files = ntohl(fst_packet_get_uint32 (msg_data));	// number of files

mantissa = ntohs(fst_packet_get_uint16 (msg_data));	// mantissa of size
exponent = ntohs(fst_packet_get_uint16 (msg_data));	// exponent of size

if (exponent >= 30)
FST_PLUGIN->stats->size = mantissa << (exponent-30);
else
FST_PLUGIN->stats->size = mantissa >> (30-exponent);	 

// what follows in the packet is the number of files and their size per media type (6 times)
// we do not currently care for those

// something else with a size of 37 byte follows, dunno what it is

FST_DBG_3 ("received network stats: %d users, %d files, %d GB", FST_PLUGIN->stats->users, FST_PLUGIN->stats->files, FST_PLUGIN->stats->size);
break;


  *)
  
  | 0x1d -> (* SessMsgNetworkName *)
      lprintf "SessMsgNetworkName: %s\n" m;
  
  | _ ->
      lprintf "   ******* Unknown message %d\n" msg_type;
      AnyEndian.dump m



let rec parse_packets pos s ciphers = 
  let len = String.length s - pos in
  if len > 0 then
    match int_of_char s.[pos] with
      0x50 ->
        lprintf "We have got a ping\n"; 
        parse_packets (pos+1) s ciphers
    | 0x52 ->
        lprintf "We have got a pong\n"; 
        parse_packets (pos+1) s ciphers
    | 0x4b ->
(*            lprintf "We have got a real packet\n"; *)
        begin
          if len > 4 then
(*                dump_sub s pos b.len; *)
            let xtype = Int64.to_int (Int64.rem ciphers.in_xinu int64_3) in
            
            let msg_type, size = 
              match xtype with
                0 ->
                  let msg_type = get_uint8 s (pos+1) in
(* zero *)
                  let len_hi = get_uint8 s (pos+3) in
                  let len_lo = get_uint8 s (pos+4) in
                  msg_type, (len_hi lsl 8) lor len_lo
              | 1 ->
(* zero *)
                  let len_hi = get_uint8 s (pos+2) in
                  let msg_type = get_uint8 s (pos+3) in
                  let len_lo = get_uint8 s (pos+4) in
                  msg_type, (len_hi lsl 8) lor len_lo
              | _ ->
(*zero*)        
                  let len_lo = get_uint8 s (pos+2) in
                  let len_hi = get_uint8 s (pos+3) in
                  let msg_type = get_uint8 s (pos+4) in
                  msg_type, (len_hi lsl 8) lor len_lo
            in
(*                lprintf "Message to read: xtype %d type %d len %d\n"
                  xtype msg_type size; *)
            
            if len >= size + 5 then begin
                
                ciphers.in_xinu <- Int64.logxor ciphers.in_xinu  
                  (Int64.logand
                    (Int64.lognot (Int64.of_int (size + msg_type))) 
                  int64_ffffffff);
                let m = String.sub s (pos+5) size in
                lprintf "\nMessage %d:\n" msg_type;
                parse_packet msg_type m;
                parse_packets (pos + size + 5) s ciphers;
              end else
              lprintf "Waiting for remaining %d bytes\n"
                (size+5 - len) 
          else
            lprintf "Packet too short\n" 
        end
    | n ->
        lprintf "Packet not understood: %d\n" n;
        raise Exit


let parse_netname start_pos s ciphers = 
  let len = String.length s in
  let rec iter pos =
    if pos < len then 
      if s.[pos] = '\000' then begin
          let netname = String.sub s start_pos (pos-start_pos) in
          lprintf "netname: [%s]\n" (String.escaped netname); 
          
          parse_packets (pos+1) s ciphers
        end else
        iter (pos+1)
    else failwith "End of name not found"
  in
  iter (start_pos)

let parse s_out s_in =
  let ciphers = {
      in_cipher = create_cipher ();
      out_cipher = create_cipher ();
      in_xinu = Int64.of_int 0x51;
      out_xinu = Int64.of_int 0x51;         
    } in
  begin
    try
      let dummy_cipher = create_cipher () in
      cipher_packet_get s_out 4 ciphers.out_cipher dummy_cipher;
      cipher_free dummy_cipher;
      cipher_packet_get s_in 0 ciphers.in_cipher ciphers.out_cipher;
      (
        let len = String.length s_out in
        let start_pos = 12 in
        apply_cipher ciphers.out_cipher s_out start_pos (len-start_pos);
(*
            lprintf "Ciphered: [%s]\n" (String.escaped 
(String.sub s_out start_pos (min (len - start_pos) 1000)));
  *)
      );
      (
        let len = String.length s_in in
        let start_pos = 8 in
        apply_cipher ciphers.in_cipher s_in start_pos (len-start_pos);
(*
            lprintf "Ciphered: [%s]\n" (String.escaped 
(String.sub s_in start_pos (min (len - start_pos) 1000)));
   *)
      );
      
      lprintf "---------------------------------------------->\n";
      parse_netname 12 s_out { ciphers with
        in_xinu = ciphers.out_xinu; in_cipher = ciphers.out_cipher };
      lprintf "<----------------------------------------------\n";
      parse_netname 8 s_in ciphers;
(*
              (*
dump_sub s (start_pos) (len - start_pos); 
  *)
              
            end
  *)
    with e ->
        lprintf "exception %s while parsing stream\n"
          (Printexc2.to_string e) 
  end;
  cipher_free ciphers.in_cipher;
  cipher_free ciphers.out_cipher

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
      try
        lprintf "CONNECTION %s:%d --> %s:%d\n" 
          cnx.ip1 cnx.port1 cnx.ip2 cnx.port2;
        
        let s = Buffer.contents cnx.packets_out in
        let len = String.length s in
        
        if String2.starts_with s "GET" || 
          String2.starts_with s "POST" then begin
(*
                lprintf "Http connect to\n";
                let h1 = iter s 0 in
                lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
                
                let s = Buffer.contents cnx.packets_in in
                if String2.starts_with s "HTTP" then begin
                    lprintf "Http connected from\n";
                    let h1 = iter s 0 in
                    lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
                  end 
                else
                  lprintf "bad HTTP reply\n"
*)
            ()
          end else begin
            parse 
              (Buffer.contents cnx.packets_out) 
            (Buffer.contents cnx.packets_in);
          end
      with           
      | e ->
          lprintf "Exception %s\n" (Printexc2.to_string e)
  ) connections

let new_packet (kind:t) (number:int) ip1 port1 ip2 port2 data = 
  match kind with
    UDP -> 
      begin
        try
(*              lprintf "New packet:\n%s\n" (String.escaped data);           *)
          ()
        with e ->
(*                lprintf "Could not parse UDP packet:\n"; *)
            ()
      end
  | TCP -> 
      let out_packet = (ip1, port1, ip2, port2) in
      let in_packet = (ip2, port2, ip1, port1) in
      
      try
        let cnx =  Hashtbl.find connections out_packet in
        Buffer.add_string cnx.packets_out data; 
        ()
      with _ ->
          try
            let cnx =  Hashtbl.find connections in_packet in
            Buffer.add_string cnx.packets_in data 
          with _ ->
              let cnx = {
                  ip1 = ip1;
                  port1 = port1;
                  ip2 = ip2;
                  port2 = port2;
                  packets_out = Buffer.create 100;
                  packets_in = Buffer.create 100;
                } in
              Hashtbl.add connections out_packet cnx;
              Buffer.add_string cnx.packets_out data
              
              
              
              