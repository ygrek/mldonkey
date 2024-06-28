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

open Printf2

open AnyEndian

open FasttrackNetwork
open FasttrackProto

(*************************************************************************)
(*                                                                       *)
(*                                                                       *)
(*                                                                       *)
(*                         READING FROM TCPDUMP                          *)
(*                                                                       *)
(*                                                                       *)
(*                                                                       *)
(*************************************************************************)

type t = UDP | TCP

type cnx = {
    time : int;
    ip1 : string;
    port1 : int;
    ip2 : string;
    port2 : int;
    packets_in : Buffer.t;
    packets_out : Buffer.t;
  }
let connections = Hashtbl.create 13

let rec parse_packets pos s ciphers =
  let len = Bytes.length s - pos in
  if len > 0 then
    let size = TcpMessages.packet_size ciphers s pos len in
    match size with
      None -> ()
    | Some size ->
        if len >= size then
          let msg = Bytes.sub s pos size in
          let addr, t = TcpMessages.parse ciphers (Bytes.to_string msg) in
          lprintf "MESSAGE: %s\n    %s\n"
            (TcpMessages.string_of_path addr)
          (TcpMessages.to_string t);
          parse_packets (pos+size) s ciphers
        else
          lprintf "Packet too short\n"

let parse_netname start_pos s ciphers =
  let len = Bytes.length s in
  let rec iter pos =
    if pos < len then
      if (Bytes.get s pos) = '\000' then begin
          let netname = Bytes.sub s start_pos (pos-start_pos) in
          lprintf "netname: [%s]\n" (Bytes.unsafe_to_string (Bytes.escaped netname));

(*          test_xinu s (pos+1) len 0x51L; *)
          parse_packets (pos+1) s ciphers
        end else
        iter (pos+1)
    else failwith "End of name not found"
  in
  iter (start_pos)

let get_xinu s pos xtype =
  match xtype with
    0 ->
      let msg_lo = get_uint8 s (pos+1) in
      let msg_hi = get_uint8 s (pos+2) in
      let len_hi = get_uint8 s (pos+3) in
      let len_lo = get_uint8 s (pos+4) in
      (msg_hi lsl 8) lor msg_lo, (len_hi lsl 8) lor len_lo
  | 1 ->
      let msg_hi = get_uint8 s (pos+1) in
      let len_hi = get_uint8 s (pos+2) in
      let msg_lo = get_uint8 s (pos+3) in
      let len_lo = get_uint8 s (pos+4) in
      (msg_hi lsl 8) lor msg_lo, (len_hi lsl 8) lor len_lo
  | _ ->
      let msg_hi = get_uint8 s (pos+1) in
      let len_lo = get_uint8 s (pos+2) in
      let len_hi = get_uint8 s (pos+3) in
      let msg_lo = get_uint8 s (pos+4) in
      (msg_hi lsl 8) lor msg_lo, (len_hi lsl 8) lor len_lo

let rec check_xinu s pos len depth =
  if depth > 5 then depth else
  if pos >= len then depth else
  match int_of_char s.[pos] with
    0x50 | 0x52 -> check_xinu  s (pos+1) len (depth + 1)
  | 0x4b ->
      if pos + 5 < len then

        let msg_type0, len0 = get_xinu s pos 0 in
        let msg_type1, len1 = get_xinu s pos 1 in
        let msg_type2, len2 = get_xinu s pos 2 in

        let check0 = check_xinu s (pos + 5 + len0) len (depth+1) in
        let check1 = check_xinu s (pos + 5 + len1) len (depth+1) in
        let check2 = check_xinu s (pos + 5 + len2) len (depth+1) in

        max check0 (max check1 check2)
      else depth
  | _ -> -10

let parse (s_out : bytes) (s_in : bytes) =
  let parsed = ref false in
  if Bytes.length s_in > 12 && Bytes.length s_out > 12 then begin
      let ciphers = {
          in_cipher = create_cipher ();
          out_cipher = create_cipher ();
          in_xinu = 0x51L;
          out_xinu = 0x51L;
        } in
      begin
        try

          get_cipher_from_packet (Bytes.to_string s_out) 4 ciphers.out_cipher;
          init_cipher ciphers.out_cipher;

          get_cipher_from_packet (Bytes.to_string s_in) 0 ciphers.in_cipher;
          init_cipher ciphers.in_cipher;

          xor_ciphers ciphers.out_cipher ciphers.in_cipher;
          init_cipher ciphers.out_cipher;

          lprintf "HEADER OF CONNECTION: %02x.%02x.%02x.%02x - %02x.%02x.%02x.%02x\n"
            (int_of_char (Bytes.get s_out 0))
          (int_of_char (Bytes.get s_out 1))
          (int_of_char (Bytes.get s_out 2))
          (int_of_char (Bytes.get s_out 3))

          (int_of_char (Bytes.get s_out 4))
          (int_of_char (Bytes.get s_out 5))
          (int_of_char (Bytes.get s_out 6))
          (int_of_char (Bytes.get s_out 7))
          ;

          begin
            let s = Bytes.make 8 '\000' in
            cipher_packet_set ciphers.out_cipher s 0;
            lprintf "OUT CIPHER: [%s]\n" (Bytes.unsafe_to_string (Bytes.escaped s));
          end;

          begin
            let s = Bytes.make 8 '\000' in
            cipher_packet_set ciphers.in_cipher s 0;
            lprintf "IN CIPHER: [%s]\n" (Bytes.unsafe_to_string (Bytes.escaped s));
          end;

          (
            let len = Bytes.length s_out in
            let start_pos = 12 in
            apply_cipher ciphers.out_cipher s_out start_pos (len-start_pos);
(*
            lprintf "Ciphered: [%s]\n" (String.escaped
(String.sub s_out start_pos (min (len - start_pos) 1000)));
  *)
          );
          (
            let len = Bytes.length s_in in
            let start_pos = 8 in
            apply_cipher ciphers.in_cipher s_in start_pos (len-start_pos);
(*
            lprintf "Ciphered: [%s]\n" (String.escaped
(String.sub s_in start_pos (min (len - start_pos) 1000)));
   *)
          );

          lprintf "---------------------------------------------->\n";
          lprintf "  HEADER[%s]\n" (Bytes.unsafe_to_string (Bytes.escaped (Bytes.sub s_out 0 4)));
          parse_netname 12 s_out { ciphers with
            in_xinu = ciphers.out_xinu; in_cipher = ciphers.out_cipher };
          lprintf "<----------------------------------------------\n";
          parse_netname 8 s_in ciphers;
          parsed := true;
(*
 (*
dump_sub s (start_pos) (len - start_pos);
  *)

   end
  *)
    with e ->
        lprintf "exception %s while parsing stream\n"
          (Printexc2.to_string e) ;
        lprintf "  [%s]\n" (Bytes.unsafe_to_string (Bytes.escaped
            (Bytes.sub s_in 0 (min 50 (Bytes.length s_in)))))
  end;
  cipher_free ciphers.in_cipher;
    cipher_free ciphers.out_cipher;
    end;
  !parsed

let find_header s pos =
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
  in
  let pos2 = iter s pos in
  String.sub s pos (pos2 - pos)

(*
let hescaped s =
  String2.replace_char s '\r' ' ';s
*)

let is_http_stream s =
  String2.starts_with s "GET" ||
  String2.starts_with s "POST" ||
  String2.starts_with s "HTTP"

type packet =
  UdpPacket of string * int * string * int * string
| TcpConnection of cnx

let packets = ref []

let print_packets () =
  let packets = List.sort (fun (t1,_) (t2,_) -> compare t1 t2) !packets in
  List.iter (fun (time, p) ->
      match p with
        UdpPacket (ip1, port1, ip2, port2, data) ->
          lprintf "Time %d\n" time;
          lprintf "UDP packet(%d) %s:%d -> %s:%d\n"
            (String.length data) ip1 port1 ip2 port2;
          let p = UdpMessages.parse data in
          lprintf "     %s\n" (UdpMessages.to_string p)

      | TcpConnection cnx ->
          try

            let s1 = Buffer.contents cnx.packets_out in
            let s2 = Buffer.contents cnx.packets_in in

            if s1 <> "" || s2 <> "" then begin

                if is_http_stream s1 || is_http_stream s2 then begin
(*
                    lprintf "Time %d\n" time;
                    lprintf "\nCONNECTION %s:%d --> %s:%d\n"
                    cnx.ip1 cnx.port1 cnx.ip2 cnx.port2;

                    (if is_http_stream s1 then
                        let header = find_header s1 0 in
                        lprintf "HTTP connection: [%s]\n" (hescaped header));
                    (if is_http_stream s2 then
                        let header2 = find_header s2 0 in
                        lprintf "HTTP reply connection: [%s]\n" (hescaped header2));
*)
                    ()
                  end else begin
                    lprintf "Time %d\n" time;
                    lprintf "CONNECTION %s:%d --> %s:%d\n"
                      cnx.ip1 cnx.port1 cnx.ip2 cnx.port2;

                    lprintf "First direction....\n";
                    let parsed = parse
                        (Buffer.to_bytes cnx.packets_out)
                      (Buffer.to_bytes cnx.packets_in) in
                    if not parsed then begin
                        lprintf "Second direction....\n";
                        let _ = parse
                            (Buffer.to_bytes cnx.packets_in)
                          (Buffer.to_bytes cnx.packets_out) in
                        ()
                      end
                  end
              end
          with
            | e ->
              lprintf "Exception %s\n" (Printexc2.to_string e)
  ) packets

let commit () =
  Hashtbl.iter (fun _ cnx ->
      packets := (cnx.time, TcpConnection cnx) :: !packets
  ) connections;
  print_packets ()

let local_net = "129.104"
let time = ref 0

let new_packet (kind:t) (number:int) ip1 port1 ip2 port2 data =
  if not (String2.starts_with ip1 local_net &&
      String2.starts_with ip2 local_net) then
    begin
      incr time;
      let time = !time in
      match kind with
        UDP ->
          begin
            try
              packets := (time, UdpPacket (ip1,port1,ip2,port2,data)) :: !packets
(*              lprintf "New packet:\n%s\n" (String.escaped data);           *)
            with e ->
(*                lprintf "Could not parse UDP packet:\n"; *)
                ()
          end
      | TCP ->

(*      if port1 = 1214 || port2 = 1214 then *)
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
                      time = time;
                      ip1 = ip1;
                      port1 = port1;
                      ip2 = ip2;
                      port2 = port2;
                      packets_out = Buffer.create 100;
                      packets_in = Buffer.create 100;
                    } in
                  Hashtbl.add connections out_packet cnx;
                  Buffer.add_string cnx.packets_out data
    end

(*************************************************************************)
(*                                                                       *)
(*                                                                       *)
(*                                                                       *)
(*                         READING FROM LOG                              *)
(*                                                                       *)
(*                                                                       *)
(*                                                                       *)
(*************************************************************************)


open Int64ops
open LittleEndian (* This is bad, since it is the contrary of the default
FT format, so we will forget it once, and our logs won't be readable
  anymore... *)

type connection = {
    c_ip : Ip.t;
    c_port : int;
    mutable c_time : int;
    mutable c_ciphers : ciphers;
    mutable c_pos : int;
    mutable c_buf : string;
    mutable c_parser : (connection -> unit);
    mutable c_npackets : int;
  }

let connections = Hashtbl.create 1000

let print c addr t =
  let module M = TcpMessages in
  match t with
        (*
  | M.SearchReq _
*)
  | M.SearchForwardReq _
    (*
  | M.SearchForward2Req _

  | M.ShareFileReq _
  | M.UnshareFileReq _
*)
    ->  lprintf "MESSAGE %d from %s:%d time:%d: %s\n    %s\n\n"
        c.c_npackets (Ip.to_string c.c_ip) c.c_port c.c_time
        (TcpMessages.string_of_path addr)
      (TcpMessages.to_string t)

  | _

    -> ()
(*    ->  lprintf "MESSAGE %d from %s:%d time:%d: %s\n    %s\n\n"
        c.c_npackets (Ip.to_string c.c_ip) c.c_port c.c_time
        (TcpMessages.string_of_path addr)
      (TcpMessages.to_string t) *)

      (*
let parse_head c s pos =
  let xtype = Int64.to_int (Int64.rem c.c_ciphers.in_xinu int64_3) in

  begin
    let msg_type0, len0 = get_xinu s pos 0 in
    let msg_type1, len1 = get_xinu s pos 1 in
    let msg_type2, len2 = get_xinu s pos 2 in

    let len = String.length s in
    let check0 = check_xinu s (pos + 5 + len0) len 0 in
    let check1 = check_xinu s (pos + 5 + len1) len 0 in
    let check2 = check_xinu s (pos + 5 + len2) len 0 in

    lprintf "    xinu: %d\n" xtype;
    lprintf "      0 : opcode = %x len = %d [%d]\n" msg_type0 len0
      check0;
    lprintf "      1 : opcode = %x len = %d [%d]\n" msg_type1 len1
      check1;
    lprintf "      2 : opcode = %x len = %d [%d]\n" msg_type2 len2
      check2;
  end;

(*  if c.c_npackets = 388 then 100, 1000 else  *)
    TcpMessages.get_xinu s pos xtype

let parse c s =
  let module T = TcpMessages in
  match int_of_char s.[0] with
    0x50 -> T.DirectPacket, T.PingReq
  | 0x52 -> T.DirectPacket, T.PongReq
  | 0x4b ->
(*          lprintf "We have got a real packet\n"; *)
      let msg_type, size = parse_head c s 0 in

      c.c_ciphers.in_xinu <- Int64.logxor c.c_ciphers.in_xinu
        (Int64.logand
          (Int64.lognot (Int64.of_int (size + msg_type)))
        int64_ffffffff);

      let msg_flags = (msg_type land 0xff00) lsr 8 in
      let msg_type = msg_type land 0xff in

      let pos, size, addr = match msg_flags with
        | 0x80 ->
            let source_ip = LittleEndian.get_ip s 5 in
            let source_port = BigEndian.get_int16  s 9 in
            let dest_ip = LittleEndian.get_ip s 11 in
            let dest_port = BigEndian.get_int16 s 15 in
            let hops = BigEndian.get_int8 s 17 in

            let addr = {
                T.unicast_source_ip = source_ip;
                T.unicast_source_port = source_port;
                T.unicast_dest_ip = dest_ip;
                T.unicast_dest_port = dest_port;
                T.unicast_hops = hops;
              } in

            5 + 13, size - 13, T.UnicastPacket addr
        | 0xC0 ->
            let source_ip = LittleEndian.get_ip s 5 in
            let source_port = BigEndian.get_int16  s 9 in
            let unknown = BigEndian.get_int16 s 11 in
            let hops = BigEndian.get_int8 s 13 in

            let addr = {
                T.broadcast_source_ip = source_ip;
                T.broadcast_source_port = source_port;
                T.broadcast_unknown = unknown;
                T.broadcast_hops = hops;
              } in

            5 + 9, size - 9, T.BroadcastPacket addr
        | 0 ->
            5, size, T.DirectPacket
        | _ ->
            lprintf "   MESSAGE HAS UNKNOWN FLAG %x\n" msg_flags;
            5, size, T.DirectPacket
      in
      let m = String.sub s pos size in
      addr,
      (try T.parse_packet msg_type m with e ->
            lprintf "Exception in parse_packet\n"; raise e)
  | n ->
      lprintf "Packet not understood: %d\n" n;
      dump s;
      T.DirectPacket, T.UnknownMessageReq (n, s)

let packet_size c s pos len =
  if len > 0 then begin

      match int_of_char s.[pos] with
        0x50 -> Some 1
      | 0x52 -> Some 1
      | 0x4b ->
(*          lprintf "We have got a real packet\n"; *)
          if len > 4 then
(*                dump_sub s b.pos b.len; *)
            let msg_type, size = parse_head c s pos in

            Some (size + 5)
          else None

      | n ->
          lprintf "Packet not understood: %d\n" n;
          raise Exit
    end else None
      *)

let rec parse_packets c =
  let pos = c.c_pos in
  let s = c.c_buf in
  let len = String.length s - pos in
  if len > 0 then
    try
      let size = TcpMessages.packet_size c.c_ciphers (Bytes.of_string s) pos len in
      match size with
        None -> ()
      | Some size ->
          if len >= size then begin
(*            lprintf "size %d+%d/%d\n" pos size (String.length s); *)
              let msg = String.sub s pos size in
              let addr, t = TcpMessages.parse c.c_ciphers msg in
              print c addr t;

(*
          if c.c_npackets >= 388 && c.c_time >= 97771248 then begin
              lprintf "size %d\n" size;
              if c.c_npackets = 390 then exit 0;
            end;          *)
              c.c_npackets <- c.c_npackets + 1;
              c.c_pos <- pos + size;
              parse_packets c
            end else ()
    with _ -> ()
  else begin
      c.c_pos <- 0;
      c.c_buf <- ""
    end

let parse_netname c =
  let start_pos = c.c_pos in
  let s = c.c_buf in
  let len = String.length s in
  let rec iter pos =
    if pos < len then
      if s.[pos] = '\000' then begin
          c.c_pos <- pos + 1;
          c.c_parser <- parse_packets;
          parse_packets c
        end else
        iter (pos+1)
  in
  iter start_pos

let received ip port time s =
(*  lprintf "Received %s:%d at %d size %d\n"
    (Ip.to_string ip) port time (String.length s); *)
  let key = (ip, port) in
  let c = try
      Hashtbl.find connections key
    with _ ->
        let ciphers = {
            in_cipher = create_cipher ();
            out_cipher = create_cipher ();
            in_xinu = 0x51L;
            out_xinu = 0x51L;
          } in
        let c = {
            c_ip = ip;
            c_time = time;
            c_port = port;
            c_pos = 0;
            c_buf = "";
            c_parser = parse_netname;
            c_ciphers = ciphers;
            c_npackets = 0;
          } in
        Hashtbl.add connections key c;
        c
  in
  c.c_buf <- c.c_buf ^ s;
  c.c_time <- time;
  c.c_parser c

let read_trace () =
  let ic = open_in "ft_supernode.dump" in
  let buffer_size = 52000 in
  let s = String.create buffer_size in
  let total = ref zero in
  let rec iter pos =
    let nread = input ic s pos (buffer_size - pos) in
    if nread = 0 then begin
        lprintf "closing...\n";
        close_in ic
      end
    else
    let len = pos + nread in
    total := !total ++ Int64.of_int nread;
    iter_log 0 len

  and iter_log pos len =
    if len > 13 then
      let size = get_int_bytes s (pos + 10) in
      let ip = LittleEndian.get_ip_bytes s pos in
      let port = get_int16_bytes s (pos+4) in
      let time = get_int_bytes s (pos+6) in
      let item_len = size + 14 in
      if item_len <= len then
        let p = Bytes.sub_string s (pos+14) size in
        received ip port time p;
        iter_log (pos + item_len) (len - item_len)
      else iter_read pos len
    else iter_read pos len

  and iter_read pos len =
    if pos = 0 then iter len
    else
      begin
        Bytes.blit s pos s 0 len;
        iter len
      end
  in
  iter 0;
  lprintf "Total %Ld\n" !total
