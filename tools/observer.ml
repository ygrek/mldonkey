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

open LittleEndian
open Unix
open DonkeyMftp
open TcpBufferedSocket

let filename =  "observer.dat"  
  
let bin_oc = open_out_gen [Open_append; Open_creat; Open_binary;
    Open_wronly] 0o644 filename

let buf = Buffer.create 70000

let dump_record t s ip =
  Buffer.clear buf;
  buf_int32 buf t; 
  buf_ip buf ip;
  buf_string buf s;
  let m = Buffer.contents buf in
  output_string bin_oc m;
  flush bin_oc

let read_record ic =
  let s = String.create 100 in
  really_input ic s 0 10;
  let t = get_int32 s 0 in
  let ip = get_ip s 4 in
  let len = get_int16 s 8 in
  let s = String.create len in
  really_input ic s 0 len;
  (t, ip, s)

(* record:

4 bytes: date
4 bytes: IP source
2 bytes: len 
char[len]: packets
  packet:
     1 byte: 0 (* magic *)
     4 bytes: IP 
     4 bytes: nrecords
     record[nrecords]: servers
       record:
           4 bytes: IP
           2 bytes: port

*)

let print_record t ip_firewall s =
  let ip = get_ip s 1 in
  let ips, pos = get_list get_peer s 5 in

  let t = Int32.to_float t in
  
  let t = localtime t in
  Printf.printf "At %02d:%02d:%02d " 
    t.tm_hour
    t.tm_min
    t.tm_sec
  ;
  print_newline ();
  Printf.printf "MLdonkey on %s (through %s): " 
    (Ip.to_string ip)
  (Ip.to_string ip_firewall)
  ;
  print_newline ();
  List.iter (fun (ip, port) ->
      Printf.printf "            Connected to %s:%d"
        (Ip.to_string ip) port;
      print_newline ()) ips
  
let create_observer port = UdpSocket.create Unix.inet_addr_any port
    (DonkeyProtoCom.udp_basic_handler (fun s p ->
        if s.[0] <> '\000' then begin
            dump s;
            failwith "Bad UDP packet"
          end;
        let ip_firewall = 
          match p.UdpSocket.addr with
            Unix.ADDR_INET (ip, port) -> Ip.of_inet_addr ip
          | _ -> Ip.localhost
        in
        
        let t = gettimeofday () in
        let t = Int32.of_float t in
        
        dump_record t s ip_firewall;
        
        print_record t ip_firewall s
    ))

let iter_file f g h =
  let ic = open_in filename in
  f ();
  try
    while true  do
      let (t, ip, s) = read_record ic in
      g t ip s
    done
  with End_of_file ->  
      close_in ic; 
      h ();
      exit 0
      
let no () = ()
      
let print_ascii () =  iter_file no print_record no

let count_records () =
  let first_record = ref None in
  let last_record = ref None in
  let counter = ref 0 in
  let clients = Hashtbl.create 100 in
  let servers = Hashtbl.create 1000 in
  let server_counter = ref 0 in
  iter_file no (fun t ip_firewall s ->
      begin
        match !first_record with
          None -> first_record := Some t
        | _ -> ()
      end;
      last_record := Some t;
      let ip = get_ip s 1 in
      let ips, pos = get_list get_peer s 5 in
      if not (Hashtbl.mem clients (ip, ip_firewall)) then
        begin
          Hashtbl.add clients  (ip, ip_firewall) ips;
          incr counter;
        end;
      List.iter (fun s ->
          if not (Hashtbl.mem servers s) then
            begin
              Hashtbl.add servers s ();
              incr server_counter;
            end;
          
      ) ips
  ) (fun _ ->
      Printf.printf "%d MLdonkey clients" !counter;
      (match !first_record, !last_record with
          Some t1, Some t2 ->
            Printf.printf " in %3.0ld seconds" (Int32.sub t2 t1)
        | _ -> ());
      Printf.printf " on %d servers" !server_counter; 
      print_newline ();
  )
  
  
let _ =
  print_newline ();
  Arg.parse [
    "-ascii", Arg.Unit print_ascii, "";
    "-count", Arg.Unit count_records, "";
    ] 
    (fun _ -> ()) ""
  
    
  
let _ =
  ignore (create_observer 3999);
  ignore (create_observer 4665)
  
let _ =
  Printf.printf "Observer started";
  print_newline ();
  BasicSocket.loop ()
  