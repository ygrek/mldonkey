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

open BasicSocket
open LittleEndian
open Unix
open DonkeyMftp
open TcpBufferedSocket
  
let filename =  "observer.dat"  

let motd_html = ref (try File.to_string "motd.html" with _ -> "")
let servers_met = ref (try File.to_string "servers.met" with _ -> "")
let peers_ocl = ref (try File.to_string "peers.ocl" with _ -> "")
let motd_conf = ref (try File.to_string "motd.conf" with _ -> "")

let redirector_info = ref ""
  
let update_redirector_info () =
  let buf = Buffer.create 1000 in
  buf_int buf 0;
  buf_int16 buf 0;
  buf_string16 buf !motd_html;
  buf_string16 buf !servers_met;
  buf_string16 buf !peers_ocl;
  buf_string16 buf !motd_conf;
  let s = Buffer.contents buf in
(* the len should be (String.length s - 4), but since the IP address (4 bytes) 
  is added at the end, it is (String.length s) *)
  let len = String.length s in
  LittleEndian.str_int s 0 len;
  redirector_info:=  s
  
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

let new_servers = ref []
let new_peers = ref []

  
let print_record t ip_firewall s =
  let ip = get_ip s 1 in
  let ips, pos = get_list get_peer s 5 in
  let t = Int32.to_float t in
  
  let t = localtime t in
  Printf.printf  "At %02d:%02d:%02d " 
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
  
  let version, uptime, shared, uploaded, pos =
    try
      let version, pos = get_string s pos in
      let uptime = get_int s pos in
      let shared = get_int64 s (pos+4) in
      let uploaded = get_int64 s (pos+12) in
      
      version, uptime, shared, uploaded, pos+20
    
    
    
    with _ ->
        "unknown", 0, Int64.zero, Int64.zero, pos
  in
  
  
  Printf.printf "Version: %s, uptime: %02d:%02d, shared: %Ld, uploaded: %Ld"
    version (uptime / 3600) ((uptime/60) mod 60) shared uploaded;
  print_newline ();
  List.iter (fun (ip, port) ->
      new_servers := (ip, port) :: !new_servers;
      Printf.printf "            Connected to %s:%d"
        (Ip.to_string ip) port;
      print_newline ()) ips;
  
  begin
    try
      let npeers = get_int s pos in
      Printf.printf "Overnet peers: %d" npeers; print_newline ();
      for i = 0 to npeers - 1 do
        let ip = get_ip s (pos+4+i*6) in
        let port = get_int16 s (pos+6+i*6) in
        new_peers := (ip, port) :: !new_peers;
        Printf.printf "         Overnet Peer %s:%d"        
        (Ip.to_string ip) port;
        print_newline ()
      done;
      
      
    with _ -> ()
  end
  
  
  
let create_observer port = 
  let sock = UdpSocket.create Unix.inet_addr_any port
    (DonkeyProtoCom.udp_basic_handler (fun s p ->
        if s.[0] <> '\000' then begin
            AnyEndian.dump s;
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
    )) in
  let sock = TcpServerSocket.create 
      "observer"
      Unix.inet_addr_any
      port 
      (fun t event ->
        match event with
          TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
            let sock = TcpBufferedSocket.create "observer connection" s 
                (fun sock event ->
                  match event with
                    BASIC_EVENT (LTIMEOUT | RTIMEOUT) -> 
                      close sock Closed_for_timeout
                  | _ -> ()
              )
            in
            set_lifetime sock 300.; 
            set_rtimeout sock 30.; 
            let b = Buffer.create 100 in
            Buffer.add_string b  !redirector_info;
            buf_ip b (peer_ip sock);
            let s = Buffer.contents b in
            let len = String.length s in
            Printf.printf "Sending %d bytes" len; print_newline ();
            set_max_write_buffer sock (len + 100);
            write_string sock s
        | _ -> ()
    ) in
  ()  

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
  

let servers_age = ref 60
let peers_age = ref 2
let _ =
  print_newline ();
  Arg.parse [
    "-ascii", Arg.Unit print_ascii, "";
    "-count", Arg.Unit count_records, "";
    "-server_age", Arg.Int ((:=) servers_age), " <int> : max server age (minutes) in servers.met";
    "-peer_age", Arg.Int ((:=) peers_age), " <int> : max server age (minutes) in servers.met";
    ] 
    (fun _ -> ()) ""

  
let time = ref 0
let servers_array = Array.create !servers_age []
let peers_array = Array.create !peers_age []

let dump_list array new_hosts adder dumper =
  try
    Printf.printf "dump server list"; print_newline ();
    incr time;
    let len = Array.length array in
    array.(!time mod Array.length array) <- !new_hosts;
    new_hosts := [];
    let servers = Hashtbl.create 97 in
    let servers_ip = Hashtbl.create 97 in
    for i = 0 to len - 1 do
      List.iter (fun ((ip,port) as key) ->
          if port <> 4662 && Ip.valid ip && ip <> Ip.localhost
              && Ip.reachable ip then
            try
              let key = Hashtbl.find servers_ip ip in
              Hashtbl.remove servers key
            with _ ->
                Hashtbl.add servers key (adder ip port);
                Hashtbl.add servers_ip ip key
      ) array.(i)
    done;
    let list = Hashtbl2.to_list servers in
    dumper list
  with e ->
      Printf.printf "error: %s" (Printexc2.to_string e);
      print_newline ()  
  
let dump_servers_list _ = 
  let module S = DonkeyImport.Server in
  (try motd_html := File.to_string "motd.html" with _ -> ());
  (try motd_conf := File.to_string "motd.conf" with _ -> ());
  update_redirector_info ();
  dump_list servers_array new_servers 
    (fun ip port ->
      { S.ip = ip; S.port = port; S.tags = []; };)
  (fun list ->
      let list,_ = List2.cut 500 list in
      let buf = Buffer.create 100 in
      S.write buf list;
      servers_met := (Buffer.contents buf);
      File.from_string "servers.met" !servers_met;
      update_redirector_info ();
(* now, what is the command to send the file to the WEB server ??? *)
(*
      ignore
      (Sys.command "scp -B -q servers.met simon_mld@subversions.gnu.org:/upload/mldonkey/network/"); *)
  )

let dump_peers_list _ =
  dump_list peers_array new_peers 
    (fun ip port -> (ip,port))
  (fun list ->
      let buf = Buffer.create 100 in
      List.iter (fun (ip, port) ->
          Printf.bprintf buf "%s,%d,X\n" (Ip.to_string ip) port;
      ) list;
      peers_ocl := (Buffer.contents buf);
      File.from_string "peers.ocl" !peers_ocl;
      update_redirector_info ();
      (*
      ignore
        (Sys.command "scp -B -q peers.ocl simon_mld@subversions.gnu.org:/upload/mldonkey/network/");
*)
  )
  
let _ =
  update_redirector_info ();
  ignore (create_observer 3999);
  ignore (create_observer 4665)
  
let _ =
  begin
  let module S = DonkeyImport.Server in
    try
      let file = File.to_string "servers.met" in
      List.iter (fun s ->
        servers_array.(0) <- (s.S.ip , s.S.port) :: servers_array.(0)
        ) (S.read file)
    with _ -> Printf.printf "Could not load old server list"; print_newline ();
  end;
  BasicSocket.add_timer 30. dump_servers_list;
  BasicSocket.add_timer 30. dump_peers_list;
  BasicSocket.add_infinite_timer 300. dump_servers_list;
  BasicSocket.add_infinite_timer 300. dump_peers_list;
  Printf.printf "Observer started";
  print_newline ();
  BasicSocket.loop ()
  
