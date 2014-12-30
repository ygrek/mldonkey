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
open AnyEndian
open BasicSocket
open LittleEndian
open Unix
open DonkeyMftp
open TcpBufferedSocket
  
let filename =  "observer.dat"  

let motd_html = ref (try File.to_string "motd.html" with _ -> "")
let servers_met = ref (try File.to_string "servers.met" with _ -> "")
let overnet_peers_ocl = ref (try File.to_string "overnet_peers.ocl" with _ -> "")
let kademlia_peers_ocl = ref (try File.to_string "kademlia_peers.ocl" with _ -> "")
let motd_conf = ref (try File.to_string "motd.conf" with _ -> "")

let redirector_info = ref ""
  
let update_redirector_info () =
  let buf = Buffer.create 1000 in
  buf_int buf 0;
  buf_int16 buf 0;
  buf_string16 buf !motd_html;
  buf_string16 buf !servers_met;
  buf_string16 buf !overnet_peers_ocl;
  buf_string16 buf !motd_conf;
  buf_string16 buf !kademlia_peers_ocl;
  let s = Buffer.contents buf in
(* the len should be (String.length s - 4), but since the IP address (4 bytes) 
  is added at the end, it is (String.length s) *)
  let len = String.length s in
  LittleEndian.str_int s 0 len;
  redirector_info:=  s
  
let bin_oc = open_out_gen [Open_append; Open_creat; Open_binary;
    Open_wronly] 0o644 filename

let observer_buf = Buffer.create 70000

let dump_record t s ip =
  let buf = observer_buf in
  Buffer.clear buf;
  buf_int64 buf t; 
  buf_ip buf ip;
  buf_string buf s;
  let m = Buffer.contents buf in
  output_string bin_oc m;
  flush bin_oc

let read_record ic =
  let s = String.create 100 in
  really_input ic s 0 10;
  let t = get_int64 s 0 in
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

module T = struct
    
    type key = Ip.t * int
    
    type 'a t = {
        objects_fifo : key Fifo.t; 
        max_objects : int;
        objects_table : (key, key) Hashtbl.t;
      }
    
    let create max_objects = {
        objects_fifo = Fifo.create ();
        max_objects = max_objects;
        objects_table = Hashtbl.create 127;
      }
      
    let add t key = 
      let (ip, port) = key in
      if Ip.valid ip && ip <> Ip.localhost && Ip.reachable ip &&
        not (Hashtbl.mem t.objects_table key) then
        begin
          Hashtbl.add t.objects_table key key;
          Fifo.put t.objects_fifo key;
          if Fifo.length t.objects_fifo = t.max_objects then 
            let key = Fifo.take t.objects_fifo in
            Hashtbl.remove t.objects_table key
        end
        
    let to_list t =
      Fifo.to_list t.objects_fifo
        
  end
  
let new_servers = T.create 200
let new_overnet_peers = T.create 1000
let new_kademlia_peers = T.create 1000
  
  
let print_record t ip_firewall s =
  try
    let ip = get_ip s 1 in
    let t = Int64.to_float t in
    
    let t = localtime t in
    lprintf  "At %02d:%02d:%02d\n" 
      t.tm_hour
      t.tm_min
      t.tm_sec
    ;
    let opcode = int_of_char s.[0] in
    match  opcode with
      0 ->
        let ips, pos = get_list get_peer s 5 in
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
        
        
        lprintf "Version: %s, uptime: %02d:%02d, shared: %Ld, uploaded: %Ld\n"
          version (uptime / 3600) ((uptime/60) mod 60) shared uploaded;
        List.iter (fun (ip, port) ->
            T.add new_servers (ip, port);
            lprintf "            Connected to %s:%d\n"
              (Ip.to_string ip) port;
        ) ips;
        begin
          try
            let npeers = get_int s pos in
            lprintf "Overnet peers: %d\n" npeers; 
            for i = 0 to npeers - 1 do
              let ip = get_ip s (pos+4+i*6) in
              let port = get_int16 s (pos+6+i*6) in
              T.add new_overnet_peers (ip, port);
              lprintf "         Overnet Peer %s:%d\n"
                (Ip.to_string ip) port;
            
            done;
          
          
          with _ -> ()
        end
    
    | 1 ->
        
        lprintf "MLdonkey on %s (through %s):\n" 
          (Ip.to_string ip)
        (Ip.to_string ip_firewall)
        ;
        
        let pos = 5 in
        let version, pos = get_string s pos in
        let uptime = get_int s pos in
        let shared = get_int64 s (pos+4) in
        let uploaded = get_int64 s (pos+12) in
        let pos = pos + 20 in
        
        lprintf "Version: %s, uptime: %02d:%02d, shared: %Ld, uploaded: %Ld\n"
          version (uptime / 3600) ((uptime/60) mod 60) shared uploaded;
        
        let upload_rate = get_int16 s pos in
        let download_rate = get_int16 s (pos+2) in
        lprintf "   upload: %d download: %d\n" upload_rate download_rate;
        let lost_upload = get_int s (pos+4) in
        let load_download = get_int s (pos+8) in

        (*
        let _ = 
          get_list (fun s pos ->
              let n = get_uint8 s pos in
              let s, pos = get_string s (pos+4) in
              
              begin
                match n with
                | 1 ->
                    let pos = 0 in
                    let ips, pos = get_list (fun s pos ->
                          (get_ip s pos, get_int16 s (pos+4)), pos
                      ) s pos in
                    
                    List.iter (fun (ip, port) ->
                        new_servers := (ip, port) :: !new_servers;
                        lprintf "            Connected to %s:%d\n"
                          (Ip.to_string ip) port;
                    ) ips;
                
                | 2 ->
                    
                    let npeers = get_int s pos in
                    lprintf "Overnet peers: %d\n" npeers; 
                    for i = 0 to npeers - 1 do
                      let ip = get_ip s (pos+4+i*6) in
                      let port = get_int16 s (pos+6+i*6) in
                      new_peers := (ip, port) :: !new_peers;
                      lprintf "         Overnet Peer %s:%d\n"
                        (Ip.to_string ip) port;
                    done;
                | n ->
                    lprintf "Unknown fragment %d\n" n
              end;
              (), pos
          ) s (pos+12) 
        in
*)        
        ()
    
    | 2 ->
        
        lprintf "MLdonkey on %s (through %s):\n" 
          (Ip.to_string ip) (Ip.to_string ip_firewall);
        
        let pos = 5 in
        let version, pos = get_string s pos in
        let uptime = get_int s pos in
        
        let max_upload_rate = get_int16 s (pos+4) in
        let max_download_rate = get_int16 s (pos+6) in
        let upload_lost = get_int s (pos+8) in
        let download_lost = get_int s (pos+12) in
        let pos = pos + 16 in
        
        
        lprintf "Version: %s, uptime: %02d:%02d\n"
          version (uptime / 3600) ((uptime/60) mod 60);
        lprintf "   upload: %d download: %d\n" 
          max_upload_rate max_download_rate;
        lprintf "   upload lost: %d download lost: %d\n" 
          upload_lost   download_lost;
        
        let list,pos = get_list (fun s pos ->
              let n, pos = get_string s pos in
              let s, pos = get_string s pos in
              ( (n,s), pos)
          ) s pos
        in

        List.iter (fun (n,s) ->
            try
              match n with
                "DKSV" -> 
                  let servers, pos = get_list (fun s pos ->
                        let ip = get_ip s pos in
                        let port = get_port s (pos+4) in
                        (ip, port), pos+6
                    ) s 0 in
                  

                  lprintf "    DKSV:\n";
                  List.iter (fun (ip, port) ->
                      T.add new_servers (ip, port);
                      lprintf "            Connected to %s:%d\n"
                        (Ip.to_string ip) port;
                  ) servers
  
              | "DKOV" -> 
                let servers, pos = get_list (fun s pos ->
                        let ip = get_ip s pos in
                        let port = get_port s (pos+4) in
                        (ip, port), pos+6
                    ) s 0 in
                  
                  
                  lprintf "    DKOV:\n";
                  List.iter (fun (ip, port) ->
                      T.add new_overnet_peers (ip, port);
                      lprintf "            Overnet peer %s:%d\n"
                        (Ip.to_string ip) port;
                  ) servers
  
              | "DKKD" -> 
                let servers, pos = get_list (fun s pos ->
                        let ip = get_ip s pos in
                        let port = get_port s (pos+4) in
                        (ip, port), pos+6
                    ) s 0 in
                  
                  
                  lprintf "    DKKD:\n";
                  List.iter (fun (ip, port) ->
                      T.add new_kademlia_peers (ip, port);
                      lprintf "            Kademlia peer %s:%d\n"
                        (Ip.to_string ip) port;
                  ) servers

  
              | "DKKO" -> 
                let servers, pos = get_list (fun s pos ->
                        let ip = get_ip s pos in
                        let port = get_port s (pos+4) in
                        (ip, port), pos+8
                    ) s 0 in
                  
                  
                  lprintf "    DKKO:\n";
                  List.iter (fun (ip, port) ->
                      T.add new_overnet_peers (ip, port);
                      lprintf "            Overnet peer %s:%d\n"
                        (Ip.to_string ip) port;
                  ) servers
                  
              | "DKKA" -> 
                let servers, pos = get_list (fun s pos ->
                        let ip = get_ip s pos in
                        let udp_port = get_port s (pos+4) in
                        let tcp_port = get_port s (pos+6) in
                        (ip, udp_port, tcp_port), pos+8
                    ) s 0 in
                  
                  lprintf "    DKKA:\n";
                  List.iter (fun (ip, udp_port, tcp_port) ->
                      T.add new_kademlia_peers (ip, udp_port);
                      lprintf "            Kademlia peer %s:%d %d\n"
                        (Ip.to_string ip) udp_port tcp_port;
                  ) servers
                  
              | "DKNB" ->  
                  
                  let len = get_int s 0 in
                  let ngood_propositions = Array.make len zero in
                  let nbad_propositions = Array.make len zero in
                  let pos = 4 in
                  for i = 0 to len - 1 do
                    ngood_propositions.(i) <- get_int64 s (pos+i*16);
                    nbad_propositions.(i) <- get_int64 s (pos+8+i*16);
                  done;
                  let pos = pos + len * 16 in
                  let waiting = get_int s pos in
                  let neighbours = get_int s (pos+4) in

                  lprintf "    DKNB:\n";
                  for i = 0 to len - 1 do
                    lprintf "        Good[%d] = %Ld\n" i ngood_propositions.(i);
                    lprintf "        Bad[%d]  = %Ld\n" i nbad_propositions.(i);
                  done;
                  lprintf "       Waiting propositions: %d\n" waiting;
                  lprintf "       Total neighbours: %d\n" neighbours
                  
              | "SHARED" -> 
                  let total_shared = get_int64 s 0 in
                  let total_uploaded = get_int64 s 8 in

                  lprintf "    SHARED:\n";
                  lprintf "       Shared: %Ld, uploaded: %Ld\n"
                    total_shared total_uploaded;
    
              | "LTCY" ->
                  let loop_delay = get_int s 0 in
                   
                  let pos = 4 in
                  let ntcp = get_int s pos in
                  for i = 0 to ntcp - 1 do
                    let ip = get_ip s (pos+4+8*i) in
                    let latency = get_int16 s (pos+8+8*i) in
                    let samples = get_int16 s (pos+10+8*i) in
                    lprintf "TCP %d %s %s %d \n" 
                      samples
                      (Ip.to_string ip_firewall)
                    (Ip.to_string ip) latency
                  done;
                  
                  let pos = pos + 4+ 8 * ntcp in
                  let nudp = get_int s pos in
                  for i = 0 to nudp - 1 do
                    let ip = get_ip s (pos+4+8*i) in
                    let latency = get_int16 s (pos+8+8*i) in
                    let samples = get_int16 s (pos+10+8*i) in
                    lprintf "UDP %d %s %s %d\n" samples
                      (Ip.to_string ip_firewall)
                    (Ip.to_string ip) latency
                  done;
                
              | _ -> lprintf "    Unknown kind of info: %s\n" n;
            with e ->
                lprintf "  Exception %s while parsing info\n"
                  (Printexc2.to_string e)
        ) list
    
    
    | _ ->
        lprintf "Unknown format\n";
        AnyEndian.dump s
  with _ -> ()
      
      
type client_info = {
    client_ip : Ip.t;
    client_port : int;
    mutable client_buf : Buffer.t;
    mutable client_ok : bool;
  }

let read_client_info c sock nread =
  let b = buf sock in
  let len = b.len in
  let pos = b.pos in
  let buf = b.buf in
  if nread > 0 && c.client_ok then begin
      Buffer.add_string c.client_buf (String.sub buf pos len);
      buf_used b len;
      let l = Buffer.length c.client_buf in
      if l > 10000 then begin
          c.client_ok <- false;
          lprintf "CLIENT NOT OK\n";
          close sock Closed_by_user
        end
    end
    
    (*
  if c.client_ok then
    if len > 5 then
      match get_uint8 buf pos with
        212 -> begin
            match get_uint8 buf (pos+1) with
            | 2 -> 
                let msg_len = 
                
            | version ->
                lprintf "Bad version for connection from %s:%d: %d\n"
                  (Ip.to_string c.client_ip) c.client_port version;
                c.client_ok <- false
                
          end
      | magic ->
          lprintf "Bad magic for connection from %s:%d: %d\n"
            (Ip.to_string c.client_ip) c.client_port magic;
          c.client_ok <- false
    else
(* not enough data *) ()
  else
    buf_used b b.len
*)
    
let create_observer port = 
  let sock = UdpSocket.create Unix.inet_addr_any port
      (DonkeyProtoCom.udp_basic_handler (fun s p ->
          let ip_firewall = 
            match p.UdpSocket.udp_addr with
              Unix.ADDR_INET (ip, port) -> Ip.of_inet_addr ip
            | _ -> Ip.localhost
          in
          
          let t = gettimeofday () in
          let t = Int64.of_float t in
          
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
            
            let c = { 
                client_ip = Ip.of_inet_addr from_ip; 
                client_port = from_port;
                client_buf = Buffer.create 100;
                client_ok = true;
              } in
            
            let token = TcpBufferedSocket.create_token TcpBufferedSocket.unlimited_connection_manager in
            let sock = TcpBufferedSocket.create token "observer connection" s 
                (fun sock event ->
                  match event with
                    BASIC_EVENT (LTIMEOUT | RTIMEOUT) -> 
                      close sock Closed_for_timeout
                  | BASIC_EVENT (CLOSED _) -> 
                      lprintf "INFO SENT TO %s:%d\n" 
                        (Ip.to_string c.client_ip) c.client_port;
                      let s = Buffer.contents c.client_buf in
                      let len = String.length s in
                      if len > 1 then
                        let s = String.sub s 1 (len-1) in
                        let ip_firewall = c.client_ip in
                        
                        let t = gettimeofday () in
                        let t = Int64.of_float t in
                        
                        dump_record t s ip_firewall;                        
                        print_record t ip_firewall s
                  | _ -> ()
              )
            in
            set_reader sock (read_client_info c);
            set_lifetime sock 300.; 
            set_rtimeout sock 30.; 
            let b = Buffer.create 100 in
            Buffer.add_string b  !redirector_info;
            buf_ip b (peer_ip sock);
            let s = Buffer.contents b in
            let len = String.length s in
(*            lprintf "Sending %d bytes\n" len;  *)
            set_max_output_buffer sock (len + 100);
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
      lprintf "%d MLdonkey clients" !counter;
      (match !first_record, !last_record with
          Some t1, Some t2 ->
            lprintf " in %3.0Ld seconds" (Int64.sub t2 t1)
        | _ -> ());
      lprintf " on %d servers\n" !server_counter; 
  )
  

let servers_age = ref 60
let peers_age = ref 2
let _ =
  Arg.parse [
    "-ascii", Arg.Unit print_ascii, "";
    "-count", Arg.Unit count_records, "";
    "-server_age", Arg.Int ((:=) servers_age), " <int> : max server age (minutes) in servers.met";
    "-peer_age", Arg.Int ((:=) peers_age), " <int> : max server age (minutes) in servers.met";
    ] 
    (fun _ -> ()) ""


let dump_list new_hosts adder dumper =
  try
(*    lprintf "dump server list\n"; *)
    dumper (List2.tail_map adder (T.to_list new_hosts));
  with e ->
      lprintf "error: %s\n" (Printexc2.to_string e)
  
let dump_servers_list _ = 
  let module S = DonkeyImport.Server in
  (try motd_html := File.to_string "motd.html" with _ -> ());
  (try motd_conf := File.to_string "motd.conf" with _ -> ());
  update_redirector_info ();
  dump_list new_servers 
    (fun (ip, port) ->
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
  let store new_peers peers_ocl peers_file =
      dump_list new_peers 
      (fun key -> key)
    (fun list ->
        let buf = Buffer.create 100 in
        List.iter (fun (ip, port) ->
            Printf.bprintf buf "%s,%d,X\n" (Ip.to_string ip) port;
        ) list;
        peers_ocl := (Buffer.contents buf);
        File.from_string peers_file !peers_ocl;
    )
  in
  store new_overnet_peers 
    overnet_peers_ocl "overnet_peers.ocl";  
  store new_kademlia_peers 
    kademlia_peers_ocl "kademlia_peers.ocl";  
  update_redirector_info ()

  
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
        T.add new_servers (s.S.ip , s.S.port)
        ) (S.read file)
    with _ -> lprintf "Could not load old server list\n"; 
  end;
  MlUnix.set_signal  Sys.sigpipe (*Sys.Signal_ignore*)
    (Sys.Signal_handle (fun _ -> lprintf "SIGPIPE\n"));
  BasicSocket.add_timer 30. dump_servers_list;
  BasicSocket.add_timer 30. dump_peers_list;
  BasicSocket.add_infinite_timer 300. dump_servers_list;
  BasicSocket.add_infinite_timer 300. dump_peers_list;
  lprintf "Observer started\n";
  BasicSocket.loop ()
  
