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

open BasicSocket
open TcpBufferedSocket
open LittleEndian

open GuiTypes
open CommonTypes
open CommonGlobals
open CommonInteractive
open CommonNetwork
open CommonOptions

open DonkeyTypes
open DonkeyGlobals
open DonkeyOptions
open DonkeyMftp
open DonkeyProtoOvernet

let overnet_port = 
  define_option downloads_ini ["overnet_port"] "port for overnet" 
    int_option (2000 + Random.int 20000)
  
let udp_sock = ref None  

let buf = Buffer.create 2000
  
let udp_send ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
      let addr = Unix.ADDR_INET (Ip.to_inet_addr ip, port) in
      try
        Buffer.clear buf;
        buf_int8 buf 227;
        DonkeyProtoOvernet.write buf msg;
        let s = Buffer.contents buf in
        Printf.printf "Sending UDP to %s:%d" (Ip.to_string ip) port;
        print_newline ();
        dump s;
        print_newline ();
        let len = String.length s in
        UdpSocket.write sock s 0 (String.length s) addr
      with e ->
          Printf.printf "Exception %s in udp_send" (Printexc.to_string e);
          print_newline () 
          
let overnet_peers = define_option servers_ini 
    ["overnet_peers"] "List of overnet peers"
    (list_option (tuple4_option (Md4.option, Ip.option, int_option, int_option)))
  []
  
let (peers_queue : (Ip.t * int) Fifo.t) = Fifo.create ()
  
let peers_by_md4 = Hashtbl.create 1000

let add_peer peer =
  if not (Hashtbl.mem peers_by_md4 peer.peer_md4) then
    begin
      overnet_peers =:= (peer.peer_md4, peer.peer_ip, peer.peer_port, peer.peer_kind) :: !!overnet_peers;
      Hashtbl.add peers_by_md4 peer.peer_md4 peer;
      Fifo.put peers_queue (peer.peer_ip, peer.peer_port)
    end
    
let connected_peers = ref []
let nconnected_peers = ref 0

module XorSet = Set.Make (struct
      type t = Md4.t * peer
      let compare (m1,p1) (m2,p2) = 
        compare (m1,p1.peer_md4, p1.peer_ip) (m2,p2.peer_md4, p2.peer_ip)
    end)
      
type overnet_search = {
    search_md4 : Md4.t;
    search_known_peers : (Ip.t * int, peer) Hashtbl.t;
    mutable search_next_peers : XorSet.t;
  }
    
let overnet_searches = Hashtbl.create 13

let add_search_peer s p = 
  if not (Hashtbl.mem s.search_known_peers (p.peer_ip, p.peer_port)) then
    begin
      Hashtbl.add s.search_known_peers (p.peer_ip, p.peer_port) p;
      let distance = Md4.xor s.search_md4 p.peer_md4 in
      s.search_next_peers <- XorSet.add (distance, p) s.search_next_peers
    end
    
let udp_client_handler t p =
  match t with
  | OvernetConnectReply peers ->
      begin
        let peers = List.rev peers in
        match peers with
          peer :: tail ->
            add_peer peer;
            connected_peers := peer :: !connected_peers;
            Printf.printf "Connected to %s:%d" (Ip.to_string peer.peer_ip)
            peer.peer_port; print_newline ();
            incr nconnected_peers;
            List.iter add_peer tail
        | _ -> ()
      end
  
  | OvernetSearchReply (md4, peers) ->
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in
          let (s_ip, s_port) as s_addr =
            match p.UdpSocket.addr with
            | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
            | _ -> 
                Printf.printf "NO SENDER"; print_newline ();
                raise Not_found
          in
          let sender = try 
              Hashtbl.find s.search_known_peers s_addr 
            with _ -> Printf.printf "Sender firewalled ?"; print_newline ();
                raise Not_found in
          udp_send s_ip s_port (OvernetGetSearchResults (md4,0,0,100));
          Printf.printf "DISTANCES: origin %s from %s" (Md4.to_string
              (Md4.xor sender.peer_md4 md4))
          (Md4.to_string sender.peer_md4)
          ; print_newline ();
          List.iter (fun p ->
              Printf.printf "DISTANCES: peer   %s from %s" (Md4.to_string
                  (Md4.xor p.peer_md4 md4))
              (Md4.to_string p.peer_md4)
              ; print_newline ();
              add_search_peer s p
          ) peers
        with _ ->
            Printf.printf "NO SUCH SEARCH ??"; print_newline ();
      end
  
  | OvernetSearchResult (md4, r_md4, r_tags) ->
      
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in
          begin
            try
              let (s_ip, s_port) as s_addr =
                match p.UdpSocket.addr with
                | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
                | _ -> 
                    Printf.printf "NO SENDER"; print_newline ();
                    raise Not_found
              in
              let sender = try 
                  Hashtbl.find s.search_known_peers s_addr 
                with _ -> Printf.printf "Sender firewalled ?"; print_newline ();
                    raise Not_found in
              Printf.printf "REPLY FROM %s --> %s:%d" 
                (Md4.to_string sender.peer_md4) (Ip.to_string s_ip) s_port;
              print_newline ();
              
              List.iter (fun tag ->
                  if tag.tag_name = "loc" then begin
                      match tag.tag_value with
                        String bcp ->
                          if String2.starts_with bcp "bcp://" then
                            let bcp2 = String.sub bcp 6 (String.length bcp - 6) 
                            in
                            match String2.split_simplify bcp2 ':' with
                            | [_ ;ip;port]
                            | [ip;port] ->
                                let ip = Ip.of_string ip in
                                let port = int_of_string port in
                                Printf.printf "Creating client %s:%d"
                                  (Ip.to_string ip) port;
                                print_newline ();
                                
                                let c = new_client (Known_location (ip, port)) in
                                DonkeyClient.connect_client (client_ip None) [] c
                            | _ ->
                                Printf.printf "Ill formed bcp: %s" bcp;
                                print_newline ();
                          else begin
                              Printf.printf "Not a bcp!!!"; print_newline ();
                            end
                      | _ -> 
                          Printf.printf "NOot a string location ??"; 
                          print_newline ();
                    end
              ) r_tags;
            with _ -> 
                Printf.printf "No info on sender"; print_newline ();
          end;
          
          Printf.printf "RESULT:"; print_newline ();
          print_tags r_tags; print_newline ();
          
        with _ ->
            Printf.printf "NO SUCH SEARCH ??"; print_newline ();
      end
      
      
  | _ -> Printf.printf "UNUSED MESSAGE"; print_newline ()

let try_connect () =
  if !nconnected_peers < 5 && not (Fifo.empty peers_queue) then begin
      let (ip, port) = Fifo.take peers_queue in
      udp_send ip port  (OvernetConnect (!!client_md4, Ip.null, 
          !!overnet_port, 0))
    end
  
let recover_file (file : DonkeyTypes.file) = 
  if not (Hashtbl.mem overnet_searches file.file_md4) then
    let search = {
        search_md4 = file.file_md4;
        search_known_peers = Hashtbl.create 13;
        search_next_peers = XorSet.empty;
      } in
    List.iter (fun peer ->
        add_search_peer search peer;
    ) !connected_peers;
    Hashtbl.add overnet_searches file.file_md4 search
    
let recover_all_files () =
  List.iter (fun file ->
      if file_state file = FileDownloading then
        recover_file file
  ) !DonkeyGlobals.current_files

let query_min_peer s =
  try
    let (_,p) as e = XorSet.min_elt s.search_next_peers in
    Printf.printf "Query New Peer"; print_newline ();

    s.search_next_peers <- XorSet.remove e s.search_next_peers;
    udp_send p.peer_ip p.peer_port  (OvernetSearch (2, s.search_md4))     
  with _ ->
      Printf.printf "Search for %s finished" (Md4.to_string s.search_md4);
      print_newline ();
      Hashtbl.remove overnet_searches s.search_md4
  
let query_next_peers () =
  Hashtbl2.safe_iter (fun s ->
      query_min_peer s;
      query_min_peer s;
  ) overnet_searches
  
let enable enabler = 

  let peers = !!overnet_peers in
  overnet_peers =:= [];
  List.iter (fun (md4, ip, port, kind) ->
      
      add_peer 
      { peer_md4 = md4;
        peer_ip = ip;
        peer_port = port;
        peer_kind = kind;
      }) peers;
  
  udp_sock := Some (UdpSocket.create (Ip.to_inet_addr !!donkey_bind_addr)
    (!!overnet_port) 
    (udp_handler udp_client_handler));
  add_session_timer enabler 1. try_connect;
  add_session_timer enabler 10. query_next_peers
    
let disable () = 
  match !udp_sock with
    None -> ()
  | Some sock -> 
      udp_sock := None;
      UdpSocket.close sock "disabled";
      Hashtbl.clear peers_by_md4;
      Fifo.clear peers_queue
      
let _ =
  register_commands 
  
    [
    "boot", Arg_two (fun ip port o ->
        Fifo.put peers_queue (Ip.of_string ip, int_of_string port);
        try_connect ();
        "peer added"
    ), " <ip> <port> : add an Overnet peer";
    
    "overnet", Arg_none (fun o ->
        recover_all_files ();
        "Overnet search started"
    ), " : use Overnet to get more sources";
  ]

      
  