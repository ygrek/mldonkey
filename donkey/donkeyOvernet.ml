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

open Md4
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

  
(*
DONE
----
Fix: Never add ourself in the peer list
Fix: never add 0.0.0.0 or a local address for a distant peer
Fix: Search and Publish fixes
Fix: Removed BCP type2 support for now
Add packets type 12/13/27/28/29 support
Remove fixed overnet_md4 address
advertize every 20 minutes the network AND close peers that we are up
new peer storage system : there is now only one peer list (Hashtbl array) in the core
   enables powerful get_uniform_distrib() and get_local_distrib(md4) functions
   gives an efficient way to answer seaches
Remove the "connected network" schema : we only send Connect if necessary, we no longer 
   try to check a "connected peers subset" every x minutes.
*)

(*
TODO:
Add new opcode support 30/21/22
Readd BCP type2 support
Answer Search and FileSearch requests
Make the search more efficient/less aggressive
--
Update get_local_distrib for more accurate responses
Wait before sending 29 (is there a way to flush the udp_sock buffer ?)
*)

module XorSet = Set.Make (
  struct
    type t = Md4.t * peer
    let compare (m1,p1) (m2,p2) = 
      compare (m1,p1.peer_md4, p1.peer_ip) (m2,p2.peer_md4, p2.peer_ip)
  end
)

let overnet_publish_files = 
  define_option downloads_ini ["overnet_publish_files"] 
  "should mldonkey try to publish on overnet (development)" bool_option false

let global_peers_size = Array.make 256 0

(* overnet_md4 should be different from client_md4 for protocol safety reasons *)
let overnet_md4 = Md4.random()

let max_peer_by_peerblock = 64

  module PeerOption = struct
      
      let value_to_peer v = 
        match v with
          SmallList [md4; ip; port; kind]
        | List [md4; ip; port; kind] ->
	  let v_md4 = Md4.value_to_hash md4 in
	  let t_md4 = Md4.up v_md4 in
	  global_peers_size.(t_md4)<-global_peers_size.(t_md4)+1;
          (t_md4,v_md4, {
            peer_md4 = v_md4; 
              peer_ip = Ip.value_to_ip ip; 
              peer_port = value_to_int port; 
              peer_kind = value_to_int kind; 
              peer_last_msg = last_time ();
          });
        | _ -> assert false
            
    let peer_to_value v = 
      match v with 
	(_,_,p) ->
	  SmallList [
          Md4.hash_to_value p.peer_md4;
          Ip.ip_to_value p.peer_ip;
          int_to_value p.peer_port;
          int_to_value p.peer_kind
	]
        
    let t = define_option_class "Peer" value_to_peer peer_to_value
  end
  
let overnet_port = 
  define_option downloads_ini ["overnet_port"] "port for overnet" 
    int_option (2000 + Random.int 20000)

let overnet_max_known_peers = 
  define_option downloads_ini ["overnet_max_known_peers"] 
  "maximal number of peers to keep for overnet boot" 
    int_option 2000

let overnet_search_keyword = 
  define_option downloads_ini ["overnet_search_keyword"] 
  "allow extended search to search on overnet" bool_option false

let overnet_search_sources = 
  define_option downloads_ini ["overnet_search_sources"] 
  "allow extended search to search on overnet" bool_option false

  (*
let overnet_max_connected_peers = 
  define_option downloads_ini ["overnet_max_connected_peers"] 
  "maximal number of peers mldonkey should connect at beginning" 
    int_option 50
    *)

let global_peers : (Md4.t, peer) Hashtbl.t array Options.option_record = define_option servers_ini 
    ["overnet_peers"] "List of overnet peers"
    (hasharray_option Md4.null PeerOption.t) (Array.init 256
    (fun _ -> Hashtbl.create 10))

let overnet_search_timeout = 
  define_option downloads_ini ["overnet_search_timeout"] 
  "How long shoud a search on Overnet wait for the last reply before terminating"
    float_option 120. 
      
let overnet_query_peer_period = 
  define_option downloads_ini ["overnet_query_peer_period"] 
  "Period between two queries in the overnet tree (should not be set under 5)"
    float_option 5. 
      
let overnet_max_search_hits = 
  define_option downloads_ini ["overnet_max_search_hits"] 
  "Max number of hits in a search on Overnet"
    int_option 200 
      
let overnet_max_waiting_peers = 
  define_option downloads_ini ["overnet_max_waiting_peers"] 
  "Max number of peers waiting in a search on Overnet"
    int_option 50 
      
let gui_overnet_options_panel = 
  define_option downloads_ini ["gui_overnet_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
  Overnet section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
    (list_option (tuple3_option (string_option, string_option, string_option)))
  [
    "Port", shortname overnet_port, "T";
    "Search for keywords", shortname overnet_search_keyword, "B";
    "Search for sources", shortname overnet_search_sources, "B";
    "Publish files", shortname overnet_publish_files, "B";
(*    "Max Connected Peers", shortname overnet_max_connected_peers, "T"; *)
    "Search Timeout", shortname overnet_search_timeout, "T";
    "Search Internal Period", shortname overnet_query_peer_period, "T";
    "Verbose", shortname verbose_overnet, "B";
    "Search Max Hits", shortname overnet_max_search_hits, "T";
    "Search Max Waiting Peers", shortname overnet_max_waiting_peers, "T";
  ]
  
let udp_sock = ref None  

let buf = Buffer.create 2000
  
let udp_send ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
      try
        Buffer.clear buf;
        buf_int8 buf 227;
        DonkeyProtoOvernet.write buf msg;
        let s = Buffer.contents buf in
        if !!verbose_overnet then begin            
            Printf.printf "Sending UDP to %s:%d" (Ip.to_string ip) port;
            print_newline ();
(*            dump s;
            print_newline (); *)
          end;
        let len = String.length s in
        UdpSocket.write sock s ip port
      with e ->
          Printf.printf "Exception %s in udp_send" (Printexc.to_string e);
          print_newline () 

                        
  let search_hits = ref 0
  let source_hits = ref 0

let remove_old_global_peers () =
  for i=0 to 255 do 
    Hashtbl.iter ( fun a b -> 
      if (b.peer_last_msg < last_time () -. 14400.) && (global_peers_size.(i)>2) then 
	begin
	  (*Printf.printf "REM global_peers %s\n" (Md4.to_string a);*)
	  Hashtbl.remove !!global_peers.(i) a;
	  global_peers_size.(i) <- global_peers_size.(i)-1;
	end
    ) !!global_peers.(i)
  done

let get_local_distribution md4 = 
  let start_pos=(Md4.up md4) and peers = ref [] and size = ref 0 in
  (*FIXME we should use a XorSet for the first test*)
  Hashtbl.iter 
    ( fun a b -> if !size<16 then begin peers := b :: !peers; incr size; end) 
  !!global_peers.(start_pos);
  for i=1 to 127 do
    if !size<16 then
      Hashtbl.iter 
        ( fun a b -> if !size<16 then 
            begin peers := b :: !peers; incr size; end) 
      !!global_peers.((start_pos+i) mod 256);
    if !size<16 then
      Hashtbl.iter 
        ( fun a b -> if !size<16 then
            begin peers := b :: !peers; incr size; end) 
      !!global_peers.((start_pos-i+256) mod 256);
  done;
  !peers

let get_uniform_distribution () =
  let peers = ref [] in
  for i=0 to 15 do
    let size = ref 0 in
    for j=0 to 15 do
      size:=!size+global_peers_size.(16*i+j);
    done;
    if !size <> 0 then
      let pos=ref (Random.int !size) in
      for j=0 to 15 do
        if global_peers_size.(16*i+j) < !pos || !pos < 0 then 
          pos := !pos - global_peers_size.(16*i+j)
        else 
          Hashtbl.iter ( fun a b -> 
              if !pos=0 then peers := b :: !peers; decr pos ) 
          !!global_peers.(16*i+j);
      done;    
  done; 
  !peers

let find_oldest_peer hashtbl =
  let md4 = ref Md4.null and time = ref (last_time () ) in 
  Hashtbl.iter ( fun a b -> 
    if b.peer_last_msg < !time then 
      begin
	md4 := a;
	time := b.peer_last_msg;
      end
  ) hashtbl;
  !md4

let add_global_peer peer =
  if (peer.peer_ip <> !!donkey_bind_addr) && (peer.peer_md4 <> overnet_md4)
  then
      begin
      let i=Md4.up peer.peer_md4 in
      if Hashtbl.mem !!global_peers.(i) peer.peer_md4 then
	begin
	  (*Printf.printf "UPD global_peers: %s\n" (Md4.to_string peer.peer_md4);*)
	  (Hashtbl.find !!global_peers.(i) peer.peer_md4).peer_last_msg <- last_time();
	end
      else
	begin
	  if global_peers_size.(i) >= max_peer_by_peerblock then
	    begin
	      let p = find_oldest_peer !!global_peers.(i) in
	      (*Printf.printf "REM global_peers: %s\n" (Md4.to_string p);*)
	      Hashtbl.remove !!global_peers.(i) p;
	      global_peers_size.(i) <- global_peers_size.(i)-1;
	    end;
	  (*Printf.printf "ADD global_peers: %s\n" (Md4.to_string peer.peer_md4);*)
	  peer.peer_last_msg <- last_time();
          Hashtbl.add !!global_peers.(i) peer.peer_md4 peer;
 	  global_peers_size.(i) <- global_peers_size.(i)+1;
	end
      end
  else
    if !!verbose_overnet then
      begin
	Printf.printf "Tried to add myself as a peer: %s/%s %s/%s\n" 
	  (Ip.to_string peer.peer_ip) (Ip.to_string !!donkey_bind_addr)
	  (Md4.to_string peer.peer_md4) (Md4.to_string overnet_md4); 	    
      end
  
  
(* Replace private IP by public IPs in peer list *)
let change_private_address ip public_ip =
  if Ip.matches ip [(Ip.of_string "0.0.0.0"); (Ip.of_string "127.0.0.255"); 
		    (Ip.of_string "10.255.255.255"); (Ip.of_string "192.168.255.255") ] 
  then public_ip
  else ip

(*advertize an uniform distribution then a local distribution (around our MD4) that we are UP*)
let publicize_peers () =
  if (!!overnet_search_sources || !!overnet_search_keyword) then
      begin
      let global_dist = get_uniform_distribution () in
      let local_dist = get_local_distribution overnet_md4 in
      if !!verbose_overnet then 
        List.iter (fun a -> Printf.printf "UNIFORM DIST: %s" (Md4.to_string a.peer_md4); print_newline () ) global_dist; 
      List.iter (fun a -> 
          udp_send a.peer_ip a.peer_port
            (OvernetPublicize(overnet_md4,
              !!donkey_bind_addr,!!overnet_port, 0) ) ) 
      global_dist;
      if !!verbose_overnet then
        List.iter (fun a -> Printf.printf "LOCAL DIST: %s" (Md4.to_string a.peer_md4); print_newline () ) local_dist;
      List.iter (fun a -> 
          udp_send a.peer_ip a.peer_port
            (OvernetPublicize(overnet_md4,
              !!donkey_bind_addr,!!overnet_port, 0) ) ) 
      local_dist
    end
	  
(* If one peer block is running low, try to get new peers using Connect *)
let find_new_peers () =
  if !!overnet_search_sources || !!overnet_search_keyword then
    begin
      if !!verbose_overnet then begin
          Printf.printf "FINDING NEW PEERS"; print_newline ();
        end;
      try 
        for i=0 to 255 do 
          if global_peers_size.(i) <= 4 then raise Not_found;
        done
      with _ -> 
          begin
            let global_dist = get_uniform_distribution () in
            List.iter (fun a -> 
                udp_send a.peer_ip a.peer_port
                  (OvernetConnect(overnet_md4,
                    !!donkey_bind_addr,!!overnet_port, 0) ) ) 
            global_dist;
          end
    end
    
type search_for =
  FileSearch of file
| KeywordSearch of CommonTypes.search list

  
type overnet_search = {
    mutable search_last_packet : float;
    search_md4 : Md4.t;
    mutable search_kind : search_for;
    search_known_peers : (Ip.t * int, peer) Hashtbl.t;
    mutable search_next_peers : XorSet.t;
    mutable search_closed : bool;
    mutable search_waiting_peers : int;
    mutable search_nresults : int;
    mutable search_nreplies : int;
    mutable search_nqueries : int;
    mutable search_start_time : float;
    mutable search_results : (Md4.t, tag list) Hashtbl.t;

    mutable search_hits : int;
    mutable search_publish_files : file list;
(* should be search publish a file ? *)
    mutable search_publish_file : bool;
(* should the search query a file ? *)
(*    mutable search_query_files : bool; *)
  }
    
let overnet_searches = Hashtbl.create 13


let add_search_peer s p = 
  if not (Hashtbl.mem s.search_known_peers (p.peer_ip, p.peer_port))   &&
    not ( is_black_address p.peer_ip p.peer_port ) then
    begin
      Hashtbl.add s.search_known_peers (p.peer_ip, p.peer_port) p;
      let distance = Md4.xor s.search_md4 p.peer_md4 in
      if s.search_waiting_peers > !!overnet_max_waiting_peers then begin
          let (dd,pp) as e = XorSet.max_elt s.search_next_peers in
          if dd>distance then begin
              s.search_next_peers <- XorSet.remove (dd, pp) s.search_next_peers;     
              s.search_next_peers <- XorSet.add (distance, p) s.search_next_peers;   
            end        
        end
      else begin
          s.search_next_peers <- XorSet.add (distance, p) s.search_next_peers;
          s.search_waiting_peers <- s.search_waiting_peers +1        
        end
    end
    
let create_simple_search kind md4 =   
  let search = {
      search_last_packet = last_time ();
      search_md4 = md4;
      search_known_peers = Hashtbl.create 13;
      search_next_peers = XorSet.empty;
      search_kind = kind;
      search_closed = false;
      search_waiting_peers = 0;
      search_start_time = last_time ();
      search_nresults = 0;
      search_nreplies = 0;
      search_nqueries = 0;
      search_publish_files = [];
      search_publish_file = false;
(*      search_query_files = true;  *)
      search_results = Hashtbl.create 13;
      search_hits = 0;
    } in
(*   Printf.printf "STARTED SEARCH FOR %s" (Md4.to_string md4); print_newline (); *)
  Hashtbl.add overnet_searches md4 search;
  search

let create_search kind md4 =   
  let search = create_simple_search kind md4 in
  List.iter (fun peer -> add_search_peer search peer;) (get_local_distribution md4) ;
  search

let create_keyword_search w =   
  let md4 = Md4.string w in
  try
    Hashtbl.find overnet_searches md4 
  with _ ->
      let search = {
          search_last_packet = last_time ();
          search_md4 = md4;
          search_known_peers = Hashtbl.create 13;
          search_next_peers = XorSet.empty;
          search_kind = KeywordSearch [];
          search_closed = false;
          search_waiting_peers = 0;
          search_publish_files = [];
          search_publish_file = false;
(*        search_query_files = true;  *)
          search_results = Hashtbl.create 13;
          search_nreplies = 0;
          search_nqueries = 0;
          search_start_time = last_time ();
          search_nresults = 0;
          search_hits = 0;
        } in
(*   Printf.printf "STARTED SEARCH FOR %s" (Md4.to_string md4); print_newline (); *)
      Hashtbl.add overnet_searches md4 search;
      List.iter (fun peer -> add_search_peer search peer;) (get_local_distribution md4);
      search
      
  
let recover_file (file : DonkeyTypes.file) = 
  if !!overnet_search_sources then
    try
      let s = Hashtbl.find  overnet_searches file.file_md4 in
(*       s.search_query_file <- true *)
      ()
    with _ ->
        ignore (create_search (FileSearch file) file.file_md4)
  
let publish_file (file : DonkeyTypes.file) = 
  if !!overnet_publish_files then begin
      if !!overnet_search_sources then begin
          try
            let s = Hashtbl.find  overnet_searches file.file_md4 in
            s.search_publish_file <- true
          with _ ->
              let s = create_search (FileSearch file) file.file_md4 in
              s.search_publish_file <- true
        end;
      if !!overnet_search_keyword then begin
          let index_string w =
            let s = create_keyword_search w in
            if not (List.memq file s.search_publish_files) then
              s.search_publish_files <- file :: s.search_publish_files;
            ()
          in
          List.iter (fun name ->
              List.iter index_string (String2.stem name)
          ) file.file_filenames;
(* We should also probably index of tags fields *)
        end
    end
    
let recover_all_files () =
  if !!overnet_search_sources then
    List.iter (fun file ->
        if file_state file = FileDownloading then
          recover_file file          
    ) !DonkeyGlobals.current_files

let ip_of_udp_packet p =
  match p.UdpSocket.addr with
    Unix.ADDR_INET (inet, port) ->
      Ip.of_inet_addr inet
  | _ -> assert false
    
let port_of_udp_packet p =
  match p.UdpSocket.addr with
    Unix.ADDR_INET (inet, port) -> port
  | _ -> assert false
 
let udp_client_handler t p =
  match t with
  | OvernetConnectReply peers ->
      begin
        let peers = List.rev peers in
        match peers with
          peer :: tail ->
            
            let other_ip = ip_of_udp_packet p in
	    if !!verbose_overnet then begin
              Printf.printf "sender IP was %s" (Ip.to_string peer.peer_ip); print_newline ();	    
	    end;
	    peer.peer_ip <- (change_private_address peer.peer_ip other_ip);

            peer.peer_last_msg <- last_time ();
	    add_global_peer peer;
            if !!verbose_overnet then 
	      begin                
                Printf.printf "Connected to %s:%d" (Ip.to_string peer.peer_ip) peer.peer_port; 
		print_newline ();
              end;

	    (* everything else sould be added *)
            List.iter add_global_peer tail;

            Hashtbl.iter (fun _ search ->add_search_peer search peer) 
            overnet_searches;
            List.iter (fun file ->
                if file_state file = FileDownloading           
                   && not (file_enough_sources file)
                   && !!overnet_search_sources &&
                  not (Hashtbl.mem overnet_searches file.file_md4) then
                  let search = create_simple_search (FileSearch file) file.file_md4 in ()
            ) !DonkeyGlobals.current_files;
            Hashtbl.iter (fun _ s -> add_search_peer s peer) overnet_searches;
        | _ -> ()
      end

  | OvernetPublicize (md4, ip, port, kind ) ->
      begin
        let other_ip = ip_of_udp_packet p in
 	if !!verbose_overnet then
	  begin
	    Printf.printf "sender IP was %s - packet was from %s" 
	      (Ip.to_string ip) (Ip.to_string other_ip); 
	    print_newline ();	    
	  end;
	add_global_peer {
 	  peer_md4=md4;
	  peer_ip=(change_private_address ip other_ip);
	  peer_port=port;
	  peer_kind=kind;
	  peer_last_msg=last_time();
	};

	(* send the PUBLICIZED message *)
	udp_send other_ip port OvernetPublicized
     end
      
  | OvernetPublicized -> ()      
  | OvernetPublished (md4) -> ()      
  | OvernetSearchReply (md4, peers) ->
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in
          s.search_nreplies <- s.search_nreplies + 1;
          begin
            try
              let (s_ip, s_port) as s_addr =
                match p.UdpSocket.addr with
                | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
                | _ -> 
                    Printf.printf "NO SENDER"; print_newline ();
                    raise Not_found
              in
              let sender = Hashtbl.find s.search_known_peers s_addr in
              sender.peer_last_msg <- last_time ();
              begin
                match s.search_kind with
                  KeywordSearch sss ->
(* Here, we could check whether the searches are finished *)
                    if sss <> [] then
		      udp_send s_ip s_port (OvernetGetSearchResults (md4,0,0,100));

                    List.iter (fun file ->
                        if !!verbose_overnet then begin
                            Printf.printf "TRY TO PUBLISH FILE FOR KEYWORD";
                            print_newline ();
                          end;
                        udp_send s_ip s_port 
                          (OvernetPublish (md4, file.file_md4, DonkeyProtoCom.tag_file file))
                    ) s.search_publish_files
                | FileSearch file ->
                    if file_state file = FileDownloading then
                      udp_send s_ip s_port (OvernetGetSearchResults (md4,0,0,100));
                    if s.search_publish_file then
                      udp_send s_ip s_port 
                        (OvernetPublish (md4, overnet_md4,
                          [{
                              tag_name = "loc";
                              tag_value = String (
                                Printf.sprintf "bcp://%s:%d"
                                  (Ip.to_string (client_ip None))
                                !!port
                                
                              )
                            }]
                          ))
              end;
              if !!verbose_overnet then begin
                  Printf.printf "DISTANCES: origin %s from %s" (Md4.to_string
                      (Md4.xor sender.peer_md4 md4))
                  (Md4.to_string sender.peer_md4)
                  ; print_newline ();
                end;
            with _ -> 
                if !!verbose_overnet then begin
                    Printf.printf "Sender firewalled ?"; print_newline ();
                  end;
          end;
          List.iter (fun p ->
              if !!verbose_overnet then begin
                  Printf.printf "DISTANCES: peer %s from %s" (Md4.to_string
                      (Md4.xor p.peer_md4 md4))
                  (Md4.to_string p.peer_md4)
                  ; print_newline ();
                end;
              add_search_peer s p;
	      add_global_peer p
          ) peers
        with _ ->
            Printf.printf "NO SUCH SEARCH ??"; print_newline ();
      end
  
  | OvernetSearchResult (md4, r_md4, r_tags) ->
      
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in
          s.search_nresults <- s.search_nresults + 1;
          begin
            try
              begin
                try 
                  let (s_ip, s_port) as s_addr =
                    match p.UdpSocket.addr with
                    | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
                    | _ -> 
                        Printf.printf "NO SENDER"; print_newline ();
                        raise Not_found
                  in                  
                  let sender = 
                    Hashtbl.find s.search_known_peers s_addr 
                  in
                  sender.peer_last_msg <- last_time ();
                  if !!verbose_overnet then begin
                      Printf.printf "REPLY FROM %s --> %s:%d" 
                        (Md4.to_string sender.peer_md4) (Ip.to_string s_ip) s_port;
                      print_newline ();
                    end;
                with _ -> 
                    Printf.printf "Sender firewalled ?"; print_newline ();
              end;
              match s.search_kind with
                FileSearch file ->
                  
                  List.iter (fun tag ->
                      if tag.tag_name = "loc" then begin
                          match tag.tag_value with
                            String bcp ->
                              if String2.starts_with bcp "bcp://" then
                                let bcp2 = String.sub bcp 6 (String.length bcp - 6) 
                                in
                                match String2.split_simplify bcp2 ':' with
                                | [_;ip;port] ->
				    if !!verbose_overnet then begin
                                      Printf.printf "FIXME: Received a BCP type 2 %s for MD4 %s/%s" bcp
					(Md4.to_string md4) (Md4.to_string r_md4);
                                      print_newline ();
                                    end   
                                | [ip;port] ->
                                    incr source_hits;
                                    let ip = Ip.of_string ip in
                                    let port = int_of_string port in
                                    if !!verbose_overnet then begin
                                        Printf.printf "Creating client %s:%d"
                                          (Ip.to_string ip) port;
                                        print_newline ();
                                      end;
                                    if Ip.valid ip then
                                    let c = new_client (Known_location (ip, port)) in
				    c.client_brand <- Brand_overnet;
                                    if not (Intmap.mem (client_num c) file.file_sources) then
                                      new_source file c;
                                    
                                    DonkeyClient.connect_as_soon_as_possible c 
                                | _ ->
                                    Printf.printf "Ill formed bcp: %s" bcp;
                                    print_newline ();
                              else begin
                                  Printf.printf "Not a bcp !!!"; print_newline ();
                                end
                          | _ -> 
                              Printf.printf "NOot a string location ??"; 
                              print_newline ();
                        end
                  ) r_tags;
                  
              | KeywordSearch sss ->
                  incr search_hits;
                  if not (Hashtbl.mem s.search_results r_md4) then begin
                      s.search_hits <- s.search_hits + 1;
                      Hashtbl.add s.search_results r_md4 r_tags;
                      List.iter (fun ss ->
                          DonkeyOneFile.search_found ss r_md4 r_tags
                      ) sss
                    end;

(*
if ss.search_nresults > !!overnet_max_search_hits then
s.search_closed <- true
*)
            with _ -> 
                Printf.printf "No info on sender"; print_newline ();
          end;
          if !!verbose_overnet then 
	    begin
              Printf.printf "RESULT:"; print_newline ();
              print_tags r_tags; print_newline ();
            end;

        with _ ->
            Printf.printf "NO SUCH SEARCH ??"; print_newline ();
      end
      
  | OvernetGetMyIP ->
      begin
        let other_ip = ip_of_udp_packet p in
	let other_port = port_of_udp_packet p in
	(* send the answer *)
	(* FIXME : should be able to flush the UDP buffer*)
	udp_send other_ip other_port (OvernetGetMyIPResult other_ip);
	udp_send other_ip other_port OvernetGetMyIPDone;
     end

  | OvernetGetMyIPResult(ip) -> ()
  | OvernetGetMyIPDone -> ()
  | _ -> 
      if !!verbose_overnet then begin
          Printf.printf "UNUSED MESSAGE"; print_newline ()
        end
    
let query_min_peer s =
  if not s.search_closed then
    try
      let (_,p) as e = XorSet.min_elt s.search_next_peers in
      if !!verbose_overnet then begin
          Printf.printf "Query New Peer"; print_newline ();
        end;
      s.search_next_peers <- XorSet.remove e s.search_next_peers;
      s.search_waiting_peers <- s.search_waiting_peers - 1;
      s.search_last_packet <- last_time ();
      udp_send p.peer_ip p.peer_port  (OvernetSearch (2, s.search_md4));
      s.search_nqueries <- s.search_nqueries + 1;
      
  with _ -> ()
        
let query_next_peers () =
  Hashtbl2.safe_iter (fun s ->
      if s.search_hits < !!overnet_max_search_hits then begin
          query_min_peer s;
          query_min_peer s;
          if s.search_last_packet +. !!overnet_search_timeout < last_time () then
            begin
              if !!verbose_overnet then begin              
                  Printf.printf "Search for %s finished (%d queries, %d replies, %d results)"
                    (Md4.to_string s.search_md4)
                  s.search_nqueries s.search_nreplies s.search_nresults
                  ;
                  print_newline ();
                end;
              Hashtbl.remove overnet_searches s.search_md4
            end        
        end
  ) overnet_searches

  (*
let remove_old_connected_peers () =
  let old_connected_peers = !connected_peers in
  connected_peers := [];
  List.iter (fun (peer, ctime) ->
      if ctime < last_time () -. 1200. then begin
          decr nconnected_peers;
          Fifo.put peers_queue (peer.peer_ip, peer.peer_port);
        end else begin
          connected_peers := (peer, ctime) :: !connected_peers
        end) old_connected_peers
    *)

let publish_shared_files () = 
  if !!overnet_search_sources || !!overnet_search_keyword then begin
      let files = DonkeyShare.all_shared () in
      List.iter publish_file files
    end

    (*
let purge_peer_list () =
  let list = Sort.list (fun p1 p2 ->
        p2.peer_last_msg > p1.peer_last_msg
    ) !!overnet_peers in
  let recent_peers, old = 
    List2.cut !!overnet_max_known_peers list
  in
  overnet_peers =:= recent_peers
    *)

(*
let enable enabler = 

  let peers = !!overnet_peers in
  overnet_peers =:= [];
  List.iter add_peer peers;
  
  let sock =   (UdpSocket.create (Ip.to_inet_addr !!donkey_bind_addr)
    (!!overnet_port) 
      (udp_handler udp_client_handler)) in
  udp_sock := Some sock;
  UdpSocket.set_write_controler sock udp_write_controler;
  add_session_timer enabler 1. try_connect;
  add_session_option_timer enabler overnet_query_peer_period query_next_peers;
  add_session_timer enabler 1200. (fun _ ->
      recover_all_files ();
      remove_old_connected_peers ();
      purge_peer_list ();
      publish_shared_files ()
  );
  add_timer 30. (fun timer -> publish_shared_files ())
  *)
    
  let enable enabler = 
  let sock = (UdpSocket.create (Ip.to_inet_addr !!donkey_bind_addr)
    (!!overnet_port) (udp_handler udp_client_handler)) in
    udp_sock := Some sock;
    UdpSocket.set_write_controler sock udp_write_controler;
  add_session_timer enabler 120. find_new_peers;
    add_session_option_timer enabler overnet_query_peer_period query_next_peers;
    add_session_timer enabler 1200. (fun _ ->
        recover_all_files ();
      remove_old_global_peers ();
      publicize_peers ();
        publish_shared_files ()
    );
  add_timer 60. (fun timer -> publicize_peers ());
  add_timer 60. (fun timer -> publish_shared_files ());
  add_timer 60. (fun timer -> find_new_peers ())


let _ =
  option_hook overnet_query_peer_period (fun _ ->
      if !!overnet_query_peer_period < 5. then
        overnet_query_peer_period =:= 5.)
  
let disable () = 
  match !udp_sock with
    None -> ()
  | Some sock -> 
      udp_sock := None;
      UdpSocket.close sock "disabled"

let parse_overnet_url url =
  match String2.split (String.escaped url) '|' with
  | "fha://" :: "boot" :: name :: port :: _
  | "boot" :: name :: port :: _ ->
      let ip = Ip.from_name name in
      let port = int_of_string port in
      udp_send ip port (OvernetConnect(overnet_md4,!!donkey_bind_addr,!!overnet_port, 0));
      true
  | _ -> false
      

let connected_peers () =
  List.map (fun p -> p.peer_ip, p.peer_port)  (get_uniform_distribution ())
      
let _ =
  register_commands 
    [
    "boot", Arg_two (fun ip port o ->
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        udp_send ip port (OvernetConnect(overnet_md4,!!donkey_bind_addr,!!overnet_port, 0));
        Printf.sprintf "peer %s:%d added" (Ip.to_string ip) port
    ), " <ip> <port> : add an Overnet peer";
    
    
    "ovlink", Arg_multiple (fun args o ->        
        let buf = o.conn_buf in
        let url = String2.unsplit args ' ' in
        if parse_overnet_url url then
          "download started"
        else "bad syntax"
    ), " <fhalink> : download fha:// link";
    
    "ovstats", Arg_none (fun o ->
        let buf = o.conn_buf and sum = ref 0 in
        Printf.bprintf buf "Overnet statistics:\n"; 
        Printf.bprintf buf "  Search hits: %d\n" !search_hits;
        Printf.bprintf buf "  Source hits: %d\n" !source_hits;
	Printf.bprintf buf "  peers blocks :\n";
	for i=0 to 15 do	  
	  Printf.bprintf buf "    ";
	  for j=0 to 15 do
	    Printf.bprintf buf "%3d: %3d " (i*16+j) global_peers_size.(i*16+j);
	    sum := !sum + global_peers_size.(i*16+j);
	  done;
	  Printf.bprintf buf "\n";
	done;
	Printf.bprintf buf "  Number of overnet peers = %d\n" !sum;

        Hashtbl.iter (fun _ s ->
            Printf.bprintf buf 
            "Search %s for %s (%d queries, %d replies, %d results, %d remaining) %s%s\n"
            
              (match s.search_kind with
                KeywordSearch _ -> "keyword"
              | FileSearch _ -> "file")
            (Md4.to_string s.search_md4)
            s.search_nqueries s.search_nreplies s.search_nresults
              (XorSet.cardinal s.search_next_peers)
            (match s.search_kind, s.search_publish_files with
              | KeywordSearch [], _ :: _  -> "publish "
              | KeywordSearch (_ :: _) , []  -> "query "
              | KeywordSearch (_ :: _), _ :: _  -> "publish+query "
              | KeywordSearch [], []  -> "??? "
              | FileSearch _, _ -> "")
            (if s.search_publish_file then "file_publish " else "")
            ;
        ) overnet_searches;
        
        "";
    ), ": Overnet Stats";
    
    "ovweb", Arg_multiple (fun args o ->
        let urls =
          match args with
            [] ->
              [
                "http://members.lycos.co.uk/appbyhp2/FlockHelpApp/contact-files/contact.ocl" ;
                "http://overnet.wiretapped.us/contact.ocl";
              ]
          | _ -> args 
        in
        List.iter (fun url -> 
            Printf.bprintf o.conn_buf "Loading %s\n" url; 
            load_url "ocl" url) urls;
        "web boot started"
    ), " <urls>: download .ocl URLS (no arg load default)";

    "ovmd4", Arg_none (fun o -> "MD4 is " ^ (Md4.to_string overnet_md4);
     ), " get client MD4 address on the overnet network";

    "ovdump", Arg_none (fun o -> 
      let buf = o.conn_buf in
      begin
	Printf.bprintf buf "overnet dump\n";
	for i=0 to 255 do 
	  if global_peers_size.(i) <> 0 then
	    Printf.bprintf buf "global_peers n°%d : %d elements\n" i global_peers_size.(i);
	  Hashtbl.iter ( fun a b -> Printf.bprintf buf "global_peers n°%d %s %f\n" 
	      i (Md4.to_string a) b.peer_last_msg) !!global_peers.(i);
	done;
      end;
      ""), "dump overnet global_peer table";

    "ovtst", Arg_none (fun o -> 
      for i=1 to 92 do
	add_global_peer {
 	peer_md4=Md4.random();
	peer_ip=Ip.of_string "10.10.10.10";
	peer_port=1;
	peer_kind=0;
	peer_last_msg=1.;
      };
      done;
      ""), "dump overnet global_peer table";

    "ovadv", Arg_none (fun o -> publicize_peers (); ""), "dump overnet global_peer table";

  ];
  add_web_kind "ocl" (fun filename ->
      Printf.printf "OCL"; print_newline ();
      let s = File.to_string filename in
      let s = String2.replace s '"' "" in
      let lines = String2.split_simplify s '\n' in
      List.iter (fun s ->
          match String2.split_simplify s ',' with
            name :: port :: _ ->
              let ip = Ip.from_name name in
              let port = int_of_string port in
              udp_send ip port (OvernetConnect(overnet_md4,!!donkey_bind_addr,!!overnet_port, 0));
          | _ -> 
              Printf.printf "BAD LINE ocl: %s" s;
              print_newline ();
      ) lines
  )

let search_keyword ss w =
  let s = create_keyword_search w in
  Hashtbl.iter (fun r_md4 r_tags ->
      DonkeyOneFile.search_found ss r_md4 r_tags
  ) s.search_results;
  match s.search_kind with
    KeywordSearch sss ->
      s.search_kind <- KeywordSearch (ss :: sss)
  | _ -> ()


let all_overnet_searches = ref ([] : int list)
  
let overnet_search (ss : search) =
  if !!overnet_search_keyword && 
    not (List.mem ss.search_num !all_overnet_searches) then
    let q = ss.search_query in
    all_overnet_searches := ss.search_num :: !all_overnet_searches;
    let ws = keywords_of_query q in
    List.iter (fun w ->
        search_keyword ss w;
    ) ws
