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

open AnyEndian
open Printf2
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
open CommonSources
  
open DonkeyTypes
open DonkeyGlobals
open DonkeyOptions
open DonkeyMftp
open DonkeyProtoOvernet

(*
TODO OVERNET
----
Enforce correct search
Add BCP type2 support again
Check 15/16/18/19/1A/1B/1C/1D/1E

overnet_md4 unchanged between sessions
save published_keyword_table to file keywords.ini
loading keywords.ini, update the "loc" tag
remove keywords too old (check if they are still valid)
*)

(*
S 18h (24) : md4 (md4 of the firewalled server) port (my TCP port) -verified-
R 19h (25) : md4
OR
R 1Ah (26) : md4

R 11h (17) : GetResult answer, the list always end by a 12h packet (No Result) -verified-
R 12h (18) : NoResult -verified-

S 1Bh (27) : port (my TCP port) -verified-
R 1Ch (28) : ip (returns my ip) -verified-

R 1Dh (29) : (nothing) -verified- (=> BCP type 1)
OR
R 1Eh (30) : (nothing) (=> BCP type 2)

(in BCP type 2 case)
S 15h (21) : md4 (my MD4) IP (my ip) port (my port UDP)
R 16h (22) : port (his ??? TCP port)
*)
  
module XorSet = Set.Make (
  struct
    type t = Md4.t * peer
    let compare (m1,p1) (m2,p2) = 
      compare (m1,p1.peer_md4, p1.peer_ip) (m2,p2.peer_md4, p2.peer_ip)
  end
)

module XorMd4Set = Set.Make (
    struct
      type t = Md4.t (* distance *) * Md4.t (* md4 *)
      let compare (m1,p1) (m2,p2) = 
        let c = compare m1 m2 in
        if c = 0 then compare p1 p2 else c
    end
  )

module XorMd4Map = Map.Make (
    struct
      type t = Md4.t
      let compare = compare
    end
  )
  

type search_for =
  FileSearch of file
| KeywordSearch of CommonTypes.search list
  
type overnet_search = {
    search_md4 : Md4.t;
    mutable search_kind : search_for;
    mutable search_last_insert : int;

    mutable search_not_asked_peers : XorSet.t;
    mutable search_asked_peers : XorSet.t;
    mutable search_done_peers : XorSet.t;

    (* nb of peers in search_not_asked_peers + search_asked_peers + search_done_peers *)
    mutable search_total_peers : int;

    (* already tested peers, either they are in asked/not_asked/done, either they are useless*)
    mutable search_known_peers : (Ip.t*int, peer) Hashtbl.t;

    mutable search_nresults : int;
    mutable search_results : (Md4.t, tag list) Hashtbl.t;
    mutable search_hits : int;
    mutable search_publish_files : file list;
    mutable search_publish_file : bool;
  }

(********************************************************************


                     STATIC MEMORY
  

*********************************************************************)
  
let min_peers_per_block = 16 (* was 2 *)
let min_peers_before_connect = 5
let max_searches_for_publish = 5
let search_max_queries = 64
  
let global_peers_size = Array.make 256 0

let is_enabled = ref false


(********************************************************************


                           OPTIONS
  

*********************************************************************)

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

let overnet_section = file_section donkey_ini ["Overnet"] "Overnet options"
  
let overnet_store_size = 
  define_option overnet_section ["overnet_store_size"] "Size of the filename storage used to answer queries" 
    int_option 2000
  
let overnet_protocol_connect_version = 
  define_option overnet_section ["overnet_protocol_connect_version"] 
    "The protocol version sent on Overnet connections"
    int_option 1044
  
let overnet_protocol_connectreply_version = 
  define_option overnet_section ["overnet_protocol_connectreply_version"] 
    "The protocol version sent on Overnet connections replies"
    int_option 44

let overnet_port = 
  define_option overnet_section ["overnet_port"] "port for overnet" 
    int_option (2000 + Random.int 20000)

let overnet_max_known_peers = 
  define_option overnet_section ["overnet_max_known_peers"] 
  "maximal number of peers to keep overnet connected (should be >2048)" 
    int_option 8192

let overnet_search_keyword = 
  define_option overnet_section ["overnet_search_keyword"] 
  "allow extended search to search on overnet" bool_option false

let overnet_search_timeout = 
  define_option overnet_section ["overnet_search_timeout"] 
  "How long shoud a search on Overnet wait for the last answer before terminating"
    int_option 140
      
let overnet_query_peer_period = 
  define_option overnet_section ["overnet_query_peer_period"] 
  "Period between two queries in the overnet tree (should not be set under 5)"
    float_option 5.
      
let overnet_max_search_hits = 
  define_option overnet_section ["overnet_max_search_hits"] 
  "Max number of hits in a search on Overnet"
    int_option 200 
      
let overnet_republish = 
  define_option overnet_section ["overnet_republish"] 
  "Interval (in seconds) before republish files"
    float_option 10800.

let overnet_exclude_peers = 
  define_option overnet_section ["overnet_exclude_peers"] 
  "These IP addresses cannot be peers. Elements are separated by spaces, wildcard=255 ie: use 192.168.0.255 for 192.168.0.* "
    ip_list_option [Ip.of_string "1.0.0.0"]   

let overnet_md4 = define_option overnet_section ["overnet_md4"]
    "The MD4 of the Overnet client" Md4.option (Md4.random ())

let gui_overnet_options_panel = 
  (*
  define_option overnet_section ["gui_overnet_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
  Overnet section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
    "Enable Overnet", shortname enable_overnet, "B";
    "Port", shortname overnet_port, "T";
    "Search for keywords", shortname overnet_search_keyword, "B";
    "Search Timeout", shortname overnet_search_timeout, "T";
    "Search Internal Period", shortname overnet_query_peer_period, "T";
    "Search Max Hits", shortname overnet_max_search_hits, "T";
  ]
      
let overnet_options_version = 
  define_option overnet_section ["overnet_options_version"] 
    "(internal)"
    int_option 0

  (*
let _ = 
  Options.set_string_wrappers overnet_exclude_peers
    (fun list ->
      List.fold_left
        (fun s ip ->
          Printf.sprintf "%s %s" (Ip.to_string ip) s
        ) "" list
    )
    (fun s ->
      let list = String2.tokens s in
      List.map (fun ip -> Ip.of_string ip) list
    )
*)
    
(********************************************************************


                        MUTABLE STRUCTURES
  

*********************************************************************)

  
let global_peers : (Md4.t, peer) Hashtbl.t array Options.option_record = 
  define_option servers_section 
    ["overnet_peers"] "List of overnet peers"
    (hasharray_option Md4.null PeerOption.t) 
    (Array.init 256 (fun _ -> Hashtbl.create 10))

let boot_peers = ref []
  
(*let firewalled_overnet_peers = Hashtbl.create 13*)

let published_keyword_set = ref XorMd4Set.empty
let published_keyword_table = Hashtbl.create 103
let published_keyword_size = ref 0

    
let search_hits = ref 0
let source_hits = ref 0

let all_overnet_searches = ref ([] : int list)
    
let udp_sock = ref None  
let tcp_sock = ref None
  
let udp_buf = Buffer.create 2000

      
let overnet_searches = Hashtbl.create 13
let files_to_be_published = ref []

(********************************************************************


                           FUNCTIONS
  

*********************************************************************)

let debug_client ip = false
(*  Ip.matches ip !!overnet_debug_clients *)

let udp_send ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
      try
        Buffer.clear udp_buf;
        buf_int8 udp_buf 227;
        DonkeyProtoOvernet.write udp_buf msg;
        let s = Buffer.contents udp_buf in
        if !verbose_overnet && debug_client ip then 
        begin            
                    lprintf "Sending UDP to %s:%d type %d (0x%02X)" 
                      (Ip.to_string ip) port (get_uint8 s 1) (get_uint8 s 1);
                    lprint_newline ();
(*dump s; lprint_newline ();*)
	  end;
        let len = String.length s in
        UdpSocket.write sock s ip port
      with e ->
          lprintf "Exception %s in udp_send" (Printexc2.to_string e);
          lprint_newline () 
            
(* Find nb peers close to md4 in the peer set *)
let get_local_distribution md4 nb = 
  let start_pos=Md4.up md4
  and peers = ref [] 
  and size = ref 0 in
  begin
    try
      let xorset = ref XorSet.empty in
      Hashtbl.iter 
	( fun a b -> xorset:=XorSet.add (Md4.xor md4 a,b) !xorset ) !!global_peers.(start_pos);
      
      begin
	try 
	  for i=1 to nb do
	    let (_,b) as e = XorSet.min_elt !xorset in
	    peers := b :: !peers;
	    xorset:=XorSet.remove e !xorset;
	    incr size;
	  done
	with _ -> ()	  
      end;
(*      
      for i=1 to 127 do
	Hashtbl.iter 
	  ( fun a b -> if !size<nb then begin peers := b :: !peers; incr size; end else raise Not_found) 
	  !!global_peers.((start_pos+i) mod 256);
	Hashtbl.iter 
	  ( fun a b -> if !size<nb then begin peers := b :: !peers; incr size; end else raise Not_found) 
	  !!global_peers.((start_pos-i+256) mod 256);
      done;
*)
    with _ -> ()
  end;
  !peers

let get_uniform_distribution () =
  let peers = ref [] in
  for i=0 to 255 do
    let size = global_peers_size.(i) in
    let size = if size > 0 then size else 1 in
    let pos = ref (Random.int size) in
    Hashtbl.iter (fun a b -> if !pos=0 then peers := b :: !peers; decr pos) !!global_peers.(i)
  done;
  !peers

let find_oldest_peer hashtbl =
  let md4 = ref Md4.null and 
      time = ref (last_time () ) in 
  Hashtbl.iter ( fun a b -> 
    if b.peer_last_msg < !time then 
      begin
	md4 := a;
	time := b.peer_last_msg;
      end
  ) hashtbl;
  !md4

let private_address ip =
  Ip.matches ip [(Ip.of_string "0.0.0.0"); (Ip.of_string "127.0.0.255"); 
		 (Ip.of_string "10.255.255.255"); (Ip.of_string "192.168.255.255") ] 

(* Replace private IP by public IPs in peer list *)
let change_private_address ip public_ip =
  if private_address ip then public_ip
  else ip

let add_global_peer peer =
  if not ( is_black_address peer.peer_ip peer.peer_port ) &&
     not ( private_address peer.peer_ip ) &&
     not (Ip.matches peer.peer_ip !!overnet_exclude_peers)
  then
    begin
      if (peer.peer_ip <> client_ip None) && (peer.peer_md4 <> !!overnet_md4) then
	begin
	  let i=Md4.up peer.peer_md4 in
	  if Hashtbl.mem !!global_peers.(i) peer.peer_md4 then
	    begin
            let p = Hashtbl.find !!global_peers.(i) peer.peer_md4 in
            if peer.peer_kind <= p.peer_kind then
            begin
              peer.peer_last_msg <- last_time();
              Hashtbl.replace !!global_peers.(i) peer.peer_md4 peer;
	      (*lprintf "UPD global_peers: %s\n" (Md4.to_string peer.peer_md4);*)
            end
	    end
	  else
	    begin
	      while global_peers_size.(i) >= (!!overnet_max_known_peers/256) do
              let old = find_oldest_peer !!global_peers.(i) in
              let p = Hashtbl.find !!global_peers.(i) old in
(* decrease reliability of this peer and eventually remove it *)
              if p.peer_kind >= 4 then begin
                Hashtbl.remove !!global_peers.(i) old;
                global_peers_size.(i) <- global_peers_size.(i)-1
              end
              else begin
                if !verbose_overnet then
                  lprintf "Increasing peer kind for block %0.2x\n" i;
                Hashtbl.iter
                  (fun md4 p -> p.peer_kind <- p.peer_kind + 1)
                  !!global_peers.(i);
              end
		done;
(*            lprintf "ADD global_peers: %s\n" (Md4.to_string peer.peer_md4); *)
	      peer.peer_last_msg <- last_time();
            Hashtbl.add !!global_peers.(i) peer.peer_md4 peer;
 	      global_peers_size.(i) <- global_peers_size.(i) + 1
	    end
	end
      else
        if !verbose_overnet then
        begin
	      lprintf "Tried to add myself as a peer: %s/%s %s/%s\n" 
	        (Ip.to_string peer.peer_ip) (Ip.to_string (client_ip None))
	        (Md4.to_string peer.peer_md4) (Md4.to_string !!overnet_md4)
        end
    end
  
(*advertize an uniform distribution then a local distribution (around our MD4) that we are UP*)
let publicize_peers () =
  let global_dist = get_uniform_distribution () in
  let local_dist = get_local_distribution !!overnet_md4 search_max_queries in
  List.iter (fun a -> 
      udp_send a.peer_ip a.peer_port
        (OvernetPublicize(!!overnet_md4,
          Ip.null,!overnet_client_port, 0) ) ) 
  global_dist;
  List.iter
    (fun a -> 
      udp_send a.peer_ip a.peer_port
        (OvernetPublicize(!!overnet_md4, Ip.null, !overnet_client_port, 0))) 
    local_dist
  
(* If one peer block is running low, try to get new peers using Connect *)
let find_new_peers () =
  try
    for i=0 to 255 do 
      if global_peers_size.(i) < min_peers_before_connect then raise Not_found;
    done
  with _ -> 
      begin
        if !verbose_overnet then
            lprintf "FINDING NEW PEERS\n";

        List.iter (fun a -> udp_send a.peer_ip a.peer_port 
              (OvernetConnect(!!overnet_md4,client_ip None,!overnet_client_port, 0)))
          (get_uniform_distribution ())
      end

let add_search_peer s p = 
  if not (Hashtbl.mem s.search_known_peers (p.peer_ip,p.peer_port)) &&
    not (is_black_address p.peer_ip p.peer_port) && 
    not (private_address p.peer_ip) then
    begin
      Hashtbl.add s.search_known_peers (p.peer_ip,p.peer_port) p;
      let distance = Md4.xor s.search_md4 p.peer_md4 in
      if s.search_total_peers >= search_max_queries then 
	begin
          let (dd1,pp1) as e1 = 
	          if (XorSet.cardinal s.search_not_asked_peers)=0 then (Md4.null,p)
		  else XorSet.max_elt s.search_not_asked_peers and
	      (dd2,pp2) as e2 = 
	          if (XorSet.cardinal s.search_asked_peers)=0 then (Md4.null,p)
		  else XorSet.max_elt s.search_asked_peers and
	      (dd3,pp3) as e3 = 
	          if (XorSet.cardinal s.search_done_peers)=0 then (Md4.null,p)
		  else XorSet.max_elt s.search_done_peers in

          if dd1>distance then 
	    begin
	    (* Better than one not-yet-asked peer : replace it ! *)
              s.search_not_asked_peers <- XorSet.remove (dd1, pp1) s.search_not_asked_peers;
              s.search_not_asked_peers <- XorSet.add (distance, p) s.search_not_asked_peers;
	      (*lprintf "add_search_peer(%s) : add peer at distance %s [REPLACE]\n"
		(Md4.to_string s.search_md4) (Md4.to_string distance);*)

            end      
	  else if dd2>distance || dd3>distance then
	    begin
	      (* We have already asked a peer which is worse. Anyway, we shouldn't drop this one*)
	      s.search_not_asked_peers <- XorSet.add (distance, p) s.search_not_asked_peers;
	      s.search_total_peers <- s.search_total_peers + 1 ;
	      (*lprintf "add_search_peer(%s) : add peer at distance %s [ADD]\n"
		(Md4.to_string s.search_md4) (Md4.to_string distance);*)
	    end
        end
      else 
	begin
          s.search_not_asked_peers <- XorSet.add (distance, p) s.search_not_asked_peers;
	  s.search_total_peers <- s.search_total_peers + 1 ;
(*
	  lprintf "add_search_peer(%s) : add peer at distance %s [NOT ENOUGH]\n"
	    (Md4.to_string s.search_md4) (Md4.to_string distance);
*)	    
        end
    end
    
let create_search kind md4 =   
  let search = {
    search_md4 = md4;
    search_kind = kind;
    search_last_insert=last_time();

    search_not_asked_peers=XorSet.empty;
    search_asked_peers=XorSet.empty;
    search_done_peers=XorSet.empty;
    search_total_peers=0;
    search_known_peers = Hashtbl.create 32;

    search_nresults = 0;
    search_publish_files = [];
    search_publish_file = false;
    search_results = Hashtbl.create 13;
    search_hits = 0;
  } in
  List.iter (fun peer -> add_search_peer search peer) (get_local_distribution md4 search_max_queries);
  search

let create_keyword_search w =   
  let md4 = Md4.string w in
  try
    let search=Hashtbl.find overnet_searches md4 in
    List.iter (fun peer -> add_search_peer search peer) (get_local_distribution md4 search_max_queries);
    search
  with _ ->
      let search = {
        search_md4 = md4;
        search_kind = KeywordSearch [];
	search_last_insert=last_time();

	search_not_asked_peers=XorSet.empty;
	search_asked_peers=XorSet.empty;
	search_done_peers=XorSet.empty;
	search_total_peers=0;
	search_known_peers = Hashtbl.create 32;

        search_publish_files = [];
        search_publish_file = false;
        search_results = Hashtbl.create 13;
        search_nresults = 0;
        search_hits = 0;
      } in
      List.iter (fun peer -> add_search_peer search peer) (get_local_distribution md4 search_max_queries);
      search  
	
let store_published_file kw_md4 file_md4 file_tags time =
  let distance = Md4.xor !!overnet_md4 kw_md4 in

  if !verbose_overnet then 
    begin
      lprintf "PUBLISH at %s (dist=%s)" (Md4.to_string kw_md4) (Md4.to_string distance);
      lprint_newline ();
    end;

  try
    let (size, files) = Hashtbl.find published_keyword_table kw_md4 in
    
    try
      let (tags, file_time) = XorMd4Map.find file_md4 !files in
      file_time := time
    with _ -> 
        incr size;
        files := XorMd4Map.add file_md4 (file_tags, ref time) !files
    
  with _ ->
      let do_it = 
        if !published_keyword_size > !!overnet_store_size then
          
(* Take the keyword which is the furthest from us, and remove it if
    it is farther than the new file *)
          
          let (max_elt, max_md4) as e = 
            XorMd4Set.min_elt !published_keyword_set in
          if distance < max_elt then begin
              published_keyword_set := XorMd4Set.remove e 
                !published_keyword_set;
              Hashtbl.remove published_keyword_table max_md4;
              decr published_keyword_size;
              true
            end else false
        else true
      in

      if do_it then begin
        if !verbose_overnet then 
          lprintf "PUBLISH at %s (dist=%s)\n" (Md4.to_string kw_md4) (Md4.to_string distance);
          incr published_keyword_size;
          Hashtbl.add published_keyword_table kw_md4 
            (ref 1, ref (XorMd4Map.add file_md4 (file_tags, ref time) 
            XorMd4Map.empty));
          published_keyword_set := XorMd4Set.add 
            (Md4.xor !!overnet_md4 kw_md4, kw_md4)
          !published_keyword_set
        end
    
let get_results_from_query ip port kw_md4 min max =
  try
    let (size, files) = Hashtbl.find published_keyword_table kw_md4 in
    let n_sent = ref 0 in
    try
      XorMd4Map.iter
        (fun file_md4 (file_tags, time) ->
          incr n_sent;
          if !n_sent > min then
            udp_send ip port (OvernetSearchResult(kw_md4,file_md4,file_tags));
          if !n_sent > max then raise Exit)
        !files
    with _ -> ()
  with _ ->
    udp_send ip port (OvernetNoResult(kw_md4))

  
let recover_file (file : DonkeyTypes.file) = 
  try
    let s = Hashtbl.find overnet_searches file.file_md4 in ()
  with _ ->
      let s=create_search (FileSearch file) file.file_md4 in
      Hashtbl.add overnet_searches s.search_md4 s
      
let publish_file (file : DonkeyTypes.file) = 
  begin
    try
      begin
        let s = Hashtbl.find overnet_searches file.file_md4 in
        s.search_publish_file <- true;	    
      end
    with _ ->
        begin
          let s = create_search (FileSearch file) file.file_md4 in
          s.search_publish_file <- true;
          files_to_be_published := s :: !files_to_be_published;
        end
  end;
  if !!overnet_search_keyword then 
    begin
      let index_string w =
        let s = create_keyword_search w in
        s.search_publish_files <- file :: s.search_publish_files;
        files_to_be_published := s :: !files_to_be_published;	 
      in
      List.iter index_string (String2.stem (file_best_name file))
    end
    
let recover_all_files () =
  List.iter (fun file ->
      if file_state file = FileDownloading then
        recover_file file          
  ) !DonkeyGlobals.current_files
  
let ip_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) ->
      Ip.of_inet_addr inet
  | _ -> assert false
    
let port_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) -> port
  | _ -> assert false
      
let rec get_filename tags =
  match tags 
  with 
    [] -> ""
  | tag :: tags ->
      begin
        if tag.tag_name = "filename"
        then 
          match tag.tag_value with 
          | String s -> s
          | _ -> ""
        else get_filename tags	
      end
      
let check_filename q tags =
  let filename = get_filename tags in 
  let rec check_iter q =
    match q with
      QAnd (q1, q2) -> (check_iter q1) && (check_iter q2)
    | QOr (q1, q2) ->  (check_iter q1) || (check_iter q2)
    | QAndNot (q1, q2) -> (check_iter q1) && (not (check_iter q2))
    | QHasWord s -> 
        begin
          if !verbose_overnet then begin
              lprintf "%s CONTAINS[%s]" filename s; 
              lprint_newline ();
            end;
	  true
	end
    | _ -> true
  in
  check_iter q

let udp_client_handler t p =
  let other_ip = ip_of_udp_packet p in
  let other_port = port_of_udp_packet p in
  if !verbose_overnet && debug_client other_ip then
    lprintf "UDP FROM %s:%d - " 
      (Ip.to_string other_ip) other_port;	    
  match t with
  
  | OvernetConnect (md4, ip, port, kind) ->
      if !verbose_overnet && debug_client other_ip then
        lprintf "CONNECT (%s): sender IP was %s:%d kind=%d\n" (Md4.to_string md4) (Ip.to_string ip) port kind; 
      if (md4 <> !!client_md4) && (md4 <> !!overnet_md4) && not (Hashtbl.mem connected_clients md4) 
      then begin
          let this_peer =
            {
              peer_md4=(!!overnet_md4);
              peer_ip=(client_ip None);
              peer_port=(!!overnet_port);
              peer_kind=0;
              peer_last_msg=last_time();
            } in
          let ip = (change_private_address ip other_ip) in
(* verificare per le Indirect_location *)

          let c = new_client (Direct_address (ip, port)) in
          c.client_ip <- ip;
          c.client_connect_time <- last_time ();
          DonkeySources.set_source_brand c.client_source true;
          c.client_brand <- Brand_overnet;
          set_client_state c (Connected (-1)); 
          Hashtbl.add connected_clients md4 c;
          
          add_global_peer
            {
            peer_md4=md4;
            peer_ip=ip;
            peer_port=port;
            peer_kind=kind;
            peer_last_msg=last_time();
          };
          
          udp_send ip port (OvernetConnectReply([this_peer]));

(* send the PUBLICIZED message *)
          udp_send other_ip port OvernetPublicized
        
        end
  
  | OvernetConnectReply peers ->
      begin
        let peers = List.rev peers in
        match peers with
          peer :: tail ->
            if !verbose_overnet && debug_client other_ip then
              lprintf "CONNECT REPLY: sender IP was %s\n" (Ip.to_string peer.peer_ip);
            peer.peer_ip <- (change_private_address peer.peer_ip other_ip);
            
            peer.peer_last_msg <- last_time ();
            add_global_peer peer;

(* everything else sould be added *)	    
            List.iter add_global_peer tail;
            
            Hashtbl.iter 
              (fun _ search -> add_search_peer search peer) 
            overnet_searches;
            
            Hashtbl.iter 
              (fun _ search -> List.iter (fun a -> add_search_peer search a) tail) 
            overnet_searches;
        
        | _ -> ()
      end
  
  | OvernetPublicize (md4, ip, port, kind ) ->
      begin
        if !verbose_overnet && debug_client other_ip then
          lprintf "PUBLICIZE (%s): sender IP was %s\n" (Md4.to_string md4) (Ip.to_string ip); 
        add_global_peer
          {
          peer_md4=md4;
          peer_ip=(change_private_address ip other_ip);
          peer_port=port;
          peer_kind=kind;
          peer_last_msg=last_time();
        };

(* send the PUBLICIZED message *)
        udp_send other_ip port OvernetPublicized
      end      
  
  | OvernetPublicized -> 
      if !verbose_overnet && debug_client other_ip then
        lprintf "PUBLICIZED\n"; 
  
  | OvernetSearch (nb, md4) -> 
      begin
        try 
          let (s_ip, s_port) as s_addr =
            match p.UdpSocket.udp_addr with
            | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
            | _ -> raise Not_found
          in                  
          
          let peers = (get_local_distribution md4 nb) in
          begin
            if !verbose_overnet && debug_client other_ip then
              lprintf "SEARCH (%s)\n" (Md4.to_string md4); 
            udp_send s_ip s_port (OvernetSearchReply(md4,peers)); 
          end
        with _ -> lprintf "Cannot find the client IP\n"
      end
  | OvernetPublish (md4, r_md4, r_tags) -> 
      if (Md4.up md4) = (Md4.up !!overnet_md4) then
        begin
          if !verbose_overnet && debug_client other_ip then
            lprintf "PUBLISH (%s): r_md4 = %s\n" (Md4.to_string md4) (Md4.to_string r_md4); 
          store_published_file md4 r_md4 r_tags (last_time ());
          udp_send other_ip other_port (OvernetPublished md4)
        end
      else
      if !verbose_overnet && debug_client other_ip then
        lprintf "NOT PUBLISH (%s): r_md4 = %s\n" (Md4.to_string md4) (Md4.to_string r_md4); 
  
  | OvernetGetSearchResults (md4, kind, min, max) ->
      if !verbose_overnet && debug_client other_ip then
        lprintf "GET SEARCH RESULT for %s %d %d %d\n" (Md4.to_string md4) kind min max;
      get_results_from_query other_ip other_port md4 min max
(*    udp_send other_ip other_port (OvernetNoResult md4) *)
  
  | OvernetPublished (md4) ->
      if !verbose_overnet && debug_client other_ip then
        lprintf "PUBLISHED (%s)\n" (Md4.to_string md4)
  
  | OvernetSearchReply (md4, peers) ->
      begin
        try
          begin
            let s = Hashtbl.find overnet_searches md4 in
            begin
              try 
                let (s_ip, s_port) as s_addr =
                  match p.UdpSocket.udp_addr with
                  | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
                  | _ -> raise Not_found
                in                  
                
                begin
                  try
                    let sender = Hashtbl.find s.search_known_peers s_addr in
                    let distance = Md4.xor sender.peer_md4 md4 in
                    
              if !verbose_overnet && sender.peer_md4 = md4 then
                lprintf "Client %s has found itself!\n" (Md4.to_string md4);

                    sender.peer_last_msg <- last_time();	  
                    if XorSet.mem (distance, sender) s.search_asked_peers then
                      begin
                        s.search_asked_peers <- XorSet.remove (distance, sender) s.search_asked_peers;
                        s.search_done_peers <- XorSet.add (distance, sender) s.search_done_peers;
                      end
                    else
                      lprintf "BUG: This peer (%s) returned results but cannot be found !\n" (Md4.to_string md4);
                    
                    if !verbose_overnet && debug_client other_ip then
                      lprintf "SEARCH REPLY(%s): got answer\n" (Md4.to_string md4);
                  with _ -> (* Cannot find this peer to update his last_msg, this is not an issue*)
                      ()
                end
              
              with _ -> ()
(* Cannot find sender IP... However, we can still use his results *)
            end;
            
            List.iter (fun p -> add_search_peer s p; add_global_peer p) peers
          end
        with _ -> ()
	(* lprintf "NO SUCH SEARCH: %s\n" (Md4.to_string md4); *)
      end
  | OvernetNoResult(md4) -> 
      if !verbose_overnet && debug_client other_ip then
        lprintf "NO RESULT (%s)\n" (Md4.to_string md4);
  
  | OvernetSearchResult (md4, r_md4, r_tags) ->      
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in
          s.search_nresults <- s.search_nresults + 1;

(* If we cannot find the peer, we can still use his results*)
          begin
            try 
              let (s_ip, s_port) as s_addr =
                match p.UdpSocket.udp_addr with
                | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
                | _ -> raise Not_found
              in                  
              let sender = Hashtbl.find s.search_known_peers s_addr in
              sender.peer_last_msg <- last_time();
            with _ -> () 
          end;
          
          if !verbose_overnet && debug_client other_ip then
            lprintf "SEARCH RESULT (%s): r_md4 = %s\n" (Md4.to_string md4) (Md4.to_string r_md4); 
          
          match s.search_kind with
            FileSearch file ->
              
              List.iter (fun tag ->
                  if tag.tag_name = "loc" then 
                    begin
                      match tag.tag_value with
                        String bcp ->
                          if String2.starts_with bcp "bcp://" then
                            let bcp2 = String.sub bcp 6 (String.length bcp - 6) 
                            in
                            begin
                              match String2.split_simplify bcp2 ':' with
                              | [_;ip;port] ->
                                  if !verbose_overnet then
                                    lprintf "FIXME: Received from %s:%d a BCP type 2 %s for MD4 %s\n"
                                      (Ip.to_string other_ip) other_port bcp (Md4.to_string md4);
(*Hashtbl.add firewalled_overnet_peers md4 file;
				udp_send 
				  (Ip.of_string ip) (int_of_string port) 
				  (OvernetFirewallConnection(r_md4,4662))*)
(* VERIFICARE SE FUNZIONA

POTREBBE DARSI CHE IN QUESTO CASO NON VIENE RESTITUITA LA LOCAZIONE DEL FILE,
MA QUELLA DI UN PEER VICINO AL FILE??

PARE DI NO. FARE DELLE PROVE INTERROGANDO L'IP INDICATO RELATIVAMENTE AL MD4 INDICATO

				let ip = Ip.of_string ip in
				let port = int_of_string port in
                           if Ip.valid ip && Ip.reachable ip then
                             let c = DonkeySources.new_source (ip, port) file in
                             c.source_overnet <- true;
*)
                              | [ip;port] ->
                                  incr source_hits;
                                  let ip = Ip.of_string ip in
                                  let port = int_of_string port in
(*lprintf "FIXME: Received a BCP type 1 %s for MD4 %s" 
				  bcp (Md4.to_string md4);
				lprint_newline (); *)
                                  if Ip.valid ip && ip_reachable ip then
                                    let s = DonkeySources.find_source_by_uid 
                                      (Direct_address (ip, port))  in
                                    DonkeySources.set_request_result s 
                                      file.file_sources File_new_source;
                                    DonkeySources.set_source_brand s true
                              | _ ->
                                  lprintf "Ill formed bcp: %s" bcp;
                                  lprint_newline ();
                            end
                          else 
                            begin
                              lprintf "Not a bcp !!!"; 
                              lprint_newline ();
                            end
                      | _ -> 
                          lprintf "Not a string location ??"; 
                          lprint_newline ();
                    end
              ) r_tags;
          | KeywordSearch sss ->
              incr search_hits;
              if not (Hashtbl.mem s.search_results r_md4) then 
                begin
                  s.search_hits <- s.search_hits + 1;
                  Hashtbl.add s.search_results r_md4 r_tags;

                if !verbose_overnet then begin
                  lprintf "FILE FOUND, TAGS: "; lprint_newline ();
		  print_tags r_tags; lprint_newline ()
                end;
                  
                  List.iter (fun ss -> 
(*
                if check_filename ss.search_query r_tags then
		      begin
			lprintf "Matched"; 
                        lprint_newline ();
*)
                      DonkeyOneFile.search_found true ss r_md4 r_tags;
(*		      end
		    else
		      begin
			lprintf "Not matched"; 
                        lprint_newline ();
		      end
*)
                  ) sss
                
                end;
        
        with _ -> ()
(*            lprintf "NO SUCH SEARCH..."; lprint_newline (); *)
      end
  
  | OvernetFirewallConnectionACK(md4) -> 
      if !verbose_overnet && debug_client other_ip then
        lprintf "OVERNET FIREWALL ACK for md4=%s" (Md4.to_string md4)
  
  | OvernetFirewallConnectionNACK(md4) ->
      if !verbose_overnet && debug_client other_ip then
        lprintf "OVERNET FIREWALL NACK for md4=%s" (Md4.to_string md4)

(* send the answer *)
  | OvernetGetMyIP other_port ->
      if !verbose_overnet && debug_client other_ip then 
        lprintf "GET MY IP (port=%d)\n" other_port;
(* FIXME : should be able to flush the UDP buffer*)
      udp_send other_ip other_port (OvernetGetMyIPResult other_ip);
      udp_send other_ip other_port OvernetGetMyIPDone
  
  | OvernetGetMyIPResult(ip) ->
      if !verbose_overnet && debug_client other_ip then 
        lprintf "GET MY IP RESULT (%s)\n" (Ip.to_string ip)
  
  | OvernetGetMyIPDone ->
      if !verbose_overnet && debug_client other_ip then 
        lprintf "GET MY IP DONE\n"
  
  | OvernetPeerNotFound peer -> 
      begin
        if !verbose_overnet && debug_client other_ip then 
          lprintf "Peer NOT FOUND %s (%s:%d) kind: %d (msg 33)\n"
            (Md4.to_string peer.peer_md4) (Ip.to_string peer.peer_ip) peer.peer_port peer.peer_kind;
        let md4 = peer.peer_md4 in
        let i = Md4.up md4 in
        if Hashtbl.mem !!global_peers.(i) md4
        then begin
            Hashtbl.remove !!global_peers.(i) md4;
            global_peers_size.(i) <- global_peers_size.(i) - 1;
            if global_peers_size.(i) < min_peers_per_block then
              find_new_peers ()
          end
      end
  
  | OvernetUnknown (opcode, s) ->
      lprintf "Unknown message from %s:%d\n" (Ip.to_string other_ip) other_port;
      lprintf "\tCode: %d\n" opcode; dump s;
      lprint_newline ()
  
  | OvernetUnknown21 peer ->
     if !verbose_overnet && debug_client other_ip then 
      lprintf "Unknown 21 message from %s:%d\n" (Ip.to_string other_ip) other_port;
      lprintf "\tPeer: %s\n\tIP: %s\n\tPort: %d\n\tKind: %d\n\n"
        (Md4.to_string peer.peer_md4) (Ip.to_string peer.peer_ip) peer.peer_port peer.peer_kind
  
  | _ -> 
      lprintf "FIXME: UNUSED MESSAGE from %s:%d\n" (Ip.to_string other_ip) other_port;
      raise Not_found
      
    
let query_min_peer s =
  try
    let (d,p) as e = XorSet.min_elt s.search_not_asked_peers in
    let nb = 4 in
    if !verbose_overnet then 
      begin
        lprintf "SEARCH(%s): Query a not asked peer at distance %s"
          (Md4.to_string s.search_md4) (Md4.to_string d); 
        lprint_newline ();
      end;
    s.search_not_asked_peers <- XorSet.remove e s.search_not_asked_peers;
    s.search_asked_peers <- XorSet.add e s.search_asked_peers;
    s.search_last_insert <- last_time();
    
    udp_send p.peer_ip p.peer_port  (OvernetSearch (nb, s.search_md4)); (* was 10 *)
    
    begin
      match s.search_kind with
        KeywordSearch sss ->
          if sss <> [] then
            udp_send p.peer_ip p.peer_port
              (OvernetGetSearchResults (s.search_md4,0,0,100));	 
          List.iter 
            (fun file ->
              if !verbose_overnet (* && debug_client p.peer_ip *) then 
                lprintf "*** Publishing keyword %s of file %s to %s:%d \n"
                  (Md4.to_string s.search_md4) (Md4.to_string file.file_md4)
                (Ip.to_string p.peer_ip) p.peer_port;
              udp_send p.peer_ip p.peer_port 
                (OvernetPublish (s.search_md4, file.file_md4, 
                  DonkeyProtoCom.tag_file file))
          ) s.search_publish_files
      | FileSearch file ->
          if file_state file = FileDownloading then
            udp_send p.peer_ip p.peer_port
              (OvernetGetSearchResults (s.search_md4,0,0,100));
          if s.search_publish_file then
            begin
              if !verbose_overnet (* && debug_client p.peer_ip *) then 
                lprintf "*** Publishing my file %s to %s:%d\n"
                  (Md4.to_string s.search_md4) 
(*                (Md4.to_string p.peer_md4) *)
                (Ip.to_string p.peer_ip) p.peer_port;
              udp_send p.peer_ip p.peer_port 
              (OvernetPublish (s.search_md4, !!overnet_md4,
                  [{
                      tag_name = "loc";
                      tag_value = String (
                        Printf.sprintf "bcp://%s:%d"
                          (Ip.to_string (client_ip None))
                        (!!overnet_port)				      
                      )
                    }]
                )
              )
            end
    end
  with _ -> ()
      
let query_next_peers () =
  Hashtbl2.safe_iter (fun s ->
      let asked_card = XorSet.cardinal s.search_asked_peers and
        not_asked_card = XorSet.cardinal s.search_not_asked_peers in

(* Cases to stop a search : 
	 1/ enough hits 
	 2/ size(asked)=0 && size(not_asked)=0 
	 3/ size(not_asked)=0 && timeout *)
      
      if (s.search_hits > !!overnet_max_search_hits) || 
        ( (s.search_last_insert + !!overnet_search_timeout < last_time ()) && not_asked_card=0 ) ||
        (  not_asked_card = 0 && asked_card = 0 ) then 
        begin
          if !verbose_overnet then begin              
              lprintf "Search for %s finished (%d hits %d results)"
                (Md4.to_string s.search_md4) s.search_hits s.search_nresults;
              lprint_newline ();
            end;
          Hashtbl.remove overnet_searches s.search_md4;
        end
      else
        begin
          query_min_peer s;
          query_min_peer s;
        end
  ) overnet_searches
  

let do_publish_shared_files () =
  let nb_searches = ref 0 in
  Hashtbl.iter (fun _ _ -> incr nb_searches) overnet_searches;
  let launch () = 
    match !files_to_be_published with 
      [] -> () 
    | file::tail -> 
        begin
          if !verbose_overnet then
            lprintf "OVERNET: I am publishing file %s\n" (Md4.to_string file.search_md4);
          files_to_be_published := tail;
          Hashtbl.add overnet_searches file.search_md4 file;
        end	      
  in
  if !nb_searches <= max_searches_for_publish then
    begin
      if !verbose_overnet then lprintf "OVERNET: currently %d searches\n" !nb_searches;
      for i=1 to 5 do
        launch ()
      done
    end
    
let publish_shared_files () = 
  match !files_to_be_published with 
    [] -> 
      if !verbose_overnet then lprintf "OVERNET: PUBLISHING ALL THE FILES!!!!\n";
      List.iter (fun file -> publish_file file) (DonkeyShare.all_shared ())
  | _ -> ()
      
let check_current_downloads () =
  List.iter (fun file ->
      if file_state file = FileDownloading           
          &&
        not (Hashtbl.mem overnet_searches file.file_md4) then
        let search = create_search (FileSearch file) file.file_md4 in
        Hashtbl.add overnet_searches file.file_md4 search;
  ) !DonkeyGlobals.current_files
  
let enable enabler = 
  if !!enable_overnet then begin
    let sock = (UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
        (!!overnet_port) (udp_handler udp_client_handler)) in
    udp_sock := Some sock;
    
    UdpSocket.set_write_controler sock udp_write_controler;
    
    begin
      try
        let sock = TcpServerSocket.create 
            "overnet client server"
            (Ip.to_inet_addr !!client_bind_addr)
          (!!overnet_port) (DonkeyClient.client_connection_handler true) in
        
        tcp_sock := Some sock;
        
        match Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock)) with
          Unix.ADDR_INET (ip, port) ->
            overnet_client_port :=  port
        | _ -> failwith "Bad socket address"
      with e ->
          lprintf "Could not affect a TCP port %d for Overnet" !!overnet_port;
          lprint_newline ();
          tcp_sock := None;
    end;
  end;

(* every 3min try a new publish search, if any *)
  add_session_timer enabler 180. (fun _ ->
      if !!enable_overnet then begin
          find_new_peers ();
          do_publish_shared_files ();
        end
  );
  
  add_session_timer enabler 1. (fun _ ->
      if !!enable_overnet then begin
          match !boot_peers with
            [] -> ()
          | _ ->
              for i = 1 to 5 do
                match !boot_peers with
                  [] -> ()
                | (ip, port) :: tail ->
                    boot_peers := tail;
                    udp_send ip port (OvernetConnect(!!overnet_md4,client_ip None,
                        !overnet_client_port, 0));
              done
        end
  );
  
  add_session_option_timer enabler overnet_query_peer_period  (fun _ ->
      if !!enable_overnet then begin
          query_next_peers ()
        end
  );

(* every 3h for re-publish *)
  add_session_timer enabler !!overnet_republish (fun _ ->
      if !!enable_overnet then begin
          publish_shared_files ()
        end
  );

(* every 30min for common operations *)
  add_session_timer enabler 1800. (fun _ ->
      if !!enable_overnet then begin
          recover_all_files ();
        end
  );

  add_timer 60. (fun _ ->
      if !!enable_overnet then recover_all_files ());
  
(* every 15min for light operations *)
  add_session_timer enabler 900. (fun _ ->
      if !!enable_overnet then begin
          publicize_peers ();
          check_current_downloads ();
        end
  );
  
(* 1st time timers : we cannot afford waiting too much to get connected *)
  add_timer 50. (fun timer -> 
      if !!enable_overnet then begin
          publicize_peers ()
        end
  );
  
  add_timer 20. (fun timer -> 
      if !!enable_overnet then begin
          find_new_peers (); 
(*publish is in fact controled by do_publish_shared_files, every 2 min*)   
          publish_shared_files ()
        end
  )
  
let _ =
  option_hook overnet_query_peer_period 
    (fun _ -> if !!overnet_query_peer_period < 5. then overnet_query_peer_period =:= 5.);
  option_hook overnet_max_known_peers 
    (fun _ -> if !!overnet_max_known_peers < 4096 then overnet_max_known_peers =:= 4096)
  
let disable () = 
  begin
  (match !udp_sock with
    None -> ()
  | Some sock -> 
      udp_sock := None;
      UdpSocket.close sock Closed_by_user);
  (match !tcp_sock with
    None -> ()
  | Some sock -> 
      tcp_sock := None;
      TcpServerSocket.close sock Closed_by_user);
  end

let _ =
  option_hook enable_overnet
  	(fun _ -> if !!enable_overnet = false then 
  	  begin 
  	    is_enabled := false;
  	    disable() 
  	  end else 
  	  if !is_enabled then begin
  	    is_enabled := true;
  	    enable is_enabled
  	  end
  	)
      
let parse_overnet_url url =
  match String2.split (String.escaped url) '|' with
  | "fha://" :: "boot" :: name :: port :: _
  | "boot" :: name :: port :: _ ->
      let ip = Ip.from_name name in
      let port = int_of_string port in
      udp_send ip port (OvernetConnect(!!overnet_md4,client_ip None,!overnet_client_port, 0));
      true
  | _ -> false
      
let connected_peers () =
  List.map (fun p -> p.peer_ip, p.peer_port)  (get_uniform_distribution ())

let register_commands list =
  register_commands
    (List2.tail_map (fun (n,f,h) -> (n, "Network/Overnet", f,h)) list)
  
let _ =
  register_commands 
    [
    "boot", Arg_two (fun ip port o ->
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        udp_send ip port (OvernetConnect(!!overnet_md4,client_ip None,!overnet_client_port, 0));
        Printf.sprintf "peer %s:%d added" (Ip.to_string ip) port
    ), "<ip> <port> :\t\t\tadd an Overnet peer";
    
    "ovlink", Arg_multiple (fun args o ->        
        let buf = o.conn_buf in
        let url = String2.unsplit args ' ' in
        if parse_overnet_url url then
          "download started"
        else "bad syntax"
    ), "<fhalink> :\t\t\tdownload fha:// link";
    
    "ovstats", Arg_none (fun o ->
        let buf = o.conn_buf and sum = ref 0 in
        Printf.bprintf buf "Overnet statistics:\n"; 
        Printf.bprintf buf "  Search hits: %d\n" !search_hits;
        Printf.bprintf buf "  Source hits: %d\n" !source_hits;
        Printf.bprintf buf "  peers blocks (max peer number per block = %d) :\n" (!!overnet_max_known_peers/256);
        Printf.bprintf buf "    This is a list of known peers, sorted by the first byte of their md4 address ( 05FE15E90678... => block 05)\n";        
        for i=0 to 15 do	  
          Printf.bprintf buf "    ";
          for j=0 to 15 do
            Printf.bprintf buf "%02X: %3d  " (i*16+j) global_peers_size.(i*16+j);
            sum := !sum + global_peers_size.(i*16+j);
          done;
          Printf.bprintf buf "\n";
        done;
        Printf.bprintf buf "  Number of known Overnet peers = %d\n" !sum;
        
        Hashtbl.iter (fun _ s ->
            Printf.bprintf buf 
              "Search %s for %s (%d not_asked, %d asked, %d done, %d hits, %d results) %s%s\n"            
              (match s.search_kind with
                KeywordSearch _ -> "keyword"
              | FileSearch _ -> "file")
            (Md4.to_string s.search_md4)
            (XorSet.cardinal s.search_not_asked_peers)
            (XorSet.cardinal s.search_asked_peers)
            (XorSet.cardinal s.search_done_peers)
            s.search_hits
              s.search_nresults
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
    ), ":\t\t\t\tOvernet Stats";
    
    "ovweb", Arg_multiple (fun args o ->
        let urls =
          match args with
            [] -> let list = ref [] in
              List.iter (fun (kind,_, url) ->
                  if kind = "ocl" then list := url :: !list
              )!!web_infos;
              !list
          | _ -> args
        in
        List.iter (fun url ->
            Printf.bprintf o.conn_buf "Loading %s\n" url;
            CommonWeb.load_url "ocl" url) urls;
        "web boot started"
    ), "<urls> :\t\t\t\tdownload .ocl URLS (no arg load default)";
    
    "ovmd4", Arg_none (fun o -> "MD4 is " ^ (Md4.to_string !!overnet_md4);
    ), ":\t\t\t\t\tget client MD4 address on the overnet network";
    
    "ovstore", Arg_none (fun o -> 
        let buf = o.conn_buf in
        
        Printf.bprintf buf "Overnet store:\n"; 
        Printf.bprintf buf "  size = %d, max_size = %d\n" 
          !published_keyword_size !!overnet_store_size; 
        Printf.bprintf buf "\n";        
        
        Hashtbl.iter (fun kw_md4 (size, files) -> 
            Printf.bprintf buf "  md4=%s\n  files =\n" (Md4.to_string kw_md4);
            XorMd4Map.iter (fun file_md4 (file_tags,_) ->
                Printf.bprintf buf "    r_md4=%s\n" (Md4.to_string file_md4);
                Printf.bprintf buf "    tags=";
                bprint_tags buf file_tags;
                Printf.bprintf buf "\n";
            ) !files
        ) published_keyword_table;
        
        ""
    ), ":\t\t\t\tdump the Overnet File Store";
    
    
    "ovtst", Arg_two (fun a b o ->
        let md41 = Md4.of_string a in
        let md42 = Md4.of_string b in
        store_published_file md41 md42
          [
          string_tag "filename" "john" ;
          int_tag "size" (Random.int 200);
        ] 0;
        ""
    ), ":\t\t\t\t\t(not documented)";
    
    "ovtst2", Arg_two (fun a b o ->
        let md4 = Md4.of_string a in
        let size = int_of_string b in
        get_results_from_query (Ip.of_string "10.0.0.10") (4665) md4 0 size;
        ""
    ), ":\t\t\t\t(not documented)";
    
    
    "send_udp", Arg_three (fun ip port hex o ->
        try
          let rec hex_to_msg h =
            match h with
              "" -> ""
            | s ->
                (String.make 1 (Char.chr (int_of_string ("0x" ^(String.sub s 0 2))))) ^
                  (hex_to_msg (String.sub s 2 (String.length s - 2))) in
          let opcode = int_of_string ("0x" ^(String.sub hex 0 2)) in
          let msg = hex_to_msg (String.sub hex 2 (String.length hex - 2)) in
          udp_send
            (Ip.of_string ip)
          (int_of_string port)
          (OvernetUnknown (opcode,msg));
          lprintf "Sending UDP message %d to %s:%s\n" opcode ip port;
          dump msg; lprint_newline (); "Sending UDP message"
        with _ ->
            lprintf "Unable to send UDP message\n"; "Unable to send UDP message"
    ), ":\t\t\t\tsend UDP message (<ip> <port> <msg in hex>)";
  
    "ovsearch", Arg_one (fun m o ->
      let s = create_search (KeywordSearch []) (Md4.of_string m) in
      Hashtbl.add overnet_searches s.search_md4 s;
      "Searching for md4"
    ), ":\t\t\t\tsearch keyword (<md4>)";
  
  ];
  
  CommonWeb.add_web_kind "ocl" (fun _ filename ->
      let s = File.to_string filename in
      let s = String2.replace s '"' "" in
      let lines = String2.split_simplify s '\n' in
      List.iter (fun s ->
          try
            match String2.split_simplify s ',' with
              name :: port :: _ ->
                let name = String2.replace name '"' "" in
                let port = String2.replace port '"' "" in
                Ip.async_ip name (fun ip ->
                    let port = int_of_string port in
                    if !verbose_overnet then begin
                        lprintf "ADDING OVERNET PEER %s:%d" name port; 
                        lprint_newline ();
                      end;
                    boot_peers := (ip, port) :: !boot_peers)
            | _ -> 
                lprintf "BAD LINE ocl: %s" s;
                lprint_newline ();
          with _ -> 
              begin
                lprintf "DNS failed"; lprint_newline ();
              end
      ) lines
  )
  
let overnet_search (ss : search) =
  if !!overnet_search_keyword && not (List.mem ss.search_num !all_overnet_searches) then
    let q = ss.search_query in
    all_overnet_searches := ss.search_num :: !all_overnet_searches;
    let ws = keywords_of_query q in
    List.iter (fun w -> 
        let s = create_keyword_search w in
        Hashtbl.iter (fun r_md4 r_tags -> 
            DonkeyOneFile.search_found true ss r_md4 r_tags) s.search_results;
        begin
          match s.search_kind with
            KeywordSearch sss -> s.search_kind <- KeywordSearch (ss :: sss)
          | _ -> ()
        end;
        Hashtbl.add overnet_searches s.search_md4 s;	 
    )
    ws

let _ =
  CommonWeb.add_redirector_info "DKOV" (fun buf ->
      let peers = connected_peers () in
      let peers, _ = List2.cut 32 peers in (* sending 32 peers is enough ! *)
      buf_int buf (List.length peers);
      List.iter (fun (ip,port) -> 
          buf_ip buf ip; 
          buf_int16 buf port) 
      peers;
  )