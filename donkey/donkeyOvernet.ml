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
More publish tuning (3min for publish, 40s timeout)
Answer ans store FilePublish requests from other peers (one more step to completion !)
  We use a local storage divided in 2 parts (80% - 20%) to keep an high-quality subset.
Fix OCL loading (change default list, and add exception catching)
Help message in ovstats
Dump packet on Udp_handler exception (to check if overnet is responsible for Not_found)
Avoid to add private addresses in search lists and peer lists
Answer FileSearch requests (one more step to protocol completion)
Have the overnet_publish flag to true by default. Publish works well and is very smooth,
    has been tested, - I believe - is ready for Prime Time :)
Rework search : clean create_search functions, they do not add searches to overnet_searches
More publish tuning : we only start publish-searches by group of 5 (there are many publish-searches
    by file, ie NB_KEYWORDS(filename)+1), every 3 minutes, only if less than 5 searches 
    are currently started...
*)

(*
TODO
----
Enforce correct search
Add BCP type2 support again
Check 15/16/18/19/1A/1B/1C/1D/1E
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

module XorSet2 = Set.Make (
  struct
    type t = Md4.t * (Md4.t * CommonTypes.tag list)
    let compare (m1,p1) (m2,p2) = compare (m1,p1) (m2,p2)
  end
)
 
let min_peers_per_block = 2
let min_peers_before_connect = 5
let max_searches_for_publish = 5
let search_max_queries = 64
let overnet_default_ocl = define_option downloads_ini 
    ["ocl_links"] ""
  (list_option string_option)
  [
    "http://savannah.nongnu.org/download/mldonkey/network/peers.ocl";        
    "http://members.lycos.co.uk/appbyhp2/FlockHelpApp/contact-files/contact.ocl" ;
  ]

let global_peers_size = Array.make 256 0
(*let firewalled_overnet_peers = Hashtbl.create 13*)

  (* 20% of max_size_file_store*)
let published_files1 : (Md4.t,Md4.t*CommonTypes.tag list) Hashtbl.t = 
  Hashtbl.create 10
let published_files_fifo1 = Queue.create ()
let published_files_size1 = ref 0
 
(* 80% of max_size_file_store*)
let published_files2 : (Md4.t,Md4.t*CommonTypes.tag list) Hashtbl.t = 
  Hashtbl.create 10
let published_files_fifo2 = Queue.create ()
let published_files_size2 = ref 0
 
(* overnet_md4 should be different from client_md4 for protocol safety reasons *)
let overnet_md4 = Md4.random()
(*let overnet_md4 = Md4.of_string "FBB5EA4C0A82FB995911223344556677";*)

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
  
let overnet_store_size = 
  define_option downloads_ini ["overnet_store_size"] "Size of the filename storage used to answer queries" 
    int_option 2000

let overnet_port = 
  define_option downloads_ini ["overnet_port"] "port for overnet" 
    int_option (2000 + Random.int 20000)

let overnet_max_known_peers = 
  define_option downloads_ini ["overnet_max_known_peers"] 
  "maximal number of peers to keep overnet connected (should be >2048)" 
    int_option 8192

let overnet_search_keyword = 
  define_option downloads_ini ["overnet_search_keyword"] 
  "allow extended search to search on overnet" bool_option false

let global_peers : (Md4.t, peer) Hashtbl.t array Options.option_record = define_option servers_ini 
    ["overnet_peers"] "List of overnet peers"
    (hasharray_option Md4.null PeerOption.t) 
    (Array.init 256 (fun _ -> Hashtbl.create 10))

let overnet_search_timeout = 
  define_option downloads_ini ["overnet_search_timeout"] 
  "How long shoud a search on Overnet wait for the last answer before terminating"
    float_option 40. 
      
let overnet_query_peer_period = 
  define_option downloads_ini ["overnet_query_peer_period"] 
  "Period between two queries in the overnet tree (should not be set under 5)"
    float_option 5.
      
let overnet_max_search_hits = 
  define_option downloads_ini ["overnet_max_search_hits"] 
  "Max number of hits in a search on Overnet"
    int_option 200 
      
let gui_overnet_options_panel = 
  define_option downloads_ini ["gui_overnet_options_panel"]
  "Which options are configurable in the GUI option panel, and in the
  Overnet section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
    (list_option (tuple3_option (string_option, string_option, string_option)))
  [
    "Port", shortname overnet_port, "T";
    "Search for keywords", shortname overnet_search_keyword, "B";
    "Search Timeout", shortname overnet_search_timeout, "T";
    "Search Internal Period", shortname overnet_query_peer_period, "T";
    "Verbose", shortname verbose_overnet, "B";
    "Search Max Hits", shortname overnet_max_search_hits, "T";
  ]
  
let udp_sock = ref None  
let tcp_sock = ref None
  
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
        if !!verbose_overnet then 
          begin            
(*Too much traffic for a correct debug*)
            if (get_int8 s 1) <> 15 && (get_int8 s 1) <> 14 && (get_int8 s 1) <> 19 then
              begin
                
                if !!verbose_overnet then begin
                    Printf.printf "Sending UDP to %s:%d type %d (0x%02X)" 
                      (Ip.to_string ip) port (get_int8 s 1) (get_int8 s 1);
                    print_newline ();
(*dump s; print_newline ();*)
                  end;
              end;
	  end;
        let len = String.length s in
        UdpSocket.write sock s ip port
      with e ->
          Printf.printf "Exception %s in udp_send" (Printexc2.to_string e);
          print_newline () 
            
let search_hits = ref 0
let source_hits = ref 0

let remove_old_global_peers () =
  for i=0 to 255 do 
    Hashtbl.iter ( fun a b -> 
      if (b.peer_last_msg < last_time () -. 14400.) && 
	(global_peers_size.(i)>min_peers_per_block) then 
	begin
	  (*Printf.printf "REM global_peers %s\n" (Md4.to_string a);*)
	  Hashtbl.remove !!global_peers.(i) a;
	  global_peers_size.(i) <- global_peers_size.(i)-1;
	end
    ) !!global_peers.(i)
  done

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
      
      for i=1 to 127 do
	Hashtbl.iter 
	  ( fun a b -> if !size<nb then begin peers := b :: !peers; incr size; end else raise Not_found) 
	  !!global_peers.((start_pos+i) mod 256);
	Hashtbl.iter 
	  ( fun a b -> if !size<nb then begin peers := b :: !peers; incr size; end else raise Not_found) 
	  !!global_peers.((start_pos-i+256) mod 256);
      done;
    with _ -> ()
  end;
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
	else Hashtbl.iter ( fun a b -> if !pos=0 then peers := b :: !peers; decr pos ) 
	    !!global_peers.(16*i+j);
      done;    
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
     not ( private_address peer.peer_ip ) then
    begin
      if (peer.peer_ip <> !!donkey_bind_addr) && (peer.peer_md4 <> overnet_md4) then
	begin
	  let i=Md4.up peer.peer_md4 in
	  if Hashtbl.mem !!global_peers.(i) peer.peer_md4 then
	    begin
	      (*Printf.printf "UPD global_peers: %s\n" (Md4.to_string peer.peer_md4);*)
	      (Hashtbl.find !!global_peers.(i) peer.peer_md4).peer_last_msg <- last_time();
	    end
	  else
	    begin
	      if global_peers_size.(i) >= (!!overnet_max_known_peers/256) then
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
    end
  
(*advertize an uniform distribution then a local distribution (around our MD4) that we are UP*)
let publicize_peers () =
  let global_dist = get_uniform_distribution () in
  let local_dist = get_local_distribution overnet_md4 search_max_queries in
  List.iter (fun a -> 
      udp_send a.peer_ip a.peer_port
        (OvernetPublicize(overnet_md4,
          !!donkey_bind_addr,!!overnet_port, 0) ) ) 
  global_dist;
  List.iter (fun a -> 
      udp_send a.peer_ip a.peer_port
        (OvernetPublicize(overnet_md4,
          !!donkey_bind_addr,!!overnet_port, 0) ) ) 
  local_dist
  
(* If one peer block is running low, try to get new peers using Connect *)
let find_new_peers () =
  try 
    for i=0 to 255 do 
      if global_peers_size.(i) < min_peers_before_connect then raise Not_found;
    done
  with _ -> 
      begin
        if !!verbose_overnet then begin
            Printf.printf "FINDING NEW PEERS"; print_newline ();
          end;
        List.iter (fun a -> udp_send a.peer_ip a.peer_port 
              (OvernetConnect(overnet_md4,!!donkey_bind_addr,!!overnet_port, 0) ) )
        (get_uniform_distribution () ) ;
      end
      
(* Used to prevent reloading the pages too often (max is 1/hour) *)
let next_automatic_ocl_load = ref 0.0
let automatic_ocl_load force =
  match get_uniform_distribution () with
    [] -> 
      if force || !next_automatic_ocl_load < last_time () then
        begin
          next_automatic_ocl_load := last_time () +. 3600.;
          if !!verbose_overnet then begin
              Printf.printf "NEED TO BOOT FROM KNOWN PEERS"; print_newline();
            end;
          List.iter (fun url ->
              Printf.printf "Loading %s\n" url;
              load_url "ocl" url) !!overnet_default_ocl;
        end
  | _ -> ()
      
type search_for =
  FileSearch of file
| KeywordSearch of CommonTypes.search list
  
type overnet_search = {
    search_md4 : Md4.t;
    mutable search_kind : search_for;
    mutable search_last_insert : float;

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
    
let overnet_searches = Hashtbl.create 13
let files_to_be_published = ref []

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
	      (*Printf.printf "add_search_peer(%s) : add peer at distance %s [REPLACE]\n"
		(Md4.to_string s.search_md4) (Md4.to_string distance);*)

            end      
	  else if dd2>distance || dd3>distance then
	    begin
	      (* We have already asked a peer which is worse. Anyway, we shouldn't drop this one*)
	      s.search_not_asked_peers <- XorSet.add (distance, p) s.search_not_asked_peers;
	      s.search_total_peers <- s.search_total_peers + 1 ;
	      (*Printf.printf "add_search_peer(%s) : add peer at distance %s [ADD]\n"
		(Md4.to_string s.search_md4) (Md4.to_string distance);*)
	    end
        end
      else 
	begin
          s.search_not_asked_peers <- XorSet.add (distance, p) s.search_not_asked_peers;
	  s.search_total_peers <- s.search_total_peers + 1 ;
	  (*Printf.printf "add_search_peer(%s) : add peer at distance %s [NOT ENOUGH]\n"
	    (Md4.to_string s.search_md4) (Md4.to_string distance);*)
	    
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
	
let store_published_file md4 file =
  let dist = Md4.xor overnet_md4 md4 in

  if !!verbose_overnet then 
    begin
      Printf.printf "PUBLISH at %s (dist=%s)" (Md4.to_string md4) (Md4.to_string dist);
      print_newline ();
    end;

  if Md4.up2 dist = 0 then
    begin
      if !published_files_size2 >= ((!!overnet_store_size*8)/10) then
      begin
        let rem = (Queue.take published_files_fifo2) in
        decr published_files_size2;
        Hashtbl.remove published_files2 rem;
      end;
      Queue.add md4 published_files_fifo2;
      Hashtbl.add published_files2 md4 file;
      incr published_files_size2;
    end
  else 
    begin
      if !published_files_size1 >= ((!!overnet_store_size*2)/10) then
      begin
        let rem = (Queue.take published_files_fifo1) in
        decr published_files_size1;
        Hashtbl.remove published_files1 rem;
      end;	
      Queue.add md4 published_files_fifo1;
      Hashtbl.add published_files1 md4 file;
      incr published_files_size1;
    end  

let get_results_from_query ip port md4 size =
  let xorset = ref XorSet2.empty and
      nb_results = ref 0 in
  let iter () = 
    while !nb_results < size do
      let (a,b) as e = XorSet2.min_elt !xorset in
      udp_send ip port (OvernetSearchResult(md4,(fst b),(snd b)));
      (*Printf.printf "SEND SEARCH: %s at dist %s" (Md4.to_string (fst b)) (Md4.to_string a); 
      print_newline ();*)
      xorset:=XorSet2.remove e !xorset;
      incr nb_results;
    done
  in

  begin
    try 
      Hashtbl.iter (fun a b -> xorset:=XorSet2.add (Md4.xor md4 a,b) !xorset ) published_files2;  
      iter();
    with _ -> ()
  end;  
  if !nb_results < size then
    begin
      try 
	Hashtbl.iter (fun a b -> xorset:=XorSet2.add (Md4.xor md4 a,b) !xorset ) published_files1;
	iter();
      with _ -> ()
    end

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
      List.iter (fun name -> List.iter index_string (String2.stem name) ) 
      file.file_filenames;
    end
    
let recover_all_files () =
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
          if !!verbose_overnet then begin
              Printf.printf "%s CONTAINS[%s]" filename s; 
              print_newline ();
            end;
	  true
	end
    | _ -> true
  in
  check_iter q

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
  | OvernetSearch (nb, md4) -> 
      begin
        try 
	  let (s_ip, s_port) as s_addr =
	    match p.UdpSocket.addr with
	    | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
	    | _ -> raise Not_found
	  in                  
	  
	  let peers = (get_local_distribution md4 nb) in
	  udp_send s_ip s_port (OvernetSearchReply(md4,peers)); 
	with _ -> Printf.printf "Cannot find the client IP\n"
      end
  | OvernetPublish (md4, r_md4, r_tags) -> 
      begin
        let other_ip = ip_of_udp_packet p in
	let other_port = port_of_udp_packet p in
        store_published_file md4 (r_md4,r_tags);
	udp_send other_ip other_port (OvernetPublished md4);
      end
  | OvernetGetSearchResults (md4, kind, min, max) ->
      begin
        let other_ip = ip_of_udp_packet p in
	let other_port = port_of_udp_packet p in
	if !!verbose_overnet then 
	  begin
	    Printf.printf "GET SEARCH RESULT for %s %d %d %d"
	      (Md4.to_string md4) kind min max;
	    print_newline ();
	  end;
	get_results_from_query other_ip other_port md4 max;
	udp_send other_ip other_port (OvernetNoResult md4);
      end
  | OvernetPublished (md4) -> ()      
  | OvernetSearchReply (md4, peers) ->
      begin
      try
	begin
          let s = Hashtbl.find overnet_searches md4 in
 	  begin
            try 
	      let (s_ip, s_port) as s_addr =
		match p.UdpSocket.addr with
		| Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
		| _ -> raise Not_found
	      in                  

	      begin
		try
		  let sender = Hashtbl.find s.search_known_peers s_addr in
		  let distance = Md4.xor sender.peer_md4 md4 in

		  sender.peer_last_msg <- last_time();	  
		  if XorSet.mem (distance, sender) s.search_asked_peers then
		    begin
		      s.search_asked_peers <- XorSet.remove (distance, sender) s.search_asked_peers;
		      s.search_done_peers <- XorSet.add (distance, sender) s.search_done_peers;
		    end
		  else
		    Printf.printf "BUG: This peer retuned results but cannot be found !\n";
		  
		  if !!verbose_overnet then 
		    begin
		      Printf.printf "SEARCH_PEER(%s): got answer" (Md4.to_string md4);
		      print_newline ();
		    end;
		with _ -> (* Cannot find this peer to update his last_msg, this is not an issue*)
		  ()
	      end

            with _ -> ()
               (* Cannot find sender IP... However, we can still use his results *)
	  end;
	  
	  List.iter (fun p -> add_search_peer s p; add_global_peer p) peers
	end
      with _ -> Printf.printf "NO SUCH SEARCH : %s" (Md4.to_string md4); print_newline ();
      end
  | OvernetNoResult(md4) -> () 
      (* Nothing to do here, there is just no results to our search *)
  | OvernetSearchResult (md4, r_md4, r_tags) ->      
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in
          s.search_nresults <- s.search_nresults + 1;

	  (* If we cannot find the peer, we can still use his results*)
          begin
            try 
              let (s_ip, s_port) as s_addr =
                match p.UdpSocket.addr with
                | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
                | _ -> raise Not_found
              in                  
              let sender = Hashtbl.find s.search_known_peers s_addr in
              sender.peer_last_msg <- last_time();
            with _ -> () 
          end;

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
				if !!verbose_overnet then begin
				  Printf.printf "FIXME: Received a BCP type 2 %s for MD4 %s, port=%d"
				    bcp (Md4.to_string md4) 4662;
				  print_newline ();
				end;
				(*Hashtbl.add firewalled_overnet_peers md4 file;
				udp_send 
				  (Ip.of_string ip) (int_of_string port) 
				  (OvernetFirewallConnection(r_md4,4662))*)
                            | [ip;port] ->
				incr source_hits;
				let ip = Ip.of_string ip in
				let port = int_of_string port in
				(*Printf.printf "FIXME: Received a BCP type 1 %s for MD4 %s" 
				  bcp (Md4.to_string md4);
				print_newline (); *)
                                  if Ip.valid ip && Ip.reachable ip then
                                    let c = DonkeySources1.new_source (ip, port) file in
                                    c.source_overnet <- true;
                              | _ ->
				Printf.printf "Ill formed bcp: %s" bcp;
				print_newline ();
			  end
			else 
			  begin
			    Printf.printf "Not a bcp !!!"; 
			    print_newline ();
			  end
		    | _ -> 
                        Printf.printf "Not a string location ??"; 
                        print_newline ();
                  end
	     ) r_tags;
          | KeywordSearch sss ->
              incr search_hits;
              if not (Hashtbl.mem s.search_results r_md4) then 
		begin
                  s.search_hits <- s.search_hits + 1;
                  Hashtbl.add s.search_results r_md4 r_tags;

		  (*Printf.printf "FILE FOUND, TAGS: "; print_newline ();
		  print_tags r_tags; print_newline ();*)
                  
		     List.iter (fun ss -> 
(*
                      if check_filename ss.search_query r_tags then
		      begin
			Printf.printf "Matched"; 
                        print_newline ();
*)
			DonkeyOneFile.search_found ss r_md4 r_tags;
(*		      end
		    else
		      begin
			Printf.printf "Not matched"; 
                        print_newline ();
		      end
*)
		  ) sss

                end;
	      
        with _ ->
          Printf.printf "NO SUCH SEARCH..."; print_newline ();
      end
      
  | OvernetFirewallConnectionACK(md4) -> 
      begin
	Printf.printf "OVERNET FIREWALL ACK for md4=%s" (Md4.to_string md4); 
	print_newline ();
      end

  | OvernetFirewallConnectionNACK(md4) ->
      begin
	Printf.printf "OVERNET FIREWALL NACK for md4=%s" (Md4.to_string md4); 
	print_newline ();
      end

  (* send the answer *)
  | OvernetGetMyIP ->
      begin
        let other_ip = ip_of_udp_packet p in
	let other_port = port_of_udp_packet p in
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
  try
    let (d,p) as e = XorSet.min_elt s.search_not_asked_peers in
    if !!verbose_overnet then 
      begin
        Printf.printf "SEARCH(%s): Query a not asked peer at distance %s"
          (Md4.to_string s.search_md4) (Md4.to_string d); 
        print_newline ();
      end;
    s.search_not_asked_peers <- XorSet.remove e s.search_not_asked_peers;
    s.search_asked_peers <- XorSet.add e s.search_asked_peers;
    s.search_last_insert <- last_time();
    
    udp_send p.peer_ip p.peer_port  (OvernetSearch (10, s.search_md4));
    
    begin
      match s.search_kind with
        KeywordSearch sss ->
          if sss <> [] then
            udp_send p.peer_ip p.peer_port
              (OvernetGetSearchResults (s.search_md4,0,0,100));	 
          List.iter 
            (fun file ->
              udp_send p.peer_ip p.peer_port 
                (OvernetPublish (s.search_md4, file.file_md4, 
                  DonkeyProtoCom.tag_file file))
          ) s.search_publish_files
      | FileSearch file ->
          if file_state file = FileDownloading then
            udp_send p.peer_ip p.peer_port
            (OvernetGetSearchResults (s.search_md4,0,0,100));
          if s.search_publish_file then
            udp_send p.peer_ip p.peer_port 
              (OvernetPublish (s.search_md4, overnet_md4,
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
        ( (s.search_last_insert +. !!overnet_search_timeout < last_time ()) && not_asked_card=0 ) ||
        (  not_asked_card = 0 && asked_card = 0 ) then 
        begin
          if !!verbose_overnet then begin              
              Printf.printf "Search for %s finished (%d hits %d results)"
                (Md4.to_string s.search_md4) s.search_hits s.search_nresults;
              print_newline ();
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
          if !!verbose_overnet then
            begin
              Printf.printf "OVERNET: I am publishing a file";
              print_newline ();
            end;
          files_to_be_published := tail;
          Hashtbl.add overnet_searches file.search_md4 file;
        end	      
  in
  if !nb_searches <= max_searches_for_publish then
    begin
(*Printf.printf "OVERNET: currently %d searches" !nb_searches;
	  print_newline ();*)
      for i=1 to 5 do
        launch ();
      done;
    end
    
let publish_shared_files () = 
  match !files_to_be_published with 
    [] -> List.iter (fun file -> publish_file file) (DonkeyShare.all_shared ())
  | _ -> ()
      
let check_curent_downloads () =
  List.iter (fun file ->
    if file_state file = FileDownloading           
        &&
      not (Hashtbl.mem overnet_searches file.file_md4) then
      let search = create_search (FileSearch file) file.file_md4 in
      Hashtbl.add overnet_searches file.file_md4 search;
    ) !DonkeyGlobals.current_files

let enable enabler = 
  let sock = (UdpSocket.create (Ip.to_inet_addr !!donkey_bind_addr)
    (!!overnet_port) (udp_handler udp_client_handler)) in
  udp_sock := Some sock;
    
  UdpSocket.set_write_controler sock udp_write_controler;

  begin
    try
  let sock = TcpServerSocket.create 
    "overnet client server"
      (Ip.to_inet_addr !!donkey_bind_addr)
    (!!overnet_port) (DonkeyClient.client_connection_handler true) in

      tcp_sock := Some sock;
    with e ->
        Printf.printf "Could not affect a TCP port %d for Overnet" (!!port + 1);
        print_newline ();
        tcp_sock := None;
  end;
  
  (* every 3min try a new publish search, if any *)
  add_session_timer enabler 180. (fun _ ->
    find_new_peers ();
    do_publish_shared_files ();
  );

  add_session_option_timer enabler 
    overnet_query_peer_period query_next_peers;

  (* every 3h for re-publish and cleaning *)
  add_session_timer enabler 10800. (fun _ ->
      remove_old_global_peers ();
      publish_shared_files ()
  );

  (* every 30min for common operations *)
  add_session_timer enabler 1800. (fun _ ->
      recover_all_files ();
      automatic_ocl_load false;
  );

  (* every 15min for light operations *)
  add_session_timer enabler 900. (fun _ ->
      publicize_peers ();
      check_curent_downloads ();
  );

  (* 1st time timers : we cannot afford waiting too much to get connected *)
  add_timer 50. (fun timer -> 
    publicize_peers ()
  );

  add_timer 20. (fun timer -> 
    automatic_ocl_load false;
    find_new_peers (); 
    (*publish is in fact controled by do_publish_shared_files, every 2 min*)   
    publish_shared_files ()
  )
  
let _ =
  option_hook overnet_query_peer_period 
    (fun _ -> if !!overnet_query_peer_period < 5. then overnet_query_peer_period =:= 5.);
  option_hook overnet_max_known_peers 
    (fun _ -> if !!overnet_max_known_peers < 2048 then overnet_max_known_peers =:= 2048)
  
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
            [] -> !!overnet_default_ocl;
          | _ -> args
        in
        List.iter (fun url ->
            Printf.bprintf o.conn_buf "Loading %s\n" url;
            load_url "ocl" url) urls;
        "web boot started"
    ), " <urls>:\t\t\t\tdownload .ocl URLS (no arg load default)";

    "ovmd4", Arg_none (fun o -> "MD4 is " ^ (Md4.to_string overnet_md4);
     ), "\t\t\t\t\tget client MD4 address on the overnet network";

    "ovstore", Arg_none (fun o -> 
        let buf = o.conn_buf in

        Printf.bprintf buf "Overnet store Level 2:\n"; 
        Printf.bprintf buf "  size = %d, max_size = %d\n" 
          !published_files_size2 ((!!overnet_store_size*8)/10); 
        Printf.bprintf buf "\n";        
   
        Hashtbl.iter (fun a b -> 
           Printf.bprintf buf "  md4=%s r_md4=%s\n" (Md4.to_string a) (Md4.to_string (fst b));
           Printf.bprintf buf "    tags=";
           bprint_tags buf (snd b);
           Printf.bprintf buf "\n";
        ) published_files2;

        Printf.bprintf buf "Overnet store Level 1:\n"; 
        Printf.bprintf buf "  size = %d, max_size = %d\n" 
          !published_files_size1 ((!!overnet_store_size*2)/10); 
        Printf.bprintf buf "\n";        

        Hashtbl.iter (fun a b -> 
           Printf.bprintf buf "  md4=%s r_md4=%s\n" (Md4.to_string a) (Md4.to_string (fst b));
           Printf.bprintf buf "    tags=";
           bprint_tags buf (snd b);
           Printf.bprintf buf "\n";
        ) published_files1;

        ""
     ), " dump the Overnet File Store";


    "ovtst", Arg_two (fun a b o ->
	let md41 = Md4.of_string a in
	let md42 = Md4.of_string b in
        store_published_file md41 (md42,
         [
           { tag_name = "filename"; tag_value = String "john" };
           { tag_name = "size"; tag_value = Uint32 (Int32.of_int (Random.int 200)) };
         ] );
        ""
    ), "";

    "ovtst2", Arg_two (fun a b o ->
	let md4 = Md4.of_string a in
	let size = int_of_string b in
        get_results_from_query (Ip.of_string "10.0.0.10") (4665) md4 size;
        ""
    ), "";

  ];

  add_web_kind "ocl" (fun filename ->
      let s = File.to_string filename in
      let s = String2.replace s '"' "" in
      let lines = String2.split_simplify s '\n' in
      List.iter (fun s ->
	try
          match String2.split_simplify s ',' with
            name :: port :: _ ->
              let ip = Ip.from_name name in
                let port = int_of_string port in
                if !!verbose_overnet then begin
                    Printf.printf "ADDING OVERNET PEER %s:%d" name port; 
                    print_newline ();
                  end;
	      udp_send ip port (OvernetConnect(overnet_md4,!!donkey_bind_addr,!!overnet_port, 0));
          | _ -> 
              Printf.printf "BAD LINE ocl: %s" s;
              print_newline ();
	with _ -> 
	  begin
	    Printf.printf "DNS failed"; print_newline ();
	  end
      ) lines
  )

let all_overnet_searches = ref ([] : int list)
  
let overnet_search (ss : search) =
  if !!overnet_search_keyword && not (List.mem ss.search_num !all_overnet_searches) then
    let q = ss.search_query in
    all_overnet_searches := ss.search_num :: !all_overnet_searches;
    let ws = keywords_of_query q in
    List.iter (fun w -> 
      let s = create_keyword_search w in
      Hashtbl.iter (fun r_md4 r_tags -> DonkeyOneFile.search_found ss r_md4 r_tags) s.search_results;
      begin
	match s.search_kind with
	  KeywordSearch sss -> s.search_kind <- KeywordSearch (ss :: sss)
	| _ -> ()
      end;
      Hashtbl.add overnet_searches s.search_md4 s;	 
      )
      ws
    
