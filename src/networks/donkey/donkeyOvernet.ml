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

(* 
* Store published files.
*)
  
type peer = 
  { 
    peer_md4 : Md4.t;
    mutable peer_ip : Ip.t;
    mutable peer_port : int;
    mutable peer_tcpport : int;
    mutable peer_kind : int;
    mutable peer_last_recv : int;
    mutable peer_last_send : int;
  }

type search_kind = 
  Search_for_keyword  of unit option
| Search_for_file
| Search_for_kind of int
  
type t =
(* KADEMLIA_BOOTSTRAP_REQ *)
| OvernetConnect of peer
  
(* KADEMLIA_BOOTSTRAP_RES *)
| OvernetConnectReply of peer list

(* KADEMLIA_HELLO_REQ *)
| OvernetPublicize of peer

(* KADEMLIA_HELLO_RES *)
| OvernetPublicized of peer option
  
(* KADEMLIA_REQ *)
| OvernetSearch of 
(* 2 is OK for most searches, number of replies? *) int * 
(* searched file or keyword *) Md4.t *
  Md4.t option (* Our UID *)

(* KADEMLIA_RES *)
| OvernetSearchReply of 
  Md4.t *
  peer list (* the two closest peers in the binary tree of md4s *)

(* KADEMLIA_SEARCH_REQ *)
| OvernetGetSearchResults of 
  Md4.t * (* target *)
  search_kind *   (* type *)
  int *   (* min *)
  int     (* max *)
  
(* The following two messages are encoded in Overnet and Kademlia in the same
message, but we prefer to have two different messages at this level, for
search for files and search for sources. *)
  
(* KADEMLIA_SEARCH_RES *)
| OvernetSearchFilesResults of
(* query *)  Md4.t *
  (Md4.t * tag list) list (* results *)

(* KADEMLIA_SEARCH_RES *)  
| OvernetSearchSourcesResults of Md4.t * peer list
  
(* KADEMLIA_PUBLISH_REQ *)
| OvernetPublishFiles of 
(* keyword or file md4 *) Md4.t *
(* md4 of file or client md4 *) (Md4.t *  tag list) list

| OvernetPublishSources of 
(* keyword or file md4 *) Md4.t * peer list

(* KADEMLIA_PUBLISH_RES *)
| OvernetPublished of Md4.t

(* All these messages have either no equivalent in Kademlia or simply
  not implemented yet :) *)
| OvernetNoResult of Md4.t

| OvernetUnknown of int * string

| OvernetGetMyIP of int

| OvernetGetMyIPResult of Ip.t

| OvernetGetMyIPDone

| OvernetFirewallConnection of Md4.t*int

| OvernetFirewallConnectionACK of Md4.t

| OvernetFirewallConnectionNACK of Md4.t

| OvernetPeerNotFound of peer

| OvernetUnknown21 of peer

let print_peer buf p =
  Printf.bprintf buf "   { md4 = %s ip = %s port = %d %s kind = %d}\n"
    (Md4.to_string p.peer_md4) (Ip.to_string p.peer_ip) p.peer_port
    (if p.peer_tcpport <> 0 then Printf.sprintf "tcp = %d" p.peer_tcpport
    else "") p.peer_kind
  
let message_to_string t =
  let buf = Buffer.create 100 in
  begin
    match t with
      OvernetConnect p -> 
        Buffer.add_string buf "OvernetConnect\n";
        print_peer buf p
    | OvernetConnectReply peers -> 
        Buffer.add_string buf "OvernetConnectReply\n";
        List.iter (print_peer buf) peers
    | OvernetPublicize p -> 
        Buffer.add_string buf "OvernetPublicize\n";
        print_peer buf p        
    | OvernetPublicized p -> 
        Buffer.add_string buf "OvernetPublicized\n";
        (match p with
          Some p -> print_peer buf p
        | None -> ())
    | OvernetSearch (nresults, md4, _) -> 
        Buffer.add_string buf "OvernetSearch\n";
        Printf.bprintf buf "   target = %s nresults = %d\n"
          (Md4.to_string md4) nresults
    | OvernetSearchReply (target, peers) -> 
        Buffer.add_string buf "OvernetSearchReply\n";
        Printf.bprintf buf "  target = %s npeers = %d\n"
          (Md4.to_string target) (List.length peers);
        List.iter (print_peer buf) peers        
    | OvernetGetSearchResults (target, _, _, _) ->
        Printf.bprintf buf "OvernetGetSearchResults %s\n" (Md4.to_string target)
    | OvernetSearchFilesResults (target, results) ->
        Printf.bprintf buf "OvernetSearchFilesResults %s\n" 
          (Md4.to_string target);
        List.iter (fun (r_md4, r_tags) ->
            Printf.bprintf buf "    %s\n       " (Md4.to_string r_md4);
            bprint_tags buf r_tags ; 
            Printf.bprintf buf "\n";
        ) results
    | OvernetSearchSourcesResults (target, peers) ->
        Printf.bprintf buf "OvernetSearchSourcesResults %s\n" 
          (Md4.to_string target);
        List.iter (print_peer buf) peers
    | OvernetPublishFiles (target, results) ->
        Printf.bprintf buf "OvernetPublish %s\n" 
          (Md4.to_string target);
        List.iter (fun (r_md4, r_tags) ->
            Printf.bprintf buf "    %s\n       " (Md4.to_string r_md4);
            bprint_tags buf r_tags ; 
            Printf.bprintf buf "\n";
        ) results
    | OvernetPublishSources (target, peers) ->
        Printf.bprintf buf "OvernetPublishSources %s\n" 
          (Md4.to_string target);
        List.iter (print_peer buf) peers
    | OvernetPublished target ->
        Printf.bprintf buf "OvernetPublished %s\n" (Md4.to_string target)
    | _ -> 
        Buffer.add_string buf "unknown\n"
  end;
  Buffer.contents buf
  
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
| FileSearch of file
| KeywordSearch of CommonTypes.search list
| FillBuckets
  
type overnet_search = {
    search_md4 : Md4.t;
    mutable search_kind : search_for;

    search_known_peers : (Ip.t * int, peer) Hashtbl.t;
    search_waiting_peers : peer Fifo.t array;
    search_asked_peers : peer Fifo.t array;
    search_ok_peers : peer Fifo.t array;
    
    mutable search_queries : int;
    mutable search_requests : int; 

    mutable search_start : int;
(*
    mutable search_last_insert : int; 
    
    mutable search_not_asked_peers : XorSet.t;
    mutable search_asked_peers : XorSet.t;
    mutable search_done_peers : XorSet.t;

(* nb of peers in search_not_asked_peers + search_asked_peers + search_done_peers *)
    mutable search_total_peers : int;

(* already tested peers, either they are in asked/not_asked/done, either they are useless*)
    mutable search_known_peers : (Ip.t*int, peer) Hashtbl.t;
*)
    
    mutable search_results : (Md4.t, tag list) Hashtbl.t;
    mutable search_nresults : int; (* number of results messages *)
    mutable search_hits : int;     (* number of diff results *)
    (*
    mutable search_publish_files : file list;
mutable search_publish_file : bool;
  *)
    }

exception MessageNotImplemented
  
module Make(Proto: sig

      val enable_overnet : bool Options.option_record
      val overnet_port : int Options.option_record
      val overnet_tcpport : int Options.option_record
      val overnet_section : Options.options_section
        
      val redirector_section : string
      val options_section_name : string
      val command_prefix : string
      val source_kind : bool
        
      val udp_send : UdpSocket.t -> Ip.t -> int -> t -> unit
      val udp_handler : (t -> UdpSocket.udp_packet -> unit) -> 
        UdpSocket.t -> UdpSocket.event -> unit
        
    end) = struct

    open Proto

    let lprintf () =
      lprintf "[%s] " redirector_section; lprintf
      
(********************************************************************


                     STATIC MEMORY
  

*********************************************************************)

    let max_peers_per_bucket = 20
      
let min_peers_per_block = 16 (* was 2 *)
let min_peers_before_connect = 5
let max_searches_for_publish = 5
let max_search_queries = 64
let max_search_requests = 20
  
let is_enabled = ref false


(********************************************************************


                           OPTIONS
  

*********************************************************************)

  (*
module PeerOption = struct
    
    let value_to_peer v = 
      match v with
        SmallList [md4; ip; port; kind]
      | List [md4; ip; port; kind] ->
          let md4 = Md4.value_to_hash md4 in
          {
              peer_md4 = md4; 
              peer_ip = Ip.value_to_ip ip; 
              peer_port = value_to_int port; 
              peer_tcpport = 0; 
              peer_kind = value_to_int kind; 
              peer_last_msg = last_time ();
            }
      | SmallList [md4; ip; port; tcpport; kind]
      | List [md4; ip; port; tcpport; kind] ->
          let v_md4 = Md4.value_to_hash md4 in
          {
              peer_md4 = v_md4; 
              peer_ip = Ip.value_to_ip ip; 
              peer_port = value_to_int port; 
              peer_tcpport = value_to_int tcpport; 
              peer_kind = value_to_int kind; 
              peer_last_msg = last_time ();
            }
      | _ -> assert false
    
    let peer_to_value v = 
      match v with 
        p ->
          if p.peer_tcpport = 0 then
            SmallList [
              Md4.hash_to_value p.peer_md4;
              Ip.ip_to_value p.peer_ip;
              int_to_value p.peer_port;
              int_to_value p.peer_kind
            ]
          else          
            SmallList [
              Md4.hash_to_value p.peer_md4;
              Ip.ip_to_value p.peer_ip;
              int_to_value p.peer_port;
              int_to_value p.peer_tcpport;
              int_to_value p.peer_kind
            ]
            
    let t = define_option_class "Peer" value_to_peer peer_to_value
end
    *)

let overnet_store_size = 
  define_option overnet_section [Proto.options_section_name; "store_size"] "Size of the filename storage used to answer queries" 
    int_option 2000

let overnet_max_known_peers = 
  define_option overnet_section [Proto.options_section_name; "max_known_peers"] 
  "maximal number of peers to keep overnet connected (should be >2048)" 
    int_option 8192

let overnet_search_keyword = 
  define_option overnet_section [Proto.options_section_name; "search_keyword"] 
  "allow extended search to search on overnet" bool_option true

let overnet_search_timeout = 
  define_option overnet_section [Proto.options_section_name; "search_timeout"] 
  "How long shoud a search on Overnet wait for the last answer before terminating"
    int_option 140
      
let overnet_query_peer_period = 
  define_option overnet_section [Proto.options_section_name; "query_peer_period"] 
  "Period between two queries in the overnet tree (should not be set under 5)"
    float_option 5.
      
let overnet_max_search_hits = 
  define_option overnet_section [Proto.options_section_name; "max_search_hits"] 
  "Max number of hits in a search on Overnet"
    int_option 200 
      
let overnet_republish = 
  define_option overnet_section [Proto.options_section_name; "republish"] 
  "Interval (in seconds) before republish files"
    float_option 10800.

let overnet_exclude_peers = 
  define_option overnet_section [Proto.options_section_name; "exclude_peers"] 
  "These IP addresses cannot be peers. Elements are separated by spaces, wildcard=255 ie: use 192.168.0.255 for 192.168.0.* "
    ip_list_option [Ip.of_string "1.0.0.0"]   

let overnet_md4 = define_option overnet_section
  [Proto.options_section_name; "md4"]
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
  define_option overnet_section [Proto.options_section_name; "options_version"] 
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

let connected_peers = ref 0

  
module LimitedList = struct
    
    type key = Ip.t * int
    
    type 'a t = {
        objects_fifo : key Fifo.t; 
        mutable max_objects : int;
        objects_table : (key, key) Hashtbl.t;
      }

    let length t = Fifo.length t.objects_fifo
      
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

    let iter f t =
      Fifo.iter f t.objects_fifo
      
    let set_max_objects t max_objects =
      while Fifo.length t.objects_fifo > max_objects do
            let key = Fifo.take t.objects_fifo in
            Hashtbl.remove t.objects_table key
      done;
      t.max_objects <- max_objects
        
      
    let value_to_t v =
      let t = create 100000 in
      match v with
        SmallList l | List l ->
          List.iter (fun v ->
              match v with
                List [ip; port] | SmallList [ip; port] ->
                  add t (Ip.of_string (value_to_string ip), 
                    value_to_int port)
              | _ -> failwith "not a limited IP list"
          ) l;
          t
      | _ -> failwith "not a limited list"

    let value_of_t t =
      List (List2.tail_map (fun (ip,port) ->
            SmallList [string_to_value (Ip.to_string ip); int_to_value port]
        ) (to_list t))
          
    let option = define_option_class "LimitedList"
          value_to_t value_of_t
      
  end
  
let boot_peers = define_option servers_section
    [Proto.options_section_name; "boot_peers"]
    "List of IP addresses to use to boot Kademlia networks"
    LimitedList.option (LimitedList.create 2000)

let boot_peers_copy = ref []
  
(* the total number of buckets used. We must fill a bucket before using the
next one. When a bucket is full, and we want to add a new peer, we must
either split the bucket, if it is the last one, or remove one peer from the
bucket. *)

let n_used_buckets = ref 0 
let buckets = Array.init 129 (fun _ -> Fifo.create ())

let known_peers = Hashtbl.create 127
let to_ping = ref []

(* 
We keep the data in buckets depending on the number of bits they have
in common with our identifier. When we exceed the desired storage,
we start removing associations from the buckets with the fewest common
bits. Once every 30 minutes, we remove the associations which are older
than 1 hour.
*)

  
(* Argh, we MUST verify how MD4s are compared, from left to right, or from
right to left ?? *)

(* common_bits: compute the number of common bits between a md4 and our
  identifier. *)
  
let common_bits m1 m2 =
  let n =
    let m = Md4.xor m1 m2 in
(*    lprintf () "XOR: %s\n" (Md4.to_bits m); *)
    let rec iter m i = 
(*      lprintf () "iter %d\n" i;  *)
      if i = 16 then 128 else
      let c = m.[i] in
      if c = '\000' then iter m (i+1)
      else
        iter_bit (i*8) (int_of_char c)
    
    and iter_bit nbits c =
(*      lprintf () "iter_bit %d %d\n" nbits c; *)
      if c land 0x80 <> 0 then nbits else
        iter_bit (nbits+1) ( (c lsl 1) land 0xff)
    in
    iter (Md4.direct_to_string m) 0
  in
  (*  lprintf () "Common bits: %s - %s = %d\n"
    (Md4.to_string m1) (Md4.to_string m2) n; *)
  n
  
let bucket_number md4 =
  common_bits md4 !!overnet_md4
  
  (*
(* TODO: this structure MUST disappear. It is not Kademlia ! *)
let global_peers : (Md4.t, peer) Hashtbl.t array Options.option_record = 
  raise Not_found
    *)

(*let firewalled_overnet_peers = Hashtbl.create 13*)
    
let search_hits = ref 0
let source_hits = ref 0

    
let udp_sock = ref None  
  
let overnet_searches = ref []

(********************************************************************


                           FUNCTIONS
  

*********************************************************************)

module Publish (M : sig
      type t    
      val item_name : string
    end) = struct
    
    type publish = {
        mutable publish_last_recv : int; 
        publish_key : (Md4.t * Md4.t);
        publish_value : M.t;
      }
      
    let keyword_to_files_buckets = Array.init 129 (fun _ -> Fifo.create ())
    
    let keyword_to_files_table =  Hashtbl.create 127
    
    let keywords_table =  Hashtbl.create 127
    
    let n_files_stored = ref 0
    let max_soft_stored_files = 1000
    let max_hard_stored_files = 2000
      
    let publish md4 r_md4 r_tags =
      let key = (md4, r_md4) in
      try
        let p = Hashtbl.find keyword_to_files_table key in
        p.publish_last_recv <- last_time ()
      with Not_found ->
          if !n_files_stored < max_hard_stored_files then begin
              let p = {
                  publish_last_recv = last_time ();
                  publish_key = key;
                  publish_value = r_tags;
                } in
              Hashtbl.add keyword_to_files_table key p;
              Hashtbl.add keywords_table md4 p;
              let bucket = bucket_number md4 in
              Fifo.put keyword_to_files_buckets.(bucket) p;
              incr n_files_stored;          
            end
    
    let refresh () =
      Hashtbl.clear keywords_table;
      Hashtbl.clear keyword_to_files_table;
      n_files_stored := 0;
      
      let maxtime = last_time () - 3600 in
      try
        for bucket = 128 downto 0 do
          
          let b = keyword_to_files_buckets.(bucket) in
          let len = Fifo.length b in
          if len > 0 then
            let bb = Fifo.create () in
            keyword_to_files_buckets.(bucket) <- bb;
            for i = 1 to len do
              let p = Fifo.take b in
              if p.publish_last_recv > maxtime && 
                !n_files_stored < max_soft_stored_files then begin
                  Fifo.put bb p;
                  Hashtbl.add keyword_to_files_table p.publish_key p;
                  let (md4, _) = p.publish_key in
                  Hashtbl.add keywords_table md4 p;
                  incr n_files_stored
                end
            done
        done
      with Exit -> ()  
          
    let print buf =
      Printf.bprintf buf "Store for %s\n" M.item_name;
      Printf.bprintf buf "  Stored items: %d (%d < %d)\n" !n_files_stored
        max_soft_stored_files max_hard_stored_files;
      for bucket = 128 downto 0 do
        let len = Fifo.length keyword_to_files_buckets.(bucket) in
        if len > 0 then
          Printf.bprintf buf "    bucket[%d] : %d items\n" bucket len
      done

    let get md4 = 
      List2.tail_map (fun p -> p.publish_value) 
      (Hashtbl.find_all keywords_table md4)
  end

module PublishedKeywords = Publish(struct
      type t = Md4.t * tag list
      let item_name = "Keywords"
    end)
  
module PublishedFiles = Publish(struct
      type t = peer
      let item_name = "Files"
    end)
  
let debug_client ip = false
(*  Ip.matches ip !!overnet_debug_clients *)

let udp_send_direct ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
      Proto.udp_send sock ip port msg

let udp_send p msg =
  p.peer_last_send <- last_time ();
  udp_send_direct p.peer_ip p.peer_port msg

let bootstrap ip port = 
  if Ip.valid ip && Ip.reachable ip && port <> 0 then begin
      LimitedList.add !!boot_peers (ip,port);
      boot_peers_copy := (ip,port) :: !boot_peers_copy
    end
      
let new_peer p =  
  let ip = p.peer_ip in
  if Ip.valid ip && ip <> Ip.localhost && Ip.reachable ip &&
    p.peer_port <> 0 then
    let key = (p.peer_ip, p.peer_port) in
    try
      let pp = Hashtbl.find known_peers key in
      if pp.peer_last_recv < p.peer_last_recv then
        pp.peer_last_recv <- p.peer_last_recv;
      pp
    with _ ->
(* Add the peer to the table of known peers *)
        Hashtbl.add known_peers key p;
(* First, enter the peer in the boot_peers to be able to use at next
restart. *)
        bootstrap p.peer_ip p.peer_port;

(* Now, enter it in the buckets *)
        let bucket = bucket_number p.peer_md4 in
        if bucket < !n_used_buckets then 
(* Maybe the bucket is already full. From Kademlia paper, we should ping
some of the peers in the bucket to decide whether or not to add this peer.
*)
          Fifo.put buckets.(bucket) p
        else begin
            Fifo.put buckets.(!n_used_buckets) p;
            
            if !n_used_buckets < 128 && 
              Fifo.length buckets.(!n_used_buckets) 
              >= max_peers_per_bucket + 1 then
              let b = buckets.(!n_used_buckets) in
              incr n_used_buckets;
              for i = 1 to Fifo.length b do
                let p = Fifo.take b in
                let bucket = bucket_number p.peer_md4 in
                Fifo.put (if bucket >= !n_used_buckets then
                    buckets.(!n_used_buckets) else b) p
              
              done
          end;
        p
  else
    p
      
      
let get_closest_peers md4 nb =
  let bucket = bucket_number md4 in
  let bucket = min !n_used_buckets bucket in
  let nb = ref nb in
  let list = ref [] in
  let add_list bucket =
    let fifo = buckets.(bucket) in
    let rec iter n =
      if n > 0 then
        let p = Fifo.take fifo in
        Fifo.put fifo p;
        if p.peer_last_recv <> 0 then begin
            lprintf () "Adding good search peer %s:%d\n"
              (Ip.to_string p.peer_ip) p.peer_port;
            decr nb;
            list := p :: !list;
          end;
        iter (n-1)
    in
    iter (min !nb (Fifo.length fifo))
  in
  add_list  bucket;
  
  if !nb > 0 then begin
      
      let rec iter bucket =
        if bucket <= !n_used_buckets then begin
            add_list bucket;
            iter (bucket+1)
          end
      in
      iter (bucket+1);
      
      let rec iter bucket =
        if bucket >= 0 then begin
            add_list bucket;
            iter (bucket-1)
        end
      in
      iter (bucket-1);

    end;
  !list

let my_peer () =
  { 
    peer_md4 = !!overnet_md4;
    peer_ip = client_ip None;
    peer_port = !!overnet_port;
    peer_kind = 0;
    peer_tcpport = !!overnet_tcpport;
    peer_last_recv = 0;
    peer_last_send = 0;
  }

let get_any_peers  nb =
(* The last peer of any peer distribution is always us ! *)
  let list = ref [my_peer ()] in
  let nb = ref (nb-1) in
  let add_list bucket =
    let fifo = buckets.(bucket) in
    let rec iter n =
      if n > 0 then
        let p = Fifo.take fifo in
        Fifo.put fifo p;
        decr nb;
        list := p :: !list;
        iter (n-1)
    in
    iter (min !nb (Fifo.length fifo))
  in
  let rec iter bucket =
    if bucket <= !n_used_buckets then begin
        add_list bucket;
        iter (bucket+1)
      end
  in
  iter 0;
  !list
  
(*  
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
    *)

let private_address ip =
  Ip.matches ip [(Ip.of_string "0.0.0.0"); (Ip.of_string "127.0.0.255"); 
		 (Ip.of_string "10.255.255.255"); (Ip.of_string "192.168.255.255") ] 

  (*
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
      *)
  
  (*
(*advertize an uniform distribution then a local distribution (around our MD4) that we are UP*)
let publicize_peers () =
  let global_dist = get_uniform_distribution () in
  let local_dist = get_local_distribution !!overnet_md4 search_max_queries in
  List.iter (fun a -> 
      udp_send a.peer_ip a.peer_port
        (OvernetPublicize (my_peer ())))
  global_dist;
  List.iter
    (fun a -> 
      udp_send a.peer_ip a.peer_port
        (OvernetPublicize(my_peer ())))
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
              (OvernetConnect (my_peer ())))
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
      *)

let add_search_peer s p =
  if p.peer_ip <> client_ip None then
  let key = (p.peer_ip, p.peer_port) in
  if not (Hashtbl.mem s.search_known_peers key) then begin
      Hashtbl.add s.search_known_peers key p;
      let nbits = common_bits p.peer_md4 s.search_md4 in
      Fifo.put s.search_waiting_peers.(nbits) p;      
    end

let create_search kind md4 =   
  lprintf () "create_search\n";
  let s = {
      search_md4 = md4;
      search_kind = kind;
      search_known_peers = Hashtbl.create 127;
      search_queries = 0;
      search_requests = 0;
      search_waiting_peers = Array.init 129 (fun _ -> Fifo.create ());
      search_asked_peers = Array.init 129 (fun _ -> Fifo.create ());
      search_ok_peers = Array.init 129 (fun _ -> Fifo.create ());
      search_start = last_time ();
      (*
      search_last_insert=last_time();
      
      search_not_asked_peers=XorSet.empty;
      search_asked_peers=XorSet.empty;
      search_done_peers=XorSet.empty;
      search_total_peers=0;
      search_known_peers = Hashtbl.create 32;
*)
      
      search_hits = 0;
      search_nresults = 0;
      search_results = Hashtbl.create 13;

(*
  search_publish_files = [];
      search_publish_file = false;

  *)
    } in
  List.iter (add_search_peer s) (get_closest_peers md4 max_search_queries);
  lprintf () "create_search done\n";
  overnet_searches := s :: !overnet_searches;
  s

let create_keyword_search w =   
  let md4 = Md4.string w in
  let search = create_search (KeywordSearch []) md4 in
  search  

      
      (*
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

      *)

  (*
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
  
    *)

let ip_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) ->
      Ip.of_inet_addr inet
  | _ -> assert false
    
let port_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) -> port
  | _ -> assert false

      (*
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
*)

let new_peer_message p = 
  if p.peer_last_recv = 0 then
    incr connected_peers;
(*  lprintf () "*** Updating time for %s:%d\n" (Ip.to_string p.peer_ip) p.peer_port; *)
  p.peer_last_recv <- last_time ()
  
let udp_client_handler t p =
  let other_ip = ip_of_udp_packet p in
  let other_port = port_of_udp_packet p in
  if !verbose_overnet  then 
    lprintf () "UDP FROM %s:%d:\n  %s " 
      (Ip.to_string other_ip) other_port
      (message_to_string t);
  match t with
  
  | OvernetConnect p ->
      new_peer_message p;
      let p = new_peer p in
      udp_send p (OvernetConnectReply (get_any_peers 20))
  
  | OvernetConnectReply ps ->
      let rec iter list =
        match list with
          [] -> ()
        | [p] ->
            new_peer_message p;
            if other_port <> p.peer_port || other_ip <> p.peer_ip then
              lprintf () "Bad IP or port";
            let p = new_peer p in
            ()
        | p :: tail ->
            let p = new_peer p in
            iter tail
      in
      iter ps;
    
  | OvernetPublicize p ->
      new_peer_message p;
      let p = new_peer p in
      udp_send p  (OvernetPublicized (Some (my_peer ())))
  
  | OvernetPublicized None ->
      ()
  
  | OvernetPublicized (Some p) ->
      new_peer_message p;
      let p = new_peer p in
      ()
  
  | OvernetSearch (nresults, md4, from_who) ->
      
      let peers = get_closest_peers md4 nresults in
      udp_send_direct other_ip other_port (OvernetSearchReply (md4,peers))
  
  | OvernetSearchReply (md4, peers) ->
      
      let peers = List2.tail_map new_peer peers in
      List.iter (fun s ->
          if s.search_md4 = md4 then begin
              List.iter (add_search_peer s) peers;
              try
                let p = Hashtbl.find s.search_known_peers
                    (other_ip, other_port) in
                new_peer_message p;
                let nbits = common_bits p.peer_md4 s.search_md4 in      
                Fifo.put s.search_ok_peers.(nbits) p;      
              with _ -> ()
            end
      ) !overnet_searches;
  
  | OvernetUnknown (opcode, s) ->
      lprintf () "Unknown message from %s:%d\n" (Ip.to_string other_ip) other_port;
      lprintf () "\tCode: %d\n" opcode; dump s;
      lprintf () "\n"
  
  | OvernetSearchFilesResults (md4, results) ->      
      List.iter (fun s -> 
          if s.search_md4 = md4 then begin
              s.search_nresults <- s.search_nresults + 1;
              
              begin
                try
                  let p = Hashtbl.find s.search_known_peers
                      (other_ip, other_port) in
                  new_peer_message p;
                with _ -> ()
              end;
              
              match s.search_kind with
                FileSearch file -> ()
              
              | KeywordSearch sss ->
                  incr search_hits;
                  List.iter (fun (r_md4, r_tags) ->
                      if not (Hashtbl.mem s.search_results r_md4) then 
                        begin
                          s.search_hits <- s.search_hits + 1;
                          Hashtbl.add s.search_results r_md4 r_tags;
                          
                          if !verbose_overnet then begin
                              lprintf () "FILE FOUND, TAGS:\n "; 
                              print_tags r_tags; lprint_newline ()
                            end;
                          
                          List.iter (fun ss -> 
                              DonkeyOneFile.search_found true ss r_md4 r_tags;
                          ) sss
                        end
                  ) results
              | FillBuckets -> ()

                  
            end
      ) !overnet_searches
  
  | OvernetSearchSourcesResults (md4, peers) ->      
      List.iter (fun s -> 
          if s.search_md4 = md4 then begin
              s.search_nresults <- s.search_nresults + 1;
              
              begin
                try
                  let p = Hashtbl.find s.search_known_peers
                      (other_ip, other_port) in
                  new_peer_message p;
                with _ -> ()
              end;
              
              match s.search_kind with
                FileSearch file -> 
                  List.iter (fun p ->
                      let ip = p.peer_ip in
                      let port = p.peer_tcpport in
                      if Ip.valid ip && ip_reachable ip && port <> 0 then
                        let s = DonkeySources.find_source_by_uid 
                            (Direct_address (ip, port))  in
                        DonkeySources.set_request_result s 
                           file.file_sources File_new_source;
                        DonkeySources.set_source_brand s true
                  ) peers
                  
              | KeywordSearch sss -> ()
              | FillBuckets -> ()

                  
            end
      ) !overnet_searches

  | OvernetPublishFiles (md4, files) ->
      List.iter (fun (r_md4, r_tags) ->
          PublishedKeywords.publish md4 r_md4 (r_md4, r_tags)
      ) files
      
  | OvernetPublishSources (md4, files) ->
      List.iter (fun p ->
          PublishedFiles.publish md4 p.peer_md4 p
      ) files

  | OvernetGetSearchResults (md4, kind, min, max) ->
      begin
        match kind with
          Search_for_file ->
            let list = PublishedFiles.get md4 in
            let _, list = List2.cut min list in
            let list, _ = List2.cut (max - min) list in
            if list <> [] then
              udp_send_direct other_ip other_port
                (OvernetSearchSourcesResults (md4, list))
        | Search_for_keyword _ ->
            let list = PublishedKeywords.get md4 in
            let _, list = List2.cut min list in
            let list, _ = List2.cut (max - min) list in
            if list <> [] then
              udp_send_direct other_ip other_port
                (OvernetSearchFilesResults (md4, list))
        | _ -> ()
      end
      
      
      
(*
      begin
        try
          let s = Hashtbl.find overnet_searches md4 in

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
  
      *)
  | _ -> failwith "Message not understood"
      
      (*
    
  | OvernetConnect p ->
      if !verbose_overnet && debug_client other_ip then
        lprintf "CONNECT (%s): sender IP was %s:%d kind=%d\n" (Md4.to_string p.peer_md4) (Ip.to_string p.peer_ip) p.peer_port p.peer_kind; 
      if (p.peer_md4 <> !!client_md4) && (p.peer_md4 <> !!overnet_md4) && not (Hashtbl.mem connected_clients p.peer_md4) 
      then begin
          let this_peer =
            {
              peer_md4=(!!overnet_md4);
              peer_ip=(client_ip None);
              peer_port=(!!overnet_port);
              peer_tcpport = 0;
              peer_kind=0;
              peer_last_msg=last_time();
            } in
          let ip = (change_private_address p.peer_ip other_ip) in
(* verificare per le Indirect_location *)

          let c = new_client (Direct_address (p.peer_ip, p.peer_port)) in
          c.client_ip <- p.peer_ip;
          c.client_connect_time <- last_time ();
          DonkeySources.set_source_brand c.client_source true;
          c.client_brand <- Brand_overnet;
          set_client_state c (Connected (-1)); 
          Hashtbl.add connected_clients p.peer_md4 c;
          
          add_global_peer p;
          
          udp_send p.peer_ip p.peer_port (OvernetConnectReply([my_peer ()]));

(* send the PUBLICIZED message *)
          udp_send other_ip p.peer_port (OvernetPublicized (Some (my_peer ())))
        
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
  
  | OvernetPublicize p ->
      begin
        if !verbose_overnet && debug_client other_ip then
          lprintf "PUBLICIZE (%s): sender IP was %s\n" 
          (Md4.to_string p.peer_md4) (Ip.to_string p.peer_ip); 
        add_global_peer p;
(*
        {
          peer_md4=md4;
          peer_ip=(change_private_address ip other_ip);
          peer_port=port;
          peer_tcpport = 0;
          peer_kind=kind;
          peer_last_msg=last_time();
        };
*)
        
(* send the PUBLICIZED message *)
        udp_send other_ip p.peer_port (OvernetPublicized (Some (my_peer ())))
      end      
  
  | OvernetPublicized p -> 
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
      *)
      
let query_next_peers () =
  List.iter (fun s ->
      let nresults = match s.search_kind with
          FillBuckets -> 10
        | _ -> 2
      in
      
      let rec iter nbits todo =
        if  nbits >= 0 then
          let len = Fifo.length s.search_waiting_peers.(nbits) in
          if len > 0 then
            let p = Fifo.take  s.search_waiting_peers.(nbits) in

            udp_send p (OvernetSearch (nresults, s.search_md4, Some p.peer_md4));
            s.search_queries <- s.search_queries + 1;
            Fifo.put  s.search_asked_peers.(nbits) p;
            
            (if todo > 1 then iter nbits (todo - 1))
          else
            iter (nbits-1) todo
        else
        if s.search_queries < max_search_queries then
          List.iter (fun p ->
              add_search_peer s p
          ) (get_closest_peers s.search_md4 max_search_queries)
      in
      iter 128 2
      
  ) !overnet_searches
  
(*
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
      *)

let recover_file file =
  if file_state file = FileDownloading then
    let search = create_search (FileSearch file) file.file_md4 in
    ()
    
let check_current_downloads () =
  List.iter recover_file !DonkeyGlobals.current_files
  
let enable enabler = 
  if !!enable_overnet then begin
      let sock = (UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
          (!!overnet_port) (Proto.udp_handler udp_client_handler)) in
      udp_sock := Some sock;
      
      UdpSocket.set_write_controler sock udp_write_controler;

(* every 3min try a new publish search, if any 
      add_session_timer enabler 180. (fun _ ->
          if !!enable_overnet then begin
              find_new_peers ();
              do_publish_shared_files ();
            end
      );
*)
      
      add_session_timer enabler 1. (fun _ ->
          if !!enable_overnet then 
            let my_peer = my_peer () in

(* ping old peers regularly *)
            begin
              match !to_ping with
                [] -> ()
              | p :: tail ->
                  to_ping := tail;
                  p.peer_last_send <- last_time ();
                  udp_send p (OvernetPublicize my_peer);
            end;

(* ping unknown peers *)
            begin
              match !boot_peers_copy with
                [] -> 
                  boot_peers_copy := LimitedList.to_list !!boot_peers
              | _ ->
                  if !connected_peers < 100 then
                    for i = 1 to 5 do
                      match !boot_peers_copy with
                        [] -> ()
                      | (ip, port) :: tail ->
                          boot_peers_copy := tail;
                          udp_send_direct ip port (OvernetConnect my_peer);
                          ()
                    
                    done
            end;
      );
      
      
      LimitedList.set_max_objects !!boot_peers  2000 ;
      add_timer 60. (fun _ ->
          LimitedList.set_max_objects !!boot_peers  2000;
      );
      
      add_session_timer enabler 10. (fun _ ->
          
          List.iter (fun s ->
              
              if s.search_requests < max_search_requests then begin
                  let nrequests = 
                    match s.search_kind with
                      FillBuckets -> 0
                    | FileSearch _ -> 1
                    | KeywordSearch _ -> 5
                  in
(* If we use fewer than 5 buckets, we shouldn't wait for 5 buckets in the
  search to start asking for results... *)
                  let min_bucket = mini !n_used_buckets 5 in
                  for i = 1 to nrequests do
                    try
                      for j = 128 downto min_bucket do
                        if Fifo.length s.search_ok_peers.(j) > 0 then
                          let p = Fifo.take s.search_ok_peers.(j) in
                          udp_send p (
                            OvernetGetSearchResults (s.search_md4,
                              (match s.search_kind with
                                  FillBuckets -> Search_for_file
                                | FileSearch _ -> Search_for_file
                                | _ -> Search_for_keyword None
                              ), 0, 100));
                          raise Exit
                      done
                    with Exit -> ()
                  done
                end
          
          ) !overnet_searches
      
      );
      
      add_session_timer enabler 60. (fun _ ->

(* compute which peers to ping in the next minute *)
          to_ping := [];
          let n_to_ping = ref 0 in
          begin
            try
              for i = !n_used_buckets downto 0 do 
                let b = buckets.(i) in
                let overtime = last_time () - 1800 in
                Fifo.iter (fun p ->
                    if p.peer_last_recv < overtime &&
                      p.peer_last_send < overtime then begin
                        to_ping := p :: !to_ping;
                        incr n_to_ping;
                        if !n_to_ping = 60 then raise Exit
                      end
                ) b
              done;
            with Exit -> ()
          end;

(* remove searches that are older than 5 minutes *)
          overnet_searches := List.filter (fun s ->
              s.search_requests < max_search_requests || 
              s.search_start > last_time () - 300
          ) !overnet_searches;
      );
      
      add_session_timer enabler 300. (fun _ ->
          let overtime = last_time () - 3600 in
          for i = !n_used_buckets - 1 downto 0 do
            let len = Fifo.length buckets.(i) in
            if len > max_peers_per_bucket then begin
                
(* First remove very old peers that we have tried to ping without success *)
                for i = 1 to len do
                  let p = Fifo.take buckets.(i) in
                  if p.peer_last_recv > overtime ||
                    p.peer_last_send < overtime then
                    Fifo.put buckets.(i) p
                done;
                if Fifo.length buckets.(i) > max_peers_per_bucket then begin
                    
(* Second, remove very old peers, even if tried *)
                    for i = 1 to Fifo.length buckets.(i) do
                      let p = Fifo.take buckets.(i) in
                      if p.peer_last_recv > overtime then
                        Fifo.put buckets.(i) p
                    done;
                    
                    if Fifo.length buckets.(i) > max_peers_per_bucket then begin
(* Third, remove everything except max_peers_per_bucket peers *)
                        for i = 1 to Fifo.length buckets.(i)
                          - max_peers_per_bucket do
                          let p = Fifo.take buckets.(i) in
                          ()
                        done;
                        
                      end
                  end
              end
          done
      );
      
(* Every hour, try a query on our UID to fill our buckets *)
      add_session_timer enabler 3600. (fun _ ->
          let s = create_search FillBuckets !!overnet_md4 in
          ()
      ); 
      let s = create_search FillBuckets !!overnet_md4 in
      
      add_session_option_timer enabler overnet_query_peer_period  (fun _ ->
          if !!enable_overnet then begin
              query_next_peers ()
            end
      );

(*
(* every 3h for re-publish *)
      add_session_timer enabler !!overnet_republish (fun _ ->
          if !!enable_overnet then begin
              publish_shared_files ()
            end
      );
      
*)
(* every 15min for light operations *)
      add_session_timer enabler 900. (fun _ ->
          if !!enable_overnet then begin
              check_current_downloads ();
            end
      );
      add_timer 30. (fun _ ->
          if !!enable_overnet then begin
              check_current_downloads ();
            end            
      );

      (*
      
      add_timer 20. (fun timer -> 
          if !!enable_overnet then begin
              find_new_peers (); 
(*publish is in fact controled by do_publish_shared_files, every 2 min*)   
              publish_shared_files ()
            end
      )

*)
  end
    
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
  
  (*
let connected_peers () =
  List.map (fun p -> p.peer_ip, p.peer_port)  (get_uniform_distribution ())

  *)


let parse_overnet_url url =
  match String2.split (String.escaped url) '|' with
  | "fha://" :: "boot" :: name :: port :: _
  | "boot" :: name :: port :: _ ->
      let ip = Ip.from_name name in
      let port = int_of_string port in
      bootstrap ip port;
      true
  | _ -> false
      
let register_commands list =
  register_commands
    (List2.tail_map (fun (n,f,h) -> (n, "Network/Overnet", f,h)) list)
  
let _ =
  register_commands 
    (List.map (fun (command, args, help) ->
        command_prefix ^ command, args, help)
    [
    "boot", Arg_two (fun ip port o ->
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        bootstrap ip port;
        Printf.sprintf "peer %s:%d added" (Ip.to_string ip) port
    ), "<ip> <port> :\t\t\tadd an Overnet peer";
    
    "link", Arg_multiple (fun args o ->        
        let buf = o.conn_buf in
        let url = String2.unsplit args ' ' in
        if parse_overnet_url url then
          "download started"
        else "bad syntax"
    ), "<fhalink> :\t\t\tdownload fha:// link";
    
    "stats", Arg_none (fun o ->
        let buf = o.conn_buf and sum = ref 0 in
        Printf.bprintf buf "Overnet statistics:\n"; 
        Printf.bprintf buf "  Search hits: %d\n" !search_hits;
        Printf.bprintf buf "  Source hits: %d\n" !source_hits;
        
        List.iter (fun s ->
            Printf.bprintf buf 
              "Search %s for %s\n"
              
(* ", %d asked, %d done, %d hits, %d results) %s%s\n" *)
              (match s.search_kind with
                KeywordSearch _ -> "keyword"
                | FileSearch _ -> "file"
                | FillBuckets -> "fillbuckets" )
              (Md4.to_string s.search_md4);
              for i = 128 downto 0 do
                let npeers = Fifo.length s.search_waiting_peers.(i) in
                let nasked = Fifo.length s.search_asked_peers.(i) in
                if npeers > 0 || nasked > 0 then
                  Printf.bprintf buf
                  "   nbits[%d] = %d peers not asked, %d peers asked\n"
                    i npeers nasked
              done;
              
              (*
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
  *)
        ) !overnet_searches;
        
        "";
    ), ":\t\t\t\tOvernet Stats";
    
    "web", Arg_multiple (fun args o ->
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
    
    "md4", Arg_none (fun o -> "MD4 is " ^ (Md4.to_string !!overnet_md4);
    ), ":\t\t\t\t\tget client MD4 address on the overnet network";
    
    "store", Arg_none (fun o -> 
        let buf = o.conn_buf in
        
          Printf.bprintf buf "Overnet store:\n"; 
          
          PublishedKeywords.print buf;
          PublishedFiles.print buf;
        
        ""
    ), ":\t\t\t\tdump the Overnet File Store";
    

      (*
    "tst", Arg_two (fun a b o ->
        let md41 = Md4.of_string a in
        let md42 = Md4.of_string b in
        store_published_file md41 md42
          [
          string_tag "filename" "john" ;
          int_tag "size" (Random.int 200);
        ] 0;
        ""
    ), ":\t\t\t\t\t(not documented)";
    
    "tst2", Arg_two (fun a b o ->
        let md4 = Md4.of_string a in
        let size = int_of_string b in
        get_results_from_query (Ip.of_string "10.0.0.10") (4665) md4 0 size;
        ""
    ), ":\t\t\t\t(not documented)";
*)
      
    
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
          udp_send_direct
            (Ip.of_string ip)
          (int_of_string port)
          (OvernetUnknown (opcode,msg));
          lprintf () "Sending UDP message %d to %s:%s\n" opcode ip port;
          dump msg; lprintf () "\n"; "Sending UDP message"
        with _ ->
            lprintf () "Unable to send UDP message\n"; "Unable to send UDP message"
    ), ":\t\t\t\tsend UDP message (<ip> <port> <msg in hex>)";

      "buckets", Arg_none (fun o ->
          let buf = o.conn_buf in
          Printf.bprintf buf "Number of used buckets %d with %d peers\n"
            !n_used_buckets !connected_peers;
          for i = 0 to !n_used_buckets do
            Printf.bprintf buf "   bucket[%d] : %d peers\n"
              i (Fifo.length buckets.(i));
          done;
          ""
      ), ":\t\t\t\tprint buckets table status";

      "boots", Arg_none (fun o ->
          let buf = o.conn_buf in
          LimitedList.iter (fun (ip, port) ->
              Printf.bprintf buf "   %s:%d\n" (Ip.to_string ip) port;
          ) !!boot_peers;
          Printf.sprintf "Boot peers: %d\n" (LimitedList.length !!boot_peers);

          
      ), ":\t\t\t\tprint buckets table status";
          
      
      (*
    "search", Arg_one (fun m o ->
      let s = create_search (KeywordSearch []) (Md4.of_string m) in
      Hashtbl.add overnet_searches s.search_md4 s;
      "Searching for md4"
    ), ":\t\t\t\tsearch keyword (<md4>)";
*)
      
  ]);
  ()
  
let overnet_search (ss : search) =
  if !!overnet_search_keyword then
    let q = ss.search_query in
    lprintf () "========= overnet_search =========\n";
    let ws = keywords_of_query q in
    List.iter (fun w -> 
        lprintf () "overnet_search for %s\n" w;
        let s = create_keyword_search w in
        Hashtbl.iter (fun r_md4 r_tags -> 
            DonkeyOneFile.search_found true ss r_md4 r_tags) s.search_results;
        begin
          match s.search_kind with
            KeywordSearch sss -> s.search_kind <- KeywordSearch (ss :: sss)
          | _ -> ()
        end;
    )
    ws
  
let _ =
  CommonWeb.add_redirector_info Proto.redirector_section (fun buf ->
      let peers = get_any_peers 32 in
      buf_int buf (List.length peers);
      List.iter (fun p ->
          buf_ip buf p.peer_ip; 
          buf_int16 buf p.peer_port) 
      peers;
  )

  let cancel_recover_file file = ()
  
end