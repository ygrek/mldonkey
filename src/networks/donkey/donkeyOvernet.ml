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

open CommonTypes
open CommonGlobals
open CommonInteractive
open CommonNetwork
open CommonOptions
open CommonSources

open DonkeyTypes
open DonkeyGlobals

(*
* Store published files.
*)

type peer =
  {
    peer_md4 : Md4.t;
    mutable peer_ip : Ip.t;
    mutable peer_port : int;
    mutable peer_tcpport : int;
    mutable peer_country_code : int option;
    mutable peer_kind : int;
    mutable peer_expire : int;
    mutable peer_last_send : int;
    peer_created : int;
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
  let hours = (last_time () - p.peer_created) / 3600 in
  let mins = (last_time () - p.peer_created) / 60 in
  Printf.bprintf buf " {%s:%s:%d%s k=%d ls=%d ex=%d cr=%d h=%d m=%d%s}\n"
    (Md4.to_string p.peer_md4) (Ip.to_string p.peer_ip) p.peer_port
    (if p.peer_tcpport <> 0 then Printf.sprintf " tcp=%d" p.peer_tcpport
    else "") p.peer_kind p.peer_last_send p.peer_expire p.peer_created
    hours mins (if p.peer_expire > last_time () && p.peer_last_send <> 0 then " OK" else "")
    

let message_to_string t =
  let buf = Buffer.create 100 in
  begin
    match t with
      OvernetConnect p ->
        Buffer.add_string buf "Connect\n";
        print_peer buf p
    | OvernetConnectReply peers ->
        Buffer.add_string buf "ConnectReply\n";
        List.iter (print_peer buf) peers
    | OvernetPublicize p ->
        Buffer.add_string buf "Publicize\n";
        print_peer buf p
    | OvernetPublicized p ->
        Buffer.add_string buf "Publicized\n";
        (match p with
          Some p -> print_peer buf p
        | None -> ())
    | OvernetSearch (nresults, md4, _) ->
        Buffer.add_string buf "Search\n";
        Printf.bprintf buf " target=%s nresults=%d\n"
          (Md4.to_string md4) nresults
    | OvernetSearchReply (target, peers) ->
        Buffer.add_string buf "SearchReply\n";
        Printf.bprintf buf " target=%s npeers=%d\n"
          (Md4.to_string target) (List.length peers);
        List.iter (print_peer buf) peers
    | OvernetGetSearchResults (target, _, _, _) ->
        Printf.bprintf buf "GetSearchResults %s\n" (Md4.to_string target)
    | OvernetSearchFilesResults (target, results) ->
        Printf.bprintf buf "SearchFilesResults %s\n"
          (Md4.to_string target);
        List.iter (fun (r_md4, r_tags) ->
            Printf.bprintf buf "  %s\n   " (Md4.to_string r_md4);
            bprint_tags buf r_tags ;
            Printf.bprintf buf "\n";
        ) results
    | OvernetSearchSourcesResults (target, peers) ->
        Printf.bprintf buf "SearchSourcesResults %s\n"
          (Md4.to_string target);
        List.iter (print_peer buf) peers
    | OvernetPublishFiles (target, results) ->
        Printf.bprintf buf "Publish %s\n"
          (Md4.to_string target);
        List.iter (fun (r_md4, r_tags) ->
            Printf.bprintf buf "  %s\n   " (Md4.to_string r_md4);
            bprint_tags buf r_tags ;
            Printf.bprintf buf "\n";
        ) results
    | OvernetPublishSources (target, peers) ->
        Printf.bprintf buf "PublishSources %s\n"
          (Md4.to_string target);
        List.iter (print_peer buf) peers
    | OvernetPublished target ->
        Printf.bprintf buf "Published %s\n" (Md4.to_string target)
    | OvernetPeerNotFound p ->
        Printf.bprintf buf "PeerNotFound\n";
        print_peer buf p
    | OvernetUnknown21 p ->
        Printf.bprintf buf "Unknown21\n";
        print_peer buf p
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

module KnownPeers = Weak.Make(struct
      type t = peer
      let hash c = Hashtbl.hash (c.peer_ip, c.peer_port)
      let equal x y = x.peer_port = y.peer_port
          && x.peer_ip = y.peer_ip
  end)

type search_for =
| FileSearch of file
| KeywordSearch of CommonTypes.search
| FillBuckets

(* using "=" to structurally compare files (-> search_fors) 
   will result in an OutOfMemory exception *)
let search_for_equals sf1 sf2 =
  match sf1 with 
    | FileSearch f1 -> 
      (match sf2 with 
        | FileSearch f2 when f1 == f2 -> true
        | _ -> false)
    | KeywordSearch k1 ->
      (match sf2 with
        | KeywordSearch k2 when k1 == k2 -> true
        | _ -> false)
    | _ -> true

type overnet_search = {
    search_md4 : Md4.t;
    mutable search_kind : search_for;

(* Peer queues of the search *)
(* Stage 1: This peers we know for this search *)
    search_peers : peer Fifo.t array;
(* Stage 2: We send a OvernetSearch *)
    search_asked_peers : KnownPeers.t;
(* Stage 3: We received from this peer a OvernetSearchReply *)
    search_ok_peers : KnownPeers.t;
(* Stage 4: We picked peers from search_ok_peers and send them a 
   OvernetSearch.*Results *)
    search_result_asked_peers : KnownPeers.t;

    mutable search_queries : int;
    mutable search_requests : int;
    mutable search_start : int;
    mutable search_lifetime : int;
    mutable search_last_recv : int;
    mutable search_results : (Md4.t, tag list) Hashtbl.t;
    mutable search_nresults : int; (* number of results messages *)
    mutable search_hits : int;     (* number of diff results *)
    }

exception MessageNotImplemented

module Make(Proto: sig

      val enable_overnet : bool Options.option_record
      val overnet_port : int Options.option_record
      val overnet_tcpport : int Options.option_record
      val overnet_section : Options.options_section

      val checking_kind_timeout : int

      val redirector_section : string
      val options_section_name : string
      val command_prefix : string
      val source_brand : bool

      val udp_send : UdpSocket.t -> Ip.t -> int -> bool -> t -> unit
      val udp_handler : (t -> UdpSocket.udp_packet -> unit) ->
        UdpSocket.t -> UdpSocket.event -> unit

      val web_info : string
      val web_info_descr : string
    end) = struct

    open Proto

    let log_prefix = (if Proto.redirector_section = "DKKO" then "[OV]" else "[KAD]")

    let lprintf_nl fmt =
      lprintf_nl2 log_prefix fmt

    let lprintf_n fmt =
      lprintf2 log_prefix fmt

    let lprintf fmt =
      lprintf2 log_prefix fmt

(********************************************************************

                     STATIC MEMORY

*********************************************************************)

let max_peers_per_bucket = 20
let max_peers_per_prebucket = 20
(* number of peers a search start with *)
(* get_any_peers and get_closest_peers change the order of the peers in the bucket fifo *)
let init_peers_per_search = ((max_peers_per_bucket*2)/3)
let max_boot_peers = 200

let is_enabled = ref false


(********************************************************************

                           OPTIONS

*********************************************************************)

let overnet_store_size =
  define_option overnet_section [Proto.options_section_name; "store_size"] "Size of the filename storage used to answer queries"
    int_option 2000

let overnet_search_keyword =
  define_option overnet_section [Proto.options_section_name; "search_keyword"]
  "allow extended search to search on overnet" bool_option true

let overnet_republish =
  define_option overnet_section [Proto.options_section_name; "republish"]
  "Interval (in seconds) before republish files"
    float_option 10800.

let overnet_md4 = define_option overnet_section
  [Proto.options_section_name; "md4"]
  "The MD4 of the Overnet client" Md4.option (Md4.random ())

let overnet_update_nodes = define_option overnet_section
  [Proto.options_section_name; "update_nodes"]
  "Set this option to false if you don't want to receive new Overnet peers"
    bool_option true

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
  ]


(********************************************************************


                        MUTABLE STRUCTURES


*********************************************************************)

let dummy_peer =
  {
    peer_md4 = Md4.null;
    peer_ip = Ip.null;
    peer_port = 0;
    peer_tcpport = 0;
    peer_country_code = None;
    peer_kind = 0;
    peer_expire = 0;
    peer_last_send = 0;
    peer_created = 0;
  }

let connected_peers = ref 0
let pre_connected_peers = ref 0

let check_peer_country_code p =
  if Geoip.active () then
    match p.peer_country_code with
    | None -> p.peer_country_code <- Geoip.get_country_code_option p.peer_ip
    | _ -> ()

let is_overnet_ip ip cc =
  let is_not_banned ip =
       match !Ip.banned (ip, cc) with
       None -> true
     | Some reason ->
         if !verbose_overnet then
           lprintf_nl "%s blocked: %s"
             (Ip.to_string ip) reason;
         false
  in
  Ip.usable ip && Ip.of_string "1.0.0.0" <> ip && is_not_banned ip

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
      if ip <> Ip.localhost && is_overnet_ip ip None &&
        not (Hashtbl.mem t.objects_table key) then
        begin
          Hashtbl.add t.objects_table key key;
          Fifo.put t.objects_fifo key;
          if Fifo.length t.objects_fifo > t.max_objects then
            let key = Fifo.take t.objects_fifo in
            Hashtbl.remove t.objects_table key
        end

    let get t =
      let key = Fifo.take t.objects_fifo in
      Hashtbl.remove t.objects_table key;
      key

    let clear t =
      Fifo.clear t.objects_fifo;
      Hashtbl.clear t.objects_table

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
    LimitedList.option (LimitedList.create max_boot_peers)

(* peers we may ping: We got only an ip and port *)
let unknown_peers = LimitedList.create 2000

(* the total number of buckets used. We must fill a bucket before using the
next one. When a bucket is full, and we want to add a new peer, we must
either split the bucket, if it is the last one, or remove one peer from the
bucket.

What we want: we don't want to put too many peers in the buckets.
The buckets should preferably contain peers that have already send us a
  message, because we are not sure for other peers.
  *)

(* FIXME: The function that adds the peers into the buckets should increase 
   n_used_buckets when needed, but I have the feeling it does not work as expected, 
   so I set n_used_buckets to some value.*)
let n_used_buckets = ref 42

(* We distinguish between buckets and prebuckets: peers in buckets are peers
that sent us a message in the last hour, whereas peers in the prebuckets
are peers that we are not sure of. *)
let buckets = Array.init 129 (fun _ -> Fifo.create ())
let prebuckets = Array.init 129 (fun _ -> Fifo.create ())

(* The peers we want to ping in the next 60 seconds *)
let to_ping = ref []

(* Every peer we have in the buckets und prebuckets goes into known_peers *)
let known_peers = KnownPeers.create 1023

(*
We keep the data in buckets depending on the number of bits they have
in common with our identifier. When we exceed the desired storage,
we start removing associations from the buckets with the fewest common
bits. 
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

(* Stats *)
let search_hits = ref 0
let source_hits = ref 0

(* To know if we are can receive UDP packets *)
let global_last_recv = ref 0
let global_last_send = ref 0

let udp_sock = ref None

let overnet_searches = ref []
let current_files = ref []


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

let remove_current_file file =
  current_files := List.filter (fun (f,start,last) ->
    if (f != file) then true else false
  ) !current_files


let add_current_file file =
  current_files := (file, last_time (), 0) :: !current_files

let debug_client ip = false
(*  Ip.matches ip !!overnet_debug_clients *)

let checking_kind p =
(* according to eMule 0.46c *)
  if (last_time () - p.peer_last_send) < 10 ||
     p.peer_kind = 4 then () else begin
    p.peer_kind <- p.peer_kind + 1;
    p.peer_expire <- last_time () + Proto.checking_kind_timeout;
  end;
  ()
  
let new_peer_message p =
(*  lprintf () "*** Updating time for %s:%d\n" (Ip.to_string p.peer_ip) p.peer_port; *)
  let calc_peer_kind p =
    (* according to eMule 0.46c *)
    let hours = (last_time () - p.peer_created) / 3600 in
    let lt = last_time () in
    match hours with
      0 -> (2, lt + 3600)  (* new kind 2, 1h timeout to ping *) 
    | 1 -> (1, lt + 5400)  (* kind 1, 1 1/2h timeout *)
    | _ -> (0, lt + 7200)  (* kind 0, 2h timeout *)
  in
  let (new_kind,new_expire) = calc_peer_kind p in
  p.peer_kind <- new_kind;
  p.peer_expire <- new_expire;
  ()

let udp_send_direct ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
(* Why check this? Because it may have been blocked since it was added *)    
     if ip <> Ip.localhost && is_overnet_ip ip None && port <> 0 then begin
      Proto.udp_send sock ip port false msg;
      global_last_send := last_time ()
     end

let udp_send_ping ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
(* Why check this? Because it may have been blocked since it was added *)    
     if ip <> Ip.localhost && is_overnet_ip ip None && port <> 0 then begin
      Proto.udp_send sock ip port true msg;
      global_last_send := last_time ()
     end

let udp_send p msg =
  p.peer_last_send <- last_time ();
  udp_send_direct p.peer_ip p.peer_port msg

let bootstrap ip port =
  if !!overnet_update_nodes && is_overnet_ip ip None &&
     port <> 0 && not (KnownPeers.mem known_peers { dummy_peer with peer_port = port ; peer_ip = ip } ) then
       LimitedList.add unknown_peers (ip,port)

let new_peer p =
  check_peer_country_code p;
  let ip = p.peer_ip in
  let port = p.peer_port in
 
    try
      let pp = KnownPeers.find known_peers p in
      (* TODO: check for changes in ip:port? Or Hash? *)
      pp
    with _ ->

   if ip <> Ip.localhost && is_overnet_ip ip p.peer_country_code
     && port <> 0 && p.peer_created <> 0 then
   
        let bucket = bucket_number p.peer_md4 in

(* Add the peer to the table of known peers *)
        if bucket <> 128 && p.peer_md4 <> Md4.null then
          KnownPeers.add known_peers p;

(* First, enter the peer in the boot_peers to be able to use at next
restart. *)
        if bucket <> 128 && p.peer_md4 <> Md4.null then
            bootstrap p.peer_ip p.peer_port;

(* bucket = 128 is returned for our very own ID, we dont want it in our buckets 
   Then, enter it in the buckets *)
        if bucket < !n_used_buckets && bucket <> 128 &&
           p.peer_md4 <> Md4.null then begin

(* If this is a good peer add it directly to the bucket. Helps at startup *)
            if p.peer_kind < 3 && Fifo.length buckets.(bucket) < max_peers_per_bucket then begin
                Fifo.put buckets.(bucket) p;
                incr connected_peers;
            end 
            else if Fifo.length prebuckets.(bucket) >= max_peers_per_prebucket then
            begin
              let pp = Fifo.take prebuckets.(bucket) in
              Fifo.put prebuckets.(bucket)
(* If the head of the bucket is not dead we should keep it *)
                (if pp.peer_kind < 4 || pp.peer_expire > last_time () then pp else p);
            end  
            else begin
               Fifo.put prebuckets.(bucket) p;
               incr pre_connected_peers
            end
        end
        else if !n_used_buckets < 128 && bucket <> 128 &&
                p.peer_md4 <> Md4.null then begin
            Fifo.put prebuckets.(!n_used_buckets) p;
            incr pre_connected_peers;
(* increase n_used_buckets if the buckets and prebuckets are full *)
(* FIXME: Does it work correctly? Until sure, n_used_buckets is set to some value *)
            while !n_used_buckets < 128 &&
              Fifo.length prebuckets.(!n_used_buckets)
              = max_peers_per_bucket do
              let b = prebuckets.(!n_used_buckets) in
              incr n_used_buckets;
              for i = 1 to Fifo.length b do
                let pp = Fifo.take b in
                let bucket = bucket_number p.peer_md4 in
                Fifo.put (if bucket >= !n_used_buckets then
                    prebuckets.(!n_used_buckets) else b) pp
              done
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
        (* kind < 3 so we do not send too much requests and avoid dead contacts not 
           yet removed because of timeouts *)
        (* TODO: Keep order? Then we need a in_use flag? *)
        if p.peer_kind < 3 && p.peer_expire > last_time () then begin
            (* if !verbose_overnet then begin
             lprintf_nl "Adding search peer %s:%d" 
              (Ip.to_string p.peer_ip) p.peer_port;
            end; *)
            decr nb;
            list := p :: !list;
          end;
        iter (n-1)
    in
    iter (min !nb (Fifo.length fifo))
  in
  add_list bucket;

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
    peer_country_code = None;
    peer_last_send = 0;
    peer_expire = 0;
    peer_created = 0;
  }

let get_any_peers  nb =
(* The last peer of any peer distribution is always us ! *)
  let list = ref [my_peer ()] in
  let nb = ref (nb-1) in
  let add_list bucket =
    let fifo = buckets.(bucket) in
    let rec iter n =
      if n > 0 then
      (* TODO: keep order *)
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

let add_search_peer s p =
  if p.peer_ip <> Ip.localhost && is_overnet_ip p.peer_ip p.peer_country_code && 
     p.peer_port <> 0 && p.peer_created <> 0 then begin
    let nbits = common_bits p.peer_md4 s.search_md4 in
(* Don't add ourself *)
    if not (nbits = 128 && s.search_kind == FillBuckets) then begin
      try
        let is_in pp =
          if pp.peer_ip = p.peer_ip && 
             pp.peer_port = p.peer_port then
            raise Exit
        in
        Fifo.iter is_in s.search_peers.(nbits);
        Fifo.put s.search_peers.(nbits) p;
      with Exit -> ()
    end
  end

let create_search kind md4 =
  if !verbose_overnet then lprintf_nl "create_search";
  let starttime = last_time () + (List.length !overnet_searches) in
  let s = ref {
      search_md4 = md4;
      search_kind = kind;
      search_queries = 0;
      search_requests = 0;
      search_peers = Array.init 129 (fun _ -> Fifo.create ());
      search_asked_peers = KnownPeers.create 129;
      search_ok_peers = KnownPeers.create 129;
      search_result_asked_peers = KnownPeers.create 129;
(* taken from eMule0.47c-Sources/srchybrid/kademlia/kademlia/Defines.h  *)
      search_lifetime = (match kind with
                       _ -> 45 );(* SEARCH_LIFETIME *)
      search_start = (match kind with
                       KeywordSearch s -> last_time ()
                     | FillBuckets -> last_time ()
                     | FileSearch s -> starttime);
      search_last_recv = last_time ();
      search_hits = 0;
      search_nresults = 0;
      search_results = Hashtbl.create 64;
    } in
  begin try
    List.iter (fun ss ->
      if ss.search_md4 = !s.search_md4 && (search_for_equals ss.search_kind !s.search_kind) then begin
(*       ss.search_start <- !s.search_start; *)
       s := ss;
       raise Exit;
      end
    ) !overnet_searches;
    begin match kind with
      FillBuckets -> List.iter (add_search_peer !s) (get_any_peers init_peers_per_search)
    | _ -> List.iter (add_search_peer !s) (get_closest_peers md4 init_peers_per_search)
    end;
    if !verbose_overnet then lprintf_nl "create_search done";
    overnet_searches := !s :: !overnet_searches;
  with Exit -> () end;
  !s

let create_keyword_search w s =
  let md4 = Md4.string w in
  let search = create_search (KeywordSearch s) md4 in
  search

(* query the best todo peers of a search s or ask it for results *)
let rec overnet_search_iter s nbits todo =
        let nresults = match s.search_kind with
            FillBuckets -> 10
          | _ -> 2
        in
        if nbits >= 0 then
          let len = Fifo.length s.search_peers.(nbits) in
          if len > 0 then
            let process_peer p =
              if not (KnownPeers.mem s.search_asked_peers p) then begin
(* Not asked: send a OvernetSearch *)
                checking_kind p;
                udp_send p (OvernetSearch (nresults, s.search_md4, Some p.peer_md4));
                s.search_queries <- s.search_queries + 1;
                KnownPeers.add s.search_asked_peers p;
                raise Exit;
              end
              else if (KnownPeers.mem s.search_ok_peers p) && 
                 not (KnownPeers.mem s.search_result_asked_peers p) then begin
(* Is ok but we did not request a result yet *)
                let kind = match s.search_kind with
                    FillBuckets -> Search_for_file
                  | FileSearch _ -> Search_for_file
                  | _ -> Search_for_keyword None
                in
                checking_kind p;
                udp_send p ( OvernetGetSearchResults (s.search_md4, kind, 0, 100));
                s.search_requests <- s.search_requests + 1;
                KnownPeers.add s.search_result_asked_peers p; 
                raise Exit;
              end
              (* raised an Exit if we send something to the peer *)
              in
            try
              Fifo.iter process_peer s.search_peers.(nbits);
              overnet_search_iter s (nbits-1) todo;
            with Exit ->
              (if todo > 1 then overnet_search_iter s nbits (todo - 1))
          else
            overnet_search_iter s (nbits-1) todo
        else
        begin
          if !verbose_overnet then
            lprintf_nl "overnet_search_iter: call add_search_peer";
          List.iter (add_search_peer s) (get_closest_peers s.search_md4 init_peers_per_search);
        end

let ip_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) ->
      Ip.of_inet_addr inet
  | _ -> assert false

let port_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) -> port
  | _ -> assert false

let udp_client_handler t p =
  let other_ip = ip_of_udp_packet p in
  let other_port = port_of_udp_packet p in
  if !verbose_overnet then
    lprintf_nl "UDP from %s:%d type %s"
      (Ip.to_string other_ip) other_port
      (message_to_string t);
  (* Emule uses other_ip:other_port, so do we *)
  (* Update known_peers by other_port:other_ip *)
  let sender = new_peer { dummy_peer with peer_port = other_port ; peer_ip = other_ip } in
  new_peer_message sender;
  global_last_recv := last_time ();
  match t with

  | OvernetConnect p ->
      if is_overnet_ip sender.peer_ip p.peer_country_code && sender.peer_port <> 0 then
        let sender = new_peer { p with peer_ip = other_ip; peer_kind = 2 } in
        (* let sender = new_peer { p with peer_port = other_port ; peer_ip = other_ip } in *)
        new_peer_message sender;
        udp_send sender (OvernetConnectReply (get_any_peers 20))
       else
         begin
           if !verbose_overnet then
       lprintf_nl "Connect: invalid IP %s:%d received from %s:%d"
                  (Ip.to_string p.peer_ip) p.peer_port (Ip.to_string other_ip) other_port;
         end

  | OvernetConnectReply ps ->
      UdpSocket.declare_pong other_ip;
      let rec iter list =
        match list with
          [] -> ()
        | [p] ->
            (* last one is always the peer itself *)
            let sender = new_peer { p with peer_ip = other_ip; peer_kind = 2 } in
            new_peer_message sender
        | p :: tail ->
            let _ = new_peer p in
            iter tail
      in
      iter ps;

  | OvernetPublicize p ->
      let sender = new_peer { p with peer_ip = other_ip; peer_kind = 2 } in
      new_peer_message sender;
      if is_overnet_ip sender.peer_ip p.peer_country_code && sender.peer_port <> 0 then
        udp_send sender (OvernetPublicized (Some (my_peer ())))
       else begin
              if !verbose_overnet then
          lprintf_nl "Publicize: invalid IP %s:%d received from %s:%d"
                  (Ip.to_string p.peer_ip) p.peer_port (Ip.to_string other_ip) other_port;
            end

  | OvernetPublicized None ->
      ()

  | OvernetPublicized (Some p) ->
      ()

  | OvernetNoResult (md4) ->
      ()

  | OvernetSearch (nresults, md4, from_who) ->
      let nresults = min 10 nresults in
      let peers = get_closest_peers md4 nresults in
      udp_send sender (OvernetSearchReply (md4,peers))

  | OvernetSearchReply (md4, peers) ->

      let peers = List2.tail_map new_peer peers in
      List.iter (fun s ->
          if s.search_md4 = md4 then begin
              List.iter (add_search_peer s) peers;
              KnownPeers.add s.search_ok_peers sender;
              s.search_last_recv <- last_time ();
              (* Now ask the next best peer *)
              overnet_search_iter s 128 1;
            end
      ) !overnet_searches;

  | OvernetUnknown (opcode, s) ->
      if !verbose_unknown_messages then
        begin
          lprintf_nl "Unknown message from %s:%d " (Ip.to_string other_ip) other_port;
          lprintf_nl "\tCode: %d" opcode; dump s;
        end

  | OvernetSearchFilesResults (md4, results) ->
      List.iter (fun s ->
          if s.search_md4 = md4 then begin
              s.search_nresults <- s.search_nresults + 1;

              match s.search_kind with
                FileSearch file -> ()

              | KeywordSearch sss ->
                  List.iter (fun (r_md4, r_tags) ->
                      if not (Hashtbl.mem s.search_results r_md4) then
                        begin
                          incr search_hits;
                          s.search_hits <- s.search_hits + 1;
                          Hashtbl.add s.search_results r_md4 r_tags;
                          if !verbose_overnet then begin
                              lprintf_nl "FILE FOUND, TAGS:";
                              print_tags r_tags;
                              lprintf_nl ""
                        end;

                          DonkeyOneFile.search_found true sss r_md4 r_tags;
                        end
                  ) results
              | FillBuckets -> ()

            end
      ) !overnet_searches

  | OvernetSearchSourcesResults (md4, peers) ->
      List.iter (fun s ->
          if s.search_md4 = md4 then begin
              s.search_nresults <- s.search_nresults + 1;

              match s.search_kind with
                FileSearch file ->
                  List.iter (fun p ->
                      let ip = p.peer_ip in
                      let port = p.peer_tcpport in
                      if is_overnet_ip ip p.peer_country_code && port <> 0 then
                        let s = DonkeySources.create_source_by_uid
                            (Direct_address (ip, port)) None in
                        if !verbose_overnet then
                          lprintf_nl "added new source %s:%d for file %s"
                            (Ip.to_string ip) port (Md4.to_string md4);
                        incr source_hits;
                        DonkeySources.set_request_result s
                           file.file_sources File_new_source;
                        DonkeySources.set_source_brand s source_brand
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
              udp_send sender
                (OvernetSearchSourcesResults (md4, list))
        | Search_for_keyword _ ->
            let list = PublishedKeywords.get md4 in
            let _, list = List2.cut min list in
            let list, _ = List2.cut (max - min) list in
            if list <> [] then
              udp_send sender
                (OvernetSearchFilesResults (md4, list))
        | _ -> ()
      end

  | OvernetFirewallConnectionACK(md4) ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl "FIREWALL ACK for md4=%s" (Md4.to_string md4)   
    
   | OvernetFirewallConnectionNACK(md4) ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl "FIREWALL NACK for md4=%s" (Md4.to_string md4)   
    
 (* send the answer *)   
   | OvernetGetMyIP other_port ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl "GET MY IP (port=%d)" other_port;   
 (* FIXME : should be able to flush the UDP buffer*)   
       udp_send sender (OvernetGetMyIPResult other_ip);   
       udp_send sender OvernetGetMyIPDone   
    
   | OvernetGetMyIPResult(ip) ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl "GET MY IP RESULT (%s)" (Ip.to_string ip)   
    
   | OvernetGetMyIPDone ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl "GET MY IP DONE"   
  
  | OvernetPeerNotFound peer ->
      begin
        if !verbose_overnet || debug_client other_ip then
          lprintf_nl "Peer NOT FOUND %s (%s:%d) kind: %d (msg 33)"
            (Md4.to_string peer.peer_md4) (Ip.to_string peer.peer_ip)
        peer.peer_port peer.peer_kind;
(* We ignore OvernetPeerNotFound, the original client sends this message very often (even if the peer is alive)
        let dp = { dummy_peer with peer_port = peer.peer_port ; peer_ip = peer.peer_ip } in
        if KnownPeers.mem known_peers dp
        then begin
(* remove it from the buckets, put it into prebuckets and set kind to 3 *)
          try
            for i = 0 to !n_used_buckets do
              let b = buckets.(i) in
              let pb = prebuckets.(i) in
              for j = 1 to Fifo.length b do
                let p = Fifo.take b in
                if p.peer_ip = peer.peer_ip && 
                    p.peer_port = peer.peer_port then begin
                  decr connected_peers;
                  if !pre_connected_peers < max_peers_per_prebucket then begin
                    incr pre_connected_peers;
                    Fifo.put pb p;
                    p.peer_kind <- 3;
                  end 
                  else begin if !verbose_overnet then lprintf_nl "peernotfound: removing %s:%d" (Ip.to_string p.peer_ip) p.peer_port;
                  KnownPeers.remove known_peers p;
                  end
                end else Fifo.put b p
              done;
            done;
          with Exit -> ();
        end;
*)
      end

   | OvernetUnknown21 peer ->
      if !verbose_overnet && debug_client other_ip then begin
          lprintf_nl "Unknown 21 message ...";
          lprintf_nl "From peer: %s ip: %s:%d kind: %d"
            (Md4.to_string peer.peer_md4) (Ip.to_string peer.peer_ip) peer.peer_port peer.peer_kind
        end

  | _ -> failwith "Message not understood"

(* Start unstarted searches, retry stalled searches *)
let overnet_search_tick () =
  List.iter (fun s ->
(* Start a search, if search_start is reached *)
         if (s.search_lifetime + s.search_start - 20) > last_time () &&
            s.search_start < last_time () then begin
         (if s.search_queries = 0 then
            overnet_search_iter s 128 3
         else if (s.search_last_recv + 3) < last_time () then
(* The search has stalled. We do what eMule call a JumpStart *)
(* FIXME: with implemented firewalling this should be 1 peer not 2*)
            overnet_search_iter s 128 2);
      end
  ) !overnet_searches

let recover_file file =
  add_current_file file

let update_buckets () =

(* 1. Clean the buckets from too old peers ( by kind and expire ) *)

  for i = 0 to !n_used_buckets do

    let b = buckets.(i) in
    for j = 1 to Fifo.length b do
      let p = Fifo.take b in
      (* bad peers have kind = 4 and did not respond within peer_expire *)
      (* Why check is_overnet_ip? Because it may have been blocked since it was added *)
      if not (p.peer_kind = 4 && p.peer_expire <= last_time ()) &&
             is_overnet_ip p.peer_ip p.peer_country_code then
        Fifo.put b p 
      else
      begin
        decr connected_peers;
        KnownPeers.remove known_peers p;
        if !verbose_overnet then lprintf_nl "update_bucket1: removing %s:%d" (Ip.to_string p.peer_ip) p.peer_port;
      end;
    done

  done;

(* 2. Complete buckets with new peers from the prebuckets with
( p.peer_kind < 3 ) *)

  for i = 0 to !n_used_buckets do
    let b = buckets.(i) in
    if Fifo.length b < max_peers_per_bucket then begin

          let pb = prebuckets.(i) in
          for j = 1 to Fifo.length pb do

            (* to keep fifo in order travel all peers *)
            let p = Fifo.take pb in
            (* good peers goto buckets *)
            if p.peer_kind < 3 && Fifo.length b < max_peers_per_bucket then begin
                Fifo.put b p;
                incr connected_peers;
                decr pre_connected_peers;
            (* bad peers are removed *)    
            (* Why check is_overnet_ip? Because it may have been blocked since it was added *)
            end else if (p.peer_kind = 4 && p.peer_expire <= last_time ()) || 
                        not (is_overnet_ip p.peer_ip p.peer_country_code) then begin
              decr pre_connected_peers;
              KnownPeers.remove known_peers p;
              if !verbose_overnet then lprintf_nl "update_bucket2: removing %s:%d" (Ip.to_string p.peer_ip) p.peer_port;
            end else
            (* the rest returns in prebuckets *)
            Fifo.put pb p
          done
      end
  done;

(* boot_peers will be saved, so we choose max_boot_peers good peers
from the active buckets and prebuckets *)
  LimitedList.clear !!boot_peers;
  let n = ref 0 in
  try
    let addtol p =
      if p.peer_kind < 3 then begin
        LimitedList.add !!boot_peers (p.peer_ip, p.peer_port);
        incr n;
        if !n = max_boot_peers then raise Exit
      end
    in

    for i = 0 to !n_used_buckets do
      Fifo.iter addtol buckets.(i)
    done;

    for i = 0 to !n_used_buckets do
      Fifo.iter addtol prebuckets.(i)
    done;
  with Exit -> ();

  ()

let compute_to_ping () =
(* compute which peers to ping in the next minute *)
          to_ping := [];
          let n_to_ping = ref 0 in

          let ping_peers b =
            Fifo.iter (fun p ->
                if (p.peer_expire <= last_time () && p.peer_kind < 4) ||
                    p.peer_last_send = 0 then begin
                    to_ping := p :: !to_ping;
                    incr n_to_ping;
                    if !n_to_ping = 60 then raise Exit
                  end
            ) b
          in
          begin
            try
              for i = !n_used_buckets downto 8 do
                ping_peers buckets.(i);
                ping_peers prebuckets.(i);
              done;
              for i = min !n_used_buckets 7 downto 0 do
                ping_peers buckets.(i);
              done;
              for i = min !n_used_buckets 7 downto 0 do
                ping_peers prebuckets.(i);
              done;
            with Exit -> ()
          end;
          ()

(* start max three searches for sources *)
let start_max_source_searches () =
  let current_files2 = ref [] in
  while (List.length !overnet_searches) < 3 && !current_files <> [] do
    match !current_files with
    | [] -> assert false
    | (file, start, last) :: tl ->
      current_files := tl;
      if file_state file = FileDownloading && (last + 900) < last_time () then
      begin
        ignore (create_search (FileSearch file) file.file_md4);
        current_files2 := (file, start, last_time ()) :: !current_files2;
      end else
        current_files2 := (file, start, last) :: !current_files2;
  done;
  List.iter (fun (file, start, last) -> current_files := (file, start, last) :: !current_files) !current_files2

let enable () =
  if !!enable_overnet && not !is_enabled then begin
      let enabler = is_enabled in
      is_enabled := true;

      let sock = (UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
          (!!overnet_port) (Proto.udp_handler udp_client_handler)) in
      udp_sock := Some sock;
      if Proto.redirector_section = "DKKO" then
        overnet_port_info := !!overnet_port;
      if Proto.redirector_section = "DKKA" then
        kademlia_port_info := !!overnet_port;
      UdpSocket.set_write_controler sock udp_write_controler;

(* copy all boot_peers to unknown_peers *)
      LimitedList.iter (fun (ip, port) ->
          if ip <> Ip.localhost && is_overnet_ip ip None && port <> 0 then
            LimitedList.add unknown_peers (ip, port)
          ) !!boot_peers;

      add_session_timer enabler 1. (fun _ ->
          if !!enable_overnet then begin
            overnet_search_tick ();

            let my_peer = my_peer () in

(* ping old peers regularly *)
            let process_to_ping () =
              begin
                match !to_ping with
                  [] -> compute_to_ping ();
                | p :: tail ->
                  (* do not hammer a peer, we could have send already a search reqeust 
                     since to_ping is rebuild at least every 60 seconds *)
                  if (last_time () - p.peer_last_send) > 61 then begin
                    checking_kind p;
                    udp_send p (OvernetPublicize my_peer);
                  end;
                  to_ping := tail;
              end;
            in
            process_to_ping ();
(* When we start with 50 searches and no peers, we have to update the buckets
   fast, otherwise we cannot add search peers (rest: sanity check) *)
           begin
             if !connected_peers < 20 && !pre_connected_peers <> 0 &&
               LimitedList.length unknown_peers <> 0 then
               update_buckets ();
               compute_to_ping ();
           end;
(* Send OvernetConnects and ping more peers *)
(* TODO: How does eMule it? are 50 ok? *)
            begin
              try
                 if !connected_peers < 10 then
                   for i = 1 to 2 do
                     process_to_ping ();
                   done;
                 (* Do not send too much OvernetConnect, there is no use *)
                 if (!connected_peers + !pre_connected_peers) < 40 then
                    for i = 1 to 3 do
                      let (ip, port) = LimitedList.get unknown_peers in
                      udp_send_ping ip port (OvernetConnect my_peer);
                    done
              with _ -> ()
            end;
          end
      );


      LimitedList.set_max_objects !!boot_peers  2000 ;
      add_timer 60. (fun _ ->
          LimitedList.set_max_objects !!boot_peers  2000;
      );

      add_session_timer enabler 60. (fun _ ->

          update_buckets ();

          compute_to_ping ();

(* dump searches older than lifetime to the logfile *)
          if !verbose_overnet then List.iter (fun s ->
            if s.search_lifetime + s.search_start <= last_time () then begin
              lprintf_nl "Removing search %s for %s (requests:%d queries:%d seconds alive:%d lifetime:%d)"
                (match s.search_kind with
                  KeywordSearch _ -> "keyword"
                  | FileSearch _ -> "file"
                  | FillBuckets -> "fillbuckets" )
                (Md4.to_string s.search_md4) s.search_requests s.search_queries (last_time ()-s.search_start) s.search_lifetime;
                for i = 128 downto 0 do
                  let npeers = Fifo.length s.search_peers.(i) in
                  let count = ref 0 in
                  let cp p = if (common_bits p.peer_md4 s.search_md4) = i then count := !count + 1 in
                  KnownPeers.iter cp s.search_asked_peers;
                  let nasked = !count in
                  count := 0;
                  KnownPeers.iter cp s.search_ok_peers;
                  let nok = !count in
                  count := 0;
                  KnownPeers.iter cp s.search_result_asked_peers;
                  let nres = !count in
                  if npeers > 0 || nasked > 0 then
                    lprintf_nl
                    " nbits[%d] = %d peer(s) total, %d peer(s) asked, %d peer(s) ok, %d peer(s) result asked"
                      i npeers nasked nok nres
                done;
            end;
          ) !overnet_searches;

(* remove searches that are older than their lifetime *)
          overnet_searches := List.filter (fun s ->
              s.search_lifetime + s.search_start > last_time ()
          ) !overnet_searches;

          start_max_source_searches ();

          (* FIXE: Dump latencies to logfile *)
          if !verbose_overnet then ignore (UdpSocket.get_latencies (ref true));

          if !verbose_overnet then begin
            lprintf_nl "%d peers (prebucket: %d peers)" !connected_peers !pre_connected_peers;
            for i = 0 to !n_used_buckets do
              if (Fifo.length buckets.(i)) <> 0 || (Fifo.length prebuckets.(i)) <> 0 then
              lprintf_nl "bucket[%d] : %d peers (prebucket %d)"
                i (Fifo.length buckets.(i)) (Fifo.length prebuckets.(i));
            done;
            lprintf_nl "unknown_peers: %d" (LimitedList.length unknown_peers);
            lprintf_nl "boot_peers: %d" (LimitedList.length !!boot_peers);
          end;
      );

(* every 15min for light operations *)
      add_session_timer enabler 900. (fun _ ->
          if !!enable_overnet then begin
              ignore(create_search FillBuckets !!overnet_md4);
              (* Remove all files not actuall downloading/pause/new etc *)
                (* TODO: Are there states enough? There are:  FileDownloading
                | FileQueued
                | FilePaused
                | FileDownloaded
                | FileShared
                | FileCancelled
                | FileNew
                | FileAborted of string
                *)
              current_files := List.filter (fun (f,_,_) ->
                match file_state f with
                    FileDownloading
                  | FileQueued
                  | FilePaused
                  | FileNew -> true;
                  | _ -> false;
              ) !current_files;
            end
      );

      begin 
        (* First fill the buckets *)
        ignore (create_search FillBuckets !!overnet_md4);
        (* start with all actual files *)
        List.iter add_current_file !DonkeyGlobals.current_files
      end;

      start_max_source_searches ();
      
      add_infinite_timer 1800. (fun _ ->
          if !!enable_overnet then begin
              PublishedKeywords.refresh ();
              PublishedFiles.refresh ();
            end            
      );

  end

let disable () =
  if !is_enabled then
    begin
      is_enabled := false;
      (match !udp_sock with
          None -> ()
        | Some sock ->
            udp_sock := None;
            UdpSocket.close sock Closed_by_user);
      if Proto.redirector_section = "DKKO" then
        overnet_port_info := 0;
      if Proto.redirector_section = "DKKA" then
        kademlia_port_info := 0;
    end

let _ =
  option_hook enable_overnet
    (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_overnet = false then disable() else enable ()
  )

let load_contact_dat filename =
  try
    let module S = DonkeyOvernetImport.Peer in
    let s = File.to_string filename in
    let ss = S.read s in
    List.iter (fun r ->
        try
          let ip = r.S.ip in
          let port = r.S.port in
          bootstrap ip port
        with _ -> ()
    ) ss;
    List.length ss
  with e ->
      lprintf_nl "Exception %s while loading %s" (Printexc2.to_string e)
      filename;
      0

let parse_overnet_url url =
  match String2.split (String.escaped url) '|' with
  | "fha://" :: "boot" :: name :: port :: _
  | "boot" :: name :: port :: _ ->
      let ip = Ip.from_name name in
      let port = int_of_string port in
      bootstrap ip port;
      true
  | _ -> false

let command_prefix_to_net =
  if command_prefix = "ov_" || Proto.redirector_section = "DKKO" then "Overnet"
  else "Kademlia"

let register_commands list =
  register_commands
    (List2.tail_map (fun (n,f,h) -> (n, "Network/Overnet", f,h)) list)

let _ =
  register_commands
    (List.map (fun (command, args, help) ->
        command_prefix ^ command, args, help)
    [
    "dump_searches", Arg_none (fun o ->
         let buf = o.conn_buf in
         List.iter ( fun s ->
           Printf.bprintf buf "Search %s for %s\nrequests:%d queries:%d seconds alive:%d lifetime:%d\n"
              (match s.search_kind with
                KeywordSearch _ -> "keyword"
                | FileSearch _ -> "file"
                | FillBuckets -> "fillbuckets" )
              (Md4.to_string s.search_md4) s.search_requests s.search_queries (last_time ()-s.search_start) s.search_lifetime;
           let pp p = print_peer buf p in
           Printf.bprintf buf "search_peers\n";
           Array.iter (fun a -> Fifo.iter (fun p -> pp p) a) s.search_peers;
           Printf.bprintf buf "search_asked_peers\n";
           KnownPeers.iter pp s.search_asked_peers;
           Printf.bprintf buf "search_ok_peers\n";
           KnownPeers.iter pp s.search_ok_peers;
           Printf.bprintf buf "search_result_asked_peers\n";
           KnownPeers.iter pp s.search_result_asked_peers;
           Printf.bprintf buf "\n";
         ) !overnet_searches;
         ""
    ), ("<bucket_nr> :\t\tdumps a search (Devel)");
    "dump_bucket", Arg_one (fun i o ->
         let i = int_of_string i in
         let i = min i !n_used_buckets in
         let buf = o.conn_buf in
         update_buckets ();

         Printf.bprintf buf "Peers of bucket number %d\n" i;
         let b = buckets.(i) in
         for i = 1 to Fifo.length b do
           let p = Fifo.take b in
           print_peer buf p;
           Fifo.put b p;
         done;

         Printf.bprintf buf "Peers of prebucket number %d\n" i;
         let pb = prebuckets.(i) in
         for i = 1 to Fifo.length pb do
           let p = Fifo.take pb in
           print_peer buf p;
           Fifo.put pb p;
         done;
         ""
    ), ("<bucket_nr> :\t\tdumps a bucket (Devel)");

    "dump_known_peers", Arg_none (fun o ->
         let buf = o.conn_buf in

         Printf.bprintf buf "Peers of known_peers:\n";
         KnownPeers.iter (fun p ->
           Printf.bprintf buf "%s:%d:" (Ip.to_string p.peer_ip) p.peer_port;
           print_peer buf p;
         ) known_peers;
         ""
    ), (":\t\t\tdumps known_peers (Devel)");

    "boot", Arg_two (fun ip port o ->
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        bootstrap ip port;
        Printf.sprintf "peer %s:%d added" (Ip.to_string ip) port
    ), ("<ip> <port> :\t\t\tadd an " ^ command_prefix_to_net ^ " peer");

    "link", Arg_multiple (fun args o ->
        let url = String2.unsplit args ' ' in
        if parse_overnet_url url then
          "download started"
        else "bad syntax"
    ), "<fhalink> :\t\t\tdownload fha:// link";

    "view_stats_cmds", Arg_none (fun o ->
        let buf = o.conn_buf in
        html_mods_commands buf "commandsTable" "commands" ([
          ("bu bbig", command_prefix_to_net ^ " boots list",
           "mSub('output','" ^ command_prefix ^ "boots')",
           command_prefix_to_net ^ " boots");
          ("bu bbig", command_prefix_to_net ^ " buckets list",
           "mSub('output','" ^ command_prefix ^ "buckets')",
           command_prefix_to_net ^ " buckets");
          ("bu bbig", command_prefix_to_net ^ " stats",
           "mSub('output','" ^ command_prefix ^ "stats')",
           command_prefix_to_net ^ " stats");
          ("bu bbig", command_prefix_to_net ^ " store list",
           "mSub('output','" ^ command_prefix ^ "store')",
           command_prefix_to_net ^ " store");
          ]);
        "";
    ), (":\t\t\t" ^ command_prefix_to_net ^ " stats commands");

    "stats", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          begin
            Printf.bprintf buf "\\<div class=results\\>";
            html_mods_table_header buf "ovstatsTable" "sources" [];
            Printf.bprintf buf "\\<tr\\>";
            html_mods_td buf [
              ("", "srh", Printf.sprintf "%s %s\n" (command_prefix_to_net)
                  (if !!enable_overnet then "is enabled" else "is DISABLED"));
              ("", "srh", Printf.sprintf "%s Connectivity: %s%s" (command_prefix_to_net)
                   (if (!connected_peers + !pre_connected_peers) > 20 then "Got enough online peers" else "NOT enough online peers")
                   (if (abs(!global_last_recv - !global_last_send)) > 60 then " ,there maybe a problem with incoming udp packets" else "" ));

            ];
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\n";

            Printf.bprintf buf "\\<div class=results\\>";
            html_mods_table_header buf "ovstatsTable" "sources" [];
            Printf.bprintf buf "\\<tr\\>";
            html_mods_td buf [
              ("", "srh", Printf.sprintf "%s statistics" command_prefix_to_net);
              ("", "srh", Printf.sprintf "Search hits: %d\n" !search_hits);
              ("", "srh", Printf.sprintf "Source hits: %d\n" !source_hits); 
              ("", "srh", Printf.sprintf "Current files: %d\n" (List.length !current_files)); ];
            if !verbose_overnet then begin
              Printf.bprintf buf "\\</tr\\>\\<tr\\>\n";
              html_mods_td buf [
                ("", "srh", Printf.sprintf "Verbose:");
                ("", "srh", Printf.sprintf "last upd send: %d\n" !global_last_send);
                ("", "srh", Printf.sprintf "last upd recv: %d\n" !global_last_recv);
                ("", "srh", Printf.sprintf "diff: %d\n" (abs(!global_last_recv - !global_last_send)));
              ];
            end;
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\n";
          end
        else
          begin
            Printf.bprintf buf "%s %s\n" (command_prefix_to_net) 
               (if !!enable_overnet then "is enabled" else "is DISABLED");
            Printf.bprintf buf "%s Connectivity: %s%s\n" (command_prefix_to_net)
                   (if (!connected_peers + !pre_connected_peers) > 20 then "Got enough online peers" else "NOT enough online peers")
                   (if (abs(!global_last_recv - !global_last_send)) > 60 then " ,there maybe a problem with incoming udp packets" else "");
            Printf.bprintf buf "%s statistics:\n"
            (command_prefix_to_net);
            Printf.bprintf buf "  Search hits: %d\n" !search_hits;
            Printf.bprintf buf "  Source hits: %d\n" !source_hits;
            Printf.bprintf buf "  Current files: %d\n" (List.length !current_files);
          end;
(* Only for debuging current_file
          List.iter (fun (file,start,last) ->
          Printf.bprintf buf "current_file %s (%d,%d)\n" (file_disk_name file) start last
        ) !current_files;
*)
        List.iter (fun s ->
            if o.conn_output = HTML then
              begin
                html_mods_table_header buf "ovstatsTable" "sources" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [
                  ("", "srh", Printf.sprintf "Search %s for %s\n"
                    (match s.search_kind with
                      KeywordSearch _ -> "keyword"
                      | FileSearch _ -> "file"
                      | FillBuckets -> "fillbuckets" )
                    (Md4.to_string s.search_md4)
                  ); ];
                Printf.bprintf buf "\\</tr\\>";
                Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
                html_mods_td buf [
                  ("", "sr", Printf.sprintf "requests:%d queries:%d seconds alive:%d lifetime:%d\n" 
                     s.search_requests s.search_queries (last_time ()-s.search_start) s.search_lifetime); ];
                Printf.bprintf buf "\\</tr\\>";
              end
            else
            Printf.bprintf buf "Search %s for %s\nrequests:%d queries:%d seconds alive:%d lifetime:%d\n"
              (match s.search_kind with
                KeywordSearch _ -> "keyword"
                | FileSearch _ -> "file"
                | FillBuckets -> "fillbuckets" )
              (Md4.to_string s.search_md4) s.search_requests s.search_queries (last_time ()-s.search_start) s.search_lifetime;
              for i = 128 downto 0 do
                let npeers = Fifo.length s.search_peers.(i) in
                let count = ref 0 in
                let cp p = if (common_bits p.peer_md4 s.search_md4) = i then count := !count + 1 in
                KnownPeers.iter cp s.search_asked_peers;
                let nasked = !count in
                count := 0;
                KnownPeers.iter cp s.search_ok_peers;
                let nok = !count in
                count := 0;
                KnownPeers.iter cp s.search_result_asked_peers;
                let nres = !count in
                if npeers > 0 || nasked > 0 then
                  if o.conn_output = HTML then
                    begin
                      Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
                      html_mods_td buf [
                        ("", "sr",
                          Printf.sprintf "nbits[%d] = %d peer(s) total, %d peer(s) asked, %d peer(s) ok, %d peer(s) result asked"
                          i npeers nasked nok nres); ];
                      Printf.bprintf buf "\\</tr\\>";

                    end
                  else
                  Printf.bprintf buf
                  "   nbits[%d] = %d peer(s) total, %d peer(s) asked, %d peer(s) ok, %d peer(s) result asked\n"
                    i npeers nasked nok nres
              done;
              if o.conn_output = HTML then
                Printf.bprintf buf "\\</table\\>\\</div\\>\n";
        ) !overnet_searches;
        if o.conn_output = HTML then
          Printf.bprintf buf "\\</div\\>\n";

        "";
    ), ":\t\t\t\t" ^ command_prefix_to_net ^ " stats";

    "web", Arg_multiple (fun args o ->
        let urls =
          match args with
            [] -> let list = ref [] in
              Hashtbl.iter (fun key w ->
                  if w.kind = web_info then list := w :: !list
              ) web_infos_table;
              !list
          | _ -> List.map (fun url -> 
                    { 
                      url = url;
                      kind = web_info;
                      period = 0;
                      state = None;
                    }) args
        in
        List.iter (fun w ->
            Printf.bprintf o.conn_buf "Loading %s\n" w.url;
            CommonWeb.load_url true w) urls;
        "web boot started"
    ), "<urls> : \t\t\tdownload .ocl URLs (no arg load default)";

    "load", Arg_one (fun filename o ->
        try
          let n = load_contact_dat filename in
          Printf.sprintf "%d overnet peers loaded" n;
        with e ->
            Printf.sprintf "error %s while loading file" (Printexc2.to_string e)
    ), "<filename> :\t\t\tload the peers from a contact.dat file";

    "md4", Arg_none (fun o -> "MD4 is " ^ (Md4.to_string !!overnet_md4);
    ), ":\t\t\t\tget client MD4 address on the Overnet/Kademlia network";

    "store", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          begin
            let buftmp = Buffer.create 100 in
            PublishedKeywords.print buftmp;
            let listtmp = String2.split (Buffer.contents buftmp) '\n' in
            let keywords = ref [] in
            List.iter (fun s ->
                keywords := !keywords @ [("", "dl-1", s);]
            ) listtmp;
            Buffer.reset buftmp;
            PublishedFiles.print buftmp;
            let listtmp = String2.split (Buffer.contents buftmp) '\n' in
            let files = ref [] in
            List.iter (fun s ->
                files := !files @ [("", "dl-1", s);]
              ) listtmp;
            Buffer.reset buftmp;
            html_mods_table_one_col buf "ovstoreTable" "results" ([
              ("", "srh", Printf.sprintf "%s store" command_prefix_to_net);
              ] @ !keywords @ !files);
          end
        else
          begin
          Printf.bprintf buf "%s store:\n"
          (command_prefix_to_net);
          PublishedKeywords.print buf;
          PublishedFiles.print buf;
        end;
        ""
    ), ":\t\t\t\tdump the " ^ command_prefix_to_net ^ " File Store";

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
          lprintf_nl "Sending UDP message %d to %s:%s" opcode ip port;
          dump msg; lprintf_nl ""; "Sending UDP message"
        with _ ->
            lprintf_nl "Unable to send UDP message"; "Unable to send UDP message"
    ), ":\t\t\t\tsend UDP message (<ip> <port> <msg in hex>)";
    
    "buckets", Arg_none (fun o ->
        let buf = o.conn_buf in
        update_buckets ();
        if o.conn_output = HTML then
          begin
            let b = ref [] in
            for i = 0 to !n_used_buckets do
              if (Fifo.length buckets.(i)) <> 0 || (Fifo.length prebuckets.(i)) <> 0 then
                b := !b @ [("", "dl-1", (Printf.sprintf "\\<a href=\\\"submit\\?q=%sdump_bucket\\+%d\\\" target=output\\>[Dump]\\</a\\>bucket[%d] : %d peers (prebucket %d)" 
                command_prefix i i (Fifo.length buckets.(i)) (Fifo.length prebuckets.(i)) ) );];
            done;
            b := !b @ [("", "dl-1", (Printf.sprintf "\\<a href=\\\"submit\\?q=%sbuckets\\\" target=output\\>[Refresh]\\</a\\>" command_prefix) );];
            html_mods_table_one_col buf "ovbucketsTable" "results" ([
              ("", "srh",
                Printf.sprintf "%d peers (prebucket: %d peers)"
                  !connected_peers !pre_connected_peers);
              ] @ !b);
        end else begin
          Printf.bprintf buf "%d peers (prebucket: %d peers)\n"
            !connected_peers !pre_connected_peers;
          for i = 0 to !n_used_buckets do
            if (Fifo.length buckets.(i)) <> 0 || (Fifo.length prebuckets.(i)) <> 0 then
              Printf.bprintf buf "   bucket[%d] : %d peers (prebucket %d)\n"
                i (Fifo.length buckets.(i)) (Fifo.length prebuckets.(i));
          done;
        end;
          ""
    ), ":\t\t\t\tprint buckets table status";

    "boots", Arg_none (fun o ->
        let buf = o.conn_buf in
        LimitedList.iter (fun (ip, port) ->
            Printf.bprintf buf "   %s:%d\n" (Ip.to_string ip) port;
        ) !!boot_peers;
        if o.conn_output = HTML then
          begin
            let listtmp = String2.split (Buffer.contents buf) '\n' in
            let boots = ref [] in
            List.iter (fun s ->
                boots := !boots @ [("", "dl-1", s);]
            ) listtmp;
            Buffer.reset buf;
            html_mods_table_one_col buf "ovbucketsTable" "results" ([
              ("", "srh",
                Printf.sprintf "Boot peers: %d\n" (LimitedList.length !!boot_peers));
              ] @ !boots);
          end
        else
          Printf.bprintf buf "Boot peers: %d\n" (LimitedList.length !!boot_peers);
        ""
    ), ":\t\t\t\tprint boot peers";

  ]);
  ()

let overnet_search (ss : search) =
  if !!overnet_search_keyword && !!enable_overnet then
    let q = ss.search_query in
    if !verbose_overnet then lprintf_nl "========= %s_search =========" command_prefix_to_net;
    let ws = keywords_of_query q in
    List.iter (fun w ->
        if !verbose_overnet then lprintf_nl "%s_search for %s" command_prefix_to_net w;
        let s = create_keyword_search w ss in
        Hashtbl.iter (fun r_md4 r_tags ->
            DonkeyOneFile.search_found true ss r_md4 r_tags) s.search_results;
    )
    ws

let forget_search ss =
  begin
(* Remove from overnet_searches *)    
  overnet_searches := List.filter (fun s ->
      match s.search_kind with
        KeywordSearch sss when ss == sss -> false
      | _ -> true) !overnet_searches
  end    

let cancel_recover_file file =
   begin
(* Remove from overnet_searches *)
   overnet_searches := List.filter (fun s ->
      match s.search_kind with
        FileSearch f when f == file -> false
      | _ -> true) !overnet_searches;
   remove_current_file file;
   end  

let _ =
  CommonWeb.add_web_kind web_info web_info_descr (fun url filename ->
    try
      DonkeyNodesDat.parse filename bootstrap
    with
      exn ->
      lprintf_nl "Parsing nodes.dat as binary failed : %s : will try as text" (Printexc2.to_string exn);
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
                    if !verbose_overnet then
                      lprintf_nl "Adding %s peer %s:%d" command_prefix_to_net name port;
                    bootstrap ip port) (fun _ -> ())
            | _ -> lprintf_nl "BAD LINE ocl: %s" s;
          with _ -> lprintf_nl "DNS failed";
      ) lines
  );

  (* Add this kind of web_info only for overnet *)
  if Proto.redirector_section = "DKKO" then
    CommonWeb.add_web_kind "contact.dat" "List of Overnet boot peers" 
      (fun url filename ->
        if !!enable_overnet && !!overnet_update_nodes then
          let n = load_contact_dat filename in
            lprintf_nl "contact.dat loaded from %s, added %d peers" url n;
        else
          if not !!enable_overnet then
            lprintf_nl "Overnet module is disabled, ignoring..."
          else
            lprintf_nl "Overnet_update_nodes is disabled, ignoring..."
      );

(*************************************************************

Define a function to be called when the "mem_stats" command
  is used to display information on structure footprint.

**************************************************************)

  Heap.add_memstat
    (if Proto.redirector_section = "DKKO" then "Overnet" else "Kademlia")
    (fun level buf ->
      Printf.bprintf buf "%s statistics:\n"
      (if Proto.redirector_section = "DKKO" then "Overnet" else "Kademlia");
      Printf.bprintf buf "  unknown_peers: %d\n" (LimitedList.length unknown_peers);
      update_buckets ();
      Printf.bprintf buf "  boot_peers: %d\n" (LimitedList.length !!boot_peers);
      Printf.bprintf buf "  %d peers and %d prebucket peers\n"
            !connected_peers !pre_connected_peers;

      Printf.bprintf buf "  Search hits: %d\n" !search_hits;
      Printf.bprintf buf "  Source hits: %d\n" !source_hits;
      Printf.bprintf buf "  known_peers stats: %d %d %d %d %d %d\n"
        ((fun (n,_,_,_,_,_) -> n)(KnownPeers.stats known_peers))
        ((fun (_,n,_,_,_,_) -> n)(KnownPeers.stats known_peers))
        ((fun (_,_,n,_,_,_) -> n)(KnownPeers.stats known_peers))
        ((fun (_,_,_,n,_,_) -> n)(KnownPeers.stats known_peers))
        ((fun (_,_,_,_,n,_) -> n)(KnownPeers.stats known_peers))
        ((fun (_,_,_,_,_,n) -> n)(KnownPeers.stats known_peers))
      ;

      
      Printf.bprintf buf "  to_ping: %d\n" (List.length !to_ping);
      let n_search_peers = ref 0 in
      let n_search_results = ref 0 in
      let n_overnet_searches = ref 0 in
      List.iter ( fun s ->
              for i = 128 downto 0 do
               n_search_peers :=
                 !n_search_peers + (Fifo.length s.search_peers.(i));
               n_search_results :=
                 !n_search_results + (Hashtbl.length s.search_results);
              done;
             incr n_overnet_searches
      ) !overnet_searches;
      Printf.bprintf buf "  n_search_peers: %d\n" !n_search_peers;
      Printf.bprintf buf "  n_search_results: %d\n" !n_search_results;
      Printf.bprintf buf "  n_overnet_searches: %d\n" !n_overnet_searches;
  );
end
