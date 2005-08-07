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
    | OvernetPeerNotFound p ->
        Printf.bprintf buf "OvernetPeerNotFound\n";
        print_peer buf p
    | OvernetUnknown21 p ->
        Printf.bprintf buf "OvernetUnknown21\n";
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

type search_for =
| FileSearch of file
| KeywordSearch of CommonTypes.search
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

      val redirector_section : string
      val options_section_name : string
      val command_prefix : string
      val source_brand : bool

      val udp_send : UdpSocket.t -> Ip.t -> int -> bool -> t -> unit
      val udp_handler : (t -> UdpSocket.udp_packet -> unit) ->
        UdpSocket.t -> UdpSocket.event -> unit

      val web_info : string
    end) = struct

    open Proto

    let lprintf_nl () =
      lprintf "%s[%s] "
      (log_time ())
      (if Proto.redirector_section = "DKKO" then "Overnet" else "Kademlia"); lprintf_nl2

    let lprintf () =
      lprintf "[%s] "
      (if Proto.redirector_section = "DKKO" then "Overnet" else "Kademlia"); lprintf

(********************************************************************

                     STATIC MEMORY

*********************************************************************)

let max_peers_per_bucket = 20
let max_peers_per_prebucket = 100
let min_peers_per_block = 16 (* was 2 *)
let min_peers_before_connect = 5
let max_searches_for_publish = 5
let max_search_queries = 64
let max_search_requests = 20

let is_enabled = ref false


(********************************************************************

                           OPTIONS

*********************************************************************)

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
    "Search Timeout", shortname overnet_search_timeout, "T";
    "Search Internal Period", shortname overnet_query_peer_period, "T";
    "Search Max Hits", shortname overnet_max_search_hits, "T";
  ]

let overnet_options_version =
  define_option overnet_section [Proto.options_section_name; "options_version"]
    "(internal)"
    int_option 0


(********************************************************************


                        MUTABLE STRUCTURES


*********************************************************************)

let connected_peers = ref 0
let pre_connected_peers = ref 0

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
          if Fifo.length t.objects_fifo > t.max_objects then
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
bucket.

What we want: we don't want to put too many peers in the buckets.
The buckets should preferably contain peers that have already send us a
  message, because we are not sure for other peers.
  *)

let n_used_buckets = ref 0

(* We distinguish between buckets and prebuckets: peers in buckets are peers
that sent us a message in the last hour, whereas peers in the prebuckets
are peers that we are not sure of. *)
let buckets = Array.init 129 (fun _ -> Fifo.create ())
let prebuckets = Array.init 129 (fun _ -> Fifo.create ())

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
      Proto.udp_send sock ip port false msg

let udp_send_ping ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
      Proto.udp_send sock ip port true msg

let udp_send p msg =
  p.peer_last_send <- last_time ();
  udp_send_direct p.peer_ip p.peer_port msg

let bootstrap ip port =
  if !!overnet_update_nodes && Ip.valid ip && Ip.reachable ip && port <> 0 then begin
      LimitedList.add !!boot_peers (ip,port);
      boot_peers_copy := (ip,port) :: !boot_peers_copy;
(* Limit boot_peers_copy, if needed by the timer it will be set to
   LimitedList.to_list !!boot_peers there *)
      if (List.length !boot_peers_copy) > (2 * LimitedList.length !!boot_peers)
      then
        boot_peers_copy := (ip,port) :: [];
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

        let bucket = bucket_number p.peer_md4 in
(* First, enter the peer in the boot_peers to be able to use at next
restart. *)
        if bucket <> 128 then
            bootstrap p.peer_ip p.peer_port;

(* bucket = 128 is returned for our very own ID, we dont want it in our buckets 
   Then, enter it in the buckets *)
        if bucket < !n_used_buckets && bucket <> 128 then begin

            if Fifo.length prebuckets.(bucket) = max_peers_per_prebucket then
	    begin
              let l = last_time () - 1800 in
              let pp = Fifo.take prebuckets.(bucket) in
              Fifo.put prebuckets.(bucket)
(* If we heard of the head of the bucket in the last 30 minutes, we should
  keep it. *)
                (if pp.peer_last_recv > l then pp else p);
              if pp.peer_last_recv <= l then
              begin
(* Add the peer to the table of known peers *)
                Hashtbl.add known_peers key p;
(* And remove pp *)
                let ppkey = (pp.peer_ip, pp.peer_port) in
                Hashtbl.remove known_peers ppkey
              end
	    end  
            else begin
               Hashtbl.add known_peers key p;
               Fifo.put prebuckets.(bucket) p;
               incr pre_connected_peers
            end
        end
        else if !n_used_buckets < 128 && bucket <> 128 then begin
            Fifo.put prebuckets.(!n_used_buckets) p;

            while !n_used_buckets < 128 &&
              Fifo.length prebuckets.(!n_used_buckets)
              = max_peers_per_bucket do
              let b = prebuckets.(!n_used_buckets) in
              incr n_used_buckets;
              for i = 1 to Fifo.length b do
                let p = Fifo.take b in
                let bucket = bucket_number p.peer_md4 in
                Fifo.put (if bucket >= !n_used_buckets then
                    prebuckets.(!n_used_buckets) else b) p
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
        if p.peer_last_recv <> 0 then begin
            if !verbose_overnet then begin
            lprintf_nl () "Adding good search peer %s:%d"
              (Ip.to_string p.peer_ip) p.peer_port;
	    end;
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

let add_search_peer s p =
  if p.peer_ip <> client_ip None then
  let key = (p.peer_ip, p.peer_port) in
  if not (Hashtbl.mem s.search_known_peers key) then begin
      Hashtbl.add s.search_known_peers key p;
      let nbits = common_bits p.peer_md4 s.search_md4 in
      Fifo.put s.search_waiting_peers.(nbits) p;
    end

let create_search kind md4 =
  if !verbose_overnet then lprintf_nl () "create_search";
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
      search_hits = 0;
      search_nresults = 0;
      search_results = Hashtbl.create 13;
    } in
  List.iter (add_search_peer s) (get_closest_peers md4 max_search_queries);
  if !verbose_overnet then lprintf_nl () "create_search done";
  overnet_searches := s :: !overnet_searches;
  s

let create_keyword_search w s =
  let md4 = Md4.string w in
  let search = create_search (KeywordSearch s) md4 in
  search

let ip_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) ->
      Ip.of_inet_addr inet
  | _ -> assert false

let port_of_udp_packet p =
  match p.UdpSocket.udp_addr with
    Unix.ADDR_INET (inet, port) -> port
  | _ -> assert false


let new_peer_message p =
(*  lprintf () "*** Updating time for %s:%d\n" (Ip.to_string p.peer_ip) p.peer_port; *)
  p.peer_last_recv <- last_time ()

let udp_client_handler t p =
  let other_ip = ip_of_udp_packet p in
  let other_port = port_of_udp_packet p in
  if !verbose_overnet then
    lprintf_nl () "UDP FROM %s:%d:\n  %s"
      (Ip.to_string other_ip) other_port
      (message_to_string t);
  match t with

  | OvernetConnect p ->
      let rec send p =
        new_peer_message p;
        let p = new_peer p in
          udp_send p (OvernetConnectReply (get_any_peers 20))
      in
	if Ip.valid p.peer_ip && ip_reachable p.peer_ip && p.peer_port <> 0 then
	  send p
	else
	  if Ip.valid other_ip && ip_reachable other_ip && other_port <> 0 then
	    begin
	      if !verbose_overnet then
	        lprintf_nl () "Connect: convert address %s:%d to %s:%d"
		  (Ip.to_string p.peer_ip) p.peer_port (Ip.to_string other_ip) other_port;
	      p.peer_ip <- other_ip;
	      p.peer_port <- other_port;
	      send p
	    end
	  else
	    begin
	      if !verbose_overnet then
	        lprintf_nl () "Connect: invalid IP %s:%d received from %s:%d"
	          (Ip.to_string p.peer_ip) p.peer_port (Ip.to_string other_ip) other_port;
	      failwith "Message not understood"
	    end

  | OvernetConnectReply ps ->
      UdpSocket.declare_pong other_ip;
      let rec iter list =
        match list with
          [] -> ()
        | [p] ->
            new_peer_message p;
            if other_port <> p.peer_port || other_ip <> p.peer_ip then
              if !verbose_overnet then
              lprintf_nl () "Bad IP or port";
            let p = new_peer p in
            ()
        | p :: tail ->
            let p = new_peer p in
            iter tail
      in
      iter ps;

  | OvernetPublicize p ->
      let rec send p =
        new_peer_message p;
        let p = new_peer p in
	  udp_send p (OvernetPublicized (Some (my_peer ())))
      in
	if Ip.valid p.peer_ip && ip_reachable p.peer_ip && p.peer_port <> 0 then
	  send p
	else
	  if Ip.valid other_ip && ip_reachable other_ip && other_port <> 0 then
	    begin
	      if !verbose_overnet then
	        lprintf_nl () "Publicize: convert address %s:%d to %s:%d"
		  (Ip.to_string p.peer_ip) p.peer_port (Ip.to_string other_ip) other_port;
	      p.peer_ip <- other_ip;
	      p.peer_port <- other_port;
	      send p
	    end
	  else
	    begin
	      if !verbose_overnet then
	        lprintf_nl () "Publicize: invalid IP %s:%d received from %s:%d"
	          (Ip.to_string p.peer_ip) p.peer_port (Ip.to_string other_ip) other_port;
	      failwith "Message not understood"
	    end

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
      if !verbose_hidden_errors then
        begin
          lprintf_nl () "Unknown message from %s:%d " (Ip.to_string other_ip) other_port;
          lprintf_nl () "\tCode: %d" opcode; dump s;
        end

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
                  List.iter (fun (r_md4, r_tags) ->
                      if not (Hashtbl.mem s.search_results r_md4) then
                        begin
                          incr search_hits;
                          s.search_hits <- s.search_hits + 1;
                          Hashtbl.add s.search_results r_md4 r_tags;
                          if !verbose_overnet then begin
                              lprintf_nl () "FILE FOUND, TAGS:";
                              print_tags r_tags;
                              lprintf_nl () ""
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

  | OvernetFirewallConnectionACK(md4) ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl () "FIREWALL ACK for md4=%s" (Md4.to_string md4)   
    
   | OvernetFirewallConnectionNACK(md4) ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl () "FIREWALL NACK for md4=%s" (Md4.to_string md4)   
    
 (* send the answer *)   
   | OvernetGetMyIP other_port ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl () "GET MY IP (port=%d)\n" other_port;   
 (* FIXME : should be able to flush the UDP buffer*)   
       udp_send_direct other_ip other_port (OvernetGetMyIPResult other_ip);   
       udp_send_direct other_ip other_port OvernetGetMyIPDone   
    
   | OvernetGetMyIPResult(ip) ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl () "GET MY IP RESULT (%s)\n" (Ip.to_string ip)   
    
   | OvernetGetMyIPDone ->   
       if !verbose_overnet && debug_client other_ip then   
         lprintf_nl () "GET MY IP DONE\n"   
  
  | OvernetPeerNotFound peer ->
      begin
        if !verbose_overnet && debug_client other_ip then
          lprintf_nl () "Peer NOT FOUND %s (%s:%d) kind: %d (msg 33)"
            (Md4.to_string peer.peer_md4) (Ip.to_string peer.peer_ip)
        peer.peer_port peer.peer_kind;
        let key = (peer.peer_ip, peer.peer_port) in
        if Hashtbl.mem known_peers key
        then begin
            Hashtbl.remove known_peers key;
          end
      end

   | OvernetUnknown21 peer ->
      if !verbose_overnet && debug_client other_ip then begin
          lprintf_nl () "Unknown 21 message ...";
          lprintf_nl () "From peer: %s ip: %s:%d kind: %d"
            (Md4.to_string peer.peer_md4) (Ip.to_string peer.peer_ip) peer.peer_port peer.peer_kind
        end

  | _ -> failwith "Message not understood"

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

let recover_file file =
  if file_state file = FileDownloading then
    let search = create_search (FileSearch file) file.file_md4 in
    ()

let check_current_downloads () =
  List.iter recover_file !DonkeyGlobals.current_files

let update_buckets () =

(* 1. Clean the buckets from too old peers ( last contact > 1 hour ) *)

  let overtime = last_time () - 3600 in

  for i = 0 to !n_used_buckets do

    let b = buckets.(i) in
    for j = 1 to Fifo.length b do
      let p = Fifo.take b in
      if p.peer_last_recv > overtime then Fifo.put b p else
      begin
        (* remove the peer also from known_peers *)
        let key = (p.peer_ip, p.peer_port) in
	Hashtbl.remove known_peers key;
        decr connected_peers
      end
    done

  done;

(* 2. Complete buckets with new peers from the prebuckets with
( last_contact < 1 hour ) *)

  for i = 0 to !n_used_buckets - 1 do
    let b = buckets.(i) in
    if Fifo.length b < max_peers_per_bucket then begin

        try
          let pb = prebuckets.(i) in
          for j = 1 to Fifo.length pb do

            let p = Fifo.take pb in
            if p.peer_last_recv > overtime then begin
                Fifo.put b p;
                incr connected_peers;
		decr pre_connected_peers;
                if Fifo.length b = max_peers_per_bucket then raise Exit
              end else Fifo.put pb p

          done
        with Exit -> ()

      end
  done;

  ()

let enable () =
  if !!enable_overnet && not !is_enabled then begin
      let enabler = is_enabled in
      is_enabled := true;

      let sock = (UdpSocket.create (Ip.to_inet_addr !!client_bind_addr)
          (!!overnet_port) (Proto.udp_handler udp_client_handler)) in
      udp_sock := Some sock;
      UdpSocket.set_write_controler sock udp_write_controler;

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
                          udp_send_ping ip port (OvernetConnect my_peer);
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

          update_buckets ();

(* compute which peers to ping in the next minute *)
          to_ping := [];
          let n_to_ping = ref 0 in

          let ping_peers b =
            let overtime = last_time () - 1800 in
            Fifo.iter (fun p ->
                if p.peer_last_recv < overtime &&
                  p.peer_last_send < overtime then begin
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

(* reset the Hashtbls and Fifos for searches that are older than 5 minutes *)
          let l = last_time () - 300 in
	  List.iter ( fun s ->
	     if s.search_requests < max_search_requests &&
	     s.search_start > l then begin
               Hashtbl.clear s.search_known_peers;
	       Array.iter (fun a -> Fifo.clear a) s.search_waiting_peers;
	       Array.iter (fun a -> Fifo.clear a) s.search_asked_peers;
	       Array.iter (fun a -> Fifo.clear a) s.search_ok_peers;
	       Hashtbl.clear s.search_results;
	     end
	  ) !overnet_searches;
(* remove searches that are older than 5 minutes *)
          overnet_searches := List.filter (fun s ->
              s.search_requests < max_search_requests &&
              s.search_start > l
          ) !overnet_searches;
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
    end

let _ =
  option_hook enable_overnet
    (fun _ ->
      if !CommonOptions.start_running_plugins then
        if !!enable_overnet = false then disable() else enable ()
  );
  option_hook overnet_query_peer_period
    (fun _ -> if !!overnet_query_peer_period < 5. then overnet_query_peer_period =:= 5.)

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
      lprintf_nl () "Exception %s while loading %s" (Printexc2.to_string e)
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
    "boot", Arg_two (fun ip port o ->
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        bootstrap ip port;
        Printf.sprintf "peer %s:%d added" (Ip.to_string ip) port
    ), ("<ip> <port> :\t\t\tadd an " ^ command_prefix_to_net ^ " peer");

    "link", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let url = String2.unsplit args ' ' in
        if parse_overnet_url url then
          "download started"
        else "bad syntax"
    ), "<fhalink> :\t\t\tdownload fha:// link";

    "view_stats_cmds", Arg_none (fun o ->
        let buf = o.conn_buf in
        html_mods_commands buf "commandsTable" "commands" ([
          ("bu bbig", command_prefix_to_net ^ " boots list",
           "top.output.location.href='submit\\?q=" ^ command_prefix ^ "boots'",
           command_prefix_to_net ^ " boots");
          ("bu bbig", command_prefix_to_net ^ " buckets list",
           "top.output.location.href='submit\\?q=" ^ command_prefix ^ "buckets'",
           command_prefix_to_net ^ " buckets");
          ("bu bbig", command_prefix_to_net ^ " stats",
           "top.output.location.href='submit\\?q=" ^ command_prefix ^ "stats'",
           command_prefix_to_net ^ " stats");
          ("bu bbig", command_prefix_to_net ^ " store list",
           "top.output.location.href='submit\\?q=" ^ command_prefix ^ "store'",
           command_prefix_to_net ^ " store");
          ]);
        "";
    ), (":\t\t\t\t" ^ command_prefix_to_net ^ " Stats commands");

    "stats", Arg_none (fun o ->
        let buf = o.conn_buf and sum = ref 0 in
        if o.conn_output = HTML then
          begin
            Printf.bprintf buf "\\<div class=results\\>";
            html_mods_table_header buf "ovstatsTable" "sources" [];
            Printf.bprintf buf "\\<tr\\>";
            html_mods_td buf [
              ("", "srh", Printf.sprintf "%s statistics" command_prefix_to_net);
              ("", "srh", Printf.sprintf "Search hits: %d\n" !search_hits);
              ("", "srh", Printf.sprintf "Source hits: %d\n" !source_hits); ];
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\n";
          end
        else
          begin
            Printf.bprintf buf "%s statistics:\n"
	    (command_prefix_to_net);
            Printf.bprintf buf "  Search hits: %d\n" !search_hits;
            Printf.bprintf buf "  Source hits: %d\n" !source_hits;
          end;
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
              end
            else
            Printf.bprintf buf "Search %s for %s\n"

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
                  if o.conn_output = HTML then
                    begin
                      Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
                      html_mods_td buf [
                        ("", "sr",
                          Printf.sprintf "nbits[%d] = %d peer(s) not asked, %d peer(s) asked"
                          i npeers nasked); ];
                      Printf.bprintf buf "\\</tr\\>";

		    end
		  else
                  Printf.bprintf buf
                  "   nbits[%d] = %d peers not asked, %d peers asked\n"
                    i npeers nasked
              done;
              if o.conn_output = HTML then
                Printf.bprintf buf "\\</table\\>\\</div\\>\n";
        ) !overnet_searches;
        if o.conn_output = HTML then
          Printf.bprintf buf "\\</div\\>\n";

        "";
    ), ":\t\t\t\t" ^ command_prefix_to_net ^ " Stats";

    "web", Arg_multiple (fun args o ->
        let urls =
          match args with
            [] -> let list = ref [] in
              List.iter (fun (kind,_, url) ->
                  if kind = web_info then list := url :: !list
              )!!web_infos;
              !list
          | _ -> args
        in
        List.iter (fun url ->
            Printf.bprintf o.conn_buf "Loading %s\n" url;
            CommonWeb.load_url web_info url) urls;
        "web boot started"
    ), "<urls> :\t\t\t\tdownload .ocl URLS (no arg load default)";

    "load", Arg_one (fun filename o ->
        let buf = o.conn_buf in
        try
          let n = load_contact_dat filename in
          Printf.sprintf "%d overnet peers loaded" n;
        with e ->
            Printf.sprintf "error %s while loading file" (Printexc2.to_string e)
    ), "<filename> :\t\t\tload the peers from a contact.dat file";

    "md4", Arg_none (fun o -> "MD4 is " ^ (Md4.to_string !!overnet_md4);
    ), ":\t\t\t\t\tget client MD4 address on the Overnet/Kademlia network";

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
            Buffer.clear buftmp;
            PublishedFiles.print buftmp;
            let listtmp = String2.split (Buffer.contents buftmp) '\n' in
            let files = ref [] in
            List.iter (fun s ->
                files := !files @ [("", "dl-1", s);]
              ) listtmp;
            Buffer.clear buftmp;
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
          lprintf_nl () "Sending UDP message %d to %s:%s" opcode ip port;
          dump msg; lprintf_nl () ""; "Sending UDP message"
        with _ ->
            lprintf_nl () "Unable to send UDP message"; "Unable to send UDP message"
    ), ":\t\t\t\tsend UDP message (<ip> <port> <msg in hex>)";

    "buckets", Arg_none (fun o ->
        let buf = o.conn_buf in
        update_buckets ();
        if o.conn_output != HTML then
          Printf.bprintf buf "Number of used buckets %d with %d peers (prebucket: %d peers)\n"
            !n_used_buckets !connected_peers !pre_connected_peers;
        for i = 0 to !n_used_buckets do
          if Fifo.length buckets.(i) > 0 ||
            Fifo.length prebuckets.(i) > 0 then
              Printf.bprintf buf "   bucket[%d] : %d peers (prebucket %d)\n"
                i (Fifo.length buckets.(i)) (Fifo.length prebuckets.(i));
        done;
        if o.conn_output = HTML then
          begin
            let listtmp = String2.split (Buffer.contents buf) '\n' in
            let buckets = ref [] in
            List.iter (fun s ->
                buckets := !buckets @ [("", "dl-1", s);]
            ) listtmp;
            Buffer.clear buf;
            html_mods_table_one_col buf "ovbucketsTable" "results" ([
              ("", "srh",
                Printf.sprintf "Number of used buckets %d with %d peers (prebucket: %d peers)"
                  !n_used_buckets !connected_peers !pre_connected_peers);
              ] @ !buckets);
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
            Buffer.clear buf;
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
    if !verbose_overnet then lprintf_nl () "========= %s_search =========" command_prefix_to_net;
    let ws = keywords_of_query q in
    List.iter (fun w ->
        if !verbose_overnet then lprintf_nl () "%s_search for %s" command_prefix_to_net w;
        let s = create_keyword_search w ss in
        Hashtbl.iter (fun r_md4 r_tags ->
            DonkeyOneFile.search_found true ss r_md4 r_tags) s.search_results;
    )
    ws

let forget_search ss =
  begin
(* reset the Hashtbls and Fifos *)
    List.iter ( fun s ->
    match s.search_kind with
      KeywordSearch sss when ss == sss ->
        begin
          Hashtbl.clear s.search_known_peers;
          Array.iter (fun a -> Fifo.clear a) s.search_waiting_peers;
          Array.iter (fun a -> Fifo.clear a) s.search_asked_peers;
          Array.iter (fun a -> Fifo.clear a) s.search_ok_peers;
          Hashtbl.clear s.search_results;
        end
      | _ -> ()
    ) !overnet_searches;
(* Remove from overnet_searches *)    
  overnet_searches := List.filter (fun s ->
      match s.search_kind with
        KeywordSearch sss when ss == sss -> false
      | _ -> true) !overnet_searches
  end    

let _ =
  CommonWeb.add_redirector_info Proto.redirector_section (fun buf ->
      update_buckets ();
      let peers = get_any_peers 32 in
      buf_int buf (List.length peers);
      List.iter (fun p ->
          buf_ip buf p.peer_ip;
          buf_int16 buf p.peer_port;
          buf_int16 buf p.peer_tcpport;
      )
      peers;
  )

let cancel_recover_file file =
   begin
(* reset the Hashtbls and Fifos *)
    List.iter ( fun s ->
    match s.search_kind with
      FileSearch f when f == file ->
        begin
          Hashtbl.clear s.search_known_peers;
          Array.iter (fun a -> Fifo.clear a) s.search_waiting_peers;
          Array.iter (fun a -> Fifo.clear a) s.search_asked_peers;
          Array.iter (fun a -> Fifo.clear a) s.search_ok_peers;
          Hashtbl.clear s.search_results;
        end
      | _ -> ()
    ) !overnet_searches;
(* Remove from overnet_searches *)
   overnet_searches := List.filter (fun s ->
      match s.search_kind with
        FileSearch f when f == file -> false
      | _ -> true) !overnet_searches
   end   

let _ =
  CommonWeb.add_web_kind web_info (fun _ filename ->
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
                        lprintf_nl () "Adding %s peer %s:%d" command_prefix_to_net name port;
                      end;
                    bootstrap ip port)
            | _ ->
                lprintf_nl () "BAD LINE ocl: %s" s;
          with _ ->
              begin
                lprintf_nl () "DNS failed";
              end
      ) lines
  );

  (* Add this kind of web_info only for overnet (Edonkey
     Kademlia has web_info = "kad"). *)
  if web_info = "contact.dat" then begin
    if !!enable_overnet && !!overnet_update_nodes then
      CommonWeb.add_web_kind "contact.dat" (fun _ filename ->
          if !verbose_overnet then
            lprintf_nl () "LOADED contact.dat";
          let n = load_contact_dat filename in
          if !verbose_overnet then
            lprintf_nl () "%d PEERS ADDED" n;
        );
    end;

(*************************************************************

Define a function to be called when the "mem_stats" command
  is used to display information on structure footprint.

**************************************************************)

  Heap.add_memstat
    (if Proto.redirector_section = "DKKO" then "Overnet" else "Kademlia")
    (fun level buf ->
      Printf.bprintf buf "%s statistics:\n"
      (if Proto.redirector_section = "DKKO" then "Overnet" else "Kademlia");
      Printf.bprintf buf "  boot_peers: %d\n" (LimitedList.length !!boot_peers);
      update_buckets ();
      Printf.bprintf buf "  %d buckets with %d peers and %d prebucket peers\n"
            !n_used_buckets !connected_peers !pre_connected_peers;

      Printf.bprintf buf "  Search hits: %d\n" !search_hits;
      Printf.bprintf buf "  Source hits: %d\n" !source_hits;
      Printf.bprintf buf "  Hashtbl.lenght known_peers: %d\n" (Hashtbl.length known_peers);
      let n_search_known_peers = ref 0 in
      let n_search_waiting_peers = ref 0 in
      let n_search_asked_peers = ref 0 in
      let n_search_ok_peers = ref 0 in
      let n_search_results = ref 0 in
      let n_overnet_searches = ref 0 in
      List.iter ( fun s ->
              for i = 128 downto 0 do
               n_search_known_peers :=
                 !n_search_known_peers + (Hashtbl.length s.search_known_peers);
               n_search_waiting_peers :=
                 !n_search_waiting_peers + (Fifo.length s.search_waiting_peers.(i));
               n_search_asked_peers :=
                 !n_search_asked_peers + (Fifo.length s.search_asked_peers.(i));
               n_search_ok_peers :=
                 !n_search_ok_peers + (Fifo.length s.search_ok_peers.(i));
               n_search_results :=
                 !n_search_results + (Hashtbl.length s.search_results);
              done;
             incr n_overnet_searches
      ) !overnet_searches;
      Printf.bprintf buf "  n_search_known_peers: %d\n" !n_search_known_peers;
      Printf.bprintf buf "  n_search_waiting_peers: %d\n" !n_search_waiting_peers;
      Printf.bprintf buf "  n_search_asked_peers: %d\n" !n_search_asked_peers;
      Printf.bprintf buf "  n_search_ok_peers: %d\n" !n_search_ok_peers;
      Printf.bprintf buf "  n_search_results: %d\n" !n_search_results;
      Printf.bprintf buf "  n_overnet_searches: %d\n" !n_overnet_searches;
  );
end
