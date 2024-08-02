(** 
  DHT

  http://www.bittorrent.org/beps/bep_0005.html
*)

open Kademlia
open Printf

let dht_query_timeout = 20
let store_peer_timeout = minutes 30
let secret_timeout = minutes 10
let alpha = 3

let log_prefix = "[dht]"
let lprintf_nl ?exn fmt = Printf2.lprintf_nl2 ?exn log_prefix fmt

let catch f x = try `Ok (f x) with e -> `Exn e
let (&) f x = f x
let (!!) = Lazy.force

let self_version =
  let module A = Autoconf in
  let n = int_of_string A.major_version * 100 + int_of_string A.minor_version * 10 + int_of_string A.sub_version - 300 in
  assert (n > 0 && n < 256);
  sprintf "ML%c%c" (if A.scm_version = "" then '=' else '+') (Char.chr n)

(* 2-level association *)
module Assoc2 : sig

  type ('a,'b,'c) t
  val create : unit -> ('a,'b,'c) t
  val add : ('a,'b,'c) t -> 'a -> 'b -> 'c -> unit
  val find_all : ('a,'b,'c) t -> 'a -> ('b,'c) Hashtbl.t
  val find : ('a,'b,'c) t -> 'a -> 'b -> 'c option
  val remove : ('a,'b,'c) t -> 'a -> 'b -> unit
  val iter : ('a,'b,'c) t -> ('a -> 'b -> 'c -> unit) -> unit
  val clear : ('a,'b,'c) t -> unit

end = struct

type ('a,'b,'c) t = ('a, ('b, 'c) Hashtbl.t) Hashtbl.t

let create () = Hashtbl.create 13
let add h a b c =
  let hh = try Hashtbl.find h a with Not_found -> Hashtbl.create 3 in
  Hashtbl.replace hh b c;
  Hashtbl.replace h a hh
let find_all h a = try Hashtbl.find h a with Not_found -> Hashtbl.create 3
let find h a b = try Some (Hashtbl.find (Hashtbl.find h a) b) with Not_found -> None
let remove h a b = try let ha = Hashtbl.find h a in Hashtbl.remove ha b; if Hashtbl.length ha = 0 then Hashtbl.remove h a with Not_found -> ()
let iter h f = Hashtbl.iter (fun a h -> Hashtbl.iter (fun b c -> f a b c) h) h
let clear h = Hashtbl.clear h

end

let stats_add h k n = Hashtbl.replace h k (n + try Hashtbl.find h k with Not_found -> 0)

module KRPC = struct

type dict = (string * Bencode.value) list
let show_dict d = String.concat "," & List.map fst d

type msg = 
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

let show_msg = function
  | Query (name,args) -> sprintf "query %s(%s)" name (show_dict args)
  | Response d -> sprintf "response (%s)" (show_dict d)
  | Error (e,s) -> sprintf "error (%Ld,%S)" e s

let encode (txn,msg) =
  let module B = Bencode in
  let x = match msg with
  | Query (name,args) -> ["y", B.String "q"; "q", B.String name; "a", B.Dictionary args]
  | Response dict -> ["y", B.String "r"; "r", B.Dictionary dict]
  | Error (code,text) -> ["y", B.String "e"; "e", B.List [B.Int code; B.String text] ]
  in
  let x = ("t", B.String txn) :: ("v", B.String self_version):: x in
  B.encode (B.Dictionary x)

let str = function Bencode.String s -> s | _ -> failwith "str"
let int = function Bencode.Int s -> s | _ -> failwith "int"
let dict = function Bencode.Dictionary s -> s | _ -> failwith "dict"
let list = function Bencode.List l -> l | _ -> failwith "list"

exception Protocol_error of string * string
exception Malformed_packet of string
exception Method_unknown of string

let decode_exn s =
  let module B = Bencode in
  let module Array = struct let get x k = match x with B.Dictionary l -> List.assoc k l | _ -> failwith "decode get" end in
  let x = try B.decode s with _ -> raise (Malformed_packet "decode") in
  let txn = try str x.("t") with _ -> raise (Malformed_packet "txn") in
  let ver = try Some (str x.("v")) with _ -> None in
  try
  let msg = match str x.("y") with
  | "q" -> Query (str x.("q"), dict x.("a"))
  | "r" -> Response (dict x.("r"))
  | "e" -> begin match list x.("e") with B.Int n :: B.String s :: _ -> Error (n, s) | _ -> failwith "decode e" end
  | _ -> failwith "type"
  in (txn, ver, msg)
  with
  exn ->
    if !verb then lprintf_nl ~exn "err";
    raise (Protocol_error (txn,"Invalid argument"))

open BasicSocket
open UdpSocket

let udp_set_reader socket f =
  set_reader socket begin fun _ ->
    try read_packets socket f with exn -> 
      if !verb then lprintf_nl ~exn "udp reader";
      close socket (Closed_for_exception exn)
  end

module A = Assoc2

let send sock stats (ip,port as addr) txnmsg =
  let s = encode txnmsg in
  if !debug then lprintf_nl "KRPC to %s : %S" (show_addr addr) s;
  stats_add stats `Sent 1;
  stats_add stats `SentBytes (String.length s);
  write sock false (Bytes.unsafe_of_string s) ip port

type stats_key = [ `Timeout | `Sent | `SentBytes | `Recv | `RecvBytes | `Decoded | `Handled | `NoTxn ]
type t =
  UdpSocket.t *
  (stats_key, int) Hashtbl.t * 
  (addr, string, (addr -> dict -> unit) * ([`Error|`Timeout]-> unit) * int) A.t
let show_stats t =
  let get k = try Hashtbl.find t k with Not_found -> 0 in
  [
    sprintf "rpc recv %d pkts (%d bytes)" (get `Recv) (get `RecvBytes);
    sprintf "rpc sent %d pkts (%d bytes)" (get `Sent) (get `SentBytes);
    sprintf "rpc decoded %d, handled %d" (get `Decoded) (get `Handled);
    sprintf "rpc timeouted %d, orphan %d" (get `Timeout) (get `NoTxn);
  ]

let create port enabler bw_control answer : t =
  let socket = create Unix.inet_addr_any port (fun sock event ->
      match event with
      | WRITE_DONE | CAN_REFILL -> ()
      | READ_DONE -> assert false (* set_reader prevents this *)
      | BASIC_EVENT x -> match x with
        | CLOSED _ -> ()
        | CAN_READ | CAN_WRITE -> assert false (* udpSocket implementation prevents this *)
        | LTIMEOUT | WTIMEOUT | RTIMEOUT -> () (*close sock (Closed_for_error "KRPC timeout")*))
  in
  set_write_controler socket bw_control;
  set_wtimeout (sock socket) 5.;
  set_rtimeout (sock socket) 5.;
  let h = A.create () in
  let stats = Hashtbl.create 10 in
  let timeout h =
    let now = last_time () in
    let bad = ref [] in
    let total = ref 0 in
    A.iter h (fun addr txn (_,kerr,t) -> incr total; if t < now then bad := (addr,txn,kerr) :: !bad);
    if !debug then lprintf_nl "timeouted %d of %d DHT queries" (List.length !bad) !total;
    stats_add stats `Timeout (List.length !bad);
    List.iter (fun (addr,txn,kerr) ->
      A.remove h addr txn;
      try kerr `Timeout with exn -> if !debug then lprintf_nl ~exn "timeout for %s" (show_addr addr)) !bad;
  in
  BasicSocket.add_session_timer enabler 5. (fun () -> timeout h);
  let handle addr (txn,ver,msg) =
    let version = lazy (match ver with Some s -> sprintf " client %S" s | None -> "") in
    if !debug then lprintf_nl "KRPC from %s %stxn %S : %s" (show_addr addr) !!version txn (show_msg msg);
    match msg with
    | Error (code,msg) ->
        if !verb then lprintf_nl "error received from %s%s : %Ld %S" (show_addr addr) !!version code msg;
        begin match A.find h addr txn with
        | None ->
          stats_add stats `NoTxn 1;
          if !verb then lprintf_nl "no txn %S for %s%s" txn (show_addr addr) !!version
        | Some (_, kerr, _) -> A.remove h addr txn; kerr `Error
        end
    | Query (name,args) ->
        let ret = answer addr name args in
        send socket stats addr (txn, ret)
    | Response ret ->
        match A.find h addr txn with
        | None ->
          stats_add stats `NoTxn 1;
          if !verb then lprintf_nl "no txn %S for %s%s" txn (show_addr addr) !!version
        | Some (k,_,_) -> A.remove h addr txn; k addr ret
  in
  let handle p =
    match p.udp_addr with
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (inet_addr,port) ->
      let addr = (Ip.of_inet_addr inet_addr, port) in
      let ret = ref None in
      try
        stats_add stats `RecvBytes (Bytes.length p.udp_content);
        stats_add stats `Recv 1;
        let r = decode_exn @@ Bytes.unsafe_to_string p.udp_content in
        stats_add stats `Decoded 1;
        ret := Some r;
        handle addr r;
        stats_add stats `Handled 1;
      with exn ->
        let version = match !ret with Some (_,Some s,_) -> sprintf " client %S" s | _ -> "" in
        if !verb then lprintf_nl ~exn "handle packet from %s%s : %S" (show_addr addr) version (Bytes.unsafe_to_string p.udp_content);
        let error txn code str = send socket stats addr (txn,(Error (Int64.of_int code,str))) in
        match exn,!ret with
        | Malformed_packet x, Some (txn, _, _)
        | Protocol_error ("",x), Some(txn, _, _) | Protocol_error (txn,x), _ -> error txn 203 x
        | Method_unknown x, Some (txn, _, _) -> error txn 204 x
        | _, Some (txn, _, Query _) -> error txn 202 ""
        | _ -> ()
  in
  udp_set_reader socket handle;
  (socket,stats,h)

let shutdown (socket,_,h) =
  close socket Closed_by_user;
  A.iter h (fun addr _ (_,kerr,_) ->
    try kerr `Timeout with exn -> if !verb then lprintf_nl ~exn "shutdown for %s" (show_addr addr));
  A.clear h

let write (socket,stats,h) msg addr k ~kerr =
  let tt = Assoc2.find_all h addr in
  let rec loop () = (* choose txn FIXME *)
    let txn = string_of_int (Random.int 1_000_000) in
    match Hashtbl.mem tt txn with
    | true -> loop ()
    | false -> txn
  in
  let txn = loop () in
  Assoc2.add h addr txn (k,kerr,last_time () + dht_query_timeout);
  send socket stats addr (txn,msg)

end (* KRPC *)

type query =
| Ping
| FindNode of id
| GetPeers of H.t
| Announce of H.t * int * string

let show_query = function
| Ping -> "ping"
| FindNode id -> sprintf "find_node %s" (show_id id)
| GetPeers h -> sprintf "get_peers %s" (show_id h)
| Announce (h,port,token) -> sprintf "announce %s port=%d token=%S" (show_id h) port token

type response =
| Ack
| Nodes of (id * addr) list
| Peers of string * addr list * (id * addr) list

let strl f l = "[" ^ (String.concat " " & List.map f l) ^ "]"

let show_node (id,addr) = sprintf "%s (%s)" (show_addr addr) (show_id id)

let show_response = function
| Ack -> "ack"
| Nodes l -> sprintf "nodes %s" (strl show_node l)
| Peers (token,peers,nodes) -> sprintf "peers token=%S %s %s" token (strl show_addr peers) (strl show_node nodes)

let parse_query_exn name args =
  let get k = List.assoc k args in
  let sha1 k = H.direct_of_string & KRPC.str & get k in
  let p = match name with
  | "ping" -> Ping
  | "find_node" -> FindNode (sha1 "target")
  | "get_peers" -> GetPeers (sha1 "info_hash")
  | "announce_peer" -> Announce (sha1 "info_hash", Int64.to_int & KRPC.int & get "port", KRPC.str & get "token")
  | s -> failwith (sprintf "parse_query name=%s" name)
  in
  sha1 "id", p

let make_query id x =
  let sha1 x = Bencode.String (H.direct_to_string x) in
  let self = ("id", sha1 id) in
  match x with
  | Ping -> KRPC.Query ("ping", [self])
  | FindNode t -> KRPC.Query ("find_node", ["target", sha1 t; self])
  | GetPeers h -> KRPC.Query ("get_peers", ["info_hash", sha1 h; self])
  | Announce (h, port, token) -> KRPC.Query ("announce_peer", 
      ["info_hash", sha1 h; 
       "port", Bencode.Int (Int64.of_int port);
       "token", Bencode.String token;
       self])

let parse_peer s =
  if String.length s <> 6 then failwith "parse_peer" else
  let c i = int_of_char & s.[i] in
  Ip.of_ints (c 0,c 1,c 2,c 3), (c 4 lsl 8 + c 5)

let parse_nodes s =
  assert (String.length s mod 26 = 0);
  let i = ref 0 in
  let nodes = ref [] in
  while !i < String.length s do
    nodes := (H.direct_of_string (String.sub s !i 20), parse_peer (String.sub s (!i+20) 6)) :: !nodes;
    i := !i + 26;
  done;
  !nodes

let make_peer (ip,port) =
  assert (port <= 0xffff);
  let (a,b,c,d) = Ip.to_ints ip in
  let e = port lsr 8 and f = port land 0xff in
  let s = Bytes.create 6 in
  let set i c = s.[i] <- char_of_int c in
  set 0 a; set 1 b; set 2 c; set 3 d; set 4 e; set 5 f;
  Bytes.unsafe_to_string s

let make_nodes nodes =
  let s = Bytes.create (26 * List.length nodes) in
  let i = ref 0 in
  List.iter (fun (id,addr) ->
    Bytes.blit_string (H.direct_to_string id) 0 s (!i*26) 20;
    Bytes.blit_string (make_peer addr) 0 s (!i*26+20) 6;
    incr i
    ) nodes;
  Bytes.unsafe_to_string s

let parse_response_exn q dict =
  let get k = List.assoc k dict in
  let sha1 k = H.direct_of_string & KRPC.str & get k in
  let p = match q with
  | Ping -> Ack
  | FindNode _ -> 
    let s = KRPC.str & get "nodes" in
    Nodes (parse_nodes s)
  | GetPeers _ ->
    let token = KRPC.str & get "token" in
    let nodes = try parse_nodes (KRPC.str & get "nodes") with Not_found -> [] in
    let peers = try List.map (fun x -> parse_peer & KRPC.str x) & (KRPC.list & get "values") with Not_found -> [] in
    Peers (token, peers, nodes)
  | Announce _ -> Ack
  in
  sha1 "id", p

let make_response id x =
  let sha1 x = Bencode.String (H.direct_to_string x) in
  let self = ("id", sha1 id) in
  let str s = Bencode.String s in
  match x with
  | Ack -> KRPC.Response [self]
  | Nodes nodes -> KRPC.Response [self;"nodes",str (make_nodes nodes)]
  | Peers (token,peers,nodes) -> KRPC.Response 
      [self;
       "token",str token;
       "nodes",str (make_nodes nodes);
       "values",Bencode.List (List.map (fun addr -> str (make_peer addr)) peers);
       ]

module Test = struct

open Bencode

let e = Dictionary ["t",String "aa"; "v", String self_version; "y", String "e"; "e", List [Int 201L; String "A Generic Error Occurred"] ]
let s = sprintf "d1:eli201e24:A Generic Error Occurrede1:t2:aa1:v4:%s1:y1:ee" self_version
let v = "aa", KRPC.Error (201L, "A Generic Error Occurred")

let () = 
  assert (encode e = s);
  assert (KRPC.decode_exn s = (fst v, Some self_version, snd v));
  assert (KRPC.encode v = s);
  ()

end

module Peers = Map.Make(struct type t = addr let compare = compare end)

module M = struct

type query_type = [ `Ping | `FindNode | `GetPeers | `Announce ]
type answer_type = [ `Answer | `Error | `Timeout ]

type t = {
  rt : Kademlia.table; (* routing table *)
  rpc : KRPC.t; (* KRPC protocol socket *)
  dht_port : int; (* port *)
  torrents : (H.t, int Peers.t) Hashtbl.t; (* torrents announced by other peers *)
  enabler : bool ref; (* timers' enabler *)
  stats : (query_type * [ `In | `Out of answer_type ], int) Hashtbl.t; (* statistics *)
}

let query_type_of_query = function
| Ping -> `Ping
| FindNode _ -> `FindNode
| GetPeers _ -> `GetPeers
| Announce _ -> `Announce

let dht_query t addr q k ~kerr =
  if !debug then lprintf_nl "DHT query to %s : %s" (show_addr addr) (show_query q);
  let qt = query_type_of_query q in
  KRPC.write t.rpc (make_query t.rt.self q) addr begin fun addr dict ->
    let (id,r) = try parse_response_exn q dict with exn -> stats_add t.stats (qt, `Out `Error) 1; kerr (); raise exn in
    if !debug then lprintf_nl "DHT response from %s (%s) : %s" (show_addr addr) (show_id id) (show_response r);
    stats_add t.stats (qt, `Out `Answer) 1;
    k (id,addr) r
  end ~kerr:(fun reason -> stats_add t.stats (qt, `Out (reason:>answer_type)) 1; kerr ())

let ping t addr k = dht_query t addr Ping begin fun node r ->
  match r with Ack -> k (Some node)
  | _ -> k None; failwith "dht_query ping" end ~kerr:(fun () -> k None)

let find_node t addr h k ~kerr = dht_query t addr (FindNode h) begin fun node r ->
  match r with Nodes l -> k node l
  | _ -> kerr (); failwith "dht_query find_node" end ~kerr

let get_peers t addr h k ~kerr = dht_query t addr (GetPeers h) begin fun node r ->
  match r with Peers (token,peers,nodes) -> k node token peers nodes
  | _ -> kerr (); failwith "dht_query get_peers" end ~kerr

let announce t addr port token h k ~kerr = dht_query t addr (Announce (h,port,token)) begin fun node r ->
  match r with Ack -> k node
  | _ -> kerr (); failwith "dht_query announce" end ~kerr

let store t info_hash addr =
  let peers = try Hashtbl.find t.torrents info_hash with Not_found -> Peers.empty in
  Hashtbl.replace t.torrents info_hash (Peers.add addr (BasicSocket.last_time () + store_peer_timeout) peers)

let manage_timeouts enabler h =
  BasicSocket.add_session_timer enabler 60. begin fun () ->
    let now = BasicSocket.last_time () in
    let torrents = Hashtbl.fold (fun k peers l -> (k,peers)::l) h [] in
    let rm = ref 0 in
    let total = ref 0 in
    List.iter (fun (id,peers) ->
      let m = Peers.fold (* removing is rare *)
        (fun peer expire m -> incr total; if expire < now then (incr rm; Peers.remove peer m) else m)
        peers peers
      in
      if Peers.is_empty m then Hashtbl.remove h id else Hashtbl.replace h id m
    ) torrents;
    if !debug then lprintf_nl "Removed %d of %d peers for announced torrents" !rm !total
  end

let create rt dht_port bw_control answer =
  let enabler = ref true in
  let rpc = KRPC.create dht_port enabler bw_control answer in
  let torrents = Hashtbl.create 8 in
  manage_timeouts enabler torrents;
  { rt = rt; rpc = rpc; torrents = torrents; dht_port = dht_port; enabler = enabler;
    stats = Hashtbl.create 10; }

let shutdown dht =
  dht.enabler := false;
  KRPC.shutdown dht.rpc

let peers_list f m = Peers.fold (fun peer tm l -> (f peer tm)::l) m []
let self_get_peers t h =
  let peers = peers_list (fun a _ -> a) (try Hashtbl.find t.torrents h with Not_found -> Peers.empty) in
  if List.length peers <= 100 then
    peers
  else
    let a = Array.of_list peers in
    Array2.shuffle a;
    Array.to_list (Array.sub a 0 100)
 
let self_find_node t h = List.map (fun node -> node.id, node.addr) & Kademlia.find_node t.rt h

end (* module M *)

module Secret : sig 

type t
val create : time -> t
val get : t -> string
val valid : t -> string -> bool
val get_prev : t -> string

end = struct

type t = { mutable cur : string; mutable prev : string; timeout : time; mutable next : time; }
let make () = string_of_int (Random.int 1_000_000)
let create tm =
  assert (tm > 0);
  let s = make () in 
  { cur = s; prev = s; timeout = tm; next = now () + tm; }
let invalidate t =
  if now () > t.next then
  begin
    t.prev <- t.cur;
    t.cur <- make ();
    t.next <- now () + t.timeout;
  end
let get t =
  invalidate t;
  t.cur
let get_prev t = t.prev
let valid t s =
  invalidate t;
  s = t.cur || s = t.prev

end

(* do not hash port cause some broken implementations change it all the time *)
let make_token (ip,_) h secret = string_of_int (Hashtbl.hash (Ip.to_string ip, H.direct_to_string h, secret))

let valid_token addr h secret token =
  let cur = Secret.get secret in
  let prev = Secret.get_prev secret in
  token = make_token addr h cur || token = make_token addr h prev

module LimitedSet = struct

module type S = sig

type elt
type t
val create : int -> t

(** @return whether the element was really added *)
val insert : t -> elt -> bool
val elements : t -> elt list
val iter : t -> (elt -> unit) -> unit
val min_elt : t -> elt

end 

module Make(Ord:Set.OrderedType) : S with type elt = Ord.t =
struct

module S = Set.Make(Ord)

type elt = Ord.t
type t = int ref * S.t ref

let create n = ref n, ref S.empty

let insert (left,set) elem =
  match S.mem elem !set with
  | true -> false
  | false ->
    match !left with
    | 0 ->
      let max = S.max_elt !set in
      if Ord.compare elem max < 0 then
        begin set := S.add elem (S.remove max !set); true end
      else 
        false
    | n ->
      set := S.add elem !set;
      decr left;
      true

let iter (_,set) f = S.iter f !set

let elements (_,set) = S.elements !set

let min_elt (_,set) = S.min_elt !set

end (* Make *)

end (* LimitedSet *)

let update dht st id addr = update (M.ping dht) dht.M.rt st id addr

exception Break

(** @param nodes nodes to start search from, will not be inserted into routing table *)
let lookup_node dht ?nodes target k =
  if !debug then lprintf_nl "lookup %s" (show_id target);
  let start = BasicSocket.last_time () in
  let module S = LimitedSet.Make(struct
    type t = id * addr
    let compare n1 n2 = Big_int.compare_big_int (distance target (fst n1)) (distance target (fst n2))
  end) in
  let found = S.create Kademlia.bucket_nodes in
  let queried = Hashtbl.create 13 in
  let active = ref 0 in
  let check_ready () =
    if 0 = !active then
    begin
      let result = S.elements found in
      if !debug then lprintf_nl "lookup_node %s done, queried %d, found %d, elapsed %ds" 
        (show_id target) (Hashtbl.length queried) (List.length result) (BasicSocket.last_time () - start);
      k result
    end
  in
  let rec round nodes =
    let inserted = List.fold_left (fun acc node -> if S.insert found node then acc + 1 else acc) 0 nodes in
    begin try
      let n = ref 0 in
      S.iter found (fun node ->
        if alpha = !n then raise Break;
        if not (Hashtbl.mem queried node) then begin incr n; query true node end)
    with Break -> () end;
    inserted
  and query store (id,addr as node) =
    incr active;
    Hashtbl.add queried node true;
    if !debug then lprintf_nl "will query node %s" (show_node node);
    M.find_node dht addr target begin fun (id,addr as node) nodes ->
      if store then update dht Good id addr;
      decr active;
      let inserted = round nodes in
      let s = try sprintf ", best %s" (show_id (fst (S.min_elt found))) with _ -> "" in
      if !debug then lprintf_nl "got %d nodes from %s, useful %d%s" (List.length nodes) (show_node node) inserted s;
      check_ready ()
    end ~kerr:(fun () -> decr active; if !debug then lprintf_nl "timeout from %s" (show_node node); check_ready ())
  in
  begin match nodes with
  | None -> let (_:int) = round (M.self_find_node dht target) in ()
  | Some l -> List.iter (query false) l
  end;
  check_ready ()

let show_torrents torrents =
  let now = BasicSocket.last_time () in
  Hashtbl.iter (fun h peers ->
    let l = M.peers_list (fun addr tm -> sprintf "%s (exp. %ds)" (show_addr addr) (tm - now)) peers in
    lprintf_nl "torrent %s : %s" (H.to_hexa h) (String.concat " " l))
  torrents

let show dht = show_table dht.M.rt; show_torrents dht.M.torrents
let stat dht =
  buckets dht.M.rt,
  size dht.M.rt,
  Hashtbl.length dht.M.torrents,
  Hashtbl.fold (fun _ peers acc -> acc + Peers.fold (fun _ _ acc -> acc + 1) peers 0) dht.M.torrents 0
let rpc_stats dht = let (_,st,_) = dht.M.rpc in KRPC.show_stats st

let bootstrap dht host addr k =
  M.ping dht addr begin function
    | Some node ->
      if !verb then lprintf_nl "bootstrap node %s (%s) is up" (show_node node) host;
      lookup_node dht ~nodes:[node] dht.M.rt.self (fun l ->
        if !debug then lprintf_nl "bootstrap via %s (%s) : found %s" (show_addr addr) host (strl show_node l);
        k (List.length l >= Kademlia.bucket_nodes))
    | None ->
      if !verb then lprintf_nl "bootstrap node %s (%s) is down" (show_addr addr) host;
      k false
  end

let bootstrap dht (host,port) k =
  Ip.async_ip host
    (fun ip -> bootstrap dht host (ip,port) k)
    (fun () -> if !verb then lprintf_nl "boostrap node %s cannot be resolved" host; k false)

let bootstrap ?(routers=[]) dht =
  lookup_node dht dht.M.rt.self begin fun l ->
    if !debug then lprintf_nl "auto bootstrap : found %s" (strl show_node l);
    let rec loop l ok =
      match ok,l with
      | true,_ -> if !verb then lprintf_nl "bootstrap ok, total nodes : %d" (size dht.M.rt)
      | false,[] -> if !verb then lprintf_nl "boostrap failed, total nodes : %d" (size dht.M.rt)
      | false,(node::nodes) -> bootstrap dht node (loop nodes)
    in
    loop routers (List.length l >= Kademlia.bucket_nodes)
  end

let query_peers dht id k =
  if !debug then lprintf_nl "query_peers: start %s" (H.to_hexa id);
  lookup_node dht id (fun nodes ->
    if !debug then lprintf_nl "query_peers: found nodes %s" (strl show_node nodes);
(*
    let found = ref Peers.empty in
    let check =
      let left = ref (List.length nodes + 1) (* one immediate check *) in
      fun () -> decr left; if 0 = !left then k (Peers.fold (fun peer () l -> peer :: l) !found [])
    in
*)
    List.iter begin fun node ->
      M.get_peers dht (snd node) id begin fun node token peers nodes ->
        if !debug then lprintf_nl "query_peers: got %d peers and %d nodes from %s with token %S" 
          (List.length peers) (List.length nodes) (show_node node) token;
        k node token peers;
(*
        found := List.fold_left (fun acc peer -> Peers.add peer () acc) !found peers;
        check ()
*)
        end
        ~kerr:(fun () -> if !debug then lprintf_nl "query_peers: get_peers error from %s" (show_node node)(*; check ()*));
(*       check () *)
    end nodes)

let start rt port bw_control =
  let secret = Secret.create secret_timeout in
  let rec dht = lazy (M.create rt port bw_control answer)
  and answer addr name args =
    let (id,q) = parse_query_exn name args in
    let node = (id,addr) in
    if !debug then lprintf_nl "DHT query from %s : %s" (show_node node) (show_query q);
    update !!dht Good id addr;
    stats_add (!!dht).M.stats (M.query_type_of_query q, `In) 1;
    let response =
      match q with
      | Ping -> Ack
      | FindNode h -> Nodes (M.self_find_node !!dht h)
      | GetPeers h -> 
        let token = make_token addr h (Secret.get secret) in
        let peers = M.self_get_peers !!dht h in
        let nodes = M.self_find_node !!dht h in
        if !debug then lprintf_nl "answer with %d peers and %d nodes" (List.length peers) (List.length nodes);
        Peers (token,peers,nodes)
      | Announce (h,port,token) ->
        if not (valid_token addr h secret token) then failwith ("invalid token " ^ token);
        M.store !!dht h (fst addr, port);
        Ack
    in
    if !debug then lprintf_nl "DHT response to %s : %s" (show_node node) (show_response response);
    make_response (!!dht).M.rt.self response
  in
  let refresh () =
    let ids = Kademlia.refresh (!!dht).M.rt in
    if !debug then lprintf_nl "will refresh %d buckets" (List.length ids);
    let cb prev_id (id,addr as node) l =
      update !!dht Good id addr; (* replied *)
      if prev_id <> id then
      begin
        if !debug then lprintf_nl "refresh: node %s changed id (was %s)" (show_node node) (show_id prev_id);
        update !!dht Bad prev_id addr;
      end;
      if !debug then lprintf_nl "refresh: got %d nodes from %s" (List.length l) (show_node node);
      List.iter (fun (id,addr) -> update !!dht Unknown id addr) l
    in
    List.iter (fun (target, nodes) ->
      List.iter (fun (id,addr) -> M.find_node !!dht addr target (cb id) ~kerr:(fun () -> ())) nodes)
    ids
  in
  if !debug then lprintf_nl "DHT size : %d self : %s" (size (!!dht).M.rt) (show_id (!!dht).M.rt.self); 
  BasicSocket.add_session_timer (!!dht).M.enabler 60. refresh;
  !!dht

let stop dht = M.shutdown dht

