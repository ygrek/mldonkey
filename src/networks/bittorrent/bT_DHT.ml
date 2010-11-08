(** 
  DHT

  http://www.bittorrent.org/beps/bep_0005.html
*)

open Kademlia
open Printf

let dht_query_timeout = 20
let store_peer_timeout = minutes 30
let secret_timeout = minutes 10
(* let alpha = 3 *)

type 'a pr = ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
type level = [ `Debug | `Info | `Warn | `Error ]

class logger prefix = 
  let int_level = function
    | `Debug -> 0
    | `Info -> 1
    | `Warn -> 2
    | `Error -> 3
  in
  let print_log limit prefix level ?exn fmt =
    let put s =
      let b = match level with 
      | 0 -> false 
      | _ -> true
      in 
      match b,exn with
      | false, _ -> ()
      | true, None -> Printf2.lprintf_nl "[%s] %s" prefix s
      | true, Some exn -> Printf2.lprintf_nl "[%s] %s : exn %s" prefix s (Printexc2.to_string exn)
  in
  ksprintf put fmt
in
object
val mutable limit = int_level `Info
method debug : 'a. 'a pr = fun ?exn fmt -> print_log limit prefix 0 ?exn fmt
method info  : 'a. 'a pr = fun ?exn fmt -> print_log limit prefix 1 ?exn fmt
method warn  : 'a. 'a pr = fun ?exn fmt -> print_log limit prefix 2 ?exn fmt
method error : 'a. 'a pr = fun ?exn fmt -> print_log limit prefix 3 ?exn fmt
method allow (level:level) = limit <- int_level level
end

let log = new logger "dht"

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

end

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
  exn -> log #warn ~exn "err"; raise (Protocol_error (txn,"Invalid argument"))

open BasicSocket
open UdpSocket

let udp_set_reader socket f =
  set_reader socket begin fun _ ->
    try read_packets socket f with exn -> 
      log #warn ~exn "udp reader";
      close socket (Closed_for_exception exn)
  end

module A = Assoc2

let send sock (ip,port as addr) txnmsg =
  let s = encode txnmsg in
  log #debug "KRPC to %s : %S" (show_addr addr) s;
  write sock false s ip port

type t = UdpSocket.t * (addr, string, (addr -> dict -> unit) * (unit -> unit) * int) A.t

let create port answer : t =
  let socket = create Unix.inet_addr_any port (fun sock event ->
      match event with
      | WRITE_DONE | CAN_REFILL -> ()
      | READ_DONE -> assert false (* set_reader prevents this *)
      | BASIC_EVENT x -> match x with
        | CLOSED _ -> ()
        | CAN_READ | CAN_WRITE -> assert false (* udpSocket implementation prevents this *)
        | LTIMEOUT | WTIMEOUT | RTIMEOUT -> () (*close sock (Closed_for_error "KRPC timeout")*))
  in
  set_wtimeout (sock socket) 5.;
  set_rtimeout (sock socket) 5.;
  let h = A.create () in
  let timeout h =
    let now = last_time () in
    let bad = ref [] in
    let total = ref 0 in
    A.iter h (fun addr txn (_,kerr,t) -> incr total; if t < now then bad := (addr,txn,kerr) :: !bad);
    log #info "timeouted %d of %d DHT queries" (List.length !bad) !total;
    List.iter (fun (addr,txn,kerr) ->
      A.remove h addr txn;
      try kerr () with exn -> log #warn ~exn "timeout for %s" (show_addr addr)) !bad;
  in
  BasicSocket.add_infinite_timer 5. (fun () -> timeout h);
  let handle addr (txn,ver,msg) =
    let version = match ver with Some s -> sprintf "client %S " s | None -> "" in
    log #debug "KRPC from %s %stxn %S : %s" (show_addr addr) version txn (show_msg msg);
    match msg with
    | Error _ ->
        begin match A.find h addr txn with
        | None -> log #warn "no txn %S for %s %s (error received)" txn (show_addr addr) version
        | Some (_, kerr, _) -> A.remove h addr txn; kerr ()
        end
    | Query (name,args) ->
        let ret = answer addr name args in
        send socket addr (txn, ret)
    | Response ret ->
        match A.find h addr txn with
        | None -> log #warn "no txn %S for %s %s" txn (show_addr addr) version
        | Some (k,_,_) -> A.remove h addr txn; k addr ret
  in
  let handle p =
    match p.udp_addr with
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (inet_addr,port) ->
      let addr = (Ip.of_inet_addr inet_addr, port) in
      let ret = ref None in
      try
        log #debug "recv %S" p.udp_content;
        let r = decode_exn p.udp_content in
        ret := Some r;
        handle addr r
      with exn ->
        let version = match !ret with Some (_,Some s,_) -> sprintf "client %S " s | _ -> "" in
        log #warn ~exn "dht handle packet from %s %s" (show_addr addr) version;
        let error txn code str = send socket addr (txn,(Error (Int64.of_int code,str))) in
        match exn,!ret with
        | Malformed_packet x, Some (txn, _, _)
        | Protocol_error ("",x), Some(txn, _, _) | Protocol_error (txn,x), _ -> error txn 203 x
        | Method_unknown x, Some (txn, _, _) -> error txn 204 x
        | _, Some (txn, _, Query _) -> error txn 202 ""
        | _ -> ()
  in
  udp_set_reader socket handle;
  (socket,h)

let write (socket,h) msg addr k ~kerr =
  let tt = Assoc2.find_all h addr in
  let rec loop () = (* choose txn FIXME *)
    let txn = string_of_int (Random.int max_int) in
    match Hashtbl.mem tt txn with
    | true -> loop ()
    | false -> txn
  in
  let txn = loop () in
  Assoc2.add h addr txn (k,kerr,last_time () + dht_query_timeout);
  send socket addr (txn,msg)

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

let strl f l = String.concat " " & List.map f l

let show_node (id,addr) = sprintf "%s (%s)" (show_addr addr) (show_id id)

let show_response = function
| Ack -> "ack"
| Nodes l -> sprintf "nodes [%s]" (strl show_node l)
| Peers (token,peers,nodes) -> sprintf "peers token=%S [%s] [%s]" token (strl show_addr peers) (strl show_node nodes)

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
  let s = String.create 6 in
  let set i c = s.[i] <- char_of_int c in
  set 0 a; set 1 b; set 2 c; set 3 d; set 4 e; set 5 f;
  s

let make_nodes nodes =
  let s = String.create (26 * List.length nodes) in
  let i = ref 0 in
  List.iter (fun (id,addr) ->
    String.blit (H.direct_to_string id) 0 s (!i*26) 20;
    String.blit (make_peer addr) 0 s (!i*26+20) 6;
    incr i
    ) nodes;
  s

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

type t = { rt : Kademlia.table; storage : string; dht : KRPC.t; torrents : (H.t, int Peers.t) Hashtbl.t; }

let dht_query t addr q k ~kerr =
  log #info "DHT query to %s : %s" (show_addr addr) (show_query q);
  KRPC.write t.dht (make_query t.rt.self q) addr begin fun addr dict ->
    let (id,r) = try parse_response_exn q dict with exn -> kerr (); raise exn in
    log #info "DHT response from %s (%s) : %s" (show_addr addr) (show_id id) (show_response r);
    k (id,addr) r
  end ~kerr

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

let manage_timeouts h =
  BasicSocket.add_infinite_timer 60. begin fun () ->
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
    log #info "Removed %d of %d peers for announced torrents" !rm !total
  end

let create storage dht_port answer =
  let rt = Kademlia.init storage in
  let dht = KRPC.create dht_port answer in
  let torrents = Hashtbl.create 8 in
  manage_timeouts torrents;
  { rt = rt; storage = storage; dht = dht; torrents = torrents; }

let shutdown dht =
  (* FIXME destroy timers and sockets *)
  Kademlia.store dht.storage dht.rt

let peers_list f m = Peers.fold (fun peer tm l -> (f peer tm)::l) m []
let self_get_peers t h = peers_list (fun a _ -> a) (try Hashtbl.find t.torrents h with Not_found -> Peers.empty)
let self_find_node t h = List.map (fun node -> node.id, node.addr) & Kademlia.find_node t.rt h

end

module Secret : sig 

type t
val create : time -> t
val get : t -> string
val valid : t -> string -> bool

end = struct

type t = { mutable cur : string; mutable prev : string; timeout : time; mutable next : time; }
let make () = string_of_int (Random.int max_int)
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
let valid t s =
  invalidate t;
  s = t.cur || s = t.prev

end

let make_token addr h secret =
  H.to_hexa (H.xor (H.string (show_addr addr)) h) ^ "/" ^ (Secret.get secret)

let valid_token addr h secret token =
  try
    Scanf.sscanf token "%40s/%s%!" (fun s1 s2 ->
      Secret.valid secret s2 && H.to_hexa (H.xor (H.string (show_addr addr)) h) = s1)
  with
  _ -> false

module LimitedSet = struct

module type S = sig

type elt
type t
val create : int -> t
(** @return whether the element was really added *)
val insert : t -> elt -> bool
val elements : t -> elt list

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

let elements (_,set) = S.elements !set

end (* Make *)

end (* LimitedSet *)

let update dht ?st id addr = update (M.ping dht) dht.M.rt ?st id addr

let lookup_node dht ?nodes target k =
  log #info "lookup %s" (show_id target);
  let module S = LimitedSet.Make(struct
    type t = id * addr
    let compare n1 n2 = Big_int.compare_big_int (distance target (fst n1)) (distance target (fst n2))
  end) in
  let found = S.create Kademlia.bucket_nodes in
  let queried = Hashtbl.create 13 in
  let active = ref 0 in
  let check_ready () = if 0 = !active then k (S.elements found) in
  (* FIXME use alpha *)
  let rec query (id,addr as node) =
    if not (Hashtbl.mem queried node) then
    begin
      incr active;
      Hashtbl.add queried node true;
      log #info "will query node %s" (show_node node);
      M.find_node dht addr target begin fun (id,addr as node) l ->
        update dht id addr;
        let (_:bool) = S.insert found node in
        decr active;
        log #info "got %d nodes from %s" (List.length l) (show_node node);
        List.iter query l;
        check_ready ()
      end ~kerr:(fun () -> decr active; log #info "timeout from %s" (show_node node); check_ready ())
    end
  in
  let nodes = match nodes with
  | None -> M.self_find_node dht target
  | Some l -> l
  in
  List.iter query nodes;
  check_ready ()

let show_torrents dht =
  let now = BasicSocket.last_time () in
  Hashtbl.iter (fun h peers ->
    let l = M.peers_list (fun addr tm -> sprintf "%s (exp. %ds)" (show_addr addr) (tm - now)) peers in
    log #info "torrent %s : %s" (H.to_hexa h) (String.concat " " l))
  dht.M.torrents

let finish dht =
  log #info "finish";
  M.shutdown dht

let bootstrap dht addr =
  M.ping dht addr begin function
    | Some node ->
      log #info "bootstrap node is up - %s" (show_node node);
      lookup_node dht ~nodes:[node] dht.M.rt.self (fun l ->
        log #info "bootstrap via %s : found %s" (show_addr addr) (strl show_node l))
    | None ->
      log #warn "bootstrap node %s is down" (show_addr addr)
  end

let bootstrap ?(routers=[]) dht =
  lookup_node dht dht.M.rt.self begin fun l ->
    log #info "auto bootstrap : found [%s]" (strl show_node l);
    if List.length l < Kademlia.bucket_nodes then
      List.iter (bootstrap dht) routers
  end

let start storage port =
  let secret = Secret.create secret_timeout in
  let rec dht = lazy (M.create storage port answer)
  and answer addr name args =
    try
    let (id,q) = parse_query_exn name args in
    let node = (id,addr) in
    log #info "DHT query from %s : %s" (show_node node) (show_query q);
    update !!dht id addr;
    let response =
      match q with
      | Ping -> Ack
      | FindNode h -> Nodes (M.self_find_node !!dht h)
      | GetPeers h -> 
        let token = make_token addr h secret in
        let peers = M.self_get_peers !!dht h in
        let nodes = M.self_find_node !!dht h in
        log #info "answer with %d peers and %d nodes" (List.length peers) (List.length nodes);
        Peers (token,peers,nodes)
      | Announce (h,port,token) ->
        if not (valid_token addr h secret token) then failwith "bad token in announce";
        M.store !!dht h (fst addr, port);
        Ack
    in
    log #info "DHT response to %s : %s" (show_node node) (show_response response);
    make_response (!!dht).M.rt.self response
    with
    exn -> log #warn ~exn "query %s from %s" name (show_addr addr); raise exn
  in
  let refresh () =
    let ids = Kademlia.refresh (!!dht).M.rt in
    log #info "will refresh %d buckets" (List.length ids);
    let cb (id,addr as node) l =
      update !!dht id addr; (* replied *)
      log #info "refresh: got %d nodes from %s" (List.length l) (show_node node);
      List.iter (fun (id,addr) -> update !!dht ~st:Unknown id addr) l
    in
    List.iter (fun target ->
      let nodes = M.self_find_node !!dht target in
      List.iter (fun (_,addr) -> M.find_node !!dht addr target cb ~kerr:(fun () -> ())) nodes)
    ids
  in
  log #info "DHT size : %d self : %s" (size (!!dht).M.rt) (show_id (!!dht).M.rt.self); 
  BasicSocket.add_infinite_timer 60. refresh;
  !!dht

