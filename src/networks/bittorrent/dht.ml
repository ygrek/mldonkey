
open Kademlia
open Printf

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

(* 2-level association *)
module Assoc2 : sig

  type ('a,'b,'c) t
  val create : unit -> ('a,'b,'c) t
  val add : ('a,'b,'c) t -> 'a -> 'b -> 'c -> unit
  val find_all : ('a,'b,'c) t -> 'a -> ('b,'c) Hashtbl.t
  val find : ('a,'b,'c) t -> 'a -> 'b -> 'c option
  val remove : ('a,'b,'c) t -> 'a -> 'b -> unit

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
  let x = ("t", B.String txn) :: x in
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
  try
  let v = match str x.("y") with
  | "q" -> Query (str x.("q"), dict x.("a"))
  | "r" -> Response (dict x.("r"))
  | "e" -> begin match list x.("e") with B.Int n :: B.String s :: _ -> Error (n, s) | _ -> failwith "decode e" end
  | _ -> failwith "type"
  in (txn, v)
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
  log #info "KRPC to %s : %S" (show_addr addr) s;
  write sock false s ip port

let create port answer =
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
  (* FIXME Expire timeouted entries *)
  let handle addr (txn,v) =
    log #info "KRPC from %s txn %S : %s" (show_addr addr) txn (show_msg v);
    match v with
    | Error _ ->
        A.remove h addr txn
    | Query (name,args) -> 
        let ret = answer addr name args in
        send socket addr (txn, ret)
    | Response ret ->
        match A.find h addr txn with
        | None -> log #warn "no txn %S for %s" txn (show_addr addr)
        | Some k -> A.remove h addr txn; k addr ret
  in
  let make_addr sockaddr = 
    try match sockaddr with Unix.ADDR_INET (inet,port) -> Some (Ip.of_inet_addr inet, port) | _ -> None with _ -> None
  in
  let handle p =
    match make_addr p.udp_addr with
    | None -> ()
    | Some addr ->
      let ret = ref None in
      try
        log #info "recv %S" p.udp_content;
        let r = decode_exn p.udp_content in
        ret := Some r;
        handle addr r
      with exn ->
        log #warn ~exn "dht handle packet"; 
        let error txn code str = send socket addr (txn,(Error (Int64.of_int code,str))) in
        match exn,!ret with
        | Malformed_packet x, Some (txn, _)
        | Protocol_error ("",x), Some(txn, _) | Protocol_error (txn,x), _ -> error txn 203 x
        | Method_unknown x, Some (txn, _) -> error txn 204 x
        | _, Some (txn, Query _) -> error txn 202 ""
        | _ -> ()
  in
  udp_set_reader socket handle;
  (socket,h)

let write (socket,h) msg addr k =
  let tt = Assoc2.find_all h addr in
  let rec loop () = (* choose txn FIXME *)
    let txn = string_of_int (Random.int max_int) in
    match Hashtbl.mem tt txn with
    | true -> loop ()
    | false -> txn
  in
  let txn = loop () in
  Assoc2.add h addr txn k;
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
| Peers (token,peers,nodes) -> sprintf "peers token=%s [%s] [%s]" token (strl show_addr peers) (strl show_node nodes)

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
       ])

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
  match x with (* FIXME *)
  | Ack -> KRPC.Response [self]
  | Nodes _ -> KRPC.Response [self]
  | Peers _ -> KRPC.Response [self]

module Test = struct

open Bencode

let e = Dictionary ["t",String "aa"; "y", String "e"; "e", List [Int 201L; String "A Generic Error Occurred"] ]
let s = "d1:eli201e24:A Generic Error Occurrede1:t2:aa1:y1:ee"
let v = "aa", KRPC.Error (201L, "A Generic Error Occurred")

let () = 
  assert (encode e = s);
  assert (KRPC.decode_exn s = v);
  assert (KRPC.encode v = s);
  ()

end

let find_node t h = []
let get_peers t h = []

let main t =
  let exit () = 
    store "dht.dat" t;
    exit 0
  in
  log #info "DHT size : %d self : %s" (size t) (show_id t.self); 
(*   let bootstrap = [Ip.of_string "router.bittorrent.com", 6881; Ip.of_string "router.utorrent.com", 6881;] in *)
  let bootstrap = Ip.of_string "67.215.242.139", 6881 in
  let _bootstrap = Ip.of_string "127.0.0.1", 12345 in
  let answer addr name args =
    try
    let (id,q) = parse_query_exn name args in
    log #info "DHT query from %s (%s) : %s" (show_addr addr) (show_id id) (show_query q);
    let response =
      match q with
      | Ping -> Ack
      | FindNode h -> Nodes (find_node t h)
      | GetPeers h -> let token = "FIXME" in Peers (token,get_peers t h,find_node t h)
      | Announce (token,peers,port) -> Ack
    in
    make_response t.self response
    with
    exn -> log #warn ~exn "query %s from %s" name (show_addr addr); raise exn
  in
  let dht = KRPC.create 12346 answer in
  let dht_query q k =
    KRPC.write dht (make_query t.self q) bootstrap begin fun addr dict ->
      try
        let (id,r) = parse_response_exn q dict in
        log #info "DHT response from %s (%s) : %s" (show_addr addr) (show_id id) (show_response r);
        k id addr r
      with
        exn -> log #warn ~exn "DHT response to %s from %s" (show_query q) (show_addr addr); raise exn
    end
  in
  dht_query Ping begin fun id addr r ->
    match r with
    | Ack -> log #info "ack"
    | _ -> log #warn "whut?"
  end;
  dht_query (FindNode t.self) begin fun _ _ r ->
    match r with
    | Nodes l -> 
      log #info "Got %d nodes" (List.length l);
      List.iter (fun (id,addr) -> insert t (new_node id addr)) l;
      exit ()
    | _ -> log #warn "whut?"
  end;
  BasicSocket.loop ()

let main () =
  bracket (init "dht.dat") (store "dht.dat") main

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  try
    main ()
  with
    exn -> log #error "main : %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())

