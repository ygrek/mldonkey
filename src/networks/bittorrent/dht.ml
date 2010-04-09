
open Kademlia

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
  Printf.ksprintf put fmt
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

type msg = 
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

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

let create answer =
  let socket = create Unix.inet_addr_any 0 (fun sock event ->
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
  let handle ((ip,port) as addr) (txn,v) =
    match v with
    | Error (code,msg) ->
        A.remove h addr txn;
        log #info "dht error %Ld : %S from %s" code msg (show_addr addr)
    | Query (name,args) -> 
        let ret = answer name args in
        write socket false (encode (txn, Response ret)) ip port
    | Response ret ->
        match A.find h addr txn with
        | None -> log #warn "no txn %S for %s" txn (show_addr addr)
        | Some k -> A.remove h addr txn; k ret
  in
  let make_addr sockaddr = 
    try match sockaddr with Unix.ADDR_INET (inet,port) -> Some (Ip.of_inet_addr inet, port) | _ -> None with _ -> None
  in
  let handle p =
    match make_addr p.udp_addr with
    | None -> ()
    | Some (ip,port as addr) ->
      let ret = ref None in
      try
        let r = decode_exn p.udp_content in
        ret := Some r;
        handle addr r
      with exn ->
        log #warn ~exn "dht handle packet"; 
        let error txn code str = write socket false (encode (txn,(Error (Int64.of_int code,str)))) ip port in
        match exn,!ret with
        | Malformed_packet x, Some (txn, _)
        | Protocol_error (txn,x), _ -> error txn 203 x
        | Method_unknown x, Some (txn, _) -> error txn 204 x
        | _, Some (txn, Query _) -> error txn 202 ""
        | _ -> ()
  in
  udp_set_reader socket handle;
  (socket,h)

let write (socket,h) msg ip port k =
  let l = try Hashtbl.find h (ip,port) with Not_found -> [] in
  let rec loop () =
    let txn = string_of_int (Random.int max_int) in
    match List.mem_assoc txn l with
    | true -> loop ()
    | false -> txn
  in
  let txn = loop () in
  Hashtbl.add h (ip,port) ((txn,k) :: l);
  let s = encode (txn,msg) in
  write socket false s ip port

end

type query =
| Ping
| FindNode of id
| GetPeers of H.t
| Announce of H.t * int * string

type response =
| Ack
| Nodes of id list
| Peers of string * addr list * id list

let (&) f x = f x

let parse_query_exn name args =
  let get k = List.assoc k args in
  let sha1 k = H.direct_of_string & KRPC.str & get k in
  let p = match name with
  | "ping" -> Ping
  | "find_node" -> FindNode (sha1 "target")
  | "get_peers" -> GetPeers (sha1"info_hash")
  | "announce_peer" -> Announce (sha1 "info_hash", Int64.to_int & KRPC.int & get "port", KRPC.str & get "token")
  | s -> failwith (Printf.sprintf "parse_query name=%s" name)
  in
  sha1 "id", p

let parse_nodes s =
  assert (String.length s mod 26 = 0);
  [] (* FIXME *)

let parse_peer s =
  assert (String.length s = 6);
  Ip.of_string "0.0.0.0", 0

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


