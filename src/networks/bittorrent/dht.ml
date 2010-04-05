
type 'a pr = ?exn:exn -> ('a, unit, string, unit) format4 -> 'a

class logger prefix = 
  let print_log prefix level ?exn fmt =
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
method debug : 'a. 'a pr = fun ?exn fmt -> print_log prefix 0 ?exn fmt
method info  : 'a. 'a pr = fun ?exn fmt -> print_log prefix 1 ?exn fmt
method warn  : 'a. 'a pr = fun ?exn fmt -> print_log prefix 2 ?exn fmt
method error : 'a. 'a pr = fun ?exn fmt -> print_log prefix 3 ?exn fmt
end

let log = new logger "dht"

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
  | "e" -> begin match list x with B.Int n :: B.String s :: _ -> Error (n, s) | _ -> failwith "decode e" end
  | _ -> failwith "type"
  in (txn, v)
  with
  exn -> raise (Protocol_error (txn,"Invalid argument"))

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
  let handle ((ip,port) as addr) txn v =
    match v with
    | Error (code,msg) ->
        A.remove h addr txn;
        log #info "dht error %Ld : %S from %s:%u" code msg (Ip.to_string ip) port
    | Query (name,args) -> 
        let ret = answer name args in ()
    | Response ret -> ()
  in
  let make_addr sockaddr = 
    try match sockaddr with Unix.ADDR_INET (inet,port) -> Some (Ip.of_inet_addr inet, port) | _ -> None with _ -> None
  in
  let handle p =
    match make_addr p.udp_addr with
    | None -> ()
    | Some addr ->
      try
        let (txn,v) = decode_exn pkt in
        handle addr p.udp_content 
      with exn ->
        log #warn ~exn "dht handle packet"; 
        let (code,str) = match exn with
        | Protocol_error x -> (203,x)
        | Method_unknown x -> (204,x)
        | _ -> (202,"")
        in
        write socket false (encode (Error (code,str))) (fst addr) (snd addr)
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

(*
type ping : ping -> pong
type ping : unit -> ping * (pong -> unit)
  | Ping of Kademlia.id
  | Pong of Kademlia.id
*)



