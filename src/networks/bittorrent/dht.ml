
open Printf2

type dict = (string * Bencode.value) list

module KRPC = struct

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

let decode_exn s =
  let module B = Bencode in
  let module Array = struct let get x k = match x with B.Dictionary l -> List.assoc k l | _ -> failwith "decode get" end in
  let x = B.decode s in
  let v = match str x.("y") with
  | "q" -> Query (str x.("q"), dict x.("a"))
  | "r" -> Response (dict x.("r"))
  | "e" -> begin match list x with B.Int n :: B.String s :: _ -> Error (n, s) | _ -> failwith "decode e" end
  | _ -> failwith "decode"
  in (str x.("t"), v)

open BasicSocket
open UdpSocket

let udp_set_reader socket f =
  set_reader socket begin fun _ ->
    try f () with exn -> 
      lprintf_nl "udp interact exn %s" (Printexc2.to_string exn);
      close socket (Closed_for_exception exn)
  end

let create () =
  let socket = create Unix.inet_addr_any 0 (fun sock event ->
(*       lprintf_nl "udpt got event %s for %s" (string_of_event event) host; *)
      match event with
      | WRITE_DONE | CAN_REFILL -> ()
      | READ_DONE -> assert false (* set_reader prevents this *)
      | BASIC_EVENT x -> match x with
        | CLOSED _ -> ()
        | CAN_READ | CAN_WRITE -> assert false (* udpSocket implementation prevents this *)
        | LTIMEOUT | WTIMEOUT | RTIMEOUT -> close sock (Closed_for_error "KRPC timeout"))
  in
  set_wtimeout (sock socket) 5.;
  set_rtimeout (sock socket) 5.;
  let h = Hashtbl.create 13 in
  udp_set_reader socket (fun () ->
    let p = read socket in
    
    ());
  (socket,h)

let write (socket,h) msg ip port k =
  let l = try Hashtbl.find h (ip,port) with Not_found -> [] in
  let rec loop () =
    let txn = Random.int max_int in
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



