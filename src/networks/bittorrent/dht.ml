
open Printf2

type dict = (string * Bencode.value) list

module KRPC = struct

type msg = 
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

let encode txn msg =
  let module B = Bencode in
  let x = match msg with
  | Query (name,args) -> ["y", B.String "q"; "q", B.String name; "a", B.Dictionary args]
  | Response dict -> ["y", B.String "r"; "r", B.Dictionary dict]
  | Error (code,text) -> ["y", B.String "e"; "e", B.List [B.Int code; B.String text] ]
  in
  let x = ("t", B.String txn) :: x in
  B.encode (B.Dictionary x)

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

let write (socket,_) msg ip port =
  write socket false msg ip port

end

(*
type ping : ping -> pong
type ping : unit -> ping * (pong -> unit)
  | Ping of Kademlia.id
  | Pong of Kademlia.id
*)



