(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Unix

type addr = Unix.inet_addr
type socket = Unix.file_descr
type group = int
type port = int
  
external setsock_multicast: socket -> int = "setsock_multicast"
external setsock_join: socket -> addr -> int = "setsock_join"
external setsock_leave: socket -> addr -> int = "setsock_leave"
  
let socket_dgram ()     = socket Unix.PF_INET Unix.SOCK_DGRAM 0
let setsock_reuse sock = Unix.setsockopt sock Unix.SO_REUSEADDR true
  
let deering_prefix = "233.252.252."

let deering_addr i =
  let i = (abs i) mod 248 in
  let inet_s = Printf.sprintf  "%s%d" deering_prefix i in
  Unix.inet_addr_of_string inet_s

let create () =
  let sock = socket_dgram () in
  setsock_reuse sock;
  sock

let bind sock port = Unix.bind sock (Unix.ADDR_INET(Unix.inet_addr_any, port)) 
  
let join sock deering =
  let multicast_addr = deering_addr deering in
  let e = setsock_multicast sock in
  if e < 0 then failwith "setsock_multicast";
  let e = setsock_join sock multicast_addr in
  if e < 0 then failwith "setsock_join"
    
let leave sock deering =
  let multicast_addr = deering_addr deering in
  let e = setsock_leave sock multicast_addr in
  if e < 0 then failwith "setsock_leave"

let server deering port =
  let sock = create () in
  bind sock port;
  join sock deering;
  sock

let recv sock buffer =
  recvfrom sock buffer 0 (String.length buffer) []
  
let send sock deering port message =
  let multicast_addr = deering_addr deering in
  Printf.printf "ADDR: %s" (string_of_inet_addr multicast_addr);
  print_newline ();
  let to_addr = Unix.ADDR_INET(multicast_addr, port) in
  let e = sendto sock message 0 (String.length message) [] to_addr in
  if e < 0 then failwith "sendto"
    
let send_and_recv group port message =
  let sock = server group port in
  send sock group port message;
  let buffer = String.create 100 in
  try
    while true do
      let rs, _, _ = select [sock] [] [] 5.0 in
      match rs with
        [] -> raise Not_found
      | _ -> 
          ignore (recv sock buffer); 
          if String.sub buffer 0 (String.length message) <> message then
            raise Exit
    done;
    raise Exit
  with 
  | Exit -> 
      close sock;
      let msg = buffer in
      Marshal.from_string buffer 0
  | e -> close sock; raise e
      
      
      