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

type addr = Unix.inet_addr
type socket = Unix.file_descr
type group = int
type port = int
  
(* [deering_addr group] creates an INET address for the multicast group
224.0.0.[group], with [group] between 0 and 247. *)
val deering_addr: group -> addr
  
(* [create ()] creates a datagram socket. *)  
val create: unit -> socket
  
(* [bind sock port] binds a datagram socket to the port [port] *)
val bind: socket -> port -> unit
  
(* [join sock group] makes the socket [sock] join the group 224.0.0.[group] *)
val join: socket ->  group -> unit

(* [leave sock group] makes the socket [sock] leave the group 224.0.0.[group] *)
val leave: socket -> group -> unit
  
(* [server group port] creates a new datagram socket, binds it to [port] and
  join the multicast group 224.0.0.[group]. *)
val server: group -> port -> socket
  
(* [recv sock buffer] waits for a message on [sock] and stores it in [buffer] *)
val recv: socket -> string -> int * Unix.sockaddr
  
(* [send sock group port message] sends a message [message] on [sock] to 
  the group 224.0.0.[group] on port [port]. *)
val send: socket -> group -> port -> string -> unit

val send_and_recv: group -> port -> string -> 'a