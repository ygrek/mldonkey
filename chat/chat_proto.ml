(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Protocol for communicating, a class
   implementing send and receive with 
   Unix unconnected sockets and a class to implement it with connected sockets.

   The protocol is the following. A message is:
   <version>\n
   <source_id>\n
   <source_hidden_id>\n
   <source_host>\n
   <source_port>\n
   <dest_id>\n
   <dest_hidden_id>\n
   <message_type>\n
   [<message_room_name>]\n
   [<message_room_people_number>]\n
   [(<message_room_people_id>\n
     <message_room_people_host>\n
     <message_room_people_port>\n)+]
   [<message_body_length>]\n
   [<message_body>]
   [<id>\n
    <hidden_id>\n
    <host>\n
    <port>]

   where <message_type> can be
   - HELLO_OK
   - HELLO
   - BYEBYE
   - MESSAGE
   - ADD_OPEN
   - ROOM_MESSAGE

   <message_body_length> appears only when the <message_type> is MESSAGE or ROOM_MESSAGE.
   <message_body> appears only after the <message_body_length>.
   [<id>\n
    <hidden_id>\n
    <host>\n
    <port>] appears only for a ADD_OPEN <message_type>
 *)

type port = int
type host = string
type address = host * port

type message = string
type version = string
type id = string

type source = version * id * address
type dest = id 


type proto = 
  | HelloOk (** Reply to an ok message *)
  | Hello  (** to signal that we are connected *)
  | Byebye  (** to signal that we are disconnecting *)
  | Message of message
  | AddOpen of id * address (** to remotely add a user *)
  | RoomMessage of id * (id * host * port) list * message 
      (** id of the room, people in the room and message *)


type packet = source * dest * proto (** source, destination, proto *)

let version = Chat_messages.software_version

let il input = 
  let l = input () in
  Chat_misc.remove_blanks l

(** read a packet with the given functions. 
   @raise Failure if an error occurs (bad format, enf of file, ...). *)
let read_packet getline input =
  try
    let v = il getline in
    let source_id = getline () in
    let source_host = getline () in
    let source_port = int_of_string (il getline) in
    let dest_id = getline () in
    let message_type = il getline in
    let proto = 
      match String.uppercase message_type with
	"HELLO_OK" -> HelloOk
      | "HELLO" -> Hello
      | "BYEBYE" -> Byebye
      | "MESSAGE" ->
	  let length = int_of_string (il getline) in
	  let s = String.create length in
	  let n = input s 0 length in
	  Message s
      |	"ROOM_MESSAGE" ->
	  let name = getline () in
	  let n = int_of_string (il getline) in
	  let rec iter acc m = 
	    if m < n then
	      iter 
		(
		 let source_id = getline () in
		 let source_host = getline () in
		 let source_port = int_of_string (il getline) in
		 (source_id, source_host, source_port) :: acc
		)
		(m+1)
	    else
	      List.rev acc
	  in
	  let people = iter [] 0 in
	  let length = int_of_string (il getline) in
	  let s = String.create length in
	  let n = input s 0 length in
	  RoomMessage (name, people, s)

      | "ADD_OPEN" ->
	  let id = il getline in
	  let host = il getline in
	  let port = int_of_string (il getline) in
	  AddOpen (id, (host, port))
      |	_ ->
	  raise (Failure "Bad message type")
    in
    let source = (v, source_id, (source_host, source_port)) in
    (source, dest_id, proto)
  with
    End_of_file -> raise (Failure "End_of_file")
  | Invalid_argument "int_of_string" -> raise (Failure "Bad format")

let read_packet_channel inch =
  read_packet (fun () -> input_line inch) (input inch)

let read_packet_buffer buf =
  read_packet (fun () -> Chat_misc.buf_get_line buf) (Chat_misc.buf_input buf)


let ol buf s = Printf.bprintf buf "%s\n" s

(** write the given paquet to the given buffer. *)
let write_packet buf packet =
  let (source, dest_id, proto) = packet in
  let (v, source_id, (source_host, source_port)) = source in
  let p = ol buf in
  let p2 s = p (Chat_misc.remove_newlines s) in
  p2 v;
  p2 source_id ;
  p2 source_host ;
  p2 (string_of_int source_port) ;
  p2 dest_id ;
  match proto with
    HelloOk -> p "HELLO_OK"
  | Hello -> p "HELLO"
  | Byebye -> p "BYEBYE"
  | Message s -> 
      p "MESSAGE"; 
      let l = String.length s in 
      p (string_of_int l);
      p s
  | AddOpen (id, (h, port)) ->
      p "ADD_OPEN";
      p2 id ;
      p2 h ;
      p2 (string_of_int port)
  | RoomMessage (name, people, s) ->
      p "ROOM_MESSAGE";
      p name ;
      p (string_of_int (List.length people));
      List.iter
	(fun (i,h,port) ->
	  p2 i ;
	  p2 h ;
	  p2 (string_of_int port)
	)
	people;
      let l = String.length s in 
      p (string_of_int l);
      p s

(** write the given paquet to the given channel. *)
let write_packet_channel ouch packet =
  let buf = Buffer.create 256 in
  write_packet buf packet;
  output_string ouch (Buffer.contents buf)

(** The classes used to send and receive messages. *)
class type com =
  object
    (** Free all what must be freed when the app is closed. *)
    method close : unit

    (** [send adr mes] sends the message [mes] to the
       application at address [adr].
       Should raise Failure with an error message if an
       error occurs.*)
    method send : id -> address -> proto -> unit

    (** [receive] returns an optional message, if
       one was pending.
       Should raise Failure with an error message if an
       error occurs.*)
    method receive : packet option
  end

(** This class implements the com interface,
   with Unix unconnected sockets.
   It needs a {!Chat_config.config} class to
   access config options.*)
class udp conf =
  let localhost = conf#hostname in
  let h = Unix.gethostbyname localhost in
  let inet_addr = h.Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET (inet_addr, conf#port) in
  let iptable = Hashtbl.create 13 in
  object (self)
    val sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 

    method private source =
      (version, conf#id, (localhost, conf#port))

    method close = Unix.close sock

    method send id adr mes =
      let (host, port) = adr in
      if host = localhost && port = conf#port then
	raise (Failure Chat_messages.dest_is_source)
      else
	(
	 let domain = Unix.PF_INET in
	 let sock = Unix.socket domain Unix.SOCK_DGRAM 0 in
	 let ip_opt =
	   try
	     Some (Hashtbl.find iptable host)
	   with Not_found ->
	     try
	       let h = Unix.gethostbyname host in
	       Some (h.Unix.h_addr_list.(0))
	     with
	       Not_found -> None
	 in
	 match ip_opt with
	   None -> ()
	 | Some ip ->
	     let sockaddr = Unix.ADDR_INET (ip, port) in
	     try
	       let buf = Buffer.create 256 in
	       write_packet buf (self#source, id, mes);
	       let s = Buffer.contents buf in
	       ignore (Unix.sendto sock s 0 (String.length s) [] sockaddr)
	     with
	     | Unix.Unix_error (e,s1,s2) ->
		 let s = (Unix.error_message e)^" :"^s1^" "^s2 in
		 raise (Failure s)
	)

    method receive =
      try
	let s_buf = String.create 66000 in
	let (len, addr) = Unix.recvfrom sock s_buf 0 66000 [] in
	(match addr with
	  Unix.ADDR_INET (a,_) -> Chat_messages.verbose ("receive from "^(Unix.string_of_inet_addr a))
	| _ -> ()
	);
	let s = String.sub s_buf 0 len in
	let buf = Buffer.create len in
	Buffer.add_string buf s;
	let paq = read_packet_buffer buf in
	match paq with
	  ((v,id,(host,port)),iddest,pro) ->
	    if v <> version then 
	      (
	       Chat_messages.verbose Chat_messages.incompatible_version ;
	       None
	      )
	    else
	      (
	       match addr with
		 Unix.ADDR_INET (a,_) ->
		   (
		    try
		      let old_a = Hashtbl.find iptable host in
		      if old_a = a then
			()
		      else
			(
			 Hashtbl.remove iptable host ;
			 raise Not_found
			)
		    with
		      Not_found ->
			Hashtbl.add iptable host a
		   );
		   Some paq
	       | _ -> 
		   None
	      )
      with
	Unix.Unix_error (Unix.EWOULDBLOCK,_,_)
      | Unix.Unix_error (Unix.EAGAIN,_,_) ->
	  None
      | Unix.Unix_error (e,s1,s2) ->
	  let s = (Unix.error_message e)^" :"^s1^" "^s2 in
	  raise (Failure s)

    initializer
      Unix.setsockopt sock Unix.SO_REUSEADDR true ;
      Unix.set_nonblock sock ;
      try
	Unix.bind sock sock_addr
      with
      | Unix.Unix_error (e,s1,s2) ->
	  let s = (Unix.error_message e)^" :"^s1^" "^s2 in
	  raise (Failure s)	  
  end

let cpt = ref 1 

(** This class implements the com interface,
   with Unix connected sockets.
   It needs a {!Chat_config.config} class to
   access config options.*)
class tcp conf =
  let localhost = conf#hostname in
(*  let h = Unix.gethostbyname localhost in*)
  let inet_addr = Unix.inet_addr_of_string "0.0.0.0" (*h.Unix.h_addr_list.(0)*) in
  let sock_addr = Unix.ADDR_INET (inet_addr, conf#port) in
  let iptable = Hashtbl.create 13 in
  object (self)
    val instance = (incr cpt; !cpt)

    val sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 

    method private source =
      (version, conf#id, (localhost, conf#port))

    method close = Unix.close sock

    method send id adr mes =
      let (host, port) = adr in
      if host = localhost && port = conf#port then
	raise (Failure Chat_messages.dest_is_source)
      else
	(
	 let domain = Unix.PF_INET in
	 let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
	 let ip_opt =
	   try
	     Some (Hashtbl.find iptable host)
	   with Not_found ->
	     try
	       let h = Unix.gethostbyname host in
	       Some (h.Unix.h_addr_list.(0))
	     with
	       Not_found -> None
	 in
	 match ip_opt with
	   None -> ()
	 | Some ip ->
	     let sockaddr = Unix.ADDR_INET (ip, port) in
	     try
	       let chanout = Unix.out_channel_of_descr sock in
	       let paq = (self#source, id, mes) in
	       Unix.connect sock sockaddr;
	       write_packet_channel chanout paq;
	       flush chanout;
	       close_out chanout
	     with
	     | Unix.Unix_error (e,s1,s2) ->
		 let s = s1^" "^s2^" : "^(Unix.error_message e) in
		 raise (Failure s)
	)

    method receive =
      try
	let (desc, addr) = Unix.accept sock in
	(match addr with
	  Unix.ADDR_INET (a,_) -> Chat_messages.verbose ("receive from "^(Unix.string_of_inet_addr a))
	| _ -> ()
	);
	let chanin = Unix.in_channel_of_descr desc in
	let chanout = Unix.out_channel_of_descr desc in
	let paq = read_packet_channel chanin in
	let ret = 
	  match paq with
	    ((v,id,(host,port)),iddest,pro) ->
	      if v <> version then 
		(
		 Chat_messages.verbose Chat_messages.incompatible_version ;
		 None
		)
	      else
		(
		 match addr with
		   Unix.ADDR_INET (a,_) ->
		     (
		      try
			let old_a = Hashtbl.find iptable host in
			if old_a = a then
			  ()
			else
			  (
			   Hashtbl.remove iptable host ;
			   raise Not_found
			  )
		      with
			Not_found ->
			  Hashtbl.add iptable host a
		     );
		     Some paq
		 | _ -> 
		     None
		)
	in
	close_out chanout;
	ret
      with
	Unix.Unix_error (Unix.EWOULDBLOCK,_,_)
      | Unix.Unix_error (Unix.EAGAIN,_,_) ->
	  None
      | Unix.Unix_error (e,s1,s2) ->
	  let s = (Unix.error_message e)^" :"^s1^" "^s2 in
	  raise (Failure s)
      |	Failure s ->
	  raise (Failure s)
      |	e ->
	  let s = Printexc2.to_string e in
	  raise (Failure s)

    initializer
      Unix.setsockopt sock Unix.SO_REUSEADDR true ;
      Unix.set_nonblock sock ;
      try
	Unix.bind sock sock_addr ;
	Unix.listen sock 15;
      with
      | Unix.Unix_error (e,s1,s2) ->
	  let s = (Unix.error_message e)^" :"^s1^" "^s2 in
	  raise (Failure s)
  end
