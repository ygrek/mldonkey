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

(** Protocol for communicating, and a class
   implementing send and receive with 
   Unix unconnected sockets. *)

type port = int
type host = string
type address = host * port

type message = string
type version = string
type id = string

type source = version * id * address

type proto = 
  | HelloOk (** Reply to an ok message *)
  | Hello  (** to signal that we are connected *)
  | Byebye  (** to signal that we are disconnecting *)
  | Message of message
  | AddOpen of id * address (** to remove add a user *)

type paquet = source * id * proto (** source, destination user id, proto *)

let version = Chat_messages.software_version

(** The classes used to send and receive messages. *)
class type com =
  object
    (** [send adr mes] sends the message [mes] to the
       application at address [adr].
       Should raise Failure with an error message if an
       error occurs.*)
    method send : id -> address -> proto -> unit

    (** [receive] returns an optional message, if
       one was pending.
       Should raise Failure with an error message if an
       error occurs.*)
    method receive : paquet option
  end

(** This class implements the com interface,
   with Unix unconnected sockets.
   It needs a {!Chat_config.config} class to
   access config options.*)
class udp conf =
  let localhost = Unix.gethostname () in
  let h = Unix.gethostbyname localhost in
  let inet_addr = h.Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET (inet_addr, conf#port) in
  let iptable = Hashtbl.create 13 in
  object (self)
    val sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 

    method private source =
      (version, conf#id, (localhost, conf#port))

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
	       let s = Marshal.to_string ((self#source, id, mes) : paquet) [] in
	       ignore (Unix.sendto sock s 0 (String.length s) [] sockaddr)
	     with
	     | Unix.Unix_error (e,s1,s2) ->
		 let s = (Unix.error_message e)^" :"^s1^" "^s2 in
		 raise (Failure s)
	)

    method receive =
      try
	let buf = String.create 66000 in
	let (len, addr) = Unix.recvfrom sock buf 0 66000 [] in
	(match addr with
	  Unix.ADDR_INET (a,_) -> Chat_messages.verbose ("receive from "^(Unix.string_of_inet_addr a))
	| _ -> ()
	);
	let s = String.sub buf 0 len in
	let (paq : paquet) = Marshal.from_string s 0 in
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
	  prerr_endline s;
	  exit 1
  end

(** This class implements the com interface,
   with Unix connected sockets.
   It needs a {!Chat_config.config} class to
   access config options.*)
class tcp conf =
  let localhost = Unix.gethostname () in
(*  let h = Unix.gethostbyname localhost in*)
  let inet_addr = Unix.inet_addr_of_string "0.0.0.0" (*h.Unix.h_addr_list.(0)*) in
  let sock_addr = Unix.ADDR_INET (inet_addr, conf#port) in
  let iptable = Hashtbl.create 13 in
  object (self)
    val sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 

    method private source =
      (version, conf#id, (localhost, conf#port))

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
	       let v = ((self#source, id, mes) : paquet) in
	       Unix.connect sock sockaddr;
	       output_value chanout v;
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
	let (paq : paquet) = input_value chanin in
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

    initializer
      Unix.setsockopt sock Unix.SO_REUSEADDR true ;
      Unix.set_nonblock sock ;
      try
	Unix.bind sock sock_addr ;
	Unix.listen sock 15
      with
      | Unix.Unix_error (e,s1,s2) ->
	  let s = (Unix.error_message e)^" :"^s1^" "^s2 in
	  prerr_endline s;
	  exit 1
  end
