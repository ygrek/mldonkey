(***********************************************************************)
(*                                MLChat                               *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The MLChat library. You can use it to add chat in your apps.*)

type port = int
type host = string
type id = string

(** {2 Configuration}*)

(** The type of the class to use as the st of options. *)
class type config = 
  object
    method args_spec : (string * Arg.spec * string) list
    method color_connected : string
    method color_connected_temp : string
    method color_myself : string
    method color_not_connected : string
    method id : string
    method hostname : string
    method people : (string * string * int) list
    method popup_all : bool
    method port : int
    method save : unit
    method set_color_connected : string -> unit
    method set_color_connected_temp : string -> unit
    method set_color_myself : string -> unit
    method set_color_not_connected : string -> unit
    method set_id : string -> unit
    method set_hostname : string -> unit
    method set_people : (id * host * port) list -> unit
    method set_popup_all : bool -> unit
    method set_port : int -> unit
    method set_timeout : int -> unit
    method timeout : int

  end

(** Create a config object from a config file name.
   Options are saved in this file. *)
class file_config : string -> config


(** {2 Communication} *)

type address = host * port
type message = string
type version
type source = version * id * address
type proto = 
    HelloOk | Hello | Byebye | Message of message
  | AddOpen of id * address
  | RoomMessage of id * (id * host * port) list * message
type packet = source * id * proto

val version : unit -> version

(** The type of the class to use to send and receive messages.*)
class type com =
  object
    method close : unit
    method receive : packet option
    method send : id -> address -> proto -> unit
  end

(** Read a packet from the given buffer. *)
val read_packet : Buffer.t -> packet

(** Read a packet from the given channel. *)
val read_packet_channel : in_channel -> packet

(** Write the given packet to the given buffer. *)
val write_packet : Buffer.t -> packet -> unit

(** Write the given packet to the given channel. *)
val write_packet_channel : out_channel -> packet -> unit

(** A class implementing the protocol with UDP sockets. *)
class udp : config -> com

(** A class implementing the protocol with TCP sockets. *)
class tcp : config -> com

(** {2 Application} *)

(** The application class. *)
class app : 
    ?no_quit: bool -> 
      ?pred: ((id*host*port) -> (id*host*port) -> bool) ->
	config -> com -> 
   object
     method box : GPack.box
     method coerce : GObj.widget

     (** Call this method with the window the box has
	been packed in.*)
     method init_window : GWindow.window -> unit
   end
