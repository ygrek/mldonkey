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

(** Config options. *)

open Chat_options

module M = Chat_messages

class config rcfile =
  let op_file = create_options_file rcfile in
  object (self)
    (** The options *)
    method args_spec = Chat_options.simple_args op_file 

    (** {2 Connection options} *)

    val id = define_option op_file ["id"]
	M.h_id string_option "mlchat"
    method id = !!id
    method set_id i = id =:= i

    val hostname = define_option op_file ["hostname"]
	M.h_id string_option (Unix.gethostname ())
    method hostname = !!id
    method set_hostname i = hostname =:= i

    val port = define_option op_file ["port"]
	M.h_port int_option 5036
    method port = !!port
    method set_port p = port =:= p

    val timeout = define_option op_file ["timeout"]
	M.h_timeout int_option 100
    method timeout = !!timeout
    method set_timeout t = timeout =:= t

    val popup_all = define_option op_file ["popup_all"]
	M.h_popup_all bool_option true
    method popup_all = !!popup_all
    method set_popup_all b = popup_all =:= b

    (** {2 Colors} *)

    val color_connected = define_option op_file ["colors" ; "connected"]
	M.h_color_connected string_option "DarkGreen"
    method color_connected = !!color_connected
    method set_color_connected c = color_connected =:= c

    val color_connected_temp = define_option op_file
	["colors" ; "connected_temp"]
	M.h_color_connected_temp string_option "Red"
    method color_connected_temp = !!color_connected_temp
    method set_color_connected_temp c = color_connected_temp =:= c

    val color_not_connected = define_option op_file ["colors" ; "not_connected"]
	M.h_color_not_connected string_option "Black"
    method color_not_connected = !!color_not_connected
    method set_color_not_connected c = color_not_connected =:= c

    val color_myself = define_option op_file ["colors" ; "myself"]
	M.h_color_myself string_option "Blue"
    method color_myself = !!color_myself
    method set_color_myself c = color_myself =:= c

    (** {2 People} *)

    val people = define_option op_file 
	["people"]
	M.h_people
	(list_option (tuple3_option
			(
			 string_option, 
			 string_option, 
			 int_option))
	)
	[]
    method people = !!people
    method set_people l = people =:= l

    (** {2 Saving options} *)

    method save = save op_file

    initializer
      load op_file;
      save op_file
  end
