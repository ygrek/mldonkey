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

(** Command line arguments. *)

(** The list of options.*)
let options_list = [
  "-v", Arg.Set Chat_messages.verbose_mode, Chat_messages.op_verbose ;
  "-c", Arg.String (fun _ -> ()), Chat_messages.op_config ;
]
  
(** Parse of the command line arguments. *)
let parse () = 
  (* we check if we must use a different config file,
     given on the command line *)
  let arg_list = Array.to_list Sys.argv in
  let rec iter = function
      [] | _ :: [] -> None
    | "-c" :: file :: q -> 
        Some file
    | _ :: q ->
        iter q
  in
  let rc_file = 
    match iter arg_list with
      None -> Filename.concat Chat_messages.home ".mlchatrc"
    | Some f -> f
  in
  let config = new Chat_config.config rc_file in
  let complete_options = 
    options_list @ 
    config#args_spec
  in
  Arg.parse complete_options 
    (fun s -> ())
    Chat_messages.usage;

  config
