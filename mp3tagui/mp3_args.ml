(***********************************************************************)
(*                               Mp3tag                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(** Analysis of command line arguments. *)

(** The files to handle. *)
let files = ref ([] : string list)

let options  = ref ([] : (string * Arg.spec * string) list)

(** Parse the command line and fill the arguments variables. *)
let parse () =
  try
    let _ = Arg.parse !options
        (fun s -> files := !files @ [s])
	(Mp3_messages.usage^Mp3_messages.options_are)
    in
    ()
  with
    Failure s ->
      prerr_endline s ;
      exit 1
