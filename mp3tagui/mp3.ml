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

(** Main module. *)

let _ = GMain.Main.init ()

let _ = Mp3_args.parse ()

let _ = List.iter 
    (fun f ->
      try Mp3tagui.edit_file Mp3tagui.Both f
      with 
	Sys_error s -> prerr_endline s 
    )
    !Mp3_args.files
