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


type id3 = Mp3_ui.id3 =
    V1
  | V2
  | Both

let edit_file = Mp3_ui.edit_file

let edit_tag_v1 = Mp3_ui.edit_tag_v1
let edit_tag_v2 = Mp3_ui.edit_tag_v2
