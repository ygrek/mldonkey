(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

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
