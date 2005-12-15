(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* $Id$ *)

open Printf2
(** Chat functions. *)

let version = Chat_proto.version

module C = Chat_proto
module O = CommonOptions

let (!!) = Options.(!!)

let send_paquet_to_mlchat (p : C.packet) =
  let domain = Unix.PF_INET in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let inet_addr =
    let host = !!O.chat_app_host in
    try Unix.inet_addr_of_string host
    with _ ->
        let h = Unix.gethostbyname host in
        h.Unix.h_addr_list.(0)
  in
  let sockaddr = Unix.ADDR_INET (inet_addr, !!O.chat_app_port) in
  let chanout = Unix.out_channel_of_descr sock in
  try
    Unix.connect sock sockaddr;
    Chat_proto.write_packet_channel chanout p;
    flush chanout;
    close_out chanout
  with
  | Unix.Unix_error (e,s1,s2) -> if !CommonOptions.verbose then begin
      let s = (Unix.error_message e)^" : "^s1^" "^s2 in
      lprintf_nl "Error %s with chat on IP %s, port %d" s
            !!O.chat_app_host !!O.chat_app_port;
      close_out chanout
      end
  | e -> if !CommonOptions.verbose then begin
      lprintf_nl "Error %s with chat on IP %s, port %d"
        (Printexc2.to_string e)
            !!O.chat_app_host !!O.chat_app_port;
      close_out chanout
      end


let send_chat_proto name ad_opt m =
  let ad =
    match ad_opt with
      None ->
	(* utiliser le port de chat et l'hostname du core donkey *)
	(Unix.gethostname (), !!O.chat_port)
    | Some ad -> ad
  in
  let source = (version, name, ad) in
  let paquet = (source, name, m) in
  send_paquet_to_mlchat paquet

let send_text name ad_opt s =
  send_chat_proto name ad_opt (C.Message s)

let send_add_open name ad_opt =
  let ad =
    match ad_opt with
      None -> (Unix.gethostname (), !!O.chat_port)
    | Some ad -> ad
  in
  let source = (version, name, ad) in
  let paquet = (source, name, (C.AddOpen (name, ad))) in
  send_paquet_to_mlchat paquet

let send_simple proto =
  let name = !!O.chat_console_id in
  let source = (version, name, (Unix.gethostname (), !!O.chat_port)) in
  let paquet = (source, name, proto) in
  send_paquet_to_mlchat paquet

let send_hello_ok () = send_simple C.HelloOk

let send_hello () = send_simple C.Hello

let send_console_message s =
  send_simple (C.Message s)

let send_warning_for_downloaded_file file =
  if !!CommonOptions.chat_warning_for_downloaded then
    let s = Printf.sprintf "I just completed the download of file %s" file in
    send_console_message s

