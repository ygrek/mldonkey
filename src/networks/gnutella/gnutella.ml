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

open CommonSwarming
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

module DG = CommonGlobals
module DO = CommonOptions

(* Here, we put functions that, depending on the server type, call
either the corresponding Gnutella1 or Gnutella2 function
*)
    
let server_ask_query s uid words xml_query = 
  if s.server_gnutella2 then
    Gnutella2.server_ask_query s uid words xml_query
  else
    Gnutella1.server_ask_query s uid words xml_query  

let server_ask_uid s uid words xml_query = 
  if s.server_gnutella2 then
    Gnutella2.server_ask_uid s uid words xml_query
  else
    Gnutella1.server_ask_uid s uid words xml_query  
    
let server_send_ping s = 
  if s.server_gnutella2 then
    Gnutella2.server_send_ping s
  else
    Gnutella1.server_send_ping s
  
let server_send_push s uid uri = 
  if s.server_gnutella2 then
    Gnutella2.server_send_push s uid uri  
  else
    Gnutella1.server_send_push s uid uri  
    
let send_qrt_sequence s =
  let update_table =
    if !new_shared_words then begin
        update_shared_words ();
        new_shared_words := false;
        true
      end else false
  in
  if s.server_gnutella2 then
    Gnutella2.send_qrt_sequence s update_table
  else
    Gnutella1.send_qrt_sequence s update_table
  
let ask_query servers words xml_query =
  if !verbose_msg_servers then begin
      lprintf "sending query for <%s> to %d servers\n" words (List.length  servers);
    end;
  let uid = Md4.random () in
  List.iter (fun s ->
      server_ask_query s uid words xml_query
  ) servers;
  uid

let ask_uid servers words fuid =
  if !verbose_msg_servers then begin
      lprintf "sending query for <%s>\n" words;
    end;
  let uid = Md4.random () in
  List.iter (fun s ->
      server_ask_uid s uid words fuid
  ) servers;
  uid

  
let get_name_keywords file_name =
  match stem file_name with 
    [] | [_] -> 
      lprintf "Not enough keywords to recover %s\n" file_name;
      [file_name]
  | l -> l
      
let gen_query file servers =
  if file.file_uids = [] then
    let keywords = get_name_keywords file.file_name in
    let words = String2.unsplit keywords ' ' in
    ignore (ask_query servers words "urn:")
  else
    List.iter (fun uid ->
        ignore (ask_uid servers "" uid)
    ) file.file_uids    
    
    
let recover_files () =
  List.iter (fun file ->
      gen_query file !connected_servers
  ) !current_files;
  ()
  
let recover_files_from_server s =
  if !verbose_msg_servers then begin
      lprintf "trying to recover files from server\n";
    end;
  List.iter (fun file ->
      gen_query file [s]
  ) !current_files;
  ()
            

      
let disconnect_from_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
      (match server_state s with 
          Connected _ ->
            lprintf "DISCONNECT FROM SERVER %s:%d\n" 
              (Ip.to_string s.server_ip) s.server_port;
        | _ -> ());
      close sock "timeout";
      s.server_sock <- None;
      set_server_state s (NotConnected (-1));
      decr nservers;
      s.server_need_qrt <- true;
      if List.memq s !connected_servers then begin
          connected_servers := List2.removeq s !connected_servers;
        end;
      server_remove s
      
let find_header header headers default =
  try
    List.assoc header headers
  with Not_found -> default

let add_uid r uid =
  if not (List.mem uid r.result_uids) then
    r.result_uids <- uid :: r.result_uids
  
