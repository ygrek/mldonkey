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

open CommonResult
open BasicSocket
open CommonGlobals
open CommonTypes
open CommonClient
open CommonComplexOptions
open Gui_proto
open Options
open CommonFile
open CommonUser
open CommonChatRoom
open CommonTypes
open CommonShared
open CommonServer
open SlskOptions
open SlskTypes

  
let set_server_state s state =
  set_server_state (as_server s.server_server) state
let set_room_state s state =
  set_room_state (as_room s.server_room) state
let server_num s = server_num (as_server s.server_server)
let file_num s = file_num (as_file s.file_file)
let server_state s = server_state (as_server s.server_server)
let file_state s = file_state (as_file s.file_file)
let server_must_update s = server_must_update (as_server s.server_server)
let file_must_update s = file_must_update (as_file s.file_file)

let nknown_servers = ref 0
let connected_servers = ref ([] : server list)

let servers_by_addr = Hashtbl.create 13
  
let new_server addr port=
  try
    Hashtbl.find servers_by_addr (addr, port) 
  with _ ->
      incr nknown_servers;
      let rec h = { 
          server_server = server_impl;
          server_room = room_impl;
          server_name = "<unknown>";
          server_addr = addr;
          server_nusers = 0;
          server_info = "";
          server_connection_control = new_connection_control 0.0;
          server_sock = None;
          server_port = port;
          server_nick = 0;
          server_last_nick = "";
          server_search = None;
          server_search_timeout = 0.0;
          server_users = [];
        } and 
        server_impl = {
          dummy_server_impl with
          impl_server_val = h;
          impl_server_ops = server_ops;
        } and 
        room_impl = {
          dummy_room_impl with
          impl_room_val = h;
          impl_room_ops = room_ops;
        }         
      in
      server_add server_impl;
      room_add room_impl;
      Hashtbl.add servers_by_addr (addr, port) h;
      h
