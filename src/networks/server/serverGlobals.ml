(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open Md4
open ServerOptions
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open DonkeyProtoCom
open ServerTypes
  
let network = CommonNetwork.new_network "Donkey:server"

let connection_manager = network.network_connection_manager
  
(*basic data structures*)
let (files_by_md4  :  (Md4.t, ServerTypes.location list) Hashtbl.t )
  
  = (if !!relais_cooperation_protocol then
                     Hashtbl.create !!max_group_clients
                     else
                      Hashtbl.create !!max_clients;)

let (clients_by_id :   (Ip.t, ServerTypes.global_client) Hashtbl.t)
  = (if !!relais_cooperation_protocol then
                        Hashtbl.create !!max_group_files
                        else
                        Hashtbl.create !!max_files;)


let get_client id = Hashtbl.find clients_by_id id


(*strucure for servers addresse*) 
(*
No more other/alive servers: all servers are in DonkeyGlobals.servers_by_key.
When a message is received from them, the server_last_message is updated
to the current date. 
  
let other_servers = ref []
let alive_servers = ref []
    *)

(* The list of servers that should be sent in the ServerList message *)
let serverList = ref ([] : DonkeyTypes.server list)
(*
  (*
(*get a part of the servers liste*)
let rec get_serverlist nb_servers =
  match servers with 
    [] -> []
    | hd :: tail ->if nb_servers = 0 then []
      else 
        { 
          M.ServerList.ip = hd.DonkeyTypes.server_ip; 
          M.ServerList.port = hd.DonkeyTypes.server_port
        } :: get_serverlist tail (nb_servers-1)
*)
 (M.ServerListReq (get_serverlist  50)); *)
  
(*counter for server stat*)
let nconnected_clients = ref 0

let nshared_md4 = ref 0

let client_counter = ref 0


(*count messages to the server*)
let nb_udp_query_sec = ref 0.
let nb_udp_query_count = ref 0
let nb_udp_loc_sec = ref 0.
let nb_udp_loc_count = ref 0
let nb_udp_loc_reply_sec = ref 0.
let nb_udp_loc_reply_count = ref 0

let nb_udp_req_sec = ref 0.
let nb_udp_req_count = ref 0
let nb_udp_ping_server_sec = ref 0. 	 
let nb_udp_ping_server_count = ref 0 

let nb_tcp_req_sec = ref 0.
let nb_tcp_req_count = ref 0
let nb_udp_reply_sec = ref 0.
let nb_udp_reply_count = ref 0

(*/////////////////////////////////////////*)
(*Group server variables*)

let (servers_by_id :  (int, ServerTypes.server) Hashtbl.t)
  = (if !!relais_cooperation_protocol then
                      Hashtbl.create 5 
                      else
                      Hashtbl.create 0;)

let to_connect = ref ([] : int list)

let local_clients = ref ([] : Ip.t list)

let group_id = ref Md4.null

let server_id = ref 0

let nconnected_servers = ref 0

let nremote_clients = ref 0

let nshared_remote_md4 = ref 0

let server_counter = ref 0

let ngroup_clients = ref 0

let ngroup_files = ref 0

let stop_udp = ref false

(* group servers observation*)

let nb_notifs = ref 0
let nb_info = ref 0
let info_percent = ref (0,0.)


  (*
let rec tag_find v tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = tag_name; tag_value = tag_value } :: _ 
    when tag_name = v -> tag_value
  | _ :: tags -> tag_find v tags


let rec tags_find_size tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = "length"; tag_value = Uint32 s } :: _ -> s
  | _ :: tags -> tags_find_size tags

let remove_file file =
  Hashtbl.remove files_by_md4 file.file_md4
      
let remove_client client sock msg =
(* remove the client from all tables *)
  Hashtbl.remove clients_by_id client.client_id;
  
(* remove the client from files *)
  List.iter (fun file -> 
      file.file_clients <- List2.removeq client file.file_clients;
      if file.file_clients = [] then
        remove_file file
  ) client.client_files;
  ()

*)

let udp_sock = ref (None: UdpSocket.t option)

let (notifications :  (Md4.t*int, ServerTypes.subscription) Hashtbl.t)
  = Hashtbl.create 13
  
(* List all tag name *)
let tag_name_list = ref ([] : string list)
let subs_match = ref ([] :  (int * Ip.t) list)
