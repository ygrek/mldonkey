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

val clients_by_id :  (Ip.t, ServerTypes.global_client) Hashtbl.t
val files_by_md4 : (Md4.t, ServerTypes.location list) Hashtbl.t 

val client_counter : int ref
  (*
val other_servers : DonkeyTypes.server list ref
val alive_servers : DonkeyTypes.server list ref
*)
val serverList :   DonkeyTypes.server list ref
  
val nconnected_clients : int ref
val nshared_md4 : int ref

(*messages counter*)
val nb_udp_query_sec : float ref 
val nb_udp_query_count : int ref 
val nb_udp_loc_sec : float ref 
val nb_udp_loc_count : int ref 

val nb_udp_loc_reply_sec : float ref 
val nb_udp_loc_reply_count : int ref 

val nb_udp_req_sec : float ref 
val nb_udp_req_count : int ref 
val nb_udp_ping_server_sec : float ref
val nb_udp_ping_server_count : int ref

val nb_tcp_req_sec : float ref 
val nb_tcp_req_count : int ref 
val nb_udp_reply_sec : float ref 
val nb_udp_reply_count : int ref 



(*Group server values*)
val servers_by_id : (int, ServerTypes.server) Hashtbl.t
val local_clients : Ip.t list ref
val to_connect : int list ref

val group_id : Md4.t ref 
val server_id : int ref 
val nconnected_servers : int ref
val nremote_clients : int ref
val nshared_remote_md4 : int ref
val server_counter : int ref
val ngroup_clients : int ref
val ngroup_files : int ref

val stop_udp : bool ref

val udp_sock : UdpSocket.t option ref
  
val notifications : (Md4.t*int, ServerTypes.subscription) Hashtbl.t

val tag_name_list : string list ref
val subs_match : (int * Ip.t) list ref

val nb_notifs : int ref
val nb_info : int ref 
val info_percent : (int * float) ref
