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

open Printf2
open Md4

open CommonShared
open CommonServer
open CommonComplexOptions
open GuiProto
open CommonClient
open CommonFile
open CommonUser
open CommonSearch
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open DonkeyMftp
open DonkeyOneFile
open DonkeyProtoCom
open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions
open DonkeyOptions
open CommonOptions
open DonkeyClient  
open CommonGlobals
open DonkeyStats

module Udp = DonkeyProtoUdp 

let search_handler s t =
  let waiting = s.search_waiting - 1 in
  s.search_waiting <- waiting;
  List.iter (fun f ->
      search_found false s f.f_md4 f.f_tags
  ) t
(*  search.search_handler (Waiting s.search_waiting) *)
    
let udp_query_locations file s =
  if !verbose then begin
      lprintf "UDP: query location %s" (Ip.to_string s.server_ip);
      lprint_newline ();
    end;
  let module Udp = DonkeyProtoUdp in
  udp_server_send s (Udp.QueryLocationUdpReq [file.file_md4])

  (*
let rec find_search_rec num list =
  match list with
    [] -> raise Not_found
  | s :: tail ->
      if s.search_search.search_num = num then s else 
        find_search_rec num tail
        
let find_search num = find_search_rec num !local_searches
    *)

let cut_for_udp_send max_servers list =
  let min_last_conn = last_time () - 8 * 3600 in
  let rec iter list n left =
    if n = 0 then 
      left, list
    else
    match list with 
      [] -> left, []
    | s :: tail ->
        if connection_last_conn s.server_connection_control > min_last_conn
        then
          iter tail (n-1) (s :: left)
        else
          iter tail n left
  in
  iter list max_servers []

let make_xs ss =
  if ss.search_num <> !xs_last_search then begin
      xs_last_search := ss.search_num;
      xs_servers_list := Hashtbl2.to_list servers_by_key;
    end;
  
  let before, after = cut_for_udp_send !!max_xs_packets !xs_servers_list in
  xs_servers_list := after;
  List.iter (fun s ->
      match s.server_sock with
      | Some sock -> ()
      | None ->
          let module M = DonkeyProtoServer in
          let module Q = M.Query in
          udp_server_send s (if server_send_multiple_replies s then
              Udp.QueryUdpReq ss.search_query
	  else Udp.QueryMultipleUdpReq ss.search_query);
  ) before;
  
  DonkeyOvernet.overnet_search ss
          
let force_check_locations () =
  try
    if !xs_last_search >= 0 then  begin
        try
          make_xs (search_find !xs_last_search)
        with _ -> ()
      end;

    let files = ref [] in
    List.iter (fun file -> 
      if file_state file = FileDownloading then 
	files := file :: !files) !current_files;

    if !files <> [] then

    let old_servers = ref [] in
    let new_servers = ref [] in
    let nservers = ref 0 in

    while !nservers < !!max_udp_sends &&
      match !udp_servers_list with
	[] -> false
      | s :: tail -> 
	  udp_servers_list := tail;
	  (match s.server_sock with
            None -> 
	      if 
		connection_last_conn s.server_connection_control + 3600*8 > last_time () &&
		s.server_next_udp <= last_time () then begin
		  (if server_accept_multiple_getsources s then
		    new_servers := s :: !new_servers
		  else
		    old_servers := s :: !old_servers);
		  incr nservers;
		end
	  | _ -> ());
	  true do
      ()
    done;

    if !new_servers <> [] then begin
      let md4s = List.map (fun file -> file.file_md4) !files in

      List.iter (fun s ->
	let module Udp = DonkeyProtoUdp in
	udp_server_send s (Udp.QueryLocationUdpReq md4s);
	s.server_next_udp <- last_time () + !!min_reask_delay
      ) !new_servers
    end;

    if !old_servers <> [] then    
      List.iter (fun file -> 
	if file_state file = FileDownloading then begin
(*(* USELESS NOW *)
   Intmap.iter (fun _ c ->
   try connect_client !!client_ip [file] c with _ -> ()) 
   file.file_known_locations;
*)            

          (* now, it is done in donkeySources
             List.iter (fun s ->
             match s.server_sock with
                  None -> () (* assert false !!! *)
                | Some sock ->
                    (try DonkeyServers.query_location file sock with _ -> ())
            ) (connected_servers());
*)
          
            List.iter (fun s  ->
              if 
                connection_last_conn s.server_connection_control + 3600*8 > last_time () &&
                s.server_next_udp <= last_time () then
                  match s.server_sock with
                  None -> udp_query_locations file s
                  | _ -> ()
	    ) !old_servers;
	  end
    ) !current_files;

    List.iter (fun s ->
        s.server_next_udp <- last_time () + !!min_reask_delay
	      ) !old_servers;
    if !udp_servers_list = [] then
          udp_servers_list := Hashtbl2.to_list servers_by_key;
    

  with e ->
      lprintf "force_check_locations: %s" (Printexc2.to_string e);
      lprint_newline ()

let add_user_friend s u = 
  let kind = 
    if Ip.valid u.user_ip && Ip.reachable u.user_ip then
      Known_location (u.user_ip, u.user_port)
    else begin
        begin
          match s.server_sock, server_state s with 
            Some sock, (Connected _ |Connected_downloading) ->
              query_id s sock u.user_ip None;
          | _ -> ()
        end;
        Indirect_location (u.user_name, u.user_md4)
      end
  in
  let c = new_client kind  in
  c.client_tags <- u.user_tags;
  set_client_name c u.user_name u.user_md4;
  friend_add c

  
let udp_from_server p =
  match p.UdpSocket.addr with
  | Unix.ADDR_INET(ip, port) ->
      let ip = Ip.of_inet_addr ip in
      if !!update_server_list then
        let s = check_add_server ip (port-4) in
(* set last_conn, but add a 2 minutes offset to prevent staying connected
to this server *)
        connection_set_last_conn s.server_connection_control (
          last_time () - 121);
        s.server_score <- s.server_score + 3;
        s
      else find_server ip (port-4)
  | _ -> raise Not_found

let udp_client_handler t p =
  let module M = DonkeyProtoServer in
  match t with
    Udp.QueryLocationReplyUdpReq t ->
(*      lprintf "Received location by UDP"; lprint_newline ();  *)
      let  s = udp_from_server p in
      List.iter (query_locations_reply s) t
      
  | Udp.QueryReplyUdpReq t ->
(*      lprintf "Received file by UDP"; lprint_newline ();  *)
      if !xs_last_search >= 0 then
        let ss = search_find !xs_last_search in
	let s = udp_from_server p in
	List.iter (fun t ->
        Hashtbl.add udp_servers_replies t.f_md4 s;
          search_handler ss [t]) t

  | Udp.PingServerReplyUdpReq t ->
      let module M = Udp.PingServerReplyUdp in
      let s = udp_from_server p in
      s.server_last_message <- last_time ();
      s.server_nfiles <- t.M.files;
      s.server_nusers <- t.M.users;
      (match t.M.max_users with Some x -> s.server_max_users <- x
      | None -> ());
      (match t.M.flags with Some x -> s.server_flags <- x
      | None -> ())

  | _ -> 
      lprintf "Unexpected UDP message: \n%s\n"
        (DonkeyProtoUdp.print t)
