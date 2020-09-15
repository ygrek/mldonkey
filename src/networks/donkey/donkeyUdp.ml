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

open CommonSearch
open CommonTypes
open Options
open BasicSocket
open DonkeyOneFile
open DonkeyProtoCom
open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions
open DonkeyOptions
open CommonOptions
open DonkeyClient
open CommonGlobals

module Udp = DonkeyProtoUdp

let udp_server_send_query_location s l =
  if s.server_has_get_sources2 then
    udp_server_send s (Udp.QueryLocationUdpReq2 l)
  else
    udp_server_send s (Udp.QueryLocationUdpReq (List.map (fun (md4,_) -> md4) l))

let search_handler s t =
  let waiting = s.search_waiting - 1 in
  s.search_waiting <- waiting;
  List.iter (fun f ->
      search_found false s f.f_md4 f.f_tags
  ) t
(*  search.search_handler (Waiting s.search_waiting) *)

let make_xs ss =
  if !verbose_udp then lprintf "******** make_xs ********\n";
  if ss.search_num <> !xs_last_search then
    begin
      xs_last_search := ss.search_num;
      xs_servers_list := Hashtbl2.to_list servers_by_key;
    end;

  let cut_for_udp_send max_servers list =
    let min_last_conn = last_time () - 8 * 3600 in
    let rec iter list n left =
      if n = 0 then
        left, list
      else
        match list with
            [] -> left, []
            | s :: tail ->
                if connection_last_conn s.server_connection_control > min_last_conn then
                  iter tail (n-1) (s :: left)
                else
                  iter tail n left
    in
    iter list max_servers []
  in
  let before, after = cut_for_udp_send !!max_xs_packets !xs_servers_list in
  xs_servers_list := after;

  List.iter (fun s ->
      match s.server_sock with
        Connection _ -> ()
      | _ ->
          let module M = DonkeyProtoServer in
          let module Q = M.Query in
          udp_server_send s (
(* By default, send the MultipleUdp !!! we have to set
server_send_multiple_replies to true by default, and change it to false
when receiving an old ping.

  if server_send_multiple_replies s then
              Udp.QueryUdpReq ss.search_query
    else *)
              Udp.QueryMultipleUdpReq ss.search_query);
  ) before;

  if !verbose_overnet then lprintf "===================== STARTING SEARCH ON OVERNET =========\n";
  DonkeyProtoOvernet.Overnet.overnet_search ss;
  DonkeyProtoKademlia.Kademlia.overnet_search ss

let extent_search () =
  try
    if !xs_last_search >= 0 then  begin
        try
          make_xs (search_find !xs_last_search)
        with _ -> ()
      end;

(*
start removed by savannah patch #3616
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
                Connection _ -> ()
              | _ ->
        if
    connection_last_conn s.server_connection_control + 3600*8 > last_time () &&
    s.server_next_udp <= last_time () then begin
      (if server_accept_multiple_getsources s then
        new_servers := s :: !new_servers
      else
        old_servers := s :: !old_servers);
      incr nservers;
    end
            );
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
                      | Connection _ -> ()
                      | _ -> udp_query_locations file s
      ) !old_servers;
    end
    ) !current_files;

    List.iter (fun s ->
        s.server_next_udp <- last_time () + !!min_reask_delay
        ) !old_servers;
    if !udp_servers_list = [] then
          udp_servers_list := Hashtbl2.to_list servers_by_key;

end removed by savannah patch #3616
*)

  with e ->
      lprintf "extent_search: %s\n" (Printexc2.to_string e)

let add_user_friend s u =
  let kind  =
    if Ip.usable u.user_ip then
      Direct_address (u.user_ip, u.user_port)
    else
        begin
          ( match s.server_sock, server_state s with
              Connection sock, (Connected _ |Connected_downloading _) ->
                query_id s.server_ip s.server_port (id_of_ip u.user_ip)
              | _ -> ()
          );
          Invalid_address (u.user_name, Md4.to_string u.user_md4)
        end
  in
  let c = new_client kind None in
  c.client_tags <- u.user_tags;
  set_client_name c u.user_name u.user_md4;
  friend_add c

let udp_client_handler t p =
  if !verbose_udp then
    lprintf_nl "Received UDP message:\n%s" (Udp.print t);

  let udp_from_server p =
    match p.UdpSocket.udp_addr with
      | Unix.ADDR_INET(ip, port) ->
          let ip = Ip.of_inet_addr ip in
          let s =
            if !!update_server_list_server then
              check_add_server ip (port-4) 
            else 
              find_server ip (port-4) in
            (* set last_conn, but add a 2 minutes offset to prevent
               staying connected to this server *)
            connection_set_last_conn s.server_connection_control (
              last_time () - 121);
            s.server_score <- s.server_score + 3;
            s.server_failed_count <- 0;
            s
      | _ -> raise Not_found
  in
  match t with
  | Udp.QueryLocationReplyUdpReq t ->
      (* lprintf "Received location by UDP\n"; *)
      let  s = udp_from_server p in
      List.iter (query_locations_reply s) t

  | Udp.QueryReplyUdpReq t ->
      (* lprintf "Received file by UDP\n"; *)
      if !xs_last_search >= 0 then
        let ss = search_find !xs_last_search in
        let s = udp_from_server p in
        List.iter (fun t ->
          Hashtbl.add udp_servers_replies t.f_md4 s;
          search_handler ss [t]
        ) t

  | Udp.PingServerReplyUdpReq t ->
      let s = udp_from_server p in
      let module M = Udp.PingServerReplyUdp in
      let check_challenge, challenge_v =
        match s.server_udp_ping_challenge with
        | Some challenge when challenge = t.M.challenge -> true, challenge
        | Some challenge -> false, challenge
        | _ -> false, 0L
      in
      if check_challenge then begin
      UdpSocket.declare_pong s.server_ip;
      let now = Unix.gettimeofday() in
      s.server_ping <- int_of_float ((now -. s.server_last_ping) *. 1000.);
      s.server_udp_ping_challenge <- None;
      s.server_has_get_sources <- t.M.get_sources;
      s.server_has_get_files <- t.M.get_files;
      s.server_has_newtags <- t.M.newtags;
      s.server_has_unicode <- t.M.unicode;
      s.server_has_get_sources2 <- t.M.get_sources2;
      s.server_has_largefiles <- t.M.largefiles;
      (match s.server_obfuscation_udp with
        | None -> if t.M.udp_obfuscation then s.server_obfuscation_udp <- Some 0
        | Some p -> if not t.M.udp_obfuscation then s.server_obfuscation_udp <- None);
      (match s.server_obfuscation_tcp with
        | None -> if t.M.tcp_obfuscation then s.server_obfuscation_tcp <- Some 0
        | Some p -> if not t.M.tcp_obfuscation then s.server_obfuscation_tcp <- None);
      if t.M.files > 0L then s.server_nfiles <- Some t.M.files;
      if t.M.users > 0L then s.server_nusers <- Some t.M.users;
      (match t.M.max_users with
           Some x when x > 0L -> s.server_max_users <- Some x
         | _ -> ());
      (match t.M.lowid_users with
           Some x when x > 0L -> s.server_lowid_users <- Some x
         | _ -> ());
      (match t.M.soft_limit with
           Some x when x > 0L -> s.server_soft_limit <- Some x
         | _ -> ());
      (match t.M.hard_limit with
           Some x when x > 0L -> s.server_hard_limit <- Some x
         | _ -> ());
      server_must_update s
      end else
        begin
          lprintf_nl "received PingServerReply from %s with invalid challenge: %Ld <> %Ld"
            (string_of_server s) challenge_v t.M.challenge;
          s.server_udp_ping_challenge <- None;
        end

  | Udp.ServerDescReplyUdpReq t ->
      let module M = Udp.ServerDescReplyUdp in
      let s = udp_from_server p in
      let check_challenge, challenge_v =
        match s.server_udp_desc_challenge with
        | Some challenge when challenge = t.M.challenge -> true, challenge
        | Some challenge -> false, challenge
        | _ -> false, 0L
      in
      if check_challenge then begin
      s.server_name <- t.M.name;
      s.server_description <- t.M.desc;
      s.server_udp_desc_challenge <- None;
      List.iter (fun tag ->
          match tag with
              { tag_name = Field_KNOWN "version"; tag_value = Uint64 i } ->
                let i = Int64.to_int i in
                s.server_version <- Printf.sprintf "%d.%d" (i lsr 16) (i land 0xFFFF);
            | { tag_name = Field_KNOWN "auxportslist" ; tag_value = String aux } ->
                s.server_auxportslist <- aux
            |  { tag_name = Field_KNOWN "dynip" ; tag_value = String dynip } ->
                s.server_dynip <- dynip
            | _ -> ()
      ) t.M.tags;

      if s.server_tags = [] then
         s.server_tags <- t.M.tags;

      server_must_update s
      end else
        begin
          lprintf_nl "received ServerDescReply from %s with invalid challenge: %Ld <> %Ld"
            (string_of_server s) challenge_v t.M.challenge;
          s.server_udp_desc_challenge <- None;
        end

  | Udp.EmuleReaskFilePingUdpReq t -> ()

  | Udp.EmulePortTestReq ->
      (match !porttest_sock with
        None -> ()
      | Some sock ->
          let s = Buffer.create 10 in
          DonkeyProtoUdp.write s Udp.EmulePortTestReq;
          TcpBufferedSocket.write_string sock (Buffer.contents s);
          porttest_sock := None)

  | _ ->
      if !verbose_unexpected_messages then
        lprintf "Unexpected UDP message: %s\n"
            (DonkeyProtoUdp.print t)
