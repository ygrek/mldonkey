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
open BasicSocket
open TcpBufferedSocket

open CommonHosts
open CommonUploads
open CommonOptions
open CommonFile
open CommonTypes
open CommonGlobals

open FasttrackNetwork
open FasttrackTypes
open FasttrackGlobals
open FasttrackProto

let declare_shared s sh info hash =
  let num = info.shared_id in
  if not (Intset.mem num s.server_shared) then begin
      s.server_shared <- Intset.add num s.server_shared;
      let module M = TcpMessages in
      let sh = {
          M.shared_type = 0;
          M.shared_hash = hash;
          M.shared_checksum =
          Int64.of_int (fst_hash_checksum (Md5Ext.direct_to_string hash));
          M.shared_size = info.shared_size;
          M.shared_tags = [
            string_tag Field_Filename (Filename.basename sh.shared_codedname)
          ];
        } in
      server_send s M.DirectPacket (M.ShareFileReq sh)
    end

let update_shared_files () =
  CommonUploads.iter (fun sh ->
      let info = IndexedSharedFiles.get_result sh.shared_info in
      let uids = info.shared_uids in
      List.iter (fun uid ->
          match Uid.to_uid uid with
            Md5Ext hash ->
              List.iter (fun s ->
                  declare_shared s sh info hash
              ) !connected_servers
          | _ -> ()
      ) uids
  )

let declare_shared_files s =
  CommonUploads.iter (fun sh ->
      let info = IndexedSharedFiles.get_result sh.shared_info in
      let uids = info.shared_uids in
      List.iter (fun uid ->
          match Uid.to_uid uid with
            Md5Ext hash ->
                  declare_shared s sh info hash
          | _ -> ()
      ) uids
  )

let udp_packet_handler ip port msg =
  let h = H.new_host ip port Ultrapeer in
  H.host_queue_add active_udp_queue h (last_time ());
  H.connected h;
(*  if !verbose_udp then
    lprintf "Received UDP packet from %s:%d: \n%s\n"
      (Ip.to_string ip) port (Print.print msg);*)
  let s = new_server ip port in
  s.server_connected <- int64_time ()

let tcp_node_handler_hook = ref None

let server_msg_handler sock s addr t =
(*  lprintf "Message received: %d len %d\n" msg_type (String.length m); *)
  let module M = TcpMessages in
  if !verbose_msg_servers then begin
      lprintf "\nRECEIVED from supernode %s:%d: %s\n%s\n"
        (Ip.string_of_addr s.server_host.host_addr)
      s.server_host.host_port
        (M.string_of_path addr) (M.to_string t);
    end;
  (match !tcp_node_handler_hook with
      None -> ()
    | Some f -> f sock s addr t);
(* Here, we only take care of Supernode -> Node messages *)
  if addr = M.DirectPacket then
    match t with

    | M.NodeListReq list ->
        set_rtimeout sock (float_of_int Date.half_day_in_secs);
        set_server_state s (Connected (-1));
        s.server_connected <- int64_time ();
        if not (List.memq s !connected_servers) then
          connected_servers := s :: !connected_servers;

        List.iter (fun (ip,port,seen,slots) ->
            try
              ignore (H.new_host (Ip.addr_of_ip ip) port Ultrapeer)
            with Not_found -> ()
        ) list;

        if s.server_host.host_kind = IndexServer then
          close sock Closed_by_user
        else begin
            List.iter (fun file ->
                List.iter (fun ss ->
                    Fifo.put s.server_searches ss) file.file_searches
            ) !current_files;
          end

    | M.DeclareNeighbours list ->

(* Hum... receiving this messages means that we are connected to a supernode
AS A SUPERNODE, which is not good at this point, since we want to
be connected only AS A NODE. We should transfer this connection to
the FasttrackSupernode module, and get rid of it. *)

        set_rtimeout sock (float_of_int Date.half_day_in_secs);
        set_server_state s (Connected (-1));
        s.server_connected <- int64_time ();
        if not (List.memq s !connected_servers) then
          connected_servers := s :: !connected_servers;

        List.iter (fun n ->
            try
              let ip = n.M.neighbour_ip in
              let port = n.M.neighbour_port in
                ignore (H.new_host (Ip.addr_of_ip ip) port Ultrapeer)
            with Not_found -> ()
        ) list;

        if s.server_host.host_kind = IndexServer then
          close sock Closed_by_user
        else begin
            List.iter (fun file ->
                List.iter (fun ss ->
                    Fifo.put s.server_searches ss) file.file_searches
            ) !current_files;
          end

    | M.QueryReplyReq ( (s_ip, s_port), id, results) ->

        let s = Hashtbl.find searches_by_uid id in

        List.iter (fun (user, meta) ->
            let fuser = new_user (Known_location (
                  user.M.user_ip, user.M.user_port)) in
            fuser.user_nick <- user.M.user_name;
(*
          let url = Printf.sprintf
            "FastTrack://%s:%d/.hash=%s" (Ip.to_string user_ip)
            user_port (Md5Ext.to_string_case false result_hash) in *)
(*          let url = Printf.sprintf
"/.hash=%s" (Md5Ext.to_string_case false result_hash) in  *)

            let result_name = M.get_filename meta.M.meta_tags in

            begin
              match s.search_search with
                UserSearch (sss, _,_) ->

                  let rs = new_result
                      result_name
                      meta.M.meta_size
                      meta.M.meta_tags [meta.M.meta_hash] [] in
                  add_source rs fuser;
                  CommonInteractive.search_add_result false sss rs

              | FileUidSearch (file, file_hash) -> ()
            end;

            try
              let file = Hashtbl.find files_by_uid meta.M.meta_hash in
              let c = new_client fuser.user_kind in
              add_download file c ()(* (FileByUrl url) *);
              add_file_filenames (as_file file) result_name

            with _ -> ()
        ) results

    | M.NetworkStatsReq (stats, netname, nusers) ->
        begin
          match stats with
            [] -> ()
          | stats :: _ ->
              s.server_nusers <- stats.TcpMessages.nusers;
              s.server_nfiles <- stats.TcpMessages.nfiles;
              s.server_nkb <- stats.TcpMessages.nkb;
              server_must_update s;
        end

    | M.NetworkNameReq netname ->
        server_send s M.DirectPacket (M.NetworkNameReq network_name)

    | M.PingReq ->
        server_send s M.DirectPacket M.PongReq

    | M.Unknown_03 ->
        declare_shared_files s;

    | M.NetworkGlobalStats _
    | M.Unknown_2b _
    | M.AskUDPConnectionReq _
    | M.RandomFilesReq _
    | M.UnknownMessageReq (_, _)
    | M.UnknownReq (_, _)
    | M.Unknown_23 _
    | M.Unknown_1e _
    | M.SearchForwardReq (_, _, _)
    | M.SearchForward2Req (_, _, _)
    | M.SearchReq _
    | M.NodeInfoReq (_, _, _, _)
    | M.UnshareFileReq _
    | M.ShareFileReq _
    | M.ExternalIpReq _
    | M.PushRequestReq _
    | M.ProtocolVersionReq _
    | M.PongReq
    | M.QueryReplyEndReq _ -> ()

let udp_node_handler_hook = ref None

let udp_client_handler ip port p =
  let module M = UdpMessages in
  let t = M.parse p in
  if !verbose_udp  then
    lprintf "UDP PACKET FROM %s:%d:\n  %s\n"
      (Ip.to_string ip) port
      (M.to_string t);
  match !udp_node_handler_hook with
  | Some f -> f ip port t
  | None ->
      match t with
      | M.PingReq (min_enc_type, _, netname) -> ()
(*          udp_send ip port (M.NodePongReq (min_enc_type, netname)) *)
      | M.SupernodePongReq (min_enc_type, _, netname) ->
          UdpSocket.declare_pong ip
      | M.NodePongReq (min_enc_type, _) ->
          UdpSocket.declare_pong ip
      | M.UnknownReq _ -> ()

let declare_word _ = ()

(* media type

        MEDIA_TYPE_UNKNOWN  = 0x00,
        MEDIA_TYPE_AUDIO    = 0x01,
        MEDIA_TYPE_VIDEO    = 0x02,
        MEDIA_TYPE_IMAGE    = 0x03,
        MEDIA_TYPE_DOCUMENT = 0x04,
        MEDIA_TYPE_SOFTWARE = 0x05
  *)
