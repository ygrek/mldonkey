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
open AnyEndian
open BasicSocket
open CommonOptions
open Options
open DonkeyOptions
open CommonTypes
open LittleEndian
open CommonGlobals
open DonkeyMftp
open DonkeyOvernet

module Proto = struct

    let log_prefix = "[OV]"

    let lprintf_nl fmt =
      lprintf_nl2 log_prefix fmt

    let lprintf_n fmt =
      lprintf2 log_prefix fmt

    let lprintf fmt =
      lprintf2 log_prefix fmt

   let names_of_tag = [
      "loc", Field_KNOWN "loc";
      ] @ file_common_tags

    let buf_peer buf p =
      buf_md4 buf p.peer_md4;
      buf_ip buf p.peer_ip;
      buf_int16 buf p.peer_port;
      buf_int8 buf p.peer_kind

    let get_peer s pos =
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos+16) in
      let port = get_int16 s (pos+20) in
(*      let kind = get_uint8 s (pos+22) in *)
      {
        peer_md4 = md4;
        peer_ip = ip;
        peer_port = port;
        peer_tcpport = 0;
        peer_country_code = Geoip.get_country_code_option ip;
        peer_kind = 3;
        peer_last_send = 0;
        peer_expire = 0;
        peer_created = last_time ();
      }, pos + 23

    let write buf t =
      match t with
      | OvernetConnect p ->
          buf_int8 buf 10;
          buf_md4 buf p.peer_md4;
          buf_ip buf p.peer_ip;
          buf_int16 buf p.peer_port;
          buf_int8 buf p.peer_kind

      | OvernetConnectReply peers ->
          buf_int8 buf 11;
          buf_list16 buf_peer buf peers

      | OvernetPublicize p ->
          buf_int8 buf 12;
          buf_md4 buf p.peer_md4;
          buf_ip buf p.peer_ip;
          buf_int16 buf p.peer_port;
          buf_int8 buf p.peer_kind

      | OvernetPublicized p ->
          buf_int8 buf 13

      | OvernetSearch (kind, md4, _) ->
          buf_int8 buf 14;
          buf_int8 buf kind;
          buf_md4 buf md4

      | OvernetSearchReply (md4, peers) ->
          buf_int8 buf 15;
          buf_md4 buf md4;
          buf_list8 buf_peer buf peers

      | OvernetGetSearchResults (md4, kind, min, max) ->
          buf_int8 buf 16;
          buf_md4 buf md4;
          buf_int8 buf 0; (* the kind in overnet seems to always be 0 *)
(*          buf_int8 buf kind; *)
          buf_int16 buf min;
          buf_int16 buf max

      | OvernetSearchFilesResults (md4, [r_md4, r_tags]) ->
          buf_int8 buf 17;
          buf_md4 buf md4;
          buf_md4 buf r_md4;
          buf_tags buf r_tags names_of_tag

      | OvernetSearchSourcesResults (md4, [p]) ->
(*          buf_int8 buf 17;
          buf_md4 buf md4;
          buf_md4 buf r_md4;
          buf_tags buf r_tags names_of_tag
*) ()

      | OvernetNoResult md4 ->
          buf_int8 buf 18;
          buf_md4 buf md4

      | OvernetPublishFiles (md4, [r_md4, r_tags]) ->
          buf_int8 buf 19;
          buf_md4 buf md4;
          buf_md4 buf r_md4;
          buf_tags buf r_tags names_of_tag

          (*
      | OvernetPublishSources (md4, [p]) ->
          buf_int8 buf 19;
          buf_md4 buf md4;
          buf_md4 buf p.peer_md4;
          buf_tags buf r_tags names_of_tag
*)

      | OvernetPublished md4 ->
          buf_int8 buf 20;
          buf_md4 buf md4

      | OvernetGetMyIP port ->
          buf_int8 buf 27;
          buf_int16 buf port

      | OvernetGetMyIPResult (ip) ->
          buf_int8 buf 28;
          buf_ip buf ip

      | OvernetGetMyIPDone ->
          buf_int8 buf 29

      | OvernetFirewallConnection(md4,port) ->
          buf_int8 buf 24;
          buf_md4 buf md4;
          buf_int16 buf port

      | OvernetFirewallConnectionACK(md4) ->
          buf_int8 buf 25;
          buf_md4 buf md4

      | OvernetFirewallConnectionNACK(md4) ->
          buf_int8 buf 26;
          buf_md4 buf md4

      | OvernetPeerNotFound peer ->
          buf_int8 buf 33;
          buf_peer buf peer

      | OvernetUnknown21 peer ->
          buf_int8 buf 21;
          buf_peer buf peer

      | OvernetUnknown (opcode, s) ->
          buf_int8 buf opcode;
          Buffer.add_string buf s
      | OvernetSearchFilesResults _
      | OvernetPublishFiles _
      | OvernetPublishSources _
      | OvernetSearchSourcesResults _ ->
          raise MessageNotImplemented

    let get_peer_from_result ip port r_md4 r_tags =
      let peer_ip = ref ip in
      let peer_udpport = ref port in
      let peer_tcpport = ref 0 in
      List.iter (fun tag ->
          match tag.tag_name with
            Field_KNOWN "loc" ->
              for_string_tag tag (fun bcp ->
                  if String2.starts_with bcp "bcp://" then
                    let bcp2 = String.sub bcp 6 (String.length bcp - 6)
                    in
                    match String2.split_simplify bcp2 ':' with
                    | [_;ip;udpport;tcpport] ->
                        peer_ip := Ip.of_string ip;
                        peer_udpport := int_of_string udpport;
                        peer_tcpport := int_of_string tcpport;

                    | [_;ip;port] -> ()
(* FIXME: A firewalled peer...
                                peer_ip := Ip.of_string ip;
                                peer_tcpport := int_of_string port;
                                *)

                    | [ip;port] ->
                        peer_ip := Ip.of_string ip;
                        peer_tcpport := int_of_string port;
                    | _ ->
                        if !verbose_overnet then
                          lprintf_nl "Ill formed bcp: [%s]" bcp;
                  else
                    if !verbose_overnet then
                      lprintf_nl "Ill formed bcp: [%s]" bcp;
              )
          | _ ->
            if !verbose_unknown_messages then
              lprintf_nl "Unused source tag [%s]"
                (escaped_string_of_field tag)
      ) r_tags;
      {
          peer_ip = !peer_ip;
          peer_port = !peer_udpport;
          peer_tcpport = !peer_tcpport;
          peer_country_code = Geoip.get_country_code_option !peer_ip;
          peer_md4 = r_md4;
          peer_last_send = 0;
          peer_expire = 0;
          peer_kind = 3;
          peer_created = last_time ();
        }

    let parse ip port opcode s =
      try
        match opcode with
        | 10 ->
            let md4 = get_md4 s 0 in
            let ip = get_ip s 16 in
            let port = get_int16 s 20 in
(*	    let kind = get_uint8 s 22 in *)
            OvernetConnect {
              peer_md4 = md4;
              peer_ip = ip;
              peer_port = port;
              peer_country_code = Geoip.get_country_code_option ip;
              peer_kind = 3;
              peer_tcpport = 0;
              peer_last_send = 0;
              peer_expire = 0;
              peer_created = last_time ();
            }
        | 11 ->
            let peers, pos = get_list16 get_peer s 0 in
            OvernetConnectReply peers
        | 12 ->
            let md4 = get_md4 s 0 in
            let ip = get_ip s 16 in
            let port = get_int16 s 20 in
(*	    let kind = get_uint8 s 22 in *)
            OvernetPublicize {
              peer_md4 = md4;
              peer_ip = ip;
              peer_port = port;
              peer_country_code = Geoip.get_country_code_option ip;
              peer_kind = 3;
              peer_tcpport = 0;
              peer_last_send = 0;
              peer_expire = 0;
              peer_created = last_time ();
            }
        | 13 ->
            OvernetPublicized None
        | 14 ->
            let kind = get_uint8 s 0 in
            let md4 = get_md4 s 1 in
            OvernetSearch (kind, md4, None)
        | 15 ->
            let md4 = get_md4 s 0 in
            let peers, pos = get_list8 get_peer s 16 in
            OvernetSearchReply (md4, peers)
        | 16 ->
            let md4 = get_md4 s 0 in
            let kind = get_uint8 s 16 in
            let min = get_int16 s 17 in
            let max = get_int16 s 19 in
            OvernetGetSearchResults (md4, Search_for_kind kind, min, max)
        | 17 ->
            let md4 = get_md4 s 0 in
            let r_md4 = get_md4 s 16 in
            let r_tags, pos = get_tags s 32 names_of_tag in
            let sources = ref false in
            List.iter (fun tag ->
                if tag.tag_name = Field_KNOWN "loc" then sources := true;
            ) r_tags;
            if !sources then
              let peer = get_peer_from_result ip port r_md4 r_tags in
              OvernetSearchSourcesResults (md4, [peer])
            else
              OvernetSearchFilesResults (md4, [r_md4, r_tags])
        | 18 ->
            let md4 = get_md4 s 0 in
            OvernetNoResult md4
        | 19 ->
            let md4 = get_md4 s 0 in
            let r_md4 = get_md4 s 16 in
            let r_tags, pos = get_tags s 32 names_of_tag in
            let sources = ref false in
            List.iter (fun tag ->
                if tag.tag_name = Field_KNOWN "loc" then sources := true;
            ) r_tags;
            if !sources then
              let peer = get_peer_from_result ip port r_md4 r_tags in
              OvernetPublishSources (md4, [peer])
            else
              OvernetPublishFiles (md4, [r_md4, r_tags])
        | 20 ->
            let md4 = get_md4 s 0 in
            OvernetPublished md4
        | 21 ->
(* idem as 33, but IP seem to be a low ID *)
            if !verbose_overnet then begin
                lprintf_nl "Received code %d message. Dump:" opcode;
                dump s;
                lprint_newline ();
              end;
            let peer, _ = get_peer s 0 in
            OvernetUnknown21 peer
        | 24 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: OVERNET FIREWALL CONNECTION (24)";
            let md4 = get_md4 s 0 in
            let port = get_int16 s 16 in
            OvernetFirewallConnection(md4,port)
        | 25 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: OVERNET FIREWALL CONNECTION ACK (25)";
            let md4 = get_md4 s 0 in
            OvernetFirewallConnectionACK(md4)
        | 26 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: OVERNET FIREWALL CONNECTION NACK (26)";
            let md4 = get_md4 s 0 in
            OvernetFirewallConnectionNACK(md4)
        | 27 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: GETMYIP MESSAGE (27)";
            OvernetGetMyIP (get_int16 s 0)
        | 28 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: GETMYIPRESULT MESSAGE (28)";
            let ip = get_ip s 0 in
            OvernetGetMyIPResult (ip)
        | 29 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: GETMYIPDONE MESSAGE (29)";
            OvernetGetMyIPDone
        | 33 ->
            if !verbose_overnet then
              lprintf_nl "RCVD: PEER NOT FOUND (33)";
            let peer, _ = get_peer s 0 in
            OvernetPeerNotFound peer
        | _ ->
            if !verbose_unknown_messages then
              begin
                lprintf_nl "unknown opcode %d" opcode;
                dump s;
                lprint_newline ();
              end;
            OvernetUnknown (opcode, s)
      with e ->
          if !verbose_unknown_messages then
            begin
              lprintf_nl "Error %s while parsing opcode %d" (Printexc2.to_string e) opcode;
              dump s;
              lprint_newline ();
            end;
          OvernetUnknown (opcode, s)

    let udp_handler f sock event =
      match event with
        UdpSocket.READ_DONE ->
          UdpSocket.read_packets sock (fun p ->
              try
                let pbuf = p.UdpSocket.udp_content in
                let len = Bytes.length pbuf in
                if len < 2 ||
                  int_of_char (Bytes.get pbuf 0) <> 227 then
                  begin
                    if !verbose_unknown_messages then begin
                        lprintf_nl "Received unknown UDP packet";
                        dump_bytes pbuf;
                      end
                  end
                else
                  begin
                    let (ip, port) =
                      match p.UdpSocket.udp_addr with
                        Unix.ADDR_INET (inet, port) ->
                          Ip.of_inet_addr inet, port
                      | _ -> assert false
                    in
                    let t = parse ip port (int_of_char (Bytes.get pbuf 1)) (Bytes.to_string (Bytes.sub pbuf 2 (len-2))) in
                    let is_not_banned ip =
                      match !Ip.banned (ip, None) with
                         None -> true
                       | Some reason ->
                      if !verbose_overnet then
                        lprintf_nl "%s blocked: %s" (Ip.to_string ip) reason;
                      false
                    in
                    if is_not_banned ip then f t p
                  end
              with e ->
                if !verbose_unknown_messages then begin
                  lprintf_nl "Error %s in udp_handler, dump of packet:"
                    (Printexc2.to_string e);
                  dump_bytes p.UdpSocket.udp_content;
                  lprint_newline ()
                end
          );
      | _ -> ()

    let checking_kind_timeout = 180

    let redirector_section = "DKKO"
    let options_section_name = overnet_options_section_name
    let overnet_section = overnet_section
    let overnet_port = overnet_port
    let overnet_tcpport = overnet_port

    let udp_buf = Buffer.create 2000

    let udp_send sock ip port ping msg =
      try
        Buffer.reset udp_buf;
        buf_int8 udp_buf 227;
        write udp_buf msg;
        let s = Buffer.to_bytes udp_buf in
        if !verbose_overnet then
          begin
            lprintf_nl "UDP to %s:%d op 0x%02X len %d type %s"
              (Ip.to_string ip) port (get_uint8_bytes s 1) (Bytes.length s) (message_to_string msg);
          end;
        UdpSocket.write sock ping s ip port
      with e ->
          lprintf_nl "Exception %s in udp_send" (Printexc2.to_string e)

    let udp_send sock ip port ping msg =
      match msg with
      | OvernetSearchFilesResults (target, ((_ :: _ :: _) as results)) ->
          List.iter (fun r ->
              udp_send sock ip port ping
                (OvernetSearchFilesResults (target, [r]))
          ) results
      | OvernetSearchSourcesResults (target, ((_ :: _ :: _) as results)) ->
          List.iter (fun r ->
              udp_send sock ip port ping
                (OvernetSearchSourcesResults (target, [r]))
          ) results
      | OvernetPublishFiles (target, ((_ :: _ :: _) as results)) ->
          List.iter (fun r ->
              udp_send sock ip port ping
                (OvernetPublishFiles (target, [r]))
          ) results
      | OvernetPublishSources (target, ((_ :: _ :: _) as results)) ->
          List.iter (fun r ->
              udp_send sock ip port ping
                (OvernetPublishSources (target, [r]))
          ) results
      | _ -> udp_send sock ip port ping msg

    let enable_overnet = enable_overnet
    let command_prefix = "ov_"
    let source_brand = true
    let web_info = "ocl"
    let web_info_descr = "Overnet network boot peers"
  end

module Overnet_initial = Make(Proto)

let overnet_protocol_connect_version =
  define_option overnet_section [
    Proto.options_section_name;
    "protocol_connect_version"]
    "The protocol version sent on Overnet connections"
    int_option 1044

let overnet_protocol_connectreply_version =
  define_option overnet_section [Proto.options_section_name; "protocol_connectreply_version"]
    "The protocol version sent on Overnet connections replies"
    int_option 44


(* In Overnet case, the TCP protocol is changed, so we need to create a special
TCP port for Overnet clients, that will not be used by normal Edonkey
and Kademlia clients. *)

let tcp_sock = ref None

module Overnet = struct
    include Overnet_initial

    let enable enabler =
      enable enabler;
      if !!enable_overnet then begin
      try
        let sock = TcpServerSocket.create
            "overnet client server"
            (Ip.to_inet_addr !!client_bind_addr)
          (!!overnet_port)
          (DonkeyClient.client_connection_handler true) in

        tcp_sock := Some sock;

        match Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock)) with
          Unix.ADDR_INET (ip, port) ->
            assert (port = !!overnet_port)
        | _ -> failwith "Bad socket address"
      with e ->
          lprintf_nl "[Overnet] Could not assign TCP port %d for Overnet" !!overnet_port;
          tcp_sock := None
      end

    let disable () =
      disable ();
      (match !tcp_sock with
          None -> ()
        | Some sock ->
            tcp_sock := None;
            TcpServerSocket.close sock Closed_by_user);
end
