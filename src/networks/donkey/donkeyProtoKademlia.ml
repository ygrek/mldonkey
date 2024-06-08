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
open Options

open AnyEndian
open BasicSocket
open LittleEndian

open CommonOptions
open CommonTypes
open CommonGlobals

open DonkeyMftp
open DonkeyOvernet
open DonkeyOptions

module P = struct

    let log_prefix = "[KAD]"

    let lprintf_nl fmt =
      lprintf_nl2 log_prefix fmt

    let lprintf_n fmt =
      lprintf2 log_prefix fmt

    let lprintf fmt =
      lprintf2 log_prefix fmt

    let names_of_tag =
      [
        "\243", Field_KNOWN "encryption"; (* 0xF3 *)
        "\248", Field_KNOWN "buddyhash"; (* 0xF8 *)
        "\249", Field_KNOWN "clientlowid"; (* 0xF9 *)
        "\250", Field_KNOWN "serverport"; (* 0xFA *)
        "\251", Field_KNOWN "serverip";   (* 0xFB *)
        "\252", Field_KNOWN "sourceuport"; (* 0xFC *)
        "\253", Field_KNOWN "sourceport"; (* 0xFD *)
        "\254", Field_KNOWN "sourceip"; (* 0xFE *)
        "\255", Field_KNOWN "sourcetype";  (* 0xFF *)
      ] @ file_common_tags

(* This fucking Emule implementation uses 4 32-bits integers instead of
  16 8-bits integers... welcome back to the non-portability problems... *)
    let get_md4 s pos =
      let ss = String.create 16 in

      ss.[0] <- s.[pos+3];
      ss.[1] <- s.[pos+2];
      ss.[2] <- s.[pos+1];
      ss.[3] <- s.[pos+0];

      ss.[4] <- s.[pos+7];
      ss.[5] <- s.[pos+6];
      ss.[6] <- s.[pos+5];
      ss.[7] <- s.[pos+4];

      ss.[8] <- s.[pos+11];
      ss.[9] <- s.[pos+10];
      ss.[10] <- s.[pos+9];
      ss.[11] <- s.[pos+8];

      ss.[12] <- s.[pos+15];
      ss.[13] <- s.[pos+14];
      ss.[14] <- s.[pos+13];
      ss.[15] <- s.[pos+12];

      Md4.direct_of_string (Bytes.to_string ss)

    let buf_md4 buf s =
      let s = Md4.direct_to_string s in

      let ss = String.create 16 in
      let pos = 0 in

      ss.[0] <- s.[pos+3];
      ss.[1] <- s.[pos+2];
      ss.[2] <- s.[pos+1];
      ss.[3] <- s.[pos+0];

      ss.[4] <- s.[pos+7];
      ss.[5] <- s.[pos+6];
      ss.[6] <- s.[pos+5];
      ss.[7] <- s.[pos+4];

      ss.[8] <- s.[pos+11];
      ss.[9] <- s.[pos+10];
      ss.[10] <- s.[pos+9];
      ss.[11] <- s.[pos+8];

      ss.[12] <- s.[pos+15];
      ss.[13] <- s.[pos+14];
      ss.[14] <- s.[pos+13];
      ss.[15] <- s.[pos+12];

      Buffer.add_bytes buf ss


(* Strange: why was the IP format changed for Kademlia ? *)
    let get_ip s pos =
      let c1 = int_of_char s.[pos] in
      let c2 = int_of_char s.[pos+1] in
      let c3 = int_of_char s.[pos+2] in
      let c4 = int_of_char s.[pos+3] in
      Ip.of_ints (c4, c3, c2, c1)

    let buf_ip buf ip =
      let (ip3,ip2,ip1,ip0) = Ip.to_ints ip in
      buf_int8 buf ip0;
      buf_int8 buf ip1;
      buf_int8 buf ip2;
      buf_int8 buf ip3

    let buf_peer buf p =
      buf_md4 buf p.peer_md4;
      buf_ip buf p.peer_ip;
      buf_int16 buf p.peer_port;
      buf_int16 buf p.peer_tcpport;
      buf_int8 buf p.peer_kind

    let write buf t =
      match t with
      | OvernetConnect p ->
          buf_int8 buf 0x00;
          buf_peer buf p

      | OvernetConnectReply peers ->
          buf_int8 buf 0x08;
          buf_int16 buf (List.length peers);
          List.iter (buf_peer buf) peers

      | OvernetPublicize p ->
          buf_int8 buf 0x10;
          buf_peer buf p

      | OvernetPublicized (Some p) ->
          buf_int8 buf 0x18;
          buf_peer buf p

      | OvernetSearch (nresults, target, Some uid) ->
          buf_int8 buf 0x20;
          buf_int8 buf (nresults land 0x1f);
          buf_md4 buf target;
          buf_md4 buf uid

      | OvernetSearchReply (md4, peers) ->
          buf_int8 buf 0x28;
          buf_md4 buf md4;
          buf_int8 buf (List.length peers);
          List.iter (buf_peer buf) peers

      | OvernetGetSearchResults (target, kind, min, max) ->
          buf_int8 buf 0x30;
          buf_md4 buf target;
          begin
            match kind with
            | Search_for_kind _
            | Search_for_file ->
                buf_int8 buf 1
            | Search_for_keyword None ->
                buf_int8 buf 0
            | Search_for_keyword (Some e) ->
                buf_int8 buf 1
          end

      | OvernetSearchFilesResults (target, results) ->
          buf_int8 buf 0x38;
          buf_md4 buf target;
          buf_int16 buf (List.length results);
          List.iter (fun (md4, tags) ->
              buf_md4 buf md4;
              buf_int8 buf (List.length tags);
              List.iter (fun tag ->
                  buf_tag buf tag names_of_tag
              ) tags
          ) results

      | OvernetPublishFiles (target, results) ->
          buf_int8 buf 0x40;
          buf_md4 buf target;
          buf_int16 buf (List.length results);
          List.iter (fun (md4, tags) ->
              buf_md4 buf md4;
              buf_int8 buf (List.length tags);
              List.iter (fun tag ->
                  buf_tag buf tag names_of_tag
              ) tags
          ) results

      | OvernetPublishSources _
      | OvernetSearchSourcesResults _
      | OvernetUnknown21 _
      | OvernetPeerNotFound _
      | OvernetFirewallConnectionNACK _
      | OvernetFirewallConnectionACK _
      | OvernetFirewallConnection (_, _)
      | OvernetGetMyIPResult _
      | OvernetGetMyIP _
      | OvernetNoResult _
      | OvernetPublished _
      | OvernetSearch (_,_, None)
      | OvernetPublicized None
      | OvernetGetMyIPDone -> raise MessageNotImplemented

      | OvernetUnknown (opcode, s) ->
          buf_int8 buf opcode;
          Buffer.add_string buf s

    let get_peer s pos =

      let md4 = get_md4 s pos in
      let ip = get_ip s (pos+16) in
      let udp_port = get_int16 s (pos + 20) in
      let tcp_port = get_int16 s (pos + 22) in
(*       let kind = get_uint8 s (pos + 24) in *)
      {
        peer_md4 = md4;
        peer_ip = ip;
        peer_port = udp_port;
        peer_tcpport = tcp_port;
        peer_country_code = Geoip.get_country_code_option ip;
        peer_kind = 3;
        peer_last_send = 0;
        peer_expire = 0;
        peer_created = last_time ();
      }, pos + 25

    let get_peers_from_results ip port answers =
      List.map (fun (r_md4, r_tags) ->
          let peer_ip = ref ip  in
          let peer_udpport = ref port in
          let peer_tcpport = ref 0 in
          let peer_kind = ref 0 in
          List.iter (fun tag ->
              match tag.tag_name with
                Field_KNOWN "sourceport" ->
                  for_int_tag tag (fun port ->
                      peer_tcpport := port)
              | Field_KNOWN "sourceuport" ->
                  for_int_tag tag (fun port ->
                      peer_udpport := port)
              | Field_KNOWN "sourceip" ->
                  for_int64_tag tag (fun ip ->
                      peer_ip := Ip.of_int64 ip
                  )
              | Field_KNOWN "sourcetype" ->
                  for_int_tag tag (fun kind ->
                      peer_kind := 3)
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
      ) answers

    let parse ip port opcode s =
      match opcode with
        0x00 ->
          let p, pos = get_peer s 0 in
          OvernetConnect p

      | 0x08 ->
(*	  let n = get_int16 s 0 in *)
          let peers, pos = get_list16 get_peer s 0 in
          OvernetConnectReply peers

      | 0x10 ->
          let p, pos = get_peer s 0 in
          OvernetPublicize p

      | 0x18 ->
          let p, pos = get_peer s 0 in
          OvernetPublicized (Some p)

      | 0x20 ->
          let nresults = (get_uint8 s 0) land 0x1f in
          let target = get_md4 s 1 in
          let uid = get_md4 s 17 in
          OvernetSearch (nresults, target, Some uid)

      | 0x28 ->
          let target = get_md4 s 0 in
          let peers, pos = get_list8 get_peer s 16 in
          OvernetSearchReply (target, peers)

      | 0x30 ->
          let target = get_md4 s 0 in
          let kind = get_uint8 s 16 in
          let kind =
            if String.length s = 17 then
              if kind = 1 then Search_for_file else
                Search_for_keyword None
            else
              Search_for_kind kind
          in
          OvernetGetSearchResults (target, kind, 0, 100)

      | 0x38 ->
          let target = get_md4 s 0 in
          let answers, pos = get_list16 (fun s pos ->
                let uid = get_md4 s pos in
                let tags, pos = get_list8 (get_tag names_of_tag)
                  s (pos + 16) in
                (uid, tags), pos
            ) s 16 in
          begin
            match answers with
              (_, first_tags) :: _ ->
                let sources = ref false in
                List.iter (fun tag ->
                    if tag.tag_name = Field_KNOWN "sourceport" then sources := true;
                ) first_tags;
                if !sources then
                  let peers = get_peers_from_results Ip.null 0 answers in
                  OvernetSearchSourcesResults (target, peers)

                else
                  OvernetSearchFilesResults (target, answers)
            | [] ->
                OvernetSearchFilesResults (target, answers)
          end

      | 0x40 ->
          let target = get_md4 s 0 in
          let answers, pos = get_list16 (fun s pos ->
                let uid = get_md4 s pos in
                let tags, pos = get_list8 (get_tag names_of_tag)
                  s (pos + 16) in
                (uid, tags), pos
            ) s 16 in

          begin
            match answers with
              (_, first_tags) :: _ ->
                let sources = ref false in
                List.iter (fun tag ->
                    if tag.tag_name = Field_KNOWN "sourceport" then sources := true;
                ) first_tags;
                if !sources then
                  let peers = get_peers_from_results ip port answers in
                  OvernetPublishSources (target, peers)

                else
                  OvernetPublishFiles (target, answers)
            | [] ->
                OvernetPublishFiles (target, answers)
          end

      | 0x48 ->
          let target = get_md4 s 0 in
          OvernetPublished target

(*
#define KADEMLIA_SRC_NOTES_RES  0x3A    // <HASH (key) [16]> <CNT1 [2]> (<HASH (answer) [16]> <CNT2 [2]> <META>*(CNT2))*(CNT1)
#define KADEMLIA_PUB_NOTES_REQ  0x42    // <HASH (key) [16]> <HASH (target) [16]> <CNT2 [2]> <META>*(CNT2))*(CNT1)
#define KADEMLIA_PUB_NOTES_RES  0x4A    // <HASH (key) [16]>
#define KADEMLIA_FIREWALLED_REQ 0x50    // <TCPPORT (sender) [2]>
#define KADEMLIA_FINDBUDDY_REQ  0x51    // <TCPPORT (sender) [2]>
#define KADEMLIA_CALLBACK_REQ   0x52    // <TCPPORT (sender) [2]>
#define KADEMLIA_FIREWALLED_RES 0x58    // <IP (sender) [4]>
#define KADEMLIA_FIREWALLED_ACK 0x59    // (null)
#define KADEMLIA_FINDBUDDY_RES  0x5A    // <TCPPORT (sender) [2]>
*)

      | _ ->
          OvernetUnknown (opcode, String.sub s 1 (String.length s - 1))

    let udp_buf = Buffer.create 2000

    let kademlia_header_code = char_of_int 0xE4
    let kademlia_packed_header_code = char_of_int 0xE5
    let kademlia_header = String.make 1 kademlia_header_code
    let kademlia_packed_header = Bytes.make 1 kademlia_packed_header_code

    let parse_message ip port pbuf =
      let len = Bytes.length pbuf in
      if len < 2 ||
        (let magic = Bytes.get pbuf 0 in
          magic <> kademlia_header_code &&
          magic <> kademlia_packed_header_code) then
        begin
          if !CommonOptions.verbose_unknown_messages then begin
              lprintf_nl "Received unknown UDP packet";
              dump_bytes pbuf;
            end;
          raise Not_found

        end
      else
      let magic = Bytes.get pbuf 0 in
      let opcode = int_of_char (Bytes.get pbuf 1) in
      let msg = Bytes.sub pbuf 2 (len-2) in
      let msg = if magic = kademlia_packed_header_code then
          let s = Zlib2.uncompress_string2 msg in
(*          lprintf "Uncompressed:\n";
          dump s; *)
          s
        else msg
      in
      let t = parse ip port opcode (Bytes.to_string msg) in
      t

    let udp_send sock ip port ping msg =
      try
        Buffer.reset udp_buf;
        write udp_buf msg;
        let s = Buffer.to_bytes udp_buf in

        let s =
          if Bytes.length s > 200 then
            let opcode = Bytes.sub s 0 1 in
            let args = Bytes.sub s 1 (Bytes.length s - 1) in
            Bytes.cat kademlia_packed_header (Bytes.cat opcode (Zlib2.compress_string args))
          else
            Bytes.cat kademlia_packed_header s
        in

        if !verbose_overnet then
          begin
            lprintf_nl "UDP to %s:%d op 0x%02X len %d type %s"
              (Ip.to_string ip) port (get_uint8_bytes s 1) (Bytes.length s) (message_to_string msg);
          end;
        (*
        let len = String.length s in
        let t = parse_message ip port s in
        if t <> msg then begin
            lprintf "********** SENT MESSAGE DIFFERS FROM EXPECTED ******\n";
          end;
*)

        UdpSocket.write sock ping s ip port
      with
      | MessageNotImplemented -> ()
      | e -> lprintf_nl "Exception %s in udp_send" (Printexc2.to_string e)

    let udp_handler f sock event =
      match event with
        UdpSocket.READ_DONE ->
          UdpSocket.read_packets sock (fun p ->
              try
                let pbuf = p.UdpSocket.udp_content in

                let (ip, port) =
                  match p.UdpSocket.udp_addr with
                    Unix.ADDR_INET (inet, port) ->
                      Ip.of_inet_addr inet, port
                  | _ -> assert false
                in
                let t = parse_message ip port pbuf in
                let is_not_banned ip =
                  match !Ip.banned (ip, None) with
                    None -> true
                   | Some reason ->
                  if !verbose_overnet then
                    lprintf_nl "%s blocked: %s" (Ip.to_string ip) reason;
                  false
                in
                if is_not_banned ip then f t p
              with e ->
                if !verbose_unknown_messages then
                begin
                  lprintf_nl "Error %s in udp_handler, dump of packet:"
                    (Printexc2.to_string e);
                  dump_bytes p.UdpSocket.udp_content;
                  lprint_newline ()
                end
          );
      | _ -> ()

    let checking_kind_timeout = 120

    let redirector_section = "DKKA"
    let options_section_name = "Kademlia"

    let enable_overnet = enable_kademlia
    let source_brand = false

    let overnet_section = file_section donkey_ini
        [ options_section_name ]
        "Kademlia options"

    let overnet_port =
      define_option overnet_section [options_section_name; "port"]
      "port for Kademlia"
        int_option (2000 + Random.int 20000)

    let overnet_tcpport = donkey_port
    let command_prefix = "kad_"

    let web_info = "kad"
    let web_info_descr = "Kad network boot peers"
  end

module Kademlia = Make(P)

