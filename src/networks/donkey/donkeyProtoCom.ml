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
open Options
open Printf2

open BasicSocket
open TcpBufferedSocket

open AnyEndian
open LittleEndian

open CommonOptions
open CommonTypes
open CommonGlobals
open CommonFile
open CommonGlobals

open DonkeyOptions
open DonkeyGlobals
open DonkeyTypes
open DonkeyMftp

let lprintf_nl () =
  lprintf "%s[EDK] "
    (log_time ()); lprintf_nl2

let lprintf_n () =
  lprintf "%s[EDK] "
    (log_time ()); lprintf

let buf = TcpBufferedSocket.internal_buf

let client_msg_to_string emule_version msg =
  Buffer.reset buf;
  buf_int8 buf 0;
  buf_int buf 0;
  let magic = DonkeyProtoClient.write emule_version buf msg in
  let s = Buffer.contents buf in
  let len = String.length s - 5 in
  s.[0] <- char_of_int magic;
  str_int s 1 len;
  s

let server_msg_to_string msg =
  Buffer.reset buf;
  buf_int8 buf 227;
  buf_int buf 0;
  DonkeyProtoServer.write buf msg;

  if !verbose_msg_servers then begin
      lprintf_nl () "MESSAGE TO SERVER:";
      DonkeyProtoServer.print msg;
      lprint_newline ();
    end;

  let s = Buffer.contents buf in
  let len = String.length s - 5 in
  str_int s 1 len;
  s

let server_send sock m =
(*
  lprintf "Message to server"; lprint_newline ();
  DonkeyProtoServer.print m;
*)
  write_string sock (server_msg_to_string m)

let direct_client_sock_send emule_version sock m =
  write_string sock (client_msg_to_string emule_version m)

let client_send c m =
  let emule_version = c.client_emule_proto in
  if !verbose_msg_clients || c.client_debug then begin
      lprintf_n () "Sent to client[%d] %s(%s)" (client_num c)
        c.client_name (brand_to_string c.client_brand);
      (match c.client_kind with
          Indirect_address _ | Invalid_address _ -> ()
        | Direct_address (ip,port) ->
            lprintf_nl2 " [%s:%d]" (Ip.to_string ip) port;

      );
      DonkeyProtoClient.print m;
      lprint_newline ();
    end;
  do_if_connected c.client_source.DonkeySources.source_sock (fun sock ->
      direct_client_sock_send emule_version sock m)

  (*
let emule_send sock m =
  let m = client_msg_to_string 0xc5 m in
  (*
  lprintf "Message to emule client:"; lprint_newline ();
  LittleEndian.dump m;
  lprint_newline ();
  lprint_newline (); *)
  write_string sock m
    *)

(* let client_msg_to_string m = client_msg_to_string 227 m *)

let servers_send socks m =
  let m = server_msg_to_string m in
  List.iter (fun s -> write_string s m) socks

let client_handler2 c ff f =
  let emule_version = match !c with
      None -> emule_proto ();
    | Some c -> c.client_emule_proto
  in
  let msgs = ref 0 in
  fun sock nread ->

    if !verbose then lprintf_nl () "between clients %d" nread;
    let module M= DonkeyProtoClient in
    let b = TcpBufferedSocket.buf sock in
    try
      while b.len >= 5 do
        let opcode = get_uint8 b.buf b.pos in
        let msg_len = get_int b.buf (b.pos+1) in
        if b.len >= 5 + msg_len then
          begin
            if !verbose then lprintf_nl () "client_to_client";
            let s = String.sub b.buf (b.pos+5) msg_len in
            buf_used b  (msg_len + 5);
            let t = M.parse emule_version opcode s in
(*          M.print t;
lprint_newline (); *)
            incr msgs;
            match !c with
              None ->
                c := ff t sock
            | Some c -> f c t sock
          end
        else raise Not_found
      done
    with Not_found -> ()

let cut_messages parse f sock nread =
  if !verbose then lprintf_nl () "server to client %d" nread;
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 5 do
      let opcode = get_uint8 b.buf b.pos in
      let msg_len = get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          if !verbose then lprintf_nl () "server_to_client";
          let s = String.sub b.buf (b.pos+5) msg_len in
          buf_used b (msg_len + 5);
          let t = parse opcode s in
          f t sock
        end
      else raise Not_found
    done
  with Not_found -> ()

let udp_send t ip port msg =

  if !verbose_udp then begin
      lprintf_nl () "Message UDP to %s:%d\n%s" (Ip.to_string ip) port
        (DonkeyProtoUdp.print msg);
    end;

  try
    Buffer.reset buf;
    DonkeyProtoUdp.write buf msg;
    let s = Buffer.contents buf in
    UdpSocket.write t false s ip port
  with e ->
      lprintf_nl () "Exception %s in udp_send" (Printexc2.to_string e)

let udp_handler f sock event =
  let module M = DonkeyProtoUdp in
  match event with
    UdpSocket.READ_DONE ->
      UdpSocket.read_packets sock (fun p ->
          try
            let pbuf = p.UdpSocket.udp_content in
            let len = String.length pbuf in
            if len > 0 then
              let t = M.parse (int_of_char pbuf.[0])
                (String.sub pbuf 1 (len-1)) in
(*              M.print t; *)
              f t p
          with e -> ()
      ) ;
  | _ -> ()

let udp_basic_handler f sock event =
  match event with
    UdpSocket.READ_DONE ->
      UdpSocket.read_packets sock (fun p ->
          try
            let pbuf = p.UdpSocket.udp_content in
            let len = String.length pbuf in
            if len = 0 ||
              int_of_char pbuf.[0] <> DonkeyOpenProtocol.udp_magic then begin
                if !verbose_unknown_messages then begin
                    lprintf_nl () "Received unknown UDP packet";
                    dump pbuf;
                  end;
              end else begin
                let t = String.sub pbuf 1 (len-1) in
                f t p
              end
          with e ->
              lprintf_nl () "Error %s in udp_basic_handler"
                (Printexc2.to_string e)
      ) ;
  | _ -> ()


let new_string msg s =
  let len = String.length s - 5 in
  str_int s 1 len

let empty_string = ""

let tag_file file =
  (string_tag Field_Filename
    (
      let name = file_best_name file in
      let name = if String2.starts_with name "hidden." then
          String.sub name 7 (String.length name - 7)
        else name in
      if !verbose then lprintf_nl () "Sharing %s" name;
      name
    ))::
  (int64_tag Field_Size file.file_file.impl_file_size) ::
  (
    (match file.file_format with
        FormatNotComputed next_time when
        next_time < last_time () ->
          (try
              if !verbose then lprintf_nl () "%s: Find format %s"
                    (string_of_date (last_time ()))
                  (file_disk_name file);
              file.file_format <- (
                match
                CommonMultimedia.get_info
                    (file_disk_name file)
                with
                  FormatUnknown -> FormatNotComputed (last_time () + 300)
                | x -> x)
            with _ -> ())
      | _ -> ()
    );

    match file.file_format with
      FormatNotComputed _ | FormatUnknown -> []
    | AVI _ ->
        [
          { tag_name = Field_Type; tag_value = String "Video" };
          { tag_name = Field_Format; tag_value = String "avi" };
        ]
    | MP3 _ ->
        [
          { tag_name = Field_Type; tag_value = String "Audio" };
          { tag_name = Field_Format; tag_value = String "mp3" };
        ]
    | OGG l ->
        begin
          let max_st = ref OGG_INDEX_STREAM in
          List.iter (fun st ->
            if st.stream_type > !max_st
            then max_st := st.stream_type;
          ) l;
          match !max_st with
              OGG_AUDIO_STREAM
            | OGG_VORBIS_STREAM ->
                [
                 { tag_name = Field_Type; tag_value = String "Audio" };
                 { tag_name = Field_Format; tag_value = String "ogg" };
                ]
            | OGG_VIDEO_STREAM
            | OGG_THEORA_STREAM ->
                [
                 { tag_name = Field_Type; tag_value = String "Video" };
                 { tag_name = Field_Format; tag_value = String "ogg" };
                ]
            | _ -> []
        end
    | FormatType (format, kind) ->
        [
          { tag_name = Field_Type; tag_value = String kind };
          { tag_name = Field_Format; tag_value = String format };
        ]
  )

(* Computes tags for shared files (for clients) *)
let make_tagged sock files =
  (List2.tail_map (fun file ->
      {
        f_md4 = file.file_md4;
        f_ip = client_ip sock;
        f_port = !!donkey_port;
        f_tags = tag_file file;
      }
    ) files)

(* Computes tags for shared files with the special ip and
   port values for newer servers. We should assume that the
   server is newer if it supports compression, like emule
   does it. *)
let make_tagged_server newer_server sock files =
  if newer_server then
      (List2.tail_map (fun file ->
          {
            f_md4 = file.file_md4;
            f_ip = (if (file_state file = FileShared) then Ip.of_string "251.251.251.251" else Ip.of_string "252.252.252.252");
            f_port = (if (file_state file = FileShared) then 0xFBFB else 0xFCFC);
            f_tags = tag_file file;
          }
      ) files)
  else
    make_tagged sock files

let server_send_share compressed sock msg =
  if !verbose then
      lprintf_nl () "Sending %d file(s) to server" (List.length msg);
  let max_len =
    !!client_buffer_size - 100
      - TcpBufferedSocket.remaining_to_write sock
  in
  Buffer.reset buf;
  let s =
      buf_int buf 0;
      let nfiles, prev_len =
        DonkeyProtoServer.Share.write_files_max buf
          ( make_tagged_server compressed (Some sock) msg )
          0 max_len
      in
      let s = Buffer.contents buf in
      str_int s 0 nfiles;
      let s = String.sub s 0 prev_len in
      if !verbose_share then
         lprintf_nl () "Sending %d share(s) to server : " nfiles;
      Buffer.reset buf;
      let s_c =
        if compressed && Autoconf.has_zlib then
          Autoconf.zlib__compress_string s
        else
          s
      in
      (* Emule only sends the string compressed when it
         is smaller in that state. *)
      if compressed && ((String.length s_c) < (String.length s))  then
        begin
          if !verbose_share then
            lprintf_nl () "Using zlib";
          buf_int8 buf 0xD4;
          buf_int buf 0;
          buf_int8 buf 21; (* ShareReq *)
          Buffer.add_string buf s_c;
          Buffer.contents buf
        end
      else
        begin
          if !verbose_share then
            lprintf_nl () "No compression";
          buf_int8 buf 227;
          buf_int buf 0;
          buf_int8 buf 21; (* ShareReq *)
          Buffer.add_string buf s;
          Buffer.contents buf
        end
  in
  let len = String.length s - 5 in
  str_int s 1 len;
  write_string sock s

let client_send_files sock msg =
  let max_len = !!client_buffer_size - 100 -
    TcpBufferedSocket.remaining_to_write sock in
  Buffer.reset buf;
  buf_int8 buf 227;
  buf_int buf 0;
  buf_int8 buf 75; (* ViewFilesReply *)
  buf_int buf 0;
  let nfiles, prev_len = DonkeyProtoClient.ViewFilesReply.write_files_max buf (
      make_tagged (Some sock) msg)
    0 max_len in
  let s = Buffer.contents buf in
  let s = String.sub s 0 prev_len in
  let len = String.length s - 5 in
  str_int s 1 len;
  str_int s 6 nfiles;
  write_string sock s

let udp_server_send s t =
  udp_send (get_udp_sock ()) s.server_ip (s.server_port+4) t
