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

open Int64ops
open Printf2
open Md4
open TcpBufferedSocket

open CommonTypes
open CommonOptions
open CommonShared

open FasttrackTypes
open FasttrackGlobals

let set_fasttrack_sock sock info ghandler =
  let gconn = {
      gconn_file_info_sent = [];
      gconn_client_info_sent = false;
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
      gconn_verbose = ref false;
    } in
  TcpBufferedSocket.set_reader sock (FasttrackFunctions.handlers info gconn);
  TcpBufferedSocket.set_refill sock (fun sock ->
      match gconn.gconn_refill with
        [] -> ()
      | refill :: _ -> refill sock
  );
  TcpBufferedSocket.set_handler sock TcpBufferedSocket.WRITE_DONE (
    fun sock ->
      match gconn.gconn_refill with
        [] -> ()
      | _ :: tail ->
          gconn.gconn_refill <- tail;
          match tail with
            [] ->
              if gconn.gconn_close_on_write then
                set_lifetime sock 30.
(*                TcpBufferedSocket.close sock "write done" *)
          | refill :: _ -> refill sock)

let udp_handler f sock event =
  match event with
    UdpSocket.READ_DONE ->
      UdpSocket.read_packets sock (fun p ->
          try
            f p
          with e ->
              lprintf "Error %s in udp_handler\n"
                (Printexc2.to_string e);
      ) ;
  | _ -> ()

let bits32 = 0xffffffffL
let neg32_bit = 0x80000000L
let len5 = Int64.of_int (128*128*128*128)
let len4 = Int64.of_int (128*128*128)
let len3 = Int64.of_int (128*128)
let len2 = Int64.of_int (128)

let int64_7f = 0x7fL
let int64_80 = 0x80L

(* TODO: check this implementation of buf_dynint, supposed to
  be better... *)
let rec iter len n =
  if n > len2 then
    let s = iter (len+1) (Int64.shift_right_logical n 7) in
    s.[len] <- char_of_int (0x80 lor (Int64.to_int (Int64.logand n int64_7f)));
    s
  else
  let s = String.create (len+1) in
  s.[len] <- char_of_int (Int64.to_int n);
  s

let buf_dynint b data =
  let data = Int64.logand bits32 data in
  Buffer.add_bytes b (iter 0 data)

let buf_dynint b data =
  let data = Int64.logand bits32 data in
  let buf = String.create 6 in

  let len =
    if data > len5 then 5 else
    if data > len4 then 4 else
    if data > len3 then 3 else
    if data > len2 then 2 else 1
  in
  let i = len - 1 in

  (* last byte doesn't have high bit set *)
  buf.[i] <-  char_of_int (Int64.to_int (Int64.logand data int64_7f));
  let data = ref (Int64.shift_right_logical data  7) in

  for i = i - 1 downto 0 do
    buf.[i] <- char_of_int (0x80 lor (Int64.to_int
          (Int64.logand !data int64_7f)));
    data := Int64.shift_right_logical !data  7;
  done;
  Buffer.add_bytes b (Bytes.sub buf 0 len)

let dynint v =
  let b = Buffer.create 10 in
  buf_dynint b v;
  Buffer.contents b

let get_dynint s pos =
  let len = String.length s in
  let rec iter len pos ret =
    if pos < len then
      let i = int_of_char s.[pos] in
      let ret = Int64.logor (Int64.shift_left ret 7)
        (Int64.of_int (i land 0x7f)) in
      if i land 0x80 <> 0 then
        iter len (pos+1) ret
      else
        ret, pos+1
    else
      ret, len
  in
  let v,pos = iter len pos zero in
  let v = if Int64.logand v neg32_bit <> zero then
      42949672956L -- v else v in
  v, pos

let known_download_headers = []

let parse_headers c first_line headers =

  begin
    try
      let (server,_) = List.assoc "server" headers in
      c.client_user.user_software <- server;
      client_must_update c;
    with Not_found -> ()
  end;

  begin
    try
      let (username,_) = List.assoc "x-kazaa-username" headers in
      c.client_user.user_nick <- username;
      client_must_update c;
    with Not_found -> ()
  end;


  if !verbose_unknown_messages then begin
      let unknown_header = ref false in
      List.iter (fun (header, _) ->
          unknown_header := !unknown_header || not (List.mem header known_download_headers)
      ) headers;
      if !unknown_header then begin
          lprintf "FT DEVEL: Download Header contains unknown fields\n";
          lprintf "    %s\n" first_line;
          List.iter (fun (header, (value,header2)) ->
              lprintf "    [%s] = [%s](%s)\n" header value header2;
          ) headers;
          lprintf "FT DEVEL: end of header\n";
        end;
    end

let fasttrack_200_ok = "no such thing :)"

(* This is the typical reply of a busy FT client.
ascii:[
HTTP/1.0 503 Service Unavailable(10)
Retry-After: 300(10)(10)
X-Kazaa-Username: K++_www.kazaaKPP.com(10)(10)
X-Kazaa-Network: KaZaA(10)(10)
X-Kazaa-IP: 80.56.???.???:3223(10)(10)
X-Kazaa-SupernodeIP: 80.57.???.???:1070(10)(10)]
*)

open CommonUploads

let headers_of_shared_file gconn sh =
  let headers = ref [] in
  List.iter (fun uid ->
      match Uid.to_uid uid with
        Md5Ext hash ->
          let hash = Md5Ext.to_hexa_case false hash in
          headers :=
          ("X-KazaaTag", Printf.sprintf "3=%s" hash) :: !headers
      | _ -> ()
  ) sh.shared_uids;

  if not (List.mem sh.shared_id gconn.gconn_file_info_sent) then begin
      gconn.gconn_file_info_sent <-
        sh.shared_id :: gconn.gconn_file_info_sent;
(* TODO: add other X-KazaaTag headers using 'name_of_tag' *)
      ()
    end;
  !headers

let request_of_download request d =
  let url = Printf.sprintf "/.hash=%s" d.download_uri in
  let s = Printf.sprintf "%s %s HTTP/1.0" request url in
  s

let make_download_request c s headers =

(* hum... what will happen if no client support HEAD request ? We might
lose them during the first connection... *)

(* TODO: use HEAD requests
  if !!fasttrack_experimental && c.client_support_head_request &&
    not d.download_head_requested then begin
      c.client_support_head_request <- false;
      d.download_head_requested <- true;
      d.download_ranges <- d.download_ranges @ [HEADReq];
      make_kazaa_request c "HEAD" url
        [
        "Accept", "multipart/byteranges";
        "Range", "0-";
      ]
    end;  *)
  let headers =
(*      Printf.bprintf buf "User-Agent: %s\r\n" (get_user_agent ()); *)
    ("X-Kazaa-Network", FasttrackNetwork.network_name) ::
    ("Connection", "Keep-Alive") ::
    ("X-Kazaa-Username", client_name ()) ::
    ("Connection", "Keep-Alive") ::
    (match c.client_host with
        None -> headers
      | Some (ip, port) ->
          ("Host", Printf.sprintf "%s:%d" (Ip.to_string ip) port) :: headers)
  in
  make_http_header s headers

let find_download_by_index index dlist = raise Not_found

let find_file_to_upload gconn url =
  let sh =
    let file = url.Url.short_file in

    if String2.starts_with file "/.hash=" then
      let hash = String.sub file 7 (String.length file - 7) in
      let hash = Md5Ext.of_hexa hash in
      let uid = Uid.create (Md5Ext hash) in
      let urn = Uid.to_string uid in
      lprintf "Trying to find %s\n" urn;
      find_by_uid uid
    else
    raise Not_found
  in

  let impl = sh.shared_impl in
  impl.impl_shared_requests <- impl.impl_shared_requests + 1;
  shared_must_update_downloaded (as_shared impl);

  let info = IndexedSharedFiles.get_result sh.shared_info in
  (fun pos upload_buffer spos rlen ->

      let impl = sh.shared_impl in
      impl.impl_shared_uploaded <-
        impl.impl_shared_uploaded ++ (Int64.of_int rlen);
      shared_must_update_downloaded (as_shared impl);

      Unix32.read sh.shared_fd pos upload_buffer spos rlen),
  info.shared_size,

  headers_of_shared_file gconn info
