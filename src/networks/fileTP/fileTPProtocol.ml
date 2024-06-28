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

open BasicSocket
open Printf2
open TcpBufferedSocket

open CommonOptions

type ghandler =
  HttpHeader of (gconn -> TcpBufferedSocket.t -> string -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;
  }

module type FileTPProtocol = sig
    val handler : gconn -> TcpBufferedSocket.t -> unit
  end

let handlers info gconn =
  let rec iter_read sock nread =
(*    lprintf "iter_read %d\n" nread;  *)
    let b = TcpBufferedSocket.buf sock in
    if b.len > 0 then
      match gconn.gconn_handler with
      | HttpHeader h ->
          let end_pos = b.pos + b.len in
          let begin_pos =  b.pos in
          let rec iter i n_read =
            if i < end_pos then
              if (Bytes.get b.buf i) = '\r' then
                iter (i+1) n_read
              else
              if (Bytes.get b.buf i) = '\n' then
                if n_read then begin
                    let header = Bytes.sub b.buf b.pos (i - b.pos) in
(*                    if info then begin
                        lprintf "HEADER : ";
                        dump header; lprint_newline ();
end; *)

                    (try h gconn sock (Bytes.to_string header) with
                        e -> close sock (Closed_for_exception e));
                    if not (TcpBufferedSocket.closed sock) then begin
                        let nused = i - b.pos + 1 in
(*                        lprintf "HEADER: buf_used %d\n" nused; *)
                        buf_used b nused;
                        iter_read sock 0
                      end
                  end else
                  iter (i+1) true
              else
                iter (i+1) false
          in
          iter begin_pos false
      | Reader h ->
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock b.len

  in
  iter_read

let set_fileTP_sock sock ghandler =
  let gconn = {
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
    } in
  TcpBufferedSocket.set_reader sock (handlers info gconn);
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

let parse_range range =
  try
    let npos = (String.index range 'b')+6 in
    let dash_pos = try String.index range '-' with _ -> -10 in
    let slash_pos = try String.index range '/' with _ -> -20 in
    let star_pos = try String.index range '*' with _ -> -30 in
    if star_pos = slash_pos-1 then
      Int64.zero, None, None (* "bytes */X" *)
    else
    let len = String.length range in
    let x = Int64.of_string (
        String.sub range npos (dash_pos - npos) )
    in
    if len = dash_pos + 1 then
(* bytes x- *)
      x, None, None
    else
    let y = Int64.of_string (
        String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
    in
    if slash_pos = star_pos - 1 then
      x, Some y, None (* "bytes x-y/*" *)
    else
(* bytes x-y/len *)

    let z = Int64.of_string (
        String.sub range (slash_pos+1) (len - slash_pos -1) )
    in
    x, Some y, Some z
  with
  | e ->
      lprintf "Exception %s for range [%s]\n"
        (Printexc2.to_string e) range;
      raise e

let parse_range range =
  let x, y, z = parse_range range in
  if !verbose then lprintf "Range parsed: %Ld-%s/%s" x
    (match y with None -> "" | Some y -> Int64.to_string y)
  (match z with None -> "*" | Some y -> Int64.to_string y);
  x, y, z

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

let known_download_headers = ([] : string list)
