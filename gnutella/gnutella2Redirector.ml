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

open CommonSwarming
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

let g2_parse_redirector_page f =
  let s = File.to_string f in
  let lines = String2.split_simplify s '\n' in
  List.iter (fun line ->
      match String2.split_simplify line '|' with
        "h" :: ip_port :: n :: _ ->
          begin
            try
              let ip, port = String2.cut_at ip_port ':' in
              Fifo.put ultrapeers2_queue
                (Ip.of_string ip, int_of_string port) 
            with _ -> ()
          end
      | _ -> ()
  ) lines

let connect () =
  if !!enable_gnutella2 then 
    List.iter (fun url ->
        let module H = Http_client in
        let r = {
            H.basic_request with
            H.req_url = Url.of_string url;
            H.req_user_agent = 
            Printf.sprintf "MLdonkey %s" Autoconf.current_version;
          } in
        H.wget r g2_parse_redirector_page
    
    ) !!gnutella2_redirectors;
    
