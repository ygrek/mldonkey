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
  clean_file s;
  let lines = String2.split_simplify s '\n' in
  List.iter (fun line ->
      match String2.split_simplify line '|' with
        "h" :: ip_port :: n :: _ ->
          begin
            try
              let ip, port = String2.cut_at ip_port ':' in
              let h = new_host 
                  (Ip.of_string ip) (int_of_string port) true 2 in
              ()
            with _ -> ()
          end
      | _ -> ()
  ) lines

let next_redirector_access = ref 0
  
let connect () =
  if !!g2_enabled && !next_redirector_access < last_time () then begin
      next_redirector_access := last_time () + 3600;
      List.iter (fun url ->
          let module H = Http_client in
          let r = {
              H.basic_request with
              H.req_url = Url.of_string url;
              H.req_proxy = !CommonOptions.http_proxy;
              H.req_user_agent = 
              Printf.sprintf "MLdonkey %s" Autoconf.current_version;
            } in
          lprintf "Connecting Gnutella2 redirector\n";
          H.wget r g2_parse_redirector_page    
      ) !!g2_redirectors;
    end else begin
      lprintf "redirector recontacted in %d seconds \n"
        (!next_redirector_access - last_time ())
    end
    
