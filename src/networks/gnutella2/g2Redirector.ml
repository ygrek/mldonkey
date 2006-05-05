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

open CommonHosts
open CommonTypes
open CommonGlobals
open Options
open G2Types
open G2Globals
open G2Options
open G2Protocol
open G2ComplexOptions

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
                ignore (H.new_host
		  (Ip.addr_of_string ip) (int_of_string port) Ultrapeer)
            with _ -> ()
          end
      | "u" :: url :: _ ->
          if not (List.mem url !!redirectors) then
            redirectors =:= url :: !!redirectors
      | _ -> ()
  ) lines

let next_redirector_access = ref 0
  
let connect () =
  if !next_redirector_access < last_time () then begin
      (* We should only contact the redirectors
         if we don't have enough hosts.
         Changed it to once every day,
         so we don't hurt the network. *)
      next_redirector_access := last_time () + (3600*24);
      List.iter (fun url ->
          let module H = Http_client in
          let url = Printf.sprintf "%s?get=1&hostfile=1&net=gnutella2&client=MLDK&version=%s"
              url Autoconf.current_version in
          let r = {
              H.basic_request with
              H.req_url = Url.of_string url;
              H.req_proxy = !CommonOptions.http_proxy;
              H.req_user_agent = get_user_agent ();
            } in
          if !verbose then
            lprintf "Connecting G2 redirector\n";
          H.wget r g2_parse_redirector_page    
      ) !!redirectors;
    end else begin
      if !verbose then
        lprintf "redirector recontacted in %d seconds \n"
          (!next_redirector_access - last_time ())
    end
    
