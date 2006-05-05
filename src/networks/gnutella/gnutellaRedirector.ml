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

open CommonDownloads
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
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

open GnutellaProto

let redirectors_urlfiles = ref []
let redirectors_hostfiles = ref []
  
let parse_urlfile file = 
  let s = File.to_string file in
  clean_file s;
  let lines = String2.split_simplify s '\n' in
  List.iter (fun line ->
      if not (List.mem line !!gnutella_hostfiles) then
        gnutella_hostfiles =:= line :: !!gnutella_hostfiles
  ) lines;
  redirectors_hostfiles := !!gnutella_hostfiles

let connect_urlfile () = 
  match !redirectors_urlfiles with
    [] ->
      redirectors_urlfiles := !!urlfiles
  | url :: tail ->
      redirectors_urlfiles := tail;
      let module H = Http_client in
      let url = Printf.sprintf "%s?urlfile=1&client=MLDK&version=%s"
          url Autoconf.current_version in
      let r = {
          H.basic_request with
          H.req_url = Url.of_string url;
          H.req_proxy = !CommonOptions.http_proxy;
          H.req_user_agent = get_user_agent ();
        } in
      if !verbose then
        lprintf "Connecting Gnutella %s\n" url;
      H.wget r parse_urlfile    
      
let parse_hostfile file = 
  let s = File.to_string file in
  clean_file s;
  let lines = String2.split_simplify s '\n' in
  List.iter (fun line ->
      try
        let ip, port = String2.cut_at line ':' in
        if !verbose then
          lprintf "gnutella1: adding ultrapeer from hostfile\n";
        ignore (H.new_host (Ip.addr_of_string ip) (int_of_string port) Ultrapeer)
      with _ -> ()
  ) lines

let next_redirector_access = ref 0
  
let connect_hostfile _ =
  match !redirectors_hostfiles with
    [] ->
      if !next_redirector_access < last_time () then begin
          (* We should only contact the redirectors
             if we don't have enough hosts.
             Changed it to once every day,
             so we don't hurt the network. *)
          next_redirector_access := last_time () + (3600*24);
          connect_urlfile ();
          redirectors_hostfiles := !!gnutella_hostfiles
        end;
  | url :: tail ->
      redirectors_hostfiles := tail;
      let module H = Http_client in
      let url = Printf.sprintf "%s?hostfile=1&client=MLDK&version=%s"
          url Autoconf.current_version in
      let r = {
          H.basic_request with
          H.req_url = Url.of_string url;
          H.req_proxy = !CommonOptions.http_proxy;
          H.req_user_agent = get_user_agent ();
        } in
      if !verbose then
        lprintf "Connecting Gnutella %s\n" url;
      H.wget r parse_hostfile    
      
let connect _ = 
  connect_hostfile ()
    
