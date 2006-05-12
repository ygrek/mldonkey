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
  
let parse_urlfile file url_string = 
  let s = File.to_string file in
  (* Http_client.wget does not delete the temp file anymore *)
  (try Sys.remove file with _ -> ());
  if !verbose then lprintf_nl () "Parsing urlfile from %s:\n%s" url_string s;
  clean_file s;
  let lines = String2.split_simplify s '\n' in
  List.iter (fun line ->
      if not (List.mem line !!gnutella_hostfiles) then begin
        gnutella_hostfiles =:= line :: !!gnutella_hostfiles;
        if !verbose then lprintf_nl () "Added GWebCache %s" line;
      end
  ) lines;
  redirectors_hostfiles := !!gnutella_hostfiles

let next_urlfile_access = ref 0

let connect_urlfile () = 
  match !redirectors_urlfiles with
    [] ->
      if !next_urlfile_access < last_time () then begin
          (* 12 hour interval between urlfile re-connection attempts *)
          next_urlfile_access := last_time () + (3600*12);
          redirectors_urlfiles := !!urlfiles;
          lprintf_nl () "added %d urlfiles" (List.length !!urlfiles);
      end else begin
        if !verbose then lprintf_nl () "connect_urlfile: no urlfiles, too soon";
      end
  | url :: tail ->
      redirectors_urlfiles := tail;
      let url_string = url in
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
        lprintf_nl () "Connecting to urlfile %s\n" url;
      H.wget r (fun filename -> parse_urlfile filename url_string)
      
let parse_hostfile file url_string = 
  let s = File.to_string file in
  (* Http_client.wget does not delete the temp file anymore *)
  (try Sys.remove file with _ -> ());
  if String2.starts_with s "ERROR" || String2.starts_with s "<" then begin
    if !verbose then lprintf_nl () "Malformed response content:\n%s" s;
    if List.mem url_string !!gnutella_hostfiles then begin
      gnutella_hostfiles =:= List.filter (fun h -> h <> url_string) !!gnutella_hostfiles;
      redirectors_hostfiles := !!gnutella_hostfiles;
      if !verbose then lprintf_nl () "Removing %s from hostfiles" url_string;
    end;
  end
  else begin
    clean_file s;
    let lines = String2.split_simplify s '\n' in
    if !verbose then lprintf_nl () "Parsing response from %s:\n%s" url_string s;
    List.iter (fun line ->
      try
        let ip, port = String2.cut_at line ':' in
        if !verbose then
          lprintf_nl () "Adding ultrapeer from hostfile %s %s" ip port;
        ignore (H.new_host (Ip.addr_of_string ip) (int_of_string port) Ultrapeer)
      with _ -> ()
    ) lines
  end

let next_redirector_access = ref 0
  
let connect_hostfile _ =
  match !redirectors_hostfiles with
    [] ->
      if !next_redirector_access < last_time () then begin
          (* 12 hour interval between redirector re-connection attempts *)
          next_redirector_access := last_time () + (3600*12);
          redirectors_hostfiles := !!gnutella_hostfiles
      end 
      else begin
        if !verbose then lprintf_nl () "connect_hostfile: no gwebcaches, too soon";
      end;
      connect_urlfile ();
  | url :: tail ->
      redirectors_hostfiles := tail;
      let module H = Http_client in
      let url_string = url in
      let url = Printf.sprintf "%s?hostfile=1&client=MLDK&version=%s"
          url Autoconf.current_version in
      let r = {
          H.basic_request with
          H.req_url = Url.of_string url;
          H.req_proxy = !CommonOptions.http_proxy;
          H.req_user_agent = get_user_agent ();
        } in
      if !verbose then
        lprintf_nl () "Connecting to hostfile %s" url;
      H.wget r (fun filename -> parse_hostfile filename url_string)
      
let connect _ = 
  connect_hostfile ()
    
