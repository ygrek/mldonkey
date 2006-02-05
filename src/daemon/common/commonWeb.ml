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

open AnyEndian
open LittleEndian

open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonOptions
open CommonTypes

let days = ref 0
let hours = ref 0

(*************************************************************************)
(*                                                                       *)
(*                         load_url                                      *)
(*                                                                       *)
(*************************************************************************)

let file_kinds = ref []

let add_web_kind kind f =
  file_kinds := (kind,f) :: !file_kinds

let mldonkey_wget url f =
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_user_agent =
      Printf.sprintf "MLDonkey/%s" Autoconf.current_version;
      H.req_max_retry = 20;
    } in
    let r1 = {
      r with
      H.req_request = H.HEAD;
    } in
    let date  = ref None in
    begin try
    H.whead r1 (fun headers ->
      List.iter (fun (name, content) ->
	if String.lowercase name = "last-modified" then
          try
	    date := Some content
	  with _ -> ()
    ) headers;
    match !date with
      None -> H.wget r f
    | Some date ->
	let html_time =
	  begin try
	    let t = Date.time_of_string date in
	      r.H.req_save_to_file_time <- t;
	      Unix.gmtime t
	  with e ->
	    let t = Unix.time () in
	      r.H.req_save_to_file_time <- t;
	      Unix.gmtime t
	  end
	in
	let file = Filename.concat "web_infos" (Filename.basename r.H.req_url.Url.short_file) in
	if not (Sys.file_exists file) then
	  H.wget r f
	else
	  begin
	    let file_date = Unix.LargeFile.stat file in
	    let file_time = Unix.gmtime file_date.Unix.LargeFile.st_mtime in
	      if html_time <= file_time then
	        begin
	        lprintf_nl "[cWeb] using local version of %s, HTML header (%s)" file date;
	        (f file : unit)
	        end
	      else
	        begin
	          lprintf_nl "[cWeb] downloading newer %s, HTML header (%s)" file date;
	          H.wget r f
	        end
	  end
      )
    with e -> 
      lprintf_nl "[cWeb] Exception %s while loading %s"
        (Printexc2.to_string e) url
    end

let load_url can_fail kind url =
  let f =
    try
      (List.assoc kind !file_kinds) url
    with e -> failwith (Printf.sprintf "Unknown kind [%s]" kind)
  in
  try
    lprintf_nl "[cWeb=%s] saving %s" kind url;
    mldonkey_wget url f
  with e ->
    if can_fail then
      failwith (Printf.sprintf "Exception %s while loading %s"
          (Printexc2.to_string e) url)
    else
      lprintf_nl "[cWeb] Exception %s while loading %s"
          (Printexc2.to_string e) url

let load_file kind file =
  try
    (List.assoc kind !file_kinds) file file
  with e ->
      lprintf_nl "[cWeb] Exception %s while loading kind %s"
        (Printexc2.to_string e) kind

(*************************************************************************)
(*                                                                       *)
(*                         load_web_infos                                *)
(*                                                                       *)
(*************************************************************************)

let load_web_infos core_start =
  List.iter (fun (kind, period, url) ->
    if (core_start && period = 0) || (period <> 0 && !hours mod period = 0) then
      begin
        try
          load_url false kind url
	with e ->
            lprintf_nl "[cWeb] %s while loading %s"
	      (Printexc2.to_string e) url
      end
  ) !!CommonOptions.web_infos

type rss_feed = {
    mutable rss_date : int;
    mutable rss_value : Rss.channel;
  }

let rss_feeds = Hashtbl.create 10


let _ =
  add_web_kind "rss" (fun url filename ->
      lprintf_nl "[cWeb=rss] parsing feed %s" url;
      let c = Rss.channel_of_file filename in
      (try Sys.remove filename with _ -> ());
      let feed =
        try Hashtbl.find rss_feeds url with
          Not_found ->
            let feed = {
                rss_date = 0;
                rss_value = c;
              } in
            Hashtbl.add rss_feeds url feed;
            feed
      in
      feed.rss_date <- last_time ();
      feed.rss_value <- c
  )
