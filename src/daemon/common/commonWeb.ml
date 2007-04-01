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
open Gettext

let _s x = _s "CommonWeb" x
let _b x = _b "CommonWeb" x

let log_prefix = "[cWeb]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let days = ref 0
let hours = ref 0

type web_jobs = {
  mutable downloaded : bool;
}

let running_jobs = Hashtbl.create (List.length !!web_infos)

let add_job url =
  let wj = {
    downloaded = false;
  } in
  Hashtbl.add running_jobs url wj

let remove_job url =
  Hashtbl.remove running_jobs url

(*************************************************************************)
(*                                                                       *)
(*                         load_url                                      *)
(*                                                                       *)
(*************************************************************************)

let file_kinds = ref []

let add_web_kind kind descr f =
  let kind_record = { f = f; description = descr } in
  file_kinds := (kind, kind_record) :: !file_kinds

  
let mldonkey_wget_url url f =
  add_job url;
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_referer = (
        let (rule_search,rule_value) =
          try (List.find(fun (rule_search,rule_value) ->
            Str.string_match (Str.regexp rule_search) url 0
          ) !!referers )
          with Not_found -> ("",url) in
        Some (Url.of_string rule_value) );
      H.req_headers = (try
        let cookies = List.assoc url !!cookies in
          [ ( "Cookie", List.fold_left (fun res (key, value) ->
              if res = "" then
                key ^ "=" ^ value
              else
                res ^ "; " ^ key ^ "=" ^ value
          ) "" cookies
        ) ]
      with Not_found -> []);
      H.req_user_agent = get_user_agent ();
      H.req_max_retry = 20;
      H.req_save = true;
    } in
    let r1 = {
      r with
      H.req_request = H.HEAD;
    } in
    let date  = ref None in
    begin try
    H.whead2 r1 (fun headers ->
      List.iter (fun (name, content) ->
	if String.lowercase name = "last-modified" then
          try
	    date := Some content
	  with _ -> ()
    ) headers;
    match !date with
      None -> H.wget r f
    | Some date ->
	let file = Filename.concat "web_infos" (Filename.basename r.H.req_url.Url.short_file) in
	r.H.req_save_to_file_time <- (begin try
	    Date.time_of_string date
	  with e ->
	    Unix.time ()
	  end);
	if not (Sys.file_exists file) then
	  H.wget r f
	else
	  begin
	    let file_date = Unix.LargeFile.stat file in
	      if r.H.req_save_to_file_time <= file_date.Unix.LargeFile.st_mtime then
	        begin
	        lprintf_nl (_b "using local version of %s (%s), HTML header (%s)")
		  file (Date.to_full_string file_date.Unix.LargeFile.st_mtime) date;
	        (f file : unit)
	        end
	      else
	        begin
	          lprintf_nl (_b "downloading newer %s, HTML header (%s)") file date;
	          H.wget r f
	        end
	  end
      )
      (fun c ->
	match c with
          | x when x < 200 || x > 299 -> begin
            (* use local version if wget fail and file exists *)
            let file = Filename.concat "web_infos" (Filename.basename r.H.req_url.Url.short_file) in
            (try (* mark this job downloaded *)
                (Hashtbl.find running_jobs url).downloaded <- true
             with Not_found -> ());
            if Sys.file_exists file then begin
              lprintf_nl (_b "using local version of %s, HTTP request failed (error %d)") file x;
              add_timer 5. (fun timer ->
                let jobs =
                  (* check if other jobs are still in downloading state to avoid calling
                     function f, which might hurt other downloads for expensive functions *)
                  let others_running = ref 0 in
                  Hashtbl.iter (fun url j ->
                    if not j.downloaded then incr others_running
                  ) running_jobs;
                  !others_running
                in
                if jobs = 0 then
                (* no other jobs in downloading state, process local versions of remotely failed job *)
                  (f file : unit)
                else
                (* other jobs in downloading state, reactivate this timer to check again in 5s *)
                  reactivate_timer timer
                );
              end
            else
              lprintf_nl (_b "local file %s not found, HTTP request failed (error %d)") file x;
            end
	  | _ -> ()
      )
    with e -> 
      remove_job url;
      lprintf_nl (_b "Exception %s while loading %s")
        (Printexc2.to_string e) url
    end

let mldonkey_wget_shell url f =
  let command_urlencoded = Str.string_after url 8 in
  let command = Url.decode command_urlencoded in
  let filename = Filename.temp_file "wget_" ".tmp" in
    ignore (Sys.command (Printf.sprintf "%s > %s" command filename));
    (f filename : unit)
      
let mldonkey_wget url f =
  if Str.string_match (Str.regexp "shell://") url 0 then
    mldonkey_wget_shell url f
  else
    mldonkey_wget_url url f
      
let load_url can_fail kind url =
  let f =
    try
      (List.assoc kind !file_kinds).f url
    with e -> failwith (Printf.sprintf "Unknown kind [%s]" kind)
  in
  try
    lprintf_nl (_b "request %s (%s)") kind url;
    mldonkey_wget url f
  with e ->
    if can_fail then
      failwith (Printf.sprintf "Exception %s while loading %s"
          (Printexc2.to_string e) url)
    else
      lprintf_nl (_b "Exception %s while loading %s")
          (Printexc2.to_string e) url

(*************************************************************************)
(*                                                                       *)
(*                         load_web_infos                                *)
(*                                                                       *)
(*************************************************************************)

let load_web_infos core_start force =
  List.iter (fun (kind, period, url) ->
    if (core_start && period = 0) || (period <> 0 && !hours mod period = 0) || force then
      begin
        try
          load_url false kind url
	with e ->
            lprintf_nl (_b "%s while loading %s")
	      (Printexc2.to_string e) url
      end
  ) !!CommonOptions.web_infos

type rss_feed = {
    mutable rss_date : int;
    mutable rss_value : Rss.channel;
  }

let rss_feeds = Hashtbl.create 10

let _ =
  add_web_kind "rss" "Syndication feeds to get periodically updated data" 
    (fun url filename ->
      lprintf_nl (_b "parsing feed %s (rss)") url;
      let c =
	(try
	  let rss_c = Rss.channel_of_file filename in
	  (try Sys.remove filename with _ -> ());
	  rss_c
	with Xml.Error _ ->
	  lprintf_nl (_b "found buggy feed, preprocessing with %s and trying again") !!rss_preprocessor;
	  (try
	    let pipe_out, pipe_in = Unix.pipe () in
	    let pid = Unix.create_process !!rss_preprocessor [| !!rss_preprocessor; filename |] 
		Unix.stdin pipe_in pipe_in in
	    Unix.close pipe_in;
	    let output = Buffer.create 1024 in
	    let buffersize = 1024 in
	    let buffer = String.create buffersize in
	    (try
	      while true do
		let nread = Unix.read pipe_out buffer 0 buffersize in
		if nread = 0 then raise End_of_file;
		Buffer.add_substring output buffer 0 nread
	      done
	     with 
	     | End_of_file -> ()
	     | Unix.Unix_error (code, f, arg) ->
		 lprintf_nl "%s failed: %s" !!rss_preprocessor (Unix.error_message code));
	    (try Unix.close pipe_out with _ -> ());
	    (try Sys.remove filename with _ -> ());
	    let _pid, _ = Unix.waitpid [] pid in
	    let result = Buffer.contents output in
	    if result = "" then begin
	      lprintf_nl (_b "%s produced empty content for feed %s, program missing?") !!rss_preprocessor url;
	      raise Not_found
	    end;
	    Rss.channel_of_string result
	  with Unix.Unix_error (code, f, arg) ->
	    lprintf_nl (_b "%s failed: %s") !!rss_preprocessor (Unix.error_message code); raise Not_found))
      in
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
      feed.rss_value <- c;
      remove_job url
  )
