(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
(* Copyright 2025 Luca Carlon *)
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

(* HTTP Requests: GET, HEAD *)

open Printf2
open BasicSocket
open Url

type http_request =
  GET
| HEAD

type error = [
  `HTTP of int
| `DNS
| `Block of Ip.t
| `CurlCode of Curl.curlCode
| `UnknownError
]

exception ScheduleRetry of error

let show_error = function
| `HTTP code -> Printf.sprintf "HTTP error code %d" code
| `DNS -> Printf.sprintf "DNS resolution failed"
| `Block ip -> Printf.sprintf "Blocked connection to %s" (Ip.to_string ip)
| `CurlCode curlCode -> Printf.sprintf "Curl error: %s" (Curl.strerror curlCode)
| `UnknownError -> Printf.sprintf "Unknown error occurred"

let verbose = ref false

type request = {
    req_headers : ( string * string ) list;
    req_user_agent : string;
    req_accept : string;
    req_proxy : (string * int * (string * string) option) option; (* (host,port,(login,password)) *)
    mutable req_url : url;
    mutable req_save_to_file_time : float;
    req_request : http_request;
    req_referer : Url.url option;
    req_retry : int;
    req_max_retry : int;
    req_save : bool;
    req_max_total_time : float;
    req_filter_ip : (Ip.t -> bool);
  }

let log_prefix = "[HTTPcl]"

let bracket res destroy k =
  let x = try k res with exn -> destroy res; raise exn in
  destroy res;
  x

let thread_pool = ThreadPool.create 4

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let def_user_agent =
  let ua = Curl.version_info () in
  let v = ua.Curl.version in
  Printf.sprintf "curl/%s" v

let basic_request = {
    req_url = Url.of_string "https://github.com/ygrek/mldonkey";
    req_referer = None;
    req_save_to_file_time = 0.;
    req_request = GET;
    req_proxy = None;
    req_headers = [];
    req_user_agent = def_user_agent;
    req_accept = "*/*";
    req_retry = 0;
    req_max_retry = 0;
    req_save = false;
    req_max_total_time = infinite_timeout;
    req_filter_ip = (fun _ -> true);
  }

let file_size filename =
  try
    let stats = Unix.stat filename in
    stats.Unix.st_size
  with
  | Unix.Unix_error (err, _, _) ->
    lprintf_nl "Error checking file size: %s" (Unix.error_message err);
    -1

let is_file_empty filename =
  let filesize = file_size filename in
  filesize <= 0

let def_ferr = (fun _ -> ())

let safe_call f write_log =
  try
    Some (f ())
  with
  | e ->
    if write_log then
      lprintf_nl "Exception occurred: %s" (Printexc.to_string e);
    None

(** Internal HTTP call implementation *)
let rec http_call_internal r write_f fretry progress =
  fretry ();
  try
    bracket (Curl.init ()) Curl.cleanup begin fun curl ->
      Curl.set_url curl (Url.to_string r.req_url);
      let headers = 
        ("User-Agent", r.req_user_agent) ::
        ("Accept", r.req_accept) ::
        r.req_headers
      in
      Curl.set_followlocation curl true;
      Curl.set_progressfunction curl (fun dltotal dlnow _ _ ->
        progress (int_of_float dlnow) (Int64.of_float dltotal);
        false
      );
      Curl.set_httpheader curl (List.map (fun (k, v) -> k ^ ": " ^ v) headers);
  
      (match r.req_request with
      | GET -> ()
      | HEAD ->
        Curl.set_nobody curl true;
        Curl.set_header curl true;
      );
  
      (match r.req_referer with
      | Some ref_url -> Curl.set_referer curl (Url.to_string_no_args ref_url)
      | None -> ());
  
      (* TODO: TEST *)
      (match r.req_proxy with
      | Some (host, port, Some (username, password)) ->
          Curl.set_proxy curl host;
          Curl.set_proxyport curl port;
          Curl.set_proxyuserpwd curl (username ^ ":" ^ password)
      | Some (host, port, None) ->
          Curl.set_proxy curl host;
          Curl.set_proxyport curl port
      | None -> ());
  
      (* TODO: TEST *)
      if (int_of_float r.req_max_total_time) <> (int_of_float infinite_timeout) then
        Curl.set_timeout curl (int_of_float r.req_max_total_time);
  
      Curl.set_writefunction curl write_f;
      Curl.perform curl;
      let http_code = Curl.getinfo curl Curl.CURLINFO_HTTP_CODE in
      match http_code with
      | Curl.CURLINFO_Long code -> (match code with
        | 200 ->
          lprintf_nl "HTTP success: %s" (Url.to_string r.req_url);
          Ok ""
        | 400 when r.req_request = HEAD ->
          lprintf_nl "Error 400 received for HEAD %s, re-try GET" (Url.to_string_no_args r.req_url);
          let r2 = {
            r with
            req_request = GET;
            req_retry = r.req_retry + 1;
          } in
          http_call_internal r2 write_f fretry progress
        | 502 | 503 | 504 ->
          raise (ScheduleRetry (`HTTP code))
        | code ->
          lprintf_nl "HTTP error occurred: %d %s" code (Url.to_string r.req_url);
          Error (`HTTP code))
      | _ ->
        lprintf_nl "HTTP error unknown: %s" (Url.to_string r.req_url);
        Error `UnknownError
    end
  with
  | ScheduleRetry error ->
    lprintf_nl "exception";
    raise (ScheduleRetry error)
  | Curl.CurlException (code, i, s) ->
    lprintf_nl "request %s failed: %s" (Url.to_string r.req_url) (Curl.strerror code);
    Error (`CurlCode code)
  | ex ->
    lprintf_nl "exception trying to download: %s" (Printexc.to_string ex);
    Error `UnknownError

(** Call an endpoint *)
let http_call r write_f fok fko fretry progress =
  let url = r.req_url in
  let name, port = match r.req_proxy with
  | None -> url.server, url.port
  | Some (s, p, _) -> s, p
  in
  try
    let host_entry = Unix.gethostbyname name in
    let ip_list = host_entry.Unix.h_addr_list in
    match Array.find_opt (fun ip -> not (r.req_filter_ip (Ip.of_inet_addr ip))) ip_list with
    | Some ip ->
      lprintf "Match found: %s!\n" (Unix.string_of_inet_addr ip);
      safe_call (fun () -> fko (`Block (Ip.of_inet_addr ip))) true |> ignore
    | None -> match http_call_internal r write_f fretry progress with
      | Ok _ -> safe_call (fun () -> fok ()) true |> ignore
      | Error code -> safe_call (fun () -> fko code) true |> ignore
  with Not_found ->
    lprintf_nl "Host not found: %s" name;
    fko `DNS

(** Download to file *)
let wget_sync r f =
  lprintf_nl "wget %s" (Url.to_string r.req_url);
  let webinfos_dir = "web_infos" in
  Unix2.safe_mkdir webinfos_dir;
  Unix2.can_write_to_directory webinfos_dir;
  let base = Filename.basename r.req_url.Url.short_file in
  let base = if base = "." || base = "/"
    then begin
      let prng = Random.State.make_self_init () in
      let rnd = (Random.State.bits prng) land 0xFFFFFF in
      Printf.sprintf "http_%06x.tmp" rnd 
    end else base 
  in
  let tmp_file = Filename.concat webinfos_dir base in
  let oc = open_out_bin tmp_file in
  let write_f = (fun data ->
    (* lprintf_nl "downloaded %d" (String.length data); *)
    output_string oc data;
    String.length data
  ) in
  let fok () =
    try
      close_out oc;
      let size = file_size tmp_file in
      if size = 0 then
        lprintf_nl "Downloaded file %s is empty" tmp_file
      else
        lprintf_nl "Downloaded file %s size is %d" tmp_file size;
      if r.req_save_to_file_time <> 0. then
        Unix.utimes tmp_file r.req_save_to_file_time r.req_save_to_file_time;
      (f tmp_file : unit);
      if not r.req_save then Sys.remove tmp_file
    with e ->  
      lprintf_nl "Exception %s in loading downloaded file %s" (Printexc2.to_string e) tmp_file;
      safe_call (fun () ->
        if not r.req_save then Sys.remove tmp_file
      ) false |> ignore
  in
  let fko err =
    safe_call (fun () -> close_out oc) false |> ignore;
    safe_call (fun () -> Sys.remove tmp_file) false |> ignore
  in
  let fretry () = seek_out oc 0 in
  http_call r write_f fok fko fretry (fun _ _ -> ())

let rec schedule_retry request f_attempt ferr =
  ThreadPool.add_task thread_pool (fun () ->
    try
      f_attempt request
    with
    | ScheduleRetry error ->
      if request.req_retry >= request.req_max_retry then
        safe_call (fun () -> ferr error) true |> ignore
      else begin
        let request2 = {
          request with
          req_retry = request.req_retry + 1;
        } in
        let seconds = float_of_int ((request.req_retry + 1)*10) in
        lprintf_nl "Reschedule request (%d out of %d) in %d seconds..."
          request2.req_retry request2.req_max_retry (int_of_float seconds);
        ignore(add_timer (seconds) (fun _ ->
          schedule_retry request2 f_attempt ferr
        ))
      end
  )

let wget r f =
  schedule_retry r (fun req ->
    wget_sync req f |> ignore
  ) (fun _ -> ())

(** GET request to buffer *)
let wget_string_sync r f ?(ferr=def_ferr) progress =
  lprintf_nl "wget_string %s" (Url.to_string r.req_url);
  let buffer = Buffer.create 1000 in
  let write_f = (fun data ->
    Buffer.add_string buffer data;
    String.length data
  ) in
  let fok () =
    f (Buffer.contents buffer)
  in
  let fko err =
    ferr err
  in
  let fretry () = () in
  http_call r write_f fok fko fretry progress

let wget_string r f ?(ferr=def_ferr) progress =
  schedule_retry r (fun req ->
    wget_string_sync req f ~ferr:ferr progress |> ignore
  ) ferr

(** HEAD request with error callback *)
let whead2 r f ferr =
  lprintf_nl "whead";
  let f_headers = fun data ->
    let lines = String.split_on_char '\n' data in
    f (List.filter_map (fun line ->
      match String.index_opt line ':' with
      | Some idx ->
          let key = String.sub line 0 idx |> String.trim in
          let value = String.sub line (idx + 1) (String.length line - idx - 1) |> String.trim in
          Some (key, value)
      | None -> None;
    ) lines)
  in
  wget_string r f_headers ~ferr:ferr (fun _ _ -> ())

(** HEAD request *)
let whead r f = whead2 r f def_ferr

let split_header header =
  let len = String.length header in
  let header_bytes = Bytes.of_string header in
  for i = 0 to len - 1 do
    if Bytes.get header_bytes i = '\r' then
      Bytes.set header_bytes i '\n'
  done;
  for i = len - 1 downto 1 do
    if Bytes.get header_bytes (i - 1) = '\n' then
      if Bytes.get header_bytes i = ' ' then (
        Bytes.set header_bytes i ',';
        Bytes.set header_bytes (i - 1) ','
      ) else if Bytes.get header_bytes i = ',' then
        Bytes.set header_bytes (i - 1) ','
  done;
  String2.split_simplify (Bytes.unsafe_to_string header_bytes) '\n'

let cut_headers headers =
  try
    List.map (fun s ->
        let pos = String.index s ':' in
        let len = String.length s in
        let key = String.sub s 0 pos in
        String.lowercase key, if pos+1 < len && s.[pos+1] = ' ' then
          String.sub s (pos+2) (len-pos-2), key
        else
          String.sub s (pos+1) (len-pos-1), key
    ) headers
  with e ->
      lprintf_nl "Exception in cut_headers: %s" (Printexc2.to_string e);
      raise e
