(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

type http_request =
  GET
| HEAD

(*
| OPTIONS of url option (* None = '*' *)
| CONNECT of string * int
*)
val verbose : bool ref
val thread_pool : ThreadPool.t
  
type request = {
    req_headers : ( string * string ) list;
    req_user_agent : string;
    req_accept : string;
    req_proxy : (string * int * (string * string) option) option; (** (host,port,(login,password)) *)
    mutable req_url : Url.url;
    mutable req_save_to_file_time : float;
(* re-download a saved file only if newer *)
    req_request : http_request;
    req_referer : Url.url option;
    req_retry : int;
    req_max_retry : int;
    req_save : bool;
    (** maximum time whole request processing is allowed to take, in seconds *)
    req_max_total_time : float;
    (** this function is called after DNS resolution,
        returning [false] will block connection to the given ip
        and HTTP request will fail with `Block error *)
    req_filter_ip : (Ip.t -> bool);
  }

val basic_request : request

(** either HTTP error code or low-level network error or DNS *)
type error = [
  `HTTP of int
| `DNS
| `Block of Ip.t
| `CurlCode of Curl.curlCode
| `UnknownError
]
val show_error : error -> string

val wget : request -> (string -> unit) -> unit
val whead : request -> ( (string * string) list -> unit) -> unit
val whead2 : request -> ( (string * string) list -> unit) -> (error -> unit) -> unit

val wget_string : request -> (string -> unit) -> ?ferr:(error -> unit) ->
  (int -> int64 -> unit) -> unit

val split_header : string -> string list
val cut_headers : string list -> (string * (string * string)) list
