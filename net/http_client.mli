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
  GET of Url.url
| POST of Url.url
| HEAD of Url.url
| PUT of Url.url
| DELETE of Url.url
| TRACE of Url.url
| OPTIONS of Url.url option (* None = '*' *)
| CONNECT of string * int

type http_headers =
| Generic of string * string
| Referer of Url.url
  
type get_args =
  Timeout of float
| Args of (string * string) list
| Headers of http_headers list
| Post
| Proxy of string * int

type content_handler = 
  int -> (string * string) list -> TcpBufferedSocket.t -> int -> unit

val get_page : Url.url -> get_args list -> content_handler ->
  (unit -> unit) -> unit

val wget : string -> (string -> unit) -> unit

  
  
  (*
val default_headers_handler : (int -> TcpBufferedSocket.t -> int -> unit) ->
  headers_handler
*)
  
val split_header : string -> string list
val cut_headers : string list -> (string * string) list
  