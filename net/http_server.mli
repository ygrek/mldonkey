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
type auth = No_auth | Read_auth | Write_auth
and header =
    Unknown of string * string
  | Referer of Url.url
  | Authorization of auth
exception ProcessForked
exception ThreadForked
type options = {
    referer : Url.url option;
    content_length : int;
    content_type : string;
    login : string;
    passwd : string;
    host : string;
    no_cache : bool;
  } 
and full_header = string * (string * string * (string * string) list)
and form_arg = {
    arg_name : string;
    arg_value : string;
    arg_args : (string * string) list;
    arg_headers : full_header list;
  } 
and version = HTTP1_0 | HTTP1_1 | HTTP
and request = {
    request : string;
    version : version;
    get_url : Url.url;
    options : options;
    headers : full_header list;
    form_args : form_arg list;
  } 
and handler = TcpBufferedSocket.t -> request -> unit
and config = {
    bind_addr : Unix.inet_addr;
    mutable port : int;
    requests : (string * handler) list;
    mutable addrs : Ip.t list;
    base_ref : string;
    default : handler;
  } 

val create : config -> TcpServerSocket.t
val need_auth : Buffer.t -> string -> unit
val html_escaped : string -> string
  
val handler : config -> 'a -> TcpServerSocket.event -> unit
