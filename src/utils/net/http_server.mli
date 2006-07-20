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
type options = {
    referer : Url.url option;
    content_length : int;
    content_type : string;
    login : string;
    passwd : string;
    host : string;
    no_cache : bool;
  } 
and full_header = string * string
and form_arg = {
    arg_name : string;
    arg_value : string;
    arg_args : (string * string) list;
    arg_headers : full_header list;
  } 
and version = HTTP1_0 | HTTP1_1 | HTTP
and request = {
    sock : TcpBufferedSocket.t;
    request : string;
    version : version;
    get_url : Url.url;
    options : options;
    headers : full_header list;
    form_args : form_arg list;
    
    mutable reply_head : string;
    mutable reply_headers : (string * string) list;
    mutable reply_content : string;
    mutable reply_stream : (TcpBufferedSocket.t -> unit) option;
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
val need_auth : request -> string -> unit
val html_escaped : string -> string
val html_real_escaped : string -> string
  
val add_reply_header : request -> string -> string -> unit
val handler : config -> 'a -> TcpServerSocket.event -> unit
val parse_head : TcpBufferedSocket.t -> string -> request
  
val verbose : bool ref 
  
val request_range : request -> int64 * (int64 option)
val parse_range : string -> int64 * int64 option * int64 option
val error_page : string -> string -> string -> string -> string -> string * string
