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
and handler = TcpClientSocket.t -> request -> unit
and config = {
  port : int;
  requests : (string * handler) list;
  addrs : Ip.t list;
  base_ref : string;
  default : handler;
} 

val create : config -> TcpServerSocket.t
val need_auth : Buffer.t -> string -> unit
val html_escaped : string -> string