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

open Printf2
open BasicSocket

let verbose = ref false

let log_prefix = "[HTTPsv]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let html_escaped s =
  String2.convert false (fun b escaped c ->
      if escaped then
        (Buffer.add_char b c; false)
      else
        match c with
        '<' -> Buffer.add_string b "&lt;"; false
      | '>' -> Buffer.add_string b "&gt;"; false
      | '&' -> Buffer.add_string b "&amp;"; false
      | '"' -> Buffer.add_string b "&quot;"; false
      | '\\' -> true
      | _ -> Buffer.add_char b c; false
  ) s

let html_real_escaped s =
  String2.convert false (fun b escaped c ->
      if escaped then
        (Buffer.add_char b c; false)
      else
        match c with
        '\039' -> Buffer.add_string b "&#39;"; false
      | '\\' -> true
      | _ -> Buffer.add_char b c; false
  ) (html_escaped s)


(* base 64 decoding *)
let decode64 s =
  let val64 c =
    match c with
    | 'A' .. 'Z' -> (Char.code c) - (Char.code 'A')
    | 'a' .. 'z' -> (Char.code c) - (Char.code 'a') + 26
    | '0' .. '9' -> (Char.code c) - (Char.code '0') + 52
    | '+' -> 62 | '/' -> 63 | '=' -> 0
    | _ -> failwith "not a base64 string" in
  let len = String.length s in
  let len_res = len * 3 / 4 in
  let res = Bytes.create len_res in
  for i=0 to len/4 - 1 do
    let i1 = 4*i and i2 = 3*i in
    let v1 = (val64 s.[i1]) lsl 18 in
    let v2 = (val64 s.[i1 + 1]) lsl 12 in
    let v3 = (val64 s.[i1 + 2]) lsl 6 in
    let v4 = val64 s.[i1 + 3] in
    let v = v1 lor v2 lor v3 lor v4 in
    res.[i2] <- Char.chr (v lsr 16);
    res.[i2 + 1] <- Char.chr (v lsr 8 land 0xFF);
    res.[i2 + 2] <- Char.chr (v land 0xFF)
  done;
  let nb_cut =
    if s.[len-1] = '=' then
      if s.[len-2] = '=' then 2 else 1
    else 0 in
  Bytes.sub_string res 0 (len_res - nb_cut)


let debug = ref false

type auth =
| No_auth
| Read_auth
| Write_auth

type error_reason =
| Blocked of Ip.t
| Not_allowed of Ip.t
| Not_Found of string
| Unauthorized
| Moved of string

type header =
  Unknown of string * string
| Referer of Url.url
| Authorization of auth


type options = {
    referer : Url.url option;
    content_length : int64;
    content_type : string;
    login : string;
    passwd : string;
    host : string;
    no_cache : bool;
  }

type full_header = string * string (* * string * (string * string) list) *)

type form_arg = {
    arg_name : string;
    arg_value : string;
    arg_args : (string * string) list;
    arg_headers : full_header list;
  }

type version =
  HTTP1_0
| HTTP1_1
| HTTP

type request = {
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
    mutable addrs : Ip_set.blocking_list;
    use_ip_block_list : bool;
    base_ref : string;
    default : handler;
  }


let escaped s =
  String2.convert () (fun b _ c ->
      match c with
        '\r' -> Buffer.add_string b "\\r"
      | '\n' -> Buffer.add_string b "\\n\n"
      | _ -> Buffer.add_char b c) s


let default_options = {
(*    authorization = No_auth;*)
    referer = None;
    content_length = -1L;
    content_type = "";
    login = "";
    passwd = "";
    host = "";
    no_cache = false;
  }

let rec parse_headers lines headers =
  match lines with
    [] -> List.rev headers
  | line :: tail ->
      let rec iter line lines =
        match lines with
          [] -> line, []
        | line2 :: tail ->
            let c = line2.[0] in
            if c = ' ' ||  c = '\t' then
              iter (line ^ line2) tail
            else line, lines
      in
      let line, tail = iter line tail in
      let len = String.length line in

      let sep = String.index line ':' in
      let name = String.sub line 0 sep in
      let rec iter sep =
        if sep < len-1 then
          if line.[sep] = ' ' then iter (sep+1) else
            String.sub line sep (len-sep)
        else ""
      in
      let value = iter (sep+1) in
      (*
      let head, args =
        try
          Http_lexer.get_value (Lexing.from_string value)
        with _ -> "", []
      in
*)
      parse_headers tail ((name, (value (* , head, args *) )) :: headers)

      (*

let content_type_str =   "Content-Type"

*)

let split_head s =
  let rec iter pos1 res =
    try
      let pos3 = String.index_from s pos1 '\n' in
      let pos2 = if pos3 > 0 && s.[pos3 - 1] = '\r' then pos3 - 1 else pos3 in
      let line = String.sub s pos1 (pos2 - pos1) in
      if line = "" then List.rev res else
      iter (pos3+1) (line :: res)
    with _ ->
        let last_line = String.sub s pos1 (String.length s - pos1) in
        List.rev (if last_line = "" then res else last_line :: res)
  in
  iter 0 []

let error_page reason my_ip my_port =
  let http_code, headers, error_text, error_text_long =
    match reason with
    | Moved url -> 301, ["Location",url], "Moved Permanently", Printf.sprintf "Redirecting to %s" (html_escaped url)
    | Unauthorized -> 401, [], "Unauthorized", "<p>Please login using a valid username and password</p>"
    | Not_allowed from_ip ->
      403, [], "Forbidden",
      Printf.sprintf "<p>Connection from %s rejected " (Ip.to_string from_ip) ^
      "(see downloads.ini, <a href=\"http://mldonkey.sourceforge.net/Allowed_ips\">allowed_ips</a>)</p>"
    | Blocked from_ip ->
      403, [], "Forbidden",
      Printf.sprintf "IP %s is blocked, its part of the used IP blocklist " (Ip.to_string from_ip)
    | Not_Found url ->
      404, [], "Not Found",
      Printf.sprintf "The requested URL %s was not found on this server." (html_escaped url)
  in
  let reject_message = Printf.sprintf
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html>\
<head><title>%d %s</title></head>\n<h1>%d %s</h1>\n%s\
<hr><address>MLDonkey/%s at %s Port %d</address></html>"
    http_code error_text http_code error_text error_text_long
    Autoconf.current_version (Ip.to_string my_ip) my_port
  in
  let headers = String.concat "\n" (List.map (fun (k,v) -> k ^ ": " ^ v) headers) in
  Printf.sprintf
"HTTP/1.1 %d %s\nServer: MLDonkey/%s\nConnection: close\n\
Content-Type: text/html; charset=iso-8859-1\n%sContent-length: %d\r\n"
    http_code error_text Autoconf.current_version headers (String.length reject_message), reject_message,
  Printf.sprintf "%d %s" http_code error_text

let parse_head sock s =
  let h = split_head s in
(*  List.iter (fun s -> lprintf_http_nl () "LINE: [%s]" (escaped s)) h; *)
  match h with
    [] -> failwith "Http_server: Empty head"
  | ans :: headers ->
(* get the status line *)
      let fin_meth = String.index ans ' ' in
      let meth = String.sub ans 0 fin_meth in
      let fin_file = String.index_from ans (fin_meth + 1) ' ' in
      let file = String.sub ans (fin_meth+2) (fin_file-fin_meth-2) in
      let version = match ans.[String.length ans - 1] with
          '0' -> HTTP1_0
        | '1' -> HTTP1_1
        | _ -> HTTP
      in
      let headers = parse_headers headers [] in
      let options = List.fold_left (fun options
              (name, value (* , head, args *)) ->
            try
              match String.lowercase name with
                "authorization" ->
                let _, pass = String2.cut_at value ' ' in
                let pass = decode64 pass in
                let login, pswd = String2.cut_at pass ':' in
                { options with
                  login = login;
                  passwd = pswd }
              | "content-length"
                ->
                { options with content_length = Int64.of_string value }
              | "content-type" ->
                { options with content_type = value }
              | "host" ->
                { options with host = value }
              | "pragma" when  value = "no-cache" ->
                { options with no_cache = true }
              | _ -> options
            with e ->
                if !debug then
                    lprintf_nl "Exception %s in header %s"
                      (Printexc2.to_string e) name;
                options

        ) default_options headers in
      {
        sock = sock;
        get_url = Url.of_string file;
        options = options;
        headers = headers;
(*
    stream_in = ic;
stream_out = Stream_out.create oc;
*)
        request = meth;
        form_args = [];
        version = version;

        reply_head = "200 OK";
        reply_headers = [];
        reply_stream = None;
        reply_content = "";
      }

      (*

let upload_limit = ref 4000000 (* 4 Mo for maximal upload *)

let convert_nl s =
  String2.convert () (fun b _ c ->
      match c with
        '\r' -> Buffer.add_char b '\n'
      | _ -> Buffer.add_char b c) s

  (*

let complete_multipart_data request ic tail =
  let _, boundary = String2.cut_at tail '=' in
  let boundary_len = String.length boundary in
  let boundary2 = "--" ^ boundary in
  let boundary3 = boundary ^ "\r" in
  let boundary4 = boundary2 ^ "\r" in
  let end_boundary = boundary ^ "--" in
  let end_boundary2 = boundary2 ^ "--" in
  let end_boundary3 = end_boundary ^ "\r" in
  let end_boundary4 = end_boundary2 ^ "\r" in
  let end_boundary_found = ref false in
  let rec find_first () =
    let line = Stream_in.input_netline ic in
    if line = boundary || line = boundary2 then
      get_one_part []
    else find_first ()

  and get_one_part previous =
    let rec get_lines lines =
      try
        let header = read_header ic in
        get_lines (header :: lines)
      with _ -> List.rev lines
    in
    let lines = get_lines [] in
    let field =
      match lines with
        ("Content-Disposition", (_, "form-data", ("name", name) ::
            args))
        :: other_lines
        ->
          begin
            match other_lines, args with
              [], [] ->
                let buf = Buffer.create 1000 in
                let rec iter () =
                  let line = Stream_in.input_netline ic in
                  if line = boundary || line = boundary2
                    || (
                      (line = end_boundary || line = end_boundary2)
                      && (end_boundary_found := true; true)
                      )
                  then
                    Buffer.contents buf
                  else
                    begin
                      Buffer.add_string buf (convert_nl line);
                      iter ()
                    end
                in
                let value = iter () in
                { arg_name = name;
                  arg_value = value;
                  arg_headers = lines;
                  arg_args = args
                }
            |  _ ->
                let tmpfile = Filename.temp_file "http_" "" in
                if !debug then
                    lprintf_http_nl () "WARNING: saving to file %s" tmpfile;
                let oc = open_out tmpfile in
                let rec iter n empty_line =
                  if n > !upload_limit then
                    failwith "File too big for upload"
                  else
                  let line = Stream_in.input_line ic in
                  if line = boundary || line = boundary2 ||
                    line = boundary3 || line = boundary4
                    || (
                      (line = end_boundary || line = end_boundary2
                          || line = end_boundary3 || line = end_boundary4
                        )
                      && (end_boundary_found := true; true)
                      )
                   then
                    close_out oc
                  else
                    begin
                      let len = String.length line in
                      output_string oc empty_line;
                      let empty_line =
                        if line = "\r" || line = "" then "\n" ^ line else ""
                      in
                      if n > 0 && empty_line = "" then output_char oc '\n';
                      output_string oc line;
                      iter (len + 1 + n) empty_line
                    end
                in
                iter 0 "";
                Finalizer.add finalizers tmpfile (fun _ ->
                    Sys.remove tmpfile);
                { arg_name = name;
                  arg_value = tmpfile;
                  arg_headers = lines;
                  arg_args = args
                }

          end
      | (name, (_,value, args)) :: lines ->
          if !debug then begin
              lprintf_http_nl () "ILL FORMED LINE: [%s]" name;
              List.iter (fun (name, v) ->
                  lprintf " (%s,%s)" name v) args;
              lprint_newline ();
            end;
          raise Exit
      | [] ->
          if !debug then lprintf_http_nl () "NO LINES";
          raise Exit
    in
    if !end_boundary_found then field :: previous else
    get_one_part (field :: previous)
  in
  let form_args = List.rev (find_first ()) in
  { request with form_args = form_args }
    *)

let parse_post_args f len req b =
(* parse post args *)
  if !verbose then lprintf_http_nl () "CALL HANDLER";
  let s = String.sub b.rbuf b.rpos len in
  Select.buf_used b len;
  let args = Url.cut_args s in
  let req = { req with get_url = { req.get_url
      with Url.args = args }} in
  f b req

let check_len f len b pos2 =
  if !verbose then lprintf_http_nl () "check_len: len %d rlen %d" len b.rlen;
  if b.rlen >= len then f b

let complete_post_request ( f : handler ) buf request =
  let len = request.options.content_length in
  if !verbose then lprintf_http_nl () "check_len: len %d rlen %d" len buf.rlen;
  if buf.rlen >= len then
    parse_post_args f len request buf
  else
  let post_reader = Select.buf_reader buf (check_len (parse_post_args f len request) len ) in
  buf.fd_task.Select.reader <- post_reader

(*
  if request.request = "POST" then
    let value = request.options.content_type in
    let head, tail = String2.cut_at value ';' in
    if head = "multipart/form-data" then
      failwith "complete_multipart_data Not implemented"
(*      complete_multipart_data request ic tail *)
    else
(*
    let s = String.create 1000 in
    let buf = Buffer.create 1000 in
    let rec iter nleft =
      if nleft > 0 then
        let nread = Stream_in.input ic s 0 (min nleft 1000) in
        Buffer.add_substring buf s 0 nread;
        if nread = nleft then Buffer.contents buf
        else
        if nread = 0 then failwith "Connection close while reading"
        else
          iter (nleft - nread)
      else Buffer.contents buf
in

let doc = iter request.options.content_length in
*)
    let args = Url.cut_args doc in
    { request with get_url = { request.get_url
      with Url.args = args }}
  else f request buf
    *)

let connection_wrapper = ref
    (fun f t x -> f t x)

let fd t = t.fd
exception ForbiddenAddr


let stream_out_string buf s =
  Select.write buf.fd_task s 0 (String.length s)

let head_404_error =
  "HTTP/1.0 404 Not Found\n" ^
  "Server: http_server (ocaml)\n" ^
  "Connection: close\n" ^
  "Content-Type: text/html; charset=iso-8859-1\n"

let error_404 oc page =
  stream_out_string oc head_404_error;
  stream_out_string oc "\n";
  stream_out_string oc page

let head_404_simple_msg =
  head_404_error ^ "\n<html><body>erreur 404</body></html>"

let simple_error_404 oc =
  error_404 oc
    "<html><body>erreur 404</body></html>"

let head_200_html_page =
  "HTTP/1.0 200 OK\n" ^
  "Server: http_server (ocaml)\n" ^
  "Connection: close\n" ^
  "Content-Type: text/html; charset=iso-8859-1\n"

let output_page oc page =
  stream_out_string oc head_200_html_page;
  stream_out_string oc "\n";
  stream_out_string oc page;
  at_write_end oc.fd_task shutdown


let print_request oc request =
  error_404 oc "";
  stream_out_string oc "<html><body>\n";
  stream_out_string oc "UNKNOWN REQUEST:<br>\n";
  stream_out_string oc (
    Printf.sprintf "REQUEST: [%s]<br>\n" request.request);
  stream_out_string oc (
    Printf.sprintf "URL: [%s]<br>\n" (Url.string_of_url request.get_url));
  List.iter (fun (name, (value,_,_)) ->
      stream_out_string oc (Printf.sprintf "HEADER: [%s]=[%s]<br>\n"
          name value)) request.headers;
  List.iter (fun (name, value) ->
      stream_out_string oc (Printf.sprintf "ARGS: [%s]=[%s]<br>\n"
          name value)) request.get_url.Url.args;
  List.iter (fun arg ->
      stream_out_string oc (Printf.sprintf "FORM ARG: [%s]=[%s]<br>\n"
          arg.arg_name arg.arg_value);
      List.iter (fun (name, (value,_,_)) ->
          stream_out_string oc (Printf.sprintf "ARG HEADER: [%s]=[%s]<br>\n"
              name value)) arg.arg_headers;
      ) request.form_args;
  stream_out_string oc "</html></body>\n";
  at_write_end oc.fd_task shutdown

let html_content =  "text/html; charset=iso-8859-1"
let content_types = [
    ".htm", html_content;
    ".html",  "text/html";
    ".htm",       "text/html";
    ".txt",       "text/plain";
    ".ps",           "application/postscript";
    ".dvi",          "application/x-dvi";
    ".gif",          "image/gif";
    ".jpeg",         "image/jpeg";
    ".jpg",          "image/jpeg";
    ".tiff",         "image/tiff";
    ".tif",          "image/tiff";
    ".png",          "image/png";
    ".au",           "audio/basic";
    ".snd",          "audio/basic";
    ".wav",          "audio/x-wav";
    ".mpeg",         "video/mpeg";
    ".mpg",          "video/mpeg";
    ".avi",          "video/avi";
    ".ogm",          "video/ogm";
    ".fli",          "video/fli";
    ".flc",          "video/fli";

    ".ps", "application/postscript";
    ".pdf","application/pdf";
    ".txt", "text/plain";
    ".gif", "image/gif";
  ]

let content_type_exts =
  List.map (fun (a,b) -> (b,a)) content_types

let content_encodings = [
    ".gz", "x-gzip";
    ".gz", "gzip";
    ".Z",            "compress";
    ".asc",          "pgp";
    ".pgp",          "pgp"
  ]

let content_encoding_exts =
  List.map (fun (a,b) -> (b,a)) content_encodings

let add_content_type oc file =
  let exts = Filename2.extensions (String.lowercase  file) in
  (try
      let rec iter exts =
        match exts with
          [] -> ()
        | ext :: exts ->
            let ext = "." ^ ext in
            try
              let t = List.assoc ext content_types in
              stream_out_string oc
                (Printf.sprintf "Content-Type: %s\n" t)
            with _ ->
(*
  lprintf "No content-type for %s" ext;
lprint_newline ();
  *)
                iter exts
      in
      iter exts
    with _ -> ())

let add_content_encoding oc file =
  let exts = Filename2.extensions (String.lowercase  file) in
  (try
      let rec iter exts =
        match exts with
          [] -> ()
        | ext :: exts ->
            try
              let ext = "." ^ ext in
              let encoding = List.assoc ext content_encodings in
              stream_out_string oc
                (Printf.sprintf "Content-Encoding: %s\n"
                  encoding)
            with _ ->
(*                lprintf "No encoding for %s" ext;
lprint_newline ();
  *)
                iter exts
      in
      iter exts
    with _ ->         ())


let give_doc buf request =
  let t = buf.fd_task.info in
  let file = request.get_url.Url.file in
  try
    if String2.subcontains file ".." then raise Not_found;
    let file =
      Filename.concat t.config.base_ref file in
    if Unix2.is_directory file then
      raise Not_found
    else
    let ans = File.to_string file  in
    stream_out_string buf "HTTP/1.0 200 OK\n";
    stream_out_string buf "Server: http_server (ocaml)\n";
    stream_out_string buf "Connection: close\n";

(* manage the extension *)
    add_content_type buf  file;
    add_content_encoding buf file;
    stream_out_string buf "\n";
(* answer *)
    stream_out_string buf ans;
    at_write_end buf.fd_task shutdown;
  with e ->
      lprintf_http_nl () "No such file: %s (%s)" file (Printexc2.to_string e);
      simple_error_404 buf;
      at_write_end buf.fd_task shutdown
*)

(*
let simple_give_auth psread pswrite request  =
  try
    if request.options.passwd = pswrite then
      (lprintf  "  Access: Write"; lprint_newline ();
        Write_auth)
    else if
      request.options.passwd = psread then
      (lprintf  "  Access: Read"; lprint_newline ();
        Read_auth)
    else raise Not_found
  with _ ->
      lprintf "  Access: Forbidden"; lprint_newline ();
      No_auth

let check_auth auth give_auth handler buf request =
  let authorization = give_auth request in
  match auth, authorization with
    No_auth, _
  | Read_auth, (Read_auth | Write_auth)
  | Write_auth, Write_auth -> handler buf request
  | _ -> need_auth buf


let fork_request handler req = handler req


let create config =
(*
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
Sys.set_signal Sys.sigchld (Sys.Signal_handle sigchild_handler);
  *)
(* bind *)
  let fds = socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  setsockopt fds Unix.SO_REUSEADDR true;
  bind fds (Unix.ADDR_INET (Unix.inet_addr_any, config.port));
  listen fds 5;
  let t =
    {
      config = config;
      fd = fds;
      regaddrs = Str.regexp config.addrs;
    } in
  let event_handler _ _ = () in (* no event expected *)
  let fd_task = Select.create t.fd t in
  fd_task.reader <- handle_connection t;
  fd_task.event_handler <- event_handler;
  t
    *)

open TcpBufferedSocket

let manage config sock head =

  let request = parse_head sock head in
  let rec iter reqs =
    match reqs with
      (file, handler) :: reqs when file = request.get_url.Url.short_file ->
        handler sock request
    | _ :: reqs -> iter reqs
    | [] ->
        config.default sock request
  in
  iter config.requests;
  let buf = Buffer.create 1000 in
  Printf.bprintf buf "HTTP/1.1 %s\r\n" request.reply_head;
  List.iter (fun (header, line) ->
      Printf.bprintf buf "%s: %s\r\n" header line
  ) request.reply_headers;
  let c = request.reply_content in
  let clen = String.length c in
  (match request.reply_stream with
      None ->
        Printf.bprintf buf "Content-length: %d\r\n" clen
    | Some _ -> ());
  Printf.bprintf buf "\r\n";

  let s = Buffer.contents buf in
  let len = String.length s in

  if len+clen > max_refill sock then
    TcpBufferedSocket.set_max_output_buffer sock (len + clen);
(*  lprintf "HTTPSEND: [%s]\n" (String.escaped s); - log commented out *)
  TcpBufferedSocket.write_string sock s;

  if request.request <> "HEAD" then begin
(*        lprintf "HTTPSEND: [%s]\n" (String.escaped c); - log commented out *)
      TcpBufferedSocket.write_string sock c;
      match request.reply_stream with
        None -> ()
      | Some refill ->
          set_refill sock refill;
    end;
  TcpBufferedSocket.close_after_write sock


let request_handler config sock nread =
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  let new_pos = max 0 (new_pos - 1) in
(*  lprintf "received [%s]\n" (String.escaped
      (String.sub b.buf new_pos nread)); - log commented out *)
  let rec iter i =
    let end_pos = b.pos + b.len in
    if i < end_pos then
      if Bytes.get b.buf i = '\n' && i <= end_pos - 2 then
        let c = Bytes.get b.buf (i+1) in
        if c = '\n' then
          let len = i + 2 - b.pos in
          let header = Bytes.sub_string b.buf b.pos len in
          buf_used b len;
          manage config sock header
        else
        if c = '\r' && i <= end_pos - 3 && Bytes.get b.buf (i+2) = '\n' then
          let len = i + 3 - b.pos in
          let header = Bytes.sub_string b.buf b.pos len in
          buf_used b len;
          manage config sock header
        else
          iter (i+1)
      else
        iter (i+1)
    else
      ()
  in
  try
    iter new_pos
  with e ->
      if !verbose then lprintf_nl "Exception %s in request_handler"
        (Printexc2.to_string e);
      close sock (Closed_for_exception e)

let request_closer sock msg =
  ()


let handler config t event =
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET(from_ip, from_port)) ->
    (* check here if ip is OK *)
      let from_ip = Ip.of_inet_addr from_ip in
      let ip_is_allowed from_ip =
        Ip_set.match_ip config.addrs from_ip
      in
      let ip_is_blocked from_ip =
        if config.use_ip_block_list then
          match !Ip.banned (from_ip, None) with
            None -> false
           | Some reason -> lprintf_nl "%s:%d blocked: %s"
               (Ip.to_string from_ip) from_port reason; true
        else
          false
      in 
      if ip_is_allowed from_ip && not (ip_is_blocked from_ip) then
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.create_simple
            token "http connection" s in
        TcpBufferedSocket.prevent_close sock;
        TcpBufferedSocket.set_reader sock (request_handler config);
        TcpBufferedSocket.set_closer sock request_closer;
        TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
          (fun _ -> TcpBufferedSocket.close sock Closed_for_overflow;
         lprintf_nl "BUFFER OVERFLOW" );  ()
      else begin
        lprintf_nl "connection from %s rejected (%s)"
          (Ip.to_string from_ip)
          (if ip_is_blocked from_ip then "IP is blocked" else "see allowed_ips setting");
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.create_simple token "http connection" s in
        let s1,s2,_ = error_page
      (if not (ip_is_allowed from_ip) then Not_allowed from_ip else Blocked from_ip)
            (TcpBufferedSocket.my_ip sock)
            config.port
        in
        TcpBufferedSocket.write_string sock (Printf.sprintf "%s\n%s" s1 s2);
        shutdown sock Closed_connect_failed;
        Unix.close s
      end
  | _ -> ()

let create config =
  let t = TcpServerSocket.create "http server" config.bind_addr
    config.port (handler config) in
  t

let add_reply_header r header value =
  let rec iter headers add =
    match headers with
      [] -> if add then [header, value] else []
    | (h, v) :: tail ->
        if h = header then
          (h, value) :: (iter tail false)
        else
          (h, v) :: (iter tail true)
  in
  r.reply_headers <- iter r.reply_headers true


let parse_range range =
  try
    let npos = (String.index range 'b')+6 in
    let len = String.length range in
    let dash_pos = try String.index range '-' with _ -> -10 in
    let slash_pos = try String.index range '/' with _ -> len in
    let star_pos = try String.index range '*' with _ -> -30 in
    if star_pos = slash_pos-1 then
      Int64.zero, None, None (* "bytes */X" *)
    else
    let x = Int64.of_string (
        String.sub range npos (dash_pos - npos) )
    in
    if len = dash_pos + 1 then
(* bytes x- *)
      x, None, None
    else
    let y = Int64.of_string (
        String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
    in
    if slash_pos = len then
      x, Some y, None (* "bytes=x-y" *)
    else
    if slash_pos = star_pos - 1 then
      x, Some y, None (* "bytes x-y/*" *)
    else
(* bytes x-y/len *)

    let z = Int64.of_string (
        String.sub range (slash_pos+1) (len - slash_pos -1) )
    in
    x, Some y, Some z
  with
  | e ->
      lprintf_nl "Exception %s for range [%s]"
        (Printexc2.to_string e) range;
      raise e

(*
let parse_range range =
  let x, y, z = parse_range range in
  lprintf "Range parsed: %Ld-%s/%s" x
    (match y with None -> "" | Some y -> Int64.to_string y)
  (match z with None -> "*" | Some y -> Int64.to_string y);
  x, y, z
    *)

(*  Range: bytes=31371876- *)
let request_range r =
  if !verbose then List.iter (fun (h, v1) ->
      lprintf_nl "HEADER [%s] = [%s]" h v1
  ) r.headers;
  let range = List.assoc "Range" r.headers in
  match parse_range range with
    x, None, _ -> x, None
  | x, Some y, Some z ->
      if y = z then (* some vendor bug *)
        Int64.pred x, Some y
      else
        x, Some (Int64.succ y)
  | x, Some y, None ->
      x, Some (Int64.succ y)

