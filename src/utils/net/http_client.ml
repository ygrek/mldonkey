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


(* HTTP Requests:
  GET, POST, HEAD, PUT, DELETE, TRACE, OPTIONS, CONNECT
*)

open Printf2
open BasicSocket
open Url
open TcpBufferedSocket
open Int64ops


type http_request =
  GET
| POST
| HEAD
| PUT
| DELETE
| TRACE

type error = [ `HTTP of int | `RST of BasicSocket.close_reason | `DNS | `Block of Ip.t ]

let show_error = function
| `HTTP code -> Printf.sprintf "HTTP error code %d" code
| `RST reason -> Printf.sprintf "Connection closed : %s" (BasicSocket.string_of_reason reason)
| `DNS -> Printf.sprintf "DNS resolution failed"
| `Block ip -> Printf.sprintf "Blocked connection to %s" (Ip.to_string ip)

let verbose = ref false

type request = {
    req_headers : ( string * string ) list;
    req_user_agent : string;
    req_accept : string;
    req_proxy : (string * int * (string * string) option) option; (* (host,port,(login,password)) *)
    mutable req_url : url;
    mutable req_gzip : bool;
    mutable req_save_to_file_time : float;
    req_request : http_request;
    req_referer : Url.url option;
    req_retry : int;
    req_max_retry : int;
    req_save : bool;
    req_max_total_time : float;
    req_filter_ip : (Ip.t -> bool);
  }

type content_handler = 
  int64 -> (string * string) list -> TcpBufferedSocket.t -> int -> unit

let log_prefix = "[HTTPcl]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let basic_request = {
    req_url = Url.of_string "http://mldonkey.sf.net/";
    req_referer = None;
    req_save_to_file_time = 0.;
    req_request = GET;
    req_gzip = false;
    req_proxy = None;
    req_headers = [];
    req_user_agent = "Wget 1.4";
    req_accept = "*/*";
    req_retry = 0;
    req_max_retry = 0;
    req_save = false;
    req_max_total_time = infinite_timeout;
    req_filter_ip = (fun _ -> true);
  }

let make_full_request r =
  let url = r.req_url in
  let args = url.args in
  let res = Buffer.create 80 in
  let is_real_post = r.req_request = POST && args <> [] in
  if is_real_post
  then Buffer.add_string res "POST "
  else 
    Buffer.add_string res (if r.req_request = HEAD then "HEAD " else "GET ");
  Buffer.add_string res (
    let url = 
      if r.req_proxy <> None
      then  Url.to_string_no_args url
      else url.short_file
    in
  (* I get a lot more bittorrent urls with this line: *)
  let url = (Str.global_replace (Str.regexp " ") "%20" url) in
    let url = if is_real_post then url else
        Url.put_args url args
    in
    url);
  Printf.bprintf res " HTTP/1.0\r\nHost: %s%s\r\n" url.server (if url.port != 80 then Printf.sprintf ":%d" url.port else "");
  List.iter (fun (a,b) ->
      Printf.bprintf res "%s: %s\r\n" a b
  ) r.req_headers;
  Printf.bprintf res "Accept-Encoding: gzip\r\n";
  Printf.bprintf res "User-Agent: %s\r\n" r.req_user_agent;
  Printf.bprintf res "Accept: %s\r\n" r.req_accept;
  Printf.bprintf res "Connection: close\r\n";
  begin match r.req_referer with 
  | None -> ()
  | Some url -> Printf.bprintf res "Referer: %s\r\n" (Url.to_string_no_args url)
  end;
  begin match r.req_proxy with
  | Some (_,_,Some (login,password)) ->
      Printf.bprintf res "Proxy-Authorization: Basic %s\n" (Base64.encode (login ^ ":" ^ password))
  | _ -> ()
  end;
  if url.user <> "" then begin
    let userpass = Printf.sprintf "%s:%s" url.user url.passwd in
    Printf.bprintf res "Authorization: Basic %s\r\n" (Base64.encode userpass)
  end;
  if is_real_post then begin
      let post = Buffer.create 80 in
      let rec make_post = function
          | [] -> assert false
        | [a, b] ->
            Printf.bprintf post "%s%c%s" (Url.encode a) '=' (Url.encode b)
        | (a,b)::l ->
            Printf.bprintf post "%s%c%s%c" 
              (Url.encode a) '=' (Url.encode b) '&';
            make_post l in
      make_post args;
      Printf.bprintf res "Content-Type: application/x-www-form-urlencoded\r\nContent-Length: %d\r\n\r\n%s"
        (Buffer.length post) (Buffer.contents post)
    end else
    Buffer.add_string res "\r\n";
  let s = Buffer.contents res in
  if !verbose then
    lprintf_nl "make_full_request on URL: %s" (String.escaped s);
  s

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

let parse_header headers_handler sock header =
  let headers = split_head header in
  match headers with 
    [] -> failwith "Ill formed reply"
  | ans :: headers ->
      if !verbose then lprintf_nl "parse_header: ANSWER %s" ans;
      let ans_code = int_of_string (String.sub ans 9 3) in
      let headers = List.map (fun s ->
            let sep = String.index s ':' in
            (* TODO: we should lowercase the names here!
               The header-names are case-insensitive,
               therefore we only use lowercased names. *)
            let name_head = String.sub s 0 sep in
            let size = String.length s in
            let content_head = String.sub s (sep+2) (size-sep-2) in
            (name_head, content_head)
        ) headers in
      try
        headers_handler sock ans_code headers;
      with _ -> 
          TcpBufferedSocket.close sock (Closed_for_error "bad header")

let read_header header_handler sock nread =  
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  let new_pos = max 0 (new_pos - 1) in
  (*
  lprintf "received [%s]" (String.escaped
      (String.sub b.buf new_pos nread));
  *)
  let rec iter i =
    let end_pos = b.pos + b.len in
    if i < end_pos then
      if Bytes.get b.buf i = '\n' && i <= end_pos - 2 then
        let c = (Bytes.get b.buf (i+1)) in
        if c = '\n' then
          let len = i + 2 - b.pos in
          let header = Bytes.sub_string b.buf b.pos len in
          buf_used b len;
          header_handler sock header
        else
        if c = '\r' && i <= end_pos - 3 && (Bytes.get b.buf (i+2)) = '\n' then
          let len = i + 3 - b.pos in
          let header = Bytes.sub_string b.buf b.pos len in
          buf_used b len;
          header_handler sock header
        else 
          iter (i+1)
      else
        iter (i+1)
    else
      ()
  in
  iter new_pos

let http_reply_handler nr headers_handler sock nread =
(*  lprintf "http_reply_handler\n"; *)
  nr := true;
  read_header (parse_header headers_handler) sock nread
  

let def_ferr = (fun _ -> ())

let rec get_page r content_handler f ferr =
  let ok = ref false in
  let ferr =
    let err_done = ref false in (* call not more than once *)
    fun c -> if not !err_done then begin err_done := true; ferr c; end 
  in
  let rec get_url level r =
  try
    let url = r.req_url in
    let level = r.req_retry in
    let request = make_full_request r in
    let server, port =
      match r.req_proxy with
      | None -> url.server, url.port
      | Some (s, p, _) -> s, p
    in
(*    lprintf "async_ip ...\n"; *)
    Ip.async_ip server (fun ip ->
        match r.req_filter_ip ip with
        | false -> ferr (`Block ip)
        | true ->
(*         lprintf "IP done %s:%d\n" (Ip.to_string ip) port;*)
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.connect token "http client connecting"
          (try Ip.to_inet_addr ip with e -> raise Not_found) port
          (fun sock e -> 
(*             if !verbose then lprintf_nl "Event %s" (string_of_event e); *)
            match e with (* FIXME content-length check *)
            | BASIC_EVENT (CLOSED (Closed_by_user | Closed_by_peer)) when !ok -> f ()
            | BASIC_EVENT (CLOSED reason) -> ferr (`RST reason)
            | BASIC_EVENT LTIMEOUT -> close sock Closed_for_lifetime
            | _ -> ())
        in

        let nread = ref false in
        if !verbose then 
          lprintf_nl "get_page: %s" (String.escaped request);
        TcpBufferedSocket.write_string sock request;
        TcpBufferedSocket.set_reader sock (http_reply_handler nread
            (default_headers_handler url level));
        set_rtimeout sock 5.;
        set_lifetime sock r.req_max_total_time;
    )
    (fun () -> ferr `DNS);
  with e -> 
    lprintf_nl "error in get_url"; 
    raise Not_found

  and default_headers_handler old_url level sock ans_code headers =
    let print_headers () =
      List.iter (fun (name, value) ->
          lprintf_nl "[%s]=[%s]" name value;
        ) headers;
    in
    if !verbose then print_headers ();
    match ans_code with
    | 200 ->
        ok := true;
        let content_length = ref (-1L) in
        List.iter (fun (name, content) ->
            match String.lowercase name with
            | "content-length" ->
                (try
                  content_length := Int64.of_string content
                with _ ->
                  lprintf_nl "bad content length [%s]" content)
            | "content-encoding" ->
                if String.lowercase content = "gzip" then r.req_gzip <- true
            | _ -> ()
        ) headers;
        let location = "Location", Url.to_string old_url in
        let content_handler = content_handler !content_length (location::headers) in
        set_reader sock content_handler;
        let buf = TcpBufferedSocket.buf sock in
        if buf.len > 0 then
          content_handler sock buf.len

    | 301 | 302 | 304 ->
        if !verbose then lprintf_nl "%d: Redirect" ans_code;
  let retrynum = r.req_retry in
        if retrynum < r.req_max_retry then begin
            try
              let url = ref "" in
            List.iter (fun (name, content) ->
                  if String.lowercase name = "location" then
                    url := content;
                ) headers;
            if !verbose then print_headers ();
              let url =
                if String2.check_prefix !url "." then url := String2.after !url 1;
              if String.length !url > 0 && !url.[0] <> '/' 
                then !url
                else Printf.sprintf "http://%s%s%s"
                    old_url.Url.server
                    (if old_url.Url.port = 80 then "" else Printf.sprintf ":%d" old_url.Url.port)
                    !url
              in

              if !verbose then lprintf_nl "Redirected to %s" url;
              r.req_url <- (Url.of_string url);
              let r = { r with
      req_url = Url.of_string url;
              req_retry = retrynum+1 
            } in
            get_page r content_handler f ferr
            
            with e ->
                lprintf_nl "error understanding redirect response %d" ans_code;
                print_headers ();
                raise Not_found
                
          end
        else begin
          lprintf_nl "more than %d redirections, aborting." r.req_max_retry;
          raise Not_found
        end
          
    | 400 when r.req_request = HEAD ->
        lprintf_nl "Error 400 received for HEAD %s, re-try GET" (Url.to_string_no_args r.req_url);
        let r2 = {
          r with
          req_request = GET;
        } in
        get_page r2 content_handler f ferr

    | 404 ->
        lprintf_nl "404: Not found for: %s" (Url.to_string_no_args r.req_url);
        close sock (Closed_for_error "bad reply");
        ferr (`HTTP ans_code);
        raise Not_found

    | 502 | 503 | 504 ->
        if !verbose then lprintf_nl "%d: Unavailable" ans_code;
  let retrynum = r.req_retry in
        if retrynum < r.req_max_retry then begin
          if !verbose then print_headers ();
      let seconds = (retrynum+1)*10 in
              lprintf_nl "retry %d/%d in %d seconds for %s"
          (retrynum+1) r.req_max_retry seconds (Url.to_string_no_args r.req_url);
          let r = { r with 
            req_retry = retrynum+1 
          } in
          add_timer (float(seconds)) (fun t -> get_page r content_handler f ferr)
    end
        else begin
          lprintf_nl "more than %d retries, aborting." r.req_max_retry;
          ferr (`HTTP ans_code);
          raise Not_found
        end
          
    | _ ->
        lprintf_nl "%d: bad reply for: %s"
          ans_code (Url.to_string_no_args r.req_url);
        close sock (Closed_for_error "bad reply");
        ferr (`HTTP ans_code);
        raise Not_found
  in
  get_url 0 r

(** Copy all data from [input] to [output] *)
let io_copy input output =
  try
    let size = 16 * 1024 in
    let s = String.create size in
    while true do
      let n = IO.input input s 0 size in
      if n = 0 then raise IO.No_more_input;
      ignore (IO.really_output output s 0 n)
    done
  with IO.No_more_input -> ()

let wget r f = 

  let file_buf = Buffer.create 1000 in
  let file_size = ref 0L in

  try
  get_page r (fun maxlen headers sock nread ->
(*      lprintf "received %d\n" nread; *)
      let buf = TcpBufferedSocket.buf sock in
      
      if nread > 0 then begin
          let left = 
            if maxlen >= 0L then
              min (Int64.to_int (maxlen -- !file_size)) nread
            else nread
          in
          Buffer.add_bytes file_buf (Bytes.sub buf.buf buf.pos left);
          buf_used buf left;
          file_size := !file_size ++ (Int64.of_int left);
          if nread > left then
            TcpBufferedSocket.close sock Closed_by_user
        end
  )
  (fun _ ->  
      let s = Buffer.contents file_buf in
      if s = "" then begin
          lprintf_nl "Empty content for url %s"
            (Url.to_string r.req_url);
      end;
      
      let webinfos_dir = "web_infos" in
      Unix2.safe_mkdir webinfos_dir;
      Unix2.can_write_to_directory webinfos_dir;
      
      let base = Filename.basename r.req_url.Url.short_file in
      (* Base could be "." for http://site.com/ *)
      let base = if base = "." 
        then begin
          let prng = Random.State.make_self_init () in
          let rnd = (Random.State.bits prng) land 0xFFFFFF in
          Printf.sprintf "http_%06x.tmp" rnd 
        end else base 
      in

      let filename = Filename.concat webinfos_dir base in
      if !verbose then lprintf_nl "Filename: %s" filename;
      if r.req_gzip then
      begin
        try
          Unix2.tryopen_write_bin filename begin fun oc ->
            let gz = Gzip.input_io (IO.input_string s) in
            io_copy gz (IO.output_channel oc)
          end
        with e ->
          lprintf_nl "Exception %s while uncompressing content from %s" (Printexc2.to_string e) (Url.to_string r.req_url);
          Sys.remove filename;
          raise Not_found
      end
      else
        Unix2.tryopen_write_bin filename (fun oc -> output_string oc s);
      if r.req_save_to_file_time <> 0. then
        Unix.utimes filename r.req_save_to_file_time r.req_save_to_file_time;
      try
        (f filename : unit);
        if not r.req_save then Sys.remove filename
      with e ->  
        lprintf_nl "Exception %s in loading downloaded file %s" (Printexc2.to_string e) filename;
        Sys.remove filename;
        raise Not_found
  ) def_ferr
  with e -> 
    lprintf_nl "Exception %s in wget" (Printexc2.to_string e); 
    raise Not_found

let whead2 r f ferr = 
  get_page r
    (fun maxlen headers ->
      (try f headers with _ -> ());
      fun sock nread -> 
        close sock Closed_by_user
    )
  (fun _ ->  ())
  ferr

let whead r f = whead2 r f def_ferr

let wget_string r f ?(ferr=def_ferr) progress =
    
  let file_buf = Buffer.create 1000 in
  let file_size = ref 0L in

  get_page r
    (fun maxlen headers sock nread ->
        let buf = TcpBufferedSocket.buf sock in
        
        if nread > 0 then begin
            let left = 
              if maxlen >= 0L then
                min (Int64.to_int (maxlen -- !file_size)) nread
              else nread
          in
          Buffer.add_bytes file_buf (Bytes.sub buf.buf buf.pos left);
          progress left maxlen;
          buf_used buf left;
          file_size := !file_size ++ (Int64.of_int left);
          if nread > left then
            TcpBufferedSocket.close sock Closed_by_user
        end)
  (fun _ ->
      let content = 
        if r.req_gzip then
          try
            let io = Gzip.input_io (IO.input_string (Buffer.contents file_buf)) in
            IO.read_all io
          with e -> 
            lprintf_nl "Exception %s while uncompressing content from %s" (Printexc2.to_string e) (Url.to_string r.req_url);
            raise Not_found
        else
          Buffer.contents file_buf
      in
      f content
  ) ferr


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
