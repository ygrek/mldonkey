
open Printf

let user_agent = "mldonkey api test"

let curl_default_setup h =
  let open Curl in
  set_useragent h user_agent;
  set_nosignal h true;
  set_connecttimeout h 5;
  set_timeout h 15;
  set_followlocation h false;
(*   set_maxredirs h 1; *)
  set_sslverifypeer h true;
  set_sslverifyhost h SSLVERIFYHOST_HOSTNAME;
(*   set_encoding h CURL_ENCODING_ANY; *)
  ()

let log_curl h ?attr ?(extra="") code =
  let url = Curl.get_effectiveurl h in
  sprintf "%3d %.2f%s %s%s%s URL: %s (%s)%s %s"
    (Curl.get_httpcode h)
    (Curl.get_totaltime h)
(*     (Action.bytes_string_f (Curl.get_sizedownload h)) *)
    (match attr with None -> "" | Some s -> sprintf " {%s}" s)
    (if Curl.get_httpcode h / 100 = 3 then sprintf "%s -> %s" url (Curl.get_redirecturl h) else url)
    (Curl.get_contenttype h)
    (match code with Curl.CURLE_OK -> "" | _ -> sprintf " %s (%d)" (Curl.strerror code) (Curl.errno code))
    extra

let with_curl f =
  let h = Curl.init () in
  Std.finally (fun () -> Curl.cleanup h) f h

let perform_http ?(setup=ignore) ?(check=(fun _ -> true)) ?(result=(fun _ _ -> ())) url =
  with_curl begin fun h ->
    Curl.set_url h url;
    curl_default_setup h;
    let () = setup h in
    let b = Buffer.create 10 in
    Curl.set_writefunction h begin fun s ->
      match check h with
      | true -> Buffer.add_string b s; String.length s
      | false -> 0
    end;
    try
      Curl.perform h;
      let () = result h Curl.CURLE_OK in
      `Ok (Curl.get_httpcode h, Buffer.contents b)
    with
    | Curl.CurlException (code,_,_) ->
      let () = result h code in
      `Error code
  end

let http_get ?setup ?check ?result url =
  match perform_http ?setup ?check ?result url with
  | `Ok (200, s) -> `Ok s
  | `Error code -> `Error (sprintf "(%d) %s" (Curl.errno code) (Curl.strerror code))
  | `Ok (n, _) -> `Error (sprintf "http %d" n)

let http_post ?(timeout=5) ~params url =
  let setup h =
    let open Curl in
    set_timeout h timeout;
    set_post h true;
    set_postfields h params;
    set_postfieldsize h (String.length params)
  in
  http_get ~setup url
