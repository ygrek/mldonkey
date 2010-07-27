
open Printf
open Bencode
module BS = BasicSocket

type network_porttest =
  PorttestNotAvailable
| PorttestNotStarted
| PorttestInProgress of int
| PorttestResult of int * string
let azureus_porttest_random = ref 0
let last_time () = 0
let (!!) = (!)
let client_port = ref 55155
let get_user_agent () = "test"

let porttest_result = ref PorttestNotStarted

let pr fmt = Printf.ksprintf print_endline fmt

let interpret_azureus_porttest s =
  let failure_message fmt = 
    Printf.sprintf ("Port test failure, " ^^ fmt) in
  try
    let value = decode s in 
    match value with
    | Dictionary alist ->
        (try
           match List.assoc "result" alist with
           | Int 1L -> "Port test OK!"
           | Int 0L ->
               (try
                  match List.assoc "reason" alist with
                  | String reason -> failure_message "%s" reason
                  | _ -> raise Not_found
                with Not_found ->
                  failure_message "%s" "no reason given")
           | Int status ->
               failure_message "unknown status code (%Ld)" status
           | _ -> raise Not_found
         with Not_found ->
           failure_message "%s" "no status given")
    | _ ->
        failure_message "unexpected value type %s" (Bencode.print value)
  with _ ->
    failure_message "%s" "broken bencoded value"

let interpret_utorrent_porttest s =
  if String2.contains s "<div class=\"status-image\">OK!</div>" then
    "OK"
  else
    "Port is not accessible"

let start tests =
  let module H = Http_client in
  match tests with
  | [] -> porttest_result := PorttestResult (last_time(), "No tests available")
  | _ ->
  porttest_result := PorttestInProgress (last_time ());
  let rec loop = function
  | [] -> exit 1
  | (url,interpret)::other ->
    let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_user_agent = get_user_agent ();
      H.req_max_total_time = 30.;
    } in
    pr "start %s" url;
    H.wget_string r 
      (fun s -> 
        pr "got %d %s" (String.length s) (interpret s); 
        porttest_result := PorttestResult (last_time (), interpret s);
        exit 0)
      ~ferr:(fun code -> 
        pr "error %d" code;
        porttest_result := PorttestResult (last_time (), sprintf "Remote service error (%d)" code);
        loop other)
      (fun _ _ -> ())
  in
  loop tests

let () =
  azureus_porttest_random := (Random.int 100000);
  let tests = [
    sprintf "http://azureus.aelitis.com/natcheck.php?port=%d&check=azureus_rand_%d"
      !client_port !azureus_porttest_random, interpret_azureus_porttest;
    sprintf "http://www.utorrent.com/testport?port=%d" !client_port, interpret_utorrent_porttest;
  ] in
  start tests;
  BasicSocket.loop ()

