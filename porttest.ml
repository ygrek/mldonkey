
open Printf
open Bencode

let url = sprintf "http://localhost/testport?port=%d"

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
let interpret_porttest s =
  s

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

let start () =
      let module H = Http_client in
      azureus_porttest_random := (Random.int 100000);
      porttest_result := PorttestInProgress (last_time ());
      let r = {
          H.basic_request with
          H.req_url =
            Url.of_string (Printf.sprintf
              "http://azureus.aelitis.com/natcheck.php?port=%d&check=azureus_rand_%d"
                !!client_port !azureus_porttest_random);
(*           H.req_proxy = !CommonOptions.http_proxy; *)
          H.req_user_agent = get_user_agent ();
        } in
      H.wget r (fun file ->
        let result = interpret_azureus_porttest (File.to_string file) in
        porttest_result := PorttestResult (last_time (), result)
      )

let start () =
  let module H = Http_client in
  porttest_result := PorttestInProgress (last_time ());
  let r = {
      H.basic_request with
      H.req_url = Url.of_string (url !client_port);
      H.req_user_agent = get_user_agent ();
    } in
  pr "start";
  H.wget_string r 
    (fun s -> 
      pr "got %d" (String.length s); 
      porttest_result := PorttestResult (last_time (), interpret_porttest s); 
      exit 0)
    ~ferr:(fun code -> 
      pr "error %d" code;
      porttest_result := PorttestResult (last_time (), sprintf "Failed (%d)" code);
      exit 1)
    (fun _ _ -> pr "progress"; ())

let () =
  start ();
  BasicSocket.loop ()

