open Prelude;;
open OUnit2;;

class server =
  object
    method query (path : string) =
      "0.1"
  end
;;

type fixture = {
  server: server;
};;

let fixture1 context = bracket
  (* set_up *)
    (fun ctx -> {
        server = new server;
      }
    )
  (* tear_down *)
    (fun fix ctx -> ())
  (* test_ctxt *)
    context
;;


let suite = "HTTP server" >::: [

  "test_version" >:: fun context -> (
    let server = (fixture1 context).server in
    let result = server#query "api_version" in
    assert_equal "0.1" result;
    ); ;

  ]
;;


