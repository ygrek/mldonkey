open Prelude;;
open OUnit2;;

let assert_equal_json expected result =
  (* TODO: Make the results be json formatting agnostic *)
  OUnit2.assert_equal expected result
    ~printer:(fun x -> x)
;;


exception Bad_query;;


class server =
  object (self)
    method query (path : string) =
      match path with
      | "api_version" -> "0.1";
      | "downloads" -> "[]";
      | _ -> raise Bad_query;

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


Test_collector.register("Json Api" >::: [

  "test_badquery" >:: fun context -> (
    let server = (fixture1 context).server in

    assert_raises Bad_query (fun _ -> server#query "bad_query")
    ); ;

  "test_version" >:: fun context -> (
    let server = (fixture1 context).server in

    assert_equal_json "0.1" (server#query "api_version");
    ); ;

  "test_downloads_whenNoSearch" >:: fun context -> (
    let server = (fixture1 context).server in

    assert_equal_json
      ("[]")
      (server#query "downloads");
    ); ;

  ]
)
;;


