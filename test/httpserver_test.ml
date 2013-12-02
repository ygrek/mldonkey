open Prelude;;
open OUnit2;;

type fixture = {
  url: string;
};;

let assert_request url expected =
  assert_equal (`Ok expected) (Web.http_get url);
;;

let fixture1 context = bracket
  (* set_up *)
    (fun ctx ->
      {
        url="http://localhost/hello";
      }
    )
  (* tear_down *)
    (fun fix ctx -> ())
  (* test_ctxt *)
    context
;;


let suite = "HTTP server" >::: [

  "test_case1" >:: fun _ -> (
    let a =4 in
    assert_equal a 4;
    ); ;

  "test_case2" >:: fun context -> (
    let afixture = fixture1 context in
    assert_equal "http://localhost/hello" afixture.url;
    ); ;

  "test_http" >:: fun context -> (
    let afixture = fixture1 context in
    assert_request afixture.url "hello";
    ); ;

  ]
;;


