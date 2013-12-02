open Prelude;;
open OUnit2;;

type fixture = {
  url: string;
};;

let assert_request url expected =
  assert_equal (`Ok expected) (Web.http_get url);
;;

let setup _ =
  {
    url="http://localhost/hello";
  }
;;

let teardown fixture context =
  ();
;;


let suite = "HTTP server" >::: [

  "test_case1" >:: fun _ -> (
    let a =4 in
    assert_equal a 4;
    ); ;

  "test_case2" >:: fun context -> (
    let fixture = bracket setup teardown context in
    assert_equal "http://localhost/hello" fixture.url;
    ); ;

  "test_http" >:: fun context -> (
    let fixture = bracket setup teardown context in
    assert_request fixture.url "hello";
    ); ;

  ]
;;


