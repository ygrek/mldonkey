
open Prelude
open OUnit

let http () =
  assert_equal (`Ok "hello") (Web.http_get "http://localhost/hello");
  ()

let run () =
  ignore @@ run_test_tt ("test" >::: [
    "sample" >:: http
  ])

let () =
  run ()
