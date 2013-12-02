open OUnit2;;

let _ =
  run_test_tt_main ("Unit Tests" >::: [
    (* Add new test suites here *)
    Httpserver_test.suite;
    Jsonapi_test.suite;
  ])
;;




