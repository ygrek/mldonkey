let all_tests = ref [];;

let register suite =
  let open OUnit2 in
  all_tests := suite :: !all_tests
;;

let run () =
  let open OUnit2 in
  run_test_tt_main ("Unit Test" >:::  !all_tests;)
;;




