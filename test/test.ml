
open OUnit

let (@@) f x = f x

let sample () =
  assert_equal 4 (2 + 2);
  ()

let run () =
  ignore @@ run_test_tt ("test" >::: [
    "sample" >:: sample
  ])

let () =
  run ()
