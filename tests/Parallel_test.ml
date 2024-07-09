(*
   Simple test suite with minimal output, used to test parallel workers
*)

let t = Testo.create
let test_sleep () = Unix.sleepf 0.05

let tests _env =
  [
    t "test sleep #1" test_sleep;
    t "test sleep #2" test_sleep;
    t "test sleep #3" test_sleep;
    t "test sleep #4" test_sleep;
    t "test sleep #5" test_sleep;
    t "test sleep #6" test_sleep;
    t "test sleep #7" test_sleep;
    t "test sleep #8" test_sleep;
  ]

let () = Testo.interpret_argv ~project_name:"testo_parallel_test" tests
