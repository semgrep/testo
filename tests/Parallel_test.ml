(*
   Simple test suite with minimal output, used to test parallel workers
*)

open Printf

let sleep_duration = 0.05
let test_sleep () = Unix.sleepf sleep_duration

let tests _env =
  List.init 8 (fun i ->
      Testo.create
        (sprintf "sleep %.0fms #%i" (sleep_duration *. 1000.) (i + 1))
        test_sleep)

let () = Testo.interpret_argv ~project_name:"testo_parallel_test" tests
