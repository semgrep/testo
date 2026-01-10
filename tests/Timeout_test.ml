(*
   Test timeouts
*)

open Printf

let create_filler_test =
  let n = ref 0 in
  fun _ ->
    incr n;
    let id = !n in
    Testo.create (sprintf "filler test %i" id) (fun () -> ())

(* A well-behaved test configured with a time limit *)
let does_not_time_out =
  Testo.create "does not time out" ~max_duration:15. (fun () -> ())

let solo_test_with_unenforceable_timeout =
  Testo.create "solo test with unenforceable timeout" ~solo:"testing"
    ~max_duration:0.0001 (fun () -> Unix.sleepf 0.001)

(* Run tests sequentially in a detached worker (with -j1) to ensure that
   the test that times out is handled correctly and the subsequent tests
   run fine after the worker process is killed and respawned. *)
let tests _env =
  let filler_tests = List.init 2 create_filler_test in
  [
    does_not_time_out;
    solo_test_with_unenforceable_timeout;
    Testo.create ~max_duration:0.2 "taking too long" (fun () -> Unix.sleepf 5.);
  ]
  @ filler_tests

let () = Testo.interpret_argv ~project_name:"testo_timeout_test" tests
