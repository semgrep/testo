(*
   Simple test suite with minimal output, used to test parallel workers
*)

open Printf

let sleep_duration = 0.05
let test_sleep () = Unix.sleepf sleep_duration

let tests _env =
  (*
     Simulate a worker that takes a while to gather the list of tests
     and might get killed before it even started listening for test requests.
     This causes a 'Sys_error "Broken pipe"' unless the sigpipe signal
     is handled in the worker.

     Output lines starting with "OPT" may or may not be printed from
     one run to another. The meta test will ignore them when checking
     the expected output.
  *)
  Unix.sleepf 0.1;
  printf "OPTIONAL harmless junk printed to stdout\n%!";
  eprintf "OPTIONAL harmless junk printed to stderr\n%!";
  at_exit (fun () ->
      printf "OPTIONAL harmless junk printed to stdout by an at_exit hook\n%!";
      eprintf "OPTIONAL harmless junk printed to stderr by an at_exit hook\n%!");
  List.init 8 (fun i ->
      Testo.create
        (sprintf "sleep %.0fms #%i" (sleep_duration *. 1000.) (i + 1))
        test_sleep)

let () = Testo.interpret_argv ~project_name:"testo_parallel_test" tests
