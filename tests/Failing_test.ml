(*
   Dummy suite that fails so we can check that failing tests are reported
   nicely.
*)

let t = Testo.create

let failing_function () =
  print_endline "<something being printed by the test>";
  raise (Failure "oh no, I'm failing")

let tests =
  [
    t "failing" failing_function;
    t "failing to fail" ~expected_outcome:(Should_fail "<reasons>") (fun () ->
        print_string "<something being printed by the test>");
    t "output mismatch" ~checked_output:Stdout (fun () ->
        print_string
          {|
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
|});
    t "missing snapshot" ~checked_output:Stdout (fun () -> ());
    t "doesn't contain substring"
      ~checked_output:Stdout
      ~mask_output:[Testo.mask_not_substring "water"]
      (fun () -> print_string "fire");
  ]

let () =
  Testo.interpret_argv ~project_name:"testo_dummy_failing_tests"
    (fun () -> tests)
