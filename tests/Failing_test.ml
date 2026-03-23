(*
   Test suite that fails on purpose so we can check that failing tests
   are reported nicely.
*)

let t = Testo.create

let failing_function () =
  print_endline "<something being printed by the test>";
  Testo.fail "oh no, I'm failing"

(*
   A custom exception whose printed form spans more than 5 lines, used to
   verify that backtrace truncation does not accidentally truncate the
   exception message itself.
*)
exception Multiline_exception

let () =
  Printexc.register_printer (function
    | Multiline_exception ->
        Some
          "Multiline_exception:\n\
           Line 2 of exception message\n\
           Line 3 of exception message\n\
           Line 4 of exception message\n\
           Line 5 of exception message\n\
           Line 6 of exception message (last line)"
    | _ -> None)

let backtrace_truncation_tag = Testo.Tag.declare "backtrace_truncation"

let tests _env =
  [
    t "failing" failing_function;
    t "multiline exception" ~tags:[ backtrace_truncation_tag ] (fun () ->
        raise Multiline_exception);
    t "failing to fail" ~expected_outcome:(Should_fail "<reasons>") (fun () ->
        print_string "<something being printed by the test>");
    t "output mismatch" ~checked_output:(Testo.stdout ()) (fun () ->
        print_string
          {|
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
|});
    t "missing snapshot" ~checked_output:(Testo.stdout ()) (fun () -> ());
    t "doesn't contain substring" ~checked_output:(Testo.stdout ())
      ~normalize:[ Testo.mask_not_substring "water" ]
      (fun () -> print_string "fire");
    t "show exception when capturing stderr" ~checked_output:(Testo.stderr ())
      (fun () -> failwith "blah");
  ]

let () = Testo.interpret_argv ~project_name:"testo_failing_tests" tests
