(*
   Test the conversion of a Testo suite to Alcotest.
*)

let t = Testo.create

let tests _env =
  [
    t "basic" (fun () -> ());
    t "xfail"
      (fun () -> failwith "this was expected")
      ~expected_outcome:(Should_fail "the test function raises an exception");
    t "capture stdout" (fun () ->
        print_string "(not captured)\n";
        let (), captured_string =
          Testo.with_capture stdout (fun () -> print_endline "hello")
        in
        Alcotest.(check string) "equal" "hello\n" captured_string);
    t "check stdout (ignored by alcotest)"
      (fun () -> print_endline "hello")
      ~checked_output:(Testo.stdout ());
  ]

let () =
  Testo.interpret_argv ~project_name:"test-alcotest" tests
    ~handle_subcommand_result:(fun exit_code _res ->
      Printf.printf "Testo exit code: %i\n" exit_code;
      Testo.to_alcotest ~alcotest_skip:Alcotest.skip (tests [])
      |> Alcotest.run "test-alcotest")
