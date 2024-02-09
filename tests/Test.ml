(*
   Dummy suite exercising a variety of test options.
*)

(* We should consider a shorter name for this library. *)
let t = Testo.create
let testing_tag = Testo.Tag.declare "testing"
let tags_tag = Testo.Tag.declare "tags"

let fruit_tests =
  Testo.categorize "fruit" [
    t "apple" (fun () -> ());
    t "kiwi" (fun () -> ());
  ]

let animal_tests =
  Testo.categorize "animal" [ t "banana slug" (fun () -> ()) ]

let categorized =
  Testo.categorize_suites "biohazard" [fruit_tests; animal_tests]

let tests =
  [
    t "simple" (fun () -> ());
    t "tags" ~tags:[ testing_tag; tags_tag ] (fun () -> ());
    t "category" ~category:[ "category"; "subcategory" ] (fun () -> ());
    t "unchecked stdout" (fun () -> print_endline "hello\nworld");
    t "unchecked stderr" (fun () -> prerr_string "hello\n");
    t "capture stdout" ~checked_output:Stdout (fun () -> print_string "hello\n");
    t "capture stderr" ~checked_output:Stderr (fun () -> prerr_string "error\n");
    t "capture stdxxx" ~checked_output:Merged_stdout_stderr (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr" ~checked_output:Separate_stdout_stderr
      (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "xfail" ~expected_outcome:(Should_fail "raises exception on purpose")
      (fun () -> failwith "this exception is expected");
    t "skipped" ~skipped:true (fun () -> failwith "this shouldn't happen");
    t "chdir" ~tolerate_chdir:true (fun () -> Sys.chdir "/");
    t ~checked_output:Stdout ~mask_output:[ String.lowercase_ascii ] "masked"
      (fun () -> print_endline "HELLO");
  ] @ categorized @ Real_unit_tests.tests

let () =
  Testo.interpret_argv ~project_name:"testo_dummy_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      (* nosemgrep: forbid-exit *)
      exit exit_code)
    (fun () -> tests)
