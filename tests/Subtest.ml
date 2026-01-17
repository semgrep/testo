(*
   Test suite exercising a variety of test options, invoked by meta-test
*)

open Printf

let ( !! ) = Fpath.to_string
let ( / ) = Fpath.( / )
let t = Testo.create
let testing_tag = Testo.Tag.declare "testing"
let tags_tag = Testo.Tag.declare "tags"

let test_internal_files =
  let category = [ "auto-approve"; "internal files" ] in
  let test_create_name_file =
    t "create name file" ~checked_output:(Testo.stdout ()) ~category (fun () ->
        ())
  in
  let test_dont_create_name_file =
    t "don't create name file" ~category (fun () -> ())
  in
  let snapshot_dir_path (test : Testo.t) =
    Fpath.v "tests/snapshots/testo_subtests" / test.id
  in
  let test_check_name_files =
    (* This test depends on previous tests. Don't try this at home. *)
    t "check for name file in previous tests" ~category (fun () ->
        let name_file_path = snapshot_dir_path test_create_name_file / "name" in
        if not (Sys.file_exists !!name_file_path) then
          Testo.fail ("Missing file: " ^ !!name_file_path);
        let missing_dir_path = snapshot_dir_path test_dont_create_name_file in
        if Sys.file_exists !!missing_dir_path then
          Testo.fail ("File should not exist: " ^ !!missing_dir_path))
  in
  (* Don't recategorize these tests as it changes their ID and would break
     the last test. *)
  [
    test_create_name_file;
    test_dont_create_name_file;
    (* must run after the two tests above *)
    test_check_name_files;
  ]

let tests env =
  print_endline "junk printed on stdout...\n... when creating the test suite";
  test_internal_files
  @ [
      t "tags" ~tags:[ testing_tag; tags_tag ] (fun () -> ());
      t "capture stdout" ~category:[ "auto-approve" ]
        ~checked_output:(Testo.stdout ()) (fun () -> print_string "hello\n");
      t "capture stderr" ~category:[ "auto-approve" ]
        ~checked_output:(Testo.stderr ()) (fun () -> prerr_string "error\n");
      t "capture stdxxx" ~category:[ "auto-approve" ]
        ~checked_output:(Testo.stdxxx ()) (fun () ->
          print_string "hello\n";
          flush stdout;
          prerr_string "error\n";
          flush stderr;
          print_string "goodbye\n");
      t "capture stdout and stderr" ~category:[ "auto-approve" ]
        ~checked_output:(Testo.split_stdout_stderr ()) (fun () ->
          print_string "hello\n";
          prerr_string "error\n");
      t "capture stdout in custom location" ~category:[ "auto-approve" ]
        ~checked_output:
          (Testo.stdout
             ~expected_stdout_path:(Fpath.v "tests/custom-snapshots/my-stdout")
             ())
        (fun () -> print_string "hello\n");
      t "capture stderr in custom location" ~category:[ "auto-approve" ]
        ~checked_output:
          (Testo.stderr
             ~expected_stderr_path:(Fpath.v "tests/custom-snapshots/my-stderr")
             ())
        (fun () -> prerr_string "error\n");
      t "capture stdxxx in custom location" ~category:[ "auto-approve" ]
        ~checked_output:
          (Testo.stdxxx
             ~expected_stdxxx_path:(Fpath.v "tests/custom-snapshots/my-stdxxx")
             ())
        (fun () ->
          print_string "hello\n";
          flush stdout;
          prerr_string "error\n";
          flush stderr;
          print_string "goodbye\n");
      t "capture stdout and stderr in custom location"
        ~category:[ "auto-approve" ]
        ~checked_output:
          (Testo.split_stdout_stderr
             ~expected_stdout_path:
               (Fpath.v "tests/custom-snapshots/split-stdout")
             ~expected_stderr_path:
               (Fpath.v "tests/custom-snapshots/split-stderr")
             ())
        (fun () ->
          print_string "hello\n";
          prerr_string "error\n");
      t "flaky" ~flaky:"this test is super flaky" (fun () ->
          Testo.fail "I am flaky");
      t "require '--env foo=bar'" (fun () ->
          match List.assoc_opt "foo" env with
          | None -> Testo.fail "Missing option: -e foo=bar"
          | Some "bar" -> ()
          | Some other ->
              Testo.fail (sprintf "Invalid value for variable foo: %S" other));
      t "xfail due to invalid output"
        ~expected_outcome:(Should_fail "produces incorrect output on purpose")
        ~checked_output:(Testo.stdout ()) (fun () ->
          print_endline
            {|This is incorrect output printed by the test on purpose.

If you approve this test's output by accident:
1. Put back the string "correct output" into the snapshot path printed
   by the test e.g. 'tests/snapshots/testo_subtests/05dd9a9f220b/stdout'
2. Run git-commit before running the meta-test again.
|});
      t "capture multiple files and stdout"
        ~normalize:[ (fun s -> "[normalized] " ^ s) ]
        ~checked_output:(Testo.stdout ())
        ~checked_output_files:
          [
            Testo.checked_output_file "results.txt";
            Testo.checked_output_file "results.json";
          ]
        (fun () ->
          Testo.with_temp_dir ~chdir:true (fun _dir ->
              print_endline "this is a message on stdout";
              let txt_path = Fpath.v "results.txt" in
              Testo.write_text_file txt_path "hello world\n";
              Testo.stash_output_file txt_path "results.txt";
              let json_path = Fpath.v "results.json" in
              Testo.write_text_file json_path "{}\n";
              Testo.stash_output_file json_path "results.json"));
      t "inline logs" ~inline_logs:On (fun () ->
          print_endline "Hello. This is a log.");
      t "auto inline logs" (fun () -> print_endline "Hello. This is a log.");
      t "no inline logs" ~inline_logs:Off (fun () ->
          print_endline "Hello. This is a log.");
      t
        "environment-sensitive"
        (* We use an environment variable to make the output of the test
           change without editing the code for the test. This allows us to
           check that Testo's diff output looks right. *)
        ~checked_output:(Testo.stdout ()) (fun () ->
          printf "Checking if the environment variable TESTO_TEST is set:\n";
          match Unix.getenv "TESTO_TEST" with
          | ""
          | (exception Not_found) ->
              printf "TESTO_TEST is empty or unset.\n"
          | str -> printf "TESTO_TEST is set to %S.\n" str);
    ]

let () =
  Testo.interpret_argv ~project_name:"testo_subtests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      exit exit_code)
    tests
