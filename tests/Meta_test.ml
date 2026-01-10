(*
   Test suite that runs the dummy test program and checks that its output
   is what we expect.

   Warning: ./meta-test produces very confusing output!
   I recommend running ./test and ./failed-test manually and see what's wrong
   there.
*)

open Printf
module T = Testo

let t = T.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   Invoke an arbitrary shell command.
*)
let shell_command ?(expected_exit_code = 0) ~__LOC__:loc command =
  printf "RUN %s\n%!" command;
  let bash_command = sprintf "bash -c \"%s\"" command in
  let exit_code = Sys.command bash_command in
  if exit_code <> expected_exit_code then
    failwith
      (sprintf "%s:\nCommand '%s' exited with code %i but code %i was expected."
         loc command exit_code expected_exit_code)

let setenv k opt_value =
  match opt_value with
  | None ->
      (* uh, how do we really unset an environment variable? *)
      Unix.putenv k ""
  | Some v -> Unix.putenv k v

let with_env (k, tmp_value) func =
  let old_value =
    try Some (Unix.getenv k) with
    | Not_found -> None
  in
  setenv k tmp_value;
  Fun.protect func ~finally:(fun () -> setenv k old_value)

let section text =
  printf
    {|#####################################################################
# %s
#####################################################################
%!|}
    text

(*****************************************************************************)
(* Exercise the regular test suite *)
(*****************************************************************************)

(*
   Invoke the pre-build test program.
*)
let test_subcommand ?expected_exit_code ~__LOC__:loc subcommand_name
    shell_command_args =
  let command =
    sprintf "./test %s -e foo=bar %s" subcommand_name shell_command_args
  in
  shell_command ?expected_exit_code ~__LOC__:loc command

(*
   0 workers: no workers are created
   1 worker: one worker is created but the output is still deterministic
   more than one worker: the output won't be in a particular order, making it
   harder to test expectations.
*)
let test_run ?expected_exit_code ?(num_workers = 1) ~__LOC__:loc
    shell_command_args =
  test_subcommand ?expected_exit_code ~__LOC__:loc "run"
    (sprintf "-j %d %s" num_workers shell_command_args)

let test_status ?expected_exit_code ~__LOC__:loc shell_command_args =
  test_subcommand ?expected_exit_code ~__LOC__:loc "status" shell_command_args

let test_approve ?expected_exit_code ~__LOC__:loc shell_command_args =
  test_subcommand ?expected_exit_code ~__LOC__:loc "approve" shell_command_args

let clear_status ~__LOC__:loc () =
  shell_command ~__LOC__:loc "rm -rf _build/testo/status/testo_tests"

(*
   These are the tests we want to approve/disapprove in the
   meta tests below and have their snapshots (files holding the expected
   output) in the default location.
   This is fragile but we don't have a better way right now.
*)
let auto_approve_tests =
  [
    "9c96a5aa8b4b";
    "0048917873df";
    "17ec149855c2";
    "02ac0ea4ae90";
    (* auto-approve > internal files > create name file *)
    "f66d12950c64";
    (* auto-approve > internal files > don't create name file *)
    "caadabfd495c";
  ]

(* Delete snapshots for the tests tagged with "auto-approve" *)
let clear_snapshots ~__LOC__:loc () =
  (* snapshots at the default location *)
  auto_approve_tests
  |> List.iter (fun id ->
         shell_command ~__LOC__:loc ("rm -rf tests/snapshots/testo_tests/" ^ id));
  (* snapshots at a custom location *)
  shell_command ~__LOC__:loc "mkdir -p tests/custom-snapshots";
  shell_command ~__LOC__:loc "rm -f tests/custom-snapshots/*"

(* FIXME: This integration test fails on Windows because the expected output
   includes paths generated on POSIX systems. It runs correctly however. *)
(* TODO: split this test into smaller tests because right now the changes
   are so frequent and so big that unwanted changes may be accidentally
   approved. *)
let test_standard_flow () =
  Fun.protect
    (fun () ->
      section "Clean start";
      clear_status ~__LOC__ ();
      clear_snapshots ~__LOC__ ();
      test_status ~__LOC__ "" ~expected_exit_code:1;
      test_run ~__LOC__ "" ~expected_exit_code:1;
      test_status ~__LOC__ "--all --long" ~expected_exit_code:1;
      test_status ~__LOC__ "" ~expected_exit_code:1;
      test_approve ~__LOC__ "-s auto-approve";
      test_approve ~__LOC__ "-s environment-sensitive";
      test_status ~__LOC__ "";
      test_status ~__LOC__ "--expert";
      test_status ~__LOC__ "--env A_b123=xxx";
      test_status ~__LOC__ "-e novalue" ~expected_exit_code:124;
      test_status ~__LOC__ "-e b@d_key=42" ~expected_exit_code:124;
      test_status ~__LOC__ "-a -t testin" ~expected_exit_code:124;
      test_status ~__LOC__ "-a -t testing";
      test_status ~__LOC__ "--strict" ~expected_exit_code:1;
      (* Modify the output of the test named 'environment-sensitive'
       by setting an environment variable it consults, simulating a bug *)
      with_env ("TESTO_TEST", Some "hello") (fun () ->
          test_run ~__LOC__ "-s environment-sensitive" ~expected_exit_code:1);
      (* "Fix the bug" in test 'environment-sensitive' *)
      test_run ~__LOC__ "-s environment-sensitive";
      section "Delete statuses but not snapshots";
      clear_status ~__LOC__ ();
      test_status ~__LOC__ "-v" ~expected_exit_code:1;
      test_status ~__LOC__ "-l" ~expected_exit_code:1;
      test_status ~__LOC__ "-a" ~expected_exit_code:1;
      test_run ~__LOC__ "";
      section "Delete snapshots but not statuses";
      clear_snapshots ~__LOC__ ();
      test_status ~__LOC__ "" ~expected_exit_code:1;
      test_approve ~__LOC__ "-s auto-approve";
      section "Delete the dead snapshots with --autoclean";
      test_status ~__LOC__ "-l --autoclean";
      section "Check that the dead snapshots are gone";
      test_status ~__LOC__ "-l")
    ~finally:(fun () ->
      section "Restore any deleted snapshots";
      shell_command ~__LOC__ "git restore tests/snapshots/testo_tests")

let test_multi_selection () =
  let (), capture =
    Testo.with_capture stdout (fun () ->
        (* Select two tests that have different names. We could pick
         any two tests for this. *)
        shell_command ~__LOC__
          "./test status -a -s 'unchecked stdout' -s 'unchecked stderr'")
  in
  assert (Testo_util.Helpers.contains_substring capture ~sub:"unchecked stdout");
  assert (Testo_util.Helpers.contains_substring capture ~sub:"unchecked stderr")

(*
   Invalid output -> XFAIL
   Approved output -> XPASS
*)
let test_approve_xfail () =
  section "Approve the incorrect output of an XFAIL test";
  test_approve ~__LOC__ "-s '05dd9a9f220b'";
  section "Now expect XPASS status";
  test_status ~__LOC__ "-s '05dd9a9f220b'" ~expected_exit_code:1;
  section "Clean up";
  shell_command ~__LOC__
    "git restore tests/snapshots/testo_tests/05dd9a9f220b/stdout"

(*
   Delete test snapshots to simulate a fresh test then check that approving
   output files works.
*)
let test_new_output_file_capture () =
  Fun.protect
    (fun () ->
      let test_name = "capture multiple files and stdout" in
      let test_id = "0658581e95c7" in
      section (sprintf "Delete snapshots for test %s '%s'" test_id test_name);
      shell_command ~__LOC__
        (sprintf "rm -rf tests/snapshots/testo_tests/%s" test_id);
      section "Run test without snapshots";
      test_run ~__LOC__ ~expected_exit_code:1 (sprintf "-s %s" test_id);
      section "Approve captured output";
      test_approve ~__LOC__ (sprintf "-s %s" test_id))
    ~finally:(fun () ->
      section "Restore any deleted snapshots";
      shell_command ~__LOC__ "git restore tests/snapshots/testo_tests")

let test_updated_output_file_capture () =
  Fun.protect
    (fun () ->
      let test_name = "capture multiple files and stdout" in
      let test_id = "0658581e95c7" in
      section (sprintf "Edit snapshot for test %s '%s'" test_id test_name);
      shell_command ~__LOC__
        (sprintf
           "echo outdated_contents >> \
            tests/snapshots/testo_tests/%s/file-results.txt"
           test_id);
      section "Run test with updated output in results.txt";
      test_run ~__LOC__ ~expected_exit_code:1 (sprintf "-s %s" test_id);
      section "Approve updated captured output";
      test_approve ~__LOC__ (sprintf "-s %s" test_id))
    ~finally:(fun () ->
      section "Restore any modified snapshot";
      shell_command ~__LOC__ "git restore tests/snapshots/testo_tests")

let test_max_inline_log_size ~limit () =
  test_run ~__LOC__ (sprintf "-s 'inline logs' --max-inline-log-bytes %s" limit)

(*****************************************************************************)
(* Exercise the parallel test suite *)
(*****************************************************************************)

let parallel_test_subcommand ~__LOC__:loc shell_command_string =
  let command = "./parallel-test " ^ shell_command_string in
  shell_command ~__LOC__:loc command

let test_fewer_workers_than_tests () =
  parallel_test_subcommand ~__LOC__ "run -j4"

let test_more_workers_than_tests () =
  parallel_test_subcommand ~__LOC__ "run -j100"

(*****************************************************************************)
(* Exercise the failing test suite *)
(*****************************************************************************)

let failing_test_subcommand ~loc shell_command_string =
  let command = "./failing-test " ^ shell_command_string in
  shell_command ~__LOC__:loc command

let test_failing_flow_run () = failing_test_subcommand ~loc:__LOC__ "run"
let test_failing_flow_status () = failing_test_subcommand ~loc:__LOC__ "status"

(*****************************************************************************)
(* Exercise the test suite with timeouts *)
(*****************************************************************************)

let timeout_test_subcommand ~loc shell_command_string =
  let command = "./timeout-test " ^ shell_command_string in
  shell_command ~__LOC__:loc ~expected_exit_code:1 command

(* -j1 = sequential run that supports timeouts *)
let test_timeout_flow_run () = timeout_test_subcommand ~loc:__LOC__ "run -j1"

let test_timeout_flow_status () =
  timeout_test_subcommand ~loc:__LOC__ "status -al"

(*****************************************************************************)
(* Meta test suite *)
(*****************************************************************************)

let delete pat = T.mask_pcre_pattern ~replace:(fun _ -> "") pat

(* Different versions of OCaml print stack traces differently:
   4.08:
     Raised at file "stdlib.ml", line 29, characters 17-33
   4.14:
     Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
*)
let mask_stack_backtrace =
  Testo.mask_line ~after:"Raised at " ~before:", line" ()

let mask_alcotest_output =
  [
    T.mask_line ~mask:"<MASKED RUN ID>" ~after:"This run has ID `" ~before:"'"
      ();
    T.mask_pcre_pattern
      ~replace:(fun _ -> "<MASKED DURATION>")
      {|in [0-9]+\.[0-9]+s|};
    T.mask_line ~after:"Called from " ();
    T.mask_line ~after:"Re-raised at " ();
    T.mask_line ~after:"Logs saved to " ();
    T.mask_line ~after:"Full test results in " ();
    (* These extra markers show up in the Alcotest output in GitHub Actions.
       There may be a better way to disable them but this will have to do for
       now. *)
    delete (Re.Pcre.quote "::group::{test}\n");
    delete (Re.Pcre.quote "::endgroup::\n");
    mask_stack_backtrace;
    (* Replace all backslashes with forward slashes to normalize Windows paths.
       This may replace more backslashes than we want. *)
    T.mask_pcre_pattern ~replace:(fun _backslash -> "/") {|\\|};
  ]

let sort_lines str =
  str |> String.split_on_char '\n' |> List.sort String.compare
  |> String.concat "\n"

(* Remove lines containing "OPTIONAL" *)
let remove_optional_lines =
  Testo.remove_matching_lines (Testo.contains_substring ~sub:"OPTIONAL")

let mask_and_sort = mask_alcotest_output @ [ sort_lines; remove_optional_lines ]

(* FIXME: Running parallel jobs on Windows is unstable, producing
   Sys_error("Invalid argument")` during some attempts to flush channels.
   Perhaps related to https://github.com/ocaml/ocaml/issues/13586 *)
let skipped =
  if Sys.win32 then Some "Running tests in parallel is unstable on Windows"
  else None

let tests =
  [
    t ~checked_output:(T.stdxxx ()) ~normalize:mask_alcotest_output
      "standard flow" test_standard_flow;
    t ~checked_output:(T.stdxxx ()) ~normalize:mask_and_sort ?skipped
      "fewer workers than tests" test_fewer_workers_than_tests;
    t ~checked_output:(T.stdxxx ()) ~normalize:mask_and_sort ?skipped
      "more workers than tests" test_more_workers_than_tests;
    t "failing flow run"
      ~expected_outcome:
        (Should_fail "the invoked test suite is designed to fail")
      test_failing_flow_run;
    t "failing flow status"
      ~expected_outcome:
        (Should_fail "the invoked test suite is designed to fail")
      test_failing_flow_status;
    t "timeout flow run" test_timeout_flow_run;
    t "timeout flow status" ~checked_output:(T.stdxxx ())
      test_timeout_flow_status;
    t "output masking for failing tests"
      ~expected_outcome:(Should_fail "expected to fail")
      ~checked_output:(T.stdout ())
      ~normalize:
        [
          (fun data ->
            if data = "not masked" then "<SUCCESSFULLY MASKED>" else data);
        ]
      (fun () ->
        print_string "not masked";
        failwith "this exception is expected");
    t "approve xfail" ~checked_output:(T.stdxxx ()) test_approve_xfail;
    t "multi selection" test_multi_selection;
    t "new output file capture" test_new_output_file_capture;
    t "updated output file capture" test_updated_output_file_capture;
    t "max inline log size" ~checked_output:(T.stdout ())
      (test_max_inline_log_size ~limit:"5");
    t "no max inline log size" ~checked_output:(T.stdout ())
      (test_max_inline_log_size ~limit:"unlimited");
  ]

let () =
  (* stdout and stderr are in "text mode" by default, and on windows this
     entails rewriting line endings to CLRF. This makes the test output
     incompatible between Windows and POSIX platforms. Setting the channels to
     binary mode ensures consistent output. *)
  set_binary_mode_out stdout true;
  set_binary_mode_out stderr true;
  (* We have a few tests that use the same workspace. To avoid conflicts,
     we run them sequentially. *)
  Testo.interpret_argv ~default_workers:(Some 0)
    ~project_name:"testo_meta_tests" (fun _env -> tests)
