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
let shell_command ?(expected_exit_code = 0) ~loc command =
  printf "RUN %s\n%!" command;
  let exit_code = Sys.command command in
  if exit_code <> expected_exit_code then
    failwith (
      sprintf "%s:\nCommand '%s' exited with code %i but code %i was expected."
        loc command exit_code expected_exit_code
    )

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
let test_subcommand ?expected_exit_code ~__LOC__:loc shell_command_string =
  let command = "./test " ^ shell_command_string in
  shell_command ?expected_exit_code ~loc command

let clear_status ~__LOC__:loc () =
  shell_command ~loc "rm -rf _build/testo/status/testo_tests"

let clear_snapshots ~__LOC__:loc () =
  shell_command ~loc "rm -rf tests/snapshots/testo_tests"

let test_standard_flow () =
  section "Clean start";
  clear_status ~__LOC__ ();
  clear_snapshots ~__LOC__ ();
  test_subcommand ~__LOC__ "status" ~expected_exit_code:1;
  test_subcommand ~__LOC__ "run" ~expected_exit_code:1;
  test_subcommand ~__LOC__ "status" ~expected_exit_code:1;
  test_subcommand ~__LOC__ "status --short" ~expected_exit_code:1;
  test_subcommand ~__LOC__ "approve";
  test_subcommand ~__LOC__ "status";
  section "Delete statuses but not snapshots";
  clear_status ~__LOC__ ();
  test_subcommand ~__LOC__ "status" ~expected_exit_code:1;
  test_subcommand ~__LOC__ "run";
  section "Delete snapshots but not statuses";
  clear_snapshots ~__LOC__ ();
  test_subcommand ~__LOC__ "status" ~expected_exit_code:1;
  test_subcommand ~__LOC__ "approve"

(*****************************************************************************)
(* Exercise the failing test suite *)
(*****************************************************************************)

let failing_test_subcommand ~loc shell_command_string =
  let command = "./failing-test " ^ shell_command_string in
  shell_command ~loc command

let test_failing_flow_run () = failing_test_subcommand ~loc:__LOC__ "run"
let test_failing_flow_status () = failing_test_subcommand ~loc:__LOC__ "status"

(*****************************************************************************)
(* Meta test suite *)
(*****************************************************************************)

let delete pat = T.mask_pcre_pattern ~mask:"" pat

let mask_alcotest_output =
  [
    T.mask_line ~mask:"<MASKED RUN ID>" ~after:"This run has ID `" ~before:"'"
      ();
    T.mask_pcre_pattern ~mask:"<MASKED DURATION>" {|in [0-9]+\.[0-9]+s|};
    T.mask_line ~after:"Called from " ();
    T.mask_line ~after:"Re-raised at " ();
    T.mask_line ~after:"Logs saved to " ();
    T.mask_line ~after:"Full test results in " ();
    (* These extra markers show up in the Alcotest output in GitHub Actions.
       There may be a better way to disable them but this will have to do for
       now. *)
    delete (Re.Pcre.quote "::group::{test}\n");
    delete (Re.Pcre.quote "::endgroup::\n");
  ]

let tests =
  [
    t ~checked_output:Merged_stdout_stderr ~normalize:mask_alcotest_output
      "standard flow" test_standard_flow;
    t "failing flow run"
      ~expected_outcome:
        (Should_fail "the invoked test suite is designed to fail")
      test_failing_flow_run;
    t "failing flow status"
      ~expected_outcome:
        (Should_fail "the invoked test suite is designed to fail")
      test_failing_flow_status;
    t "output masking for failing tests"
      ~expected_outcome:(Should_fail "expected to fail")
      ~checked_output:Stdout
      ~normalize:[ (fun data ->
        if data = "not masked" then "<SUCCESSFULLY MASKED>" else data)
      ]
      (fun () ->
         print_string "not masked";
         failwith "this exception is expected");
  ]

let () =
  Testo.interpret_argv ~project_name:"testo_meta_tests" (fun () ->
      tests)
