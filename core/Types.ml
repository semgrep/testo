(*
   Mixed bag of types used by most modules of the library, including private
   modules.

   Some of these types are exported by the main module Testo.
   Feel free to isolate types in their own modules if it makes things clearer.
*)

(*
   There are four statuses for a completed test, like in pytest:

   Should_succeed, Succeded -> PASS (good)
   Should_succeed, Failed -> FAIL (bad)
   Should_fail, Failed -> XFAIL (good)
   Should_fail, Succeeded -> XPASS (bad)

   When running a test suite, it's considered a success if all the tests
   complete with status PASS or XFAIL.
*)
type outcome = Succeeded | Failed

type captured_output =
  | Ignored of string (* unchecked combined output *)
  | Captured_stdout of string * string (* stdout, unchecked output *)
  | Captured_stderr of string * string (* stderr, unchecked output *)
  | Captured_stdout_stderr of string * string (* stdout, stderr *)
  | Captured_merged of string (* combined output *)

type expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string (* stdout, stderr *)
  | Expected_merged of string (* combined output *)

type result = { outcome : outcome; captured_output : captured_output }

type expected_outcome =
  | Should_succeed
  | Should_fail of string (* explains why we expect this test to fail *)

type missing_files = Missing_files of Fpath.t list

(*
   The expected output is optional so as to allow new tests for which
   there's no expected output yet but there's an expected outcome defined
   in the test suite.
*)
type expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, missing_files) Result.t;
}

(*
   Usually, a successful status is one where both result and expectation
   are not None and are identical. It can be useful to distinguish various
   other statuses such as: missing expectation, missing result, xpass
   (success when expected outcome was Failed), ...
*)
type status = {
  expectation : expectation;
  result : (result, missing_files) Result.t;
}

type fail_reason = Exception | Wrong_output | Exception_and_wrong_output

(* A summary of the 'status' object using the same language as pytest.

   PASS: expected success, actual success
   FAIL: expected success, actual failure
   XFAIL: expected failure, actual failure
   XPASS: expected failure, actual success
   MISS: missing data

   Maximum string length for display: 5 characters
*)
type status_class =
  | PASS
  | FAIL of fail_reason
  | XFAIL of fail_reason
  | XPASS
  | MISS

type status_summary = {
  status_class : status_class;
  has_expected_output : bool;
}

type checked_output_options = {
  (* If specified, this is where the output file will be stored instead
     of the default location. A relative path is recommended. *)
  expected_output_path : Fpath.t option;
}

type checked_output_kind =
  | Ignore_output
  | Stdout of checked_output_options
  | Stderr of checked_output_options
  | Stdxxx of checked_output_options
  | Split_stdout_stderr of checked_output_options * checked_output_options

(* public *)
type test = {
  (* The ID will be used as a compact key
     for referencing tests in filters and in file names.
     It's a hash of the internal full name. Both must be unique. *)
  id : string;
  (* 'internal_full_name' is derived from 'category' and 'name' and is not
     expected to change. It may be used for display purposes but we could
     choose to display the test name differently in the future. *)
  internal_full_name : string;
  category : string list;
  name : string;
  func : unit -> unit Promise.t;
  (* Options *)
  expected_outcome : expected_outcome;
  tags : Testo_util.Tag.t list;
  normalize : (string -> string) list;
  checked_output : checked_output_kind;
  skipped : string option;
  solo : string option;
  tolerate_chdir : bool;
}

type test_with_status = test * status * status_summary

(* TODO: move to a module that has an mli? *)
(* "path > to > name" *)
let recompute_internal_full_name (test : test) =
  String.concat " > " (test.category @ [ test.name ])

(*
   Compare the captured output that is checked and ignore the unchecked output.
*)
let equal_checked_output (a : expected_output) (b : captured_output) =
  let eq = String.equal in
  match (a, b) with
  | Ignored, Ignored _ -> true
  | Expected_stdout out, Captured_stdout (out2, _) -> eq out out2
  | Expected_stderr err, Captured_stderr (err2, _) -> eq err err2
  | Expected_merged data, Captured_merged data2 -> eq data data2
  | Expected_stdout_stderr (out, err), Captured_stdout_stderr (out2, err2) ->
      eq out out2 && eq err err2
  | _ -> false
