(*
   Manage the storage of test statuses and test results.
   Much of this is private to this library.
*)

val default_expectation_workspace_root : Fpath.t
val default_status_workspace_root : Fpath.t

(*
   This function must be called exactly once to define where things are
   stored. It doesn't write to the file system.
*)
val init_settings :
  ?expectation_workspace_root:Fpath.t ->
  ?status_workspace_root:Fpath.t ->
  project_name:string ->
  unit ->
  unit

(*
   Create missing folders.
*)
val init_workspace : unit -> unit

(*
   All the data we need to handle the files that contain the captured output
   for a test after applying all defaults and options.
*)
type capture_paths = {
  (* Human-friendly name: "stdout", "stderr", or "stdxxx" *)
  standard_name : string;
  (* Human-friendly name: "stdout" or the basename of user-specified file
     path. *)
  short_name : string;
  (* None if this is file that holds the leftover logs that are not
     checked against expectations but directed to a file nonetheless. *)
  path_to_expected_output : Fpath.t option;
  (* Path to the file where the captured output is redirected. *)
  path_to_output : Fpath.t;
}

(*
   - For diffing output against expected output.
   - For checking uniqueness of custom storage paths.
*)
val capture_paths_of_test : Types.test -> capture_paths list

(*
   Ordinary output that's not compared against expectations.
   This is what's left of stdout and stderr after redirections.

   Return a pair (English description, data).
*)
val get_unchecked_output : Types.test -> (string * string) option

(*
   These functions are available after the call to 'init'.
*)
val get_status_workspace : unit -> Fpath.t
val get_expectation_workspace : unit -> Fpath.t
val get_status : Types.test -> Types.status

(*
   Summarize the status of a test.

   The option 'accept_missing_expected_output' is for the special case of
   new tests for which we don't have a reference output yet. The default
   is false. Note however that a user typically wants the first output of
   the test to become the reference with no questions asked.
*)
val status_summary_of_status : Types.status -> Types.status_summary

type changed = Changed | Unchanged

(*
   Replace the expected output of a test with a satisfying outcome
   (pass or xfail).

   Returns an error message if the test status is not PASS or XFAIL.
*)
val approve_new_output : Types.test -> (changed, string) Result.t

(*
   If a test is configured to normalize its output, this returns the
   suffix for the backup file holding the original captured output.
*)
val get_orig_output_suffix : Types.test -> string option

type dead_snapshot = { dir_or_junk_file : Fpath.t; test_name : string option }

(*
   Identify any file or folder in the snapshot folder that doesn't
   belong to a test in the list. The name of the test is returned alongside
   the test's folder if possible.
   Folders that contain only the test name are removed silently.
*)
val find_dead_snapshots : Types.test list -> dead_snapshot list

(**************************************************************************)
(* User-facing utilities *)
(**************************************************************************)

(* Capture stdout or stderr as a string. *)
val with_capture :
  out_channel -> (unit -> 'a Promise.t) -> ('a * string) Promise.t

(**************************************************************************)
(* Wrappers for capturing test output *)
(**************************************************************************)

val with_result_capture :
  Types.test -> (unit -> unit Promise.t) -> unit -> unit Promise.t
