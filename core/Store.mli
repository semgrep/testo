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
   Create missing base folders.
*)
val init_workspace : unit -> unit

(*
   Create the test's workspace folders:
   - status workspace: this folder holds test results, captured output
     (stdout, stderr, stashed output files)
   - snapshot workspace: this folder holds reference output (snapshots);
     it's optional.
*)
val init_test_workspace : Types.test -> unit

(*
   Std: checked output (stdout and/or stderr)
   File: checked output file
   Log: unchecked output
*)
type capture_kind = Std | File | Log

(*
   All the data we need to handle the files that contain the captured output
   for a test after applying all defaults and options.
*)
type capture_paths = {
  kind : capture_kind;
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
   Return the location of the files that store captured stdout/stderr output:
   - For diffing output against expected output.
   - For checking uniqueness of custom storage paths.
*)
val std_capture_paths_of_test : Types.test -> capture_paths list

(*
   Return the location of the files that store captured output files,
   similarly to 'std_capture_paths_of_test'.
*)
val file_capture_paths_of_test : Types.test -> capture_paths list

(* Union of std_capture_paths_of_test and file_capture_paths_of_test *)
val all_capture_paths_of_test : Types.test -> capture_paths list

(*
   Record exception and stack trace raised by a test in a file.

   If the argument is 'None', the file is deleted if it exists.
   If the argument is 'Some msg', 'msg' is saved. 'msg' must be
   a human-readable representation of the exception raised by the test.
*)
val store_exception : Types.test -> string option -> unit

(*
   Get the contents of the file containing the exception raised by a test.
   This file exists if the test failed due to an exception.
*)
val get_exception : Types.test -> string option

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
val approve_new_output : Types.test -> (changed, Testo_util.Error.msg) Result.t

(*
   If a test is configured to normalize its output, this returns the
   suffix for the backup file holding the original captured output.
   This is only for stdout/stderr checked output, not for
   checked output files.
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

(* Remove a snapshot folder *)
val remove_dead_snapshot : dead_snapshot -> unit

(* Mark a test as timed out. This is done in the master process. It should
   be done after the worker running the test was killed to ensure
   that worker doesn't try to write another status at the same time. *)
val mark_test_as_timed_out : Types.test -> unit

(* Copy a checked output file identified by its unique name (not its path) *)
val stash_output_file :
  Types.test -> Fpath.t -> Types.checked_output_file -> unit

(* Clean up stashed output files from previous runs. *)
val remove_stashed_output_files : Types.test -> unit

(**************************************************************************)
(* User-facing utilities *)
(**************************************************************************)

(* Capture stdout or stderr as a string. *)
val with_capture :
  ?binary:bool ->
  out_channel ->
  (unit -> 'a Promise.t) ->
  ('a * string) Promise.t

(**************************************************************************)
(* Wrappers for capturing test output *)
(**************************************************************************)

(* for use by Lazy_with_output *)
val with_redirect_fds :
  from:Unix.file_descr list ->
  to_:Unix.file_descr ->
  (unit -> 'a Promise.t) ->
  unit ->
  'a Promise.t

val with_result_capture :
  Types.test -> (unit -> unit Promise.t) -> unit -> unit Promise.t
