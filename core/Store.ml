(*
   Manage the storage of test statuses and test results.

   We store the following:

   - result for the last run of each test: in a hidden folder
   - captured output for the last run of each test: in a hidden folder
   - expected output for each test: in a persistent folder

   We distinguish two levels of "statuses":

   - test result: the test result before comparing it to expectations:
     * Did it return or raise an exception?
     * What output did we capture?
   - test status: the test result confronted to our expectations:
     * Did the test run at all?
     * Does the test result match our expectations?
*)

open Printf
open Testo_util
open Fpath_.Operators (* // / !! *)
open Promise.Operators (* >>= *)
module T = Types
module P = Promise

(**************************************************************************)
(* Helpers *)
(**************************************************************************)
(*
   Some of these helpers are provided by nice third-party libraries but we're
   not using them to minimize dependencies, this being a test framework
   that all library authors should be able to use.
*)

(* All the data we need to handle the files that contain the captured output
   for a test after applying all defaults and options. *)
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

let list_map f xs = List.rev_map f xs |> List.rev

let list_result_of_result_list (xs : ('a, 'b) Result.t list) :
    ('a list, 'b list) Result.t =
  let oks, errs =
    List.fold_right
      (fun res (oks, errs) ->
        match res with
        | Ok x -> (x :: oks, errs)
        | Error x -> (oks, x :: errs))
      xs ([], [])
  in
  match errs with
  | [] -> Ok oks
  | errs -> Error errs

let with_file_in path f =
  if Sys.file_exists !!path then
    let ic = open_in_bin !!path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Ok (f ic))
  else Error path

let read_file path : (string, Fpath.t (* missing file *)) Result.t =
  with_file_in path (fun ic -> really_input_string ic (in_channel_length ic))

let errmsg_of_missing_file (path : Fpath.t) : string =
  sprintf "Missing or inaccessible file %s" !!path

let read_file_exn path : string =
  match read_file path with
  | Ok data -> data
  | Error path -> Error.user_error (errmsg_of_missing_file path)

let with_file_out path f =
  let oc = open_out_bin !!path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let write_file path data = with_file_out path (fun oc -> output_string oc data)
let remove_file path = if Sys.file_exists !!path then Sys.remove !!path

(**************************************************************************)
(* Global settings *)
(**************************************************************************)

(*
   The status workspace is a temporary folder outside of version control.
*)
let default_status_workspace_root = Fpath.v "_build" / "testo" / "status"

(*
   The expectation workspace is under version control.
*)
let default_expectation_workspace_root = Fpath.v "tests" / "snapshots"

let not_initialized () =
  Error.user_error
    "The Testo workspace was not initialized properly or at all. This is \
     probably a bug in Testo."

let already_initialized () =
  Error.user_error
    "Internal error in Testo: there was an attempt to initialize the workspace \
     more than once."

let make_late_init () =
  let var = ref None in
  let get () =
    match !var with
    | None -> not_initialized ()
    | Some x -> x
  in
  let set value =
    match !var with
    | Some _ -> already_initialized ()
    | None -> var := Some value
  in
  (get, set)

let get_status_workspace, set_status_workspace = make_late_init ()
let get_expectation_workspace, set_expectation_workspace = make_late_init ()

let init_settings
    ?(expectation_workspace_root = default_expectation_workspace_root)
    ?(status_workspace_root = default_status_workspace_root) ~project_name () =
  if status_workspace_root = expectation_workspace_root then
    Error.user_error
      (sprintf
         {|status_workspace and expectation_workspace must be different folders
but they are both set to the following path:
  %s|}
         !!status_workspace_root);
  set_status_workspace (status_workspace_root / project_name);
  set_expectation_workspace (expectation_workspace_root / project_name)

let init_workspace () =
  Helpers.make_dir_if_not_exists ~recursive:true (get_status_workspace ());
  Helpers.make_dir_if_not_exists ~recursive:true (get_expectation_workspace ())

let get_test_status_workspace (test : T.test) =
  get_status_workspace () / test.id

let get_test_expectation_workspace (test : T.test) =
  get_expectation_workspace () / test.id

let name_file_name = "name"
let get_name_file_path_from_dir dir = dir / name_file_name

let get_name_file_path (test : T.test) =
  get_name_file_path_from_dir (get_test_expectation_workspace test)

(* This is for reviewing snapshot folders that are no longer associated
   with any test because their ID changed or they were removed from the
   test suite. *)
let write_name_file (test : T.test) =
  let contents = test.internal_full_name ^ "\n" in
  Helpers.write_file (get_name_file_path test) contents

let must_create_expectation_workspace_for_test (test : T.test) =
  let uses_internal_storage (x : T.checked_output_options) =
    match x.expected_output_path with
    | None -> true
    | Some _user_provided_path -> false
  in
  match test.checked_output with
  | Ignore_output -> false
  | Stdout options
  | Stderr options
  | Stdxxx options ->
      uses_internal_storage options
  | Split_stdout_stderr (options1, options2) ->
      uses_internal_storage options1 || uses_internal_storage options2

let init_expectation_workspace test =
  (* Don't create a folder and a 'name' file if no snapshots are going to
     be stored there. *)
  if must_create_expectation_workspace_for_test test then (
    Helpers.make_dir_if_not_exists (get_test_expectation_workspace test);
    write_name_file test)

let init_test_workspace test =
  Helpers.make_dir_if_not_exists (get_test_status_workspace test);
  init_expectation_workspace test

(**************************************************************************)
(* Read/write data *)
(**************************************************************************)

let corrupted_file path =
  Error.user_error
    (sprintf
       "Uh oh, the test framework ran into a corrupted file: %S\n\
        Remove it and retry." !!path)

let get_outcome_path (test : T.test) =
  get_test_status_workspace test / "outcome"

let string_of_outcome (outcome : T.outcome) =
  match outcome with
  | Succeeded -> "Succeeded"
  | Failed -> "Failed"

let outcome_of_string path data : T.outcome =
  match data with
  | "Succeeded" -> Succeeded
  | "Failed" -> Failed
  | _ -> corrupted_file path

let set_outcome (test : T.test) outcome =
  let path = get_outcome_path test in
  outcome |> string_of_outcome |> Helpers.write_file path

let get_outcome (test : T.test) :
    (T.outcome, Fpath.t (* missing file *)) Result.t =
  let path = get_outcome_path test in
  match read_file path with
  | Ok data -> Ok (outcome_of_string path data)
  | Error path -> Error path

(* File names used to the test output, possibly after masking the variable
   parts. *)
let stdout_filename = "stdout"
let stderr_filename = "stderr"
let stdxxx_filename = "stdxxx"
let unchecked_filename = "log"

(* stdout.orig, stderr.orig, etc. obtained after masking the variable parts
   of the test output as specified by the option 'mask_output' function. *)
let orig_suffix = ".orig"

let get_orig_output_suffix (test : T.test) =
  match test.normalize with
  | [] -> None
  | _ -> Some orig_suffix

let get_expected_output_path (test : T.test) default_name
    (options : T.checked_output_options) =
  match options.expected_output_path with
  | None -> get_expectation_workspace () / test.id / default_name
  | Some path -> path

let short_name_of_checked_output_options default_name
    (options : T.checked_output_options) =
  match options.expected_output_path with
  | None -> default_name
  | Some path -> Fpath.basename path

let get_output_path (test : T.test) filename =
  get_status_workspace () / test.id / filename

let get_exception_path (test : T.test) = get_output_path test "exception"

let store_exception (test : T.test) opt_msg =
  let path = get_exception_path test in
  match opt_msg with
  | None -> remove_file path
  | Some msg -> write_file path msg

let get_exception (test : T.test) =
  let path = get_exception_path test in
  match read_file path with
  | Ok data -> Some data
  | Error _path -> None

(*
   Derive the various file paths related to a given test, but excluding
   unchecked output (logs).
*)
let capture_paths_of_test (test : T.test) : capture_paths list =
  let unchecked_paths =
    {
      standard_name = unchecked_filename;
      short_name = unchecked_filename;
      path_to_expected_output = None;
      path_to_output = get_output_path test unchecked_filename;
    }
  in
  match test.checked_output with
  | Ignore_output -> [ unchecked_paths ]
  | Stdout options ->
      [
        {
          standard_name = stdout_filename;
          short_name =
            short_name_of_checked_output_options stdout_filename options;
          path_to_expected_output =
            Some (get_expected_output_path test stdout_filename options);
          path_to_output = get_output_path test stdout_filename;
        };
        unchecked_paths;
      ]
  | Stderr options ->
      [
        {
          standard_name = stderr_filename;
          short_name =
            short_name_of_checked_output_options stderr_filename options;
          path_to_expected_output =
            Some (get_expected_output_path test stderr_filename options);
          path_to_output = get_output_path test stderr_filename;
        };
        unchecked_paths;
      ]
  | Stdxxx options ->
      [
        {
          standard_name = stdxxx_filename;
          short_name =
            short_name_of_checked_output_options stdxxx_filename options;
          path_to_expected_output =
            Some (get_expected_output_path test stdxxx_filename options);
          path_to_output = get_output_path test stdxxx_filename;
        };
      ]
  | Split_stdout_stderr (stdout_options, stderr_options) ->
      [
        {
          standard_name = stdout_filename;
          short_name =
            short_name_of_checked_output_options stdout_filename stdout_options;
          path_to_expected_output =
            Some (get_expected_output_path test stdout_filename stdout_options);
          path_to_output = get_output_path test stdout_filename;
        };
        {
          standard_name = stderr_filename;
          short_name =
            short_name_of_checked_output_options stderr_filename stderr_options;
          path_to_expected_output =
            Some (get_expected_output_path test stderr_filename stderr_options);
          path_to_output = get_output_path test stderr_filename;
        };
      ]

let describe_unchecked_output (output : T.checked_output_kind) : string option =
  match output with
  | Ignore_output -> Some "stdout, stderr"
  | Stdout _ -> Some "stderr"
  | Stderr _ -> Some "stdout"
  | Stdxxx _ -> None
  | Split_stdout_stderr _ -> None

(* paths to freshly captured output, both checked and unchecked. *)
let get_output_paths (paths : capture_paths list) =
  paths |> list_map (fun x -> x.path_to_output)

(* paths to freshly captured output, excluding unchecked output (logs). *)
let get_checked_output_paths (paths : capture_paths list) =
  paths
  |> List.filter (fun x -> x.path_to_expected_output <> None)
  |> get_output_paths

let get_unchecked_output_path (test : T.test) =
  get_output_path test unchecked_filename

let get_output (paths : capture_paths list) =
  paths |> get_output_paths |> list_map read_file

let get_checked_output (paths : capture_paths list) =
  paths |> get_checked_output_paths |> list_map read_file

let get_unchecked_output (test : T.test) =
  match describe_unchecked_output test.checked_output with
  | Some log_description -> (
      let path = get_unchecked_output_path test in
      match read_file path with
      | Ok data -> Some (log_description, data)
      | Error _cant_read_file -> None)
  | None -> None

let get_expected_output_paths (paths : capture_paths list) =
  paths |> List.filter_map (fun x -> x.path_to_expected_output)

let get_expected_output (paths : capture_paths list) =
  paths |> get_expected_output_paths |> list_map read_file

let set_expected_output (test : T.test) (capture_paths : capture_paths list)
    (data : string list) =
  let paths = capture_paths |> get_expected_output_paths in
  if List.length data <> List.length paths then
    Error.invalid_arg ~__LOC__
      (sprintf "Store.set_expected_output: test %s, data:%i, paths:%i" test.name
         (List.length data) (List.length paths))
  else (
    init_expectation_workspace test;
    List.iter2 (fun path data -> Helpers.write_file path data) paths data)

let clear_expected_output (test : T.test) =
  test |> capture_paths_of_test
  |> List.iter (fun x -> Option.iter remove_file x.path_to_expected_output)

let read_name_file ~dir =
  let name_file_path = get_name_file_path_from_dir dir in
  if Sys.file_exists !!name_file_path then
    let contents = Helpers.read_file name_file_path in
    let len = String.length contents in
    if len > 0 && contents.[len - 1] = '\n' then
      Some (String.sub contents 0 (len - 1))
    else (* malformed contents: must be LF-terminated *)
      None
  else (* missing file *)
    None

type dead_snapshot = { dir_or_junk_file : Fpath.t; test_name : string option }

(*
   Identify snapshot folders (expected output) that don't belong to any
   test in the current test suite.
*)
let find_dead_snapshots tests : dead_snapshot list =
  let folder = get_expectation_workspace () in
  let names = Helpers.list_files folder in
  let names_tbl = Hashtbl.create 1000 in
  List.iter (fun name -> Hashtbl.replace names_tbl name ()) names;
  List.iter (fun (test : T.test) -> Hashtbl.remove names_tbl test.id) tests;
  let unknown_names = List.filter (Hashtbl.mem names_tbl) names in
  List.filter_map
    (fun name ->
      let dir = folder / name in
      let test_name, is_empty =
        match read_name_file ~dir with
        | None -> (None, false)
        | Some _ as test_name ->
            let other_data_files =
              dir |> Helpers.list_files
              |> List.filter (fun fname -> fname <> name_file_name)
            in
            (test_name, other_data_files = [])
      in
      if is_empty then (
        (* remove silently a folder that contains no critical data *)
        Helpers.remove_file_or_dir dir;
        None)
      else Some { dir_or_junk_file = dir; test_name })
    unknown_names

(**************************************************************************)
(* Output redirection *)
(**************************************************************************)

(* Redirect e.g. stderr to stdout during the execution of the function func.
   Usage:

     with_redirect Unix.stderr Unix.stdout do_something

   redirects stderr to stdout.
*)
let with_redirect_fd ~from ~to_ func () =
  (* keep the original file alive *)
  let original = Unix.dup from in
  P.protect
    ~finally:(fun () ->
      Unix.close original;
      P.return ())
    (fun () ->
      (* redirect to file *)
      Unix.dup2 to_ from;
      P.protect
        ~finally:(fun () ->
          (* cancel the redirect *)
          Unix.dup2 original from;
          P.return ())
        func)

(* Redirect stdout or stderr to a file *)
let with_redirect_fd_to_file fd filename func () =
  let file = Unix.openfile !!filename [ O_CREAT; O_TRUNC; O_WRONLY ] 0o666 in
  P.protect
    ~finally:(fun () ->
      Unix.close file;
      P.return ())
    (with_redirect_fd ~from:fd ~to_:file func)

(* stdout/stderr redirect using buffered channels. We're careful about
   flushing the channel of interest (from) before any redirection. *)
let with_redirect ~from ~to_ func () =
  flush from;
  let from_fd = Unix.descr_of_out_channel from in
  let to_fd = Unix.descr_of_out_channel to_ in
  with_redirect_fd ~from:from_fd ~to_:to_fd
    (fun () ->
      P.protect
        ~finally:(fun () ->
          flush from;
          P.return ())
        func)
    ()

(* Redirect a buffered channel to a file. *)
let with_redirect_to_file from filename func () =
  flush from;
  let from_fd = Unix.descr_of_out_channel from in
  with_redirect_fd_to_file from_fd filename
    (fun () ->
      P.protect
        ~finally:(fun () ->
          flush from;
          P.return ())
        func)
    ()

(* This is offered directly to users. *)
let with_capture from func =
  Temp_file.with_temp_file ~suffix:".out" (fun path ->
      with_redirect_to_file from path func () >>= fun res ->
      let output = read_file_exn path in
      P.return (res, output))

(* Apply functions to the data as a pipeline, from left to right. *)
let compose_functions_left_to_right funcs x =
  List.fold_left (fun x f -> f x) x funcs

(* Iff the test is configured to rewrite its output so as to mask the
   unpredicable parts, we rewrite the standard output file and we make a
   backup of the original. *)
let normalize_output (test : T.test) =
  match get_orig_output_suffix test with
  | None -> ()
  | Some orig_suffix ->
      let rewrite_string = compose_functions_left_to_right test.normalize in
      let paths = capture_paths_of_test test in
      get_checked_output_paths paths
      |> List.iter (fun std_path ->
             let backup_path = Fpath.v (!!std_path ^ orig_suffix) in
             if Sys.file_exists !!backup_path then Sys.remove !!backup_path;
             Sys.rename !!std_path !!backup_path;
             let orig_data = read_file_exn backup_path in
             let normalized_data =
               try rewrite_string orig_data with
               | e ->
                   Error.user_error
                     (sprintf
                        "Exception raised by the test's normalize_output \
                         function: %s"
                        (Printexc.to_string e))
             in
             Helpers.write_file std_path normalized_data)

let with_redirect_merged_stdout_stderr path func =
  (* redirect stderr to stdout, then redirect stdout to stdxxx file *)
  with_redirect_to_file stdout path
    (with_redirect ~from:stderr ~to_:stdout func)

let with_output_capture (test : T.test) (func : unit -> 'unit_promise) =
  let capture_paths = capture_paths_of_test test in
  let func =
    match (test.checked_output, capture_paths) with
    | Ignore_output, [ log_paths ] ->
        with_redirect_merged_stdout_stderr log_paths.path_to_output func
    | Stdout _, [ paths; log_paths ] ->
        with_redirect_to_file stderr log_paths.path_to_output
          (with_redirect_to_file stdout paths.path_to_output func)
    | Stderr _, [ paths; log_paths ] ->
        with_redirect_to_file stdout log_paths.path_to_output
          (with_redirect_to_file stderr paths.path_to_output func)
    | Stdxxx _, [ paths ] ->
        with_redirect_merged_stdout_stderr paths.path_to_output func
    | Split_stdout_stderr _, [ stdout_paths; stderr_paths ] ->
        with_redirect_to_file stdout stdout_paths.path_to_output
          (with_redirect_to_file stderr stderr_paths.path_to_output func)
    | _ -> (* bug: invalid combination *) Error.assert_false ~__LOC__ ()
  in
  fun () ->
    P.protect func ~finally:(fun () ->
        normalize_output test;
        P.return ())

let with_outcome_capture (test : T.test) func : unit -> unit Promise.t =
 fun () ->
  P.catch
    (fun () ->
      func () >>= fun res ->
      set_outcome test Succeeded;
      P.return res)
    (fun e trace ->
      set_outcome test Failed;
      (Printexc.raise_with_backtrace e trace : 'unit_promise))

(* Subtle: keep this a two-stage invocation:
   1. Build the 'func' closures. If there's a bug internal to Testo
      such as a missing path, it should be reported at this time
      to prevent the error from being caught or swallowed up as part of
      the test execution.
   2. Run the test by calling the resulting 'func'. This takes place in
      a special environment within wrappers. Testo's internal machinery
      should run as little as possible at this time to avoid mixing up
      Testo's errors with the test's execution.
*)
let with_result_capture (test : T.test) func : unit -> unit Promise.t =
  init_test_workspace test;
  let func = with_output_capture test func in
  let func = with_outcome_capture test func in
  func

(**************************************************************************)
(* High-level interface *)
(**************************************************************************)

let captured_output_of_data (kind : T.checked_output_kind) (data : string list)
    : T.captured_output =
  match (kind, data) with
  | Ignore_output, [ unchecked ] -> Ignored unchecked
  | Stdout _, [ out; unchecked ] -> Captured_stdout (out, unchecked)
  | Stderr _, [ err; unchecked ] -> Captured_stderr (err, unchecked)
  | Stdxxx _, [ data ] -> Captured_merged data
  | Split_stdout_stderr _, [ out; err ] -> Captured_stdout_stderr (out, err)
  | (Ignore_output | Stdout _ | Stderr _ | Stdxxx _ | Split_stdout_stderr _), _
    ->
      Error.assert_false ~__LOC__ ()

let expected_output_of_data (kind : T.checked_output_kind) (data : string list)
    : T.expected_output =
  match (kind, data) with
  | Ignore_output, [] -> Ignored
  | Stdout _, [ out ] -> Expected_stdout out
  | Stderr _, [ err ] -> Expected_stderr err
  | Stdxxx _, [ data ] -> Expected_merged data
  | Split_stdout_stderr _, [ out; err ] -> Expected_stdout_stderr (out, err)
  | (Ignore_output | Stdout _ | Stderr _ | Stdxxx _ | Split_stdout_stderr _), _
    ->
      Error.assert_false ~__LOC__ ()

let get_expectation (test : T.test) (paths : capture_paths list) : T.expectation
    =
  let expected_output =
    paths |> get_expected_output |> list_result_of_result_list |> function
    | Ok x -> Ok (expected_output_of_data test.checked_output x)
    | Error missing_files -> Error (T.Missing_files missing_files)
  in
  { expected_outcome = test.expected_outcome; expected_output }

let get_result (test : T.test) (paths : capture_paths list) :
    (T.result, T.missing_files) Result.t =
  match get_outcome test with
  | Error missing_file -> Error (Missing_files [ missing_file ])
  | Ok outcome -> (
      let opt_captured_output =
        paths |> get_output |> list_result_of_result_list
        |> Result.map (captured_output_of_data test.checked_output)
      in
      match opt_captured_output with
      | Error missing_files -> Error (Missing_files missing_files)
      | Ok captured_output -> Ok { outcome; captured_output })

let get_status (test : T.test) : T.status =
  let paths = capture_paths_of_test test in
  let expectation = get_expectation test paths in
  let result = get_result test paths in
  { expectation; result }

let fail_reason_of_pair (outcome : T.outcome) (output_matches : bool) :
    T.fail_reason option =
  match (outcome, output_matches) with
  | Failed, false -> Some Exception_and_wrong_output
  | Failed, true -> Some Exception
  | Succeeded, false -> Some Wrong_output
  | Succeeded, true -> None

let status_summary_of_status (status : T.status) : T.status_summary =
  match status.result with
  | Error _ ->
      {
        status_class = MISS;
        has_expected_output = false (* incorrect but does it matter? *);
      }
  | Ok result ->
      let expect = status.expectation in
      let has_expected_output, output_matches =
        match (expect.expected_output, result.captured_output) with
        | Ok output1, output2 when T.equal_checked_output output1 output2 ->
            (true, true)
        | Ok _, _ -> (true, false)
        | Error _, _ -> (false, true)
      in
      let fail_reason = fail_reason_of_pair result.outcome output_matches in
      let status_class : T.status_class =
        match (expect.expected_outcome, fail_reason) with
        | Should_succeed, None -> PASS
        | Should_succeed, Some fail_reason -> FAIL fail_reason
        | Should_fail _, None -> XPASS
        | Should_fail _, Some fail_reason -> XFAIL fail_reason
      in
      { status_class; has_expected_output }

let check_outcome (test : T.test) =
  match (test.expected_outcome, get_outcome test) with
  | Should_succeed, Ok Succeeded
  | Should_fail _, Ok Failed ->
      Ok ()
  | Should_succeed, Ok Failed ->
      Error
        (sprintf "Cannot approve test %S because it raised an exception."
           test.id)
  | Should_fail reason, Ok Succeeded ->
      Error
        (sprintf
           {|Cannot approve est %S because it succeeded but
was expected to fail by raising an exception.
The original reason given was:
  %S|}
           test.id reason)
  | _, Error missing_file -> Error (errmsg_of_missing_file missing_file)

type changed = Changed | Unchanged

exception Local_error of string

let approve_new_output (test : T.test) : (changed, string) Result.t =
  match test.skipped with
  | Some _reason -> Ok Unchanged
  | None -> (
      let paths = capture_paths_of_test test in
      match check_outcome test with
      | Error _ as res -> res
      | Ok () -> (
          let old_expectation = get_expectation test paths in
          clear_expected_output test;
          try
            let data =
              paths |> get_checked_output
              |> list_map (function
                   | Ok data -> data
                   | Error missing_file ->
                       raise (Local_error (errmsg_of_missing_file missing_file)))
            in
            set_expected_output test paths data;
            let new_expectation = get_expectation test paths in
            let changed =
              if old_expectation = new_expectation then Unchanged else Changed
            in
            Ok changed
          with
          | Local_error msg ->
              Error
                (sprintf "Cannot approve output for test %s: %s" test.id msg)))
