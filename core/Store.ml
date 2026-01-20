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

type capture_kind = Std | File | Log

(* All the data we need to handle the files that contain the captured output
   for a test after applying all defaults and options. *)
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

let with_file_in ?(binary = false) path f =
  if Sys.file_exists !!path then
    let ic = (if binary then open_in_bin else open_in) !!path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Ok (f ic))
  else Error path

let read_file ?binary path : (string, Fpath.t (* missing file *)) Result.t =
  with_file_in ?binary path Helpers.input_all

let errmsg_of_missing_file (path : Fpath.t) : string =
  sprintf "Missing or inaccessible file %s" !!path

let errmsg_of_missing_files (T.Missing_files paths) : string =
  match paths with
  | [ path ] -> errmsg_of_missing_file path
  | paths ->
      sprintf "Missing or inaccessible files: %s"
        (paths |> List.map Fpath.to_string |> String.concat ", ")

let read_file_exn ?binary path : string =
  match read_file ?binary path with
  | Ok data -> data
  | Error path -> Error.user_error (errmsg_of_missing_file path)

let with_text_file_out path f =
  let oc = open_out !!path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let write_text_file path data =
  with_text_file_out path (fun oc -> output_string oc data)

let remove_file path = if Sys.file_exists !!path then Sys.remove !!path

(*
   Keep track of a user-provided relative path and keep it in its original
   form for as a long as the current folder is still the same.

   We use this for the workspace folder paths from which other file paths
   are derived.
*)
type friendly_path = {
  cwd : Fpath.t;
  (* A relative or absolute path *)
  path : Fpath.t;
  (* The absolute path precomputed from the other fields *)
  abs_path : Fpath.t;
}

let create_friendly_path path : friendly_path =
  let cwd = Fpath.v (Sys.getcwd ()) in
  { cwd; path; abs_path = Fpath.(cwd // path) }

(* Return a relative path if the current folder hasn't changed since
   the path was provided. *)
let get_short_path (x : friendly_path) : Fpath.t =
  if Sys.getcwd () = !!(x.cwd) then x.path else x.abs_path

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

let get_full_status_workspace, set_status_workspace = make_late_init ()

let get_full_expectation_workspace, set_expectation_workspace =
  make_late_init ()

(* These functions return the original relative paths when possible
   i.e. if the current folder is the same as when starting the test program. *)
let get_status_workspace () = get_full_status_workspace () |> get_short_path

let get_expectation_workspace () =
  get_full_expectation_workspace () |> get_short_path

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
  set_status_workspace
    (create_friendly_path (status_workspace_root / project_name));
  set_expectation_workspace
    (create_friendly_path (expectation_workspace_root / project_name))

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
  Helpers.write_text_file (get_name_file_path test) contents

let must_create_expectation_workspace_for_test (test : T.test) =
  test.checked_output_files <> []
  ||
  let uses_internal_storage (x : T.checked_output_options) =
    match x.snapshot_path with
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

let init_status_workspace test =
  Helpers.make_dir_if_not_exists (get_test_status_workspace test)

let init_test_workspace test =
  init_status_workspace test;
  init_expectation_workspace test

(**************************************************************************)
(* Read/write data *)
(**************************************************************************)

let corrupted_file path =
  Error.user_error
    (sprintf
       "Uh oh, the test framework ran into a corrupted file: %S\n\
        Remove it and retry."
       !!path)

let get_completion_status_path (test : T.test) =
  get_test_status_workspace test / "completion_status"

let string_of_completion_status (x : T.completion_status) =
  match x with
  | Test_function_returned -> "Test_function_returned"
  | Test_function_raised_an_exception -> "Test_function_raised_an_exception"
  | Test_timeout -> "Test_timeout"

let completion_status_of_string path data : T.completion_status =
  match data with
  | "Test_function_returned" -> Test_function_returned
  | "Test_function_raised_an_exception" -> Test_function_raised_an_exception
  | "Test_timeout" -> Test_timeout
  | _ -> corrupted_file path

let set_completion_status (test : T.test) completion_status =
  let path = get_completion_status_path test in
  completion_status |> string_of_completion_status
  |> Helpers.write_text_file path

let get_completion_status (test : T.test) :
    (T.completion_status, Fpath.t (* missing file *)) Result.t =
  let path = get_completion_status_path test in
  match read_file path with
  | Ok data -> Ok (completion_status_of_string path data)
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

let get_std_snapshot_path (test : T.test) default_name
    (options : T.checked_output_options) =
  match options.snapshot_path with
  | None -> get_expectation_workspace () / test.id / default_name
  | Some path -> path

let get_file_snapshot_path (test : T.test) (x : T.checked_output_file) =
  match x.options.snapshot_path with
  | None -> get_expectation_workspace () / test.id / ("file-" ^ x.name)
  | Some path -> path

let short_name_of_checked_output_options default_name
    (options : T.checked_output_options) =
  match options.snapshot_path with
  | None -> default_name
  | Some path -> Fpath.basename path

(* This function may be used only for fixed, reserved filenames *)
let get_std_output_path (test : T.test) filename =
  get_status_workspace () / test.id / filename

(* This reserves the "file-" prefix to store all the captured output files
   named by the user *)
let get_output_file_path (test : T.test) (x : T.checked_output_file) =
  get_status_workspace () / test.id / ("file-" ^ x.name)

let get_exception_path (test : T.test) = get_std_output_path test "exception"

let store_exception (test : T.test) opt_msg =
  let path = get_exception_path test in
  match opt_msg with
  | None -> remove_file path
  | Some msg -> write_text_file path msg

let get_exception (test : T.test) =
  let path = get_exception_path test in
  match read_file path with
  | Ok data -> Some data
  | Error _path -> None

(*
   Derive the various file paths related to a given test, but excluding
   unchecked output (logs).
*)
let std_capture_paths_of_test (test : T.test) : capture_paths list =
  let unchecked_paths =
    {
      kind = Log;
      short_name = unchecked_filename;
      path_to_expected_output = None;
      path_to_output = get_std_output_path test unchecked_filename;
    }
  in
  match test.checked_output with
  | Ignore_output -> [ unchecked_paths ]
  | Stdout options ->
      [
        {
          kind = Std;
          short_name =
            short_name_of_checked_output_options stdout_filename options;
          path_to_expected_output =
            Some (get_std_snapshot_path test stdout_filename options);
          path_to_output = get_std_output_path test stdout_filename;
        };
        unchecked_paths;
      ]
  | Stderr options ->
      [
        {
          kind = Std;
          short_name =
            short_name_of_checked_output_options stderr_filename options;
          path_to_expected_output =
            Some (get_std_snapshot_path test stderr_filename options);
          path_to_output = get_std_output_path test stderr_filename;
        };
        unchecked_paths;
      ]
  | Stdxxx options ->
      [
        {
          kind = Std;
          short_name =
            short_name_of_checked_output_options stdxxx_filename options;
          path_to_expected_output =
            Some (get_std_snapshot_path test stdxxx_filename options);
          path_to_output = get_std_output_path test stdxxx_filename;
        };
      ]
  | Split_stdout_stderr (stdout_options, stderr_options) ->
      [
        {
          kind = Std;
          short_name =
            short_name_of_checked_output_options stdout_filename stdout_options;
          path_to_expected_output =
            Some (get_std_snapshot_path test stdout_filename stdout_options);
          path_to_output = get_std_output_path test stdout_filename;
        };
        {
          kind = Std;
          short_name =
            short_name_of_checked_output_options stderr_filename stderr_options;
          path_to_expected_output =
            Some (get_std_snapshot_path test stderr_filename stderr_options);
          path_to_output = get_std_output_path test stderr_filename;
        };
      ]

let file_capture_paths_of_test (test : T.test) : capture_paths list =
  test.checked_output_files
  |> Helpers.list_map (fun (x : T.checked_output_file) ->
      {
        kind = File;
        short_name = x.name;
        path_to_expected_output = Some (get_file_snapshot_path test x);
        path_to_output = get_output_file_path test x;
      })

let all_capture_paths_of_test (test : T.test) =
  std_capture_paths_of_test test @ file_capture_paths_of_test test

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
  get_std_output_path test unchecked_filename

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

let get_snapshot_paths (paths : capture_paths list) =
  paths |> List.filter_map (fun x -> x.path_to_expected_output)

let get_expected_output (paths : capture_paths list) =
  paths |> get_snapshot_paths |> list_map read_file

let set_expected_output (test : T.test) (capture_paths : capture_paths list)
    (data : string list) =
  let paths = capture_paths |> get_snapshot_paths in
  if List.length data <> List.length paths then
    Error.invalid_arg ~__LOC__
      (sprintf "Store.set_expected_output: test %s, data:%i, paths:%i" test.name
         (List.length data) (List.length paths))
  else (
    init_expectation_workspace test;
    List.iter2 (fun path data -> Helpers.write_text_file path data) paths data)

let clear_expected_output (test : T.test) =
  test |> std_capture_paths_of_test
  |> List.iter (fun x -> Option.iter remove_file x.path_to_expected_output);
  test.checked_output_files
  |> List.iter (fun (x : T.checked_output_file) ->
      remove_file (get_file_snapshot_path test x))

let read_name_file ~dir =
  let name_file_path = get_name_file_path_from_dir dir in
  if Sys.file_exists !!name_file_path then
    let contents = Helpers.read_text_file name_file_path in
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

let remove_dead_snapshot (x : dead_snapshot) =
  Helpers.remove_file_or_dir x.dir_or_junk_file

(**************************************************************************)
(* Output redirection *)
(**************************************************************************)

(* Redirect e.g. stderr to stdout during the execution of the function func.
   Usage:

     with_redirect [Unix.stderr] Unix.stdout do_something

   redirects stderr to stdout while running do_something.

     with_redirect [Unix.stderr; Unix.stdout] fd do_something

   redirects both stderr and stdout to fd while running do_something.
*)
let with_redirect_fds ~(from : Unix.file_descr list) ~to_ func () =
  (* keep the original file descriptors (fds) alive *)
  let originals = List.map Unix.dup from in
  P.protect
    ~finally:(fun () ->
      List.iter Unix.close originals;
      P.return ())
    (fun () ->
      (* redirect all fds in [from] to the fd [to_] *)
      List.iter (Unix.dup2 to_) from;
      P.protect
        ~finally:(fun () ->
          (* cancel the redirects by restoring the [from] fds to their
             originals *)
          List.iter2 Unix.dup2 originals from;
          P.return ())
        func)

(* Redirect a list of buffered channels to a file. *)
let with_redirects_to_out_channel from to_oc func () =
  (* Before redirecting, flush all pending writes to the channels *)
  List.iter flush from;
  let from_fds = List.map Unix.descr_of_out_channel from in
  with_redirect_fds ~from:from_fds
    ~to_:(Unix.descr_of_out_channel to_oc)
    (fun () ->
      P.protect
        ~finally:(fun () ->
          (* Before cancelling the redirects, flush all pending writes *)
          List.iter flush from;
          P.return ())
        func)
    ()

let with_open_out path func =
  let oc = open_out !!path in
  Fun.protect (fun () -> func oc) ~finally:(fun () -> close_out_noerr oc)

let with_redirects_to_file from to_file func () =
  with_open_out to_file (fun oc ->
      with_redirects_to_out_channel from oc func ())

(* Redirect a buffered channel to a file. *)
let with_redirect_to_file from filename func () =
  with_redirects_to_file [ from ] filename func ()

(* This is offered directly to users. *)
let with_capture ?binary from func =
  Temp_file.with_open_temp_text_file ~suffix:".out" (fun path oc ->
      with_redirects_to_out_channel [ from ] oc func () >>= fun res ->
      let output = read_file_exn ?binary path in
      P.return (res, output))

(* Apply functions to the data as a pipeline, from left to right. *)
let compose_functions_left_to_right funcs x =
  List.fold_left (fun x f -> f x) x funcs

(* Iff the test is configured to rewrite its output so as to mask the
   unpredicable parts, we rewrite the standard output file and we make a
   backup of the original.

   We normalize only the captured stdout/stderr output, not checked output
   files (because the user can do it themselves from within the test
   function).
*)
let normalize_output (test : T.test) =
  match get_orig_output_suffix test with
  | None -> ()
  | Some orig_suffix ->
      let rewrite_string = compose_functions_left_to_right test.normalize in
      let paths = std_capture_paths_of_test test in
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
          Helpers.write_text_file std_path normalized_data)

let with_redirect_merged_stdout_stderr path func =
  (* redirect stderr and stdout to a stdxxx file *)
  with_redirects_to_file [ stdout; stderr ] path func

let with_output_capture (test : T.test) (func : unit -> 'unit_promise) =
  let capture_paths = std_capture_paths_of_test test in
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

let with_completion_status_capture (test : T.test) func : unit -> unit Promise.t
    =
 fun () ->
  P.catch
    (fun () ->
      func () >>= fun res ->
      set_completion_status test Test_function_returned;
      P.return res)
    (fun e trace ->
      set_completion_status test Test_function_raised_an_exception;
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
  let func = with_output_capture test func in
  let func = with_completion_status_capture test func in
  func

let mark_test_as_timed_out (test : T.test) =
  set_completion_status test Test_timeout

let stash_output_file (test : T.test) src_path (x : T.checked_output_file) :
    unit =
  let dst_path = get_output_file_path test x in
  Helpers.copy_text_file src_path dst_path

let remove_stashed_output_files (test : T.test) =
  test.checked_output_files
  |> List.iter (fun (x : T.checked_output_file) ->
      remove_file (get_output_file_path test x))

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

(* This is rather horrible. Simplify? *)
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

let get_expectation (test : T.test) ~(std_capture_paths : capture_paths list) :
    T.expectation =
  let expected_output =
    std_capture_paths |> get_expected_output |> list_result_of_result_list
    |> function
    | Ok x -> Ok (expected_output_of_data test.checked_output x)
    | Error missing_files -> Error (T.Missing_files missing_files)
  in
  let expected_output_files =
    test.checked_output_files
    |> Helpers.list_map (fun (checked_file : T.checked_output_file) ->
        match read_file (get_file_snapshot_path test checked_file) with
        | Ok contents ->
            Ok
              ({ checked_file; contents } : T.checked_output_file_with_contents)
        | Error path -> Error path)
  in
  {
    expected_outcome = test.expected_outcome;
    expected_output;
    expected_output_files;
  }

(* Fail if any capture file is missing *)
let get_result (test : T.test) (paths : capture_paths list) :
    (T.result, T.missing_files) Result.t =
  match get_completion_status test with
  | Error missing_file -> Error (Missing_files [ missing_file ])
  | Ok completion_status -> (
      (* captured output files *)
      let captured_output_files, missing_output_files =
        test.checked_output_files
        |> Helpers.list_map (fun (checked_file : T.checked_output_file) ->
            match read_file (get_output_file_path test checked_file) with
            | Ok contents ->
                Ok
                  ({ checked_file; contents }
                    : T.checked_output_file_with_contents)
            | Error missing_file -> Error missing_file)
        |> Helpers.split_result_list
      in
      (* captured standard output *)
      let opt_captured_output =
        paths |> get_output |> list_result_of_result_list
        |> Result.map (captured_output_of_data test.checked_output)
      in
      (*
         - If the test ran previously and nobody messed with Testo's
           workspace, missing output files are due to a test function
           that didn't make all the calls to 'stash_output_file' it should
           have. It could be because the test raised an exception because
           it could happen or it could be that programmer forgot or
           didn't realize they should have called 'stash_output_file'.
         - Missing files for captured stdout or stderr on the other hand
           are restricted to the former cases i.e. not an error in the test
           function.
      *)
      match (opt_captured_output, missing_output_files) with
      | Ok captured_output, missing_output_files ->
          (* This is most likely a test failure (see above).
             It's more important to report the exception if there was one
             than a missing output file. This is why we don't return an Error
             here even if some checked output files are missing. *)
          Ok
            {
              completion_status;
              captured_output;
              captured_output_files;
              missing_output_files;
            }
      | Error missing_std_files, missing_output_files ->
          Error (Missing_files (missing_std_files @ missing_output_files)))

let get_status (test : T.test) : T.status =
  let paths = std_capture_paths_of_test test in
  let expectation = get_expectation test ~std_capture_paths:paths in
  let result = get_result test paths in
  { expectation; result }

let create_outcome (completion_status : T.completion_status)
    ~(has_all_output_files : bool) ~(output_matches : bool) : T.outcome =
  match completion_status with
  | Test_function_raised_an_exception ->
      (* If the test raised an exception, there's a good chance we don't
         have some of the output files. In this case, we report the cause
         of failure as the exception, not as the missing output files. *)
      Failed Raised_exception
  | Test_function_returned ->
      if has_all_output_files then
        match output_matches with
        | false -> Failed Incorrect_output
        | true -> Succeeded
      else Failed Missing_output_file
  | Test_timeout -> Failed Timeout

let outcome_of_expectation_and_result (expect : T.expectation)
    (result : T.result) : T.outcome * bool =
  let has_expected_std_output, output_matches =
    match (expect.expected_output, result.captured_output) with
    | Ok output1, output2 when T.equal_checked_output output1 output2 ->
        (true, true)
    | Ok _, _ -> (true, false)
    | Error _, _ -> (false, true)
  in
  let has_expected_output_files, output_files_match =
    match result.missing_output_files with
    | [] ->
        (* Assume the lists of expected files and captured files are complete
           and in the same order. *)
        List.fold_left2
          (fun (has_expected_output_files, output_files_match)
               (expect :
                 (T.checked_output_file_with_contents, Fpath.t) Result.t)
               (result : T.checked_output_file_with_contents) ->
            match expect with
            | Ok expected ->
                assert (expected.checked_file = result.checked_file);
                if expected.contents = result.contents then
                  (has_expected_output_files, output_files_match)
                else (has_expected_output_files, false)
            | Error _missing_expected_file -> (false, false))
          (true, true) expect.expected_output_files result.captured_output_files
    | _ -> (false, false)
  in
  let outcome =
    create_outcome result.completion_status
      ~has_all_output_files:has_expected_output_files
      ~output_matches:(output_matches && output_files_match)
  in
  (outcome, has_expected_std_output && has_expected_output_files)

let status_summary_of_status (status : T.status) : T.status_summary =
  match status.result with
  | Error missing_files ->
      {
        passing_status = MISS missing_files;
        (* These two fields are meaningless *)
        outcome = Failed Raised_exception;
        has_expected_output = false;
      }
  | Ok result ->
      let expect = status.expectation in
      let outcome, has_expected_output =
        outcome_of_expectation_and_result expect result
      in
      let passing_status : T.passing_status =
        match (expect.expected_outcome, outcome) with
        | Should_succeed, Succeeded -> PASS
        | Should_succeed, Failed fail_reason -> FAIL fail_reason
        | Should_fail _, Succeeded -> XPASS
        | Should_fail _, Failed fail_reason -> XFAIL fail_reason
      in
      { passing_status; outcome; has_expected_output }

let check_status_before_approval (test : T.test) : (unit, Error.msg) result =
  let status = get_status test in
  let status_summary = status_summary_of_status status in
  match status_summary.passing_status with
  | PASS
  | XPASS ->
      Ok ()
  | FAIL Raised_exception
  | XFAIL Raised_exception ->
      Error
        (Error.Warning
           (sprintf
              "Cannot approve test because it raised an exception: %s '%s'"
              test.id test.internal_full_name))
  | FAIL Missing_output_file
  | XFAIL Missing_output_file ->
      (* Approving the test will create the missing snapshot file *)
      Ok ()
  | FAIL Incorrect_output
  | XFAIL Incorrect_output ->
      (* The user is approving the new output reported as incorrect *)
      Ok ()
  | FAIL Timeout
  | XFAIL Timeout ->
      Error
        (Error.Error
           (sprintf "Cannot approve test because it timed out: %s '%s'" test.id
              test.internal_full_name))
  | MISS missing_files ->
      Error (Error.Error (errmsg_of_missing_files missing_files))

type changed = Changed | Unchanged

exception Local_error of string

let approve_new_output (test : T.test) : (changed, Error.msg) Result.t =
  match test.skipped with
  | Some _reason -> Ok Unchanged
  | None -> (
      let std_capture_paths = std_capture_paths_of_test test in
      let file_capture_paths = file_capture_paths_of_test test in
      let all_capture_paths = std_capture_paths @ file_capture_paths in
      match check_status_before_approval test with
      | Error _ as res -> res
      | Ok () -> (
          let old_expectation = get_expectation test ~std_capture_paths in
          clear_expected_output test;
          try
            let data =
              all_capture_paths |> get_checked_output
              |> list_map (function
                | Ok data -> data
                | Error missing_file ->
                    raise (Local_error (errmsg_of_missing_file missing_file)))
            in
            set_expected_output test all_capture_paths data;
            let new_expectation = get_expectation test ~std_capture_paths in
            let changed =
              if old_expectation = new_expectation then Unchanged else Changed
            in
            Ok changed
          with
          | Local_error msg ->
              Error
                (Error.Error
                   (sprintf "Cannot approve output for test %s: %s" test.id msg))
          ))
