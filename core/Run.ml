(*
   Filter and run tests
*)

open Printf
open Testo_util
open Fpath_.Operators
open Promise.Operators
module T = Types
module P = Promise

type status_output_style =
  | Long_all
  | Compact_all
  | Long_important
  | Compact_important

type status_stats = {
  total_tests : int;
  selected_tests : int;
  skipped_tests : int ref;
  pass : int ref;
  fail : int ref;
  xfail : int ref;
  xpass : int ref;
  miss : int ref;
  needs_approval : int ref;
}

type success = OK | OK_but_new | Not_OK of T.fail_reason option
type alcotest_test_case = string * [ `Quick | `Slow ] * (unit -> unit Promise.t)
type alcotest_test = string * alcotest_test_case list

(* The exit codes used by the built-in test runner. *)
let exit_success = 0
let exit_failure = 1
let exit_internal_error = 2

(* Left margin for text relating to a test *)
let bullet = Style.color Faint "• "

(* For appending an 's' at the end for words in messages *)
let if_plural num s = if num >= 2 then s else ""

(* For appending an 's' to conjugated verbs in messages *)
let if_singular num s = if num <= 1 then s else ""

(*
   Compute a checksum used to detect obvious differences between
   the list of tests obtained in the master process and the list of tests
   obtained in a worker process in charge of taking a slice of that list.

   The checksum is passed by the master to each worker via a command-line
   option.
*)
let get_checksum (tests : T.test list) =
  tests
  |> Helpers.list_map (fun (x : T.test) -> x.id)
  |> String.concat " " |> Digest.string |> Digest.to_hex

let check_checksum_or_abort ~expected_checksum tests =
  match expected_checksum with
  | None -> ()
  | Some expected ->
      let checksum = get_checksum tests in
      if checksum <> expected then
        Worker.Server.fatal_error
          "Checksum mismatch: the test suite in a worker process is different \
           than the list of tests in the master process. You need to make sure \
           that the name and the order of the tests in the test suite is \
           deterministic and independent of any internal command-line option \
           (e.g. '--worker')."

(*
   Check that no two tests have the same full name or the same ID.
*)
let check_id_uniqueness (tests : T.test list) =
  let id_tbl = Hashtbl.create 1000 in
  tests
  |> List.iter (fun (test : T.test) ->
         let id = test.id in
         let name = test.internal_full_name in
         match Hashtbl.find_opt id_tbl id with
         | None -> Hashtbl.add id_tbl id test.internal_full_name
         | Some name0 ->
             if name = name0 then
               Error.user_error
                 (sprintf "Two tests have the same name: %s" name)
             else
               Error.user_error
                 (sprintf
                    "Hash collision for two tests with different names:\n\
                    \  %S\n\
                    \  %S\n\
                     These names result in the same hash ID: %s\n\
                     If this is accidental, please report the problem to the \
                     authors of\n\
                     testo."
                    name0 name id))

(*
   Check that the user didn't mistakenly reuse the same path for keeping
   output snapshots since we now support custom paths.
   Raises an exception.
*)
let check_snapshot_uniqueness (tests : T.test list) =
  let path_tbl = Hashtbl.create 1000 in
  tests
  |> List.iter (fun (test : T.test) ->
         test |> Store.capture_paths_of_test
         |> List.iter (fun (paths : Store.capture_paths) ->
                match paths.path_to_expected_output with
                | None -> ()
                | Some path -> (
                    (* This may or may not be a custom path. *)
                    let test_name = test.internal_full_name in
                    match Hashtbl.find_opt path_tbl path with
                    | None -> Hashtbl.add path_tbl path test_name
                    | Some test_name0 ->
                        if test_name = test_name0 then
                          Error.user_error
                            (sprintf
                               "A test uses the same snapshot path twice:\n\
                                - test name: %S\n\
                                - conflicting snapshot path: %s\n\
                                Fix it in the test definition.\n"
                               test_name !!path)
                        else
                          Error.user_error
                            (sprintf
                               "Two different tests use the same snapshot path:\n\
                                - first test: %S\n\
                                - second test: %S\n\
                                - conflicting snapshot path: %s\n"
                               test_name0 test_name !!path))))

let check_test_definitions tests =
  check_id_uniqueness tests;
  check_snapshot_uniqueness tests

let string_of_status_summary (sum : T.status_summary) =
  let approval_suffix = if sum.has_expected_output then "" else "*" in
  match sum.status_class with
  | PASS -> "PASS" ^ approval_suffix
  | FAIL _ -> "FAIL" ^ approval_suffix
  | XFAIL _ -> "XFAIL" ^ approval_suffix
  | XPASS -> "XPASS" ^ approval_suffix
  | MISS -> "MISS"

let success_of_status_summary (sum : T.status_summary) =
  match sum.status_class with
  | PASS
  | XFAIL _ ->
      if sum.has_expected_output then OK else OK_but_new
  | FAIL fail_reason -> Not_OK (Some fail_reason)
  | XPASS -> Not_OK None
  | MISS -> OK_but_new

let color_of_status_summary (sum : T.status_summary) : Style.color =
  match success_of_status_summary sum with
  | OK -> Green
  | OK_but_new -> Yellow
  | Not_OK _ -> Red

let brackets s = sprintf "[%s]" s

(* Fixed-width output: "[PASS] ", "[XFAIL]" *)
let format_status_summary (sum : T.status_summary) =
  let style = color_of_status_summary sum in
  let displayed_string = sum |> string_of_status_summary |> brackets in
  Style.left_col displayed_string |> Style.color style

let stats_of_tests tests tests_with_status =
  let stats =
    {
      total_tests = List.length tests;
      selected_tests = List.length tests_with_status;
      skipped_tests = ref 0;
      pass = ref 0;
      fail = ref 0;
      xfail = ref 0;
      xpass = ref 0;
      miss = ref 0;
      needs_approval = ref 0;
    }
  in
  tests_with_status
  |> List.iter (fun ((test : T.test), _status, (sum : T.status_summary)) ->
         match test.skipped with
         | Some _reason -> incr stats.skipped_tests
         | None ->
             (match sum.status_class with
             | MISS -> ()
             | _ ->
                 if not sum.has_expected_output then incr stats.needs_approval);
             incr
               (match sum.status_class with
               | PASS -> stats.pass
               | FAIL _ -> stats.fail
               | XFAIL _ -> stats.xfail
               | XPASS -> stats.xpass
               | MISS -> stats.miss));
  stats

(* Sample output: "", " {foo, bar}" *)
let format_tags (test : T.test) =
  match test.tags with
  | [] -> ""
  | tags ->
      let tags =
        List.sort Tag.compare tags
        |> Helpers.list_map (fun tag -> Style.color Bold (Tag.to_string tag))
      in
      sprintf " (%s)" (String.concat " " tags)

let format_title (test : T.test) : string =
  sprintf "%s%s %s" test.id (format_tags test)
    (test.category @ [ test.name ]
    |> Helpers.list_map (Style.color Cyan)
    |> String.concat " > ")

(*
   Group pairs by the first value of the pair, preserving the original
   order as much as possible.
*)
let group_by_key key_value_list =
  let tbl = Hashtbl.create 100 in
  key_value_list
  |> List.iteri (fun pos (k, v) ->
         let tbl_v =
           match Hashtbl.find_opt tbl k with
           | None -> (pos, [ v ])
           | Some (pos, vl) -> (pos, v :: vl)
         in
         Hashtbl.replace tbl k tbl_v);
  let clusters =
    Hashtbl.fold (fun k (pos, vl) acc -> (pos, (k, List.rev vl)) :: acc) tbl []
  in
  clusters
  |> List.sort (fun (pos1, _) (pos2, _) -> compare pos1 pos2)
  |> Helpers.list_map snd

let chdir_error (test : T.test) =
  if test.tolerate_chdir then None
  else
    Some
      (fun old new_ ->
        sprintf "Current working directory (cwd) wasn't restored: %s -> %s" old
          new_)

(*
   Protect against tests that mutate global variables.

   A way to do this would be to fork off a new process for each test but
   it's probably too costly and prevents the test from using hacks relying
   on globals (such as the lazy initialization of some shared resource).

   The Testo system needs at least the relative path to its own
   files to remain valid even if a test changes the current directory
   by a call to chdir. An alternative way to do that would be to use
   absolute paths but it makes error messages a little uglier.

   Some other mutable globals could also be protected. They include:
   - environment variables
   - whether the stack backtrace should be recorded
   - terminal settings (affecting color output)
   - ...

   TODO: add options at test creation time to tolerate the failure to restore
   this or that setting.
*)
let protect_globals (test : T.test) (func : unit -> 'promise) : unit -> 'promise
    =
  let protect_global ?error_if_changed get set func () =
    let original_value = get () in
    P.protect
      ~finally:(fun () ->
        let current_value = get () in
        set original_value;
        (match error_if_changed with
        | Some err_msg_func when current_value <> original_value ->
            Error.fail_test (err_msg_func original_value current_value)
        | _ -> ());
        P.return ())
      func
  in
  func
  |> protect_global Sys.getcwd Sys.chdir ?error_if_changed:(chdir_error test)
  |> protect_global Printexc.backtrace_status Printexc.record_backtrace
(* TODO: more universal settings to protect? *)

let to_alcotest_gen ~(alcotest_skip : unit -> _)
    ~(wrap_test_function : T.test -> (unit -> 'a) -> unit -> 'a)
    (tests : T.test list) : _ list =
  tests
  |> Helpers.list_map (fun (test : T.test) ->
         let suite_name =
           match test.category with
           | [] -> test.name
           | path -> String.concat " > " path
         in
         let xfail_note =
           match test.expected_outcome with
           | Should_succeed -> ""
           | Should_fail _reason -> " [xfail]"
         in
         let suite_name =
           sprintf "%s%s%s %s" test.id xfail_note (format_tags test) suite_name
         in
         let func =
           (*
              A "skipped" test is marked as skipped in Alcotest's run output
              and leaves no trace such that Testo thinks it never ran.
           *)
           match test.skipped with
           | Some _reason ->
               fun () ->
                 alcotest_skip () |> ignore;
                 Error.user_error
                   "The function 'alcotest_skip' passed to 'Testo.to_alcotest' \
                    didn't raise\n\
                    an exception as expected. 'Testo.to_alcotest' should be \
                    called with\n\
                    '~alcotest_skip:Alcotest.skip'."
           | None -> (wrap_test_function test test.func : unit -> _)
         in
         (* This is the format expected by Alcotest: *)
         (suite_name, (test.name, `Quick, func)))
  |> group_by_key

let with_store_exception (test : T.test) func () =
  P.catch
    (fun () ->
      func () >>= fun () ->
      Store.store_exception test None;
      P.return ())
    (fun exn trace ->
      let msg =
        sprintf "%s\n%s" (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string trace)
      in
      Store.store_exception test (Some msg);
      (Printexc.raise_with_backtrace exn trace : unit Promise.t))

let with_flip_xfail_outcome (test : T.test) func =
  match test.expected_outcome with
  | Should_succeed -> func
  | Should_fail _reason ->
      fun () ->
        P.catch
          (fun () ->
            func () >>= fun () ->
            Error.fail_test "XPASS: This test failed to raise an exception")
          (fun exn trace ->
            eprintf "XFAIL: As expected, an exception was raised: %s\n%s\n"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string trace);
            P.return ())

let conditional_wrap condition wrapper func =
  if condition then wrapper func else func

let wrap_test_function ~with_storage ~flip_xfail_outcome test func =
  func
  |> conditional_wrap with_storage (with_store_exception test)
  |> conditional_wrap flip_xfail_outcome (with_flip_xfail_outcome test)
  |> protect_globals test
  |> conditional_wrap with_storage (Store.with_result_capture test)

let to_alcotest_internal ~alcotest_skip ~with_storage ~flip_xfail_outcome tests
    =
  to_alcotest_gen ~alcotest_skip
    ~wrap_test_function:(wrap_test_function ~with_storage ~flip_xfail_outcome)
    tests

(* Exported versions that exposes a plain Alcotest test suite that doesn't
   write test statuses and prints "OK" for XFAIL statuses. *)
let to_alcotest ~alcotest_skip tests =
  to_alcotest_internal ~alcotest_skip ~with_storage:false
    ~flip_xfail_outcome:true tests

let filter ~filter_by_substring ~filter_by_tag tests =
  let filter_sub =
    match filter_by_substring with
    | None -> None
    | Some sub ->
        let contains_sub = Helpers.contains_substring sub in
        Some
          (fun (test : T.test) ->
            contains_sub test.internal_full_name || contains_sub test.id)
  in
  let filter_tag =
    match filter_by_tag with
    | None -> None
    | Some tag ->
        Some (fun (test : T.test) -> List.exists (Tag.equal tag) test.tags)
  in
  let filters = [ filter_sub; filter_tag ] |> List.filter_map (fun x -> x) in
  match filters with
  | [] -> tests
  | _ ->
      tests
      |> List.filter (fun test ->
             List.for_all (fun filter -> filter test) filters)

(* Returns an exit code *)
let print_errors (xs : (Store.changed, string) Result.t list) : int =
  let changed = ref 0 in
  let error_messages = ref [] in
  xs
  |> List.iter (function
       | Ok Store.Changed -> incr changed
       | Ok Store.Unchanged -> ()
       | Error msg -> error_messages := msg :: !error_messages);
  let changed = !changed in
  let error_messages = List.rev !error_messages in
  printf "Expected output changed for %i test%s.\n%!" changed
    (if_plural changed "s");
  match error_messages with
  | [] -> exit_success
  | xs ->
      let n_errors = List.length xs in
      let error_str =
        if n_errors >= 2 then Style.color Red "Errors:\n"
        else Style.color Red "Error: "
      in
      let msg = String.concat "\n" error_messages in
      eprintf "%s%s\n%!" error_str msg;
      exit_failure

let is_important_status ((test : T.test), _status, (sum : T.status_summary)) =
  test.skipped = None
  && ((not sum.has_expected_output)
     ||
     match success_of_status_summary sum with
     | OK -> false
     | OK_but_new
     | Not_OK _ ->
         true)

(*
   Show difference between expected and actual output if both files are
   available.
*)
let show_diff (output_kind : string) path_to_expected_output path_to_output =
  if Sys.file_exists !!path_to_expected_output then
    let equal, diffs = Diff.files path_to_expected_output path_to_output in
    if not equal then
      printf "%sCaptured %s differs from expectation:\n%s" bullet output_kind
        diffs

let show_output_details (test : T.test) (sum : T.status_summary)
    (capture_paths : Store.capture_paths list) =
  let success = success_of_status_summary sum in
  capture_paths
  |> List.iter
       (fun
         ({ short_name; path_to_expected_output; path_to_output; _ } :
           Store.capture_paths)
       ->
         flush stdout;
         flush stderr;
         (match path_to_expected_output with
         | None -> ()
         | Some path_to_expected_output ->
             (match success with
             | OK
             | OK_but_new ->
                 ()
             | Not_OK _ ->
                 show_diff short_name path_to_expected_output path_to_output);
             if success <> OK_but_new then
               printf "%sPath to expected %s: %s\n" bullet short_name
                 !!path_to_expected_output);
         printf "%sPath to captured %s: %s%s\n" bullet short_name
           !!path_to_output
           (match Store.get_orig_output_suffix test with
           | Some suffix -> sprintf " [%s]" suffix
           | None -> ""))

let print_error text = printf "%s%s\n" bullet (Style.color Red text)

let format_one_line_status ((test : T.test), (_status : T.status), sum) =
  sprintf "%s%s" (format_status_summary sum) (format_title test)

let print_one_line_status test_with_status =
  printf "%s\n" (format_one_line_status test_with_status)

let with_highlight_test ~highlight_test ~title func =
  if highlight_test then printf "%s" (Style.frame title)
  else printf "%s\n" title;
  func ();
  if highlight_test then print_string (Style.horizontal_line ())

let ends_with_newline str =
  (* not available in ocaml 4.08:
     String.ends_with ~suffix:"\n" str
  *)
  str <> "" && str.[String.length str - 1] = '\n'

let print_status ~highlight_test ~always_show_unchecked_output
    (((test : T.test), (status : T.status), sum) as test_with_status) =
  let title = format_one_line_status test_with_status in
  with_highlight_test ~highlight_test ~title (fun () ->
      (* Details about expectations *)
      match test.skipped with
      | Some _reason -> printf "%sAlways skipped\n" bullet
      | None -> (
          (match status.expectation.expected_outcome with
          | Should_succeed -> ()
          | Should_fail reason ->
              printf "%sExpected to fail: %s\n" bullet reason);
          (match test.checked_output with
          | Ignore_output -> ()
          | _ ->
              let text =
                match test.checked_output with
                | Ignore_output -> Error.assert_false ~__LOC__ ()
                | Stdout _ -> "stdout"
                | Stderr _ -> "stderr"
                | Stdxxx _ -> "merged stdout and stderr"
                | Split_stdout_stderr _ -> "separate stdout and stderr"
              in
              printf "%sChecked output: %s\n" bullet text);
          (* Details about results *)
          (match status.expectation.expected_output with
          | Error (Missing_files [ path ]) ->
              print_error
                (sprintf "Missing file containing the expected output: %s"
                   !!path)
          | Error (Missing_files paths) ->
              print_error
                (sprintf "Missing files containing the expected output: %s"
                   (String.concat ", " (Fpath_.to_string_list paths)))
          | Ok _expected_output -> (
              match status.result with
              | Error (Missing_files [ path ]) ->
                  print_error
                    (sprintf "Missing file containing the test output: %s"
                       !!path)
              | Error (Missing_files paths) ->
                  print_error
                    (sprintf "Missing files containing the test output: %s"
                       (String.concat ", " (Fpath_.to_string_list paths)))
              | Ok _ -> ()));
          let capture_paths = Store.capture_paths_of_test test in
          show_output_details test sum capture_paths;
          let success = success_of_status_summary sum in
          let show_unchecked_output =
            always_show_unchecked_output
            ||
            match success with
            | OK -> false
            | OK_but_new -> true
            | Not_OK _ -> true
          in
          (* TODO: show the checked output to be approved? *)
          (if show_unchecked_output then
             match Store.get_unchecked_output test with
             | None -> (
                 match success with
                 | OK -> ()
                 | OK_but_new -> ()
                 | Not_OK (Some (Exception | Exception_and_wrong_output)) ->
                     printf
                       "%sFailed due to an exception. See captured output.\n"
                       bullet
                 | Not_OK (Some Wrong_output) ->
                     printf "%sFailed due to wrong output.\n" bullet
                 | Not_OK None ->
                     printf
                       "%sSucceded when it should have failed. See captured \
                        output.\n"
                       bullet)
             | Some (log_description, data) -> (
                 match data with
                 | "" -> printf "%sLog (%s) is empty.\n" bullet log_description
                 | _ ->
                     printf "%sLog (%s):\n%s" bullet log_description
                       (Style.quote_multiline_text data);
                     if not (ends_with_newline data) then print_char '\n'));
          (* Show the exception in case of a test failure due to an exception *)
          if show_unchecked_output then
            match success with
            | Not_OK (Some (Exception | Exception_and_wrong_output)) -> (
                match Store.get_exception test with
                | Some msg ->
                    printf "%sException raised by the test:\n%s" bullet
                      (Style.color Red (Style.quote_multiline_text msg))
                | None ->
                    (* = assert false *)
                    ())
            | OK
            | OK_but_new
              when always_show_unchecked_output -> (
                match Store.get_exception test with
                | Some msg ->
                    printf "%sException raised by the test:\n%s" bullet
                      (Style.color Green (Style.quote_multiline_text msg))
                | None -> ())
            | OK
            | OK_but_new
            | Not_OK (Some Wrong_output)
            | Not_OK None ->
                ()));
  flush stdout

let print_statuses ~highlight_test ~always_show_unchecked_output
    tests_with_status =
  tests_with_status
  |> List.iter (print_status ~highlight_test ~always_show_unchecked_output)

(*
   If anything's not perfect, consider it a failure.
*)
let is_overall_success statuses =
  statuses
  |> List.for_all (fun ((test : T.test), _status, sum) ->
         test.skipped <> None
         ||
         match sum |> success_of_status_summary with
         | OK -> true
         | OK_but_new -> false
         | Not_OK _ -> false)

(*
   Status output:
   0. Introduction: explain how to read the output.
   1. Long status: for each selected test, show all the details one might
      want to know about the test.
   2. Short status: show only the status of the tests that need our attention.
   3. Summary: give the counts for each test with a particular state.

   Options:
   --full: all of the above, the default
   --short: show only short status and summary
*)
let print_status_introduction () =
  printf
    {|Legend:
%s[PASS]: a successful test that was expected to succeed (good);
%s[FAIL]: a failing test that was expected to succeed (needs fixing);
%s[XFAIL]: a failing test that was expected to fail (tolerated failure);
%s[XPASS]: a successful test that was expected to fail (progress?).
%s[MISS]: a test that never ran;
%s[SKIP]: a test that is always skipped but kept around for some reason;
%s[xxxx*]: a new test for which there's no expected output yet.
  In this case, you should review the test output and run the 'approve'
  subcommand once you're satisfied with the output.
|}
    bullet bullet bullet bullet bullet bullet bullet

(*
   Print one status line per test

   'important': report only tests that need attention
*)
let print_compact_status ?(important = false) tests_with_status =
  let tests_with_status =
    if important then List.filter is_important_status tests_with_status
    else tests_with_status
  in
  List.iter print_one_line_status tests_with_status;
  if is_overall_success tests_with_status then exit_success else exit_failure

let print_short_status ~always_show_unchecked_output tests_with_status =
  let tests_with_status = List.filter is_important_status tests_with_status in
  match tests_with_status with
  | [] -> ()
  | _ ->
      print_statuses ~highlight_test:true ~always_show_unchecked_output
        tests_with_status

let print_long_status ~always_show_unchecked_output tests_with_status =
  match tests_with_status with
  | [] -> ()
  | _ ->
      print_statuses ~highlight_test:false ~always_show_unchecked_output
        tests_with_status

let report_dead_snapshots all_tests =
  let dead_snapshots = Store.find_dead_snapshots all_tests in
  let n = List.length dead_snapshots in
  if n > 0 then (
    printf
      "%i folder%s no longer belong%s to the test suite and can be removed:\n" n
      (if_plural n "s") (if_singular n "s");
    List.iter
      (fun (x : Store.dead_snapshot) ->
        let msg =
          match x.test_name with
          | None -> "??"
          | Some name -> name
        in
        printf "  %s %s\n" !!(x.dir_or_junk_file) msg)
      dead_snapshots)

let print_status_summary tests tests_with_status =
  report_dead_snapshots tests;
  let stats = stats_of_tests tests tests_with_status in
  let overall_success = is_overall_success tests_with_status in
  printf "%i/%i selected test%s:\n" stats.selected_tests stats.total_tests
    (if_plural stats.total_tests "s");
  if !(stats.skipped_tests) > 0 then
    printf "  %i skipped\n" !(stats.skipped_tests);
  printf "  %i successful (%i pass, %i xfail)\n"
    (!(stats.pass) + !(stats.xfail))
    !(stats.pass) !(stats.xfail);
  printf "  %i unsuccessful (%i fail, %i xpass)\n"
    (!(stats.fail) + !(stats.xpass))
    !(stats.fail) !(stats.xpass);
  if !(stats.miss) > 0 then
    printf "%i new test%s\n" !(stats.miss) (if_plural !(stats.miss) "s");
  if !(stats.needs_approval) > 0 then
    printf "%i test%s whose output needs first-time approval\n"
      !(stats.needs_approval)
      (if_plural !(stats.needs_approval) "s");
  printf "overall status: %s\n"
    (if overall_success then Style.color Green "success"
     else Style.color Red "failure");
  if overall_success then exit_success else exit_failure

let print_all_statuses ~always_show_unchecked_output tests tests_with_status =
  print_status_introduction ();
  print_newline ();
  print_long_status ~always_show_unchecked_output tests_with_status;
  print_newline ();
  print_short_status ~always_show_unchecked_output tests_with_status;
  print_status_summary tests tests_with_status

let print_important_statuses ~always_show_unchecked_output tests
    tests_with_status =
  (* Print details about each test that needs attention *)
  print_short_status ~always_show_unchecked_output tests_with_status;
  print_status_summary tests tests_with_status

let get_test_with_status test =
  let status = Store.get_status test in
  (test, status, Store.status_summary_of_status status)

let get_tests_with_status tests = tests |> Helpers.list_map get_test_with_status

(*
   Entry point for the 'status' subcommand
*)
let cmd_status ~always_show_unchecked_output ~filter_by_substring ~filter_by_tag
    ~output_style tests =
  check_test_definitions tests;
  let selected_tests = filter ~filter_by_substring ~filter_by_tag tests in
  let tests_with_status = get_tests_with_status selected_tests in
  let exit_code =
    match output_style with
    | Long_all ->
        print_all_statuses ~always_show_unchecked_output tests tests_with_status
    | Long_important ->
        print_important_statuses ~always_show_unchecked_output tests
          tests_with_status
    | Compact_all -> print_compact_status tests_with_status
    | Compact_important ->
        print_compact_status ~important:true tests_with_status
  in
  (exit_code, tests_with_status)

let start_test worker test =
  printf "%s%s\n%!"
    (Style.left_col (Style.color Yellow "[RUN]"))
    (format_title test);
  Worker.Client.write worker (Start_test test.id)

let feed_worker test_queue worker =
  match Queue.take_opt test_queue with
  | Some test -> start_test worker test
  | None -> Worker.Client.close_worker worker

let report_skip_test test reason =
  printf "%s%s\n%!"
    (Style.left_col (Style.color Yellow (sprintf "[SKIP: %s] " reason)))
    (format_title test)

let run_tests_in_workers ~always_show_unchecked_output ~argv ~num_workers
    ~test_list_checksum tests =
  let workers =
    Worker.Client.create ~num_workers ~original_argv:argv ~test_list_checksum
  in
  let get_test =
    let tbl = Hashtbl.create (2 * List.length tests) in
    List.iter (fun (x : T.test) -> Hashtbl.add tbl x.id x) tests;
    fun test_id ->
      try Hashtbl.find tbl test_id with
      | Not_found ->
          failwith
            (sprintf "Internal error: received invalid test ID from worker: %S"
               test_id)
  in
  let test_queue = tests |> List.to_seq |> Queue.of_seq in
  (* Send a first task (test) to each worker *)
  Worker.Client.iter_workers workers (fun worker ->
      feed_worker test_queue worker);
  (* Wait for responses from workers and feed them until the queue is empty *)
  let rec loop () =
    match Worker.Client.read workers with
    | None ->
        (* There are no more workers = they were closed after we
           tried to feed each of them from the empty queue. *)
        ()
    | Some (worker, msg) ->
        let worker_id = Worker.Client.worker_id worker in
        (match msg with
        | End_test test_id ->
            let test = get_test test_id in
            (match test.skipped with
            | Some reason -> report_skip_test test reason
            | None ->
                get_test_with_status test
                |> print_status ~highlight_test:false
                     ~always_show_unchecked_output);
            feed_worker test_queue worker
        | Error msg ->
            eprintf "*** Internal error in worker %s: %s\n%!" worker_id msg;
            Worker.Client.close workers;
            exit exit_internal_error
        | Junk line -> printf "[worker %s] %s\n%!" worker_id line);
        loop ()
  in
  loop ()

let report_start_test test =
  printf "%s%s\n%!"
    (Style.left_col (Style.color Yellow "[RUN]"))
    (format_title test)

let report_end_test ~always_show_unchecked_output test =
  get_test_with_status test
  |> print_status ~highlight_test:false ~always_show_unchecked_output

(*
   Run tests and report progress. This is done in the main process when
   running without worker processes ('-j 0' option).
*)
let run_tests_sequentially ~always_show_unchecked_output (tests : T.test list) :
    'unit_promise =
  List.fold_left
    (fun previous (test : T.test) ->
      let test_func : unit -> 'unit_promise =
        wrap_test_function ~with_storage:true ~flip_xfail_outcome:false test
          test.func
      in
      previous >>= fun () ->
      match test.skipped with
      | Some reason ->
          report_skip_test test reason;
          P.return ()
      | None ->
          report_start_test test;
          P.catch test_func (fun _exn _trace -> P.return ()) >>= fun () ->
          report_end_test ~always_show_unchecked_output test;
          P.return ())
    (P.return ()) tests

let run_tests_requested_by_master (tests : T.test list) : 'unit_promise =
  let get_test =
    let tbl = Hashtbl.create (2 * List.length tests) in
    List.iter (fun (test : T.test) -> Hashtbl.add tbl test.id test) tests;
    fun test_id ->
      try Hashtbl.find tbl test_id with
      | Not_found ->
          failwith (sprintf "Invalid test ID received by worker: %S" test_id)
  in
  let rec loop previous =
    previous >>= fun () ->
    match Worker.Server.read () with
    | None -> exit 0
    | Some (Start_test test_id) ->
        let test = get_test test_id in
        let test_func : unit -> 'unit_promise =
          wrap_test_function ~with_storage:true ~flip_xfail_outcome:false test
            test.func
        in
        let job =
          match test.skipped with
          | Some _reason ->
              (* This shouldn't happen but it's not a problem *)
              Worker.Server.write (End_test test_id);
              P.return ()
          | None ->
              P.catch test_func (fun _exn _trace -> P.return ()) >>= fun () ->
              Worker.Server.write (End_test test_id);
              P.return ()
        in
        loop job
  in
  loop (P.return ())

(* Select tests to run. This runs in both master and workers but results
   in a shorter list of tests in workers. *)
let select_tests ~filter_by_substring ~filter_by_tag ~lazy_ ~slice tests =
  let tests =
    match lazy_ with
    | false -> tests
    | true ->
        (* Read the status of each test so we can skip them *)
        let tests_with_status = get_tests_with_status tests in
        List.filter is_important_status tests_with_status
        |> Helpers.list_map (fun (test, _, _) -> test)
  in
  filter ~filter_by_substring ~filter_by_tag tests |> Slice.apply_slices slice

(* This runs in the master process before a run or Lwt run
   and returns the list of selected tests that will be dispatched to
   the workers. *)
let before_run ~filter_by_substring ~filter_by_tag ~lazy_ ~slice tests =
  Store.init_workspace ();
  check_test_definitions tests;
  let selected_tests =
    select_tests ~filter_by_substring ~filter_by_tag ~lazy_ ~slice tests
  in
  print_status_introduction ();
  selected_tests

(* Run this after a run or Lwt run. *)
let after_run ~always_show_unchecked_output tests selected_tests =
  let tests_with_status = get_tests_with_status selected_tests in
  let exit_code =
    (* Print details about each test that needs attention *)
    print_short_status ~always_show_unchecked_output tests_with_status;
    (* Print one line per test that needs attention *)
    print_compact_status ~important:true tests_with_status |> ignore;
    print_status_summary tests tests_with_status
  in
  (exit_code, tests_with_status)

(*
   Entry point for the 'run' subcommand

   It splits into master and worker. Options that affect formatting
   are for the master. Options that affect test execution, if any, are
   for the workers. Options that select which tests should run are used by
   both the master and the workers (because the master doesn't pass the
   list of selected tests to the workers).
*)
let cmd_run ~always_show_unchecked_output ~argv ~filter_by_substring
    ~filter_by_tag ~is_worker ~jobs ~lazy_ ~slice
    ~test_list_checksum:expected_checksum tests cont =
  if is_worker then (
    (*
        The checksum is computed on the list of tests before applying
        the --slice options since a --slice option is added to the
        worker's command line.
    *)
    check_checksum_or_abort ~expected_checksum tests;
    let selected_tests =
      select_tests ~filter_by_substring ~filter_by_tag ~lazy_ ~slice tests
    in
    run_tests_requested_by_master selected_tests >>= fun () -> exit 0)
  else
    let num_workers =
      match jobs with
      | Some n -> max 0 n
      | None -> (
          match CPU.get_count () with
          | None -> 0
          | Some n -> n)
    in
    let selected_tests =
      before_run ~filter_by_substring ~filter_by_tag ~lazy_ ~slice tests
    in
    (match num_workers with
    | 0 ->
        (* Run in the same process. This is for situations or platforms where
           multiprocessing might not work for whatever reason. *)
        run_tests_sequentially ~always_show_unchecked_output selected_tests
    | _ ->
        run_tests_in_workers ~always_show_unchecked_output ~argv ~num_workers
          ~test_list_checksum:(get_checksum tests) selected_tests;
        P.return ())
    >>= fun () ->
    let exit_code, tests_with_status =
      after_run ~always_show_unchecked_output tests selected_tests
    in
    cont exit_code tests_with_status |> ignore;
    (* The continuation 'cont' should exit but otherwise we exit once
       it's done. *)
    exit exit_code

(*
   Entry point for the 'approve' subcommand
*)
let cmd_approve ~filter_by_substring ~filter_by_tag tests =
  Store.init_workspace ();
  check_test_definitions tests;
  tests
  |> filter ~filter_by_substring ~filter_by_tag
  |> Helpers.list_map Store.approve_new_output
  |> print_errors
