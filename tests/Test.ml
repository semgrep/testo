(*
   Test suite exercising a variety of test options.
*)

open Printf

let ( !! ) = Fpath.to_string
let ( / ) = Fpath.( / )

(* We should consider a shorter name for this library. *)
let t = Testo.create
let testing_tag = Testo.Tag.declare "testing"
let tags_tag = Testo.Tag.declare "tags"

let fruit_tests =
  Testo.categorize "fruit" [ t "apple" (fun () -> ()); t "kiwi" (fun () -> ()) ]

let animal_tests = Testo.categorize "animal" [ t "banana slug" (fun () -> ()) ]

let categorized =
  Testo.categorize_suites "biohazard" [ fruit_tests; animal_tests ]

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
    Fpath.v "tests/snapshots/testo_tests" / test.id
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

let test_mask_pcre_pattern () =
  let test_one (pat, subj, expected_result, mask) =
    printf "pat=%S subj=%S expected_result=%S mask=%s\n" pat subj
      expected_result
      (match mask with
      | None -> "default"
      | Some x -> sprintf "%S" x);
    let replace = Option.map (fun mask _ -> mask) mask in
    let res = Testo.mask_pcre_pattern ?replace pat subj in
    Alcotest.(check string) __LOC__ expected_result res
  in
  [
    ("", "", "", None);
    ("", "aa", "XaXa", Some "X");
    ("", "aa", "<MASKED>a<MASKED>a", None);
    ("a", "aa", "XX", Some "X");
    ("a", "a,a", "X,X", Some "X");
    ("a+", "xxaxxxaaaxxa", "xxAxxxAxxA", Some "A");
    ("<(a+)>", "xxaxxx<aaa>xx<a>", "xxaxxx<A>xx<A>", Some "A");
    ("<a+>", "xxaxxx<aaa>xx<a>", "xxaxxxAxxA", Some "A");
  ]
  |> List.iter test_one

(* TODO: test Windows paths *)
let test_mask_temp_paths () =
  let temp_dir = Fpath.v "/tmp" in
  let includes_temp_dir = "/var/tmp/1234" in
  let test_one (depth, input_str, expected_result) =
    printf "depth=%S input_str=%S expected_result=%S\n%!"
      (match depth with
      | Some (Some n) -> string_of_int n
      | Some None -> "no limit"
      | None -> "default")
      input_str expected_result;
    let res = Testo.mask_temp_paths ?depth ~temp_dir () input_str in
    Alcotest.(check string) __LOC__ expected_result res
  in
  let tmp rel_path = sprintf "%s/%s" !!temp_dir rel_path in
  [
    (None, !!temp_dir, "<TMP>");
    (None, tmp "", "<TMP>/");
    (None, tmp "a", "<TMP>/<MASKED>");
    (None, tmp "a/", "<TMP>/<MASKED>/");
    (None, tmp "a/b", "<TMP>/<MASKED>/b");
    (None, tmp "a//b", "<TMP>/<MASKED>//b");
    (None, tmp "a-b_12/bcccc", "<TMP>/<MASKED>/bcccc");
    (None, " " ^ tmp "a-b_12//bcccc/// ", " <TMP>/<MASKED>//bcccc/// ");
    (None, tmp "a" ^ " " ^ tmp "b", "<TMP>/<MASKED> <TMP>/<MASKED>");
    (* don't mask sub-paths *)
    (None, includes_temp_dir, includes_temp_dir);
    (* no depth limit *)
    (Some None, !!temp_dir, "<TMP>");
    (Some None, tmp "", "<TMP>/");
    ( Some None,
      tmp "a/b/c/d/e",
      "<TMP>/<MASKED>/<MASKED>/<MASKED>/<MASKED>/<MASKED>" );
    (* default: None <=> Some (Some 1) *)
    (Some (Some 1), !!temp_dir, "<TMP>");
    (Some (Some 1), tmp "", "<TMP>/");
    (Some (Some 1), tmp "a/b", "<TMP>/<MASKED>/b");
    (* mask only /tmp or /tmp/ *)
    (Some (Some 0), !!temp_dir, "<TMP>");
    (Some (Some 0), tmp "", "<TMP>/");
    (Some (Some 0), tmp "a/b", "<TMP>/a/b");
    (* mask up to 3 segments after /tmp *)
    (Some (Some 3), tmp "a/b/c/d", "<TMP>/<MASKED>/<MASKED>/<MASKED>/d");
    (Some (Some 3), tmp "a/b/c", "<TMP>/<MASKED>/<MASKED>/<MASKED>");
    (Some (Some 3), tmp "a/b", "<TMP>/<MASKED>/<MASKED>");
  ]
  |> List.iter test_one

(* TODO: test Windows paths *)
let test_mask_exotic_temp_paths () =
  let test_one (temp_dir, data, expected_masked_data) =
    let masked_data =
      Testo.mask_temp_paths ~temp_dir:(Fpath.v temp_dir) ~depth:(Some 0) () data
    in
    Alcotest.(check string)
      (sprintf "temp dir %S" temp_dir)
      expected_masked_data masked_data
  in
  [
    ("/", "/", "<TMP>");
    (* Fpath normalizes multiple leading slashes into "//" rather than "/".
       I don't know why or whether it matters in practice. *)
    ("////", "//", "<TMP>");
    ("/", "/a", "<TMP>a");
    ("////", "//a", "<TMP>a");
    ("a", "a", "<TMP>");
    ("a/", "a", "<TMP>");
    ("a////", "a", "<TMP>");
    ("a////", "a/", "<TMP>/");
    ("a////", "a/b", "<TMP>/b");
    ("/a", "/a", "<TMP>");
    ("/a/", "/a", "<TMP>");
    ("/a////", "/a", "<TMP>");
    ("/a////", "/a/", "<TMP>/");
  ]
  |> List.iter test_one

let test_contains_substring () =
  assert (Testo.contains_substring ~sub:"abc" "xxxabcxxx");
  assert (Testo.contains_substring ~sub:"a\nb" "xxxa\nbxxx");
  assert (not (Testo.contains_substring ~sub:"abc" "xxxabxxx"))

let test_contains_pcre_pattern () =
  assert (Testo.contains_pcre_pattern ~pat:"b" "abc");
  assert (Testo.contains_pcre_pattern ~pat:"[a-z]" "--x--");
  assert (not (Testo.contains_pcre_pattern ~pat:"[a-z]" "----"))

let test_filter_map_lines () =
  let res =
    "a\nbc\nd\r\ne\n\nff\r\ngg\nh"
    |> Testo.filter_map_lines (fun line ->
           match line with
           | "bc" -> None
           | "d" -> None
           | other -> Some (String.uppercase_ascii other))
  in
  Alcotest.(check string) "equal" "A\nE\n\nFF\r\nGG\nH" res

let test_remove_matching_lines () =
  let res =
    Testo.remove_matching_lines
      (Testo.contains_substring ~sub:"DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Alcotest.(check string) "equal" "hello\n" res;
  let res =
    Testo.remove_matching_lines
      (Testo.contains_pcre_pattern ~pat:"^DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Alcotest.(check string) "equal" "xxxDEBUGxxx\nhello\n" res

let test_keep_matching_lines () =
  let res =
    Testo.keep_matching_lines
      (Testo.contains_substring ~sub:"DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Alcotest.(check string) "equal" "DEBUG\nxxxDEBUGxxx\n" res;
  let res =
    Testo.keep_matching_lines
      (Testo.contains_pcre_pattern ~pat:"^DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Alcotest.(check string) "equal" "DEBUG\n" res

let test_alcotest_error_formatting () =
  printf
    "This alcotest check is expected to fail with nice error formatting.\n%!";
  let exn =
    try
      Alcotest.(check string) "<description>" "a" "b";
      assert false
    with
    | e -> e
  in
  eprintf "%s%!" (Printexc.to_string exn)

let test_temporary_files () =
  let path_ref = ref None in
  try
    Testo.with_temp_file ~contents:"hello" (fun path ->
        path_ref := Some path;
        let data = Testo.read_file path in
        Alcotest.(check string) "file contents" "hello" data;
        Testo.write_file path "";
        let data = Testo.read_file path in
        Alcotest.(check string) "empty file contents" "" data;
        raise Exit |> ignore);
    assert false
  with
  | Exit -> (
      match !path_ref with
      | None -> assert false
      | Some path -> assert (not (Sys.file_exists (Fpath.to_string path))))

let test_user_output_capture () =
  let result, capture =
    Testo.with_capture stdout (fun () ->
        print_string "hello";
        42)
  in
  Alcotest.(check string) "captured stdout" "hello" capture;
  Alcotest.(check int) "result" 42 result;
  (* capture to snapshot file *)
  print_string "snapshot data\n";
  (* more capture from the same channel *)
  let (), capture2 = Testo.with_capture stdout (fun () -> print_string "wow") in
  Alcotest.(check string) "more captured stdout" "wow" capture2

(*
   Test the formatting for diffs, as implemented in the testo-util library.
   The diff computation itself is tested in testo-diff.

   To test the diff between two files, pick a name and place two files
   '<name>.left' and '<name>.right' into 'tests/diff-data/'.

   Note that we use Testo to produce diffs of Testo diffs with the
   possibility that they're all broken, making such test failures
   particularly confusing. For troubleshooting, it's easier to call
   'bin/testo-diff' on the files in 'tests/diff-data' manually and
   compare with the output of 'diff -u --color'.
*)
let test_diff name =
  let dir = Fpath.v "tests/diff-data/" in
  let func () =
    Testo.with_chdir dir (fun () ->
      let equal, diff_text =
        Testo_util.Diff.files
          (Fpath.v (name ^ ".left"))
          (Fpath.v (name ^ ".right"))
      in
      assert (not equal);
      print_string diff_text
    )
  in
  Testo.create name func ~category:["diff"]
    ~checked_output:(Testo.stdout
                       ~expected_stdout_path:(dir / (name ^ ".diff"))
                       ())

(* For tests that need porting to Windows *)
let is_windows = Sys.os_type = "Win32"

let windows_todo : Testo.expected_outcome =
  if is_windows then Should_fail "this test needs to be ported to Windows"
  else Should_succeed

(*
   The tests marked as "auto-approve" are tests that capture their output
   and will be automatically approved by the meta test suite.
   Normal tests should not be auto-approved since it would defeat the purpose
   of approval.
*)
let tests env =
  print_endline "junk printed on stdout...\n... when creating the test suite";
  [
    t "simple" (fun () -> ());
    t "tags" ~tags:[ testing_tag; tags_tag ] (fun () -> ());
    t "category" ~category:[ "category"; "subcategory" ] (fun () -> ());
    t "unchecked stdout" (fun () -> print_endline "hello\nworld");
    t "unchecked stderr" (fun () -> prerr_string "hello\n");
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
           ~expected_stdout_path:(Fpath.v "tests/custom-snapshots/split-stdout")
           ~expected_stderr_path:(Fpath.v "tests/custom-snapshots/split-stderr")
           ())
      (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
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
    t "xfail" ~expected_outcome:(Should_fail "raises exception on purpose")
      (fun () -> failwith "this exception is expected");
    t "xfail due to invalid output"
      ~expected_outcome:(Should_fail "produces incorrect output on purpose")
      ~checked_output:(Testo.stdout ())
      (fun () -> print_endline "incorrect output printed by the test");
    t "skipped" ~skipped:"some reason" (fun () ->
        failwith "this shouldn't happen");
    t "broken" ~broken:"this test is super flaky" (fun () ->
        failwith "I am broken");
    t "tracking URL" ~tracking_url:"https://example.com/issue/1234" (fun () ->
        ());
    t "chdir" ~tolerate_chdir:true (fun () -> Sys.chdir "/");
    t "with environment variables ok" (fun () ->
        Alcotest.(check (option string)) "equal" None (Sys.getenv_opt "B");
        Testo.with_environment_variables
          [ ("A", "a"); ("B", "b"); ("C", "c") ]
          (fun () ->
            Alcotest.(check (option string))
              "equal" (Some "a") (Sys.getenv_opt "A");
            Alcotest.(check (option string))
              "equal" (Some "b") (Sys.getenv_opt "B");
            Testo.with_environment_variables
              [ ("B", "z") ]
              (fun () ->
                Alcotest.(check (option string))
                  "equal" (Some "a") (Sys.getenv_opt "A");
                Alcotest.(check (option string))
                  "equal" (Some "z") (Sys.getenv_opt "B"));
            Alcotest.(check (option string))
              "equal" (Some "b") (Sys.getenv_opt "B"));
        Alcotest.(check (option string)) "equal" (Some "") (Sys.getenv_opt "B"));
    t "with environment variables bad"
      ~expected_outcome:(Should_fail "fail to restore environment variable")
      (fun () ->
        Testo.with_environment_variables
          [ ("A", "a"); ("B", "b"); ("C", "c") ]
          (fun () ->
            (* change value and don't restore it -> test failure *)
            Unix.putenv "B" "z"));
    t ~checked_output:(Testo.stdout ()) ~normalize:[ String.lowercase_ascii ]
      "masked" (fun () -> print_endline "HELLO");
    t "mask_pcre_pattern" test_mask_pcre_pattern;
    t "mask_temp_paths" ~expected_outcome:windows_todo test_mask_temp_paths;
    t "mask exotic temp paths" ~expected_outcome:windows_todo
      test_mask_exotic_temp_paths;
    t "contains substring" test_contains_substring;
    t "contains pcre pattern" test_contains_pcre_pattern;
    t "filter_map_lines" test_filter_map_lines;
    t "remove matching lines" test_remove_matching_lines;
    t "keep matching lines" test_keep_matching_lines;
    t ~checked_output:(Testo.stdout ())
      ~normalize:[ Testo.mask_not_substring "water" ]
      "check for substring in stdout"
      (fun () ->
        let random_string = string_of_float (Unix.gettimeofday ()) in
        printf "[%s] water is wet.\n" random_string);
    t ~checked_output:(Testo.stdout ())
      ~normalize:[ Testo.mask_not_pcre_pattern "[A-Za-z]+" ]
      "check words in stdout"
      (fun () ->
        let random_string = string_of_float (Unix.gettimeofday ()) in
        printf "[%s] water is wet.\n" random_string);
    t ~checked_output:(Testo.stdout ())
      ~normalize:
        [ Testo.mask_not_substrings [ "a"; "def"; "defgh"; "efghij"; "z" ] ]
      "check for substrings in stdout"
      (fun () -> printf "abcdefghijklmnoprstuvwxyz");
    t "alcotest error formatting" ~checked_output:(Testo.stderr ())
      ~normalize:
        [
          (* In our CI environment for ocaml 4.08, the line
             "File "tests/Test.ml", line ..." is missing, so we mask it if
             it exists. *)
          Testo.mask_pcre_pattern
            "Alcotest assertion failure((?:\nFile [^\n]*)?)\n";
        ]
      test_alcotest_error_formatting;
    t "temporary files" test_temporary_files;
    t "user output capture" ~checked_output:(Testo.stdout ())
      test_user_output_capture;
    t "require '--env foo=bar'" (fun () ->
        match List.assoc_opt "foo" env with
        | None -> Alcotest.fail "Missing option: -e foo=bar"
        | Some "bar" -> ()
        | Some other ->
            Alcotest.fail (sprintf "Invalid value for variable foo: %S" other));
    t "solo 1/2" (fun () -> Unix.sleepf 0.02) ~solo:"testing";
    t "solo 2/2" (fun () -> Unix.sleepf 0.02) ~solo:"testing";
    test_diff "hello";
    test_diff "six-lines";
    test_diff "leading-context";
    test_diff "leading-context-short";
    test_diff "trailing-context";
    test_diff "joined-context";
    test_diff "gap-in-context";
  ]
  @ categorized @ test_internal_files
  @ Testo.categorize "Slice"
      (List.map
         (fun (name, func) -> Testo.create name func)
         Testo_util.Slice.tests)

let () =
  Testo.interpret_argv ~project_name:"testo_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      exit exit_code)
    tests
