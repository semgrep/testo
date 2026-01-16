(*
   Test suite exercising a variety of test options.
*)

open Printf

let ( !! ) = Fpath.to_string
let ( / ) = Fpath.( / )
let t = Testo.create
let testing_tag = Testo.Tag.declare "testing"
let tags_tag = Testo.Tag.declare "tags"

(* Tag for tests that are designed to be run by the meta-test program
   or that don't succeed without special flags or preparation.
   We use this exclude this tag to exclude these special tests when running
   the simple test suite normally:
     ./test -t 'not meta'
*)
let meta_tag = Testo.Tag.declare "meta"

let fruit_tests =
  Testo.categorize "fruit" [ t "apple" (fun () -> ()); t "kiwi" (fun () -> ()) ]

let animal_tests = Testo.categorize "animal" [ t "banana slug" (fun () -> ()) ]

let categorized =
  Testo.categorize_suites "biohazard" [ fruit_tests; animal_tests ]

(* Tests that we don't run on Windows due to a different behavior:
   CRLF is translated to LF when reading a text file on Windows but
   not when reading the same file that wasn't translated when moved to Unix,
   where ideally such files should not exist.

   These tests are automatically skipped on Windows and we provide a
   'unix' tag to deselect these tests when running the meta test suite
   so that we run the same tests and get the same output on all platforms.
*)
let skipped_on_windows =
  if Sys.win32 then Some "This test does not make sense on Windows" else None

let unix_tag = Testo.Tag.declare "unix_only"

(*
   Test the 'validate_name' function used by 'checked_output_file'.
*)
let test_checked_output_file_names () =
  let invalid_names =
    [
      "";
      ".";
      "..";
      "a/b";
      "a\\b";
      "a:b";
      " ";
      "a b";
      "a\nb";
      "a\n";
      "\na";
      "+";
      "a+b";
      ".a";
      "a.";
    ]
  in
  let valid_names =
    [
      "a";
      "A";
      "aaa";
      "AAA";
      "_";
      "___";
      "-";
      "---";
      "a-b";
      "a.b";
      "3";
      "333";
      "ab.cd.ef";
      "a..b";
    ]
  in
  List.iter
    (fun name ->
      printf "Check that name is invalid: %S\n%!" name;
      try
        ignore (Testo.checked_output_file name);
        Testo.fail "failed to raise Invalid_argument"
      with
      | Invalid_argument _ -> ())
    invalid_names;
  List.iter
    (fun name ->
      printf "Check that name is valid: %S\n%!" name;
      ignore (Testo.checked_output_file name))
    valid_names

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
    t "check for name file in previous tests" ~tags:[ meta_tag ] ~category
      (fun () ->
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
    Testo.(check string) ~msg:__LOC__ expected_result res
  in
  (* re.1.12.0 requires ocaml >= 4.12 but we want to support older versions
     of ocaml so we're not sure of the behavior of the re library here.
     The relevant fix in re is https://github.com/ocaml/ocaml-re/pull/233
  *)
  let _tests_requiring_re_1_12 =
    [
      ("", "", "<MASKED>", None);
      ("", "aa", "XaXaX", Some "X");
      ({|\b|}, "word", "XwordX", Some "X");
      ("", "aa", "<MASKED>a<MASKED>a<MASKED>", None);
    ]
  in
  (* These tests require re 1.10 or 1.11: *)
  let _tests_requiring_re_1_10 =
    [
      ("", "", "", None);
      ("", "aa", "XaXa", Some "X");
      ({|\b|}, "word", "Xword", Some "X");
      ("", "aa", "<MASKED>a<MASKED>a", None);
    ]
  in
  [
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
    Testo.(check string) ~msg:__LOC__ expected_result res
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
    Testo.(check string)
      ~msg:(sprintf "temp dir %S" temp_dir)
      expected_masked_data masked_data
  in
  ([
     ("/", "/", "<TMP>");
     (* Fpath normalizes multiple leading slashes into "//" rather than "/".
       I don't know why or whether it matters in practice. *)
     ("/", "/a", "<TMP>a");
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
  @
  if Sys.win32 then []
  else
    (* [Fpath.v "////"] fails with `Exception: Invalid_argument "\"////\": invalid path".`
         on windows, but it *is* a valid path on POSIX systems. *)
    [
      (* Fpath normalizes multiple leading slashes into "//" rather than "/".
           I don't know why or whether it matters in practice. *)
      ("////", "//", "<TMP>");
      ("////", "//a", "<TMP>a");
    ])
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
  Testo.(check string) "A\nE\n\nFF\r\nGG\nH" res

let test_remove_matching_lines () =
  let res =
    Testo.remove_matching_lines
      (Testo.contains_substring ~sub:"DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Testo.(check string) "hello\n" res;
  let res =
    Testo.remove_matching_lines
      (Testo.contains_pcre_pattern ~pat:"^DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Testo.(check string) "xxxDEBUGxxx\nhello\n" res

let test_keep_matching_lines () =
  let res =
    Testo.keep_matching_lines
      (Testo.contains_substring ~sub:"DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Testo.(check string) "DEBUG\nxxxDEBUGxxx\n" res;
  let res =
    Testo.keep_matching_lines
      (Testo.contains_pcre_pattern ~pat:"^DEBUG")
      "DEBUG\nxxxDEBUGxxx\nhello\n"
  in
  Testo.(check string) "DEBUG\n" res

let test_error_formatting () =
  printf "This check is expected to fail with nice error formatting.\n%!";
  let exn =
    try
      Testo.(check string) ~msg:"<description>" "a" "b";
      assert false
    with
    | e -> e
  in
  eprintf "%s%!" (Printexc.to_string exn)

let test_error_formatting_multiline ~as_text () =
  printf "This check is expected to fail with nice error formatting.\n%!";
  let exn =
    try
      Testo.(check (if as_text then text else string))
        ~msg:"<description>" "The quick brown\nfox jumps over\nthe lazy dog.\n"
        "The quick red\nfox jumps over\nthe lazy dog.\n";
      assert false
    with
    | e -> e
  in
  eprintf "%s%!" (Printexc.to_string exn)

let test_error_formatting_text () =
  test_error_formatting_multiline ~as_text:true ()

let test_error_formatting_multiline_quoted_string () =
  test_error_formatting_multiline ~as_text:false ()

let test_error_formatting_short_list () =
  printf "This check is expected to fail with nice error formatting.\n%!";
  let exn =
    try
      Testo.(check (list (tuple2 string int)))
        [ ("a", 1); ("b", 2) ]
        [ ("a", 1); ("b", 2); ("c", 3) ];
      assert false
    with
    | e -> e
  in
  eprintf "%s%!" (Printexc.to_string exn)

let test_error_formatting_long_list () =
  printf "This check is expected to fail with nice error formatting.\n%!";
  let exn =
    try
      Testo.(check (list (list string)))
        [
          [
            "The";
            "quick";
            "brown";
            "fox";
            "jumps";
            "over";
            "the";
            "lazy";
            "dog.";
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          ];
        ]
        [
          [ "The"; "quick"; "brown"; "fox"; "jumps"; "over" ];
          [ "the"; "lazy"; "dog."; "aaaaaaaaaaaaaaaa\naaaaaaaaaaaaaaaaaa" ];
        ];
      assert false
    with
    | e -> e
  in
  eprintf "%s%!" (Printexc.to_string exn)

let test_error_formatting_long_tuple () =
  printf "This check is expected to fail with nice error formatting.\n%!";
  let exn =
    try
      Testo.(check (tuple5 string int float bool bool))
        ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
          12345,
          -9.87e6,
          true,
          false )
        ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaXXXX",
          12345,
          -9.87e6,
          true,
          false );
      assert false
    with
    | e -> e
  in
  eprintf "%s%!" (Printexc.to_string exn)

let test_temporary_files () =
  let path_ref = ref None in
  try
    Testo.with_temp_text_file ~contents:"hello" (fun path ->
        path_ref := Some path;
        let data = Testo.read_text_file path in
        Testo.(check string) ~msg:"file contents" "hello" data;
        Testo.write_text_file path "";
        let data = Testo.read_text_file path in
        Testo.(check string) ~msg:"empty file contents" "" data;
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
  Testo.(check string) ~msg:"captured stdout" "hello" capture;
  Testo.(check int) 42 result;
  (* capture to snapshot file *)
  print_string "snapshot data\n";
  (* more capture from the same channel *)
  let (), capture2 = Testo.with_capture stdout (fun () -> print_string "wow") in
  Testo.(check string) ~msg:"more captured stdout" "wow" capture2

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
let test_diff ?skipped ?tags name =
  let dir = Fpath.v "tests/diff-data/" in
  let func () =
    Testo.with_chdir dir (fun () ->
        match
          Testo_util.Diff.files
            (Fpath.v (name ^ ".left"))
            (Fpath.v (name ^ ".right"))
        with
        | None -> assert false
        | Some diff_text -> print_string diff_text)
  in
  Testo.create name func ?skipped ?tags ~category:[ "diff" ]
    ~checked_output:
      (Testo.stdout ~expected_stdout_path:(dir / (name ^ ".diff")) ())

let test_tag_selector =
  let _tag_a = Testo.Tag.declare "a" in
  let _tag_b = Testo.Tag.declare "b" in
  let _tag_c = Testo.Tag.declare "c" in
  fun query_str tags expected_match ->
    let query =
      match Testo_util.Tag_query.parse query_str with
      | Ok x -> x
      | Error msg -> Testo.fail msg
    in
    let tags = List.map Testo_util.Tag.of_string_exn tags in
    let res = Testo_util.Tag_query.match_ tags query in
    Testo.(check bool) expected_match res

let tag_selection_tests =
  [
    ("simple in", "a", [ "a" ], true);
    ("simple out", "a", [ "b" ], false);
    ("complex in", "(a or b) and not c", [ "a" ], true);
    ("complex out", "(a or b) and not c", [ "a"; "c" ], false);
    ("not", "not c", [ "a"; "b" ], true);
    ("all", "all", [ "a"; "b" ], true);
    ("none", "none", [ "a"; "b" ], false);
  ]
  |> List.map (fun (name, query, tags, expect) ->
         Testo.create name (fun () -> test_tag_selector query tags expect))
  |> Testo.categorize "tag selection"

let test_write_read_map () =
  let contents = "hello" in
  Testo.with_temp_text_file (fun src_path ->
      Testo.write_text_file src_path contents;
      Testo.with_temp_text_file (fun dst_path ->
          let contents2 = Testo.read_text_file src_path in
          Testo.(check string) ~msg:"read" contents contents2;
          let new_contents = "new" in
          Testo.map_text_file
            (fun contents3 ->
              Testo.(check string) ~msg:"map_file input" contents contents3;
              new_contents)
            src_path dst_path;
          let new_contents2 = Testo.read_text_file dst_path in
          Testo.(check string) ~msg:"map_file output" new_contents new_contents2))

let test_write_read_map_in_place () =
  let contents = "hello" in
  Testo.with_temp_text_file (fun path ->
      Testo.write_text_file path contents;
      let contents2 = Testo.read_text_file path in
      Testo.(check string) ~msg:"read" contents contents2;
      let new_contents = "new" in
      Testo.map_text_file
        (fun contents3 ->
          Testo.(check string) ~msg:"map_file input" contents contents3;
          new_contents)
        path path;
      let new_contents2 = Testo.read_text_file path in
      Testo.(check string) ~msg:"map_file output" new_contents new_contents2)

let shared_context_merged_output =
  (* get_count is used to check that the lazy block is evaluated only once *)
  let counter = ref 0 in
  let get_count () =
    incr counter;
    !counter
  in
  Testo.Lazy_with_output.create ~redirect:Stderr_to_stdout (fun () ->
      printf "stdout a\n%!";
      eprintf "stderr b\n%!";
      printf "stdout c\n%!";
      get_count ())

(* This test function will run twice in the same process *)
let test_shared_context_merged_output test_name =
  Testo.create ~solo:"run in same process so as to share context" test_name
    (fun () ->
      let (res, out), err =
        Testo.with_capture stderr (fun () ->
            Testo.with_capture stdout (fun () ->
                Testo.Lazy_with_output.force shared_context_merged_output))
      in
      Testo.(check int) 1 res;
      Testo.(check string) ~msg:"error output" "" err;
      Testo.(check string)
        ~msg:"standard output" "stdout a\nstderr b\nstdout c\n" out)

let shared_context_split_output =
  Testo.Lazy_with_output.create (fun () ->
      printf "stdout a\n%!";
      eprintf "stderr b\n%!";
      printf "stdout c\n%!")

(* This test function will run twice in the same process *)
let test_shared_context_split_output test_name =
  Testo.create ~solo:"run in same process so as to share context" test_name
    (fun () ->
      let ((), out), err =
        Testo.with_capture stderr (fun () ->
            Testo.with_capture stdout (fun () ->
                Testo.Lazy_with_output.force shared_context_split_output))
      in
      Testo.(check string) ~msg:"error output" "stderr b\n" err;
      Testo.(check string) ~msg:"standard output" "stdout a\nstdout c\n" out)

let shared_context_tests =
  [
    test_shared_context_merged_output "shared context merged output 1";
    test_shared_context_merged_output "shared context merged output 2";
    test_shared_context_split_output "shared context split output 1";
    test_shared_context_split_output "shared context split output 2";
  ]

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
    t "checked output file names" test_checked_output_file_names;
    t "unchecked stdout" (fun () -> print_endline "hello\nworld");
    t "unchecked stderr" (fun () -> prerr_string "hello\n");
    t "capture stdout" ~tags:[ meta_tag ] ~category:[ "auto-approve" ]
      ~checked_output:(Testo.stdout ()) (fun () -> print_string "hello\n");
    t "capture stderr" ~tags:[ meta_tag ] ~category:[ "auto-approve" ]
      ~checked_output:(Testo.stderr ()) (fun () -> prerr_string "error\n");
    t "capture stdxxx" ~tags:[ meta_tag ] ~category:[ "auto-approve" ]
      ~checked_output:(Testo.stdxxx ()) (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr" ~tags:[ meta_tag ]
      ~category:[ "auto-approve" ]
      ~checked_output:(Testo.split_stdout_stderr ()) (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "capture stdout in custom location" ~tags:[ meta_tag ]
      ~category:[ "auto-approve" ]
      ~checked_output:
        (Testo.stdout
           ~expected_stdout_path:(Fpath.v "tests/custom-snapshots/my-stdout")
           ())
      (fun () -> print_string "hello\n");
    t "capture stderr in custom location" ~tags:[ meta_tag ]
      ~category:[ "auto-approve" ]
      ~checked_output:
        (Testo.stderr
           ~expected_stderr_path:(Fpath.v "tests/custom-snapshots/my-stderr")
           ())
      (fun () -> prerr_string "error\n");
    t "capture stdxxx in custom location" ~tags:[ meta_tag ]
      ~category:[ "auto-approve" ]
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
    t "capture stdout and stderr in custom location" ~tags:[ meta_tag ]
      ~category:[ "auto-approve" ]
      ~checked_output:
        (Testo.split_stdout_stderr
           ~expected_stdout_path:(Fpath.v "tests/custom-snapshots/split-stdout")
           ~expected_stderr_path:(Fpath.v "tests/custom-snapshots/split-stderr")
           ())
      (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "capture one file"
      ~checked_output_files:[ Testo.checked_output_file "results.txt" ]
      (fun () ->
        Testo.with_temp_dir ~chdir:true (fun _dir ->
            let output_file = Fpath.v "results" in
            Testo.write_text_file output_file "hello world\n";
            Testo.stash_output_file output_file "results.txt"));
    t "fail to capture one file"
      ~expected_outcome:(Should_fail "must raise exception")
      ~checked_output_files:[ Testo.checked_output_file "results.txt" ]
      (fun () -> failwith "I am failing on purpose");
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
    t "show exception on xfail"
      ~expected_outcome:(Should_fail "raises Failure exception on purpose")
      ~inline_logs:On (fun () ->
        failwith "This exception was raised on purpose");
    t "auto show exception on xfail"
      ~expected_outcome:(Should_fail "raises Failure exception on purpose")
      (fun () -> failwith "This exception was raised on purpose");
    t "don't show exception on xfail"
      ~expected_outcome:(Should_fail "raises Failure exception on purpose")
      ~inline_logs:Off (fun () ->
        failwith "This exception was raised on purpose");
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
      ~checked_output:(Testo.stdout ()) (fun () ->
        print_endline "incorrect output printed by the test");
    t "skipped" ~skipped:"some reason" (fun () ->
        failwith "this shouldn't happen");
    t "broken" ~tags:[ meta_tag ] ~broken:"this test is super flaky" (fun () ->
        failwith "I am broken");
    t "tracking URL" ~tracking_url:"https://example.com/issue/1234" (fun () ->
        ());
    t "chdir" ~tolerate_chdir:true (fun () -> Sys.chdir "/");
    t "with environment variables ok" (fun () ->
        Testo.(check (option string)) None (Sys.getenv_opt "B");
        Testo.with_environment_variables
          [ ("A", "a"); ("B", "b"); ("C", "c") ]
          (fun () ->
            Testo.(check (option string)) (Some "a") (Sys.getenv_opt "A");
            Testo.(check (option string)) (Some "b") (Sys.getenv_opt "B");
            Testo.with_environment_variables
              [ ("B", "z") ]
              (fun () ->
                Testo.(check (option string)) (Some "a") (Sys.getenv_opt "A");
                Testo.(check (option string)) (Some "z") (Sys.getenv_opt "B"));
            Testo.(check (option string)) (Some "b") (Sys.getenv_opt "B"));
        (* Check that the env var is cleared again outside of the combinator *)
        let expected_unset_B =
          (* On Windows, [Sys.getenv_opt v = None] for empty variables *)
          if Sys.win32 then None else Some ""
        in
        Testo.(check (option string)) expected_unset_B (Sys.getenv_opt "B"));
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
    t "mask_temp_paths" test_mask_temp_paths;
    t "mask exotic temp paths" test_mask_exotic_temp_paths;
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
    t "error formatting" ~checked_output:(Testo.stderr ()) test_error_formatting;
    t "error formatting text" ~checked_output:(Testo.stderr ())
      test_error_formatting_text;
    t "error formatting multiline quoted string"
      ~checked_output:(Testo.stderr ())
      test_error_formatting_multiline_quoted_string;
    t "error formatting short list" ~checked_output:(Testo.stderr ())
      test_error_formatting_short_list;
    t "error formatting long list" ~checked_output:(Testo.stderr ())
      test_error_formatting_long_list;
    t "error formatting long tuple" ~checked_output:(Testo.stderr ())
      test_error_formatting_long_tuple;
    t "temporary files" test_temporary_files;
    t "user output capture" ~checked_output:(Testo.stdout ())
      test_user_output_capture;
    t "require '--env foo=bar'" ~tags:[ meta_tag ] (fun () ->
        match List.assoc_opt "foo" env with
        | None -> Testo.fail "Missing option: -e foo=bar"
        | Some "bar" -> ()
        | Some other ->
            Testo.fail (sprintf "Invalid value for variable foo: %S" other));
    t "solo 1/2" (fun () -> Unix.sleepf 0.02) ~solo:"testing";
    t "solo 2/2" (fun () -> Unix.sleepf 0.02) ~solo:"testing";
    test_diff "hello";
    test_diff "six-lines";
    test_diff "leading-context";
    test_diff "leading-context-short";
    test_diff "trailing-context";
    test_diff "joined-context";
    test_diff "gap-in-context";
    (* These tests are about reading files with CRLF on Unix where they
       don't get translated into LFs during reads like on Windows *)
    test_diff ?skipped:skipped_on_windows ~tags:[ unix_tag ] "lf-crlf-only";
    test_diff ?skipped:skipped_on_windows ~tags:[ unix_tag ] "crlf-lf-only";
    test_diff ?skipped:skipped_on_windows ~tags:[ unix_tag ] "lf-crlf";
    test_diff ?skipped:skipped_on_windows ~tags:[ unix_tag ] "crlf-lf";
    test_diff "missing-eol-only";
    test_diff "missing-eol";
    t "current test" (fun () ->
        match Testo.get_current_test () with
        | None -> Testo.fail "current test is unset"
        | Some test ->
            Testo.(check string) ~msg:"test name" "current test" test.name);
    t "write/read/map file" test_write_read_map;
    t "write/read/map file in place" test_write_read_map_in_place;
    t "string list fail if DEMO is set" (fun () ->
        match Sys.getenv_opt "DEMO" with
        | None -> ()
        | Some _ ->
            Testo.(check (list string))
              [
                "The quick brown fox jumps over the lazy dog.";
                "Dude, where's my car?";
                "Sir, this is a Wendy's";
                "You get a car!\n\
                 You get a car!\n\
                 You get a car!\n\
                 Everybody gets a car!";
                "Electrolytes. That's what plants crave.";
              ]
              [
                "The quick brown fox jumps over the lazy dog.";
                "Dude, where's my car?";
                "Sir, this is a Wendy's";
                "You get a car!\n\
                 You get a car.\n\
                 You get a car!\n\
                 Everybody gets a car!";
                "Electrolytes. That's what plants crave.";
              ]);
    t "text fail if DEMO is set" (fun () ->
        match Sys.getenv_opt "DEMO" with
        | None -> ()
        | Some _ ->
            Testo.(check text)
              "The quick brown fox jumps over the lazy dog.\n\
               Dude, where's my car?\n\
               Sir, this is a Wendy's.\n\
               Electrolytes. That's what plants crave.\n"
              "The quick brown fox jumps over the lazy dog.\n\
               Dude, where's my car?\n\
               Sir, this is a Wendy's.\r\n\
               Electrolytes. That's what plants crave.\n");
  ]
  @ categorized @ test_internal_files
  @ Testo.categorize "Slice"
      (List.map
         (fun (name, func) -> Testo.create name func)
         Testo_util.Slice.tests)
  @ tag_selection_tests @ shared_context_tests

let () =
  Testo.interpret_argv ~project_name:"testo_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      exit exit_code)
    tests
