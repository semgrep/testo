(*
   Test suite exercising a variety of test options.
*)

open Printf

let ( !! ) = Fpath.to_string

(* We should consider a shorter name for this library. *)
let t = Testo.create
let testing_tag = Testo.Tag.declare "testing"
let tags_tag = Testo.Tag.declare "tags"

let fruit_tests =
  Testo.categorize "fruit" [
    t "apple" (fun () -> ());
    t "kiwi" (fun () -> ());
  ]

let animal_tests =
  Testo.categorize "animal" [ t "banana slug" (fun () -> ()) ]

let categorized =
  Testo.categorize_suites "biohazard" [fruit_tests; animal_tests]

let test_mask_pcre_pattern () =
  let test_one (pat, subj, expected_result, mask) =
    printf "pat=%S subj=%S expected_result=%S mask=%s\n"
      pat subj expected_result
      (match mask with None -> "default" | Some x -> sprintf "%S" x);
    let replace = Option.map (fun mask -> (fun _ -> mask)) mask in
    let res = Testo.mask_pcre_pattern ?replace pat subj in
    Alcotest.(check string) __LOC__ expected_result res
  in
  [
    "", "", "", None;
    "", "aa", "XaXa", Some "X";
    "", "aa", "<MASKED>a<MASKED>a", None;
    "a", "aa", "XX", Some "X";
    "a", "a,a", "X,X", Some "X";
    "a+", "xxaxxxaaaxxa", "xxAxxxAxxA", Some "A";
    "<(a+)>", "xxaxxx<aaa>xx<a>", "xxaxxx<A>xx<A>", Some "A";
    "<a+>", "xxaxxx<aaa>xx<a>", "xxaxxxAxxA", Some "A";
  ] |> List.iter test_one

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
      input_str
      expected_result;
    let res = Testo.mask_temp_paths ?depth ~temp_dir () input_str in
    Alcotest.(check string) __LOC__ expected_result res
  in
  let tmp rel_path = sprintf "%s/%s" !!temp_dir rel_path in
  [
    None, !!temp_dir, "<TMP>";
    None, tmp "", "<TMP>/";
    None, tmp "a", "<TMP>/<MASKED>";
    None, tmp "a/", "<TMP>/<MASKED>/";
    None, tmp "a/b", "<TMP>/<MASKED>/b";
    None, tmp "a//b", "<TMP>/<MASKED>//b";
    None, tmp "a-b_12/bcccc", "<TMP>/<MASKED>/bcccc";
    None, " " ^ tmp "a-b_12//bcccc/// ", " <TMP>/<MASKED>//bcccc/// ";
    None, tmp "a" ^ " " ^ tmp "b", "<TMP>/<MASKED> <TMP>/<MASKED>";
    (* don't mask sub-paths *)
    None, includes_temp_dir, includes_temp_dir;
    (* no depth limit *)
    Some None, !!temp_dir, "<TMP>";
    Some None, tmp "", "<TMP>/";
    Some None, tmp "a/b/c/d/e",
    "<TMP>/<MASKED>/<MASKED>/<MASKED>/<MASKED>/<MASKED>";
    (* default: None <=> Some (Some 1) *)
    Some (Some 1), !!temp_dir, "<TMP>";
    Some (Some 1), tmp "", "<TMP>/";
    Some (Some 1), tmp "a/b", "<TMP>/<MASKED>/b";
    (* mask only /tmp or /tmp/ *)
    Some (Some 0), !!temp_dir, "<TMP>";
    Some (Some 0), tmp "", "<TMP>/";
    Some (Some 0), tmp "a/b", "<TMP>/a/b";
    (* mask up to 3 segments after /tmp *)
    Some (Some 3), tmp "a/b/c/d", "<TMP>/<MASKED>/<MASKED>/<MASKED>/d";
    Some (Some 3), tmp "a/b/c", "<TMP>/<MASKED>/<MASKED>/<MASKED>";
    Some (Some 3), tmp "a/b", "<TMP>/<MASKED>/<MASKED>";
  ] |> List.iter test_one

(* TODO: test Windows paths *)
let test_mask_exotic_temp_paths () =
  let test_one (temp_dir, data, expected_masked_data) =
    let masked_data =
      Testo.mask_temp_paths
        ~temp_dir:(Fpath.v temp_dir)
        ~depth:(Some 0)
        ()
        data
    in
    Alcotest.(check string)
      (sprintf "temp dir %S" temp_dir)
      expected_masked_data masked_data
  in
  [
    "/", "/", "<TMP>";
    "////", "/", "<TMP>";
    "/", "/a", "<TMP>a";
    "////", "/a", "<TMP>a";
    "a", "a", "<TMP>";
    "a/", "a", "<TMP>";
    "a////", "a", "<TMP>";
    "a////", "a/", "<TMP>/";
    "a////", "a/b", "<TMP>/b";
    "/a", "/a", "<TMP>";
    "/a/", "/a", "<TMP>";
    "/a////", "/a", "<TMP>";
    "/a////", "/a/", "<TMP>/";
  ] |> List.iter test_one

let test_alcotest_error_formatting () =
  printf "This alcotest check is expected to fail \
          with nice error formatting.\n%!";
  let exn =
    try
      Alcotest.(check string) "<description>" "a" "b";
      assert false
    with e -> e
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
      raise Exit |> ignore
    );
    assert false
  with Exit -> (
      match !path_ref with
      | None -> assert false
      | Some path ->
          assert (not (Sys.file_exists (Fpath.to_string path)))
    )

let test_user_output_capture () =
  let result, capture =
    Testo.with_capture stdout (fun () ->
      print_string "hello";
      42
    )
  in
  Alcotest.(check string) "captured stdout" "hello" capture;
  Alcotest.(check int) "result" 42 result;
  (* capture to snapshot file *)
  print_string "snapshot data\n";
  (* more capture from the same channel *)
  let (), capture2 =
    Testo.with_capture stdout (fun () ->
      print_string "wow"
    )
  in
  Alcotest.(check string) "more captured stdout" "wow" capture2

(* For tests that need porting to Windows *)
let is_windows =
  Sys.os_type = "Win32"

let windows_todo : Testo.expected_outcome =
  if is_windows then
    Should_fail "this test needs to be ported to Windows"
  else
    Should_succeed

(*
   The tests marked as "auto-approve" are tests that capture their output
   and will be automatically approved by the meta test suite.
   Normal tests should not be auto-approved since it would defeat the purpose
   of approval.
*)
let tests =
  [
    t "simple" (fun () -> ());
    t "tags" ~tags:[ testing_tag; tags_tag ] (fun () -> ());
    t "category" ~category:[ "category"; "subcategory" ] (fun () -> ());
    t "unchecked stdout" (fun () -> print_endline "hello\nworld");
    t "unchecked stderr" (fun () -> prerr_string "hello\n");
    t "capture stdout"
      ~category:["auto-approve"]
      ~checked_output:(Testo.stdout ()) (fun () -> print_string "hello\n");
    t "capture stderr"
      ~category:["auto-approve"]
      ~checked_output:(Testo.stderr ()) (fun () -> prerr_string "error\n");
    t "capture stdxxx"
      ~category:["auto-approve"]
      ~checked_output:(Testo.stdxxx ()) (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr"
      ~category:["auto-approve"]
      ~checked_output:(Testo.split_stdout_stderr ())
      (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "capture stdout in custom location"
      ~category:["auto-approve"]
      ~checked_output:(
        Testo.stdout
          ~expected_stdout_path:(Fpath.v "tests/custom-snapshots/my-stdout") ()
      )
      (fun () -> print_string "hello\n");
    t "capture stderr in custom location"
      ~category:["auto-approve"]
      ~checked_output:(
        Testo.stderr
          ~expected_stderr_path:(Fpath.v "tests/custom-snapshots/my-stderr") ()
      )
      (fun () -> prerr_string "error\n");
    t "capture stdxxx in custom location"
      ~category:["auto-approve"]
      ~checked_output:(
        Testo.stdxxx
          ~expected_stdxxx_path:(Fpath.v "tests/custom-snapshots/my-stdxxx") ()
      )
      (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr in custom location"
      ~category:["auto-approve"]
      ~checked_output:(
        Testo.split_stdout_stderr
          ~expected_stdout_path:(Fpath.v "tests/custom-snapshots/split-stdout")
          ~expected_stderr_path:(Fpath.v "tests/custom-snapshots/split-stderr")
          ()
      )
      (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "xfail" ~expected_outcome:(Should_fail "raises exception on purpose")
      (fun () -> failwith "this exception is expected");
    t "skipped" ~skipped:true (fun () -> failwith "this shouldn't happen");
    t "chdir" ~tolerate_chdir:true (fun () -> Sys.chdir "/");
    t ~checked_output:(Testo.stdout ())
      ~normalize:[ String.lowercase_ascii ]
      "masked"
      (fun () -> print_endline "HELLO");
    t "mask_pcre_pattern" test_mask_pcre_pattern;
    t "mask_temp_paths"
      ~expected_outcome:windows_todo
      test_mask_temp_paths;
    t "mask exotic temp paths"
      ~expected_outcome:windows_todo
      test_mask_exotic_temp_paths;
    t ~checked_output:(Testo.stdout ())
      ~normalize:[Testo.mask_not_substring "water"]
      "check for substring in stdout"
      (fun () ->
         let random_string = string_of_float (Unix.gettimeofday ()) in
         printf "[%s] water is wet.\n" random_string
      );
    t ~checked_output:(Testo.stdout ())
      ~normalize:[Testo.mask_not_pcre_pattern "[A-Za-z]+"]
      "check words in stdout"
      (fun () ->
         let random_string = string_of_float (Unix.gettimeofday ()) in
         printf "[%s] water is wet.\n" random_string
      );
    t ~checked_output:(Testo.stdout ())
      ~normalize:[Testo.mask_not_substrings
                    ["a"; "def"; "defgh"; "efghij"; "z"]]
      "check for substrings in stdout"
      (fun () ->
         printf "abcdefghijklmnoprstuvwxyz"
      );
    t "alcotest error formatting"
      ~checked_output:(Testo.stderr ())
      ~normalize:[
        (* In our CI environment for ocaml 4.08, the line
           "File "tests/Test.ml", line ..." is missing, so we mask it if
           it exists. *)
        Testo.mask_pcre_pattern
          "Alcotest assertion failure((?:\nFile [^\n]*)?)\n"
      ]
      test_alcotest_error_formatting;
    t "temporary files" test_temporary_files;
    t "user output capture"
      ~checked_output:(Testo.stdout ())
      test_user_output_capture;
  ] @ categorized

let () =
  Testo.interpret_argv ~project_name:"testo_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      exit exit_code)
    (fun () -> tests)
