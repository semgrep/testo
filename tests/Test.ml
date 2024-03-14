(*
   Test suite exercising a variety of test options.
*)

open Printf

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

let test_mask_temp_paths () =
  let tmpdir = "tmpdir" in
  let test_one (depth, input_str, expected_result) =
    printf "depth=%S input_str=%S expected_result=%S\n%!"
      (match depth with
       | Some (Some n) -> string_of_int n
       | Some None -> "no limit"
       | None -> "default")
      input_str
      expected_result;
    let res = Testo.mask_temp_paths ?depth ~tmpdir () input_str in
    Alcotest.(check string) __LOC__ expected_result res
  in
  let tmp rel_path = sprintf "%s/%s" tmpdir rel_path in
  [
    None, tmpdir, "<TMP>";
    None, tmp "", "<TMP>/";
    None, tmp "a", "<TMP>/<MASKED>";
    None, tmp "a/", "<TMP>/<MASKED>/";
    None, tmp "a/b", "<TMP>/<MASKED>/b";
    None, tmp "a//b", "<TMP>/<MASKED>//b";
    None, tmp "a-b_12/bcccc", "<TMP>/<MASKED>/bcccc";
    None, " " ^ tmp "a-b_12//bcccc/// ", " <TMP>/<MASKED>//bcccc/// ";
    None, tmp "a" ^ " " ^ tmp "b", "<TMP>/<MASKED> <TMP>/<MASKED>";
    (* no depth limit *)
    Some None, tmpdir, "<TMP>";
    Some None, tmp "", "<TMP>/";
    Some None, tmp "a/b/c/d/e",
    "<TMP>/<MASKED>/<MASKED>/<MASKED>/<MASKED>/<MASKED>";
    (* default: None <=> Some (Some 1) *)
    Some (Some 1), tmpdir, "<TMP>";
    Some (Some 1), tmp "", "<TMP>/";
    Some (Some 1), tmp "a/b", "<TMP>/<MASKED>/b";
    (* mask only /tmp or /tmp/ *)
    Some (Some 0), tmpdir, "<TMP>";
    Some (Some 0), tmp "", "<TMP>/";
    Some (Some 0), tmp "a/b", "<TMP>/a/b";
    (* mask up to 3 segments after /tmp *)
    Some (Some 3), tmp "a/b/c/d", "<TMP>/<MASKED>/<MASKED>/<MASKED>/d";
    Some (Some 3), tmp "a/b/c", "<TMP>/<MASKED>/<MASKED>/<MASKED>";
    Some (Some 3), tmp "a/b", "<TMP>/<MASKED>/<MASKED>";
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
      ~checked_output:Stdout (fun () -> print_string "hello\n");
    t "capture stderr"
      ~category:["auto-approve"]
      ~checked_output:Stderr (fun () -> prerr_string "error\n");
    t "capture stdxxx"
      ~category:["auto-approve"]
      ~checked_output:Merged_stdout_stderr (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr"
      ~category:["auto-approve"]
      ~checked_output:Separate_stdout_stderr
      (fun () ->
        print_string "hello\n";
        prerr_string "error\n");
    t "xfail" ~expected_outcome:(Should_fail "raises exception on purpose")
      (fun () -> failwith "this exception is expected");
    t "skipped" ~skipped:true (fun () -> failwith "this shouldn't happen");
    t "chdir" ~tolerate_chdir:true (fun () -> Sys.chdir "/");
    t ~checked_output:Stdout ~normalize:[ String.lowercase_ascii ] "masked"
      (fun () -> print_endline "HELLO");
    t "mask_pcre_pattern" test_mask_pcre_pattern;
    t "mask_temp_paths" test_mask_temp_paths;
    t ~checked_output:Stdout
      ~normalize:[Testo.mask_not_substring "water"]
      "check for substring in stdout"
      (fun () ->
         let random_string = string_of_float (Unix.gettimeofday ()) in
         printf "[%s] water is wet.\n" random_string
      );
    t ~checked_output:Stdout
      ~normalize:[Testo.mask_not_pcre_pattern "[A-Za-z]+"]
      "check words in stdout"
      (fun () ->
         let random_string = string_of_float (Unix.gettimeofday ()) in
         printf "[%s] water is wet.\n" random_string
      );
    t ~checked_output:Stdout
      ~normalize:[Testo.mask_not_substrings
                    ["a"; "def"; "defgh"; "efghij"; "z"]]
      "check for substrings in stdout"
      (fun () ->
         printf "abcdefghijklmnoprstuvwxyz"
      );
    t "alcotest error formatting"
      ~checked_output:Stderr
      ~normalize:[Testo.mask_line ~after:{|File "tests/Test.ml", line |} ()]
      test_alcotest_error_formatting
  ] @ categorized

let () =
  Testo.interpret_argv ~project_name:"testo_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      exit exit_code)
    (fun () -> tests)
