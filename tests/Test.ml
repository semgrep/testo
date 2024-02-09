(*
   Dummy suite exercising a variety of test options.
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
    let res = Testo.mask_pcre_pattern ?mask pat subj in
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

let tests =
  [
    t "simple" (fun () -> ());
    t "tags" ~tags:[ testing_tag; tags_tag ] (fun () -> ());
    t "category" ~category:[ "category"; "subcategory" ] (fun () -> ());
    t "unchecked stdout" (fun () -> print_endline "hello\nworld");
    t "unchecked stderr" (fun () -> prerr_string "hello\n");
    t "capture stdout" ~checked_output:Stdout (fun () -> print_string "hello\n");
    t "capture stderr" ~checked_output:Stderr (fun () -> prerr_string "error\n");
    t "capture stdxxx" ~checked_output:Merged_stdout_stderr (fun () ->
        print_string "hello\n";
        flush stdout;
        prerr_string "error\n";
        flush stderr;
        print_string "goodbye\n");
    t "capture stdout and stderr" ~checked_output:Separate_stdout_stderr
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
  ] @ categorized

let () =
  Testo.interpret_argv ~project_name:"testo_dummy_tests"
    ~handle_subcommand_result:(fun exit_code _ ->
      print_endline "<handling result before exiting>";
      (* nosemgrep: forbid-exit *)
      exit exit_code)
    (fun () -> tests)
