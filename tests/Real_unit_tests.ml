(*
   Unit tests for Testo's implementation: testing this and that OCaml function
   exposed by the Testo module.
*)

open Printf

let t = Testo.create

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

let tests = Testo.categorize "unit tests" [
  t "mask_pcre_pattern" test_mask_pcre_pattern;
]
