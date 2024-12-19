(*
   Unit tests for testo-diff
*)

open Printf
module Diff = Testo_diff.Make (String)

(* OCaml syntax for copy-pasting *)
let ocaml_string_of_array a =
  sprintf "[|%s|]"
    (a |> Array.to_list |> List.map (sprintf "%S") |> String.concat "; ")

(* Line syntax to feed an external 'diff' program *)
let lines_of_array a =
  a
  |> Array.map (fun s -> sprintf "%s\n" s)
  |> Array.to_list |> String.concat ""

let string_of_array a =
  sprintf "%s\n--- begin lines ---\n%s--- end lines ---\n"
    (ocaml_string_of_array a) (lines_of_array a)

let string_of_diff (diff : Diff.diff) =
  let prefix, lines =
    match diff with
    | Equal a -> (" ", a)
    | Added a -> ("+", a)
    | Deleted a -> ("-", a)
  in
  lines |> Array.to_list
  |> List.map (fun line -> sprintf "%s%s\n" prefix line)
  |> String.concat ""

let string_of_diffs diffs = diffs |> List.map string_of_diff |> String.concat ""

let count_edits (diffs : Diff.t) =
  diffs
  |> List.fold_left
       (fun n x ->
         match (x : Diff.diff) with
         | Equal _ -> n
         | Added a
         | Deleted a ->
             n + Array.length a)
       0

let roundtrip expected_number_of_edits a b () =
  printf "Old input: %s\nNew input: %s\n%!" (string_of_array a)
    (string_of_array b);
  let diffs = Diff.get_diff a b in
  let number_of_edits = count_edits diffs in
  printf "Diff:\n%s%!" (string_of_diffs diffs);
  let ((a', b') as recovered_input) = Diff.recover_input diffs in
  printf "Recovered old input: %s\nRecovered new input: %s\n%!"
    (string_of_array a') (string_of_array b');
  printf "Expected number of edits, actual number: %i, %i\n%!"
    expected_number_of_edits number_of_edits;
  assert (recovered_input = (a, b));
  assert (number_of_edits = expected_number_of_edits)

(* Check both "diff a b" and "diff b a" *)
let roundtrip_sym number_of_edits a b () =
  roundtrip number_of_edits a b ();
  printf "Check symmetric operation\n%!";
  roundtrip number_of_edits b a ()

let tests =
  [
    ("empty", roundtrip 0 [||] [||]);
    ("add one", roundtrip 1 [||] [| "a" |]);
    ("delete one", roundtrip 1 [| "a" |] [||]);
    ("replace one", roundtrip 2 [| "a" |] [| "b" |]);
    ("one line", roundtrip 0 [| "a" |] [| "a" |]);
    ("two lines", roundtrip 0 [| "a"; "b" |] [| "a"; "b" |]);
    ("add first", roundtrip 1 [| "b" |] [| "a"; "b" |]);
    ("add last", roundtrip 1 [| "a" |] [| "a"; "b" |]);
    ("delete first", roundtrip 1 [| "a"; "b" |] [| "b" |]);
    ("delete last", roundtrip 1 [| "a"; "b" |] [| "a" |]);
    ("add inside", roundtrip 1 [| "a"; "c" |] [| "a"; "b"; "c" |]);
    ("delete inside", roundtrip 1 [| "a"; "b"; "c" |] [| "a"; "c" |]);
    ( "random 1",
      roundtrip_sym 6
        [| "a"; "b"; "c"; "d"; "e"; "a"; "b"; "f"; "c"; "g"; "h" |]
        [|
          "a";
          "c";
          "d";
          "e";
          "b";
          "a";
          "b";
          "f";
          "c";
          "d";
          "b";
          "f";
          "c";
          "g";
          "h";
        |] );
    ( "random 2",
      roundtrip_sym 10
        [|
          "a";
          "b";
          "a";
          "b";
          "b";
          "e";
          "c";
          "d";
          "f";
          "d";
          "e";
          "a";
          "b";
          "f";
          "c";
          "g";
          "h";
        |]
        [|
          "a";
          "c";
          "b";
          "d";
          "e";
          "b";
          "a";
          "b";
          "c";
          "d";
          "f";
          "c";
          "e";
          "d";
          "b";
          "f";
          "c";
          "g";
          "h";
        |] );
    ("bug",
     roundtrip_sym 1
       [| "1"; "2"; "3"; "4"; "5"; "6"; |]
       [| "Inserted"; "1"; "2"; "3"; "4"; "5"; "6"; |]);
  ]

(* Who needs a test framework anyway? *)
let main () =
  let failed =
    tests
    |> List.fold_left
         (fun failed (name, test_func) ->
           printf "### TEST %s ###\n%!" name;
           try
             test_func ();
             failed
           with
           | e ->
               eprintf "*** Test %S failed: %s\n%!" name (Printexc.to_string e);
               name :: failed)
         []
    |> List.rev
  in
  match failed with
  | [] -> ()
  | failed ->
      eprintf "*** The following tests failed:\n%s%!"
        (failed
        |> List.map (fun name -> sprintf "  - %s\n" name)
        |> String.concat "");
      exit 1

let () = main ()
