(*
   Unit tests for testo-diff
*)

open Printf
module Diff = Testo_diff.Make (String)

let string_of_array a =
  sprintf "[|%s|]"
    (a |> Array.to_list |> List.map (sprintf "%S") |> String.concat "; ")

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

let roundtrip a b () =
  printf "Input:\n  old: %s\n  new: %s\n%!" (string_of_array a)
    (string_of_array b);
  let diffs = Diff.get_diff a b in
  printf "Diff:\n%s%!" (string_of_diffs diffs);
  let ((a', b') as recovered_input) = Diff.recover_input diffs in
  printf "Recovered input:\n  old: %s\n  new: %s\n%!" (string_of_array a')
    (string_of_array b');
  assert (recovered_input = (a, b))

let tests =
  [
    ("empty", roundtrip [||] [||]);
    ("add one", roundtrip [||] [| "a" |]);
    ("delete one", roundtrip [| "a" |] [||]);
    ("replace one", roundtrip [| "a" |] [| "b" |]);
    ("one line", roundtrip [| "a" |] [| "a" |]);
    ("two lines", roundtrip [| "a"; "b" |] [| "a"; "b" |]);
    ("add first", roundtrip [| "b" |] [| "a"; "b" |]);
    ("add last", roundtrip [| "a" |] [| "a"; "b" |]);
    ("delete first", roundtrip [| "a"; "b" |] [| "b" |]);
    ("delete last", roundtrip [| "a"; "b" |] [| "a" |]);
    ("add inside", roundtrip [| "a"; "c" |] [| "a"; "b"; "c" |]);
    ("delete inside", roundtrip [| "a"; "b"; "c" |] [| "a"; "c" |]);
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
