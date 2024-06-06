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
    ("one line", roundtrip [| "a" |] [| "a" |]);
    ("two lines", roundtrip [| "a"; "b" |] [| "a"; "b" |]);
    ("diffs 1", roundtrip [| "a" |] [| "b" |]);
    ("diffs 2", roundtrip [| "a"; "c" |] [| "a"; "b"; "c" |]);
  ]

(* Who needs a test framework anyway? *)
let main () =
  tests
  |> List.iter (fun (name, test_func) ->
         try test_func () with
         | e -> eprintf "*** Test %s failed: %s\n%!" name (Printexc.to_string e))

let () = main ()
