(*
   A slice is a range of tests defined by the slice number and the total
   number of slices such as "3/4" or "2/8".
*)

open Printf

type t = { num : int; out_of : int }

let create ~num ~out_of =
  if num >= 1 && num <= out_of then Some { num; out_of } else None

let of_string str =
  match String.split_on_char '/' str with
  | [ a; b ] -> (
      try create ~num:(int_of_string a) ~out_of:(int_of_string b) with
      | _ -> None)
  | _ -> None

let to_string { num; out_of } = Printf.sprintf "%d/%d" num out_of

(*
   Algorithm to split N elements into M even batches:
   1. N/M gives the minimum number of elements in each batch.
   2. The remainder R is distributed to the first R batches.
   This results in the first R batches having (N/M)+1 elements and
   the remaining M-R batches having N/M elements.
*)
let apply_to_array slice ar =
  let n = Array.length ar in
  let m = slice.out_of in
  let min_slice_len = n / m in
  let r = n mod m in
  let get_slice_start slice_num =
    let slice_index = slice_num - 1 in
    if slice_index <= r then (min_slice_len + 1) * slice_index
    else ((min_slice_len + 1) * r) + (min_slice_len * (slice_index - r))
  in
  let start_index = get_slice_start slice.num in
  let end_index = get_slice_start (slice.num + 1) in
  (*
  printf "%s: len=%i, min_slice_len=%i, start=%i, end=%i\n%!"
    (to_string slice) n min_slice_len start_index end_index;
  *)
  Array.sub ar start_index (end_index - start_index)

let apply slice list =
  list |> Array.of_list |> apply_to_array slice |> Array.to_list

let apply_slices slices list =
  List.fold_left (fun acc slice -> apply slice acc) list slices

(****************** Unit tests **********************)

let string_of_list xs =
  sprintf "[%s]" (xs |> List.map string_of_int |> String.concat ",")

let tests =
  [
    ("empty 1/1", [ "1/1" ], [], []);
    ("empty 1/5", [ "1/5" ], [], []);
    ("empty 5/5", [ "5/5" ], [], []);
    ("short 1/5", [ "1/5" ], [ 1; 2; 3 ], [ 1 ]);
    ("short 3/5", [ "3/5" ], [ 1; 2; 3 ], [ 3 ]);
    ("short 4/5", [ "4/5" ], [ 1; 2; 3 ], []);
    ("short 5/5", [ "5/5" ], [ 1; 2; 3 ], []);
    ("even 1/3", [ "1/3" ], [ 1; 2; 3; 4; 5; 6 ], [ 1; 2 ]);
    ("even 2/3", [ "2/3" ], [ 1; 2; 3; 4; 5; 6 ], [ 3; 4 ]);
    ("even 3/3", [ "3/3" ], [ 1; 2; 3; 4; 5; 6 ], [ 5; 6 ]);
    ("long 1/3", [ "1/3" ], [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ], [ 1; 2; 3; 4 ]);
    ("long 2/3", [ "2/3" ], [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ], [ 5; 6; 7 ]);
    ("long 3/3", [ "3/3" ], [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ], [ 8; 9; 10 ]);
    ( "chained 1/3 2/2",
      [ "1/3"; "2/2" ],
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ],
      [ 3; 4 ] );
    ( "chained 2/3 1/2",
      [ "2/3"; "1/2" ],
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ],
      [ 5; 6 ] );
    ( "chained 2/3 2/2",
      [ "2/3"; "2/2" ],
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ],
      [ 7 ] );
  ]
  |> List.map (fun (name, slice_strs, input, expected_result) ->
         let func () =
           printf "Expected result: %s\n%!" (string_of_list expected_result);
           let slices =
             List.map of_string slice_strs
             |> List.map (function
                  | Some x -> x
                  | None -> assert false)
           in
           let result = apply_slices slices input in
           printf "Result: %s\n%!" (string_of_list result);
           assert (result = expected_result)
         in
         (name, func))
