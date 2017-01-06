type diff_lines_t =
  | Deleted of string array
  | Added of string array
  | Equal of string array


module CounterMap = Map.Make(String)


let map_counter keys =
  let keys_and_indices = Array.mapi (fun index key -> index, key) keys in
  Array.fold_left (fun map (index, key) ->
    let indices = try CounterMap.find key map with | Not_found -> [] in
    CounterMap.add key (index :: indices) map
  ) CounterMap.empty keys_and_indices


let rec get_diff old_lines new_lines =
  match (old_lines, new_lines) with
  | ([||], [||]) -> []
  | _ ->
    let old_values_counter = map_counter old_lines in
    let overlap = Hashtbl.create 5000 in
    let sub_start_old = ref 0 in
    let sub_start_new = ref 0 in
    let longest_subsequence = ref 0 in

    Array.iteri (fun new_index new_value ->
      let indices = try CounterMap.find new_value old_values_counter with
        | Not_found -> []
      in
      List.iter (fun old_index ->
        let prev_subsequence = try Hashtbl.find overlap (old_index - 1) with | Not_found -> 0 in
        let new_subsequence = prev_subsequence + 1 in
        Hashtbl.add overlap old_index new_subsequence;

        if new_subsequence > !longest_subsequence then
          let () = Printf.printf "new_index - %i; old_index - %i; sub_start_new: %i\n" new_index old_index !sub_start_new in
          sub_start_old := old_index - new_subsequence + 1;
          sub_start_new := new_index - new_subsequence + 1;
          longest_subsequence := new_subsequence;
      ) indices;
    ) new_lines;

    if !longest_subsequence == 0 then
      [Deleted old_lines; Added new_lines]
    else
      let old_lines_length = Array.length old_lines in
      let new_lines_length = Array.length new_lines in
      Printf.printf "sub_start_old: %i\n" !sub_start_old;
      Printf.printf "sub_start_new: %i\n" !sub_start_new;
      let old_lines_presubseq = Array.sub old_lines 0 !sub_start_old in
      let new_lines_presubseq = Array.sub new_lines 0 !sub_start_new in
      Printf.printf "old_subsequence: %i\n" (!sub_start_old + !longest_subsequence);
      Printf.printf "new_subsequence: %i\n" (!sub_start_new + !longest_subsequence);
      let old_lines_postsubseq =
        let starting_index = !sub_start_old + !longest_subsequence in
        Array.sub old_lines starting_index (old_lines_length - starting_index)
      in
      Printf.printf "new_lines\n";
      let new_lines_postsubseq =
        let starting_index = !sub_start_new + !longest_subsequence in
        Array.sub new_lines starting_index (new_lines_length - starting_index)
      in
      let unchanged_lines = Array.sub new_lines !sub_start_new !longest_subsequence in
      get_diff old_lines_presubseq new_lines_presubseq @
      [Equal unchanged_lines] @
      get_diff old_lines_postsubseq new_lines_postsubseq


let string_of_diff diffs =
  let concat symbol lines =
    let lines = List.map (fun line -> symbol ^ " " ^ line) (Array.to_list lines) in
    String.concat "\n" lines
  in
  let stringify str diff =
    match diff with
    | Added lines   -> str ^ concat "+" lines
    | Deleted lines   -> str ^ concat "-" lines
    | Equal lines -> str
  in
  List.fold_left stringify "" diffs

let old_value = "Foo bar baz\nTesticles\nmurder\nshe\nwrote\nend"

let new_value = "Foo bar baz\nTest\nmurder\nshe\nwrote\ntrip"

let () =
  let new_lines = Re_str.split (Re_str.regexp "\n") new_value |> Array.of_list in
  let old_lines = Re_str.split (Re_str.regexp "\n") old_value |> Array.of_list in
  let content = get_diff old_lines new_lines |> string_of_diff in
  Printf.printf "%s" content
