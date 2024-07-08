(*
   Show the differences between the contents of two files

   We use the simple-diff library for this because it's available, it's
   pure OCaml, and it's easy to use.
*)

open Printf
open Fpath_.Operators
module Diff = Testo_diff.Make (String)

type diff = Diff.diff

(* 1-based line number in each file *)
type pos = int * int
type span = { start_line : int; length : int }

type hunk = {
  span1 : span;
  span2 : span;
  left_context : string array;
  edits : diff list;
  right_context : string array;
}

let read_lines path =
  path |> Helpers.read_file |> String.split_on_char '\n' |> Array.of_list

(* Minimum number of lines of context to show before or after a deletion
   or an insertion when possible *)
let context_len = 3

(* Assign line numbers to each Equal/Added/Deleted block *)
let number_diffs (diffs : diff list) : (diff * pos) list =
  let line1 = ref 1 in
  let line2 = ref 1 in
  (* fold_left guarantees left-to-right evaluation unlike List.map *)
  List.fold_left
    (fun acc (x : diff) ->
      match x with
      | Equal lines ->
          let res = (x, (!line1, !line2)) in
          line1 := !line1 + Array.length lines;
          line2 := !line2 + Array.length lines;
          res :: acc
      | Added lines ->
          let res = (x, (!line1, !line2)) in
          line2 := !line2 + Array.length lines;
          res :: acc
      | Deleted lines ->
          let res = (x, (!line1, !line2)) in
          line1 := !line1 + Array.length lines;
          res :: acc)
    [] diffs
  |> List.rev

let is_nontrivial_hunk (xs : (diff * pos) list) =
  match xs with
  | [] -> false
  | [ (Equal _, _) ] -> false
  | _ -> true

(*
   There's a gap between two hunks if they're separated by more than
   6 identical lines (lines of context * 2).
*)
let group_diffs_by_hunk (diffs : (diff * pos) list) : (diff * pos) list list =
  let rec fold hunks current_hunk (diffs : (diff * pos) list) =
    match diffs with
    | ((Equal lines, _) as ed) :: diffs ->
        if Array.length lines > 2 * context_len then
          (* close current hunk, start a new one.
             The trick is to add the Equal block to both the previous
             and next hunk. *)
          fold (List.rev ((ed :: current_hunk) :: hunks)) [ ed ] diffs
        else (* extend current hunk *)
          fold hunks (ed :: current_hunk) diffs
    | (((Added _ | Deleted _), _) as ed) :: diffs ->
        fold hunks (ed :: current_hunk) diffs
    | [] -> List.rev (List.rev current_hunk :: hunks)
  in
  fold [] [] diffs |> List.filter is_nontrivial_hunk

(*
   When encountering an Equal block, determine whether to create a boundary
   between hunks.
*)
let elide_equal_lines ~context_len lines =
  let len = Array.length lines in
  if len <= 2 * context_len then None
  else
    Some
      ( Array.sub lines 0 context_len,
        len - (2 * context_len),
        Array.sub lines (len - context_len) context_len )

(* Trim the leading and trailing Equal blocks and figure out the
   start and length of the lines represented by the hunk in each file. *)
let finalize_hunk (hunks : (diff * pos) list) =
  let left_context, start1, start2, hunks =
    match hunks with
    | (Equal lines, (start1, start2)) :: hunks -> (
        match elide_equal_lines ~context_len lines with
        | None -> (lines, start1, start2, hunks)
        | Some (left, mid, right) ->
            let offset = Array.length left + mid in
            (right, start1 + offset, start2 + offset, hunks))
    | ((Added _ | Deleted _), (start1, start2)) :: _ ->
        ([||], start1, start2, hunks)
    | [] -> assert false
  in
  let right_context, hunks =
    match List.rev hunks with
    | (Equal lines, _loc) :: rev_hunks -> (
        match elide_equal_lines ~context_len lines with
        | None -> (lines, List.rev rev_hunks)
        | Some (left, _mid, _right) -> (left, List.rev rev_hunks))
    | ((Added _ | Deleted _), _) :: _ -> ([||], hunks)
    | [] -> assert false
  in
  let length_without_context1, length_without_context2 =
    List.fold_left
      (fun (length1, length2) ((ed : diff), _pos) ->
        match ed with
        | Equal lines ->
            let n = Array.length lines in
            (length1 + n, length2 + n)
        | Added lines ->
            let n = Array.length lines in
            (length1, length2 + n)
        | Deleted lines ->
            let n = Array.length lines in
            (length1 + n, length2))
      (0, 0) hunks
  in
  let context_length = Array.length left_context + Array.length right_context in
  let length1 = length_without_context1 + context_length in
  let length2 = length_without_context2 + context_length in
  {
    span1 = { start_line = start1; length = length1 };
    span2 = { start_line = start2; length = length2 };
    left_context;
    edits = Helpers.list_map fst hunks;
    right_context;
  }

let hunks_of_edits (edits : diff list) : hunk list =
  edits |> number_diffs |> group_diffs_by_hunk |> Helpers.list_map finalize_hunk

let format_header ~color buf path1 path2 =
  (* I like having the styling markup open and close on the same line if
     possible. *)
  let line1 = sprintf "--- %s" !!path1 in
  let line2 = sprintf "+++ %s" !!path2 in
  bprintf buf "%s\n%s\n"
    (Style.opt_color color Bold line1)
    (Style.opt_color color Bold line2)

let format_context buf lines =
  Array.iter (fun line -> bprintf buf " %s\n" line) lines

let format_edit ~color buf (x : diff) =
  match x with
  | Equal lines -> format_context buf lines
  | Added lines ->
      Array.iter
        (fun line ->
          bprintf buf "%s\n" (sprintf "+%s" line |> Style.opt_color color Green))
        lines
  | Deleted lines ->
      Array.iter
        (fun line ->
          bprintf buf "%s\n" (sprintf "-%s" line |> Style.opt_color color Red))
        lines

let format_hunk ~color buf (x : hunk) =
  sprintf "@@ -%d,%d +%d,%d @@" x.span1.start_line x.span1.length
    x.span2.start_line x.span2.length
  |> Style.opt_color color Cyan |> bprintf buf "%s\n";
  format_context buf x.left_context;
  List.iter (format_edit ~color buf) x.edits;
  format_context buf x.right_context

(*
   Print in the Unified format.
   See https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html
*)
let format ~color buf path1 path2 (edits : diff list) : unit =
  let hunks = hunks_of_edits edits in
  format_header ~color buf path1 path2;
  List.iter (format_hunk ~color buf) hunks

let print_to_string ~color path1 path2 edits =
  let buf = Buffer.create 1000 in
  format ~color buf path1 path2 edits;
  Buffer.contents buf

let lines ?(color = true) ?(path1 = Fpath.v "a") ?(path2 = Fpath.v "b") lines1
    lines2 =
  let edits = Diff.get_diff lines1 lines2 in
  print_to_string ~color path1 path2 edits

let files ?color path1 path2 =
  let lines1 = read_lines path1 in
  let lines2 = read_lines path2 in
  (lines1 = lines2, lines ?color ~path1 ~path2 lines1 lines2)
