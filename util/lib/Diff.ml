(*
   Show the differences between the contents of two files

   We use the simple-diff library for this because it's available, it's
   pure OCaml, and it's easy to use.
*)

open Printf
open Fpath_.Operators

type eol = LF | CRLF | No_EOL [@@deriving show]

module Diff = Testo_diff.Make (struct
  type t = string * eol [@@deriving show]

  let compare = compare
end)

let debug = false

type diff = Diff.diff [@@deriving show]
type diffs = diff list [@@deriving show]

(* 1-based line number in each file *)
type pos = int * int [@@deriving show]
type pre_hunk = (diff * pos) list [@@deriving show]
type pre_hunks = pre_hunk list [@@deriving show]
type span = { start_line : int; length : int } [@@deriving show]

type hunk = {
  span1 : span;
  span2 : span;
  left_context : (string * eol) array;
  edits : diff list;
  right_context : (string * eol) array;
}
[@@deriving show]

type hunks = hunk list [@@deriving show]
type eol_style = LF | CRLF | Mixed [@@deriving show]

type eol_analysis = {
  style1 : eol_style;
  style2 : eol_style;
  equal_after_normalization : bool;
  consistent_style : bool;
  same_style : bool;
}
[@@deriving show]

let ends_in_cr str = str <> "" && str.[String.length str - 1] = '\r'

let classify_ordinary_line str : string * eol =
  if ends_in_cr str then (String.sub str 0 (String.length str - 1), CRLF)
  else (str, LF)

(* Classify each line of input coming from the splitting around LFs.
   The last element of the list is the last "line" without an end-of-line.
   If the last element is the empty string, we remove it because it's
   not a line. *)
let classify_lines (lines : string list) : (string * eol) list =
  match List.rev lines with
  | "" :: lines -> List.rev_map classify_ordinary_line lines
  | last :: lines ->
      (* file has no trailing EOL *)
      List.fold_left
        (fun acc line -> classify_ordinary_line line :: acc)
        [ (last, No_EOL) ]
        lines
  | [] -> []

let read_lines_from_string str : (string * eol) list =
  str |> String.split_on_char '\n' |> classify_lines

let detect_eol_style lines : eol_style =
  let has_crlf = ref false in
  let has_lf = ref false in
  List.iter
    (fun (_line, (eol : eol)) ->
      match eol with
      | LF -> has_lf := true
      | CRLF -> has_crlf := true
      | No_EOL -> ())
    lines;
  match (!has_lf, !has_crlf) with
  | true, false -> LF
  | false, true -> CRLF
  | true, true -> Mixed
  | false, false -> (* one line file without eol *) LF

let analyze_eol_style lines1 lines2 =
  let style1 = detect_eol_style lines1 in
  let style2 = detect_eol_style lines2 in
  let equal_after_normalization =
    (* normalization = pretend CRLF is equivalent to LF *)
    try
      List.for_all2
        (fun (line1, eol1) (line2, eol2) ->
          line1 = line2
          &&
          match (eol1, eol2) with
          | No_EOL, No_EOL -> true
          | No_EOL, _
          | _, No_EOL ->
              false
          | _ -> true)
        lines1 lines2
    with
    | Invalid_argument _ -> false
  in
  let consistent_style = style1 <> Mixed && style2 <> Mixed in
  let same_style =
    match (style1, style2) with
    | LF, LF
    | CRLF, CRLF ->
        true
    | _ -> false
  in
  { style1; style2; equal_after_normalization; consistent_style; same_style }

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
          fold (List.rev (ed :: current_hunk) :: hunks) [ ed ] diffs
        else (* extend current hunk *)
          fold hunks (ed :: current_hunk) diffs
    | (((Added _ | Deleted _), _) as ed) :: diffs ->
        fold hunks (ed :: current_hunk) diffs
    | [] -> List.rev (List.rev current_hunk :: hunks)
  in
  if debug then printf "diffs:\n%s\n" (show_pre_hunk diffs);
  let pre_hunks = fold [] [] diffs |> List.filter is_nontrivial_hunk in
  if debug then printf "pre-hunks:\n%s\n" (show_pre_hunks pre_hunks);
  pre_hunks

(*
   Truncate the context (Equal block) on the left of a hunk.
   Return (removed length, remaining part).
*)
let truncate_left_context ~context_len lines =
  let len = Array.length lines in
  if len <= context_len then None
  else Some (len - context_len, Array.sub lines (len - context_len) context_len)

(*
   Truncate the context (Equal block) on the right of a hunk.
   Return (remaining part, removed length).
*)
let truncate_right_context ~context_len lines =
  let len = Array.length lines in
  if len <= context_len then None
  else Some (Array.sub lines 0 context_len, len - context_len)

(* Trim the leading and trailing Equal blocks and figure out the
   start and length of the lines represented by the hunk in each file. *)
let finalize_hunk (hunks : (diff * pos) list) =
  let left_context, start1, start2, hunks =
    match hunks with
    | (Equal lines, (start1, start2)) :: hunks -> (
        match truncate_left_context ~context_len lines with
        | None -> (lines, start1, start2, hunks)
        | Some (removed, right) ->
            let offset = removed in
            (right, start1 + offset, start2 + offset, hunks))
    | ((Added _ | Deleted _), (start1, start2)) :: _ ->
        ([||], start1, start2, hunks)
    | [] -> assert false
  in
  let right_context, hunks =
    match List.rev hunks with
    | (Equal lines, _loc) :: rev_hunks -> (
        match truncate_right_context ~context_len lines with
        | None -> (lines, List.rev rev_hunks)
        | Some (left, _removed) -> (left, List.rev rev_hunks))
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
  if debug then printf "edits:\n%s\n" (show_diffs edits);
  edits |> number_diffs |> group_diffs_by_hunk |> Helpers.list_map finalize_hunk

let format_header ~color buf path1 path2 =
  (* I like having the styling markup open and close on the same line if
     possible. *)
  let line1 = sprintf "--- %s" !!path1 in
  let line2 = sprintf "+++ %s" !!path2 in
  bprintf buf "%s\n%s\n"
    (Style.opt_color color Bold line1)
    (Style.opt_color color Bold line2)

let render_eol ~is_file ~show_eol (eol : eol) =
  match eol with
  | LF when show_eol -> "·"
  | CRLF when show_eol -> "↵↵"
  | No_EOL ->
      sprintf {|\ No newline at end of %s|}
        (if is_file then "file" else "input")
  | LF
  | CRLF ->
      ""

let format_context ~is_file ~show_eol buf lines =
  Array.iter
    (fun (line, eol) ->
      bprintf buf " %s%s\n" line (render_eol ~is_file ~show_eol eol))
    lines

let format_edit ~color ~is_file ~show_eol buf (x : diff) =
  match x with
  | Equal lines -> format_context ~is_file ~show_eol buf lines
  | Added lines ->
      Array.iter
        (fun (line, eol) ->
          bprintf buf "%s\n"
            (sprintf "+%s%s" line (render_eol ~is_file ~show_eol eol)
            |> Style.opt_color color Green))
        lines
  | Deleted lines ->
      Array.iter
        (fun (line, eol) ->
          bprintf buf "%s\n"
            (sprintf "-%s%s" line (render_eol ~is_file ~show_eol eol)
            |> Style.opt_color color Red))
        lines

let format_hunk ~color ~is_file ~show_eol buf (x : hunk) =
  sprintf "@@ -%d,%d +%d,%d @@" x.span1.start_line x.span1.length
    x.span2.start_line x.span2.length
  |> Style.opt_color color Cyan |> bprintf buf "%s\n";
  format_context ~is_file ~show_eol buf x.left_context;
  List.iter (format_edit ~color ~is_file ~show_eol buf) x.edits;
  format_context ~is_file ~show_eol buf x.right_context

(*
   Print in the Unified format.
   See https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html
*)
let format ~color ~is_file ~show_eol buf path1 path2 (edits : diff list) : unit
    =
  let hunks = hunks_of_edits edits in
  if debug then printf "hunks:\n%s\n" (show_hunks hunks);
  format_header ~color buf path1 path2;
  List.iter (format_hunk ~color ~is_file ~show_eol buf) hunks

let print_diff_to_string ~color ~is_file ~show_eol path1 path2 edits =
  let buf = Buffer.create 1000 in
  format ~color ~is_file ~show_eol buf path1 path2 edits;
  Buffer.contents buf

let format_eol_style eol_style =
  match eol_style with
  | LF -> "LF"
  | CRLF -> "CRLF"
  | Mixed -> "mixed LF/CRLF"

let print_eol_diff_to_string ?(color = true) path1 path2 eol_res =
  let buf = Buffer.create 100 in
  format_header ~color buf path1 path2;
  bprintf buf "Files differ only by line endings (%s ↔ %s)\n"
    (format_eol_style eol_res.style1)
    (format_eol_style eol_res.style2);
  Buffer.contents buf

let lines ?(color = true) ?(is_file = false) ?(path1 = Fpath.v "a")
    ?(path2 = Fpath.v "b") ?(show_eol = false) lines1 lines2 =
  let edits = Diff.get_diff lines1 lines2 in
  print_diff_to_string ~color ~is_file ~show_eol path1 path2 edits

let strings ?color ?is_file ~path1 ~path2 str1 str2 =
  let lines1 = read_lines_from_string str1 in
  let lines2 = read_lines_from_string str2 in
  if lines1 = lines2 then None
  else
    let eol_res = analyze_eol_style lines1 lines2 in
    if eol_res.equal_after_normalization && eol_res.consistent_style then
      Some (print_eol_diff_to_string ?color path1 path2 eol_res)
    else
      Some
        (lines ?color ?is_file ~path1 ~path2 ~show_eol:(not eol_res.same_style)
           (Array.of_list lines1) (Array.of_list lines2))

let files ?color path1 path2 =
  let str1 = Helpers.read_text_file path1 in
  let str2 = Helpers.read_text_file path2 in
  strings ?color ~is_file:true ~path1 ~path2 str1 str2
