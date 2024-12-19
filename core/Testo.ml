(*
   Utilities for writing test suites for Alcotest.

   Keep in mind that some of this may become its own library or move to
   Alcotest.
*)

open Printf
open Testo_util
open Fpath_.Operators
module T = Types

(****************************************************************************)
(* Main types *)
(****************************************************************************)

type expected_outcome = T.expected_outcome =
  | Should_succeed
  | Should_fail of string

type completion_status = T.completion_status =
  | Test_function_returned
  | Test_function_raised_an_exception

type fail_reason = T.fail_reason = Raised_exception | Incorrect_output
type outcome = T.outcome = Succeeded | Failed of fail_reason

type captured_output = T.captured_output =
  | Ignored of string
  | Captured_stdout of string * string
  | Captured_stderr of string * string
  | Captured_stdout_stderr of string * string
  | Captured_merged of string

type expected_output = T.expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string (* stdout, stderr *)
  | Expected_merged of string (* combined output *)

type result = T.result = {
  completion_status : completion_status;
  captured_output : captured_output;
}

type missing_files = T.missing_files = Missing_files of Fpath.t list

type expectation = T.expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, missing_files) Result.t;
}

type status = T.status = {
  expectation : expectation;
  result : (result, missing_files) Result.t;
}

type passing_status = T.passing_status =
  | PASS
  | FAIL of fail_reason
  | XFAIL of fail_reason
  | XPASS
  | MISS of missing_files

type status_summary = T.status_summary = {
  passing_status : passing_status;
  outcome : outcome;
  has_expected_output : bool;
}

type test_with_status = T.test * status * status_summary

type subcommand_result = Cmd.subcommand_result =
  | Run_result of test_with_status list
  | Status_result of test_with_status list
  | Approve_result

(* export *)
module Promise = Promise
module Tag = Tag

type checked_output_kind = T.checked_output_kind

type t = T.test = {
  id : string;
  internal_full_name : string;
  category : string list;
  name : string;
  func : unit -> unit Promise.t;
  broken : string option;
  checked_output : checked_output_kind;
  expected_outcome : expected_outcome;
  normalize : (string -> string) list;
  skipped : string option;
  solo : string option;
  tags : Tag.t list;
  tolerate_chdir : bool;
  tracking_url : string option;
}

(* Polymorphic type alias for an Alcotest's 'test_case'. *)
type alcotest_test_case = string * [ `Quick | `Slow ] * (unit -> unit Promise.t)

(* Polymorphic type alias for an Alcotest's 'test'. *)
type alcotest_test = string * alcotest_test_case list

(****************************************************************************)
(* Conversions *)
(****************************************************************************)

let stdout ?expected_stdout_path () : T.checked_output_kind =
  Stdout { expected_output_path = expected_stdout_path }

let stderr ?expected_stderr_path () : T.checked_output_kind =
  Stderr { expected_output_path = expected_stderr_path }

let stdxxx ?expected_stdxxx_path () : T.checked_output_kind =
  Stdxxx { expected_output_path = expected_stdxxx_path }

let split_stdout_stderr ?expected_stdout_path ?expected_stderr_path () :
    T.checked_output_kind =
  Split_stdout_stderr
    ( { expected_output_path = expected_stdout_path },
      { expected_output_path = expected_stderr_path } )

(*
   Create an hexadecimal hash that is just long enough to not suffer from
   collisions in realistic use cases defined as:
   "works fine up to 1 million tests".

   With 12 hexadecimal characters (48 bits), the probability of a collision
   within 1 million hashes is 0.0018. If anyone runs into collisions due
   to having more than a million tests, we can add an option to increase
   the number of bits.
*)
let update_id (test : t) =
  let internal_full_name = T.recompute_internal_full_name test in
  let md5_hex = internal_full_name |> Digest.string |> Digest.to_hex in
  assert (String.length md5_hex = 32);
  (* nosemgrep: ocamllint-str-first-chars *)
  let id = String.sub md5_hex 0 12 in
  { test with id; internal_full_name }

let create ?broken ?(category = []) ?(checked_output = T.Ignore_output)
    ?(expected_outcome = Should_succeed) ?(normalize = []) ?skipped ?solo
    ?(tags = []) ?(tolerate_chdir = false) ?tracking_url name func =
  {
    id = "";
    internal_full_name = "";
    category;
    name;
    func;
    broken;
    checked_output;
    expected_outcome;
    normalize;
    skipped;
    solo;
    tags;
    tolerate_chdir;
    tracking_url;
  }
  |> update_id

let opt option default = Option.value option ~default

let update ?broken ?category ?checked_output ?expected_outcome ?func ?normalize
    ?name ?skipped ?solo ?tags ?tolerate_chdir ?tracking_url old =
  {
    id = "";
    internal_full_name = "";
    category = opt category old.category;
    name = opt name old.name;
    func = opt func old.func;
    (* requires same type for func and old.func *)
    broken = opt broken old.broken;
    checked_output = opt checked_output old.checked_output;
    expected_outcome = opt expected_outcome old.expected_outcome;
    normalize = opt normalize old.normalize;
    skipped = opt skipped old.skipped;
    solo = opt solo old.solo;
    tags = opt tags old.tags;
    tolerate_chdir = opt tolerate_chdir old.tolerate_chdir;
    tracking_url = opt tracking_url old.tracking_url;
  }
  |> update_id

exception Test_failure = Testo_util.Error.Test_failure

(* 'Testo.fail' is unambiguous to the user, no need to call it
   'Testo.fail_test'. *)
let fail = Testo_util.Error.fail_test

(**************************************************************************)
(* Files and output manipulation *)
(**************************************************************************)

let write_file = Helpers.write_file
let read_file = Helpers.read_file
let with_temp_file = Temp_file.with_temp_file
let with_capture = Store.with_capture

let with_chdir path func =
  let orig_cwd = Unix.getcwd () in
  Unix.chdir !!path;
  Fun.protect
    ~finally:(fun () -> Unix.chdir orig_cwd)
    func

(**************************************************************************)
(* Hygiene *)
(**************************************************************************)

let getenv_string k =
  match Sys.getenv_opt k with
  | None -> ""
  | Some v -> v

let with_environment_variables env func =
  let envs =
    Helpers.list_map
      (fun (k, tmp) ->
        (* OCaml doesn't provide an 'unsetenv' function, so we won't
           distinguish an unset variable from one whose value is the
           empty string. *)
        let orig = getenv_string k in
        Unix.putenv k tmp;
        (k, orig, tmp))
      env
  in
  Promise.protect
    ~finally:(fun () ->
      List.iter
        (fun (k, orig, tmp) ->
          let cur = getenv_string k in
          Unix.putenv k orig;
          if cur <> tmp then
            Error.fail_test
              (sprintf
                 "The environment variable %S was modified by the test \
                  function but not restored. Its value is %S but %S was \
                  expected."
                 k cur tmp))
        envs;
      Promise.return ())
    func

(**************************************************************************)
(* Output masking *)
(**************************************************************************)

let mask_line ?(mask = "<MASKED>") ?(after = "") ?(before = "") () =
  let pat =
    Printf.sprintf {|%s[^\n]*%s|} (Re.Pcre.quote after) (Re.Pcre.quote before)
  in
  let rex = Re.Pcre.regexp pat in
  let subst _matched = after ^ mask ^ before in
  fun subj -> Re.Pcre.substitute ~rex ~subst subj

(*
   Find all the matches for the pattern in the subject string and replace them
   by the mask string:
   - If the pattern contains a group and the first group matches, only this
     matched group is replaced by the mask.
   - Otherwise, the whole match is replaced by the mask.
*)
let mask_pcre_pattern ?replace pat =
  let re = Re.Pcre.regexp pat in
  let replace =
    match replace with
    | None -> fun _ -> "<MASKED>"
    | Some replace -> replace
  in
  fun subj ->
    Re.split_full re subj
    |> Helpers.list_map (function
         | `Text (* unmatched input *) str -> str
         | `Delim (* match *) groups ->
             let match_start, match_stop =
               try Re.Group.offset groups 0 with
               | Not_found -> assert false
             in
             let group_start, group_stop =
               try Re.Group.offset groups 1 with
               | Not_found -> (match_start, match_stop)
             in
             assert (group_start >= match_start);
             assert (group_stop <= match_stop);
             let frag1 =
               String.sub subj match_start (group_start - match_start)
             in
             let to_be_replaced =
               String.sub subj group_start (group_stop - group_start)
             in
             let frag2 = String.sub subj group_stop (match_stop - group_stop) in
             frag1 ^ replace to_be_replaced ^ frag2)
    |> String.concat ""

let represent_lines_as_pairs (xs : Re.Pcre.split_result list) :
    (string * string) list =
  let rec map acc (xs : Re.Pcre.split_result list) =
    match xs with
    | Text line :: Delim newline :: xs -> map ((line, newline) :: acc) xs
    | Delim newline :: xs -> map (("", newline) :: acc) xs
    | Text line :: xs -> map ((line, "") :: acc) xs
    | (Group _ | NoGroup) :: _ -> map acc xs
    | [] -> List.rev acc
  in
  map [] xs

(* Respect Unix (\n) and Windows line endings (\r\n) *)
let parse_lines : string -> (string * string) list =
  let line_sep = Re.Pcre.regexp "\r?\n" in
  fun str -> str |> Re.Pcre.full_split ~rex:line_sep |> represent_lines_as_pairs

let concatenate_lines (lines : (string * string) list) : string =
  let buf = Buffer.create 500 in
  List.iter
    (fun (line, newline) ->
      Buffer.add_string buf line;
      Buffer.add_string buf newline)
    lines;
  Buffer.contents buf

let filter_map_lines func str =
  let lines = parse_lines str in
  List.filter_map
    (fun (line, newline) ->
      match func line with
      | None -> None
      | Some line -> Some (line, newline))
    lines
  |> concatenate_lines

let contains_pcre_pattern ~pat =
  let rex = Re.Pcre.regexp pat in
  fun str -> Re.Pcre.pmatch ~rex str

let contains_substring ~sub = contains_pcre_pattern ~pat:(Re.Pcre.quote sub)

let remove_matching_lines cond str =
  filter_map_lines (fun line -> if cond line then None else Some line) str

let keep_matching_lines cond str =
  remove_matching_lines (fun x -> not (cond x)) str

let path_segment_re = Re.Pcre.regexp {|[^\\/]+|}

(* Internal function.
   Replace "/tmp/a/b" with "<TMP>/<MASKED>/<MASKED>" *)
let default_replace_path ~temp_dir str =
  let prefix_len = String.length !!temp_dir in
  let len = String.length str in
  assert (prefix_len <= len);
  let suffix = String.sub str prefix_len (len - prefix_len) in
  let new_suffix =
    Re.Pcre.substitute ~rex:path_segment_re
      ~subst:(fun _seg -> "<MASKED>")
      suffix
  in
  "<TMP>" ^ new_suffix

let path_character_range = {|/\\:A-Za-z0-9_.-|}
let path_character = "[" ^ path_character_range ^ "]"
let nonpath_character = "[^" ^ path_character_range ^ "]"

let mask_temp_paths ?(depth = Some 1) ?replace
    ?(temp_dir = Filename_.get_temp_dir_name ()) () =
  let temp_dir = Fpath.rem_empty_seg temp_dir in
  let temp_dir_pat = Re.Pcre.quote !!temp_dir in
  let suffix_pat =
    match depth with
    | None -> sprintf {|%s*|} path_character
    | Some n ->
        if n < 0 then
          Error.invalid_arg ~__LOC__
            "Testo.mask_temp_paths: depth must be (Some <nonzero>) or None"
        else
          let sep = {|[/\\]+|} in
          let segment = {|[A-Za-z0-9_.-]+|} in
          let repeat pat = Printf.sprintf "(?:%s){0,%d}" pat n in
          repeat (sep ^ segment)
  in
  let pat =
    let not_preceded_by_a_path_character =
      (* ocaml-re doesn't support lookbehind assertions so we match this
         pattern to ensure we don't start the match in a middle of a path. *)
      sprintf {|(?:^|%s)|} nonpath_character
    in
    (* The captured group in parentheses is what will get replaced by the
       'replace' function. It's a full path. *)
    sprintf "%s(%s%s)" not_preceded_by_a_path_character temp_dir_pat suffix_pat
  in
  let replace =
    match replace with
    | None -> default_replace_path ~temp_dir
    | Some f -> f
  in
  mask_pcre_pattern ~replace pat

let mask_not_pcre_pattern ?(mask = "<MASKED>") pat =
  let re = Re.Pcre.regexp pat in
  fun subj ->
    Re.split_full re subj
    |> Helpers.list_map (function
         | `Text _ -> mask
         | `Delim groups -> (
             match Re.Group.get_opt groups 0 with
             | Some substring -> substring
             | None -> (* assert false *) ""))
    |> String.concat ""

let mask_not_substrings ?mask substrings =
  mask_not_pcre_pattern ?mask
    ((* Sort the substrings by decreasing length so as to give preference to
        the longest match possible when two of them share a prefix. *)
     substrings
    |> List.stable_sort (fun a b ->
           Int.compare (String.length b) (String.length a))
    |> Helpers.list_map Re.Pcre.quote
    |> String.concat "|")

let mask_not_substring ?mask substring = mask_not_substrings ?mask [ substring ]
let has_tag tag test = List.mem tag test.tags

let categorize name (tests : _ list) : _ list =
  Helpers.list_map (fun x -> update x ~category:(name :: x.category)) tests

let categorize_suites name (tests : t list list) : t list =
  tests |> Helpers.list_flatten |> categorize name

(*
   Sort by category and test name.
*)
let sort (tests : t list) : t list =
  tests
  |> List.stable_sort (fun a b ->
         let c = compare a.category b.category in
         if c <> 0 then c else String.compare a.name b.name)

let to_alcotest = Run.to_alcotest
let registered_tests : t list ref = ref []
let register x = registered_tests := x :: !registered_tests

let test ?category ?checked_output ?expected_outcome ?normalize ?skipped ?solo
    ?tags ?tolerate_chdir name func =
  create ?category ?checked_output ?expected_outcome ?normalize ?skipped ?solo
    ?tags ?tolerate_chdir name func
  |> register

let get_registered_tests () = List.rev !registered_tests

let interpret_argv ?argv ?default_workers ?expectation_workspace_root
    ?handle_subcommand_result ?status_workspace_root ~project_name get_tests =
  Cmd.interpret_argv ?argv ?default_workers ?expectation_workspace_root
    ?handle_subcommand_result ?status_workspace_root ~project_name get_tests
