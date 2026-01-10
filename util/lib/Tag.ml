(*
   Test tags
*)

open Printf

type t = (* private *) string

type query =
  | Has_tag of t
  | All
  | None
  | Not of query
  | And of query * query
  | Or of query * query

let compare = String.compare
let equal = String.equal
let show x = x
let to_string x = x

(*
   The tag syntax is a dot-separated identifier similar to pytest markers.
   coupling: update the error message below when changing this syntax
*)
let tag_syntax = {|\A[a-z_][a-z_0-9]*(?:[.][a-z_][a-z_0-9]*)*\z|}

let has_valid_tag_syntax =
  (* We use the same regexp library as Alcotest to facilitate
     future integration efforts. *)
  let re = Re.Pcre.regexp tag_syntax in
  fun tag ->
    match tag with
    | "and"
    | "or"
    | "not"
    | "all"
    | "none" ->
        false
    | tag -> Re.execp re tag

let check_tag_syntax tag =
  if not (has_valid_tag_syntax tag) then
    Error.invalid_arg ~__LOC__
      (sprintf
         "Testo.declare_tag: invalid syntax for test tag '%s'.\n\
          A tag must be a dot-separated sequence of one or more lowercase \
          identifiers of the form '[a-z_][a-z_0-9]*' such as \
          'foo_bar.v2.todo'. Additionally, a tag may not be a reserved keyword \
          ('and', 'or', 'not', 'all', 'none')."
         tag)

(* no duplicates are allowed *)
let declared_tags : (t, unit) Hashtbl.t = Hashtbl.create 100
let of_string_opt str = if Hashtbl.mem declared_tags str then Some str else None

let of_string_exn str =
  match of_string_opt str with
  | Some tag -> tag
  | None -> failwith ("Not a valid tag for this test suite: " ^ str)

let declare tag =
  check_tag_syntax tag;
  if Hashtbl.mem declared_tags tag then
    Error.invalid_arg ~__LOC__
      (sprintf
         "Testo.declare_tag: tag %S was declared multiple times.\n\
          Each tag must be declared exactly once to avoid accidental conflicts."
         tag)
  else Hashtbl.add declared_tags tag ();
  tag

let list () =
  Hashtbl.fold (fun tag () acc -> tag :: acc) declared_tags []
  |> List.sort String.compare
