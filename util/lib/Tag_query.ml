(*
   Query language for selecting tests based on their tags

   Sample input: (foo or bar) and not e2e
*)

open Printf

type t = Tag.query =
  | Has_tag of Tag.t
  | Not of t
  | And of t * t
  | Or of t * t

let parse str =
  try
    let lexbuf = Lexing.from_string str in
    let query = Tag_query_parser.main Tag_query_lexer.token lexbuf in
    Ok query
  with e ->
    Error (sprintf "Syntax error in tag query '%s': %s"
             str (Printexc.to_string e))

let rec match_ tags q =
  match q with
  | Has_tag t -> List.exists (Tag.equal t) tags
  | Not q -> not (match_ tags q)
  | And (a, b) -> match_ tags a && match_ tags b
  | Or (a, b) -> match_ tags a || match_ tags b

let needs_parens = function
  | Has_tag _
  | Not _ -> false
  | And _
  | Or _ -> true

let show q =
  let rec show buf = function
    | Has_tag x -> Buffer.add_string buf (Tag.show x)
    | Not x -> bprintf buf "not %a" show_inner x
    | And (a, b) -> bprintf buf "%a and %a" show_inner a show_inner b
    | Or (a, b) -> bprintf buf "%a or %a" show_inner a show_inner b
  and show_inner buf x =
    if needs_parens x then
      bprintf buf "(%a)" show x
    else
      show buf x
  in
  let buf = Buffer.create 100 in
  show buf q;
  Buffer.contents buf

let pp ppf q = Format.pp_print_string ppf (show q)
