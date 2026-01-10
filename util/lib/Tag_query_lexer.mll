(*
   Tokenizer for tag queries
*)
{
  open Printf
  open Tag_query_parser
}
rule token = parse
  | [' ' '\t' '\n']   { token lexbuf }
  | "or"              { OR }
  | "and"             { AND }
  | "not"             { NOT }
  | "all"             { ALL }
  | "none"            { NONE }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  (* Tag regexp: must come after the alphanumeric keywords (or, and, not) *)
  | ['a'-'z''_']['a'-'z''_''0'-'9']* as tag_str {
      TAG (Tag.of_string_exn tag_str)
    }
  | eof               { EOF }
  | _ as c            { failwith (sprintf
                                    "invalid character in tag query: %C" c) }
