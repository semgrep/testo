(*
   Text highlighting
*)

type color = Default | Red | Green | Yellow | Cyan | Bold | Faint

val color : color -> string -> string

(* Add color iff the condition is true *)
val opt_color : bool -> color -> string -> string

(*
   Turn a string containing no newlines into a framed string.
   The result is LF-terminated.
*)
val frame : string -> string

(*
   Return an LF-terminated line.
*)
val horizontal_line : unit -> string

(*
   Pad text to be the correct width for the left column.
*)
val left_col : string -> string

(*
   Print multiline text with an indentation. Always adds a trailing newline.

   The midsection is elided if the input string exceeds 'max_bytes'.
   Data fragments can be highlighted in a different color than the
   text we insert to signal the elision.

   Pros: disambiguates the quoted text from other output
   Cons: interferes with proper copy-pasting (since it inserts whitespace)
*)
val quote_multiline_text :
  ?decorate_comment:(string -> string) ->
  ?decorate_data_fragment:(string -> string) ->
  ?max_bytes:int ->
  string ->
  string
