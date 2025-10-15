(*
   Text highlighting
*)

open Printf

type color = Default | Red | Green | Yellow | Cyan | Bold | Faint

let ansi_code_of_style = function
  | Default -> "0"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Cyan -> "36"
  | Bold -> "1"
  | Faint -> "2"

let color color str =
  Printf.sprintf "\027[%sm%s\027[%sm" (ansi_code_of_style color) str
    (ansi_code_of_style Default)

let opt_color cond color_ str = if cond then color color_ str else str

(* Remove formatting. It's a bit ugly but convenient to
   compute the length of a sequence of characters. *)
let strip =
  let re = Re.Pcre.regexp "\027\\[[0-9]+m" in
  fun str -> Re.replace re ~f:(fun _ -> "") str

(*
   Return the number of code points assuming UTF-8-compatible encoding
   (and no surrogate pairs i.e. WTF-8).
*)
let utf8_length str =
  let count = ref 0 in
  String.iter
    (fun c ->
      if Char.code c <= 0b01111111 || Char.code c >= 0b11000000 then incr count)
    str;
  !count

(*
   The physical length of a rendered string assuming UTF-8 encoding,
   one character per UTF-8 code point, and a fixed width of 1 per character.
   The result will be wrong if the string contains control characters such
   as newlines.
*)
let graph_length str = strip str |> utf8_length

let pad str min_len =
  let len = graph_length str in
  if len >= min_len then str ^ " " else str ^ String.make (min_len - len) ' '

let left_col text = pad text 8

(* Same as String.make but repeats a multi-byte unit rather than a byte *)
let string_make len unit =
  let buf = Buffer.create (len * String.length unit) in
  for _ = 1 to len do
    Buffer.add_string buf unit
  done;
  Buffer.contents buf

let term_width = 80

let horizontal_line =
  let res = (string_make term_width "─" |> color Faint) ^ "\n" in
  fun () -> res

let frame str =
  let padded_str = pad str (term_width - 4) in
  let horizontal_line =
    string_make (String.length (strip padded_str) + 2) "─"
  in
  let top_line = sprintf "┌%s┐\n" horizontal_line |> color Faint in
  let bottom_line = sprintf "└%s┘\n" horizontal_line |> color Faint in
  let contents_line =
    sprintf "%s %s %s\n" (color Faint "│") padded_str (color Faint "│")
  in
  top_line ^ contents_line ^ bottom_line

let truncate_text ~decorate_comment ~decorate_data_fragment ~max_bytes str =
  match max_bytes with
  | None -> str
  | Some max_len ->
      let orig_len = String.length str in
      let max_len = max 0 max_len in
      if orig_len <= max_len then
        str
      else
        (* Let's keep the beginning and the end of the text since they're
           more likely to contain useful information than the middle.

           We don't care about breaking multibyte characters.
           UTF-8 decoders are expected to recover from broken multibyte
           sequences. *)
        let head_len = max_len / 2 in
        let tail_len = max_len - head_len in
        if head_len <= 0 || tail_len <= 0 then
          str
        else
          let mid_len = orig_len - head_len - tail_len in
          let head = String.sub str 0 head_len in
          let tail = String.sub str (head_len + mid_len) tail_len in
          let mid =
            sprintf "\n###### [hidden: %d bytes] ######\n"
              mid_len
          in
          let warning =
            sprintf "###### Warning: bytes %d-%d below were elided ######\n"
              head_len (head_len + mid_len - 1)
          in
          String.concat "" [
            decorate_comment warning;
            decorate_data_fragment head;
            decorate_comment mid;
            decorate_data_fragment tail
          ]

(*
   Add a trailing newline and indent each line.
*)
let quote_multiline_text =
  (* Indent by one space, similarly to 'diff -u' output *)
  let margin = " " in
  fun
    ?(decorate_comment = color Yellow)
    ?(decorate_data_fragment = fun str -> str)
    ?max_bytes str ->
    str
    |> truncate_text ~decorate_comment ~decorate_data_fragment ~max_bytes
    |> String.split_on_char '\n'
    |> Helpers.list_map (fun line -> margin ^ line ^ "\n")
    |> String.concat ""
