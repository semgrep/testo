(*
   Show the differences between the contents of two files
*)

val files : ?color:bool -> Fpath.t -> Fpath.t -> string option
(** Compare two files and show differences as lines starting with [+] or [-],
    optionally in color. The output format resembles the Unified format.

    If the two files use different line endings, the line endings are shown in
    diffs.

    If the two files differ only by their line endings, a special message is
    shown instead.

    The return value is [Some formatted_diffs] if the files differ, [None]
    otherwise. *)

type eol = LF | CRLF | No_EOL

val lines :
  ?color:bool ->
  ?path1:Fpath.t ->
  ?path2:Fpath.t ->
  ?show_eol:bool ->
  (string * eol) array ->
  (string * eol) array ->
  string
