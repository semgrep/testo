(*
   Show the differences between the contents of two files
*)

(*
   Compare two files and show differences as lines starting with
   '+' or '-', optionally in color.
   This uses the widely used Unified format.

   The return value is (equal, formatted_diffs).
*)
val files : ?color:bool -> Fpath.t -> Fpath.t -> bool * string

val lines :
  ?color:bool ->
  ?path1:Fpath.t ->
  ?path2:Fpath.t ->
  string array ->
  string array ->
  string
