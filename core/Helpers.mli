(*
   Generic functions used by more than one module in this library
*)

(* Safe version of List.map for ocaml < 5 *)
val list_map : ('a -> 'b) -> 'a list -> 'b list

(* Safe version of List.flatten *)
val list_flatten : 'a list list -> 'a list

(* Create a directory if it doesn't exist.
   Also create its parents if they don't exist and 'recursive' is true.
*)
val make_dir_if_not_exists : ?recursive:bool -> Fpath.t -> unit

(* List files in a folder, returning them as a list of names.
   Files are sorted alphabetically and don't include "." or "..". *)
val list_files : Filename_.t -> string list

(* Delete files recursively *)
val remove_file_or_dir : Filename_.t -> unit

val contains_pcre_pattern : string -> (string -> bool)

val contains_substring : string -> (string -> bool)

val write_file : Fpath.t -> string -> unit
val read_file : Fpath.t -> string

(* Work with a temporary file, ensuring its eventual deletion. *)
val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> 'a Promise.t) -> 'a Promise.t
