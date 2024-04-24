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
val make_dir_if_not_exists : ?recursive:bool -> Filename_.t -> unit

val contains_pcre_pattern : string -> (string -> bool)

val contains_substring : string -> (string -> bool)

(* Work with a temporary file, ensuring its eventual deletion. *)
val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:string ->
  (string -> 'a Promise.t) -> 'a Promise.t
