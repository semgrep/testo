(*
   Generic functions used by more than one module in this library
*)

(* Safe version of List.map for ocaml < 5 *)
val list_map : ('a -> 'b) -> 'a list -> 'b list

(* Safe version of List.flatten *)
val list_flatten : 'a list list -> 'a list

(* Split ok results from errors *)
val split_result_list : ('a, 'b) Result.t list -> 'a list * 'b list

(* String.for_all is only available from OCaml 4.13 *)
val string_for_all : (char -> bool) -> string -> bool

(* Create a directory if it doesn't exist.
   Also create its parents if they don't exist and 'recursive' is true.
*)
val make_dir_if_not_exists : ?recursive:bool -> Fpath.t -> unit

(* List files in a folder, returning them as a list of names.
   Files are sorted alphabetically and don't include "." or "..". *)
val list_files : Fpath.t -> string list

(* Similar to In_channel.input_all which is available starting with
   OCaml 4.14 *)
val input_all : in_channel -> string

(* Delete files recursively *)
val remove_file_or_dir : Fpath.t -> unit
val contains_pcre_pattern : pat:string -> string -> bool
val contains_substring : sub:string -> string -> bool
val write_text_file : Fpath.t -> string -> unit
val read_text_file : Fpath.t -> string
val map_text_file : (string -> string) -> Fpath.t -> Fpath.t -> unit
val copy_text_file : Fpath.t -> Fpath.t -> unit

(* Change the current directory temporarily *)
val with_chdir : Fpath.t -> (unit -> 'a) -> 'a
val with_opt_chdir : Fpath.t option -> (unit -> 'a) -> 'a
