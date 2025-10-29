(**
   {1 Test tags}

   A tag contributes to describing what a test is about in addition to
   the test's name and category. Additionally, it allows for the precise
   selection of a group of tests via the command line.
*)

type t = private string
(**
   Tags are strings. This is nice and extensible, but to prevent
   misspellings and conflicts, we require them to be registered
   using {!declare}.
*)

type query =
  | Has_tag of t
  | All
  | None
  | Not of query
  | And of query * query
  | Or of query * query
(** Type of a query for selecting tests based on their tags *)

val declare : string -> t
(** Create and register a tag. This function raises exceptions if used
    improperly.

    A tag must be created exactly once. It must be a non-empty dot-separated
    sequence of lowercase alphanumeric identifiers ([[a-z_][a-z_0-9]*]).
*)

val list : unit -> t list
(** List all the registered tags. *)

val compare : t -> t -> int
(** Compare two tags in ASCII (alphabetic) order. *)

val equal : t -> t -> bool
(** Determine if two tags are equal. *)

val to_string : t -> string
(** Convert the tag back to the original string. *)

val of_string_opt : string -> t option
(** Convert a tag from a string, failing is the tag wasn't declared. *)

val of_string_exn : string -> t
(** Convert a tag from a string, raising a [Failure] exception is the tag
    wasn't declared. *)

val show : t -> string
(** Convert the tag to human-readable form (which happens to be the same as
    the original string). *)
