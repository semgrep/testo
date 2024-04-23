(**
   {1 Test tags}

   A tag contributes to describing what a test is about in addition to
   the test's name and category. Additionally, it allows for the precise
   selection of a group of tests via the command line.
*)

(**
   Tags are strings. This is nice and extensible, but to prevent
   misspellings and conflicts, we require them to be registered
   using {!declare}.
*)
type t = private string

(** Create and register a tag. This function raises exceptions if used
    improperly.

    A tag must be created exactly once. It must be a non-empty dot-separated
    sequence of lowercase alphanumeric identifiers ([[a-z_][a-z_0-9]*]).
*)
val declare : string -> t

(** List all the registered tags. *)
val list : unit -> t list

(** Compare two tags in ASCII (alphabetic) order. *)
val compare : t -> t -> int

(** Determine if two tags are equal. *)
val equal : t -> t -> bool

(** Convert the tag back to the original string. *)
val to_string : t -> string

(** Convert the tag to human-readable form (which happens to be the same as
    the original string). *)
val show : t -> string
