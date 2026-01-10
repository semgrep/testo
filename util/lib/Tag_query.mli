(*
   Query language for selecting tests based on their tags

   Sample input: (foo or bar) and not e2e
*)

type t = Tag.query =
  | Has_tag of Tag.t
  | All
  | None
  | Not of t
  | And of t * t
  | Or of t * t

val parse : string -> (t, string) result
val match_ : Tag.t list -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit
