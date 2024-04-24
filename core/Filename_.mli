(*
   A trivial wrapper around "string" to make signatures clearer.
   For internal use only.

   It would be nicer to use 'Fpath' but we're trying to minimize
   dependencies outside the standard library.
*)

type t

val of_string : string -> t
val to_string : t -> string

(* Functions from the 'Filename' module. *)
val basename : t -> string
val dirname : t -> t
val is_relative : t -> bool
val concat : t -> t -> t
val dir_sep : string
val temp_file : ?temp_dir:t -> string -> string -> t

(* Extensions *)
module Operators : sig
  (* Turn a, b into a/b; turn a, /b into /b.
     Same as ( // ) in Fpath. *)
  val ( // ) : t -> t -> t

  (* Append a path segment.
     Same as ( / ) in Fpath.
     The appended segment shouldn't contain slashes or backslashes
     but this isn't enforced. *)
  val ( / ) : t -> string -> t

  (* to_string *)
  val ( !! ) : t -> string
end
