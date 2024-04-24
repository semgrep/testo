(*
   Extensions of the Fpath module which deals with file paths.
*)

(* Operators can be put in scope with 'open Fpath_.Operators' *)
module Operators : sig
  val ( // ) : Fpath.t -> Fpath.t -> Fpath.t
  val ( / ) : Fpath.t -> string -> Fpath.t

  (* to_string *)
  val ( !! ) : Fpath.t -> string
end

val to_string_list : Fpath.t list -> string list
