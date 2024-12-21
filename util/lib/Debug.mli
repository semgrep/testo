(*
   Log messages for debugging purposes

   This is mostly for tracing the master/worker interactions.
*)

(* This is turned on with '--debug' *)
val debug : bool ref

(* Log a message on stderr iff !debug is true. *)
val log : (unit -> string) -> unit
