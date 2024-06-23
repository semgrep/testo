(*
   A slice is a range of tests defined by the slice number and the total
   number of slices such as "3/4" or "2/8".

   It is used to split a test suite into smaller test suites for
   parallel execution.
*)

(*
   A slice denoted "3/4" is the third slice out of four slices and is
   represented by { num = 3; out_of = 4 }.
*)
type t = private { num : int; out_of : int }

val of_string : string -> t option
val to_string : t -> string

(* Take a slice out of a list *)
val apply : t -> 'a list -> 'a list

(* Take a slice, then take a slice within that slice, and so on *)
val apply_slices : t list -> 'a list -> 'a list
val tests : (string * (unit -> unit)) list
