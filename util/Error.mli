(*
   Error management and exception printing
*)

(*
   Same as 'Failure' but comes with a nicer pre-registered exception printer.
   That won't conflict with the user's exception printers.

   This exception is for any error that may be the user's fault.
*)
exception Testo_failure of string

(*
   'assert false' or internal 'invalid_arg' indicating a bug.

   This exception is for any error that is not the user's fault.
*)
exception Testo_internal_error of { loc: string; msg: string }

(* Raise the 'Testo_failure' exception. *)
val fail : string -> 'a

(* Raise the 'Testo_internal_error' exception. *)
val internal_error : __LOC__:string -> string -> 'a

(* Raise the 'Testo_internal_error' exception with an error message indicating
   that this shouldn't have happened. *)
val assert_false : __LOC__:string -> unit -> 'a

(* A replacement for the standard 'invalid_arg' function and exception. *)
val invalid_arg : __LOC__:string -> string -> 'a
