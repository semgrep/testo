(*
   Error management and exception printing
*)

(*
   An exception that can be used in a test function to signal a test failure.
   The advantage over using the built-in 'Failure' exception is that
   it indicates clearly that the error was raised by test code.
*)
exception Test_failure of string

(*
   Same as 'Failure' but comes with a nicer pre-registered exception printer.
   That won't conflict with the user's exception printers.

   This exception is for any error that may be the user's fault.
*)
exception User_error of string

(*
   'assert false' or internal 'invalid_arg' indicating a bug.

   This exception is for any error that is not the user's fault.
*)
exception Internal_error of { loc : string; msg : string }

(* Raise the 'Test_failure' exception. *)
val fail_test : string -> 'a

(* Raise the 'User_error' exception. *)
val user_error : string -> 'a

(* Raise the 'Testo_internal_error' exception. *)
val internal_error : __LOC__:string -> string -> 'a

(* Raise the 'Testo_internal_error' exception with an error message indicating
   that this shouldn't have happened. *)
val assert_false : __LOC__:string -> unit -> 'a

(* A replacement for the standard 'invalid_arg' function and exception. *)
val invalid_arg : __LOC__:string -> string -> 'a

module Exit_code : sig
  val success : int

  (* one or more tests failed *)
  val test_failure : int

  (* misconfiguration or other misuse fixable by the user *)
  val configuration_error : int

  (* other errors: bugs, broken files reserved for Testo's use *)
  val internal_error : int
end
