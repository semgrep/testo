(*
   Messages sent from the master process to a worker.
*)

(*
   The master is in charge of sending the test ID of a test to run
   as soon as the worker is available.

   Start_test test_id: notifies the worker that is should run this test.
   End: notifies the worker that it can exit.
*)
type t = Start_test of string

val of_string : string -> t
val to_string : t -> string
