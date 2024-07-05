(*
   Read and write messages between worker and master
*)

(* A message produced by a worker (server) to notify the master (client)

   Start_test test_id: notifies the master that the test started.
   End_test test_id: notifies the master the test ended.
   End: notifies the master that the worker is done running all the tests
        and is about to terminate.
   Junk: any line of input that couldn't be parsed.

   It is the client's responsibility to obtain and report test details.
*)
type t = Start_test of string | End_test of string | End | Junk of string

(* Parse a line of input. The input string may not contain a newline
   character. *)
val of_string : string -> t

(* Create a message. The result does not include the trailing newline
   that terminates the message. *)
val to_string : t -> string
