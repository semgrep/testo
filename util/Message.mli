(*
   Read and write messages between worker and master
*)

(* A message produced by a worker (server) to notify the master (client)

   Start test_id: notifies the master that the test started.
   End test_id: notifies the master the test ended.

   It is the client's responsibility to obtain and report test details.
*)
type t = Start_test of string | End_test of string | End

(* Parse a line of input. The input string may not contain a newline
   character. *)
val of_string : string -> t option

(* Create a message. The result does not include the trailing newline
   that terminates the message. *)
val to_string : t -> string
