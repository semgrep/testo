(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

(* Master side *)
module Client : sig
  type t

  (*
     Create a set of workers. Each will execute a command derived from
     the same command as the current process but with modifications
     (e.g. '--worker' is added, a '--slice' option is added, ...)
  *)
  val create :
    num_workers:int ->
    original_argv:string array ->
    test_list_checksum:string ->
    t

  (*
     Kill the worker processes.
  *)
  val close : t -> unit

  (*
     Read the next available message from a worker, returning the worker's
     identifier and the message.

     A 'None' result indicates that all the workers are done.
  *)
  val read : t -> (string * Message.t) option
end

(* Worker side *)
module Server : sig
  (*
     In a worker, write a newline-delimited message to stdout.
  *)
  val write : Message.t -> unit

  (*
     In a worker, report an error and exit with a nonzero status.
  *)
  val fatal_error : string -> 'a
end
