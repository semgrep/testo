(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

(* Master side *)
module Client : sig
  type t
  type worker

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

  val worker_id : worker -> string
  val iter_workers : t -> (worker -> unit) -> unit
  val close_worker : worker -> unit

  (*
     Kill all the worker processes.
  *)
  val close : t -> unit

  (*
     Read the next available message from a worker, returning the worker's
     identifier and the message.

     A 'None' result indicates that all the workers are done.
  *)
  val read : t -> (worker * Msg_from_worker.t) option

  (*
     Send a request to an available worker.
     An available worker is one that's not currently running a test as
     determined by the messages we receive from it.
  *)
  val write : worker -> Msg_from_master.t -> unit
end

(* Worker side *)
module Server : sig
  (*
     Read a message from stdin, normally connected to the master process
     via a pipe. 'None' indicates that the input channel was closed.
  *)
  val read : unit -> Msg_from_master.t option

  (*
     In a worker, write a newline-delimited message to stdout.
  *)
  val write : Msg_from_worker.t -> unit

  (*
     In a worker, report an error and exit with a nonzero status.
  *)
  val fatal_error : string -> 'a
end
