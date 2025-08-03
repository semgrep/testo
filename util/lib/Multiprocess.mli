(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

(* Master side

   This code runs in the master process which is a client to the workers.

   TODO: move the code from Run.run_tests_in_workers to this module so as to
   not expose state-sensitive operations (a worker object can become closed
   or can be killed and recreated after a timeout)?
   We assume the following about Testo:
   - the full list of tests is known at the beginning. No need to support
     a mutable queue that might be refilled after starting the tests.
   So we could pass the list of tests to the 'Client.create' function
   and completely hide the operations on workers (write/read/close).
   The caller of run_tests_in_workers would still need to provide
   functions to report the progress (start test, end test, ...).
*)
module Client : sig
  type t
  type worker

  (*
     Create a set of workers. Each will execute a command derived from
     the same command as the current process but with modifications
     (e.g. '--worker' is added, a '--slice' option is added, ...)
  *)
  val create :
    test_timeout_secs:float option ->
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
