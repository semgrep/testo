(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

(* Master side

   This code runs in the master process which is a client to the workers.
   The child process will initialize from argv, collecting the list
   of tests again. If the list of tests is obtained by scanning the
   filesystem for test cases, then this discovery will take place again
   in each new worker.

   Here, all we need to know about tests is their unique identifier
   provided by 'test_test_id'.
*)
module Client : sig
  type worker

  val run_tests_in_workers :
    argv:string array ->
    get_test_id:('test -> string) ->
    get_timed_out_workers:(unit -> (worker * (unit -> unit)) list) ->
    num_workers:int ->
    on_end_test:('test -> unit) ->
    on_start_test:(worker option -> 'test -> unit) ->
    test_list_checksum:string ->
    'test list ->
    (unit, string) result
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
