(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

type t

(*
   Create a set of workers. Each will execute a command derived from
   the same command as the current process but with modifications
   (e.g. '--worker' is added, a '--slice' option is added, ...)
*)
val create : num_workers:int -> original_argv:string array -> t

(*
   Read the next available message from a worker.
   A 'None' result indicates that all the workers are done.
*)
val read : t -> Message.t option
