(*
   Manage timers for the tests that have a time limit.
*)

(* The set of tests running in separate worker processes
   that are currently being timed. Periodically, the master process will
   check for timeouts by invoking remove_timeouts.
*)
type t

(* Create a table holding the timers. There's normally just one such
   table in the master process. *)
val create : unit -> t

(* Add a test and the worker running the test so we can kill that
   worker if the test times out. *)
val add_test : t -> Types.test -> Testo_util.Multiprocess.Client.worker -> unit

(* Remove the timer associated with a test, if any. This is normally
   called when a test terminates. *)
val remove_test : t -> Types.test -> unit

(* Identify all the running tests that may have timed out and remove
   them from the set of timers.
   The float is the maximum duration of the test, not the actual duration.
*)
val remove_timeouts :
  t -> (Types.test * float * Testo_util.Multiprocess.Client.worker) list
