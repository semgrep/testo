(*
   Manage timers for the tests that have a time limit.
*)

open Testo_util
module T = Types

type timer = {
  test: T.test;
  worker: Multiprocess.Client.worker;
  start_time: float;
  max_duration: float;
}

(* test_id -> timer *)
type t = (string, timer) Hashtbl.t

let create () = Hashtbl.create 100

let add_test timers (test : T.test) worker =
  match test.max_duration with
  | None -> ()
  | Some max_duration ->
      let timer = {
        test;
        worker;
        start_time = Unix.gettimeofday ();
        max_duration;
      } in
      Hashtbl.replace timers test.id timer

let remove_test timers (test : T.test) =
  Hashtbl.remove timers test.id

let remove_timeouts timers =
  let now = Unix.gettimeofday () in
  let timeouts =
    Hashtbl.fold (fun _test_id timer acc ->
      (* not sure if it's safe to remove elements from the table while
         we're iterating over it, so we'll remove the timed-out elements in
         a second pass. *)
      let elapsed = now -. timer.start_time in
      if elapsed > timer.max_duration then
        timer :: acc
      else
        acc
    ) timers []
  in
  List.iter (fun timer -> remove_test timers timer.test) timeouts;
  Helpers.list_map (fun timer ->
    (timer.test, timer.max_duration, timer.worker)
  ) timeouts
