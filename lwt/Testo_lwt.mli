(*
   Run tests that return Lwt promises

   This is specialization of the few generic types and functions from
   the core library's module Testo.
*)

(* The type of a test that returns an Lwt promise. *)
type test = unit Lwt.t Testo.t

(* Operations over Lwt promises *)
val mona : unit Lwt.t Testo.Mona.t

(* Create a test that returns a promise.
   See the documentation for Testo.create. *)
val create :
  ?category:string list ->
  ?checked_output:Testo.output_kind ->
  ?expected_outcome:Testo.expected_outcome ->
  ?normalize:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Testo.Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> unit Lwt.t) ->
  test

(* Interpret the command line and do something with the test suite.
   See the documentation for Testo.interpret_argv. *)
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:
    (int -> unit Lwt.t Testo.subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  project_name:string ->
  (unit -> test list) ->
  unit Lwt.t
