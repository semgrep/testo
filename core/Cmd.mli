(*
   Command-line interface generated for a test program.

   For now at least, this is a simplified interface over what Alcotest
   supports.
*)

type subcommand_result =
  | Run_result of Types.test_with_status list
  | Status_result of Types.test_with_status list
  | Approve_result

(*
   Run a test suite and return an exit code.

   Usage:

     Cmd.interpret_argv (fun env -> tests)
*)
val interpret_argv :
  ?argv:string array ->
  ?default_workers:int option ->
  ?expectation_workspace_root:Fpath.t ->
  ?handle_subcommand_result:(int -> subcommand_result -> unit) ->
  ?status_workspace_root:Fpath.t ->
  project_name:string ->
  ((string * string) list -> Types.test list) ->
  unit Promise.t
