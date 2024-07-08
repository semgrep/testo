(*
   Command-line interface generated for a test program.
*)

open Printf
open Testo_util
open Fpath_.Operators
open Cmdliner

(*
   Configuration object type that is used for all subcommands although
   not all of them use all the fields.
*)
type conf = {
  (* All subcommands *)
  debug : bool;
  filter_by_substring : string option;
  filter_by_tag : Tag.t option;
  env : (string * string) list;
  (* Run and Status *)
  show_output : bool;
  (* Status *)
  status_output_style : Run.status_output_style;
  (* Run *)
  argv : string array;
  lazy_ : bool;
  slice : Slice.t list;
  is_worker : bool;
  jobs : int option;
  test_list_checksum : string option;
}

let default_conf =
  {
    debug = false;
    filter_by_substring = None;
    filter_by_tag = None;
    env = [];
    show_output = false;
    status_output_style = Compact_important;
    argv = Sys.argv;
    lazy_ = false;
    slice = [];
    is_worker = false;
    jobs = None;
    test_list_checksum = None;
  }

(*
   Subcommands:
   - run
   - status
   - approve
*)
type cmd_conf = Run_tests of conf | Status of conf | Approve of conf

type subcommand_result =
  | Run_result of Types.test_with_status list
  | Status_result of Types.test_with_status list
  | Approve_result

type 'continuation_result test_spec =
  ((string * string) list -> Types.test list)
  * (int -> subcommand_result -> 'continuation_result)

(****************************************************************************)
(* Dispatch subcommands to do real work *)
(****************************************************************************)

let fatal_error msg =
  eprintf "Error: %s\n%!" msg;
  exit 1

let run_with_conf ((get_tests, handle_subcommand_result) : _ test_spec)
    (cmd_conf : cmd_conf) : unit =
  (*
     The creation of tests can take a while so it's delayed until we
     really need the tests. This makes '--help' fast.
  *)
  match cmd_conf with
  | Run_tests conf ->
      Debug.debug := conf.debug;
      Run.cmd_run ~always_show_unchecked_output:conf.show_output ~argv:conf.argv
        ~filter_by_substring:conf.filter_by_substring
        ~filter_by_tag:conf.filter_by_tag ~is_worker:conf.is_worker
        ~jobs:conf.jobs ~lazy_:conf.lazy_ ~slice:conf.slice
        ~test_list_checksum:conf.test_list_checksum (get_tests conf.env)
        (fun exit_code tests_with_status ->
          handle_subcommand_result exit_code (Run_result tests_with_status))
      |> (* TODO: ignoring this promise doesn't make sense.
            The whole Lwt support needs testing and probably doesn't
            work as is. If someone really needs it, please provide a test
            environment where it's justified i.e. where we can't
            call 'Lwt_main.run' so we can make this work. *) ignore
  | Status conf ->
      Debug.debug := conf.debug;
      let exit_code, tests_with_status =
        Run.cmd_status ~always_show_unchecked_output:conf.show_output
          ~filter_by_substring:conf.filter_by_substring
          ~filter_by_tag:conf.filter_by_tag
          ~output_style:conf.status_output_style (get_tests conf.env)
      in
      handle_subcommand_result exit_code (Status_result tests_with_status)
  | Approve conf ->
      Debug.debug := conf.debug;
      let exit_code =
        Run.cmd_approve ~filter_by_substring:conf.filter_by_substring
          ~filter_by_tag:conf.filter_by_tag (get_tests conf.env)
      in
      handle_subcommand_result exit_code Approve_result

(****************************************************************************)
(* Command-line options *)
(****************************************************************************)
(*
   Some of the command-line options are shared among subcommands.
*)

let debug_term : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:"Log information that can be useful for debugging the Testo library."
  in
  Arg.value (Arg.flag info)

let filter_by_substring_term : string option Term.t =
  let info =
    Arg.info
      [ "s"; "filter-substring" ]
      ~docv:"SUBSTRING" ~doc:"Select tests whose description contains $(docv)."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let check_tag filter_by_tag =
  filter_by_tag
  |> Option.map (fun str ->
         match Tag.of_string_opt str with
         | Some tag -> tag
         | None -> fatal_error (sprintf "Unknown or misspelled tag: %s" str))

(* This option currently supports only one tag. In the future, we might
   want to support boolean queries e.g. '-t "lang.python and not todo"' *)
let filter_by_tag_term : string option Term.t =
  let info =
    Arg.info [ "t"; "filter-tag" ] ~docv:"TAG"
      ~doc:
        "Select tests tagged with $(docv). Filtering by tag is generally more \
         robust than selecting tests by text contained in their name with \
         '-s'."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let show_output_term : bool Term.t =
  let info =
    Arg.info [ "w"; "show-output" ]
      ~doc:
        "Show the output of all tests rather than only showing the output of \
         the failed tests. This excludes the output (stdout, stderr, or both) \
         that may be captured explicitly to be compared against expectations."
  in
  Arg.value (Arg.flag info)

let verbose_run_term : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        "Print more details than by default. Currently, this is equivalent to \
         '--show-output' but it may be extended in the future to bundle up \
         more options."
  in
  Arg.value (Arg.flag info)

(* Converter for arguments of the form KEY=VALUE *)
let env_conv =
  let key_re = Re.Pcre.regexp {|\A[A-Za-z_][A-Za-z_0-9]*\z|} in
  let error str = Error (sprintf "Malformed KEY=VALUE pair: %s" str) in
  let parse str =
    match String.index_opt str '=' with
    | None -> error str
    | Some pos ->
        let key = String.sub str 0 pos in
        if not (Re.Pcre.pmatch ~rex:key_re key) then error str
        else
          let value = String.sub str (pos + 1) (String.length str - pos - 1) in
          Ok (key, value)
  in
  let print fmt (key, value) = Format.fprintf fmt "%s=%s" key value in
  Arg.conv' ~docv:"KEY=VALUE" (parse, print)

let env_term : (string * string) list Term.t =
  let info =
    Arg.info [ "e"; "env" ] ~docv:"KEY=VALUE"
      ~doc:
        "Pass a key/value pair to the function that creates the test suite. \
         KEY must be an alphanumeric identifier of the form \
         [A-Za-z_][A-Za-z_0-9]*. VALUE can be any string. This mechanism for \
         passing arbitrary runtime settings to the test suite is offered as a \
         safer alternative to environment variables. Multiple -e/--env options \
         are supported in the same command, each defining one key/value pair."
  in
  Arg.value (Arg.opt_all env_conv [] info)

(****************************************************************************)
(* Subcommand: run (replaces alcotest's 'test') *)
(****************************************************************************)

let jobs_term : int option Term.t =
  let info =
    Arg.info [ "j"; "jobs" ] ~docv:"NUM"
      ~doc:
        "Specify the number of jobs to run in parallel. By default, this value \
         is determined automatically. To enforce sequential execution, use '-j \
         0' or '-j 1'."
  in
  Arg.value (Arg.opt (Arg.some Arg.int) None info)

let lazy_term : bool Term.t =
  let info =
    Arg.info [ "lazy" ]
      ~doc:"Run only the tests that were not previously successful."
  in
  Arg.value (Arg.flag info)

(* Converter for arguments of the form NUM/NUM_SLICES *)
let slice_conv =
  let parse str =
    match Slice.of_string str with
    | None -> Error (sprintf "Malformed slice: %s" str)
    | Some x -> Ok x
  in
  let print fmt slice = Format.pp_print_string fmt (Slice.to_string slice) in
  Arg.conv' ~docv:"NUM/NUM_SLICES" (parse, print)

let slice_term : Slice.t list Term.t =
  let info =
    Arg.info [ "slice" ] ~docv:"NUM/NUM_SLICES"
      ~doc:
        "Divide the test suite into NUM_SLICES and work on slice NUM only \
         (1-based). For example, '1/4' is the first of four slices and '4/4' \
         is the last one. If multiple '--slice' options are specified, they \
         are applied sequentially from left to right. If the original suite \
         defines 23 tests, '--slice 2/4' selects tests [7,8,9,10,11,12] \
         (1-based). If additionally '--slice 1/3' is specified after it on the \
         same command line, the remaining tests will be [7,8]. This filter is \
         meant for running tests in parallel, possibly across multiple hosts. \
         It is applied after any other filters to as to divide the work more \
         evenly."
  in
  Arg.value (Arg.opt_all slice_conv [] info)

let test_list_checksum_term : string option Term.t =
  let info =
    Arg.info [ "test-list-checksum" ] ~docv:"STR"
      ~doc:
        "Internal use only. This is a checksum used to check that the list of \
         tests generated in a worker is the same as the list of tests \
         generated by the master."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let worker_term : bool Term.t =
  let info =
    Arg.info [ "worker" ]
      ~doc:"Internal option used to launch a parallel worker."
  in
  Arg.value (Arg.flag info)

let run_doc = "run the tests"

let subcmd_run_term ~argv (test_spec : _ test_spec) : unit Term.t =
  let combine debug env filter_by_substring filter_by_tag jobs lazy_ show_output
      slice test_list_checksum verbose worker =
    let show_output = show_output || verbose in
    Run_tests
      {
        default_conf with
        debug;
        env;
        filter_by_substring;
        filter_by_tag = check_tag filter_by_tag;
        is_worker = worker;
        jobs;
        lazy_;
        show_output;
        slice;
        test_list_checksum;
        argv;
      }
    |> run_with_conf test_spec
  in
  Term.(
    const combine $ debug_term $ env_term $ filter_by_substring_term
    $ filter_by_tag_term $ jobs_term $ lazy_term $ show_output_term $ slice_term
    $ test_list_checksum_term $ verbose_run_term $ worker_term)

let subcmd_run ~argv test_spec =
  let info = Cmd.info "run" ~doc:run_doc in
  Cmd.v info (subcmd_run_term ~argv test_spec)

(****************************************************************************)
(* Subcommand: status (replaces alcotest's 'list') *)
(****************************************************************************)

(*
   Design: the options '-l' and '-a' were chosen for two reasons:
   - make the status output compact by default;
   - adopt a similar behavior as the '-l' and '-a' options of 'ls'.
*)
let long_term : bool Term.t =
  let info =
    Arg.info [ "l"; "long" ]
      ~doc:"Print details instead of just a one-line summary for each test."
  in
  Arg.value (Arg.flag info)

let all_term : bool Term.t =
  let info =
    Arg.info [ "a"; "all" ]
      ~doc:
        "Report tests in all statuses instead of only the tests that\n\
        \            need attention."
  in
  Arg.value (Arg.flag info)

let verbose_status_term : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        "Report the status of the tests with maximum verbosity.\n\
        \            This is currently equivalent to '-alw'."
  in
  Arg.value (Arg.flag info)

let status_doc = "show test status"

let subcmd_status_term tests : unit Term.t =
  let combine all debug env filter_by_substring filter_by_tag long show_output
      verbose =
    let status_output_style : Run.status_output_style =
      if verbose then Long_all
      else
        match (long, all) with
        | true, true -> Long_all
        | false, true -> Compact_all
        | true, false -> Long_important
        | false, false -> Compact_important
    in
    let show_output = show_output || verbose in
    Status
      {
        default_conf with
        debug;
        env;
        filter_by_substring;
        filter_by_tag = check_tag filter_by_tag;
        show_output;
        status_output_style;
      }
    |> run_with_conf tests
  in
  Term.(
    const combine $ all_term $ debug_term $ env_term $ filter_by_substring_term
    $ filter_by_tag_term $ long_term $ show_output_term $ verbose_status_term)

let subcmd_status tests =
  let info = Cmd.info "status" ~doc:status_doc in
  Cmd.v info (subcmd_status_term tests)

(****************************************************************************)
(* Subcommand: approve *)
(****************************************************************************)

let approve_doc = "approve new test output"

let subcmd_approve_term tests : unit Term.t =
  let combine debug env filter_by_substring filter_by_tag =
    Approve
      {
        default_conf with
        debug;
        env;
        filter_by_substring;
        filter_by_tag = check_tag filter_by_tag;
      }
    |> run_with_conf tests
  in
  Term.(
    const combine $ debug_term $ env_term $ filter_by_substring_term
    $ filter_by_tag_term)

let subcmd_approve tests =
  let info = Cmd.info "approve" ~doc:approve_doc in
  Cmd.v info (subcmd_approve_term tests)

(****************************************************************************)
(* Main command *)
(****************************************************************************)

let root_doc ~project_name = sprintf "run tests for %s" project_name

let root_man ~project_name : Manpage.block list =
  [
    `S Manpage.s_description;
    `P
      (sprintf
         {|This is the program built for running and managing the tests for this project,
%s. It revolves around 3 main subcommands: 'run', 'status', and 'approve'.
Use the 'status' subcommand to check the status of each test without having
to re-run them. 'approve' must be used on tests whose output is captured
so as to make their latest output the new reference.
|}
         project_name);
    `P
      (sprintf
         {|This test program was configured to store the temporary results in
'%s' and the expected test output in the persistent folder '%s'.
The latter should be kept under version control (git or similar).
|}
         !!(Store.get_status_workspace ())
         !!(Store.get_expectation_workspace ()));
  ]

let root_info ~project_name =
  let name = Filename.basename Sys.argv.(0) in
  Cmd.info name ~doc:(root_doc ~project_name) ~man:(root_man ~project_name)

let root_term ~argv test_spec =
  (*
  Term.ret (Term.const (`Help (`Pager, None)))
*)
  subcmd_run_term ~argv test_spec

let subcommands ~argv test_spec =
  [
    subcmd_run ~argv test_spec;
    subcmd_status test_spec;
    subcmd_approve test_spec;
  ]

let with_record_backtrace func =
  let original_state = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace original_state) func

(*
     $ cmdliner-demo-subcmd           -> parsed as root subcommand
     $ cmdliner-demo-subcmd --help    -> also parsed as root subcommand
     $ cmdliner-demo-subcmd subcmd1   -> parsed as 'subcmd1' subcommand

   If there is a request to display the help page, it displayed at this point,
   returning '`Help'.

   Otherwise, 'conf' is returned to the application.
*)
let interpret_argv ?(argv = Sys.argv) ?expectation_workspace_root
    ?(handle_subcommand_result = fun exit_code _ -> exit exit_code)
    ?status_workspace_root ~project_name
    (get_tests : (string * string) list -> Types.test list) =
  (* TODO: is there any reason why we shouldn't always record a stack
     backtrace when running tests? *)
  let test_spec = (get_tests, handle_subcommand_result) in
  with_record_backtrace (fun () ->
      Store.init_settings ?expectation_workspace_root ?status_workspace_root
        ~project_name ();
      Cmd.group
        ~default:(root_term ~argv test_spec)
        (root_info ~project_name)
        (subcommands ~argv test_spec)
      |> Cmd.eval ~argv |> (* does not reach this point by default *) exit)
