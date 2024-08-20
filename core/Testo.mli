(**
   Testo library - Utilities for writing OCaml test suites
*)

(*
   This is the only module exported by this library. Other modules are
   either hidden or included as submodules such as 'Tag'
   which is exposed as 'Testo.Tag'. This allows us to:
   - use dedicated file-modules for data structures without cramming
     everything into this single file such as Tag module providing Tag.t
   - hide internal modules that shouldn't be accessed by users of the library

   Dune exposes only this module as long as its name matches the name of the
   library.
*)

(****************************************************************************)
(** {1 Internal types}

    These types are documented in the library's source code in [Types.ml].
    They are subject to frequent and unannounced changes at the whim of the
    library's authors. A casual user should not need them.
*)

type expected_outcome =
  | Should_succeed
  | Should_fail of string  (** explains why we expect this test to fail *)

type outcome = Succeeded | Failed

type captured_output =
  | Ignored of string  (** unchecked combined output *)
  | Captured_stdout of string * string  (** stdout, unchecked output *)
  | Captured_stderr of string * string  (** stderr, unchecked output *)
  | Captured_stdout_stderr of string * string  (** stdout, stderr *)
  | Captured_merged of string  (** combined output *)

type expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string  (** stdout, stderr *)
  | Expected_merged of string  (** combined output *)

type result = { outcome : outcome; captured_output : captured_output }
type missing_files = Types.missing_files = Missing_files of Fpath.t list

type expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, missing_files) Result.t;
}

type status = {
  expectation : expectation;
  result : (result, missing_files) Result.t;
}

type fail_reason = Exception | Wrong_output | Exception_and_wrong_output

type status_class =
  | PASS
  | FAIL of fail_reason
  | XFAIL of fail_reason
  | XPASS
  | MISS

type status_summary = {
  status_class : status_class;
  has_expected_output : bool;
}

(****************************************************************************)
(** {1 Main interface } *)
(****************************************************************************)

(** {2 Test creation} *)

type checked_output_kind
(** This type specifies what part of the output of a test (stdout, stderr)
    should be captured and compared against expectations.

    Use the provided functions {!val:stdout}, {!val:stderr}, {!val:stdxxx},
    and {!val:split_stdout_stderr} to create such an object.
*)

val stdout : ?expected_stdout_path:Fpath.t -> unit -> checked_output_kind
(** Create an object of type {!type:checked_output_kind} specifying
    that the test's standard output must be checked against a reference file.
*)

val stderr : ?expected_stderr_path:Fpath.t -> unit -> checked_output_kind
(** Same as {!val:stdout} but for capturing stderr instead. *)

val stdxxx : ?expected_stdxxx_path:Fpath.t -> unit -> checked_output_kind
(** Same as {!val:stdout} but for capturing the combined stdout and stderr
    outputs. *)

val split_stdout_stderr :
  ?expected_stdout_path:Fpath.t ->
  ?expected_stderr_path:Fpath.t ->
  unit ->
  checked_output_kind
(** Same as {!val:stdxxx} but keep stdout and stderr separate. *)

module Promise : module type of Promise
(** Wrapper allowing for asynchronous test functions (Lwt and such). *)

module Tag : module type of Testo_util.Tag
(** The type of tags which can be used to define subsets of tests precisely. *)

type t = private {
  id : string;
      (** Hash of the full name of the test, computed automatically. *)
  internal_full_name : string;
      (** Full name of the test, derived automatically from category and name. *)
  category : string list;
      (** Categories are made for organizing tests as a tree which is useful
        for display and filtering. A new category is created typically when
        grouping multiple test suites into one with 'categorize_suites'
        or when assigning a category to a list of tests with 'categorize'.
        e.g. [["food"; "fruit"; "kiwi"]] *)
  name : string;
  func : unit -> unit Promise.t;
  (***** Options *****)
  expected_outcome : expected_outcome;
  tags : Tag.t list;  (** Tags must be declared once using [create_tag]. *)
  normalize : (string -> string) list;
      (** An optional function to rewrite any output data so as to mask the
        variable parts. *)
  checked_output : checked_output_kind;
  skipped : string option;
      (** If not [None], the [skipped] property causes a test to be skipped
          by Alcotest but still shown as ["[SKIP]"] rather than being
          omitted. The string should give a reason why the test is being
          skipped. *)
  solo : string option;
      (** If not [None], this test will never run concurrently with other
          tests. The string should give a reason why the test should not
          run in parallel with other tests. *)
  tolerate_chdir : bool;
      (** If the test function changes the current directory without restoring
          it, it's an error unless this flag is set. All the tests in a test
          suite should share this field. *)
  tracking_url : string option;
      (** A link to the relevant entry in a bug tracking system. *)
}
(**
   [t] is the type of a test. A test suite is a flat list of tests.

   A test is at a minimum a name and a test function that raises exceptions
   to signal test failure. It is created with {!create} or other similar
   functions provided by this module.

   There are two main recommended ways of writing the test function:

   1. With [assert false]:

   Each test may use [assert false] to indicate that the test doesn't pass.
   This is the simplest way of failing while also showing the location
   of the failure. When using [assert false], you should generally take
   care of printing the expected value and actual value to make debugging
   easier later.

   2. With [Alcotest.(check ...)]:

   This is a little nicer because the error messages print something like
   ["Expecting 'foo', got 'bar'"]. However, this can make tests slightly
   more complicated to write. If the test already prints the expected value
   and the actual value as its output, it's just easier to fail with
   [assert false].

   In any case, Alcotest will capture the output (stdout, stderr) of each
   test and put it in its own file so we can consult it later. Don't
   hesitate to log a lot during the execution of the test.
*)

type test_with_status = t * status * status_summary

(**
   The return type of each subcommand.
   It allows custom code to do something with the test data e.g. export
   to the JUnit format via the optional [handle_subcommand_result] argument
   of [interpret_argv].
*)
type subcommand_result =
  | Run_result of test_with_status list
  | Status_result of test_with_status list
  | Approve_result

val create :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?normalize:(string -> string) list ->
  ?skipped:string ->
  ?solo:string ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  ?tracking_url:string ->
  string ->
  (unit -> unit Promise.t) ->
  t
(**
   Create a test to appear in a test suite.

{ul
   {- [category]: the nested category to assign to the test. The category
      can be nested further using {!categorize} or {!categorize_suites}.}
   {- [checked_output]: determines how to capture the test's output. Defaults
      to no capture.}
   {- [expected_outcome]: whether a test is expected to complete without
      raising an exception (default) or by raising an exception. }
   {- [normalize]: a list of functions applied in turn to transform the
      captured output before comparing it to the reference snapshot.
      See {!mask_line} and other functions with the [mask] prefix which are
      provided for this purpose.}
   {- [skipped]: specify that the test must be skipped.
      This is intended for tests
      that give inconsistent results and need fixing. The string should explain
      why the test is being skipped.
      See also [expected_outcome].}
   {- [solo]: specify that the test may not run in concurrently with other
      tests. The string should explain why.}
   {- [tags]: a list of tags to apply to the test. See {!module:Tag}.}
   {- [tolerate_chdir]: by default, a test will fail if it modifies the
      current directory and doesn't restore it. This flag cancels this check.
      Note that Testo will always restore the current directory after running
      a test regardless of this setting.}
}
*)

val update :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?func:(unit -> unit Promise.t) ->
  ?normalize:(string -> string) list ->
  ?name:string ->
  ?skipped:string option ->
  ?solo:string option ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  ?tracking_url:string option ->
  t ->
  t
(**
   Update some of the test's fields. This ensures that the test's unique
   identifier {!field:id} is recomputed correctly. When specified, an
   optional property will replace the previous value.
*)

(** {2 Assertions and exceptions}

    Signaling a test failure is done by raising an exception.
    You may raise any exception to signal a test failure.

    At this time, Testo doesn't provide advanced functions for checking
    a result against an expected value and printing these values nicely.
    For these, you may want to use `Alcotest.check` from the
    [alcotest] library.
*)

exception Test_failure of string
(** The exception raised by {!fail} *)

val fail : string -> unit
(** Raise the {!Test_failure} exception with a message indicating
    the reason for the failure. *)

(** {2 Temporary files and output redirection} *)

val write_file : Fpath.t -> string -> unit
(** Write data to a regular file. Create the file if it doesn't exist.
    Erase any existing data.

    Usage: [write_file path data]
*)

val read_file : Fpath.t -> string
(** Read the contents of a regular file. *)

val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> 'a Promise.t) ->
  'a Promise.t
(** [with_temp_file func] creates a temporary file, passes its path to
    the user-specified function [func], and returns the result.
    The temporary file is deleted when [func] terminates, even if it
    raises an exception.

    Options:
{ul
    {- [contents]: data to write to the file. If unspecified, the file is
                   created empty.}
    {- [persist]: if true, the temporary file is not deleted when done as
                  is normally the case. This intended for a user to inspect
                  the file when debugging.}
    {- [prefix]: prefix for the temporary file name. The default is
                 ["testo-"].}
    {- [suffix]: a suffix to append to the temporary file name. The default
                 is empty.}
    {- [temp_dir]: the path to the folder where the temporary file must
                   be created. The default is the system default returned
                   by [Filename.get_temp_dir_name ()].}
}
*)

val with_capture :
  out_channel -> (unit -> 'a Promise.t) -> ('a * string) Promise.t
(** [with_capture stdout func] evaluates [func ()] while
    capturing the output of the given channel [stdout] as a string. *)

(** {2 Environment control} *)

val with_environment_variables :
  (string * string) list -> (unit -> 'a Promise.t) -> 'a Promise.t
(** [with_environment_variables ["FOO", "42"; "BAR", "hello"] func]
    sets the environment variables [FOO] and [BAR] during the execution of
    [func] and then restores them to their original values.

    Additionally, a test failure is produced if [func] modifies these
    environment variables without restoring them to the state in which
    it found them.

    Due to a limitation in OCaml's "Unix" library, environment
    variables cannot be unset. If an environment variable was originally
    unset, restoring this original state isn't possible. Instead,
    the environment variable will be set to the empty string when
    [with_environment_variables] returns.
*)

(** {2 Output masking functions}

   Functions with the [mask_] prefix are string replacement
   utilities to be used for masking the variable parts of test output in order
   to make them stable and comparable.
   This is for the [normalize] option of {!create}.

   Testo will keep a copy of the original, unmasked output for the developer
   to consult.
   In particular, this masking functionality will not prevent sensitive data
   such as passwords or secret keys from being stored in the local file system.
*)

val mask_line :
  ?mask:string -> ?after:string -> ?before:string -> unit -> string -> string
(** Mask partially each line that contains [before] or [after].

    If both [after] and [before] are specified, they must occur in that
    order on a line to have an effect. The text between these markers is
    replaced by [mask]. If only [before] is specified, the portion of masked
    text starts at the beginning of the line. If only [after] is specified,
    the portion of masked text extends to the end of the line.

    For example, [(mask_line ~after:"time:" ()) "London time: 10:15,\nBlah"]
    produces ["London time:<MASKED>\nBlah"].
*)

val mask_pcre_pattern :
  ?replace:(string -> string) -> string -> string -> string
(**
   Mask all occurrences of this PCRE pattern. The syntax is limited to
   what the ocaml-re library supports.

   In the case that the pattern contains a capturing group and it
   (the first group) matches, only this substring is replaced
   rather than the whole match. The default [replace] function replaces
   the capture by ["<MASKED>"].

   Examples:
{v
     (* without a capturing group: *)
     mask_pcre_pattern ~replace:(fun _ -> "X") {|<[0-9]+>|} "xxx <42> xxx"
       = "xxx X xxx"
v}
{v
     (* with a capturing group: *)
     mask_pcre_pattern ~replace:(fun _ -> "X") {|<([0-9]+)>|} "xxx <42> xxx"
       = "xxx <X> xxx"
v}
*)

val contains_substring : sub:string -> string -> bool
(** Test if a string contains a substring [sub]. *)

val contains_pcre_pattern : pat:string -> string -> bool
(** Test if a string contains an unanchored PCRE pattern [pat]. *)

val filter_map_lines : (string -> string option) -> string -> string
(** Edit or remove each line of text.
    [filter_map_lines edit text] applies the function [edit] in turn
    to each line of [text] without its line terminator.
    Returning [None] removes the line.
    Line terminators [\n] or [\r\n] are preserved if and only if the
    line is not removed.
*)

val remove_matching_lines : (string -> bool) -> string -> string
(** [remove_matching_lines cond text] removes any line from [text]
    that validates [cond].

    For example, [remove_matching_lines (contains_substring ~sub:"DEBUG")]
    is a function that removes from a string all the lines containing
    [DEBUG].
    [remove_matching_lines (contains_pcre_pattern ~pat:"^DEBUG")] is a
    function that removes only the lines that start with [DEBUG].
*)

val keep_matching_lines : (string -> bool) -> string -> string
(** [remove_matching_lines cond text] removes any line from [text]
    that that doesn't validate [cond]. *)

val mask_temp_paths :
  ?depth:int option ->
  ?replace:(string -> string) ->
  ?temp_dir:Fpath.t ->
  unit ->
  string ->
  string
(**
   Mask strings that look like temporary file paths. This is useful in the
   following cases:

{ul
   {- the temporary folder depends on the platform (Unix, Windows) or
      on the environment (TMPDIR environment variable or equivalent);}
   {- the files placed in the system's temporary folder are assigned
      random names.}
}

   Options:
{ul
   {- [depth]: maximum number of path segments to mask after [/tmp] or
               equivalent.
               For example, [/tmp/b4ac9882/foo/bar] will become
               [<TMP>/<MASKED>/foo/bar] with the default depth
               of [Some 1]. With a depth of 2, if would become
               [<TMP>/<MASKED>/<MASKED>/bar]. Use [None] to mask the full
               path. Use [Some 0] to mask only [/tmp] or equivalent.}
   {- [replace]: function that determines what to replace the matched path
               with.}
   {- [temp_dir]: the path to the temporary folder to use instead of the
                  system default.}
}
*)

val mask_not_substring : ?mask:string -> string -> string -> string
(**
   Keep the given substring and mask everything else.
   This is for tests that only care about a particular substring being
   present in the output.
*)

val mask_not_substrings : ?mask:string -> string list -> string -> string
(** Keep all the given substrings and mask everything else.

   In case of overlaps between matching substrings, priority is given
   to the one starting earlier. If two substrings share a prefix, the
   longest match is preferred.

   Examples:
{ul
   {- [["cute"; "exec"]] will cause ["execute"] to become ["exec<MASKED>"]
      because [exec] occurs first in the target string.}
   {- [["wat"; "water"]] will cause ["hard water"]
      to become ["<MASKED>water"] and not ["<MASKED>wat<MASKED>"] because
      [water] is a longer match than [wat] starting at the same position.}
}
*)

val mask_not_pcre_pattern : ?mask:string -> string -> string -> string
(**
   Keep the substrings that match the given PCRE pattern and mask
   everything else.
*)

(** {2 Inline tests} *)

val test :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?normalize:(string -> string) list ->
  ?skipped:string ->
  ?solo:string ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> unit Promise.t) ->
  unit
(** Add a test to the global test suite that can be recovered with
    {!get_registered_tests}.

    This mechanism supports only synchronous tests i.e. ordinary tests whose
    test function has type [unit -> unit].

    It is meant to declare inline tests as follows:

{v
     let () = Testo.test "foo" (fun () ->
       (* test body raising exceptions to signal failure *)
       ...
     )
v}
*)

val get_registered_tests : unit -> t list
(** Recover the list of tests registered with {!val:test}. *)

(** {2 Categorization and filtering of test suites}

    A Testo test suite is a flat list of test cases. However, each test
    belongs to a category. Categories can be arbitrarily nested and can
    be exported as a tree if desired.
*)

val categorize : string -> t list -> t list
(**
   Put a list of tests into a parent category.

   Usage:
{v
     let apple_tests =
       categorize "apples" [test_color; test_juiciness]
v}
*)

val categorize_suites : string -> t list list -> t list
(** Variant of {!categorize} that flattens the nested list first.

{v
     let fruit_tests =
       categorize_suites "fruit" [apple_tests; banana_tests; strawberry_tests]
v}
*)

val sort : t list -> t list
(**
   Sort tests by category and name, alphabetically.

   Non-ASCII path components are currently sorted by byte order,
   possibly giving unexpected results.
*)

val has_tag : Tag.t -> t -> bool
(** Whether a test has this tag. This is meant for filtering test suites. *)

(** {2 Conversion to Alcotest test suites} *)

type alcotest_test_case = string * [ `Quick | `Slow ] * (unit -> unit Promise.t)
(** A type alias for Alcotest test cases. *)

type alcotest_test = string * alcotest_test_case list
(** A type alias for an Alcotest [test]. *)

val to_alcotest : alcotest_skip:(unit -> _) -> t list -> alcotest_test list
(**
   Export our tests to a list of tests that can run in Alcotest.
   This removes the ability to store test outcomes or to check the test output
   against expectations. Tests that are expected to fail and indeed fail
   (XFAIL) will be treated as successful by Alcotest. Conversely, tests that
   fail to raise an exception (XPASS) will be shown as failed by Alcotest.

   This function is provided to facilitate migrations between Alcotest
   and Testo, not for long-term use. It is independent of the Alcotest library
   except for the [Alcotest.skip] function that must be provided
   via the [alcotest_skip] argument.

   Usage: [Testo.to_alcotest ~alcotest_skip:Alcotest.skip tests]
*)

(** {2 Command-line interpretation} *)

val interpret_argv :
  ?argv:string array ->
  ?default_workers:int option ->
  ?expectation_workspace_root:Fpath.t ->
  ?handle_subcommand_result:(int -> subcommand_result -> unit) ->
  ?status_workspace_root:Fpath.t ->
  project_name:string ->
  ((string * string) list -> t list) ->
  unit Promise.t
(**
   Launch the command-line interface. It provides subcommands for running
   the tests, for checking test statuses, and for approving
   new output.

   A simple call is of the form
   [interpret_argv ~project_name:"my project" create_tests]
   where [create_tests] is the user-defined function that produces the
   test suite. [create_tests] gets called as [create_tests env] where [env]
   is the list of key/value pairs specified on the command line with
   [-e KEY1=VALUE1 -e KEY2=VALUE2 ...]. It gives an opportunity to
   parametrize the tests or to even ignore some tests. Note however that
   in general, it is preferable for [create_tests] to always produce the
   same list of tests regardless of the parameters passed to the program.
   For skipping a test without making it invisible, use [create ~skipped:true].
   For running a test that is expected to fail, use
   [create ~expected_outcome:(Should_fail "reason")].
   For filtering tests in other ways, use tags or search by substring.
   See {!create} and the command-line help available with [--help].

{ul
   {- [argv]: command line to parse. Defaults to [Sys.argv].}
   {- [default_workers]: the default number of workers to use in parallel
      runs when [-j] or [--jobs] isn't specified on the command line.
      It defaults to [None], indicating that the number of workers will
      be set to the number of CPUs detected on the machine.}
   {- [expectation_workspace_root]: storage path for expected output.
      The default is [tests/snapshots].}
   {- [handle_subcommand_result]: optional function to call on the result
      of the subcommand before exiting. It can be used to export test
      results to a specific format.}
   {- [status_workspace_root]: storage path for test results. The default is
      [_build/testo/status].}
   {- [project_name]: name of the program as shown in the [--help] page
      and used as a folder name for storing test results.}
}
*)
