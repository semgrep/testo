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
  | Should_fail of string (** explains why we expect this test to fail *)

type outcome = Succeeded | Failed

type captured_output =
  | Ignored of string (** unchecked combined output *)
  | Captured_stdout of string * string (** stdout, unchecked output *)
  | Captured_stderr of string * string (** stderr, unchecked output *)
  | Captured_stdout_stderr of string * string (** stdout, stderr *)
  | Captured_merged of string (** combined output *)

type expected_output =
  | Ignored
  | Expected_stdout of string
  | Expected_stderr of string
  | Expected_stdout_stderr of string * string (** stdout, stderr *)
  | Expected_merged of string (** combined output *)

type result = { outcome : outcome; captured_output : captured_output }

type missing_files = Types.missing_files = Missing_files of string list

type expectation = {
  expected_outcome : expected_outcome;
  expected_output :
    (expected_output, missing_files) Result.t;
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

(** This type specifies what part of the output of a test (stdout, stderr)
    should be captured and compared against expectations.

    Use the provided functions {!val:stdout}, {!val:stderr}, {!val:stdxxx},
    and {!val:split_stdout_stderr} to create such an object.
*)
type checked_output_kind

(** Create an object of type {!type:checked_output_kind} specifying
    that the test's standard output must be checked against a reference file.
*)
val stdout : ?expected_stdout_path:string -> unit -> checked_output_kind

(** Same as {!val:stdout} but for capturing stderr instead. *)
val stderr : ?expected_stderr_path:string -> unit -> checked_output_kind

(** Same as {!val:stdout} but for capturing the combined stdout and stderr
    outputs. *)
val stdxxx : ?expected_stdxxx_path:string -> unit -> checked_output_kind

(** Same as {!val:stdxxx} but keep stdout and stderr separate. *)
val split_stdout_stderr :
  ?expected_stdout_path:string ->
  ?expected_stderr_path:string ->
  unit -> checked_output_kind

(** Wrapper allowing for asynchronous test functions (Lwt and such). *)
module Mona : module type of Mona

(** The type of tags which can be used to define subsets of tests precisely. *)
module Tag : module type of Tag

(**
   [t] is the type of a test. A test suite is a flat list of tests.
   For ordinary tests, the type parameter is [unit]. Type {!type:test}
   is an alias for [unit t].

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
type 'unit_promise t = private {
  id : string;
    (** Hash of the full name of the test, computed automatically. *)

  internal_full_name : string;
    (** Full name of the test, derived automatically from category and name. *)

  category : string list;
    (** Categories are made for organizing tests as a tree which is useful
        for display and filtering. A new category is created typically when
        grouping multiple test suites into one with 'categorize_suites'
        or when assigning a category to a list of tests with 'categorize'.
        e.g. ["food"; "fruit"; "kiwi"] *)

  name : string;
  func : unit -> 'unit_promise;

  (***** Options *****)
  expected_outcome : expected_outcome;
  tags : Tag.t list;
    (** Tags must be declared once using 'create_tag'. *)

  normalize : (string -> string) list;
    (** An optional function to rewrite any output data so as to mask the
        variable parts. *)

  checked_output : checked_output_kind;
    (** The 'skipped' property causes a test to be skipped by Alcotest but
        still shown as "[SKIP]" rather than being omitted. *)

  skipped : bool;
    (** If the test function changes the current directory without restoring
        it, it's an error unless this flag is set. *)

  tolerate_chdir : bool;
    (** All the tests in a test suite should share this field. *)

  m : 'unit_promise Mona.t;
}

(** An alias for the type of an ordinary test, i.e. one that returns
    when it's done rather than one that returns a deferred computation
    ([Lwt], [Async], etc.). *)
type test = unit t

type 'unit_promise test_with_status = 'unit_promise t * status * status_summary

(**
   The return type of each subcommand.
   It allows custom code to do something with the test data e.g. export
   to the JUnit format via the optional [handle_subcommand_result] argument
   of [interpret_argv].
*)
type 'unit_promise subcommand_result =
  | Run_result of 'unit_promise test_with_status list
  | Status_result of 'unit_promise test_with_status list
  | Approve_result

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
   {- [skipped]: whether the test should be skipped. This is intended for tests
      that give inconsistent results and need fixing.
      See also [expected_outcome].}
   {- [tags]: a list of tags to apply to the test. See {!module:Tag}.}
   {- [tolerate_chdir]: by default, a test will fail if it modifies the
      current directory and doesn't restore it. This flag cancels this check.
      Note that Testo will always restore the current directory after running
      a test regardless of this setting.}
}
*)
val create :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?normalize:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> unit) ->
  unit t

(**
   Generic version of [create] provided for libraries whose test function
   returns a promise (Lwt, Async, ...).
*)
val create_gen :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?normalize:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  'unit_promise Mona.t ->
  string ->
  (unit -> 'unit_promise) ->
  'unit_promise t

(**
   Update some of the test's fields. This ensures that the test's unique
   identifier {!field:id} is recomputed correctly. When specified, an
   optional property will replace the previous value.
*)
val update :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?func:(unit -> 'unit_promise) ->
  ?normalize:(string -> string) list ->
  ?name:string ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  'unit_promise t ->
  'unit_promise t

(**
   Special case of the [update] function that allows a different type
   for the new test function. This is useful for converting an Lwt test
   into a regular one.
*)
val update_func :
  'unit_promise t ->
  'unit_promise2 Mona.t ->
  (unit -> 'unit_promise2) ->
  'unit_promise2 t

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

(** Mask partially each line that contains [before] or [after].

    If both [after] and [before] are specified, they must occur in that
    order on a line to have an effect. The text between these markers is
    replaced by [mask]. If only [before] is specified, the portion of masked
    text starts at the beginning of the line. If only [after] is specified,
    the portion of masked text extends to the end of the line.

    For example, [(mask_line ~after:"time:" ()) "London time: 10:15,\nBlah"]
    produces ["London time:<MASKED>\nBlah"].
*)
val mask_line :
  ?mask:string -> ?after:string -> ?before:string -> unit -> (string -> string)

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
val mask_pcre_pattern :
  ?replace:(string -> string) ->
  string -> (string -> string)

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
   {- [tmpdir]: the path to the temporary folder to use instead of the
                system default.}
}
*)
val mask_temp_paths :
  ?depth:int option ->
  ?replace:(string -> string) ->
  ?tmpdir:string ->
  unit -> (string -> string)

(**
   Keep the given substring and mask everything else.
   This is for tests that only care about a particular substring being
   present in the output.
*)
val mask_not_substring : ?mask:string -> string -> (string -> string)

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
val mask_not_substrings : ?mask:string -> string list -> (string -> string)

(**
   Keep the substrings that match the given PCRE pattern and mask
   everything else.
*)
val mask_not_pcre_pattern : ?mask:string -> string -> (string -> string)

(** {2 Inline tests} *)

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
val test :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?expected_outcome:expected_outcome ->
  ?normalize:(string -> string) list ->
  ?skipped:bool ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  string ->
  (unit -> unit) ->
  unit

(** Recover the list of tests registered with {!val:test}. *)
val get_registered_tests : unit -> test list

(** {2 Categorization and filtering of test suites}

    A Testo test suite is a flat list of test cases. However, each test
    belongs to a category. Categories can be arbitrarily nested and can
    be exported as a tree if desired.
*)

(**
   Put a list of tests into a parent category.

   Usage:
{v
     let apple_tests =
       categorize "apples" [test_color; test_juiciness]
v}
*)
val categorize : string -> 'a t list -> 'a t list

(** Variant of {!categorize} that flattens the nested list first.

{v
     let fruit_tests =
       categorize_suites "fruit" [apple_tests; banana_tests; strawberry_tests]
v}
*)
val categorize_suites : string -> 'a t list list -> 'a t list

(**
   Sort tests by category and name, alphabetically.

   Non-ASCII path components are currently sorted by byte order,
   possibly giving unexpected results.
*)
val sort : 'a t list -> 'a t list

(** Whether a test has this tag. This is meant for filtering test suites. *)
val has_tag : Tag.t -> 'a t -> bool

(** {2 Conversion to Alcotest test suites} *)

(** A type alias for Alcotest test cases. *)
type 'unit_promise alcotest_test_case =
  string * [ `Quick | `Slow ] * (unit -> 'unit_promise)

(** A type alias for an Alcotest [test]. *)
type 'unit_promise alcotest_test =
  string * 'unit_promise alcotest_test_case list

(**
   Export our tests to a list of tests that can run in Alcotest.
   This removes the ability to store test outcomes or to check the test output
   against expectations. Tests that are expected to fail and indeed fail
   (XFAIL) will be treated as successful by Alcotest. Conversely, tests that
   fail to raise an exception (XPASS) will be shown as failed by Alcotest.

   This function is provided to facilitate migrations between Alcotest
   and Testo, not for long-term use.
*)
val to_alcotest : 'unit_promise t list -> 'unit_promise alcotest_test list

(** {2 Command-line interpretation} *)

(**
   Launch the command-line interface. It provides subcommands for running
   the tests, for checking test statuses, and for approving
   new output.

   Return value: exit code reflecting overall success or failure (0 or 1),
   and subcommand-specific data for export to JUnit or similar.

{ul
   {- [argv]: command line to parse. Defaults to [Sys.argv].}
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
val interpret_argv :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:(int -> unit subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  project_name:string ->
  (unit -> test list) ->
  unit

(** Generic variant of {!interpret_argv}. It requires an extra argument
    [mona] that is defined once and for all by a library like
    [Testo_lwt]. *)
val interpret_argv_gen :
  ?argv:string array ->
  ?expectation_workspace_root:string ->
  ?handle_subcommand_result:(int -> 'unit_promise subcommand_result -> unit) ->
  ?status_workspace_root:string ->
  mona:'unit_promise Mona.t ->
  project_name:string ->
  (unit -> 'unit_promise t list) ->
  'unit_promise
