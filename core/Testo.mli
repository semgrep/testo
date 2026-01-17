(** Testo library - Utilities for writing OCaml test suites *)

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

    These types are documented in the library's source code in [Types.ml]. They
    are subject to frequent and unannounced changes at the whim of the library's
    authors. A casual user should not need them. *)

type expected_outcome =
  | Should_succeed
  | Should_fail of string  (** explains why we expect this test to fail *)

type completion_status =
  | Test_function_returned
  | Test_function_raised_an_exception
  | Test_timeout

type fail_reason =
  | Raised_exception
  | Missing_output_file
  | Incorrect_output
  | Timeout

type outcome = Succeeded | Failed of fail_reason

type checked_output_file
(** A test's output file whose contents captured from stdout or stderr should be
    checked and reported when it changes. *)

type checked_output_file_with_contents
(** A test's output file that is produced by the test function, checked, and
    reported when it changes. *)

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

type result = {
  completion_status : completion_status;
  captured_output : captured_output;
  captured_output_files : checked_output_file_with_contents list;
  missing_output_files : Fpath.t list;
}

type missing_files = Types.missing_files = Missing_files of Fpath.t list

type expectation = {
  expected_outcome : expected_outcome;
  expected_output : (expected_output, missing_files) Result.t;
  expected_output_files :
    (checked_output_file_with_contents, Fpath.t) Result.t list;
}

type status = {
  expectation : expectation;
  result : (result, missing_files) Result.t;
}

type passing_status =
  | PASS
  | FAIL of fail_reason
  | XFAIL of fail_reason
  | XPASS
  | MISS of missing_files

type status_summary = {
  passing_status : passing_status;
  outcome : outcome;
  has_expected_output : bool;
}

(****************************************************************************)
(** {1 Main interface} *)
(****************************************************************************)

(** {2 Test creation} *)

type checked_output_kind
(** This type specifies what part of the output of a test (stdout, stderr)
    should be captured and compared against expectations.

    Use the provided functions {!val:stdout}, {!val:stderr}, {!val:stdxxx}, and
    {!val:split_stdout_stderr} to create such an object. *)

val stdout : ?expected_stdout_path:Fpath.t -> unit -> checked_output_kind
(** Create an object of type {!type:checked_output_kind} specifying that the
    test's standard output must be checked against a reference file. *)

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

val checked_output_file :
  ?snapshot_path:Fpath.t -> string -> checked_output_file
(** [checked_output_file name] creates the specification for a checked output
    file identified by the name [name]. Popular names include ["results.txt"]
    and ["results.json"]. [name] must be a nonempty sequence of ASCII letters,
    digits, underscores, dashes, or periods. Periods are not allowed in first or
    last position. It is used by Testo in messaging and in file names. The
    [snapshot_path] option specifies an alternate location for the snapshot file
    that serves as the expectation for future test runs. *)

val stash_output_file : Fpath.t -> string -> unit
(** [stash_output_file src_path dst_name] copies a checked output file of the
    current test identified by its name [dst_name] into Testo's status
    workspace.

    This function must be called by the test function for each checked output
    file. *)

module Promise : module type of Promise
(** Wrapper allowing for asynchronous test functions (Lwt and such).

    For ordinary Testo use where test function computations are synchronous
    (i.e. not Lwt), any occurrence of the type [xxx Promise.t] is the same as
    just [xxx]. *)

(** {b EXPERIMENTAL} Lazy computations that capture and restore stdout/stderr.

    This is intended to save computation time when multiple tests share the same
    preliminary, costly computation. *)
module Lazy_with_output : sig
  type 'a t
  (** {b EXPERIMENTAL}

      A lazy computation similar to OCaml's [Lazy.t] that also captures and
      restores stdout and stderr output just like it catches, stores and
      re-raises exceptions. *)

  (** [Stdout_to_stderr] cause all stdout output to be printed on stderr.
      [Stderr_to_stdout] is the other way around. *)
  type redirect = Stdout_to_stderr | Stderr_to_stdout

  val create : ?redirect:redirect -> (unit -> 'a Promise.t) -> 'a t
  (** {b EXPERIMENTAL}

      Store a lazy computation. This doesn't compute it yet.

      During the computation, Stdout and stderr outputs are captured separately,
      then printed out one after the other, with stdout coming first before
      stderr. To preserve the original interleaving of standard output and error
      output as it would normally appear in a console, use a redirect from
      stdout to stderr or vice-versa with the [redirect] option so as to merge
      the two streams into one for the duration of the computation. *)

  val force : 'a t -> 'a Promise.t
  (** {b EXPERIMENTAL}

      Run the lazy computation if it hasn't run yet. If the computation was
      already performed, the original standard and error outputs are printed
      again. The original result is returned or the original exception is
      re-raised with the original stack backtrace if applicable. *)
end

module Tag : module type of Testo_util.Tag
(** The type of tags which can be used to define subsets of tests precisely. *)

type inline_logs = On | Off | Auto

type t = private {
  id : string;
      (** Hash of the full name of the test, computed automatically. *)
  internal_full_name : string;
      (** Full name of the test, derived automatically from category and name.
      *)
  category : string list;
      (** Categories are made for organizing tests as a tree which is useful for
          display and filtering. A new category is created typically when
          grouping multiple test suites into one with 'categorize_suites' or
          when assigning a category to a list of tests with 'categorize'. e.g.
          [["food"; "fruit"; "kiwi"]] *)
  name : string;
  func : unit -> unit Promise.t;
  (***** Options *****)
  checked_output : checked_output_kind;
  checked_output_files : checked_output_file list;
      (** Files created by the test function whose contents must match a
          reference snapshot. The test function must copy its checked output
          files into Testo's workspace using {!stash_output_files}. *)
  expected_outcome : expected_outcome;
  flaky : string option;
      (** If not [None], the [flaky] property causes the test to run normally
          but it will be ignored when determining the success of the test suite.
          This allows flaky tests to be kept around until they can be fixed. Use
          the string argument to explain briefly why the test is marked as
          flaky. The [--strict] command-line option causes the flaky status to
          be ignored i.e. a test run will fail if a flaky test fails. *)
  inline_logs : inline_logs;
  max_duration : float option;
      (** A time limit for the test when running in a detached worker, in
          seconds. This setting is ignored when running tests sequentially in
          the master process such as with [-j0] or in [solo] mode. *)
  normalize : (string -> string) list;
      (** An optional function to rewrite any output data so as to mask the
          variable parts. This normalization is only applied to captured stdout
          or stderr, not to output files. If you wish to normalize output files,
          the test function should handle it, possibly with the provided
          {!map_file} function. *)
  skipped : string option;
      (** If not [None], the [skipped] property causes a test to be skipped by
          Alcotest but still shown as ["[SKIP]"] rather than being omitted. The
          string should give a reason why the test is being skipped. *)
  solo : string option;
      (** If not [None], this test will never run concurrently with other tests.
          The string should give a reason why the test should not run in
          parallel with other tests. *)
  tags : Tag.t list;  (** Tags must be declared once using [create_tag]. *)
  tolerate_chdir : bool;
      (** If the test function changes the current directory without restoring
          it, it's an error unless this flag is set. All the tests in a test
          suite should share this field. To change the current directory
          temporarily and safely, use {!with_chdir}. *)
  tracking_url : string option;
      (** A link to the relevant entry in a bug tracking system. *)
}
(** [t] is the type of a test. A test suite is a flat list of tests.

    A test is at a minimum a name and a test function that raises exceptions to
    signal test failure. It is created with {!create} or other similar functions
    provided by this module.

    There are two main recommended ways of writing the test function:

    1. With [assert false]:

    Each test may use [assert false] to indicate that the test doesn't pass.
    This is the simplest way of failing while also showing the location of the
    failure. When using [assert false], you should generally take care of
    printing the expected value and actual value to make debugging easier later.

    2. With [Alcotest.(check ...)]:

    This is a little nicer because the error messages print something like
    ["Expecting 'foo', got 'bar'"]. However, this can make tests slightly more
    complicated to write. If the test already prints the expected value and the
    actual value as its output, it's just easier to fail with [assert false].

    In any case, Alcotest will capture the output (stdout, stderr) of each test
    and put it in its own file so we can consult it later. Don't hesitate to log
    a lot during the execution of the test. *)

type test_with_status = t * status * status_summary

(** The return type of each subcommand. It allows custom code to do something
    with the test data e.g. export to the JUnit format via the optional
    [handle_subcommand_result] argument of [interpret_argv]. *)
type subcommand_result =
  | Run_result of test_with_status list
  | Status_result of test_with_status list
  | Approve_result

val create :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?checked_output_files:checked_output_file list ->
  ?expected_outcome:expected_outcome ->
  ?flaky:string ->
  ?inline_logs:inline_logs ->
  ?max_duration:float ->
  ?normalize:(string -> string) list ->
  ?skipped:string ->
  ?solo:string ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  ?tracking_url:string ->
  string ->
  (unit -> unit Promise.t) ->
  t
(** Create a test to appear in a test suite.

    - [broken]: the old name for [flaky] used in testo-0.2.0.
    - [category]: the nested category to assign to the test. The category can be
      nested further using {!categorize} or {!categorize_suites}.
    - [checked_output]: determines how to capture the test's output. Defaults to
      no capture.
    - [checked_output_files]: specifies the test's output files that should
      remain the same from one test run to another.
    - [expected_outcome]: whether a test is expected to complete without raising
      an exception (default) or by raising an exception.
    - [flaky]: a flaky test that fails will be reported as such but won't make
      the test run fail unless the [--strict] is passed on the command line.
    - [inline_logs]: determines whether to show the test's logs inline when
      running a test or showing its status. For the sake of this option, logs
      include uncaptured stdout or stderr output and any information about
      exceptions produced by the test. [Auto] is the default behavior and
      currently consists of printing the logs only if the test fails or if
      [-w]/[--show-output] is on. [On] and [Off] unconditionally enable or
      disable inline logs, overriding the command-line option
      [-w]/[--show-output]. Note that logs are saved in files in Testo's
      workspace regardless of these settings.
    - [max_duration]: a time limit to run the test, in seconds. It is honored
      only in tests running in workers i.e. not with the [-j0] option of the
      test program.
    - [normalize]: a list of functions applied in turn to transform the captured
      output before comparing it to the reference snapshot. See {!mask_line} and
      other functions with the [mask] prefix which are provided for this
      purpose.
    - [skipped]: specify that the test must be skipped. This is intended for
      tests that give inconsistent results and need fixing. The string should
      explain why the test is being skipped. See also [expected_outcome].
    - [solo]: specify that the test may not run in concurrently with other
      tests. The string should explain why.
    - [tags]: a list of tags to apply to the test. See {!module:Tag}.
    - [tolerate_chdir]: by default, a test will fail if it modifies the current
      directory and doesn't restore it. This flag cancels this check. Note that
      Testo will always restore the current directory after running a test
      regardless of this setting. *)

val update :
  ?category:string list ->
  ?checked_output:checked_output_kind ->
  ?checked_output_files:checked_output_file list ->
  ?expected_outcome:expected_outcome ->
  ?flaky:string option ->
  ?func:(unit -> unit Promise.t) ->
  ?inline_logs:inline_logs ->
  ?max_duration:float option ->
  ?normalize:(string -> string) list ->
  ?name:string ->
  ?skipped:string option ->
  ?solo:string option ->
  ?tags:Tag.t list ->
  ?tolerate_chdir:bool ->
  ?tracking_url:string option ->
  t ->
  t
(** Update some of the test's fields. This ensures that the test's unique
    identifier {!field:id} is recomputed correctly. When specified, an optional
    property will replace the previous value. *)

(** {2 Assertions and exceptions}

    Signaling a test failure is done by raising an exception. You may raise any
    exception to signal a test failure. *)

exception Test_failure of string
(** The exception raised by {!check} and {!fail}.

    This exception as well as any exception defined outside [Testo] will be
    interpreted as a test failure. *)

type 'a testable = { show : 'a -> string; equal : 'a -> 'a -> bool }
(** A testable is the pair of functions needed to compare an expected value
    against an actual result produced in a test using the {!check} function, and
    to print error messages.

    Testables were directly inspired by Alcotest's testables and work similarly.

    For example, checking the value of an int is done as follows:
    {v
      let res = 1 + 1 in
      Testo.(check int) 2 res
    v}

    Checking a list of ints is done as follows:
    {v
      let res = List.map (fun x -> x + 1) [1; 2; 3] in
      Testo.(check (list int)) [2; 3; 4] res
    v} *)

val testable : ('a -> string) -> ('a -> 'a -> bool) -> 'a testable
(** A function to create a [testable] record in a more compact fashion.

    In a typical module that provides a type [t], a function [show], and a
    function [equal], a testable is defined as
    [let testable = Testo.testable {show; equal}] or equivalently as
    [let testable = Testo.testable show equal] with the signature
    [val testable : t Testo.testable]. *)

val check : 'a testable -> ?msg:string -> 'a -> 'a -> unit
(** Check an expected result (left) against an actual result (right). If the
    values don't match, they will be printed to stderr and the {!Test_failure}
    exception will be raised, signaling a test failure.

    An optional message [msg] can be provided. It will be printed in the case of
    a test failure. *)

val bool : bool testable
val int : int testable
val int32 : int32 testable
val int64 : int64 testable

val float : float testable
(** The {!float} testable uses the vanilla [Float.to_string] function to render
    floats. Unfortunately, the exact output is platform-dependent. For example,
    Windows prints [1e+012] when on Linux (GLibc?) we get [1e+12]. This may
    result in distracting diffs being reported for floats when reporting
    legitimate differences for complex data. *)

val string : string testable
(** The {!string} testable is the safe default for rendering strings in error
    messages and diffs shown by Testo when a test fails. Strings are formatted
    as valid OCaml string literals with visible line breaks. All bytes outside
    the printable ASCII range \[32-126\] are escaped. If you prefer to visualize
    all characters as rendered by your terminal, use {!text} or your own
    testable. *)

val text : string testable
(** Multiline strings that are unlikely to contains control characters should
    use {!text} rather than {!string} for best diff rendering. *)

val bytes : bytes testable
(** This is the same as the {!string} testable except that it operates on
    mutable strings known as [bytes]. *)

val list : 'a testable -> 'a list testable
val array : 'a testable -> 'a array testable
val seq : 'a testable -> 'a Seq.t testable
val option : 'a testable -> 'a option testable
val result : 'a testable -> 'b testable -> ('a, 'b) Result.t testable
val tuple2 : 'a testable -> 'b testable -> ('a * 'b) testable

val tuple3 :
  'a testable -> 'b testable -> 'c testable -> ('a * 'b * 'c) testable

val tuple4 :
  'a testable ->
  'b testable ->
  'c testable ->
  'd testable ->
  ('a * 'b * 'c * 'd) testable

val tuple5 :
  'a testable ->
  'b testable ->
  'c testable ->
  'd testable ->
  'e testable ->
  ('a * 'b * 'c * 'd * 'e) testable

val fail : string -> 'a
(** Raise the {!Test_failure} exception with a message indicating the reason for
    the failure. [fail msg] has the same effect as [failwith msg] but clarifies
    that it's a failed check rather than some other error. A {!Test_failure}
    exception may also be reported by Testo in a nicer way than a generic
    [Failure] raised by [failwith]. *)

(** {2 Temporary files and output redirection} *)

val write_file : Fpath.t -> string -> unit
[@@deprecated "Use write_text_file instead."]

val read_file : Fpath.t -> string [@@deprecated "Use read_text_file instead."]

val map_file : (string -> string) -> Fpath.t -> Fpath.t -> unit
[@@deprecated "Use map_text_file instead."]

val copy_file : Fpath.t -> Fpath.t -> unit
[@@deprecated "Use copy_text_file instead."]

val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> 'a Promise.t) ->
  'a Promise.t
[@@deprecated "Use with_temp_text_file instead."]

val write_text_file : Fpath.t -> string -> unit
(** Write data to a regular file. Create the file if it doesn't exist. Erase any
    existing data.

    Usage: [write_file path data] *)

val read_text_file : Fpath.t -> string
(** Read the contents of a regular file or symbolic link to a regular file. *)

val map_text_file : (string -> string) -> Fpath.t -> Fpath.t -> unit
(** [map_file func src dst] reads the contents of file (regular or symlink)
    [src], applies [func] to its contents, and writes the result into file
    [dst]. If file [dst] already exists, it is truncated and overwritten.
    Otherwise, a regular file is created. If [src] and [dst] represent the same
    file, [src] will be overwritten with the new contents. *)

val copy_text_file : Fpath.t -> Fpath.t -> unit
(** Copy a file. [copy_file src dst] is a shortcut for
    [map_file (fun data -> data) src dst]. *)

val with_temp_text_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> 'a Promise.t) ->
  'a Promise.t
(** [with_temp_file func] creates a temporary file, passes its path to the
    user-specified function [func], and returns the result. The temporary file
    is deleted when [func] terminates, even if it raises an exception.

    Options:
    - [contents]: data to write to the file. If unspecified, the file is created
      empty.
    - [persist]: if true, the temporary file is not deleted when done as is
      normally the case. This intended for a user to inspect the file when
      debugging.
    - [prefix]: prefix for the temporary file name. The default is ["testo-"].
    - [suffix]: a suffix to append to the temporary file name. The default is
      empty.
    - [temp_dir]: the path to the folder where the temporary file must be
      created. The default is the system default returned by
      [Filename.get_temp_dir_name ()]. *)

val with_capture :
  out_channel -> (unit -> 'a Promise.t) -> ('a * string) Promise.t
(** [with_capture stdout func] evaluates [func ()] while capturing the output of
    the given channel [stdout] as a string. *)

(** {2 Environment control} *)

val with_environment_variables :
  (string * string) list -> (unit -> 'a Promise.t) -> 'a Promise.t
(** [with_environment_variables ["FOO", "42"; "BAR", "hello"] func] sets the
    environment variables [FOO] and [BAR] during the execution of [func] and
    then restores them to their original values.

    Additionally, a test failure is produced if [func] modifies these
    environment variables without restoring them to the state in which it found
    them.

    Due to a limitation in OCaml's "Unix" library, environment variables cannot
    be unset. If an environment variable was originally unset, restoring this
    original state isn't possible. Instead, the environment variable will be set
    to the empty string when [with_environment_variables] returns. *)

val with_chdir : Fpath.t -> (unit -> 'a) -> 'a
(** [with_chdir path func] changes the current directory associated with the
    process to [path] before calling the function [func]. The original value of
    the current directory is restored when the function returns or raises an
    exception. *)

val with_temp_dir :
  ?chdir:bool ->
  ?parent:Fpath.t ->
  ?perms:int ->
  ?prefix:string ->
  ?suffix:string ->
  (Fpath.t -> 'a) ->
  'a
(** [with_temp_dir func] creates temporary folder [dir], calls the function
    [func dir] and returns its result. [dir] and its contents are removed before
    [with_temp_dir] returns or raises an exception. If [chdir] is set to true,
    the current folder is set to [dir] during the execution of [func] (default:
    false). [parent] is the parent folder of the temporary folder to create and
    defaults to [Filename.get_temp_dir_name ()]. [perms] is the Unix-style
    permission mask used when creating [dir] with [Sys.mkdir] or equivalent.
    [prefix] and [suffix] are a prefix and a suffix to use for the name of [dir]
    (defaults: unspecified and subject to change). *)

val get_current_test : unit -> t option
(** Return the test currently running. *)

(** {2 Output masking functions}

    Functions with the [mask_] prefix are string replacement utilities to be
    used for masking the variable parts of test output in order to make them
    stable and comparable. This is for the [normalize] option of {!create}.

    Testo will keep a copy of the original, unmasked output for the developer to
    consult. In particular, this masking functionality will not prevent
    sensitive data such as passwords or secret keys from being stored in the
    local file system. *)

val mask_line :
  ?mask:string -> ?after:string -> ?before:string -> unit -> string -> string
(** Mask partially each line that contains [before] or [after].

    If both [after] and [before] are specified, they must occur in that order on
    a line to have an effect. The text between these markers is replaced by
    [mask]. If only [before] is specified, the portion of masked text starts at
    the beginning of the line. If only [after] is specified, the portion of
    masked text extends to the end of the line.

    For example, [(mask_line ~after:"time:" ()) "London time: 10:15,\nBlah"]
    produces ["London time:<MASKED>\nBlah"]. *)

val mask_pcre_pattern :
  ?replace:(string -> string) -> string -> string -> string
(** Mask all occurrences of this PCRE pattern. The syntax is limited to what the
    ocaml-re library supports.

    In the case that the pattern contains a capturing group and it (the first
    group) matches, only this substring is replaced rather than the whole match.
    The default [replace] function replaces the capture by ["<MASKED>"].

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
    v} *)

val contains_substring : sub:string -> string -> bool
(** Test if a string contains a substring [sub]. *)

val contains_pcre_pattern : pat:string -> string -> bool
(** Test if a string contains an unanchored PCRE pattern [pat]. *)

val filter_map_lines : (string -> string option) -> string -> string
(** Edit or remove each line of text. [filter_map_lines edit text] applies the
    function [edit] in turn to each line of [text] without its line terminator.
    Returning [None] removes the line. Line terminators [\n] or [\r\n] are
    preserved if and only if the line is not removed. *)

val remove_matching_lines : (string -> bool) -> string -> string
(** [remove_matching_lines cond text] removes any line from [text] that
    validates [cond].

    For example, [remove_matching_lines (contains_substring ~sub:"DEBUG")] is a
    function that removes from a string all the lines containing [DEBUG].
    [remove_matching_lines (contains_pcre_pattern ~pat:"^DEBUG")] is a function
    that removes only the lines that start with [DEBUG]. *)

val keep_matching_lines : (string -> bool) -> string -> string
(** [remove_matching_lines cond text] removes any line from [text] that that
    doesn't validate [cond]. *)

val mask_temp_paths :
  ?depth:int option ->
  ?replace:(string -> string) ->
  ?temp_dir:Fpath.t ->
  unit ->
  string ->
  string
(** Mask strings that look like temporary file paths. This is useful in the
    following cases:

    - the temporary folder depends on the platform (Unix, Windows) or on the
      environment (TMPDIR environment variable or equivalent);
    - the files placed in the system's temporary folder are assigned random
      names.

    Options:
    - [depth]: maximum number of path segments to mask after [/tmp] or
      equivalent. For example, [/tmp/b4ac9882/foo/bar] will become
      [<TMP>/<MASKED>/foo/bar] with the default depth of [Some 1]. With a depth
      of 2, if would become [<TMP>/<MASKED>/<MASKED>/bar]. Use [None] to mask
      the full path. Use [Some 0] to mask only [/tmp] or equivalent.
    - [replace]: function that determines what to replace the matched path with.
    - [temp_dir]: the path to the temporary folder to use instead of the system
      default. *)

val mask_not_substring : ?mask:string -> string -> string -> string
(** Keep the given substring and mask everything else. This is for tests that
    only care about a particular substring being present in the output. *)

val mask_not_substrings : ?mask:string -> string list -> string -> string
(** Keep all the given substrings and mask everything else.

    In case of overlaps between matching substrings, priority is given to the
    one starting earlier. If two substrings share a prefix, the longest match is
    preferred.

    Examples:
    - [["cute"; "exec"]] will cause ["execute"] to become ["exec<MASKED>"]
      because [exec] occurs first in the target string.
    - [["wat"; "water"]] will cause ["hard water"] to become ["<MASKED>water"]
      and not ["<MASKED>wat<MASKED>"] because [water] is a longer match than
      [wat] starting at the same position. *)

val mask_not_pcre_pattern : ?mask:string -> string -> string -> string
(** Keep the substrings that match the given PCRE pattern and mask everything
    else. *)

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
    v} *)

val get_registered_tests : unit -> t list
(** Recover the list of tests registered with {!val:test}. *)

(** {2 Categorization and filtering of test suites}

    A Testo test suite is a flat list of test cases. However, each test belongs
    to a category. Categories can be arbitrarily nested and can be exported as a
    tree if desired. *)

val categorize : string -> t list -> t list
(** Put a list of tests into a parent category.

    Usage:
    {v
     let apple_tests =
       categorize "apples" [test_color; test_juiciness]
    v} *)

val categorize_suites : string -> t list list -> t list
(** Variant of {!categorize} that flattens the nested list first.

    {v
     let fruit_tests =
       categorize_suites "fruit" [apple_tests; banana_tests; strawberry_tests]
    v} *)

val sort : t list -> t list
(** Sort tests by category and name, alphabetically.

    Non-ASCII path components are currently sorted by byte order, possibly
    giving unexpected results. *)

val has_tag : Tag.t -> t -> bool
(** Whether a test has this tag. This is meant for filtering test suites. *)

(** {2 Conversion to Alcotest test suites} *)

type alcotest_test_case = string * [ `Quick | `Slow ] * (unit -> unit Promise.t)
(** A type alias for Alcotest test cases. *)

type alcotest_test = string * alcotest_test_case list
(** A type alias for an Alcotest [test]. *)

val to_alcotest : alcotest_skip:(unit -> _) -> t list -> alcotest_test list
(** Export our tests to a list of tests that can run in Alcotest. This removes
    the ability to store test outcomes or to check the test output against
    expectations. Tests that are expected to fail and indeed fail (XFAIL) will
    be treated as successful by Alcotest. Conversely, tests that fail to raise
    an exception (XPASS) will be shown as failed by Alcotest.

    This function is provided to facilitate migrations between Alcotest and
    Testo, not for long-term use. It is independent of the Alcotest library
    except for the [Alcotest.skip] function that must be provided via the
    [alcotest_skip] argument.

    Usage: [Testo.to_alcotest ~alcotest_skip:Alcotest.skip tests] *)

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
(** Launch the command-line interface. It provides subcommands for running the
    tests, for checking test statuses, and for approving new output.

    A simple call is of the form
    [interpret_argv ~project_name:"my project" create_tests] where
    [create_tests] is the user-defined function that produces the test suite.
    [create_tests] gets called as [create_tests env] where [env] is the list of
    key/value pairs specified on the command line with
    [-e KEY1=VALUE1 -e KEY2=VALUE2 ...]. It gives an opportunity to parametrize
    the tests or to even ignore some tests. Note however that in general, it is
    preferable for [create_tests] to always produce the same list of tests
    regardless of the parameters passed to the program. For skipping a test
    without making it invisible, use [create ~skipped:true]. For running a test
    that is expected to fail, use
    [create ~expected_outcome:(Should_fail "reason")]. For filtering tests in
    other ways, use tags or search by substring. See {!create} and the
    command-line help available with [--help].

    - [argv]: command line to parse. Defaults to [Sys.argv].
    - [default_workers]: the default number of workers to use in parallel runs
      when [-j] or [--jobs] isn't specified on the command line. It defaults to
      [None], indicating that the number of workers will be set to the number of
      CPUs detected on the machine.
    - [expectation_workspace_root]: storage path for expected output. The
      default is [tests/snapshots].
    - [handle_subcommand_result]: optional function to call on the result of the
      subcommand before exiting. It can be used to export test results to a
      specific format.
    - [status_workspace_root]: storage path for test results. The default is
      [_build/testo/status].
    - [project_name]: name of the program as shown in the [--help] page and used
      as a folder name for storing test results. *)
