% Testo howtos

<!-- note to authors:
We don't have automatic validation of the code snippets shown here.
It's a good idea to check that your code at least compiles.
For this, you may want to use `dune utop` and paste your code into it.
-->

For getting started with [Testo](https://github.com/semgrep/testo)
and discovering the basics, make sure to read the
[tutorial](../tutorial) first.

How to write an assertion (without resorting to an extra library)?
--

A simple solution is to use the built-in `assert` construct:

```
let test_addition =
  Testo.create "addition"
    (fun () ->
      let res = 1 + 1 in
      assert (res = 2);
      let res = 2 + 2 in
      assert (res = 4)
    )
```

When such a test fails, the location of the `assert ...` expression is
shown to the user. The advantage over using a dedicated function
like `Alcotest.check` is that it's a little quicker to write and
simpler to understand.
The main disadvantage is that the
expected and actual values are not printed. In this case, it's best
for the test to print enough information about the condition being
tested e.g.

```
open Printf

let test_addition =
  Testo.create "addition"
    (fun () ->
      let add a b expected_result =
        let res = a + b in
        printf "checking %i + %i, expecting %i, result is %i.\n"
          a b expected_result res;
        assert (res = expected_result)
      in
      add 1 1 2;
      add 2 2 4
    )
```

In case of a failure, the test's output will be shown to the user.

How to write an assertion for a value of a simple type with Alcotest?
--

Testo was originally meant to extend
[Alcotest](https://github.com/mirage/alcotest), but it became too
different and ended up being a separate project. However, Alcotest provides
some functionality that wasn't reimplemented in Testo and can benefit
Testo tests. Like Testo, Alcotest is an ordinary OCaml library that is
typically installed with Opam:
```
$ opam install alcotest
```

`Alcotest.check` is the function we'll use to check test results
against expectations. If a check fails, an exception is raised and it is
formatted as a nice error message.

Here's a test that checks values of type `int`:

```
let test_addition =
  Testo.create "addition"
    (fun () ->
      let res = 1 + 1 in
      Alcotest.check Alcotest.int "sum" 2 res;
      let res = 2 + 2 in
      Alcotest.check Alcotest.int "sum" 4 res
    )
```

It is more compactly written as:
```
let test_addition =
  Testo.create "addition"
    (fun () ->
      let res = 1 + 1 in
      Alcotest.(check int) "sum" 2 res;
      let res = 2 + 2 in
      Alcotest.(check int) "sum" 4 res
    )
```

`Alcotest.int` is called a
["testable"](https://mirage.github.io/alcotest/alcotest/Alcotest/index.html#testable-values).
There are predefined testables
for OCaml's simple types such as `bool`, `int`, `string`, etc.

Checking a list of strings can be done as follows:

```
let test_items =
   Testo.create "items"
     (fun () ->
       let res = ["a"] @ ["b"] in
       Alcotest.(check (list string)) "sum" ["a"; "b"] res
     )
```

How to write an assertion for a custom type?
--

A testable (see previous section) must be created for types such as
records or variants. This is done with `Alcotest.testable print equal`
where `print` is a printer and `equal` is an equality function.

Let's assume we're testing values of type `Thing.t`. Module `Thing`
exposes the following signature:
```
module Thing : sig
  type t
  val to_string : t -> string
  val compare : t -> t -> int
  ...
end
```

The test program will define a testable as follows:
```
let print_thing fmt x = Format.pp_print_string fmt (Thing.to_string x)
let equal_thing a b = (Thing.compare a b = 0)
let thing = Alcotest.testable print_thing equal_thing
```

A test function will call `Alcotest.check` as follows:
```
let test_things =
  Testo.create
    "things"
    (fun () ->
      let result = ... in
      let expected_thing = ... in
      Alcotest.check thing "equal" expected_thing result
    )
```

How to write tests ahead of time?
--

Testo supports two ways of writing tests that are known to fail.
If the test consistently fails in all environments, the recommended
solution is to use `~expected_outcome:(Should_fail "<insert excuse>")`:
```
Testo.create
  "my test"
    ~expected_outcome:(Should_fail "TODO")
    (fun () ->
      (* code that raises an exception *)
      ...
    )
```

If such a test raises an exception as expected, its status will be
shown as `XFAIL` and considered successful.

Now, if a test is "flaky" i.e. it sometimes fails and sometimes
succeed, it can be marked as "skipped". The only difference with a
test that's completely removed from the test suite is that it's still
listed by `./test status -a` and marked as `SKIP` instead of being
invisible and forgotten. In this example, we'll skip a test only if
the platform is Windows:

```
let is_windows =
  Sys.os_type = "Win32"

let test_something =
  (* We don't run this test on Windows because <reasons> *)
  Testo.create
    "something"
    ~skipped:is_windows
    (fun () -> ...)
```

How to pass arbitrary options to the test program?
--

For example, the tests may need to read a configuration file whose
path isn't fixed, or maybe we want to see what happens
when we change some settings.

One solution is to set an environment variable in the shell or parent
process of the test program. However, environment variables are not
great for the following reasons:

* Environment variables can easily leak into child processes in which
  these variables shouldn't be set, or worse it can result in passing
  sensitive data that shouldn't be accessed by other processes.
* Environment variables are hard to track.
  They affect the program's behavior but their existence is not
  obvious because they don't necessarily appear on the command line.

To avoid these issues and make parameters explicit, Testo provides a
`--env` (or `-e`) option that allows passing key/value pairs when
invoking the test program. Consult `--help` for guidance:

```
$ ./test --help
...
       -e KEY=VALUE, --env=KEY=VALUE
           Pass a key/value pair to the function that creates the test suite.
           KEY must be an alphanumeric identifier of the form
           [A-Za-z_][A-Za-z_0-9]*. VALUE can be any string. This mechanism for
           passing arbitrary runtime settings to the test suite is offered as
           a safer alternative to environment variables.
```

Suppose we want to pass the path `etc/special-foo.conf` to the test
program. We would invoke it as follows:

```
$ ./test run --env conf=etc/special-foo.conf
...
```

The part of the OCaml program that produces the list of tests would
consult the `conf` variable as follows:

```
let tests env : Testo.t list =
  let config_path =
    match List.assoc_opt "conf" env with
    | None -> "foo.conf" (* default *)
    | Some path -> path
  in
  [
    Testo.create ...;
    ...
  ]

let () =
  Testo.interpret_argv
    ~project_name:"foo"
    tests
```

Passing multiple key/value pairs is done with multiple `-e` or `--env`
options:

```
$ ./test run -e conf=etc/special-foo.conf -e foo=bar
...
```

How to write tests for Lwt, Async or other kinds of promises?
--

The simplest way is to start and stop the event loop within each test.
With `Lwt`, this is normally done with `Lwt_main.run`:

```
(* Generic higher-level function that converts a asynchronous
   computation into a synchronous one. *)
let sync (func : unit -> unit Lwt.t) =
  fun () ->
    Lwt_main.run func

let test_sleep () : unit Lwt.t =
  Lwt_unix.sleep 0.05

let tests = [
  Testo.create "sleep" (sync test_sleep);
]
```

On some platforms such as a JavaScript runtime, there is no equivalent
of `Lwt_main.run`. For these cases, we provide the library
`testo-lwt`. It exposes a
[`Testo_lwt` module](https://semgrep.github.io/testo/reference/testo-lwt/index.html)
whose interface is almost identical to `Testo` except that
test functions have type `unit -> unit Lwt.t` instead of `unit ->
unit`.

For the Async library, we don't provide `testo-async` but it should be
straightforward to add. Check the status of the [corresponding GitHub
issue](https://github.com/semgrep/testo/issues/73).

How to export test output to JUnit?
--

🚧 not implemented,
see [Issue #14](https://github.com/semgrep/testo/issues/14).

How to run tests in parallel?
--

🚧 not implemented,
see [Issue #8](https://github.com/semgrep/testo/issues/8).
