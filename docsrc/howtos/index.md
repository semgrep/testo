% Testo howtos

How to write an assertion (without resorting to an extra library)?
--

A simple solution is to use the built-in `assert`:

```
let test_addition =
  Testo.create "addition"
    (fun () ->
      let res = 1 + 1 in
      assert (res = 2);
      let res = 2 + 2 in
      assert (res = 4);
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
      add 2 2 4;
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

How to work on failed tests?
--

🚧

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

🚧 not implemented, see https://github.com/semgrep/testo/issues/14

How to run tests in parallel?
--

🚧 not implemented, see https://github.com/semgrep/testo/issues/8
