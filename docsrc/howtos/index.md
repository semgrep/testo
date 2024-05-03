% Testo howtos

How to write an assertion for a simple type with Alcotest?
--

To check a result against an expected value and print it in case of an
error, Alcotest provides functions that can do this nicely for us for
predefined types. For example, `Alcotest.int` is predefined and is
used to check values of type `int`:

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

How to write an assertion for a type without Alcotest?
--

A simple solution is to use `assert`:

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
shown to the user. The advantage over using `Alcotest.check` is that
it's a little quicker to write. The main disadvantage is that the
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

How to write an assertion for a custom type?
--

A testable (see previous section) must be created for types such as
records or variants. This is done with `Alcotest.testable pp eq`
where `pp` is a printer and `eq` is an equality function.

...

How to write tests ahead of time?
--

...

How to work on failed tests?
--

...

How to run only a subset of a test suite?
--

...

How to write tests for Lwt, Async or other kinds of promises?
--

...

How to check test output against expected output?
--

...

How to export test output to JUnit?
--

...

How to run tests in parallel?
--

...
