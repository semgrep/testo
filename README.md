Testo
==

> Party hard, test harder &mdash; DJ Testo

🚧 _This project hasn't been released on Opam yet, and is still
subject to frequent interface changes._

![image](https://github.com/semgrep/testo/assets/343265/51f3e6a0-fdb1-400b-a146-4312746ae8d0)

[Documentation](https://semgrep.github.io/testo/)
--

* [Getting started](https://semgrep.github.io/testo/tutorial)
* [Howtos](https://semgrep.github.io/testo/howtos)
* [API reference](https://semgrep.github.io/testo/reference/testo/Testo/index.html)

Features
--

Testo is a test framework for OCaml that takes inspiration from its
predecessor [Alcotest](https://github.com/mirage/alcotest) and from
[pytest](https://pypi.org/project/pytest/).
Features include:

- support for explicit XFAIL tests i.e. tests that are expected to fail, indicating that they should be fixed eventually;
- support for test snapshots i.e. persistent storage of captured stdout or stderr;
- reviewing and approving tests without re-running them;
- nested test suites;
- various ways to select tests;
- parallel execution (using multiprocessing);
- supports OCaml >= 4.08.

Like with Alcotest, a test executable is generated from a list of tests
written in OCaml. The function to interpret the command line
and run things is `Testo.interpret_argv`.
The core subcommands supported by a test executable are:

- `run`: run tests
- `status`: check the status of the tests without re-running them
- `approve`: approve test output and make it the new reference

A test is fundamentally a name and test function of type
`unit -> unit`. A test is considered successful if the test function
returns normally and is considered failed if it raises an exception.
A test is created with `Testo.create` which takes a variety of options
in addition to the name and the test function.

Testo doesn't provide a library for writing assertions. Using the
[Alcotest](https://mirage.github.io/alcotest/alcotest/Alcotest/index.html)
module for this is recommended. For example,
checking that some test result `res` equals an expected value of `42`
is written as:
```ocaml
Alcotest.(check int) "equal" 42 res;
```
This raises an exception that is turned into a nice error message.
