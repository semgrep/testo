Testo(caml) [![CircleCI badge](https://circleci.com/gh/semgrep/testo.svg?style=svg)](https://app.circleci.com/pipelines/github/semgrep/testo)
==

Quick start
--

```
$ opam update
$ opam install testo
$ git clone https://github.com/semgrep/testo-template.git
$ cd testo-template
$ dune build
$ ./test
```

Copy the template and adapt it as needed.

Documentation: [testocaml.net](https://testocaml.net)
--

* [Getting started](https://testocaml.net/tutorial)
* [Howtos](https://testocaml.net/howtos)
* [API reference](https://testocaml.net/reference/testo/Testo/index.html)

Contributing
--

Testo is maintained by the OCaml community for the OCaml community.
If you see anything you like or dislike,
[let us know](https://github.com/semgrep/testo/issues)!

See our [contribution guidelines](https://github.com/semgrep/testo/blob/main/CONTRIBUTING.md).

Features
--

Testo is a test framework for OCaml that takes inspiration from its
predecessor [Alcotest](https://github.com/mirage/alcotest) and from
[pytest](https://pypi.org/project/pytest/).
Features include:

- support for explicit XFAIL tests i.e. tests that are expected to fail, indicating that they should be fixed eventually;
- support for test snapshots i.e. persistent storage of captured stdout, stderr, or output files;
- reviewing and approving tests without re-running them;
- nested test suites;
- various ways to select tests;
- parallel execution (using multiprocessing);
- supports OCaml >= 4.08.

A test executable is generated from a list of tests
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

For example, checking that some test result `res` equals an expected
value of `42` is written as:
```ocaml
Testo.(check int) 42 res;
```
This raises an exception that is turned into a nice error message.
