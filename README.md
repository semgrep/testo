# Testo

> Party hard, test harder &mdash; DJ Testo

🚧 _This project has not been yet released on Opam, lacks basic
documentation, and is still subject to frequent interface changes._

Testo is a test framework for OCaml that takes inspiration from its
predecessor [Alcotest](https://github.com/mirage/alcotest). Additional
features it provides include:

- comparing test outcome with expectation, allowing tests to be expected to
  fail;
- comparing captured output with expected output;
- approving new output;
- reviewing test status without re-running them;
- nested test suites;
- tags;
- more ways to select tests.

Like with Alcotest, a test executable is generated from a list of tests
written in OCaml. The core subcommands are:

- `run`
- `status`
- `approve`
