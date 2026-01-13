A template for an OCaml project using Testo with Dune
==

Copy these files into your project:

* The `testo` script will be your main command to run tests and to manage
  test results. It is short and customizable.
* `tests/test.ml` is the entry point of your OCaml test program.
  Add tests in there.
* `tests/dune` is set up to build and run tests with dune.

Commands you should try:

* `./testo`: this will run the test executable after building it if
  necessary. This is the same as `./testo run`.
* `./testo status`: this will print the status of the test results
  without running tests. Try `--help` to see what options are available.
* `dune runtest`: build and run the test suite without arguments. Use
  `-f` to force a re-run.
