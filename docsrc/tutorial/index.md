---
title: "Getting Started with Testo: A Hands-On Tutorial"
description: "A hands-on tutorial that introduces Testo step by
  step. Learn what Testo can do, explore its core features, and get
  started with practical examples in minutes."
---

Introduction
--

[Testo](../)
is a test framework for [OCaml](https://ocaml.org/). Like with
[OUnit](https://github.com/gildor478/ounit) or
[Alcotest](https://github.com/mirage/alcotest), the
user writes a collection of tests. A test consists of a name and an
OCaml test function to run, and supports many options.
If the test function returns,
the test is considered successful but if it raises an exception, it
is considered failed.

The test suite is compiled into a test executable with
a command-line interface provided by the Testo library. The test
executable is called manually or by CI jobs to run tests and review
the results.

Main features
--

* Most tests can be defined by providing only a name and a function of
  type `unit -> unit`.
* Tests can be placed into categories, subcategories, etc.
  to make it easier to work with groups of related tests.
* Tests can be tagged so as to select groups of tests independently
  from hierarchical categories.
* A test suite is always a flat list of tests regardless of categories
  or tags.
* Tests that are expected to fail ("XFAIL") can be marked as such. This
  allows writing tests ahead of feature implementation or bug fixes.
* Supports output snapshots, i.e. capturing stdout or stderr from a
  test and comparing it with a reference file.
* Reviewing previous test outcomes can be done without rerunning the
  tests.
* Provides various utilities for capturing stdout or stderr, and
  masking variable parts of test output such as temporary file paths.
* Support for tests that return Lwt promises.
* Fast parallel execution using multiprocessing.
* Non-intrusive support for timeouts.
* Support for older OCaml versions starting from OCaml 4.08.
* Windows support.

XFAIL outcomes and snapshot files are two features borrowed from
[Pytest](https://docs.pytest.org/)
that would have required massive changes in Alcotest and led to
the creation of a new project.

Recommended uses
--

Testo was designed to support older OCaml versions starting from 4.08
and to be maintained by the community of users. We've been using it to test
[Semgrep](https://github.com/semgrep/semgrep) which has about 7000
OCaml tests, many of which were originally migrated from Alcotest.
Check out the
[known missing features](https://github.com/semgrep/testo/issues) to
see if anything critical to you is missing.

Getting started
--

### Install the `testo` library

Installing `testo` with [Opam](https://opam.ocaml.org/)
using `opam install testo`.

### Set up your project

At this stage, you need an OCaml project that uses Dune and Git.
First, clone [our template](https://github.com/semgrep/testo-template):
```
$ git clone https://github.com/semgrep/testo-template.git
$ cd testo-template
```

Then, check that it works:
```
$ dune build
$ ./test
```

This will run the test suite, showing output similar to this:
```
$ ./test
Legend:
• [PASS]: a successful test that was expected to succeed (good);
• [FAIL]: a failing test that was expected to succeed (needs fixing);
• [XFAIL]: a failing test that was expected to fail (tolerated failure);
• [XPASS]: a successful test that was expected to fail (progress?).
• [MISS]: a test that never ran;
• [SKIP]: a test that is always skipped but kept around for some reason;
• [xxxx*]: a new test for which there's no expected output yet.
  In this case, you should review the test output and run the 'approve'
  subcommand once you're satisfied with the output.
Try '--help' for options.
[RUN]   5d41402abc4b hello
[PASS]  5d41402abc4b hello
• Path to captured log: _build/testo/status/my_project/5d41402abc4b/log
1/1 selected test:
  1 successful (1 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
overall status: success
```

### Inspect the existing tests

`tests/test.ml` is the entry point for the test suite.
Inspect it and you'll see:

* a test suite which is a flat list of test cases. For now, it contains just
  one test named `hello`.
* the invocation of `Testo.interpret_argv`. It is in charge of
  interpreting the command line and performing the requested actions.

### Add a new test

Copy the existing `hello` test and call it `"welcome message"`.
Rebuild and rerun the test suite. You should
now see two successful tests.

### Make a test fail

Make the "welcome message" test fail.
For example, you can use this code:
```
let test_welcome_message =
  Testo.create "welcome message"
    (fun () ->
       let expected = "welcome" in
       let result = "hello" in
       Testo.(check string) expected result
    )
```

Run the tests. You should now see a failing test:
```
...

┌──────────────────────────────────────────────────────────────────────────────┐
│ [FAIL]  504541c02761 welcome message                                         │
└──────────────────────────────────────────────────────────────────────────────┘
• Path to captured log: _build/testo-template/status/my_project/504541c02761/log
• Log (stdout, stderr) is empty.
• Exception raised by the test:
 Test failed: not equal:
   expected (left): "welcome"
   actual  (right): "hello"
 differences:
 --- expected
 +++ actual
 @@ -1,1 +1,1 @@
 -"welcome"
 +"hello"
 
 
────────────────────────────────────────────────────────────────────────────────
[FAIL]  504541c02761 welcome message
2/2 selected tests:
  1 successful (1 pass, 0 xfail)
  1 unsuccessful (1 fail, 0 xpass)
overall status: failure
```

### Make a test check its output

Now instead of comparing strings, let's compare the output of some
code that writes to stdout.

Modify the "welcome message" test to print `welcome` on stdout:
```
let test_welcome_message =
  Testo.create "welcome message"
    (fun () ->
       print_endline "welcome"
    )
```

To check what's printed on stdout, use `Testo.with_capture`:
```
let test_hello =
  Testo.create "hello"
    (fun () ->
      let (), out =
        Testo.with_capture stdout
          (fun () -> print_endline "welcome")
      in
      Testo.(check string) "welcome" out
    )
```

Try it.

### Check long outputs

Assume you want to check the help page printed by a program. As an
exercise, use `dune --help`. The output takes multiple
screens and is cumbersome to copy-paste and escape correctly due to
the presence of special characters:

```
$ dune --help
DUNE(1)                           Dune Manual                          DUNE(1)



NAME
       dune - composable build system for OCaml

SYNOPSIS
...
```

It wouldn't be convenient to store this as a double-quoted string in
our OCaml file `test.ml`. Testo allows capturing stdout or stderr as a file or
"snapshot" that will serve as a reference for future runs. Do this
with the `~checked_output` option as follows:
```
let test_dune_help =
  Testo.create "dune help"
    ~checked_output:(Testo.stdout ())
    (fun () -> Sys.command "dune --help" |> ignore)
```

Add it to your test suite.

Run `./test` with the updated code. It almost works but reports a
failure and tells you that something's missing:
```
...
┌──────────────────────────────────────────────────────────────────────────────┐
│ [PASS*] 06e03989d7ca dune help                                               │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
• Missing file containing the expected output: tests/snapshots/my_project/06e03989d7ca/stdout
• Path to captured stdout: _build/testo-template/status/my_project/06e03989d7ca/stdout
• Path to captured log: _build/testo-template/status/my_project/06e03989d7ca/log
• Log (stderr) is empty.
...
1 test whose output needs first-time approval
...
```

The status `PASS*` indicates that the test passed but some user action
is needed. This was expected since we don't have a reference output
for our test. First, we're going to check that the captured output is
what we were expecting:
```
$ less _build/testo-template/status/my_project/06e03989d7ca/stdout
DUNE(1)                           Dune Manual                          DUNE(1)



NAME
       dune - composable build system for OCaml
...
```

To troubleshoot just one test, select it with the `-s` ("select") filter. This
will hide the status of any other failing test while you work on this
one:
```
$ ./test status -s dune
[PASS*] 06e03989d7ca dune help
```

The test ID can also be used to select a test. Use `-a` ("all") to list
the successful tests. Check that the following command lists the desired test:
```
$ ./test status -a -s 5d41402abc4b
[PASS]  5d41402abc4b hello
```

Check out details with the `-l` ("long output") option:
```
$ ./test status -l
┌──────────────────────────────────────────────────────────────────────────────┐
│ [PASS*] 06e03989d7ca dune help                                               │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
• Missing file containing the expected output: tests/snapshots/my_project/06e03989d7ca/stdout
• Path to captured stdout: _build/testo-template/status/my_project/06e03989d7ca/stdout
• Path to captured log: _build/testo-template/status/my_project/06e03989d7ca/log
• Log (stderr) is empty.
────────────────────────────────────────────────────────────────────────────────
2/2 selected tests:
  2 successful (2 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
1 test whose output needs first-time approval
overall status: failure
```

Approve the output of "dune help" and make it the reference
snapshot with `./test approve`:
```
$ ./test approve
Expected output changed for 1 test.
```

In practice, you might have several tests requiring approval.
To approve a specific test rather than all of them, use `-s`:
```
$ ./test approve -s 06e03989d7ca
Expected output changed for 1 test.
```

Check the new status:
```
$ ./test status
$ echo $?  # check the process exit status
0
```

An exit status of 0 indicates a full success. This is confirmed by
listing all the tests:
```
$ ./test status -a
[PASS]  5d41402abc4b hello
[PASS]  504541c02761 welcome message
[PASS]  06e03989d7ca dune help
```

Now, there should be a snapshot file somewhere in our file system.
Git shows you that `tests/snapshots` was created:
```
$ git status
...
Untracked files:
  (use "git add <file>..." to include in what will be committed)
	tests/snapshots/
```

The test files are organized as follows:
```
$ tree tests/
tests/
├── dune
├── snapshots
│   └── my_project
│       └── 06e03989d7ca
│           ├── name
│           └── stdout
└── test.ml

3 directories, 4 files
```

The path to the captured output for our test is
`tests/snapshots/my_project/06e03989d7ca/stdout`, as shown in the
original test output.

It would be nicer to have the snapshot file with a good name,
say `dune-help.txt` next to the test code. Do it by passing
the relevant option to `Testo.stdout`:
```
let test_dune_help =
  Testo.create "dune help"
    ~checked_output:
      (Testo.stdout
        ~expected_stdout_path:(Fpath.v "tests/dune-help.txt") ())
    (fun () -> Sys.command "dune --help" |> ignore)
```

Re-run everything to get the following file tree:
```
tests/
├── dune
├── dune-help.txt
├── snapshots
│   └── my_project
└── test.ml
```

Add all these files including the snapshots to your Git repo:
```
$ git add tests/
$ git commit -m 'Add tests'
```

### Check diffs

Check what happens if you replace the command `dune --help` with
`dune build --help` in `test.ml`.
The "dune help" test should fail and you should see a diff against the
expected output.

What's next?
--

You're now ready to use Testo. To discover more functionality, explore our
[how-tos](../howtos) and consult the [reference API](../reference)
for all technical details.
