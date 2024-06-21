% Testo tutorial

Introduction
--

[Testo](https://github.com/semgrep/testo)
is a test framework for [OCaml](https://ocaml.org/). Like with
[OUnit](https://github.com/gildor478/ounit) or
[Alcotest](https://github.com/mirage/alcotest), the
user writes a collection of tests. A test consists of a name and an
OCaml test function to run, with some options. If the test function returns,
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

XFAIL outcomes and snapshot files are two features borrowed from
[Pytest](https://docs.pytest.org/)
that would have required massive changes in Alcotest and led to
the creation of a new project.

Should I use Testo?
--

Testo was designed to support older OCaml versions starting from 4.08
and to be maintained by the community of users. It is being used to test
[Semgrep](https://github.com/semgrep/semgrep) which has about 5000
OCaml tests, most of which were originally migrated from Alcotest.
Check out the
[known missing features](https://github.com/semgrep/testo/issues) to
see if anything critical to you is missing.

Getting started
--

### Install the `testo` library

🚧 ~~Install `testo` with [Opam](https://opam.ocaml.org/)
using `opam install testo`~~ We'll be providing an Opam package soon. For
now, we recommend using `testo` as a git submodule. Dune will pick
it up and build it as part of your project like an ordinary library.

### Set up your project

At this stage, you need an OCaml project that uses Dune and Git. If
you don't have one, you can download and run the
[test script](https://github.com/semgrep/testo/blob/main/docsrc/tutorial/run-tutorial)
which will create one for you and will run some of the steps below.

The folder `tests/snapshots/` will be used by Testo to
store test snapshots to be tracked by your favorite version
control system (git, ...). Storing other test data under `tests/` is
encouraged as long as you let Testo manage `tests/snapshots/`.

### Write a test executable

The test executable can be placed anywhere in your Dune project. We
recommend having only one such program if possible and calling it
`test`. In this tutorial, we'll put it in the `tests/` folder.

We are going to create the following folders and files:

* `tests/Test.ml`: the entry point of the `test` program
* `tests/dune`: the Dune file that defines how to build the test executable
* `tests/snapshots/`: created and managed by Testo, under version control
* `test`: a symbolic link to `_build/default/tests/test.exe`

Create the following `tests/dune` whose job is to build an ordinary executable
named `test`:

```
; Build the test executable for our project
(executable
 (name test)
 (modules Test)
 (libraries
    testo
 )
)
```

We recommend running the test program always from the project root so
as to reference any files using paths relative to the project root.
Create the symbolic link that will allow us to call the test program
directly from the project root:

```
$ ln -s _build/default/tests/test.exe test
```

If you already have a `test` file or folder, you may pick another
name for the Testo program, it doesn't matter. The examples in this tutorial
assume `./test` calls our Testo-based test program.

The last part of this setup is to write the OCaml file
`tests/Test.ml`. Let's use this:

```
(*
   The entry point for the 'test' program that runs the suite of OCaml
   tests for <this project>.
*)

let test_hello =
  Testo.create "hello"
    (fun () -> print_endline "hello!")

let tests _env = [
  test_hello;
]

let () =
  Testo.interpret_argv
    ~project_name:"my_project"
    tests
```

### Check your setup

From the project root, build your project as usual with Dune:
```
$ dune build
```

If everything went according to plan, running the test program with
`--help` will list the subcommands supported by `./test`:

```
$ ./test --help
TEST(1)                           Test Manual                          TEST(1)



NAME
       test - run tests for my_project

SYNOPSIS
       test [COMMAND] …

DESCRIPTION

...

COMMANDS
       approve [--filter-substring=SUBSTRING] [OPTION]…
           approve new test output

       run [OPTION]…
           run the tests

       status [OPTION]…
           show test status
...
```

Let's run our test suite with `./test run` or just `./test`:

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
[PASS]  5d41402abc4b hello
• Path to captured log: _build/testo/status/my_project/5d41402abc4b/log
1/1 selected test:
  1 successful (1 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
overall status: success
```

The script
[run-tutorial](https://github.com/semgrep/testo/blob/main/docsrc/tutorial/run-tutorial)
runs all the steps above to create a sample `my_test` project.

Congratulations, the "hello" test passed!

... but did it, though? Did it output `hello!` like it was supposed to?
The output from `./test` gives us the path to the captured log:

```
$ cat _build/testo/status/my_project/5d41402abc4b/log
hello!
```

The test was successful because it didn't raised any exception, not
because it printed `hello!` correctly.

### Make the test check its output

The output of our test is `hello!\n` which is short and simple. To
check this, we'll use the `Testo.with_capture` function to turn the
standard output into a string. Then, we'll compare it against the
expected string. This is done by wrapping the original test function
as follows:
```
let test_hello =
  Testo.create "hello"
    (fun () ->
      let res =
        Testo.with_capture stdout
          (fun () -> print_endline "hello!")
      in
      assert (res = "hello!\n")
    )
```

Try it and check that the test fails if the expectation is different
from the actual output.

### What if a test's output is very long?

Say we want to check the help page printed by a program. As an
exercise, let's use `dune --help`. The standard output takes multiple
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
our OCaml test. Testo allows capturing stdout or stderr as a file or
"snapshot" that will serve as a reference for future runs. This is
done with the `~checked_output` option as follows:
```
let test_dune_help =
  Testo.create "dune help"
    ~checked_output:(Testo.stdout ())
    (fun () -> Sys.command "dune --help" |> ignore)
```

After adding this test to the suite, your test suite should look like
this:
```
let tests = [
  test_hello;
  test_dune_help;
]
```

Running `./test` with the updated code almost works but reports a
failure and tells us that something's missing:
```
...
┌──────────────────────────────────────────────────────────────────────────────┐
│ [PASS*] 06e03989d7ca dune help                                               │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
• Missing file containing the expected output: tests/snapshots/my_project/06e03989d7ca/stdout
• Path to captured stdout: _build/testo/status/my_project/06e03989d7ca/stdout
• Path to captured log: _build/testo/status/my_project/06e03989d7ca/log
• Log (stderr) is empty.
────────────────────────────────────────────────────────────────────────────────
2/2 selected tests:
  2 successful (2 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
1 test whose output needs first-time approval
overall status: failure
```

The status `PASS*` means that the test passed but some user action is needed.
This was expected since we don't have a reference output for our test.
First, we're going to check that the captured output is what we were
expecting:
```
$ less _build/testo/status/my_project/06e03989d7ca/stdout
DUNE(1)                           Dune Manual                          DUNE(1)



NAME
       dune - composable build system for OCaml
...
```

It looks good. Note that at any time, we can get a summary of the
tests that need attention using `./test status`, without having to
re-run the tests:
```
$ ./test status
[PASS*] 06e03989d7ca dune help
```

Listing all the tests requires `-a`:
```
$ ./test status -a
[PASS]  5d41402abc4b hello
[PASS*] 06e03989d7ca dune help
```

Selecting tests can be done with `./test -s`. It searches for a substring
e.g. `dune`:

```
$ ./test status -a -s dune
[PASS*] 06e03989d7ca dune help
```

The test ID can be used to select a single test:
```
$ ./test status -a -s 5d41402abc4b
[PASS]  5d41402abc4b hello
```

We can see details with the `-l` ("long output") option:
```
$ ./test status -l
┌──────────────────────────────────────────────────────────────────────────────┐
│ [PASS*] 06e03989d7ca dune help                                               │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
• Missing file containing the expected output: tests/snapshots/my_project/06e03989d7ca/stdout
• Path to captured stdout: _build/testo/status/my_project/06e03989d7ca/stdout
• Path to captured log: _build/testo/status/my_project/06e03989d7ca/log
• Log (stderr) is empty.
────────────────────────────────────────────────────────────────────────────────
2/2 selected tests:
  2 successful (2 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
1 test whose output needs first-time approval
overall status: failure
```

Let's approve the output of "dune help" and make it the reference
snapshot with `./test approve`:
```
$ ./test approve
Expected output changed for 1 test.
```

In practice, we might have several tests requiring approval.
To approve a specific test rather than all of them, use `-s`:
```
$ ./test approve -s 06e03989d7ca
Expected output changed for 1 test.
```

Let's check the new status:
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
[PASS]  06e03989d7ca dune help
```

Now, there should be a snapshot file somewhere in our file system.
Git shows us that `tests/snapshots` was created:
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
└── Test.ml

3 directories, 4 files
```

The path to the captured output for our test is
`tests/snapshots/my_project/06e03989d7ca/stdout`, as shown in the
original test output.

It would be nicer to have the snapshot file with a good name,
say `dune-help.txt` next to the test code. This is done by passing
the relevant option to `Testo.stdout`:
```
let test_dune_help =
  Testo.create "dune help"
    ~checked_output:
      (Testo.stdout
        ~expected_stdout_path:(Fpath.v "tests/dune-help.txt") ())
    (fun () -> Sys.command "dune --help" |> ignore)
```

Re-running everything gives us the following file tree:
```
tests/
├── dune
├── dune-help.txt
├── snapshots
│   └── my_project
└── Test.ml
```

All these files including the snapshots should be tracked by git:
```
$ git add tests/
$ git commit -m 'Add tests'
```

### When a test fails

Check what happens if you replace the command `dune --help` with
`dune build --help` in `Test.ml`.
The "dune help" test should fail and you should see a diff against the
expected output.

What's next?
--

You're now ready to use Testo. To discover more functionality, explore our
[how-tos](../howtos) and consult the [reference API](../reference)
for technical details.
