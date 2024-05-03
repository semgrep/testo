% Testo tutorial

Introduction
--

Testo is a test framework for OCaml. Like with OUnit or Alcotest, the
user writes a collection of tests. A test consists of a name and an
OCaml test function to run, with some options. If the test function returns,
the test is considered successful but if it raises an exception, it
is considered failed.

The test suite is compiled into a test executable with a fancy
command-line interface provided by the Testo library.

Main features
--

* Most tests can be defined by providing only a name and a function of
  type `unit -> unit`.
* Tests can be assigned nested categories of arbitrary depth
  but are conveniently handled as a single flat list.
* Tests that are expected to fail ("XFAIL") can be marked as such. This
  allows writing tests ahead of feature implementation or bug fixes.
* Supports output snapshots, i.e. capturing stdout or stderr from a
  test and comparing it with a reference file.
* Reviewing previous test outcomes can done without rerunning the
  tests.
* Provides various utilities for capturing stdout or stderr, and
  masking variable parts of test output such as temporary file paths.
* Support for tests that return Lwt promises.

XFAIL outcomes and snapshot files are two features borrowed from
Pytest that would have required massive changes in Alcotest and led to
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

🚧 ~~Install `testo` with Opam using `opam install testo`~~ For
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

let tests = [
  test_hello;
]

let () =
  Testo.interpret_argv
    ~project_name:"my_project"
    (fun () -> tests)
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

To enforce that our test prints what it's supposed to, we're going to
specify that we want to capture the standard output (stdout) produced
by the test and compare it against a reference. If the output of the
test changes in the future, it will be detected and reported as a test
failure.

First, let's modify the "hello" test to check stdout by specifying the
`checked_output` option:
```
let test_hello =
  Testo.create "hello"
    ~checked_output:(Testo.stdout ())
    (fun () -> print_endline "hello!")
```

Running `./test` with the updated code reports a failure and tells us
that something's missing:
```
...
┌──────────────────────────────────────────────────────────────────────────────┐
│ [PASS*] 5d41402abc4b hello                                                   │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
• Missing file containing the expected output: tests/snapshots/my_project/5d41402abc4b/stdout
• Path to captured stdout: _build/testo/status/my_project/5d41402abc4b/stdout
• Path to captured log: _build/testo/status/my_project/5d41402abc4b/log
• Log (stderr) is empty.
────────────────────────────────────────────────────────────────────────────────
1/1 selected test:
  1 successful (1 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
1 test whose output needs first-time approval
overall status: failure
```

This was expected since we don't have a reference output for our test.
First, we're going to check that the captured output is what we were
expecting:
```
$ cat _build/testo/status/my_project/5d41402abc4b/stdout
hello!
```

Note that at any time, we can get a summary of the tests that need
attention using `./test status`, without having to re-run the tests:
```
$ ./test status
[PASS*] 5d41402abc4b hello
```

We can see details with the `-l` ("long output") option:
```
┌──────────────────────────────────────────────────────────────────────────────┐
│ [PASS*] 5d41402abc4b hello                                                   │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
• Missing file containing the expected output: tests/snapshots/my_project/5d41402abc4b/stdout
• Path to captured stdout: _build/testo/status/my_project/5d41402abc4b/stdout
• Path to captured log: _build/testo/status/my_project/5d41402abc4b/log
• Log (stderr) is empty.
────────────────────────────────────────────────────────────────────────────────
1/1 selected test:
  1 successful (1 pass, 0 xfail)
  0 unsuccessful (0 fail, 0 xpass)
1 test whose output needs first-time approval
overall status: failure
```

Then, we're going to approve this output and make it the reference
snapshot with `./test approve`:
```
$ ./test approve
Expected output changed for 1 test.
```

Let's check the new status:
```
$ ./test status
```

Nothing is printed because all the tests passed and are in the best
state possible. To see the full list of tests, use `-a` ("all"):
```
$ ./test status -a
[PASS]  5d41402abc4b hello
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

The snapshot files are organized as follows:
```
$ tree tests/snapshots/
tests/snapshots/
└── my_project
    └── 5d41402abc4b
        ├── name
        └── stdout

2 directories, 2 files
```

The path to the captured output for our test is
`tests/snapshots/my_project/5d41402abc4b/stdout`, as shown in the
original test output:

```
$ cat tests/snapshots/my_project/5d41402abc4b/stdout
hello!
```

Add the `snapshots/` folder to the git repository:
```
$ git add tests/snapshots
$ git commit -m 'Add test snapshots'
```

### When a test fails

Let's make our test function print `hello, world!` instead of
`hello` to see what happens:
```
let test_hello =
  Testo.create "hello"
    ~checked_output:(Testo.stdout ())
    (fun () -> print_endline "hello, world!")
```

Recompile and re-run `./test`:
```
$ dune build
$ ./test
...
┌──────────────────────────────────────────────────────────────────────────────┐
│ [FAIL]  5d41402abc4b hello                                                   │
└──────────────────────────────────────────────────────────────────────────────┘
• Checked output: stdout
--- tests/snapshots/my_project/5d41402abc4b/stdout	2024-05-02 18:06:06.863630709 -0700
+++ _build/testo/status/my_project/5d41402abc4b/stdout	2024-05-02 18:06:13.391623357 -0700
@@ -1 +1 @@
-hello!
+hello, world!
• Captured stdout differs from expectation.
• Path to expected stdout: tests/snapshots/my_project/5d41402abc4b/stdout
• Path to captured stdout: _build/testo/status/my_project/5d41402abc4b/stdout
• Path to captured log: _build/testo/status/my_project/5d41402abc4b/log
• Log (stderr) is empty.
────────────────────────────────────────────────────────────────────────────────
1/1 selected test:
  0 successful (0 pass, 0 xfail)
  1 unsuccessful (1 fail, 0 xpass)
overall status: failure
```

The diff between the expected output and the new output is shown as
```
-hello!
+hello, world!
```

If the new output is correct, run `./test approve`. Otherwise, edit
your source code until you're satisfied with the new output.

## What's next?

You're now ready to use Testo. To discover more functionality, explore our
[how-tos](../howtos) and consult the [reference API](../reference)
for technical details.
