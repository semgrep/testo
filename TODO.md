To-do list for Testo
==

This list should be converted into GitHub issues once Testo has
its own repository.

Things to do:

* translate this list into GitHub issues
* make color output optional (on/off/auto)
* protect against tests that might change terminal settings mid-run
  and cause color to disappear mid-run (already an issue with plain Alcotest)
* use the Duff library for portable, color diff ('diff --color' is not
  available everywhere)
* add an option for early abort: exit as soon a test fails
* add option to filter by tag
* add option to filter by tag using and/or/not syntax
* add an option to run the previously-failed tests first (like
  `--lazy`) and then the other tests
* report and store run times (alongside success/failure outcome)
* add an option for running tests in parallel. Portable suggestion:
  Divide jobs crudely into N batches and relaunch the test program
  from scratch with Unix.create_process by passing an argument such as
  '--child 3/8' assuming all the children create the same list of tests.
  Don't display progress. Print the status when all the children are done
  (use Unix.waitpid for this).
* for improved parallelism: provide a way to record test durations and then
  use them to partition the tests more evenly. Tests for which no duration
  is known (e.g. new tests) would be assigned randomly to other batches
  using the fallback rule (select test if (test_index + batch_index) mod
  num_batches = 0).
