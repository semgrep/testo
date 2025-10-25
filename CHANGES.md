x.x.x (xxxx-xx-xx)
------------------

* Add support for checked output files
  ([#134](https://github.com/semgrep/testo/issues/134)).
* Add an option `inline_logs` to create a test for which logs are
  always or never shown inline
  ([#142](https://github.com/semgrep/testo/issues/142)).
* Add a command-line option `--max-inline-log-bytes` to limit the size
  of unchecked test output (logs) shown inline when reporting the
  status of a test. The default limit is 1MB
  ([#144](https://github.com/semgrep/testo/issues/144)).
* Improve internal error handling ([#153](https://github.com/semgrep/testo/pull/153), [#154](https://github.com/semgrep/testo/pull/154)).

0.2.0 (2025-09-11)
------------------

* Fix: handle windows paths correctly CLI help output
  (https://github.com/semgrep/testo/pull/121)
* Fix: correct path masking on Windows paths
  (https://github.com/semgrep/testo/pull/121)
* Fix: prevent "Bad file descriptor" errors arising from output redirection on
  Windows (https://github.com/semgrep/testo/pull/121)
* Fix: handle temporary file deletion cleanly on windows
  (https://github.com/semgrep/testo/pull/119)
* Fix: don't set signals on Windows (https://github.com/semgrep/testo/pull/118).
* Add `Testo.with_chdir` (https://github.com/semgrep/testo/pull/104).
* Fix nonsensical diff formatting (https://github.com/semgrep/testo/pull/104).
* Fix: enable the approval of the output of a test that is expected to
  complete but produces the incorrect output. Running the `approve`
  subcommand on such a test now successfully changes its status from
  XFAIL to XPASS ([#103](https://github.com/semgrep/testo/pull/103)).
* Allow multiple `-s` search queries in the same test command,
  allowing the selection of various tests by their name or hash
  ([#110](https://github.com/semgrep/testo/pull/110)).
* Add a `--expert` option to hide the legend printed by `run` and
  `status` ([#109](https://github.com/semgrep/testo/issues/109)).
* Add a `--autoclean` option to `run` and `status` subcommands to
  delete test snapshots that don't belong to any known test as it
  typically happens after tests are renamed
  ([#126](https://github.com/semgrep/testo/pull/126)).
* Add support for timeouts
  ([#127](https://github.com/semgrep/testo/issues/127)).

0.1.0 (2024-11-10)
------------------

First official release
