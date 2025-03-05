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

0.1.0 (2024-11-10)
------------------

First official release
