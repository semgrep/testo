* Fix: enable the approval of the output of a test that is expected to
  complete but produces the incorrect output. Running the `approve`
  subcommand on such a test now successfully changes its status from
  XFAIL to XPASS ([#103](https://github.com/semgrep/testo/pull/103)).

0.1.0 (2024-11-10)
------------------

First official release
