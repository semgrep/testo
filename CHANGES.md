* Add `Testo.with_chdir` (https://github.com/semgrep/testo/pull/104).
* Fix nonsensical diff formatting (https://github.com/semgrep/testo/pull/104).
* Fix: enable the approval of the output of a test that is expected to
  complete but produces the incorrect output. Running the `approve`
  subcommand on such a test now successfully changes its status from
  XFAIL to XPASS ([#103](https://github.com/semgrep/testo/pull/103)).
* Require re >= 1.12.0 to get consistent results when using
  `Testo.mask_pcre_pattern` with some patterns that match the empty
  substring at the end of the input. For example,
  `Testo.mask_pcre_pattern {|\b|} "word"` would give `<MASKED>word`
  with re 1.11.0 instead of `<MASKED>word<MASKED>` with re 1.12.0.

0.1.0 (2024-11-10)
------------------

First official release
