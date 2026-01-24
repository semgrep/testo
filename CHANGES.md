## unreleased

- Experimental: `Testo.with_capture` now supports an `is_binary_mode`
  option for reading back binary data correctly according to the
  channel's mode on Windows
  ([#178](https://github.com/semgrep/testo/issues/178)).
- De-deprecated `Testo.with_open_temp_file` which now supports
  `get_random_key`, `perms`, `windows_binary`, and
  `windows_file_share_delete` options. The main novelty is that
  temporary files are now opened with `FILE_SHARE_DELETE` on Windows
  to avoid a class of cleanup failures linked to duplicate file handles
  ([#180](https://github.com/semgrep/testo/issues/180)).

## 0.3.4 (2026-01-17)

- Add support for checked output files ([#134](https://github.com/semgrep/testo/issues/134)).
- Add an option `inline_logs` to create a test for which logs are always or never shown inline ([#142](https://github.com/semgrep/testo/issues/142)).
- Add a command-line option `--max-inline-log-bytes` to limit the size of unchecked test output (logs) shown inline when reporting the status of a test. The default limit is 1MB ([#144](https://github.com/semgrep/testo/issues/144)).
- Improve internal error handling ([#153](https://github.com/semgrep/testo/pull/153), [#154](https://github.com/semgrep/testo/pull/154)).
- Add support for boolean selection queries on test tags, extending `-t` ([#5](https://github.com/semgrep/testo/issues/5)). The query language keywords `and`, `or`, `not`, `all`, and `none` can no longer be used as tag names.
- New experimental submodule `Testo.Lazy_with_output` module for lazy computations that cache standard output and error output in addition to the computation’s result or exception. This allows for sharing context between tests running in the same worker process. It saves unnecessary computations while providing the same logs for each test sharing this context ([#156](https://github.com/semgrep/testo/issues/156)).
- Report and highlight differences in Unix vs. Windows line endings as well as missing trailing newlines ([#163](https://github.com/semgrep/testo/pull/163)).
- Testo’s snapshot files are now open in text mode for Windows-Unix compatibility. When reading files on Windows, CRLFs are converted to LFs. When writing, LFs are converted to CRLFs. Git or equivalent must be set up to convert line endings appropriately when moving files across platforms ([#165](https://github.com/semgrep/testo/pull/165)).
- A series of functions for reading and writing files has been deprecated and renamed to hint that we’re reading or writing in text mode on Windows. These functions are `write_file`, `read_file`, `map_file`, `copy_file`, and `with_temp_file`. The new names are `write_text_file`, `read_text_file`, etc. ([#165](https://github.com/semgrep/testo/pull/165)).
- Testo’s own test suite now passes successfully on Windows.
- Add a `-C`/`--chdir` option to set the current working directory ([#167](https://github.com/semgrep/testo/pull/167)).
- Add support for a `Testo.check` function and testables, replicating the similar functionality found in Alcotest. This makes it practical to write tests that don’t depend on the Alcotest library ([#169](https://github.com/semgrep/testo/pull/169)).
- Show current working directory (cwd) when reporting missing files if one of the paths is relative ([#170](https://github.com/semgrep/testo/pull/170)).
- Rename the `broken` option of `Testo.create` and `Testo.update` to `flaky` ([#172](https://github.com/semgrep/testo/issues/172)).

## 0.2.0 (2025-09-11)

- Fix: handle windows paths correctly CLI help output (https://github.com/semgrep/testo/pull/121)
- Fix: correct path masking on Windows paths (https://github.com/semgrep/testo/pull/121)
- Fix: prevent “Bad file descriptor” errors arising from output redirection on Windows (https://github.com/semgrep/testo/pull/121)
- Fix: handle temporary file deletion cleanly on windows (https://github.com/semgrep/testo/pull/119)
- Fix: don’t set signals on Windows (https://github.com/semgrep/testo/pull/118).
- Add `Testo.with_chdir` (https://github.com/semgrep/testo/pull/104).
- Fix nonsensical diff formatting (https://github.com/semgrep/testo/pull/104).
- Fix: enable the approval of the output of a test that is expected to complete but produces the incorrect output. Running the `approve` subcommand on such a test now successfully changes its status from XFAIL to XPASS ([#103](https://github.com/semgrep/testo/pull/103)).
- Allow multiple `-s` search queries in the same test command, allowing the selection of various tests by their name or hash ([#110](https://github.com/semgrep/testo/pull/110)).
- Add a `--expert` option to hide the legend printed by `run` and `status` ([#109](https://github.com/semgrep/testo/issues/109)).
- Add a `--autoclean` option to `run` and `status` subcommands to delete test snapshots that don’t belong to any known test as it typically happens after tests are renamed ([#126](https://github.com/semgrep/testo/pull/126)).
- Add support for timeouts ([#127](https://github.com/semgrep/testo/issues/127)).

## 0.1.0 (2024-11-10)

First official release
