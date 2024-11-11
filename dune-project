(lang dune 3.7)
(name testo)
(using menhir 2.1)

; Generate the opam files used to install dependencies.
;
(generate_opam_files)

; Default attributes of opam packages
(license "ISC")
(source (github semgrep/testo))
(homepage "https://github.com/semgrep/testo")
(maintainers "Martin Jambon <martin@semgrep.com>")
(authors
  "Martin Jambon"
  "Gabriel Jaldon"
)

; Opam packages

(package
  (name testo-diff)
  (synopsis "Pure-OCaml diff implementation")
  (description "\
This is a pure-OCaml implementation for computing line-by-line diffs.
The current implementation uses an algorithm similar to gestalt pattern
matching ported to OCaml by Gabriel Jaldon from Paul Butler's
Python implementation.
See https://github.com/paulgb/simplediff")
  (depends
    (ocaml (>= 4.08.0))
  )
)

(package
  (name testo-util)
  (synopsis "Modules shared by testo, testo-lwt, etc")
  (description "\
Testo is a test framework for OCaml.")
  (depends
    (ocaml (>= 4.08.0))
    fpath
    re
    testo-diff
  )
)

(package
  (name testo)
  (synopsis "Test framework for OCaml")
  (description "\
Testo is a test framework for OCaml providing new subcommands for capturing,
checking, and approving the output of tests.")
  (depends
    (ocaml (>= 4.08.0))
    (alcotest :with-test)
    cmdliner
    fpath
    re
    testo-util
  )
)

(package
  (name testo-lwt)
  (synopsis "Test framework for OCaml, Lwt variant")
  (description "\
Use this if the tests return Lwt promises and you can't make them synchronous
because 'Lwt_main.run' is not supported by your platform e.g. JavaScript.")
  (depends
    lwt
    ; Same direct dependencies as 'testo' above
    (ocaml (>= 4.08.0))
    (alcotest :with-test)
    cmdliner
    fpath
    re
    testo-util
  )
)
