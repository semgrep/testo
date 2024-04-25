(lang dune 3.7)
(name semgrep)
(using menhir 2.1)

; Generate the opam files used to install dependencies.
;
(generate_opam_files)

; Default attributes of opam packages
(source (github semgrep/testo))
(homepage "https://github.com/semgrep/testo")
(maintainers "Martin Jambon" "OCaml community")
(authors "Martin Jambon")

; Opam packages

(package
  (name testo)
  (synopsis "Test framework for OCaml")
  (description "\
Testo extends the functionality of the Alcotest library by providing
new subcommands for capturing, checking, and approving the output of tests.")
  (depends
    (ocaml (>= 4.08.0))
    ; Alcotest is needed by the functions that export a test suite to
    ; Alcotest. If this dependency is a problem, we could create a separate
    ; package for this functionality.
    alcotest
    cmdliner
    fpath
    re
  )
)

(package
  (name testo-lwt)
  (synopsis "Test framework for OCaml, Lwt variant")
  (description "\
Use this if the tests return Lwt promises and you can't make them synchronous
because 'Lwt_main.run' is not supported by your platform e.g. JavaScript.")
  (depends
    ; Same dependencies as 'testo' above plus Lwt
    (ocaml (>= 4.08.0))
    alcotest
    cmdliner
    fpath
    re
    lwt
  )
)
