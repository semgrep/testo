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
by wrapping them in 'Lwt_main.run'.")
  (depends
    (ocaml (>= 4.08.0))
    alcotest
    cmdliner
    fpath
    re
    lwt
  )
)
