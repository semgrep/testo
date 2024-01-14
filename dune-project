(lang dune 3.7)
(name semgrep)
(using menhir 2.1)

; Generate the opam files used to install dependencies.
;
; 'dune build' (+ early interruption) is sufficient to trigger an update
; of the opam files.
; TODO: find the minimum command for this and add it as a pre-commit hook.
;
(generate_opam_files)

; Default attributes of opam packages
(source (github semgrep/testo))
(homepage "https://github.com/semgrep/testo")
(maintainers "Martin Jambon" "OCaml community")
(authors "Martin Jambon")

; Opam packages

(package
  (name alcotest_ext)
  (synopsis "Extension of the Alcotest library")
  (description "\
Alcotest_ext extends the functionality of the Alcotest library by providing
new subcommands for capturing, checking, and approving the output of tests.")
  (depends
    (ocaml (>= 4.08.0))
    alcotest
    cmdliner
    re
  )
)

(package
  (name alcotest_ext_lwt)
  (synopsis "Extension of the Alcotest library for Lwt")
  (description "\
Use this if the tests return Lwt promises and you can't make them synchronous
by wrapping them in 'Lwt_main.run'.")
  (depends
    (ocaml (>= 4.08.0))
    alcotest_ext
  )
)
