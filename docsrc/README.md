How we maintain documentation for Testo
==

The online documentation for Testo consists of:

* Reference documentation converted from `.mli` files to HTML with Odoc.
  Extra handwritten documentation is found in `.mld` files.
* Handwritten documentation for the tutorial and the howtos, converted
  from Markdown to HTML with Pandoc.
* An HTTP server provided by GitHub Pages. It serves the HTML files
  found under `/docs` on the main branch at
  https://semgrep.github.io/testo, redirected to https://testocaml.net/.

See https://ocaml.org/docs/generating-documentation to understand the
Odoc setup.

To build or rebuild the documentation, run `make` from here
or `make doc` from the project root.

To preview the documentation locally, run `make live` (requires Python).
