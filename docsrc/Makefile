# Convert Markdown documents to HTML.
#
# This is crude but will have to do for now.
# We don't want to depend on a theme will break in a couple years.
#
# Run 'make build' to rebuild the docs.

# The destination folder mandated by GitHub Pages (web server)
DOCROOT = ../docs

# The pages are served at https://semgrep.github.io/testo/
WWWROOT = /testo

PAGES = tutorial howtos

HTML = \
  $(DOCROOT)/index.html \
  $(addprefix $(DOCROOT)/, $(addsuffix /index.html, $(PAGES)))

CONVERT_MARKDOWN_TO_HTML = \
  pandoc -t html5 --standalone \
    --css=$(WWWROOT)/css/testo.css

.PHONY: build
build:
	cp -a css $(DOCROOT)/
	# Markdown source -> HTML
	$(MAKE) $(HTML)
	# OCaml source -> HTML
	# We need to run 'dune build @doc' from the project root. Why?
	(cd .. && dune build @doc)
	rm -rf $(DOCROOT)/reference
	cp -a ../_build/default/_doc/_html $(DOCROOT)/reference

$(DOCROOT)/index.html: index.md Makefile
	$(CONVERT_MARKDOWN_TO_HTML) $< -o $@

$(DOCROOT)/%/index.html: %/index.md Makefile
	$(CONVERT_MARKDOWN_TO_HTML) $< -o $@

# Run a local HTTP server to preview the docs
live:
	# The path to the "site" is /testo/ like for the GitHub Pages site.
	xdg-open http://0.0.0.0:8000/testo
	python3 -m http.server --directory . 8000
