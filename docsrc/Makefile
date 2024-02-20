# Convert Markdown documents to HTML.
#
# This is crude but will have to do for now.
# We don't want to depend on a theme will break in a couple years.
#
# Run 'make build' to rebuild the docs.

# The destination folder mandated by GitHub Pages (web server)
DOCROOT = ../docs

PAGES = tutorial howtos

HTML = \
  $(DOCROOT)/index.html \
  $(addprefix $(DOCROOT)/, $(addsuffix /index.html, $(PAGES)))

CONVERT_MARKDOWN_TO_HTML = pandoc -t html5 --standalone

.PHONY: build
build:
	# Markdown source -> HTML
	$(MAKE) $(HTML)
	# OCaml source -> HTML
	# We need to run 'dune build @doc' from the project root. Why?
	(cd .. && dune build @doc)
	rm -rf ../docs/reference
	cp -a ../_build/default/_doc/_html ../docs/reference

$(DOCROOT)/index.html: index.md Makefile
	$(CONVERT_MARKDOWN_TO_HTML) $< -o $@

$(DOCROOT)/%/index.html: %/index.md Makefile
	$(CONVERT_MARKDOWN_TO_HTML) $< -o $@
