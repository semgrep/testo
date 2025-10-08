# Convert Markdown documents to HTML.
#
# This is crude but will have to do for now.
# We don't want to depend on a theme will break in a couple years.
#
# Run 'make build' to rebuild the docs.

# The destination folder mandated by GitHub Pages (web server)
DOCROOT = ../docs

# The pages are served at https://testocaml.net/
WWWROOT =

PAGES = tutorial howtos

HTML = \
  $(DOCROOT)/index.html \
  $(addprefix $(DOCROOT)/, $(addsuffix /index.html, $(PAGES)))

# We reuse Odoc's style sheet for our Pandoc-generated documents so as to
# get a uniform style.
CONVERT_MARKDOWN_TO_HTML = \
  pandoc -t html5 --standalone \
    --toc \
    --css=$(WWWROOT)/reference/odoc.support/odoc.css \
    --css=$(WWWROOT)/generic.css

.PHONY: build
build:
	# Markdown source -> HTML
	$(MAKE) $(HTML)
	cp generic.css $(DOCROOT)/
	# This file proves ownership to Google Search Console, helping us
	# to figure out why the docs are not being indexed.
	cp googlea3b4cee899c0b364.html $(DOCROOT)/
	# OCaml source -> HTML
	# We need to run 'dune build @doc' from the project root. Why?
	(cd .. && dune build @doc)
	rm -rf $(DOCROOT)/reference
	cp -a ../_build/default/_doc/_html $(DOCROOT)/reference

# Check that the tutorial's sample code works
.PHONY: test
test:
	cd tutorial && ./run-tutorial

$(DOCROOT)/index.html: index.md Makefile
	$(CONVERT_MARKDOWN_TO_HTML) $< -o $@

$(DOCROOT)/%/index.html: %/index.md Makefile
	$(CONVERT_MARKDOWN_TO_HTML) $< -o $@

# Run a local HTTP server to preview the docs
live:
	# The path to the "site" is /testo/ like for the GitHub Pages site.
	xdg-open http://0.0.0.0:8000/testo
	python3 -m http.server --directory . 8000
