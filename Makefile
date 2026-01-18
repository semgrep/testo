#
# The usual Make targets
#

.PHONY: build
build: symlinks
	dune build .

# dune-release gets confused by dead symlinks so we create them dynamically
.PHONY: symlinks
symlinks:
	ln -sf ../../bin/testo-diff tests/diff-data/testo-diff
	# expose testo-diff in bin/
	ln -sf _build/install/default/bin .

.PHONY: delete-symlinks
delete-symlinks:
	rm -f tests/diff-data/testo-diff bin

# Install opam dependencies. This requires pre-commit which requires
# Python. See scripts/dev-setup-alpine for an example of how to install
# pre-commit.
.PHONY: setup
setup:
	./scripts/dev-setup-all-platforms

# Create a clean local opam switch for development purposes
.PHONY: opam-setup
opam-setup:
	opam switch create . ocaml-base-compiler.5.4.0 --deps-only

# Run all the tests.
# coupling: 'dune runtest' is also set up to run the same tests.
.PHONY: test
test: build
	dune exec diff/tests/test.exe
	./test-alcotest
	./test
	./meta-test

# The test program offers various options. This is one of them.
.PHONY: retest
retest: build
	./meta-test --lazy

# See doc/README.md for info on how build and contribute to the docs
.PHONY: doc
doc:
	$(MAKE) -C docsrc
	$(MAKE) -C docsrc test

# Run HTTP server that serves the docs (requires Python)
.PHONY: livedoc
livedoc:
	$(MAKE) -C docsrc live

.PHONY: clean
clean:
	dune clean
	rm -rf tests/custom-snapshots/
	$(MAKE) delete-symlinks

# This is only part of the release process.
# See complete release instructions in CONTRIBUTING.md.
#
.PHONY: opam-release
opam-release: delete-symlinks opam-files changes
	dune-release tag
	dune-release bistro --draft

.PHONY: opam-files
opam-files:
	dune build *.opam

# Rewrap the changelog because GitHub incorrectly treats single line breaks
# as significant on release notes and pull request descriptions.
.PHONY: changes
changes:
	pandoc -f markdown -t gfm --wrap=none CHANGES.md -o CHANGES.md
