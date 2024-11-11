#
# The usual Make targets
#

.PHONY: build
build:
	dune build .

# dune-release gets confused by dead symlinks so we create them dynamically
.PHONY: symlinks
symlinks:
	ln -sf _build/default/tests/test.exe test
	ln -sf _build/default/tests/failing_test.exe failing-test
	ln -sf _build/default/tests/parallel_test.exe parallel-test
	ln -sf _build/default/tests/test_alcotest.exe test-alcotest
	ln -sf _build/default/tests/meta_test.exe meta-test

.PHONY: delete-symlinks
delete-symlinks:
	rm -f test failing-test parallel-test test-alcotest meta-test

# Install opam dependencies. This requires pre-commit which requires
# Python. See scripts/dev-setup-alpine for an example of how to install
# pre-commit.
.PHONY: setup
setup:
	./scripts/dev-setup-all-platforms

.PHONY: test
test: build
	dune exec diff/tests/test.exe
	./test-alcotest
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
opam-release: delete-symlinks opam-files
	dune-release tag
	dune-release bistro

.PHONY: opam-files
opam-files:
	dune build *.opam
