#
# The usual Make targets
#

.PHONY: build
build:
	dune build .

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

.PHONY: clean
clean:
	dune clean
	rm -rf tests/custom-snapshots/

# This is only part of the release process.
# See complete release instructions in CONTRIBUTING.md.
#
.PHONY: opam-release
opam-release:
	dune-release tag
	dune-release bistro
