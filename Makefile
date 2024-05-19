#
# The usual Make targets
#

.PHONY: build
build:
	dune build .

# Install opam dependencies
.PHONY: setup
setup:
	./scripts/dev-setup-all-platforms
	# dev only:
	pre-commit install

.PHONY: test
test: build
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
